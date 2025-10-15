FUNCTION z_fi_outbound_customer.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      OUTCUSTOMER STRUCTURE  ZFIE_CUSTOMER
*"      OUTBANK STRUCTURE  ZFIE_BANK
*"----------------------------------------------------------------------

*--> 22.08.2023 23:58:08 - Migração S4 – ML - Início
  DATA: cl_proxy    TYPE REF TO zco_z_fi_outbound_customer_por,
        v_input	    TYPE zzfi_outbound_customer_input2,
        "v_output    TYPE zzfi_outbound_customer_outpu2,
        v_item_bank TYPE zzfie_bank,
        v_item_cust TYPE zzfie_customer.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-outbank-item[],
               v_input-outcustomer-item[].

      LOOP AT outcustomer INTO DATA(w_outcustomer).
        CLEAR: v_item_cust.
        v_item_cust-id_cliente = w_outcustomer-id_cliente.
        v_item_cust-empresa = w_outcustomer-empresa.
        v_item_cust-gr_conta = w_outcustomer-gr_conta.
        v_item_cust-tx_tratamento = w_outcustomer-tx_tratamento.
        v_item_cust-descricao = w_outcustomer-descricao.
        v_item_cust-tr_pesquisa = w_outcustomer-tr_pesquisa.
        v_item_cust-rua = w_outcustomer-rua.
        v_item_cust-numero = w_outcustomer-numero.
        v_item_cust-bairro = w_outcustomer-bairro.
        v_item_cust-cd_postal = w_outcustomer-cd_postal.
        v_item_cust-cidade = w_outcustomer-cidade.
        v_item_cust-estado = w_outcustomer-estado.
        v_item_cust-cx_postal = w_outcustomer-cx_postal.
        v_item_cust-cd_caixa_postal = w_outcustomer-cd_caixa_postal.
        v_item_cust-idioma = w_outcustomer-idioma.
        v_item_cust-nu_telefone = w_outcustomer-nu_telefone.
        v_item_cust-nu_ramal = w_outcustomer-nu_ramal.
        v_item_cust-nu_celular = w_outcustomer-nu_celular.
        v_item_cust-nu_fax = w_outcustomer-nu_fax.
        v_item_cust-nu_ramal_fax = w_outcustomer-nu_ramal_fax.
        v_item_cust-mo_comunicacao = w_outcustomer-mo_comunicacao.
        v_item_cust-ob_endereco = w_outcustomer-ob_endereco.
        v_item_cust-cnpj = w_outcustomer-cnpj.
        v_item_cust-fl_pessoa_fisica = w_outcustomer-fl_pessoa_fisica.
        v_item_cust-cpf = w_outcustomer-cpf.
        v_item_cust-insestadual = w_outcustomer-insestadual.
        v_item_cust-insmunicipal = w_outcustomer-insmunicipal.
        v_item_cust-nome = w_outcustomer-nome.
        v_item_cust-ds_nome = w_outcustomer-ds_nome.
        v_item_cust-dp_contato = w_outcustomer-dp_contato.
        v_item_cust-fn_contato = w_outcustomer-fn_contato.
        v_item_cust-cn_reconciliacao = w_outcustomer-cn_reconciliacao.
        v_item_cust-ch_ordenacao = w_outcustomer-ch_ordenacao.
        v_item_cust-gr_administracao = w_outcustomer-gr_administracao.
        v_item_cust-id_sigam = w_outcustomer-id_sigam.
        v_item_cust-ch_pagamento = w_outcustomer-ch_pagamento.
        v_item_cust-fr_pagamento = w_outcustomer-fr_pagamento.
        v_item_cust-cd_imposto_retido = w_outcustomer-cd_imposto_retido.
        v_item_cust-email = w_outcustomer-email.
        v_item_cust-st_atualizacao = w_outcustomer-st_atualizacao.
        v_item_cust-dt_atualizacao = w_outcustomer-dt_atualizacao.
        v_item_cust-hr_atualizacao = w_outcustomer-hr_atualizacao.
        v_item_cust-cd_transacao = w_outcustomer-cd_transacao.
        v_item_cust-bl_toda_empresa = w_outcustomer-bl_toda_empresa.
        v_item_cust-bl_empresa = w_outcustomer-bl_empresa.
        v_item_cust-el_toda_area = w_outcustomer-el_toda_area.
        v_item_cust-el_empresa = w_outcustomer-el_empresa.
        v_item_cust-el_dados = w_outcustomer-el_dados.
        v_item_cust-el_empresa_dados = w_outcustomer-el_empresa_dados.
        v_item_cust-suframa = w_outcustomer-suframa.
        v_item_cust-setor_industrial = w_outcustomer-setor_industrial.
        v_item_cust-vat = w_outcustomer-vat.
        v_item_cust-pais = w_outcustomer-pais.
        v_item_cust-descricao2 = w_outcustomer-descricao2.
        v_item_cust-sala_edificio = w_outcustomer-sala_edificio.
        v_item_cust-sala = w_outcustomer-sala.
        v_item_cust-andar = w_outcustomer-andar.
        v_item_cust-stcd5 = w_outcustomer-stcd5.
        APPEND v_item_cust TO v_input-outcustomer-item.
      ENDLOOP.

      LOOP AT outbank INTO DATA(w_outbank).
        CLEAR: v_item_bank.
        v_item_bank-id_cliente = w_outbank-id_cliente.
        v_item_bank-dt_atualizacao = w_outbank-dt_atualizacao.
        v_item_bank-hr_atualizacao = w_outbank-hr_atualizacao.
        v_item_bank-cd_pais_banco = w_outbank-cd_pais_banco.
        v_item_bank-ch_banco = w_outbank-ch_banco.
        v_item_bank-nu_conta_bancaria = w_outbank-nu_conta_bancaria.
        v_item_bank-dv_agencia = w_outbank-dv_agencia.
        v_item_bank-banco_parceiro = w_outbank-banco_parceiro.
        v_item_bank-st_atualizacao = w_outbank-st_atualizacao.
        v_item_bank-ind_ref_banco = w_outbank-ind_ref_banco.
        APPEND v_item_bank TO v_input-outbank-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_customer
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.

*<-- 22.08.2023 23:58:08 - Migração S4 – ML – Fim

ENDFUNCTION.

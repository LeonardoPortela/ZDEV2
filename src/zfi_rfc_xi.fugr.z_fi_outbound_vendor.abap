FUNCTION z_fi_outbound_vendor.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      OUTVENDOR STRUCTURE  ZFIE_VENDOR
*"      OUTBANK STRUCTURE  ZFIE_BANK
*"----------------------------------------------------------------------


*--> 24.08.2023 17:35:44 - Migração S4 – ML - Início
  DATA: cl_proxy    TYPE REF TO zco_z_fi_outbound_vendor_port,
        v_input	    TYPE zzfi_outbound_vendor_input,
        "v_output    TYPE zzfi_outbound_vendor_output,
        v_item_bank TYPE zzfie_bank,
        v_item_vend TYPE zzfie_vendor.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-outbank-item[],
               v_input-outvendor-item[].

      LOOP AT outvendor INTO DATA(w_outvendor).
        CLEAR: v_item_vend.
        v_item_vend-id_fornecedor = w_outvendor-id_fornecedor.
        v_item_vend-empresa = w_outvendor-empresa.
        v_item_vend-gr_conta = w_outvendor-gr_conta.
        v_item_vend-tx_tratamento = w_outvendor-tx_tratamento.
        v_item_vend-descricao = w_outvendor-descricao.
        v_item_vend-tr_pesquisa = w_outvendor-tr_pesquisa.
        v_item_vend-rua = w_outvendor-rua.
        v_item_vend-numero = w_outvendor-numero.
        v_item_vend-bairro = w_outvendor-bairro.
        v_item_vend-cd_postal = w_outvendor-cd_postal.
        v_item_vend-cidade = w_outvendor-cidade.
        v_item_vend-estado = w_outvendor-estado.
        v_item_vend-cx_postal = w_outvendor-cx_postal.
        v_item_vend-cd_caixa_postal = w_outvendor-cd_caixa_postal.
        v_item_vend-idioma = w_outvendor-idioma.
        v_item_vend-nu_telefone = w_outvendor-nu_telefone.
        v_item_vend-nu_ramal = w_outvendor-nu_ramal.
        v_item_vend-nu_celular = w_outvendor-nu_celular.
        v_item_vend-nu_fax = w_outvendor-nu_fax.
        v_item_vend-nu_ramal_fax = w_outvendor-nu_ramal_fax.
        v_item_vend-mo_comunicacao = w_outvendor-mo_comunicacao.
        v_item_vend-ob_endereco = w_outvendor-ob_endereco.
        v_item_vend-cnpj = w_outvendor-cnpj.
        v_item_vend-fl_pessoa_fisica = w_outvendor-fl_pessoa_fisica.
        v_item_vend-cpf = w_outvendor-cpf.
        v_item_vend-insestadual = w_outvendor-insestadual.
        v_item_vend-insmunicipal = w_outvendor-insmunicipal.
        v_item_vend-id_pis = w_outvendor-id_pis.
        v_item_vend-nome = w_outvendor-nome.
        v_item_vend-ds_nome = w_outvendor-ds_nome.
        v_item_vend-dp_contato = w_outvendor-dp_contato.
        v_item_vend-fn_contato = w_outvendor-fn_contato.
        v_item_vend-cn_reconciliacao = w_outvendor-cn_reconciliacao.
        v_item_vend-ch_ordenacao = w_outvendor-ch_ordenacao.
        v_item_vend-gr_administracao = w_outvendor-gr_administracao.
        v_item_vend-id_sigam = w_outvendor-id_sigam.
        v_item_vend-ch_pagamento = w_outvendor-ch_pagamento.
        v_item_vend-fr_pagamento = w_outvendor-fr_pagamento.
        v_item_vend-cd_imposto_retido = w_outvendor-cd_imposto_retido.
        v_item_vend-email = w_outvendor-email.
        v_item_vend-st_atualizacao = w_outvendor-st_atualizacao.
        v_item_vend-dt_atualizacao = w_outvendor-dt_atualizacao.
        v_item_vend-hr_atualizacao = w_outvendor-hr_atualizacao.
        v_item_vend-cd_transacao = w_outvendor-cd_transacao.
        v_item_vend-bl_toda_empresa = w_outvendor-bl_toda_empresa.
        v_item_vend-bl_qualidade = w_outvendor-bl_qualidade.
        v_item_vend-bl_empresa = w_outvendor-bl_empresa.
        v_item_vend-el_toda_area = w_outvendor-el_toda_area.
        v_item_vend-el_empresa = w_outvendor-el_empresa.
        v_item_vend-el_dados = w_outvendor-el_dados.
        v_item_vend-el_empresa_dados = w_outvendor-el_empresa_dados.
        v_item_vend-rntrc = w_outvendor-rntrc.
        v_item_vend-setor_industrial = w_outvendor-setor_industrial.
        v_item_vend-vat = w_outvendor-vat.
        v_item_vend-pais = w_outvendor-pais.
        v_item_vend-descricao2 = w_outvendor-descricao2.
        v_item_vend-fnc_bloqueio = w_outvendor-fnc_bloqueio.
        v_item_vend-sala_edificio = w_outvendor-sala_edificio.
        v_item_vend-sala = w_outvendor-sala.
        v_item_vend-andar = w_outvendor-andar.
        v_item_vend-stcd5 = w_outvendor-stcd5.
        v_item_vend-gbort = w_outvendor-gbort.
        v_item_vend-gbdat = w_outvendor-gbdat.
        v_item_vend-sexkz = w_outvendor-sexkz.
        v_item_vend-emissor_nfe = w_outvendor-emissor_nfe.
        v_item_vend-home_city = w_outvendor-home_city.
        v_item_vend-tipo_industria = w_outvendor-tipo_industria.
        APPEND v_item_vend TO v_input-outvendor-item.
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

      CALL METHOD cl_proxy->z_fi_outbound_vendor
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 24.08.2023 17:35:44 - Migração S4 – ML – Fim


ENDFUNCTION.

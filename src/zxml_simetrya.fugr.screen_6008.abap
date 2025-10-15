
process before output.

  module status_6008.
*
process after input.

  chain.
    field zcte_ciot-rm_codigo.
    field zcte_ciot-rm_nome.
    field zcte_ciot-rm_razao.
    field zcte_ciot-rm_cnpj.
    field zcte_ciot-rm_cpf.
    field zcte_ciot-rm_logradouro.
    field zcte_ciot-rm_numero.
    field zcte_ciot-rm_complemento.
    field zcte_ciot-rm_bairro.
    field zcte_ciot-rm_uf.
    field zcte_ciot-rm_municipio.
    field zcte_ciot-rm_cep.
    field zcte_ciot-rm_fone.
    module set_update_flag on chain-request.
  endchain.

process on value-request.
  field zcte_ciot-rm_municipio module cte_busca_mun_rm.

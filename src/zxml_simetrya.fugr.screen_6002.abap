
process before output.

  module status_6002.
*
process after input.

  chain.
    field zcte_ciot-ct_codigo.
    field zcte_ciot-ct_nome.
    field zcte_ciot-ct_razao.
    field zcte_ciot-ct_cnpj.
    field zcte_ciot-ct_cpf.
    field zcte_ciot-ct_logradouro.
    field zcte_ciot-ct_numero.
    field zcte_ciot-ct_complemento.
    field zcte_ciot-ct_bairro.
    field zcte_ciot-ct_uf.
    field zcte_ciot-ct_municipio.
    field zcte_ciot-ct_cep.
    field zcte_ciot-ct_fone.
    module set_update_flag on chain-request.
  endchain.

process on value-request.
  field zcte_ciot-ct_municipio module cte_busca_mun_ct.

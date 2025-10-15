
process before output.

  module status_6007.
*
process after input.

  chain.
    field zcte_ciot-cs_codigo.
    field zcte_ciot-cs_nome.
    field zcte_ciot-cs_razao.
    field zcte_ciot-cs_cnpj.
    field zcte_ciot-cs_cpf.
    field zcte_ciot-cs_logradouro.
    field zcte_ciot-cs_numero.
    field zcte_ciot-cs_complemento.
    field zcte_ciot-cs_bairro.
    field zcte_ciot-cs_uf.
    field zcte_ciot-cs_municipio.
    field zcte_ciot-cs_cep.
    field zcte_ciot-cs_fone.
    module set_update_flag on chain-request.
  endchain.

process on value-request.
  field zcte_ciot-cs_municipio module cte_busca_mun_cs.

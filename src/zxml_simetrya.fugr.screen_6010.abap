
process before output.

  module status_6010.
*
process after input.

  chain.
    field zcte_ciot-tr_codigo.
    field zcte_ciot-tr_nome.
    field zcte_ciot-tr_razao.
    field zcte_ciot-tr_cnpj.
    field zcte_ciot-tr_cpf.
    field zcte_ciot-tr_logradouro.
    field zcte_ciot-tr_numero.
    field zcte_ciot-tr_complemento.
    field zcte_ciot-tr_bairro.
    field zcte_ciot-tr_uf.
    field zcte_ciot-tr_municipio.
    field zcte_ciot-tr_cep.
    field zcte_ciot-tr_fone.
    module set_update_flag on chain-request.
  endchain.

process on value-request.
  field zcte_ciot-tr_municipio module cte_busca_mun_tr.

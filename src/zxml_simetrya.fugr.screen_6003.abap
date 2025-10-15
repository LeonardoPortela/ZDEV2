
process before output.

  module status_6003.
*
process after input.

  chain.
    field zcte_ciot-dt_codigo.
    field zcte_ciot-dt_nome.
    field zcte_ciot-dt_razao.
    field zcte_ciot-dt_cnpj.
    field zcte_ciot-dt_cpf.
    field zcte_ciot-dt_logradouro.
    field zcte_ciot-dt_numero.
    field zcte_ciot-dt_complemento.
    field zcte_ciot-dt_bairro.
    field zcte_ciot-dt_uf.
    field zcte_ciot-dt_municipio.
    field zcte_ciot-dt_cep.
    field zcte_ciot-dt_fone.
    module set_update_flag on chain-request.
  endchain.

process on value-request.
  field zcte_ciot-dt_municipio module cte_busca_mun_dt.

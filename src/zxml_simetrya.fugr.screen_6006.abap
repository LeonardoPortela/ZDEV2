
process before output.

  module status_6006.
*
process after input.

  chain.
    field zcte_ciot-sb_codigo.
    field zcte_ciot-sb_nome.
    field zcte_ciot-sb_razao.
    field zcte_ciot-sb_cnpj.
    field zcte_ciot-sb_cpf.
    field zcte_ciot-sb_logradouro.
    field zcte_ciot-sb_numero.
    field zcte_ciot-sb_complemento.
    field zcte_ciot-sb_bairro.
    field zcte_ciot-sb_uf.
    field zcte_ciot-sb_municipio.
    field zcte_ciot-sb_cep.
    field zcte_ciot-sb_fone.
    module set_update_flag on chain-request.
  endchain.

process on value-request.

  field zcte_ciot-sb_municipio module cte_busca_mun_sb.

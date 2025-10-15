
process before output.

  module cte_status_0105.
*
process after input.

  chain.
    field zcte_identifica-cmunenv.
    module cte_muda_muni_envio on chain-request.
  endchain.

  chain.
    field zcte_identifica-cmunini.
    module cte_muda_muni_inicial on chain-request.
  endchain.

  chain.
    field zcte_identifica-cmunfim.
    module cte_muda_muni_final on chain-request.
  endchain.

  module user_command_0105.

process on value-request.

  field zcte_identifica-cmunini module cte_busca_mun_inicio.
  field zcte_identifica-cmunfim module cte_busca_mun_fim.


process before output.
  module detail_set_stu.
  module detail_init.

process after input.

  module detail_exit_command at exit-command.
  module detail_set_pfstatus.

  chain.
    field zlest0041-quantidade .
    field zlest0041-data_emissao .
    field zlest0041-cod_material .
    field zlest0041-docnum.
    "module zbusca_info_nota.
  endchain.

  chain.
    field zlest0041-centro_comprador .
    field zlest0041-nr_nf .
    field zlest0041-cod_cliente .
    field zlest0041-serie .
    module set_update_flag on chain-request.
  endchain.

  chain.
    field  zlest0041-cod_cliente .
    module zverifica_cliente on chain-request.
  endchain.

  chain.
    field zlest0041-centro_comprador .
    field zlest0041-nr_nf_propria.
    field zlest0041-serie_propria.
    module zbusca_info_nota on chain-request.
  endchain.

  module salvar.

  chain.
    field zlest0041-nr_nf .
    field zlest0041-serie .
    module detail_pai.
  endchain.

  module inserir.
  module detail_pai.

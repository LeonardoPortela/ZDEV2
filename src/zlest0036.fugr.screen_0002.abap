
process before output.
  module detail_init.
  module travacampos.
*
process after input.

  module detail_exit_command at exit-command.
  module detail_set_pfstatus.
  chain.
    field zlest0036-bukrs          .
    field zlest0036-branch         .
    field zlest0036-transportadora .
    field zlest0036-nr_cf_inicial  .
    field zlest0036-nr_cf_final    .
    field zlest0036-observacao     .
    module set_update_flag on chain-request.
  endchain.

  field zlest0036-nr_cf_inicial module zverifica_inicial on request.
  field zlest0036-nr_cf_final   module zverifica_final   on request.
  "field zlest0036-transportadora module zverifica_transp on request.
  chain.
    field zlest0036-nrparametro.
    module zgeranumero.
  endchain.

  chain.
    field zlest0036-nrparametro .
    module detail_pai.
  endchain.

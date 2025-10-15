
process before output.
  module detail_init.
*
process after input.
  module detail_exit_command at exit-command.
  module detail_set_pfstatus.
  chain.
    field zpfe_lote_his_de-chvid_adm .
    field zpfe_lote_his_de-chvid_emp .
    module set_update_flag on chain-request.
  endchain.
  chain.
    field zpfe_lote_his_de-chvid_adm .
    module detail_pai.
  endchain.

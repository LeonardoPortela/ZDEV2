
process before output.

  module status_9002.
*
process after input.

  chain.
    field zgl015_dre_est01-bukrs module z_nome_empresa on input .
  endchain.

  module user_command_9002_exit at exit-command.

  module user_command_9002.

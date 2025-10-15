
process before output.

  module status_0021.

  module visibilidade_campos.
*
process after input.

  chain.
    field znom_programacao-id_nomeacao_tran.
    field znom_programacao-id_empresa.
    field znom_programacao-id_filial module check_filial on request.
    field znom_programacao-id_material.
    "field znom_programacao-id_cliente.
    field znom_programacao-nr_programada.
    field znom_programacao-id_unidade.
  endchain.

  module user_command_0021_exit at exit-command.

  module user_command_0021.

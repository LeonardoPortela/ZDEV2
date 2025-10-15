
process before output.
  module status_2000.
  module trata_fields.
  module cria_objetos_2000.
*
process after input.
  module user_command_2000.
  module user_command_exit at exit-command.

process on value-request.
  field wg_cadinvo-lifnr     module search_fornecedor.

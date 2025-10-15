
process before output.

  module status_9960.
*
process after input.

  module user_command_9960_exit at exit-command.

  chain.
    field: zcte_identifica-reason
    module reason_input_9960 on request.
  endchain.

  module user_command_9960.

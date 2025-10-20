
process before output.

  module status_0200.

process after input.

  module user_exit_0200 at exit-command.

  field: v_placa1 module z_placa1 on request,
         v_placa2 module z_placa2 on request,
         v_placa3 module z_placa3 on request,
         v_placa4 module z_placa4 on request.

  module user_command_0200.

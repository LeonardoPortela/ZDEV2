
process before output.

  module status_0001.

  call subscreen sub001 including sy-repid imp_dynnr_000.
*
process after input.

  module user_command_exit at exit-command.

  module user_command_0001.

  call subscreen sub001.

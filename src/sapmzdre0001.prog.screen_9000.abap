
process before output.

  module status_9000.
*
  call subscreen: sub9000 including sy-cprog tl_9000.

process after input.

  module user_command_9000_exit at exit-command.

  call subscreen: sub9000.

  module user_command_9000.

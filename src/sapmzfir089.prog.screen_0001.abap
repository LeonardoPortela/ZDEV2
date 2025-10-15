
process before output.

  module status_0001.

  call subscreen: sub0002 including sy-cprog tl_0002.

process after input.

  call subscreen: sub0002.

  module user_command_0001_exit at exit-command.

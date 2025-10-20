
process before output.

  module status_3000.
*
process after input.

  module user_command_3000_exit at exit-command.

  chain.
    module get_scroll_info.
    module get_selected_rows.
    module user_command_3000.
  endchain.

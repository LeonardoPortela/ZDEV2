PROCESS BEFORE OUTPUT.

  MODULE status_0100.
*
PROCESS AFTER INPUT.

  MODULE user_command_0100_exit AT EXIT-COMMAND.

  CHAIN.

    MODULE get_scroll_info.

    MODULE get_selected_rows.

    MODULE user_command_0100.

  ENDCHAIN.

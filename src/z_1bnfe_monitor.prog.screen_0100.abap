
PROCESS BEFORE OUTPUT.

  MODULE log_icon_init.

  MODULE create_objects.

  MODULE status_0100.

  MODULE set_scroll_info.

PROCESS AFTER INPUT.

  MODULE exit_command AT EXIT-COMMAND.

* Also in case of error messages all modules are to be processed
  CHAIN.

    MODULE get_scroll_info.

    MODULE get_selected_rows.

    MODULE user_command_0100.

  ENDCHAIN.

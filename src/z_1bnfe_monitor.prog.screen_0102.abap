
* This screen is delivered with note 1144194

PROCESS BEFORE OUTPUT.

  MODULE create_objects_0102.

  MODULE fill_alv_0102.

  MODULE status_0102.

  MODULE set_scroll_info_0102.


PROCESS AFTER INPUT.

  MODULE exit_command_0102 AT EXIT-COMMAND.

* Also in case of error messages all modules are to be processed
  CHAIN.

    MODULE get_scroll_info_0102.

    FIELD: j_1bnfe_active-reason
    MODULE process_input_0102 ON CHAIN-INPUT.

    MODULE get_selected_rows_0102.

    MODULE user_command_0102.

  ENDCHAIN.

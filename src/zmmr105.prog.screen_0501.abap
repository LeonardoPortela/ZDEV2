
PROCESS BEFORE OUTPUT.

  MODULE status_0501.
*
PROCESS AFTER INPUT.

  MODULE user_command_0501_exit AT EXIT-COMMAND.

  CHAIN.
    FIELD: wa_peso_liberado-qt_diferenca.
    MODULE alterou_diferenca ON CHAIN-REQUEST.

    MODULE get_scroll_info_0501.

    MODULE get_selected_rows_0501.

    MODULE user_command_0501.

  ENDCHAIN.

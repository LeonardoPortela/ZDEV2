PROCESS BEFORE OUTPUT.
  MODULE drop_down_box.
  MODULE status_0200.
*
PROCESS AFTER INPUT.
  CHAIN.
    FIELD: g_dt_baixa MODULE check_dt_baixa,
           g_baixar   MODULE check_baixar.
  ENDCHAIN.

  MODULE user_command_0200.

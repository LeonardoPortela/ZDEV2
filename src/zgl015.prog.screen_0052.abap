
PROCESS BEFORE OUTPUT.
   MODULE ts_100_active_tab_set.

  CALL SUBSCREEN ts_100_sca
    INCLUDING g_ts_100-prog g_ts_100-subscreen.



PROCESS AFTER INPUT.
  CALL SUBSCREEN ts_100_sca.
  MODULE ts_100_active_tab_get.

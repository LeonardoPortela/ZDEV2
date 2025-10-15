FUNCTION zsd_log_cancel_romaneio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_DADOS STRUCTURE  ZMMT0008
*"----------------------------------------------------------------------

  tg_dados[] = t_dados[].

  FREE: obj_container,
        obj_alv,
        obj_toolbar.

  CALL SCREEN 0100 STARTING AT 25  05
                     ENDING AT 150 16.

ENDFUNCTION.

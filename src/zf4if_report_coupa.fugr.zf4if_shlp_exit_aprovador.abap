FUNCTION zf4if_shlp_exit_aprovador.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) TYPE  DDSHF4CTRL
*"----------------------------------------------------------------------

  CASE callcontrol-step.
    WHEN 'DISP'.
      SORT record_tab.
      DELETE ADJACENT DUPLICATES FROM record_tab.
  ENDCASE.

ENDFUNCTION.

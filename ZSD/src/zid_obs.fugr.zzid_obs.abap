FUNCTION zzid_obs.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------


  IF callcontrol-step = 'DISP'.


    TYPES : BEGIN OF ty_result,
              id_obs  TYPE zsdt0287-id_obs,
              observ  TYPE zsdt0287-observ,
              observ2 TYPE zsdt0287-observ2,
    end of ty_result.


    DATA : li_result TYPE TABLE OF ty_result,
           lw_result TYPE ty_result.

    REFRESH : record_tab.


    SELECT id_obs  observ  observ2 FROM zsdt0287
       INTO TABLE li_result WHERE cancelado NE 'X'.


      CALL FUNCTION 'F4UT_RESULTS_MAP'
        EXPORTING
          apply_restrictions = 'X'
        TABLES
          shlp_tab           = shlp_tab
          record_tab         = record_tab
          source_tab         = li_result
        CHANGING
          shlp               = shlp
          callcontrol        = callcontrol
        EXCEPTIONS
          illegal_structure  = 1
          OTHERS             = 2.

    ENDIF.

  ENDFUNCTION.

FUNCTION zsdmf_insumos_atu_impo_fertili.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IR_VBELN_VA) TYPE  FIP_T_VBELN_RANGE OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_ERRO) TYPE  FLAG
*"----------------------------------------------------------------------

  PERFORM f_refresh_9000.

  PERFORM f_get_j1btax_tab.

  IF ir_vbeln_va[] IS NOT INITIAL.

    so_vbeln[] = ir_vbeln_va[].

    PERFORM f_search_9000.

  ENDIF.

  CALL SCREEN 9000 STARTING AT 20 1.

  ev_erro = gv_9000_canc.

ENDFUNCTION.

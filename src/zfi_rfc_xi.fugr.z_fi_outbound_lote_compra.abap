FUNCTION z_fi_outbound_lote_compra.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      RETURN STRUCTURE  ZGL002_COMP_F44
*"----------------------------------------------------------------------
*--> 24.08.2023 18:05:12 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_lote_compra,
        v_input	 TYPE zzfi_outbound_lote_compra_in1,
        "v_output TYPE zzfi_outbound_lote_compra_ou1,
        v_item   TYPE zzgl002_comp_f44.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-return-item[].

      LOOP AT return INTO DATA(w_return).
        CLEAR: v_item.
        v_item-mandt = w_return-mandt.
        v_item-bukrs = w_return-bukrs.
        v_item-lote = w_return-lote.
        v_item-status = w_return-status.
        v_item-sgtxt = w_return-sgtxt.
        v_item-data = w_return-data.
        v_item-hora = w_return-hora.
        v_item-data_comp = w_return-data_comp.
        v_item-interface = w_return-interface.
        v_item-info_adicional_1 = w_return-info_adicional_1.
        v_item-info_adicional_2 = w_return-info_adicional_2.
        v_item-info_adicional_3 = w_return-info_adicional_3.
        APPEND v_item TO v_input-return-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_lote_compra
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 24.08.2023 18:05:12 - Migração S4 – ML – Fim
ENDFUNCTION.

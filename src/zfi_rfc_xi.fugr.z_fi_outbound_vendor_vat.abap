FUNCTION z_fi_outbound_vendor_vat.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_LFAS STRUCTURE  ZLFAS_OUT
*"----------------------------------------------------------------------


*--> 24.08.2023 17:35:44 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_vendor_vat_p,
        v_input	 TYPE zzfi_outbound_vendor_vat_inp1,
        "v_output TYPE zzfi_outbound_vendor_vat_out1,
        v_item   TYPE zzlfas_out.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-t_lfas-item[].

      LOOP AT t_lfas INTO DATA(w_lfas).
        CLEAR: v_item.
        v_item-lifnr = w_lfas-lifnr.
        v_item-land1 = w_lfas-land1.
        v_item-stceg = w_lfas-stceg.
        v_item-tp_atualizacao = w_lfas-tp_atualizacao.
        v_item-dt_atualizacao = w_lfas-dt_atualizacao.
        v_item-land1_old = w_lfas-land1_old.
        v_item-stceg_old = w_lfas-stceg_old.
        APPEND v_item TO v_input-t_lfas-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_vendor_vat
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 24.08.2023 17:35:44 - Migração S4 – ML – Fim


ENDFUNCTION.

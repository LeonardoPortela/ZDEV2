FUNCTION z_fi_outbound_customer_vat.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_KNAS STRUCTURE  ZKNAS_OUT
*"----------------------------------------------------------------------

*--> 23.08.2023 16:59:57 - Migração S4 – ML - Início

  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_customer_vat,
        v_input	 TYPE zzfi_outbound_customer_vat_i1,
        "v_output TYPE zzfi_outbound_customer_vat_o1,
        v_item   TYPE zzknas_out.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-t_knas-item[].

      LOOP AT t_knas INTO DATA(w_knas).
        CLEAR: v_item.
        v_item-kunnr = w_knas-kunnr.
        v_item-land1 = w_knas-land1.
        v_item-stceg = w_knas-stceg.
        v_item-tp_atualizacao = w_knas-tp_atualizacao.
        v_item-dt_atualizacao = w_knas-dt_atualizacao.
        v_item-land1_old = w_knas-land1_old.
        v_item-stceg_old = w_knas-stceg_old.
        APPEND v_item TO v_input-t_knas-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_customer_vat
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.

*<-- 23.08.2023 16:59:57 - Migração S4 – ML – Fim

ENDFUNCTION.

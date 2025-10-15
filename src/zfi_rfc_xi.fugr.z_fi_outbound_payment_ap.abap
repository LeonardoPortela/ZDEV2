FUNCTION z_fi_outbound_payment_ap .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      OUTPAYMENT STRUCTURE  ZFIE_ITEMS_PAYMENT
*"----------------------------------------------------------------------

*--> 24.08.2023 17:15:10 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_payment_ap_p,
        v_input	 TYPE zzfi_outbound_payment_ap_inp1,
        "v_output TYPE zzfi_outbound_payment_ap_out1,
        v_item   TYPE zzfie_items_payment.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-outpayment-item[].

      LOOP AT outpayment INTO DATA(w_outpayment).
        CLEAR: v_item.
        v_item-obj_key = w_outpayment-obj_key.
        v_item-belnr = w_outpayment-belnr.
        v_item-augdt = w_outpayment-augdt.
        v_item-augbl = w_outpayment-augbl.
        v_item-nebtr = w_outpayment-nebtr.
        v_item-wrbtr = w_outpayment-wrbtr.
        v_item-wskto = w_outpayment-wskto.
        v_item-st_atualizacao = w_outpayment-st_atualizacao.
        v_item-dt_atualizacao = w_outpayment-dt_atualizacao.
        v_item-hr_atualizacao = w_outpayment-hr_atualizacao.
        v_item-cd_transacao = w_outpayment-cd_transacao.
        APPEND v_item TO v_input-outpayment-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_payment_ap
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 24.08.2023 17:15:10 - Migração S4 – ML – Fim

ENDFUNCTION.

FUNCTION z_fi_outbound_reverse_log .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      OUTREVERSE STRUCTURE  ZFIE_REVERSE_DOCUMENT_LOG
*"----------------------------------------------------------------------
*--> 25.08.2023 11:00:27 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_reverse_log,
        v_input	 TYPE zzfi_outbound_reverse_log_in1,
        "v_output TYPE zzfi_outbound_reverse_log_ou1,
        v_item   TYPE zzfie_reverse_document_log.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-outreverse-item[].

      LOOP AT outreverse INTO DATA(w_outreverse).
        CLEAR: v_item.
        v_item-obj_key_estorno = w_outreverse-obj_key_estorno.
        v_item-bukrs = w_outreverse-bukrs.
        v_item-belnr = w_outreverse-belnr.
        v_item-gjahr = w_outreverse-gjahr.
        v_item-docnum = w_outreverse-docnum.
        v_item-dt_atualizacao = w_outreverse-dt_atualizacao.
        v_item-hr_atualizacao = w_outreverse-hr_atualizacao.
        v_item-cd_transacao = w_outreverse-cd_transacao.
        v_item-type = w_outreverse-type.
        v_item-id = w_outreverse-id.
        v_item-num = w_outreverse-num.
        v_item-message = w_outreverse-message.
        v_item-message_v1 = w_outreverse-message_v1.
        v_item-message_v2 = w_outreverse-message_v2.
        v_item-message_v3 = w_outreverse-message_v3.
        v_item-message_v4 = w_outreverse-message_v4.
        APPEND v_item TO v_input-outreverse-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_reverse_log
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 25.08.2023 11:00:27 - Migração S4 – ML – Fim
ENDFUNCTION.

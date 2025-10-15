FUNCTION z_fi_outbound_reverse .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      OUTREVERSE STRUCTURE  ZFIE_REVERSE_DOCUMENT_RET
*"----------------------------------------------------------------------
*--> 24.08.2023 18:46:38 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_reverse_port,
        v_input	 TYPE zzfi_outbound_reverse_input,
        "v_output TYPE zzfi_outbound_reverse_output1,
        v_item   TYPE zzfie_reverse_document_ret.

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
        v_item-bukrs_e = w_outreverse-bukrs_e.
        v_item-belnr_e = w_outreverse-belnr_e.
        v_item-gjahr_e = w_outreverse-gjahr_e.
        v_item-docnum_e = w_outreverse-docnum_e.
        APPEND v_item TO v_input-outreverse-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_reverse
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 24.08.2023 18:46:38 - Migração S4 – ML – Fim
ENDFUNCTION.

FUNCTION z_fi_outbound_change_doc .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      OUTCHANGE STRUCTURE  ZFIE_DOCUMENT_CHANGE
*"----------------------------------------------------------------------

*--> 22.08.2023 18:04:10 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_change_doc_p,
        v_input	 TYPE zzfi_outbound_change_doc_inp1,
        "v_output TYPE zzfi_outbound_change_doc_out1,
        v_item   TYPE zzfie_document_change.


  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-outchange-item[].

      LOOP AT outchange INTO DATA(w_outchange).
        CLEAR: v_item.
        v_item-obj_key = w_outchange-obj_key.
        v_item-belnr = w_outchange-belnr.
        v_item-dt_atualizacao = w_outchange-dt_atualizacao.
        v_item-hr_atualizacao = w_outchange-hr_atualizacao.
        v_item-cd_transacao = w_outchange-cd_transacao.
        v_item-zfbdt = w_outchange-zfbdt.
        v_item-zuonr = w_outchange-zuonr.
        v_item-xblnr = w_outchange-xblnr.
        v_item-bktxt = w_outchange-bktxt.
        v_item-sgtxt = w_outchange-sgtxt.
        APPEND v_item TO v_input-outchange-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_change_doc
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 22.08.2023 18:04:10 - Migração S4 – ML – Fim

ENDFUNCTION.

FUNCTION zsd_outbound_remessa_bloq.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_ZSDT0001_BLOQ STRUCTURE  ZSDT0001_BLOQ
*"----------------------------------------------------------------------
*--> 25.08.2023 16:00:22 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_zsd_outbound_remessa_bloq,
        v_input	 TYPE zzsd_outbound_remessa_bloq_in1,
        "v_output TYPE zzsd_outbound_remessa_bloq_ou1,
        v_item   TYPE zzsdt0001_bloq.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-it_zsdt0001_bloq-item[].

      LOOP AT it_zsdt0001_bloq INTO DATA(w_zsdt0001_bloq).
        CLEAR: v_item.
        v_item-ch_referencia = w_zsdt0001_bloq-ch_referencia.
        v_item-tp_movimento = w_zsdt0001_bloq-tp_movimento.
        v_item-doc_rem = w_zsdt0001_bloq-doc_rem.
        v_item-bloquear = w_zsdt0001_bloq-bloquear.
        v_item-st_cct = w_zsdt0001_bloq-st_cct.
        v_item-ct_aquav = w_zsdt0001_bloq-ct_aquav.
        APPEND v_item TO v_input-it_zsdt0001_bloq-item.
      ENDLOOP.

      CALL METHOD cl_proxy->zsd_outbound_remessa_bloq
        EXPORTING
          input  = v_input.
        "IMPORTING
        "  output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.

  COMMIT WORK.
*<-- 25.08.2023 16:00:22 - Migração S4 – ML – Fim
ENDFUNCTION.

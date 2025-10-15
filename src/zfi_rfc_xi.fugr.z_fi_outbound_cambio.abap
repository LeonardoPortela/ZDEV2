FUNCTION z_fi_outbound_cambio.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_TCURR STRUCTURE  ZMME_TCURR
*"----------------------------------------------------------------------
*--> 25.08.2023 16:40:09 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_cambio_port,
        v_input	 TYPE zzfi_outbound_cambio_input,
        "v_output TYPE zzfi_outbound_cambio_output,
        v_item   TYPE zzmme_tcurr.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-t_tcurr-item[].

      LOOP AT t_tcurr INTO DATA(w_tcurr).
        CLEAR: v_item.
        v_item-kurst = w_tcurr-kurst.
        v_item-fcurr = w_tcurr-fcurr.
        v_item-tcurr = w_tcurr-tcurr.
        v_item-gdatu = w_tcurr-gdatu.
        v_item-ukurs = w_tcurr-ukurs.
        v_item-ffact = w_tcurr-ffact.
        v_item-tfact = w_tcurr-tfact.
        v_item-atividade = w_tcurr-atividade.
        v_item-dt_atualizacao = w_tcurr-dt_atualizacao.
        v_item-hr_atualizacao = w_tcurr-hr_atualizacao.
        APPEND v_item TO v_input-t_tcurr-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_cambio
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 25.08.2023 16:40:09 - Migração S4 – ML – Fim
ENDFUNCTION.

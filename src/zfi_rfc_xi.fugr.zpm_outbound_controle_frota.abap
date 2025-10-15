FUNCTION zpm_outbound_controle_frota.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_CONTROLE_FROTA STRUCTURE  ZPME_RETURN_CONTROLE_FROTA
*"----------------------------------------------------------------------
*--> 25.08.2023 14:45:09 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_zpm_outbound_controle_frot,
        v_input	 TYPE zzpm_outbound_controle_frota_2,
        "v_output TYPE zzpm_outbound_controle_frota_1,
        v_item   TYPE zzpme_return_controle_frota.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-t_controle_frota-item[].

      CLEAR: v_input.

      LOOP AT t_controle_frota INTO DATA(w_controle_frota).
        CLEAR: v_item.
        v_item-no_oservico = w_controle_frota-no_oservico.
        v_item-fg_origem = w_controle_frota-fg_origem.
        v_item-fg_tipo = w_controle_frota-fg_tipo.
        v_item-cd_clasmanut = w_controle_frota-cd_clasmanut.
        v_item-cd_equipto = w_controle_frota-cd_equipto.
        v_item-cd_ccusto = w_controle_frota-cd_ccusto.
        v_item-dt_entrada = w_controle_frota-dt_entrada.
        v_item-unidade_adm = w_controle_frota-unidade_adm.
        v_item-cd_funcionario = w_controle_frota-cd_funcionario.
        v_item-cd_operacao = w_controle_frota-cd_operacao.
        v_item-dt_inicio = w_controle_frota-dt_inicio.
        v_item-qt_hr_total = w_controle_frota-qt_hr_total.
        APPEND v_item TO v_input-t_controle_frota-item.
      ENDLOOP.

      CALL METHOD cl_proxy->zpm_outbound_controle_frota
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 25.08.2023 14:45:09 - Migração S4 – ML – Fim
ENDFUNCTION.

FUNCTION z_fi_outbound_pagamento_item.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_RETORNO_RETENCAO_ITEM STRUCTURE  ZRETENCAO_ITEM
*"----------------------------------------------------------------------
*--> 25.08.2023 15:33:36 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_pagamento_it,
        v_input	 TYPE zzfi_outbound_pagamento_item3,
        "v_output TYPE zzfi_outbound_pagamento_item2,
        v_item   TYPE zzretencao_item.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-t_retorno_retencao_item-item[].

      LOOP AT t_retorno_retencao_item INTO DATA(w_retorno_retencao_item).
        CLEAR: v_item.
        v_item-obj_key = w_retorno_retencao_item-obj_key.
        v_item-wi_tax_code = w_retorno_retencao_item-wi_tax_code.
        v_item-wt_withcd = w_retorno_retencao_item-wt_withcd.
        v_item-wi_tax_base = w_retorno_retencao_item-wi_tax_base.
        v_item-wi_tax_amt = w_retorno_retencao_item-wi_tax_amt.
        v_item-nr_ordem_pago = w_retorno_retencao_item-nr_ordem_pago.
        v_item-zdt_atlz = w_retorno_retencao_item-zdt_atlz.
        v_item-zhr_atlz = w_retorno_retencao_item-zhr_atlz.
        v_item-zrg_atlz = w_retorno_retencao_item-zrg_atlz.
        v_item-zcertificado = w_retorno_retencao_item-zcertificado.
        APPEND v_item TO v_input-t_retorno_retencao_item-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_pagamento_item
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 25.08.2023 15:33:36 - Migração S4 – ML – Fim
ENDFUNCTION.

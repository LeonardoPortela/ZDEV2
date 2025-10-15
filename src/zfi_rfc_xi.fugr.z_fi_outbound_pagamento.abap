FUNCTION z_fi_outbound_pagamento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_RETORNO_RETENCAO STRUCTURE  ZRETENCAO
*"----------------------------------------------------------------------
*--> 25.08.2023 15:25:43 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_fi_outbound_pagamento_po,
        v_input	 TYPE zzfi_outbound_pagamento_inpu1,
        "v_output TYPE zzfi_outbound_pagamento_outp1,
        v_item   TYPE zzretencao.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      REFRESH: v_input-t_retorno_retencao-item[].

      LOOP AT t_retorno_retencao INTO DATA(w_retorno_retencao).
        CLEAR: v_item.
        v_item-obj_key = w_retorno_retencao-obj_key.
        v_item-bukrs = w_retorno_retencao-bukrs.
        v_item-lifnr = w_retorno_retencao-lifnr.
        v_item-augdt = w_retorno_retencao-augdt.
        v_item-augbl = w_retorno_retencao-augbl.
        v_item-waers = w_retorno_retencao-waers.
        v_item-dmbtr = w_retorno_retencao-dmbtr.
        v_item-zdt_atlz = w_retorno_retencao-zdt_atlz.
        v_item-zhr_atlz = w_retorno_retencao-zhr_atlz.
        v_item-zrg_atlz = w_retorno_retencao-zrg_atlz.
        v_item-id_corredor = w_retorno_retencao-id_corredor.
        v_item-dt_comp = w_retorno_retencao-dt_comp.
        APPEND v_item TO v_input-t_retorno_retencao-item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_fi_outbound_pagamento
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 25.08.2023 15:25:43 - Migração S4 – ML – Fim
ENDFUNCTION.

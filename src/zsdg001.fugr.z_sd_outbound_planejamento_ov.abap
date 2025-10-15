FUNCTION z_sd_outbound_planejamento_ov.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(VBELN) TYPE  VBEP-VBELN OPTIONAL
*"     VALUE(POSNR) TYPE  VBEP-POSNR OPTIONAL
*"     VALUE(EDATU) TYPE  VBEP-EDATU OPTIONAL
*"     VALUE(BMENG) TYPE  VBEP-BMENG OPTIONAL
*"     VALUE(STATUS) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------
*--> 25.08.2023 11:39:58 - Migração S4 – ML - Início
  DATA: cl_proxy TYPE REF TO zco_z_sd_outbound_planejamento,
        v_input	 TYPE zzsd_outbound_planejamento_o1.
       "v_output TYPE zzsd_outbound_planejamento_ov.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      CLEAR: v_input.
      v_input-vbeln = vbeln.
      v_input-posnr = posnr.
      v_input-edatu = edatu.
      v_input-bmeng = bmeng.
      v_input-status = status.

      CALL METHOD cl_proxy->z_sd_outbound_planejamento_ov
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault INTO DATA(lv_fault).
      DATA(lv_msg) = lv_fault->errortext.
  ENDTRY.
*<-- 25.08.2023 11:39:58 - Migração S4 – ML – Fim
ENDFUNCTION.

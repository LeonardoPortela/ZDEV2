FUNCTION z_sd_outbound_sales.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      T_ZSALES_SIACORP STRUCTURE  ZSALES_SIACORP
*"----------------------------------------------------------------------

*  perform : zseleciona_dados_sales,
*            f_saida_sales .
*
*  refresh: t_zsales_siacorp.
*
*  loop at it_sales_siacorp into wa_sales_siacorp.
*    append wa_sales_siacorp to t_zsales_siacorp.
*  endloop.

* ---> S4 Migration - 28/08/2023 - JGP - Inicio
  DATA:
    cl_proxy TYPE REF TO zco_z_sd_outbound_sales_port_t,
    v_input  TYPE zzsd_outbound_sales_input,
    "v_output TYPE zzsd_outbound_sales_output,
    w_item   TYPE zzsales_siacorp.

  TRY.
      IF cl_proxy IS NOT BOUND.
        CREATE OBJECT cl_proxy.
      ENDIF.

      LOOP AT t_zsales_siacorp INTO DATA(w_zsales_siacorp).
        w_item-codclientesap         = w_zsales_siacorp-codclientesap.
        w_item-codmes                = w_zsales_siacorp-codmes.
        w_item-codano                = w_zsales_siacorp-codano.
        w_item-valvendas_moeda_int   = w_zsales_siacorp-valvendas_moeda_int.
        w_item-dt_referencia         = w_zsales_siacorp-dt_referencia.
        w_item-valvendas_moeda_forte = w_zsales_siacorp-valvendas_moeda_forte.
        w_item-st_adto               = w_zsales_siacorp-st_adto.
        w_item-empresa               = w_zsales_siacorp-empresa.
        APPEND w_item TO v_input-t_zsales_siacorp-item.
        CLEAR w_item.
      ENDLOOP.

      CALL METHOD cl_proxy->z_sd_outbound_sales
        EXPORTING
          input  = v_input.
        "IMPORTING
          "output = v_output.

    CATCH cx_ai_system_fault.
  ENDTRY.
* <--- S4 Migration - 28/08/2023 - JGP - Fim
ENDFUNCTION.

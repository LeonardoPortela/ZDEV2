FUNCTION ZFI_VIP_EXCHANGE_RATES.
*"----------------------------------------------------------------------
*"*"Interface local:
*"----------------------------------------------------------------------

  DATA(OBJ_VIP) = NEW ZCL_INTEGRA_VIP( ).

  OBJ_VIP->SET_EXCHANGE_RATES( ).

ENDFUNCTION.

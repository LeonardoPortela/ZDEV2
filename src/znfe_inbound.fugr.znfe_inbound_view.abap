FUNCTION ZNFE_INBOUND_VIEW.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_NOTA) TYPE REF TO  ZCL_NFE_INBOUND
*"  RAISING
*"      ZCX_NFE_INBOUND_EXCEPTION
*"      ZCX_CADASTRO
*"----------------------------------------------------------------------

  CREATE OBJECT OBJ_NFE_INBOUND.

  WA_NFE_INBOUND = I_NOTA->GET_INFO_NOTA( ).

  "Chamar Tela

ENDFUNCTION.

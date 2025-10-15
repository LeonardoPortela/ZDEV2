FUNCTION ZNFE_INBOUND_MIGO_MIRO_CANC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_AVISO_RECEBIMENTO) TYPE  VBELN_VL
*"  EXPORTING
*"     REFERENCE(R_CANCELOU) TYPE  CHAR01
*"  RAISING
*"      ZCX_PEDIDO_COMPRA_EXCEPTION
*"      ZCX_CADASTRO
*"      ZCX_NFE_INBOUND_EXCEPTION
*"      ZCX_MIRO_EXCEPTION
*"----------------------------------------------------------------------

  R_CANCELOU = ZCL_NFE_INBOUND=>NFE_INBOUND_CAN_MIGO_MIRO( I_VBELN = I_AVISO_RECEBIMENTO ).

ENDFUNCTION.

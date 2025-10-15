FUNCTION zfi_vip_counterparties.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LFA1) TYPE  LFA1 OPTIONAL
*"     REFERENCE(I_KNA1) TYPE  KNA1 OPTIONAL
*"     REFERENCE(I_CREATION) TYPE  CHAR1 OPTIONAL
*"----------------------------------------------------------------------

  DATA(obj_vip) = NEW zcl_integra_vip( ).

  IF ( i_creation = abap_true ).
    EXIT.
  ENDIF.

  IF ( i_lfa1 IS NOT INITIAL ).
    " Mestre de fornecedores (parte geral)
    obj_vip->set_counterparties( EXPORTING i_lfa1 = i_lfa1 i_creation = i_creation ).
  ELSE.
    " Mestre de clientes (parte geral)
    obj_vip->set_counterparties( EXPORTING i_kna1 = i_kna1 i_creation = i_creation ).
  ENDIF.



ENDFUNCTION.

class ZCL_IM_IWO1_ORDER_BADI definition
  public
  final
  create public .

public section.

  interfaces IF_EX_IWO1_ORDER_BADI .
protected section.
private section.
ENDCLASS.



CLASS ZCL_IM_IWO1_ORDER_BADI IMPLEMENTATION.


  method IF_EX_IWO1_ORDER_BADI~AUDISP_FOR_REFURB_ORDER.

     break rfreitas.

  endmethod.


  method IF_EX_IWO1_ORDER_BADI~AUTHORITY_CHECK_AUART_ACTIVIT.
  endmethod.


  METHOD if_ex_iwo1_order_badi~change_costrelevncy.

    BREAK rfreitas.

    IF sy-subrc IS INITIAL.
      BREAK rfreitas.
    ENDIF.

  ENDMETHOD.


  method IF_EX_IWO1_ORDER_BADI~CONFIRMATIONDATE_CHANGEABLE.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~CONTRACT_PRICE_GET.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~CREATE_BANF_FOR_IND_STOCK.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~INVEST_ORDER_XRAIST01_ALTER.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~NO_CONTRACT_DATA_GET.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~NO_RES_FOR_IND_STOCK.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~ORDER_SCHEDULE.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~REFERENCE_ORDER_CHK.

   break rfreitas.

  endmethod.


  method IF_EX_IWO1_ORDER_BADI~RKPF_MODIFY.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~UPDATE_COMPONENT_ADDRESS.
  endmethod.


  method IF_EX_IWO1_ORDER_BADI~UPDATE_SUBOPERATION_ADDRESS.
  endmethod.
ENDCLASS.

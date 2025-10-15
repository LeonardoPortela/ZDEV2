*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2000 OUTPUT.

  SET PF-STATUS '2000'.
  SET TITLEBAR '2000'.

  go_inbound_nfse->set_payment( iv_payment = zspayment_data_nfse_inbound-pymt_meth ).
  go_inbound_nfse->set_vencimento( iv_vencimento = zspayment_data_nfse_inbound-dt_vencimento ).
  go_inbound_nfse->set_bloqueio( iv_bloqueio = zspayment_data_nfse_inbound-zlspr ).
  go_inbound_nfse->set_boleto( iv_boleto = zspayment_data_nfse_inbound-boleto ).
  go_inbound_nfse->set_observacao( iv_observacao = zspayment_data_nfse_inbound-obs_financeira ).

  zsheader_data_nfse_inbound  = go_inbound_nfse->get_header_data( ).
  zspayment_data_nfse_inbound = go_inbound_nfse->get_payment_data( ).

  IF zspayment_data_nfse_inbound-pymt_meth IS NOT INITIAL.
    SELECT SINGLE text1 INTO t042z-text1
      FROM t042z
     WHERE land1 EQ 'BR'
      AND  zlsch EQ zspayment_data_nfse_inbound-pymt_meth.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_COND_LIST_CHANGE_TC_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_cond_list_change_tc_attr OUTPUT.
  DESCRIBE TABLE gt_scr_condition_list LINES tc_condition_list-lines.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_POPUP_LIST_ASSIGN_ATTR  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_popup_list_assign_attr OUTPUT.

  IF go_purchase_order IS BOUND.
    go_purchase_order->get_searched_po( IMPORTING
                                          et_searched_po    = gt_scr_popup_search ).

    go_purchase_order->get_associated_po( IMPORTING
                                           et_associated_po = gt_scr_popup_list ).
  ENDIF.

  DESCRIBE TABLE gt_scr_popup_list   LINES tc_popup_list-lines.
  DESCRIBE TABLE gt_scr_popup_search LINES tc_popup_search-lines.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CONTROL_SCR_POPUP_LIST  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE control_scr_popup_list OUTPUT.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_POPUP_LIST_SET_INPUT_FIELD  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_popup_list_set_input_field OUTPUT.

  IF gw_scr_popup_list-checkbox EQ 'X'.
    IF gw_scr_popup_list-menge_iv EQ 0.
      SET CURSOR FIELD 'GW_SCR_POPUP_LIST-MENGE_IV' LINE tc_popup_list-current_line.
    ENDIF.
    LOOP AT SCREEN.
      IF screen-name = 'GW_SCR_POPUP_LIST-MENGE_IV'.
        screen-input = 1.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  TC_POPUP_LIST_GET_LINES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tc_popup_list_get_lines OUTPUT.
  gv_tc_popup_list_lines = sy-loopc.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_9050  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_3000 OUTPUT.
  SET PF-STATUS '3000'.
  SET TITLEBAR '3000'.
ENDMODULE.

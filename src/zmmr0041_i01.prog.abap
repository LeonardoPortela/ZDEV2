*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  CASE sy-ucomm.
    WHEN 'SAVE'.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'DOWN_PDF'.
    WHEN 'DOWN_XML'.
    WHEN 'ML81N'.
    WHEN 'MIGO'.
    WHEN 'MIRO'.
    WHEN 'EVENT'.
    WHEN 'ESTORNO'.
      CREATE OBJECT go_estorno_document
        EXPORTING
          iv_nfse_data = go_inbound_nfse->get_nfse_data( ).
    WHEN 'ASSOCIARPO'.
      CREATE OBJECT go_purchase_order
        EXPORTING
          iv_nfse_data = go_inbound_nfse->get_nfse_data( ).

      go_purchase_order->get_po( IMPORTING
                                  ev_ebeln = DATA(pedido)
                                  ev_ebelp = DATA(item_pedido) ).

      go_inbound_nfse->set_pedido( EXPORTING
                                    iv_ebeln = pedido
                                    iv_ebelp = item_pedido ).
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECKBOX_SEARCH  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE checkbox_search INPUT.

  IF gw_scr_popup_search-checkbox IS INITIAL.

    CLEAR: gw_scr_popup_search-menge_iv,  "Invoice Verification Qty
           gw_scr_popup_search-dmbtr_iv.  "Calculate Value

  ELSE.
    "Set same Qty of PO
    gw_scr_popup_search-menge_iv = gw_scr_popup_search-menge.
  ENDIF.

  MODIFY gt_scr_popup_search FROM gw_scr_popup_search
                           INDEX tc_popup_search-current_line
                           TRANSPORTING checkbox
                                        menge_iv
                                        dmbtr_iv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECKBOX  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE checkbox INPUT.

  MODIFY gt_scr_popup_list FROM gw_scr_popup_list
                           INDEX tc_popup_list-current_line.

  IF gw_scr_popup_list-menge_iv > gw_scr_popup_list-menge.
    MESSAGE 'Quantidade preenchida é maior que a disponível no Pedido de Compras' TYPE 'E'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_3000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_3000 INPUT.

  DATA: lt_ebeln TYPE ace_generic_range_t.

  lt_ebeln[] = CORRESPONDING #( s_ebeln3[] ).

  CASE ok_code_9050.

    WHEN 'SEARCH'.

      go_purchase_order->search_po( EXPORTING
                                      iv_ebeln = lt_ebeln[] ).

      REFRESH: gt_scr_popup_search.
      REFRESH CONTROL: 'TC_POPUP_SEARCH' FROM SCREEN sy-dynnr.

    WHEN 'OK'.

      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).
      go_purchase_order->apply_po_asssociation( ).

      CLEAR: ok_code_9050.
      LEAVE TO SCREEN 0 .

    WHEN 'CANCEL'.

      CLEAR: ok_code_9050.
      LEAVE TO SCREEN 0 .

    WHEN 'ASSIGN'.

      go_purchase_order->set_searched_po(   it_searched_po   = gt_scr_popup_search ).
      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).
      go_purchase_order->associate_po( ).

      REFRESH: gt_scr_popup_list.
      REFRESH CONTROL: 'TC_POPUP_LIST' FROM SCREEN sy-dynnr.

    WHEN 'UNASSIGN'.

      DELETE gt_scr_popup_list WHERE checkbox = abap_true.
      go_purchase_order->set_searched_po(   it_searched_po   = gt_scr_popup_search ).
      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).

    WHEN 'SEARCH_SAL'.

      LOOP AT gt_scr_popup_search ASSIGNING FIELD-SYMBOL(<popup_search>).
        <popup_search>-checkbox = abap_true.
      ENDLOOP.

      go_purchase_order->set_searched_po(   it_searched_po   = gt_scr_popup_search ).
      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).

    WHEN 'SEARCH_DAL'.

      LOOP AT gt_scr_popup_search ASSIGNING <popup_search>.
        CLEAR: <popup_search>-checkbox.
      ENDLOOP.

      go_purchase_order->set_searched_po(   it_searched_po   = gt_scr_popup_search ).
      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).

    WHEN 'ASSIGN_SAL'.

      LOOP AT gt_scr_popup_list ASSIGNING FIELD-SYMBOL(<popup_list>).
        <popup_list>-checkbox = abap_true.
      ENDLOOP.

      go_purchase_order->set_searched_po(   it_searched_po   = gt_scr_popup_search ).
      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).

    WHEN 'ASSIGN_DAL'.

      LOOP AT gt_scr_popup_list ASSIGNING <popup_list>.
        CLEAR: <popup_list>-checkbox.
      ENDLOOP.

      go_purchase_order->set_searched_po(   it_searched_po   = gt_scr_popup_search ).
      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).

    WHEN 'ENTER'.

      go_purchase_order->set_searched_po(   it_searched_po   = gt_scr_popup_search ).
      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).

    WHEN 'CHECKBOX'.

      go_purchase_order->set_searched_po(   it_searched_po   = gt_scr_popup_search ).
      go_purchase_order->set_associated_po( it_associated_po = gt_scr_popup_list ).

  ENDCASE.

  CLEAR: ok_code_9050.

ENDMODULE.

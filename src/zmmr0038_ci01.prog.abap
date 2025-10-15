CLASS lcl_utilities IMPLEMENTATION. "Utilidades para todos as clases.

  METHOD calculate_days_by_datum.
    rv_date = sy-datum - iv_number_of_days.
  ENDMETHOD.

  METHOD remover_zeros_esquerda.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_ouput.

  ENDMETHOD.

  METHOD adicionar_zeros_esquerda.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_input
      IMPORTING
        output = ev_ouput.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_coupa_integration_log IMPLEMENTATION. "Classe responsável por administrar log de integração - Tabela ZINTEGRCOUPA01.

  METHOD insert_new_log_key.

    DATA: ls_zintegrcoupa01 TYPE zintegrcoupa01.

    FREE: ls_zintegrcoupa01.

    ls_zintegrcoupa01-id_integr  = iv_zcoupa_integration_key-id_integr.
    ls_zintegrcoupa01-ident_proc = iv_zcoupa_integration_key-ident_proc.
    ls_zintegrcoupa01-dt_atual   = sy-datum.
    ls_zintegrcoupa01-hr_atual   = sy-uzeit.
    ls_zintegrcoupa01-status     = space.
    APPEND ls_zintegrcoupa01 TO me->gt_zintegrcoupa01.

  ENDMETHOD.

  METHOD set_executed_status.

    READ TABLE me->gt_zintegrcoupa01 ASSIGNING FIELD-SYMBOL(<zintegrcoupa0>) WITH KEY id_integr  = iv_zcoupa_integration_key-id_integr
                                                                                      ident_proc = iv_zcoupa_integration_key-ident_proc.
    IF sy-subrc IS INITIAL.
      <zintegrcoupa0>-status = 'S'. "Status - Integração processa com sucesso.
    ENDIF.

  ENDMETHOD.

  METHOD save_log.

    CHECK me->gt_zintegrcoupa01 IS NOT INITIAL.

    MODIFY zintegrcoupa01 FROM TABLE me->gt_zintegrcoupa01.

    COMMIT WORK.

  ENDMETHOD.

  METHOD check_existence_of_key.

    SELECT COUNT(*)
      FROM zintegrcoupa01
      WHERE id_integr  = iv_zcoupa_integration_key-id_integr
      AND   ident_proc = iv_zcoupa_integration_key-ident_proc
      AND   status     = 'S'.
    IF sy-subrc IS INITIAL.
      rv_existence = 'X'.
      IF s_mblnr IS NOT INITIAL AND sy-batch IS INITIAL.
        MESSAGE i012(z_mm) WITH iv_zcoupa_integration_key-id_integr(10).
      ENDIF.
    ELSE.
      me->insert_new_log_key( iv_zcoupa_integration_key
                                   = iv_zcoupa_integration_key ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_invoice_data_coupa IMPLEMENTATION.

  METHOD constructor.

    IF iv_days IS NOT INITIAL.
      APPEND VALUE #( sign = 'I' option = 'BT' low = sy-datum - iv_days high = sy-datum ) TO me->gt_range_date.
    ELSE.
      APPEND VALUE #( sign = 'I' option = 'BT' low = sy-datum - 1 high = sy-datum ) TO me->gt_range_date.
    ENDIF.

    CREATE OBJECT me->go_coupa_integration_log.

  ENDMETHOD.

  METHOD build_data.

    DATA: ls_integration_key TYPE zcoupa_integration_key.

    DATA: l_item_coupa  TYPE char10,
          l_qtd_service TYPE ekbe-menge.

    SELECT ekbe~bldat, ekbe~menge, ekbe~dmbtr,
           ekbe~shkzg, ekbe~budat, ekbe~ebelp, ekbe~gjahr,
           ekbe~ebeln, ekbe~belnr, ekko~bukrs, ekko~bsart
      FROM ekbe INNER JOIN ekko
      ON ekbe~ebeln = ekko~ebeln
      INTO TABLE @DATA(lt_recibos)
      WHERE ekbe~belnr IN @s_mblnr
      AND   ekbe~vgabe EQ '1'
      AND   ekbe~budat IN @me->gt_range_date
      AND   ekbe~gjahr EQ @sy-datum(4)

      AND   NOT EXISTS ( SELECT * FROM zintegrcoupa01 WHERE zintegrcoupa01~ident_proc = 'FS'
                                                      AND   zintegrcoupa01~fields     = ekbe~lfbnr
                                                      AND   zintegrcoupa01~fields     <> ' ' ) "RECIBO COUPA
      AND EXISTS ( SELECT *
                    FROM  setleaf
                    WHERE setname EQ 'MAGGI_EMPRESAS_COUPA'
                    AND    valfrom = ekko~bukrs
                    AND EXISTS ( SELECT *
                                  FROM setleaf
                                  WHERE setname EQ 'MAGGI_PEDIDOS_COUPA'
                                  AND   valfrom = ekko~bsart ) ).

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zmmt0158
        INTO TABLE @DATA(lt_pedidos_servico)
        FOR ALL ENTRIES IN @lt_recibos
        WHERE ebeln = @lt_recibos-ebeln
        AND   ebelp = @lt_recibos-ebelp.
      IF sy-subrc IS INITIAL.
        SORT lt_pedidos_servico BY ebeln ebelp.
      ENDIF.

      SELECT ebeln, ebelp, netpr
        FROM ekpo
        INTO TABLE @DATA(lt_itens_pedidos)
         FOR ALL ENTRIES IN @lt_recibos
        WHERE ebeln = @lt_recibos-ebeln
        AND   ebelp = @lt_recibos-ebelp.
      IF sy-subrc IS INITIAL.
        SORT lt_itens_pedidos BY ebeln ebelp.
      ENDIF.
    ENDIF.

    LOOP AT lt_recibos INTO DATA(ls_recibos).
      CLEAR: gs_coupa_recibo, ls_integration_key.
      gs_coupa_recibo-custom_fields-id_sap = ls_recibos-belnr && ls_recibos-ebelp.
      CONCATENATE gs_coupa_recibo-custom_fields-id_sap ls_recibos-gjahr INTO ls_integration_key-id_integr.
      ls_integration_key-ident_proc = 'MG'.

      IF me->go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
                                                                   = ls_integration_key ) EQ abap_true.
        CONTINUE.
      ENDIF.

      READ TABLE lt_pedidos_servico INTO DATA(ls_pedidos_servico) WITH KEY ebeln = ls_recibos-ebeln
                                                                           ebelp = ls_recibos-ebelp BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        READ TABLE lt_itens_pedidos INTO DATA(ls_itens_pedidos) WITH KEY ebeln = ls_recibos-ebeln
                                                                         ebelp = ls_recibos-ebelp BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          l_qtd_service            = ( ls_recibos-dmbtr / ls_itens_pedidos-netpr ) * ls_pedidos_servico-menge.
          gs_coupa_recibo-quantity = l_qtd_service.
        ENDIF.
      ELSE.
        "</quantity>
        gs_coupa_recibo-quantity = ls_recibos-menge.
      ENDIF.

      IF ls_recibos-shkzg = 'S'.
        "<type>
        gs_coupa_recibo-type   = 'ReceivingQuantityConsumption'.
        "<status>
        gs_coupa_recibo-status = 'Created'.
      ELSEIF ls_recibos-shkzg = 'H'.
        "<type>
        gs_coupa_recibo-type   = 'VoidReceivingQuantityConsumption'.
        "<status>
        gs_coupa_recibo-status = 'Voided'.
      ENDIF.

      "<total>
      gs_coupa_recibo-total = ls_recibos-dmbtr.
      "<exported>
      gs_coupa_recibo-exported = 'true'.
      "<transaction-date>
*-CS1092037-#RIMINI-05.26.2023-BEGIN
*      CONCATENATE ls_recibos-budat+6(2) ls_recibos-budat+4(2) ls_recibos-budat(4) INTO gs_coupa_recibo-transaction_date SEPARATED BY '-'.
      CONCATENATE ls_recibos-budat+4(2) ls_recibos-budat+6(2) ls_recibos-budat(4) INTO gs_coupa_recibo-transaction_date SEPARATED BY '-'.
*-CS1092037-#RIMINI-05.26.2023-END
      "<order-header> <id>
      gs_coupa_recibo-order_line-order_header_id = zcl_int_coupa_get_pedido=>zif_int_coupa_get_pedido~get_instance( )->get_pedido( EXPORTING
                                                                                                                                    iv_pedido      = ls_recibos-ebeln
                                                                                                                                    iv_item_pedido = ls_recibos-ebelp
                                                                                                                                   IMPORTING
                                                                                                                                   ev_item_coupa   = l_item_coupa ).
      "<order-header> <id> "<order-line> <id>
      gs_coupa_recibo-order_line-id = l_item_coupa.
      "<custom-fields> <id-sap>
      gs_coupa_recibo-custom_fields-id_sap = ls_recibos-belnr && ls_recibos-ebelp.

      gs_coupa_recibo-ebeln = ls_recibos-ebeln.
      gs_coupa_recibo-ebelp = ls_recibos-ebelp.
      gs_coupa_recibo-belnr = ls_recibos-belnr.

*      CONCATENATE gs_coupa_recibo-custom_fields-id_sap ls_recibos-gjahr INTO ls_integration_key-id_integr.
*      ls_integration_key-ident_proc = 'MG'.
*
*      CHECK me->go_coupa_integration_log->check_existence_of_key( iv_zcoupa_integration_key
*                                                                   = ls_integration_key ) EQ abap_false.

      APPEND gs_coupa_recibo TO me->gt_coupa_recibo.
    ENDLOOP.

    FREE: gs_coupa_recibo, ls_integration_key, lt_recibos, ls_recibos.

  ENDMETHOD.

  METHOD execute.
    me->build_data( ).
  ENDMETHOD.

  METHOD get_log_object.
    ro_integration_log = me->go_coupa_integration_log.
  ENDMETHOD.

  METHOD get_import_data.
    rt_import_data = me->gt_coupa_recibo.
  ENDMETHOD.

  METHOD save_recibo_coupa_sap.

    DATA: lt_xml_tab  TYPE srt_xml_data_tab,
          lt_zmmt0155 TYPE STANDARD TABLE OF zmmt0155.

    DATA: lv_string  TYPE string,
          lv_xstring TYPE xstring.

    FIELD-SYMBOLS: <zmmt0155> TYPE zmmt0155.

    APPEND INITIAL LINE TO lt_zmmt0155 ASSIGNING <zmmt0155>.

    <zmmt0155>-ebeln           = iv_import_data-ebeln.
    <zmmt0155>-ebelp           = iv_import_data-ebelp.
    <zmmt0155>-belnr           = iv_import_data-belnr.
    <zmmt0155>-id_pedido_coupa = iv_import_data-order_line-order_header_id.
    <zmmt0155>-id_item_coupa   = iv_import_data-order_line-id.

    lv_string = iv_integration_log-ds_data_retorno.

    CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
      EXPORTING
        text   = lv_string
      IMPORTING
        buffer = lv_xstring
      EXCEPTIONS
        failed = 1
        OTHERS = 2.
    IF sy-subrc <> 0.
    ENDIF.

    CALL FUNCTION 'SRTUTIL_CONVERT_XML_TO_TABLE'
      EXPORTING
        xdoc = lv_xstring
      IMPORTING
        data = lt_xml_tab.

    READ TABLE lt_xml_tab INTO DATA(ls_xml) WITH KEY tag_name  = 'id'
                                                     tag_level = 2.
    IF sy-subrc IS INITIAL.
      <zmmt0155>-id_recibo_coupa = ls_xml-tag_value.
    ENDIF.

    MODIFY zmmt0155 FROM TABLE lt_zmmt0155.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_return IMPLEMENTATION.

  METHOD constructor.

    cl_salv_table=>factory( IMPORTING
                              r_salv_table = go_alv
                            CHANGING
                              t_table      = gt_alv_data ).

    me->config_alv_columns( ).
    me->config_alv_functions( ).
    me->config_alv_events( ).

  ENDMETHOD.

  METHOD append_new_line.

    DATA: ls_alv TYPE zcoupa_import_data_alv.

    ls_alv-id_referencia  = iv_import_data-id_integr.
    ls_alv-tp_referencia  = iv_import_data-ident_proc.
    ls_alv-xml_icon       = '@0U@'.
    ls_alv-xml             = iv_integration_log-ds_data_retorno.

    IF iv_integration_log-nm_code NE 200 AND
       iv_integration_log-nm_code NE 201 AND
       iv_integration_log-nm_code NE 202.
      ls_alv-status = '@02@'.
    ELSE.
      ls_alv-status = '@01@'.
    ENDIF.

    APPEND ls_alv TO me->gt_alv_data.

  ENDMETHOD.

  METHOD config_alv_columns.

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'XML' ).
    go_column->set_visible( abap_false ).

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'XML_ICON' ).
    go_column->set_icon( abap_true ).
    go_column->set_cell_type( EXPORTING
                                value = if_salv_c_cell_type=>hotspot ).
    go_column->set_short_text( 'XML').

    go_columns = go_alv->get_columns( ).
    go_column ?= go_columns->get_column( 'STATUS' ).
    go_column->set_icon( abap_true ).
    go_column->set_short_text( 'Status').

  ENDMETHOD.

  METHOD config_alv_functions.

    go_funct = go_alv->get_functions( ).
    go_funct->set_all( abap_true ).

  ENDMETHOD.

  METHOD config_alv_events.

    go_events = go_alv->get_event( ).
    SET HANDLER go_alv_return->on_link_click FOR go_events.

  ENDMETHOD.

  METHOD display.

    go_alv->display( ).

  ENDMETHOD.

  METHOD on_link_click.

    READ TABLE gt_alv_data INTO DATA(ls_alv_data) INDEX row.
    IF sy-subrc IS INITIAL.
      CALL METHOD cl_abap_browser=>show_xml
        EXPORTING
          xml_string = ls_alv_data-xml.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

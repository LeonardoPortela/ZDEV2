*&---------------------------------------------------------------------*
*&  Include           ZPPR001MDL
*&---------------------------------------------------------------------*
CLASS cx_local_exception DEFINITION
                         INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    METHODS get_text REDEFINITION.

    METHODS constructor
      IMPORTING text TYPE itex132.

    METHODS message
      IMPORTING type    TYPE c
                display TYPE c OPTIONAL.

    DATA text TYPE itex132.
ENDCLASS.

CLASS services DEFINITION.
  PUBLIC SECTION.

    METHODS planning_production
      RETURNING VALUE(r_reference) TYPE REF TO zcl_pp_services.

  PRIVATE SECTION.
    DATA mo_planning_production TYPE REF TO zcl_pp_services.
ENDCLASS.

CLASS services IMPLEMENTATION.
  METHOD planning_production.
    IF ( mo_planning_production IS NOT BOUND ).
      mo_planning_production = NEW zcl_pp_services( ).
    ENDIF.

    MOVE mo_planning_production TO r_reference.
  ENDMETHOD.
ENDCLASS.

CLASS cx_local_exception IMPLEMENTATION.
  METHOD constructor.
    CALL METHOD super->constructor.
    me->text = text.
  ENDMETHOD.

  METHOD get_text.
    MOVE me->text TO result.
  ENDMETHOD.

  METHOD message.
    MESSAGE me->get_text( ) TYPE type DISPLAY LIKE display.
  ENDMETHOD.
ENDCLASS.

CLASS cl_utils DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS display_messages
      IMPORTING table TYPE bapiret2_t.

    CLASS-METHODS convert_date_external
      IMPORTING date         TYPE sy-datum
      RETURNING VALUE(value) TYPE string.

    CLASS-METHODS build_fieldcatalog
      IMPORTING
        fieldname   TYPE lvc_fname
        description TYPE scrtext_l  OPTIONAL
        optimize    TYPE lvc_colopt OPTIONAL
        hotspot     TYPE lvc_hotspt OPTIONAL
        no_zero     TYPE lvc_nozero OPTIONAL
        outputlen   TYPE lvc_outlen OPTIONAL
        icon        TYPE lvc_icon   OPTIONAL
        edit        TYPE lvc_edit   OPTIONAL
        ref_table   TYPE lvc_rtname OPTIONAL
        ref_field   TYPE lvc_rfname OPTIONAL
        tabname     TYPE lvc_tname  OPTIONAL
        style       TYPE lvc_style  OPTIONAL
        f4          TYPE ddf4avail  OPTIONAL
        just        TYPE c          OPTIONAL
        sum         TYPE abap_bool  OPTIONAL
        quantity    TYPE char3      OPTIONAL
      CHANGING
        r_table     TYPE lvc_t_fcat.

    CLASS-METHODS message_process_indicator
      IMPORTING
        percentage TYPE any
        message    TYPE itex132.

*    CLASS-METHODS GET_ERROR_MESSAGES
*      RETURNING VALUE(TABLE) TYPE ZFITRS0002.

*    CLASS-METHODS REFRESH_ERROR_MESSAGES.

  PRIVATE SECTION.
    CLASS-DATA:
      mw_order_header_x TYPE bapisdh1x,
      mw_return         TYPE bapiret2,
*      MESSAGES          TYPE TABLE OF ZFITRS0002,
      mt_return         TYPE TABLE OF bapiret2.

    TYPES ty_vbpa TYPE STANDARD TABLE OF vbpa WITH KEY vbeln.
ENDCLASS.

CLASS cl_utils IMPLEMENTATION.
  METHOD display_messages.
    CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
      EXPORTING
        it_message = table.
  ENDMETHOD.

  METHOD convert_date_external.
    TRY.
        cl_abap_datfm=>conv_date_int_to_ext(
          EXPORTING
            im_datint = date
          IMPORTING
            ex_datext = value ).
      CATCH cx_abap_datfm_format_unknown.
    ENDTRY.
  ENDMETHOD.

  METHOD message_process_indicator.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = percentage
        text       = message.
  ENDMETHOD.

  METHOD build_fieldcatalog.
    DATA(fcat) = VALUE lvc_s_fcat(
                       fieldname  = fieldname
                       scrtext_l  = description
                       scrtext_m  = description
                       scrtext_s  = description
                       reptext    = description
                       coltext    = description
                       col_opt    = optimize
                       hotspot    = hotspot
                       no_zero    = no_zero
                       outputlen  = outputlen
                       do_sum     = sum
                       quantity   = quantity
                       icon       = icon
                       edit       = edit
                       style      = style
                       ref_table  = ref_table
                       ref_field  = ref_field
                       tabname    = tabname
                       f4availabl = f4
                       just       = just
                                ).
    APPEND fcat TO r_table.
  ENDMETHOD.
ENDCLASS.

CLASS main_model DEFINITION
                 INHERITING FROM zsapmvc_model.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS select_authorized_orders
      EXCEPTIONS auth_not_found.

    METHODS select_all_orders
      RAISING cx_local_exception.

    METHODS select_orders_planning.
    METHODS select_orders_header.
    METHODS select_orders_item.

    METHODS replanning_shipments_transit
      IMPORTING
        ordem TYPE vbeln
        item  TYPE posnr.

    METHODS refresh_nodes.

    METHODS set_hierarchy_orders
      CHANGING alv_tree TYPE REF TO cl_gui_alv_tree.

    METHODS set_top_nodes_key
      IMPORTING node_key TYPE lvc_nkey.

    METHODS get_order_header
      RETURNING VALUE(r_table) TYPE vbak_t.

    METHODS get_top_nodes_key
      RETURNING VALUE(table) TYPE lvc_t_nkey.

    METHODS get_authorized_orders
      RETURNING VALUE(r_table) TYPE zsdct001.

    METHODS get_order_items
      RETURNING VALUE(r_table) TYPE vbap_t.

    METHODS get_order_plannings
      RETURNING VALUE(r_table) TYPE zppct001.

    METHODS get_value_ranges
      IMPORTING i_name         TYPE rsscr_name
      RETURNING VALUE(r_table) TYPE rsis_t_range.

    METHODS get_total_quantity_ov
      IMPORTING vbeln        TYPE vbeln
                posnr        TYPE posnr
                items        TYPE zppct001
                oauth        TYPE zsdct001
      RETURNING VALUE(saldo) TYPE wmeng.

  PRIVATE SECTION.
    DATA services        TYPE REF TO services.
    DATA product_revenda TYPE mara-mtart VALUE 'ZHAW'.

    "// Internal tables
    DATA mt_selopts   TYPE STANDARD TABLE OF rsparams WITH KEY selname.
    DATA mt_ordens    TYPE TABLE OF ty_ordens.
    DATA mt_zsdt0082  TYPE TABLE OF zsdt0082.
    DATA mt_zsdt0001od  TYPE TABLE OF zsdt0001od.
    DATA order_items  TYPE TABLE OF vbap.
    DATA order_divisions TYPE TABLE OF vbep.
    DATA mt_zppt0008  TYPE TABLE OF zppt0008.
    DATA mt_vbak      TYPE TABLE OF vbak.
    DATA mt_afpo      TYPE TABLE OF afpo.
    DATA mt_nodes     TYPE lvc_t_nkey.

    "// Range of selection screen
    DATA mr_werks     TYPE rsis_t_range.
    DATA mr_vbeln     TYPE rsis_t_range.
    DATA mr_dtlib     TYPE rsis_t_range.
    DATA mr_matnr     TYPE rsis_t_range.
    DATA mr_kunnr     TYPE rsis_t_range.
    DATA mr_status    TYPE rsis_t_range.
    DATA mr_spart     TYPE rsis_t_range.
ENDCLASS.

CLASS main_model IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    services = NEW services( ).

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = sy-repid
      TABLES
        selection_table = me->mt_selopts
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.

    DELETE me->mt_selopts WHERE option IS INITIAL.
    CHECK ( sy-subrc IS INITIAL ).

    me->mr_werks = me->get_value_ranges( 'S_WERKS' ).
    me->mr_vbeln = me->get_value_ranges( 'S_VBELN' ).
    me->mr_dtlib = me->get_value_ranges( 'S_DTLIB' ).
    me->mr_matnr = me->get_value_ranges( 'S_MATNR' ).
    me->mr_kunnr = me->get_value_ranges( 'S_KUNNR' ).

    me->mr_status =
        VALUE #( ( sign = 'I' option = 'EQ' low = '2' ) "//Liberado
                 ( sign = 'I' option = 'EQ' low = '5' ) "//Planejado-Produzido
               ).

*    ME->MR_SPART =
*        VALUE #( ( SIGN = 'I' OPTION = 'EQ' LOW = '02' )"//Fertilizantes
*               ).
  ENDMETHOD.

  METHOD select_authorized_orders.
*    IF ( SY-MANDT NE '060' ).

    SELECT *
      FROM zsdt0082  AS a
     INNER JOIN vbak AS b ON a~vbeln = b~vbeln
     INNER JOIN vbap AS c ON a~vbeln = c~vbeln
                         AND a~posnr = c~posnr
     INNER JOIN mara AS d ON c~matnr = d~matnr
      INTO CORRESPONDING FIELDS OF TABLE me->mt_zsdt0082
     WHERE a~werks    IN me->mr_werks
       AND a~vbeln    IN me->mr_vbeln
       AND a~dt_liber IN me->mr_dtlib
       AND a~status   IN me->mr_status
       AND a~spart    IN me->mr_spart
       AND b~kunnr    IN me->mr_kunnr
       AND c~matnr    IN me->mr_matnr
       AND d~mtart    NE me->product_revenda.

    IF NOT ( sy-subrc IS INITIAL ).
      RAISE auth_not_found.
    ENDIF.
*
*    ELSE.
*      SELECT *
*        FROM ZSDT0082
*        INTO TABLE ME->MT_ZSDT0082
*       WHERE SPART IN ME->MR_SPART.
*    ENDIF.

    SORT me->mt_zsdt0082 BY vbeln posnr seq_lib qte_lib.
  ENDMETHOD.

  METHOD select_orders_header.
    SELECT *
      FROM vbak
      INTO TABLE me->mt_vbak
   FOR ALL ENTRIES IN me->mt_zsdt0082
     WHERE vbeln = me->mt_zsdt0082-vbeln.
  ENDMETHOD.

  METHOD select_orders_item.
    SELECT *
      FROM vbap AS a
      INTO CORRESPONDING FIELDS OF TABLE me->order_items
   FOR ALL ENTRIES IN me->mt_zsdt0082
     WHERE a~vbeln = me->mt_zsdt0082-vbeln
       AND a~posnr = me->mt_zsdt0082-posnr.
  ENDMETHOD.

  METHOD select_orders_planning.
    SELECT *
      FROM zppt0008
      INTO TABLE me->mt_zppt0008
    FOR ALL ENTRIES IN me->order_items
     WHERE vbeln =  me->order_items-vbeln
       AND posnr =  me->order_items-posnr.
  ENDMETHOD.

  METHOD select_all_orders.
    CALL METHOD me->select_authorized_orders
      EXCEPTIONS
        auth_not_found = 4.

    IF ( sy-subrc IS INITIAL ).
      me->select_orders_header( ).
      me->select_orders_item( ).
      me->select_orders_planning( ).
    ELSE.
      RAISE EXCEPTION TYPE cx_local_exception
        EXPORTING
          text = TEXT-001.
    ENDIF.
  ENDMETHOD.

  "// Set hierarchy OV
  METHOD set_hierarchy_orders.
    DATA(_authorized) = me->get_authorized_orders( ).
    DATA(_plannings)  = me->get_order_plannings( ).
    DATA(_header)     = me->get_order_header( ).
    DATA(_items)      = me->get_order_items( ).

    SORT _items BY vbeln posnr.
    me->refresh_nodes( ).

    LOOP AT _items INTO DATA(_item).
      me->services->planning_production( )->select_customer(
        EXPORTING
          kunnr = _header[ vbeln = _item-vbeln ]-kunnr
        IMPORTING
          data  = DATA(_customer) ).

      me->services->planning_production( )->select_shipments(
        EXPORTING
          vbeln = _item-vbeln
          posnr = _item-posnr
        IMPORTING
          data  = DATA(_shipments)
        EXCEPTIONS
          data_not_found = 4 ).

      me->services->planning_production( )->select_shipments_transit(
        EXPORTING
          vbeln = _item-vbeln
          posnr = _item-posnr
        IMPORTING
          data  = DATA(_transits)
        EXCEPTIONS
          data_not_found = 4 ).

      DATA(_total_authorized) = REDUCE wmeng(
                                 INIT x = 0 FOR _auth IN _authorized
                                WHERE ( vbeln = _item-vbeln
                                  AND   posnr = _item-posnr )
                                 NEXT x = x + _auth-qte_lib ).

      DATA(_total_planned)   = REDUCE wmeng(
                                 INIT x = 0 FOR _planning IN _plannings
                                WHERE ( vbeln EQ _item-vbeln
                                  AND   posnr EQ _item-posnr  )
                                 NEXT x = x + _planning-wmeng ).

      DATA(_total_faturado)  = REDUCE wmeng(
                                 INIT x = 0 FOR _shipment IN _shipments
                                 NEXT x = x + COND #( LET y = _shipment-wemng IN WHEN  y IS NOT INITIAL
                                                     THEN y ELSE 0 ) ).

      DATA(_total_released)  = REDUCE wmeng(
                                 INIT x = 0 FOR _shipment IN _shipments
                                 NEXT x = x + COND #( WHEN _shipment-wemng IS INITIAL
                                 THEN _shipment-psmng ELSE 0 ) ).

      DATA(_total_transit)   = REDUCE wmeng(
                                 INIT x = 0 FOR _transit IN _transits
                                 NEXT x = x + _transit-quantidade ).

      "// Set values in fields to tree
      "// The description of fields are in the type definition
      DATA(outtab_lines) =
          VALUE ty_ordens( ordem            = _item-vbeln
                           item             = _item-posnr
                           centro           = _item-werks
                           emissor          = |{ _customer-kunnr } - { _customer-name1 }|
                           material         = |{ _item-matnr } - { _item-arktx }|
                           qtd_ordem_venda  = _total_authorized
                           qtd_disponivel   = _total_authorized - _total_planned
                           qtd_planejada    = _total_planned
                           qtd_carregada    = _total_faturado + _total_released
                           qtd_faturada     = _total_faturado
                           qtd_em_andamento = _total_released
                           qtd_em_transito  = _total_transit
      ).
*--> Inicio - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
      IF outtab_lines-qtd_disponivel < 0.
        outtab_lines-qtd_disponivel = outtab_lines-qtd_disponivel * -1.
      ENDIF.
*<-- Fim - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã

      SHIFT outtab_lines-material LEFT DELETING LEADING '0'.
      SHIFT outtab_lines-emissor  LEFT DELETING LEADING '0'.

      "// Sets higher tree
      AT NEW vbeln.
        APPEND sy-tabix TO nodes.

        DATA(layout_node) = VALUE lvc_s_layn(
                                  isfolder  = abap_false
                                  n_image   = icon_order
                                  exp_image = icon_order
                                  style     = cl_gui_column_tree=>style_intensified ).

        CALL METHOD alv_tree->add_node
          EXPORTING
            i_relat_node_key = space
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = CONV #( _item-vbeln )
            is_node_layout   = layout_node
          IMPORTING
            e_new_node_key   = DATA(item_key).

        me->set_top_nodes_key( item_key ).
      ENDAT.

      DATA(item_layout) = VALUE lvc_t_layi( ( fieldname = 'QTD_ORDEM_VENDA'
                                              style     = COND #( WHEN outtab_lines-qtd_ordem_venda  > 0
                                                                  THEN cl_gui_column_tree=>style_emphasized_positive
                                                                  ELSE cl_gui_column_tree=>style_emphasized_negative ) )
                                            ( fieldname = 'QTD_DISPONIVEL'
                                              style     = COND #( WHEN outtab_lines-qtd_disponivel  > 0
                                                                  THEN cl_gui_column_tree=>style_emphasized_positive
                                                                  ELSE cl_gui_column_tree=>style_emphasized_negative ) )
                                            ( fieldname = 'QTD_PLANEJADA'
                                              style     = COND #( WHEN ( outtab_lines-qtd_planejada > 0 )
                                                                  THEN cl_gui_column_tree=>style_emphasized_positive
                                                                  ELSE cl_gui_column_tree=>style_emphasized_negative ) )
                                            ( fieldname = 'QTD_FATURADA'
                                              style     = COND #( WHEN ( outtab_lines-qtd_faturada > 0 )
                                                                  THEN cl_gui_column_tree=>style_emphasized_positive
                                                                  ELSE cl_gui_column_tree=>style_emphasized_negative ) )
                                            ( fieldname = 'QTD_EM_ANDAMENTO'
                                              style     = COND #( WHEN ( outtab_lines-qtd_em_andamento > 0 )
                                                                  THEN cl_gui_column_tree=>style_emphasized_positive
                                                                  ELSE cl_gui_column_tree=>style_emphasized_negative ) )
                                            ( fieldname = 'QTD_EM_TRANSITO'
                                              style     = COND #( WHEN ( outtab_lines-qtd_em_transito > 0 )
                                                                  THEN cl_gui_column_tree=>style_emphasized_positive
                                                                  ELSE cl_gui_column_tree=>style_emphasized_negative ) )
                                          ).
      "// Sets lower tree
      CALL METHOD alv_tree->add_node
        EXPORTING
          i_relat_node_key = item_key
          i_relationship   = cl_gui_column_tree=>relat_last_child
          i_node_text      = CONV #( _item-posnr )
*         is_node_layout   = ls_node_layout
          is_outtab_line   = outtab_lines
          it_item_layout   = item_layout
        IMPORTING
          e_new_node_key   = DATA(last_key).
    ENDLOOP.
  ENDMETHOD.

  METHOD set_top_nodes_key.
    APPEND node_key TO me->mt_nodes.
  ENDMETHOD.

  "// Get OV header informations
  METHOD get_order_header.
    MOVE me->mt_vbak TO r_table.
  ENDMETHOD.

  METHOD get_top_nodes_key.
    MOVE me->mt_nodes TO table.
  ENDMETHOD.

  METHOD refresh_nodes.
    REFRESH me->mt_nodes.
  ENDMETHOD.

  "// Get OV quantity authorized
  METHOD get_authorized_orders.
    MOVE me->mt_zsdt0082 TO r_table.
  ENDMETHOD.

  "// Get OV items
  METHOD get_order_items.
    MOVE me->order_items TO r_table.
  ENDMETHOD.

  "// Get OV plannings
  METHOD get_order_plannings.
    MOVE me->mt_zppt0008 TO r_table.
  ENDMETHOD.

*  METHOD GET_PRD_ORDERS.
*    MOVE ME->MT_AFPO TO R_TABLE.
*  ENDMETHOD.

  METHOD get_value_ranges.
    r_table = VALUE #(
                FOR ls_selopts IN me->mt_selopts WHERE ( selname = i_name )
                  ( sign   = ls_selopts-sign
                    option = ls_selopts-option
                    low    = ls_selopts-low
                    high   = ls_selopts-high
                   ) ).
  ENDMETHOD.

  "// --------------------------------------------------------
  "// PARAMETERS | DESCRIPTION:
  "// vbeln        Order number.
  "// posnr        Order item.
  "// items        All shippings already made for an order.
  "// oauth        All authorized balance for an order/item.
  "// --------------------------------------------------------
  METHOD get_total_quantity_ov.

    "// Sum of all quantity of balances and subtracting by all of shipping divisions
    "// to get the remaining balance.
    saldo = REDUCE wmeng( INIT x = 0
                           FOR ls_oauth IN oauth WHERE ( vbeln = vbeln AND
                                                         posnr = posnr )

                          NEXT x = x + ls_oauth-qte_lib - REDUCE conv(
                          INIT y = 0
                           FOR ls_items IN items
                          NEXT y = y + ls_items-wmeng ) ).
  ENDMETHOD.

  METHOD replanning_shipments_transit.
    SELECT *
      FROM zppt0009
      INTO TABLE @DATA(_shipments_transit)
     WHERE vbeln EQ @ordem
       AND posnr EQ @item
       AND data  < @sy-datum
       AND status NE @abap_true.

    LOOP AT _shipments_transit ASSIGNING FIELD-SYMBOL(<shipment_transit>).
      <shipment_transit>-data = sy-datum.
    ENDLOOP.

    MODIFY zppt0009 FROM TABLE _shipments_transit.
    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.

CLASS shipments_model DEFINITION
                      INHERITING FROM zsapmvc_model.
  PUBLIC SECTION.
    METHODS constructor.

*    METHODS select_shipments
*      IMPORTING
*        edatu TYPE edatu
*        vbeln TYPE vbeln
*        posnr TYPE posnr
*      RAISING
*        cx_local_exception.

    METHODS print
      IMPORTING
        selected_rows TYPE lvc_t_row
        print_type    TYPE string.

    METHODS storno
      IMPORTING
        selected_rows TYPE lvc_t_row
        storno_type   TYPE string.

    METHODS storno_interface
      IMPORTING
        order_carregamento TYPE zpped003
      EXPORTING
        sucess             TYPE c.

    METHODS confirm
      IMPORTING order_carregamento TYPE zpped003
      RETURNING VALUE(return)      TYPE char1.

    METHODS print_tag
      IMPORTING
        ordem_carregamento TYPE aufk-ordemcarreg
        print_type         TYPE string.

    METHODS print_process_order
      IMPORTING
        ordem_carregamento TYPE aufk-ordemcarreg.

    METHODS enviar_supervisorio
      IMPORTING
        selected_rows TYPE lvc_t_row.

    METHODS authorize_entry
      IMPORTING
        selected_rows TYPE lvc_t_row.

    METHODS set_shipments
      IMPORTING
        new      TYPE c OPTIONAL
        transits TYPE ty_t_shipments_transit OPTIONAL
        items    TYPE ANY TABLE.

    METHODS get_shipments
      RETURNING VALUE(r_table) TYPE ty_t_shipments.

    METHODS set_ordem_opus
      IMPORTING
        ordem TYPE zde_nr_ordem.

    METHODS get_ordem_opus
      RETURNING VALUE(r_table) TYPE zsdt0001od_t.

  PRIVATE SECTION.
    DATA services TYPE REF TO services.
    DATA print_is_open TYPE abap_bool.
ENDCLASS.

CLASS shipments_model IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).
    services = NEW services( ).
  ENDMETHOD.

  METHOD print.
    DATA(_items) = me->get_shipments( ).

    MOVE abap_on TO dialog_is_open.

    IF ( selected_rows IS INITIAL ).
      MESSAGE TEXT-027 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      LOOP AT selected_rows INTO DATA(_row).
        ASSIGN _items[ _row-index ] TO FIELD-SYMBOL(<fs_item>).

        IF ( <fs_item>-txt04 NE 'TRAN' ).
          CASE print_type.
            WHEN 'ORDEM_PRODUCAO'.
              me->print_process_order( <fs_item>-ordemcarreg ).
            WHEN 'ETIQUETA' OR 'ETIQUETA1' OR 'ETIQUETA2'.

              me->print_tag( ordem_carregamento = <fs_item>-ordemcarreg
                             print_type         = print_type ).
          ENDCASE.
        ELSE.
          MESSAGE s008(zppm001) WITH <fs_item>-ordemcarreg DISPLAY LIKE 'W'.
        ENDIF.
      ENDLOOP.
    ENDIF.

    MOVE abap_off TO dialog_is_open.
  ENDMETHOD.

  METHOD storno.
    DATA: answer    TYPE string,
          lv_sucess TYPE c,
          lv_aufnr  TYPE aufnr.
    DATA: item_division_remes TYPE TABLE OF vbep.


    DATA(_items) = me->get_shipments( ).

    IF ( selected_rows IS INITIAL ).
      MESSAGE TEXT-027 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      LOOP AT selected_rows INTO DATA(_row).
        ASSIGN _items[ _row-index ] TO FIELD-SYMBOL(<fs_item>).

        DATA(_external_order_carregamento) = <fs_item>-ordemcarreg.

        CHECK me->confirm( _external_order_carregamento ) EQ 1.

        me->storno_interface( EXPORTING order_carregamento = _external_order_carregamento
                              IMPORTING sucess             = lv_sucess ).

        IF lv_sucess IS NOT INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = <fs_item>-aufnr
            IMPORTING
              output = lv_aufnr.

          TRY .
              zcl_int_ob_desativa_ordem_supe=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = lv_aufnr ).
            CATCH zcx_integracao INTO DATA(zcx_integracao).
              MESSAGE ID zcx_integracao->zif_error~msgid TYPE 'I'
                   NUMBER zcx_integracao->zif_error~msgno
                     WITH zcx_integracao->zif_error~msgv1
                          zcx_integracao->zif_error~msgv2
                          zcx_integracao->zif_error~msgv3
                          zcx_integracao->zif_error~msgv4.
            CATCH zcx_error INTO DATA(zcx_error).
              MESSAGE ID zcx_error->zif_error~msgid TYPE 'I'
               NUMBER zcx_error->zif_error~msgno
                 WITH zcx_error->zif_error~msgv1
                      zcx_error->zif_error~msgv2
                      zcx_error->zif_error~msgv3
                      zcx_error->zif_error~msgv4.
          ENDTRY.

        ENDIF.

*--> Inicio - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
***** Ajuste para atualização da tabela VBEP onde é recuperado o valor da Quantidade da ordem
        IF gw_order IS NOT INITIAL.
* Recupera dados da ordem na tabela VBEP
          me->services->planning_production( )->select_ov_divisions(
            EXPORTING
              vbeln = gw_order-ordem
              posnr = gw_order-item
            IMPORTING
              items = item_division_remes
          ).

          SORT item_division_remes BY edatu.

          DATA(lv_lines) = lines( item_division_remes ).

          LOOP AT item_division_remes ASSIGNING FIELD-SYMBOL(<fs_item_division_remes>).

            IF <fs_item_division_remes>-edatu = <fs_item>-erdat.
              IF <fs_item_division_remes>-wmeng IS NOT INITIAL AND <fs_item_division_remes>-wmeng < <fs_item>-psmng.
*** Exibir a mensagem informando que o valor estornado é maior
                MESSAGE 'Favor verificar com Insumos o Saldo da divisão de remessa na respectiva da data!' TYPE 'S' DISPLAY LIKE 'E'.

              ELSEIF <fs_item_division_remes>-wmeng >= <fs_item>-psmng.
* Subtrair o valor do estorno da linha da tabela da ordem
                <fs_item_division_remes>-wmeng = <fs_item_division_remes>-wmeng - <fs_item>-psmng.

              ENDIF.
            ENDIF.

* Somar o valor do estorno na ultima linha da tabela da ordem
            IF sy-tabix = lv_lines.
              <fs_item_division_remes>-wmeng = <fs_item_division_remes>-wmeng + <fs_item>-psmng.

            ENDIF.
          ENDLOOP.

          MODIFY vbep FROM TABLE item_division_remes.
          COMMIT WORK.

        ENDIF.
*<-- Fim - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã

*&================Comentado novamente devido erro em PRD / linha 728 até 828 / Bug Solto 148003& / AOENNING. / 07/03/2025.
*        DATA(_EXTERNAL_ORDER_NUMBER) = <FS_ITEM>-AUFNR.
*        SHIFT _EXTERNAL_ORDER_NUMBER LEFT DELETING LEADING '0'.
*
*        DATA(_STATUS) =
*         ME->SERVICES->PLANNING_PRODUCTION( )->GET_TEXT_STATUS( SWITCH #( STORNO_TYPE WHEN 'RELEASE_PO' THEN 'AUTZ' ELSE 'ENT' ) ).
*
*        CASE STORNO_TYPE.
*          WHEN 'AUTHORIZATION_ENTRY'.
*
*            CASE <FS_ITEM>-TXT04.
*              WHEN 'TRAN' OR 'AUTZ' OR 'ENT'.
*
*                CALL FUNCTION 'POPUP_TO_CONFIRM'
*                  EXPORTING
*                    TITLEBAR              = 'Estornar Ordem de Carregamento'
*                    TEXT_QUESTION         = |Tem certeza que deseja estornar a Ordem de Carregamento { <FS_ITEM>-ORDEMCARREG } ?|
*                    TEXT_BUTTON_1         = 'Sim'
*                    ICON_BUTTON_1         = 'ICON_OKAY'
*                    TEXT_BUTTON_2         = 'Não'
*                    ICON_BUTTON_2         = 'ICON_CANCEL'
*                    DISPLAY_CANCEL_BUTTON = ''
*                  IMPORTING
*                    ANSWER                = ANSWER.
*
*                CHECK ANSWER = 1. "//Yes
*
*                IF <FS_ITEM>-ORDEMCARREG IS NOT INITIAL.
*
*                  ME->SERVICES->PLANNING_PRODUCTION( )->SELECT_SHIPMENT(
*                    EXPORTING
*                      ORDEM_CARREGAMENTO = <FS_ITEM>-ORDEMCARREG
*                    EXCEPTIONS
*                      SHIPMENT_NOT_FOUND = 4 ).
*
*                  ME->SERVICES->PLANNING_PRODUCTION( )->PROCESS_ORDER_IS_LOCKED(
*                     ME->SERVICES->PLANNING_PRODUCTION( )->GET_SHIPMENT( )-AUFNR ).
*
*                  ME->SET_ORDEM_OPUS( CONV #( <FS_ITEM>-ORDEMCARREG ) ).
*                  IF ME->GET_ORDEM_OPUS( ) IS INITIAL.
*                    ME->SERVICES->PLANNING_PRODUCTION( )->STORNO_PROCESS_ORDER(
*                                 ORDEM_CARREGAMENTO = <FS_ITEM>-ORDEMCARREG ).
*                  ENDIF.
*                ENDIF.
*
*                TRY.
*                    ME->SERVICES->PLANNING_PRODUCTION( )->CHANGE_PO_USER_STATUS(
*                      EXPORTING
*                       ORDER  = <FS_ITEM>-AUFNR
*                       STATUS = _STATUS-TXT04
*                   ).
*
*                    <FS_ITEM>-TXT04    = _STATUS-TXT04.
*                    <FS_ITEM>-TXT30    = _STATUS-TXT30.
*                    <FS_ITEM>-ROWCOLOR = SPACE.
*
*                    MESSAGE S016(ZPPM001) WITH <FS_ITEM>-AUFNR _STATUS-TXT30.
*
*                  CATCH ZCX_PP_SERVICES.
*                    CL_UTILS=>DISPLAY_MESSAGES( ME->SERVICES->PLANNING_PRODUCTION( )->GET_MESSAGES( ) ).
*                ENDTRY.
*
*              WHEN OTHERS.
*                MESSAGE S035(ZPPM001) WITH 'em Transito ou Autorização de Entrada' DISPLAY LIKE 'W'.
*            ENDCASE.
*
*          WHEN 'RELEASE_PO'.
*            IF <FS_ITEM>-TXT04 = 'PROD'.
*              CALL FUNCTION 'POPUP_TO_CONFIRM'
*                EXPORTING
*                  TITLEBAR              = 'Estornar 1º Pesagem'
*                  TEXT_QUESTION         = |Tem certeza que deseja estornar a 1º pesagem da ordem { _EXTERNAL_ORDER_NUMBER } ?|
*                  TEXT_BUTTON_1         = 'Sim'
*                  ICON_BUTTON_1         = 'ICON_OKAY'
*                  TEXT_BUTTON_2         = 'Não'
*                  ICON_BUTTON_2         = 'ICON_CANCEL'
*                  DISPLAY_CANCEL_BUTTON = ''
*                IMPORTING
*                  ANSWER                = ANSWER.
*
*              CHECK ANSWER = 1. "//Yes
*
*              TRY.
*                  ME->SERVICES->PLANNING_PRODUCTION( )->CHANGE_PO_USER_STATUS(
*                    EXPORTING
*                     ORDER  = <FS_ITEM>-AUFNR
*                     STATUS = _STATUS-TXT04
*                 ).
*
*                  <FS_ITEM>-TXT04    = _STATUS-TXT04.
*                  <FS_ITEM>-TXT30    = _STATUS-TXT30.
*                  <FS_ITEM>-ROWCOLOR = COL_YELLOW_INT.
*
*                  MESSAGE S016(ZPPM001) WITH <FS_ITEM>-AUFNR _STATUS-TXT30.
*
*                CATCH ZCX_PP_SERVICES.
*                  CL_UTILS=>DISPLAY_MESSAGES( ME->SERVICES->PLANNING_PRODUCTION( )->GET_MESSAGES( ) ).
*              ENDTRY.
*            ELSE.
*              MESSAGE S018(ZPPM001) WITH 'Em Produção' DISPLAY LIKE 'W'.
*            ENDIF.
*        ENDCASE.
*&================Retono do bloco comentado da linha 728 até 828 / Bug Solto 148003& / AOENNING.
      ENDLOOP.
    ENDIF.

    me->set_shipments( items = _items ).
  ENDMETHOD.

  METHOD storno_interface.

    CONSTANTS tabname_value TYPE string VALUE 'errordetails'.

    DATA: return_code TYPE i,
          e_resultado TYPE string,
          code        TYPE i,
          reason      TYPE string,
          host        TYPE string,
          port        TYPE string,
          result      TYPE TABLE OF string.

    cl_http_server=>if_http_server~get_location( IMPORTING host = host  port = port ).

    port = '8000'.

    DATA(url) = |http://{ host }:{ port }/planning_production/ProcessOrder('{ order_carregamento }')|.

    "//Call service
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = CONV #( url )
      IMPORTING
        client             = DATA(http_client)
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        OTHERS             = 4.

    http_client->request->set_header_field( name = '~request_method'  value = 'DELETE' ).
    http_client->request->set_header_field( name = '~server_protocol' value = 'HTTP/1.0').
    http_client->request->set_header_field( name = 'X-Requested-With' value = 'X').
    http_client->request->set_header_field( name = 'content-type'     value = 'application/xml; charset=UTF-8').
    http_client->send( ).
    http_client->receive( ).
    http_client->response->get_status( IMPORTING code = code reason = reason ).

    DATA(xml_return) = NEW cl_xml_document( ).

    xml_return->parse_string( stream = http_client->response->get_cdata( ) ).
    http_client->response->get_status( IMPORTING code = return_code ).
    e_resultado = http_client->response->get_cdata( ).

    xml_return->find_node_table(
      EXPORTING
        tabname = tabname_value
      IMPORTING
        t_nodes = DATA(_nodes)
    ).

    LOOP AT _nodes INTO DATA(_node).
      APPEND CAST if_ixml_node( _node-node )->get_value( ) TO result.
    ENDLOOP.

    TRY.
        DATA(_code)    = CAST if_ixml_node( _nodes[ 1 ]-node )->get_value( ).
        DATA(_message) = CAST if_ixml_node( _nodes[ 2 ]-node )->get_value( ).
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.

    CALL METHOD http_client->close
      EXCEPTIONS
        http_invalid_state = 1
        OTHERS             = 2.

    IF _nodes IS NOT INITIAL.
      IF _message IS NOT INITIAL.
        MESSAGE |{ _message }| TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        MESSAGE 'Erro desconhecido' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.
    ELSE.
      MESSAGE |Ordem { order_carregamento } estornada com Sucesso! | TYPE 'S'.

      sucess = abap_true.

    ENDIF.

  ENDMETHOD.

  METHOD confirm.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar              = 'Estornar Ordem de Carregamento'
        text_question         = |Tem certeza que deseja estornar a Ordem de Carregamento { order_carregamento } ?|
        text_button_1         = 'Sim'
        icon_button_1         = 'ICON_OKAY'
        text_button_2         = 'Não'
        icon_button_2         = 'ICON_CANCEL'
        display_cancel_button = ''
      IMPORTING
        answer                = return.

  ENDMETHOD.

  METHOD print_tag.

    DATA order_form_name TYPE rs38l_fnam.
    DATA return          TYPE c.
    DATA etiqueta_personalizada TYPE string.
    DATA smart TYPE tdsfname.

    etiqueta_personalizada = COND #( WHEN print_type EQ 'ETIQUETA' THEN abap_true ELSE abap_false ).
    smart = COND #( WHEN print_type EQ 'ETIQUETA2' THEN 'ZPPF0002' ELSE 'ZPPSF_ETIQUETA' ).

    DATA(_fields) =
      VALUE scmg_tt_sval( ( tabname   = 'ZPPET002'
                            fieldname = 'QTD_ETIQUETA' ) ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Informe a quantidade de etiquetas'
        start_column    = 90
        start_row       = 10
      IMPORTING
        returncode      = return
      TABLES
        fields          = _fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    CHECK _fields[ 1 ]-value > 0
      AND return IS INITIAL.

    DATA(_options_etiqueta) =
      VALUE ssfcompop(
                       tddest    = COND #( WHEN print_type EQ 'ETIQUETA2' THEN 'LOCL' ELSE 'LOCP' )
                       tdnoprint = abap_false
                       tdimmed   = abap_true
                       tdnewid   = abap_true
                       tdnoarch  = abap_true
                       tdcopies  = COND #( WHEN print_type EQ 'ETIQUETA2' THEN _fields[ 1 ]-value ) ).

    DATA(_control_etiqueta) =
      VALUE ssfctrlop( device    = 'PRINTER'
                       preview   = abap_false
                       no_dialog = COND #( WHEN print_type EQ 'ETIQUETA2' THEN abap_false ELSE abap_true ) ).

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = smart
      IMPORTING
        fm_name  = order_form_name.

    CALL FUNCTION order_form_name
      EXPORTING
        control_parameters     = _control_etiqueta
        output_options         = _options_etiqueta
        user_settings          = abap_false
        ordem_carregamento     = ordem_carregamento
        qtd_etiquetas          = CONV numc5( _fields[ 1 ]-value )
        etiqueta_personalizada = etiqueta_personalizada
      EXCEPTIONS
        formatting_error       = 1
        internal_error         = 2
        send_error             = 3
        user_canceled          = 4
        OTHERS                 = 5.

  ENDMETHOD.

  METHOD print_process_order.
    DATA order_form_name TYPE rs38l_fnam.

    DATA(_options_ordem) =
      VALUE ssfcompop( tddest    = 'LOCL'
                       tdnoprint = abap_false
                       tdimmed   = abap_true
                       tdnewid   = abap_true
                       tdnoarch  = abap_true ).

    DATA(_control_ordem) =
      VALUE ssfctrlop( device    = 'PRINTER'
                       preview   = abap_true
                       no_dialog = abap_false ).

    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname = 'ZPPSF_ORDEM_PRODUCAO_V1'
      IMPORTING
        fm_name  = order_form_name.

    CALL FUNCTION order_form_name
      EXPORTING
        control_parameters = _control_ordem
        output_options     = _options_ordem
        user_settings      = abap_true
        ordem_carregamento = ordem_carregamento
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
  ENDMETHOD.

  METHOD enviar_supervisorio.

    DATA: le_ordem     TYPE zppe_supervisorio.

    DATA(lt_itens) = me->get_shipments( ).

    SELECT *
      FROM zppt0038
      FOR ALL ENTRIES IN @lt_itens
      WHERE ordem = @lt_itens-aufnr
      INTO TABLE @DATA(lt_0038).
    IF sy-subrc IS INITIAL.
      SORT lt_0038 BY ordem.

      READ TABLE lt_0038 ASSIGNING FIELD-SYMBOL(<fs_0038>) INDEX 1.
      IF sy-subrc IS INITIAL.
        IF <fs_0038>-status = 'SINCRONIZADO'.
          MESSAGE 'Não é possível enviar ao supervisório, ordem já sincronizada!' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.
    ELSE.
      EXIT.
    ENDIF.

    LOOP AT selected_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
      ASSIGN lt_itens[ <fs_rows>-index ] TO FIELD-SYMBOL(<fs_item>).

      READ TABLE lt_0038 ASSIGNING <fs_0038>
      WITH KEY ordem = <fs_item>-aufnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING <fs_0038> TO le_ordem.

        TRY .
            zcl_int_ob_cria_ordem_supervis=>zif_integracao_outbound~get_instance( )->execute_request( i_info_request = le_ordem ).
          CATCH zcx_integracao INTO DATA(zcx_integracao).

          CATCH zcx_error INTO DATA(zcx_error).

        ENDTRY.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD authorize_entry.
    DATA(_items) = me->get_shipments( ).

    IF ( selected_rows IS INITIAL ).
      MESSAGE TEXT-027 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.

      DATA(_text_status) =
        me->services->planning_production( )->get_text_status( 'AUTZ' ).

      LOOP AT selected_rows INTO DATA(_row).
        ASSIGN _items[ _row-index ] TO FIELD-SYMBOL(<fs_item>).

        CASE <fs_item>-txt04.
          WHEN 'ENT'.
            TRY.
                me->services->planning_production(
                    )->change_po_user_status( order  = <fs_item>-aufnr
                                              status = _text_status-txt04
                                            ).

                <fs_item>-txt04    = _text_status-txt04.
                <fs_item>-txt30    = _text_status-txt30.
                <fs_item>-rowcolor = col_yellow_int.

                MESSAGE s002(zppm001) WITH <fs_item>-ordemcarreg.

              CATCH zcx_pp_services.
                cl_utils=>display_messages( me->services->planning_production( )->get_messages( ) ).
            ENDTRY.

          WHEN 'TRAN'.
            MESSAGE s003(zppm001) WITH <fs_item>-ordemcarreg DISPLAY LIKE 'W'.
          WHEN OTHERS.
            MESSAGE s004(zppm001) WITH <fs_item>-ordemcarreg DISPLAY LIKE 'W'.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    me->set_shipments( _items ).
  ENDMETHOD.

  METHOD set_shipments.
    DATA item TYPE zppet001.

    IF ( new EQ abap_on ).
      CLEAR gt_shipments.

      "//Set shipments loadeds
      LOOP AT items INTO item.
        CHECK ( item-txt04 <> 'ESTN' ).
        CHECK ( item-txt04 <> 'PSVA' ).

        APPEND VALUE
            ty_shipments(
                aufnr = item-aufnr
                ordemcarreg = item-ordemcarreg
                placa = item-placa
                psmng = COND #( WHEN item-txt04 = 'CPRD' AND
                                     item-wemng IS NOT INITIAL THEN item-wemng ELSE item-psmng )
*--> Inicio - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
                erdat = item-erdat
*<-- Fim - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
                txt04 = item-txt04
                txt30 = item-txt30
                rowcolor = SWITCH #( item-txt04
                             WHEN 'PESA' THEN col_violett
                             WHEN 'AUTZ' THEN col_yellow_int
                             WHEN 'PROD' THEN col_violett_int
                             WHEN 'CPRD' THEN col_green_int )
                        ) TO gt_shipments.

      ENDLOOP.

      "//Set shipments in traffic
      LOOP AT transits INTO DATA(_transit).
        APPEND VALUE
            ty_shipments(
                ordemcarreg = _transit-ordem_carreg
                placa       = _transit-placa
                psmng       = _transit-quantidade
                txt04       = 'TRAN'
                txt30       = 'Em trânsito'
                rowcolor    = col_lightgrey_int
                        ) TO gt_shipments.
      ENDLOOP.

    ELSE.
      MOVE items TO gt_shipments.
    ENDIF.
  ENDMETHOD.

  METHOD get_shipments.
    MOVE gt_shipments TO r_table.
  ENDMETHOD.

  METHOD set_ordem_opus.
    DATA: r_werks TYPE RANGE OF werks_d.

    DATA: it_values  TYPE TABLE OF rgsb4.

    DATA: wa_values TYPE rgsb4,
          wa_werks  LIKE LINE OF r_werks.

    REFRESH it_values.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = 'MV45AFZZ_WERKS'
        table         = 'VBAP'
        class         = '0000'
        fieldname     = 'WERKS'
      TABLES
        set_values    = it_values
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc IS INITIAL.

      LOOP AT it_values INTO wa_values.
        wa_werks = 'IEQ'.
        wa_werks-low    = wa_values-from.
        IF wa_values-to IS NOT INITIAL.
          wa_werks = 'IBT'.
          wa_werks-high = wa_values-to.
        ENDIF.

        APPEND wa_werks TO r_werks.
      ENDLOOP.
    ENDIF.

    SELECT *
      FROM zsdt0001od AS a
      INTO CORRESPONDING FIELDS OF TABLE gt_ordem_opus
     WHERE a~nr_ordem = ordem
      AND id_branch IN r_werks
      AND tp_status NE 'CA'.
  ENDMETHOD.

  METHOD get_ordem_opus.
    MOVE gt_ordem_opus TO r_table.
  ENDMETHOD.

ENDCLASS.

CLASS display_model DEFINITION
                    INHERITING FROM zsapmvc_model.
  PUBLIC SECTION.
    "//Methods
    METHODS constructor.

    METHODS set_header_remessa
      IMPORTING line TYPE ty_ordens.

    METHODS set_planning
      IMPORTING
        items TYPE ANY TABLE
        new   TYPE abap_bool OPTIONAL.

    METHODS set_planning_deleted
      IMPORTING
        item TYPE ty_planning.

    METHODS set_solicitations
      IMPORTING
        items TYPE zsdct001.

    METHODS set_items_before_edit
      IMPORTING items TYPE ty_t_planning.

    METHODS set_row_reference
      IMPORTING
        row_reference TYPE sy-tabix.

    METHODS get_plannings
      RETURNING VALUE(r_table) TYPE ty_t_planning.

    METHODS get_items_before_edit
      RETURNING VALUE(r_table) TYPE ty_t_planning.

    METHODS get_plannings_deleted
      RETURNING VALUE(r_table) TYPE ty_t_planning.

    METHODS get_header_remessa
      RETURNING VALUE(r_table) TYPE REF TO object.

    METHODS get_planning
      RETURNING VALUE(table) TYPE zppt0008.

    METHODS get_row_reference
      RETURNING VALUE(r_value) TYPE sy-tabix.

    METHODS get_solicitations
      RETURNING VALUE(table) TYPE ty_t_solicitation.

    METHODS get_trip_guide
      IMPORTING
        no_solicitation TYPE zsdt0082-nro_sol
        order           TYPE zsdt0082-vbeln
        item            TYPE zsdt0082-posnr
      RETURNING
        VALUE(table)    TYPE catsxt_longtext_itab.

    METHODS if_planning_was_saved
      IMPORTING
        items TYPE ty_t_planning
      EXCEPTIONS
        records_were_not_saved.

    METHODS if_item_was_changed
      IMPORTING
        old_item TYPE any
        new_item TYPE any
      EXCEPTIONS
        record_was_changed.

    METHODS insert_planning
      IMPORTING
        items TYPE ty_planning.

    METHODS modify_planning
      IMPORTING
        item TYPE ty_planning.

    METHODS modify_plannings
      IMPORTING
        items TYPE ty_t_planning.

    METHODS update_planning
      IMPORTING
        parameter TYPE string
        vbeln     TYPE vbeln
        posnr     TYPE posnr
        edatu     TYPE edatu.

    METHODS new_planning.

    METHODS send_planning_to_opus
      IMPORTING
        selected_row TYPE lvc_t_row.

    METHODS update_planning_row
      IMPORTING
        line_changed TYPE lvc_t_modi
        order_items  TYPE vbap_t.

    METHODS info_error_message.

    METHODS save_planning
      IMPORTING
        solicitations TYPE zsdct001
      EXCEPTIONS
        incomplete_data.

    METHODS modify_planning_row
      IMPORTING
        selected_row TYPE lvc_t_row.

    METHODS delete_planning_row
      IMPORTING
        selected_row TYPE lvc_t_row.

    METHODS refresh_items_fragments.
    METHODS clear_data_display.

    METHODS selected_dados_material IMPORTING i_matnr TYPE matnr EXPORTING e_mara TYPE mara.
    METHODS check_material_fert_producao IMPORTING i_mara          TYPE mara
                                                   i_werks         TYPE werks_d
                                         EXPORTING e_fert_producao TYPE char01.



    "//Attributes
    DATA services TYPE REF TO services.
    DATA action   TYPE sy-ucomm.

  PROTECTED SECTION.
    "// Set Conditions to View Notifications
    METHODS view_notification REDEFINITION.

    DATA solicitation_released TYPE c VALUE '2'.
    DATA solicitation_planned  TYPE c VALUE '5'.

  PRIVATE SECTION.
    DATA dd_document    TYPE REF TO cl_dd_document.
    DATA mt_vbep        TYPE TABLE OF vbep.
    DATA process_orders TYPE TABLE OF aufk.
    DATA items_deleted  TYPE TABLE OF ty_planning.
    DATA plannings      TYPE TABLE OF zppt0008.
    DATA solicitations  TYPE TABLE OF ty_solicitation.
    DATA deleted_row    TYPE TABLE OF string.
    DATA order_header   TYPE vbak.
    DATA order_item     TYPE vbap.
    DATA solicitation   TYPE zsdt0082.
    DATA sched_lines    TYPE oij_bapischdl_t.
    DATA sched_linesx   TYPE oij_bapischdlx_t.
    DATA old_items      TYPE ty_t_planning.
    DATA mw_zppt0008    TYPE zppt0008.
    DATA num_fragments  TYPE numc10.
    DATA row_reference  TYPE sy-tabix.
ENDCLASS.

CLASS display_model IMPLEMENTATION.
  METHOD constructor.

    super->constructor( ).
    services = NEW services( ).
  ENDMETHOD.

  METHOD set_header_remessa.

*    DATA TABLE_ELEMENT  TYPE REF TO CL_DD_TABLE_ELEMENT.
*    DATA TABLE_TEXT     TYPE SDYDO_TEXT_TABLE.
*    DATA COLUMN         TYPE REF TO CL_DD_AREA.
*
*    DATA(_LINE) = LINE.
*
*    IF ( ME->DD_DOCUMENT IS NOT BOUND ).
*      CREATE OBJECT ME->DD_DOCUMENT.
*    ELSE.
*      ME->DD_DOCUMENT->INITIALIZE_DOCUMENT( ).
*    ENDIF.
*
*    "//Build title text
*    ME->DD_DOCUMENT->ADD_TEXT( TEXT      = 'Divisões de Remessa'
*                               SAP_STYLE = CL_DD_AREA=>HEADING ).
*
*    ME->DD_DOCUMENT->NEW_LINE( 1 ).
*
*    SHIFT _LINE-VBELN LEFT DELETING LEADING '0'.
*    SHIFT _LINE-POSNR LEFT DELETING LEADING '0'.
*    SHIFT _LINE-KUNNR LEFT DELETING LEADING '0'.
*    SHIFT _LINE-MATNR LEFT DELETING LEADING '0'.
*
*    "//Build items text
*    TABLE_TEXT = VALUE #( ( |Ordem..:   { _LINE-VBELN } / Item: { _LINE-POSNR }| )
*                          ( |Emissor:  { _LINE-KUNNR } - { _LINE-KNAME }|       )
*                          ( |Material: { _LINE-MATNR } - { _LINE-MAKTX }|       ) ).
*
*    CALL METHOD DD_DOCUMENT->ADD_TEXT
*      EXPORTING
*        TEXT_TABLE   = TABLE_TEXT
*        SAP_FONTSIZE = CL_DD_AREA=>LARGE
*        SAP_COLOR    = CL_DD_AREA=>LIST_POSITIVE_INT
*        FIX_LINES    = 'X'.
*
*    CALL METHOD DD_DOCUMENT->ADD_TEXT
*      EXPORTING
*        TEXT         = 'Qtd. Ordem: 5000'
*        SAP_FONTSIZE = CL_DD_AREA=>LARGE
*        SAP_COLOR    = CL_DD_AREA=>LIST_NEGATIVE
*        FIX_LINES    = 'X'.
*
*    ME->DD_DOCUMENT->MERGE_DOCUMENT( ).
  ENDMETHOD.

  "// --------------------------------------------------------
  "// PARAMETERS | DESCRIPTION:
  "// line         Order line.
  "// oauth        All authorizared balance for an order/item.
  "// items        All shippings alredy made for an order.
  "// --------------------------------------------------------
  METHOD set_planning.
    DATA items_planning  TYPE TABLE OF zppt0008.
    DATA qtd_em_patio    TYPE char2.
    DATA qtd_em_transito TYPE char2.

    IF ( new IS NOT INITIAL ).
      MOVE items TO items_planning.

      LOOP AT items_planning INTO DATA(_item) WHERE ( vbeln = gw_order-ordem )
                                                AND ( posnr = gw_order-item ).

        me->services->planning_production(
            )->select_shipments( EXPORTING
                                  vbeln = _item-vbeln
                                  posnr = _item-posnr
                                  edatu = _item-edatu
                                 IMPORTING
                                  data  = DATA(_shipments)
                                 EXCEPTIONS
                                  data_not_found = 4
                               ).

        me->services->planning_production(
            )->select_shipments_transit(
                                 EXPORTING
                                  vbeln = _item-vbeln
                                  posnr = _item-posnr
                                  date  = _item-edatu
                                 IMPORTING
                                  data  = DATA(_transits)
                                 EXCEPTIONS
                                  data_not_found = 7
                               ).

        APPEND VALUE
          ty_planning( vbeln = _item-vbeln
                       posnr = _item-posnr
                       edatu = _item-edatu
                       wmeng = _item-wmeng
                       wdisp = _item-wdisp
                       wcarr = REDUCE #( INIT x = 0 FOR _shipment IN _shipments
                                                   NEXT x = x + COND #( LET y = _shipment-wemng IN
                                                                       WHEN _shipment-txt04 = 'CPRD' AND y IS NOT INITIAL
                                                                       THEN y ELSE _shipment-psmng )
                                                                      )
                       iopus =  SWITCH #( _item-iopus
                                 WHEN ' ' THEN icon_light_out
                                 WHEN 'A' THEN icon_yellow_light
                                 WHEN 'P' THEN icon_green_light
                                          )

                       created_by    = _item-createdby
                       changed_by    = _item-changedby
                       created_dt    = _item-createddt
                       changed_dt    = _item-changeddt
                       t_shipments   = _shipments
                       t_transits    = _transits
                       qtd_caminhoes = |{ icon_transport } ({ lines( _shipments ) + lines( _transits ) })|
                       style         = VALUE #( ( fieldname = 'WMENG' style = cl_gui_alv_grid=>mc_style_disabled )
                                                ( fieldname = 'EDATU' style = cl_gui_alv_grid=>mc_style_disabled ) )

                     ) TO gt_plannings.

        CLEAR: _shipments, _transits.
      ENDLOOP.

      SORT gt_plannings BY edatu.
      me->set_items_before_edit( gt_plannings ).

    ELSE.
      MOVE items TO gt_plannings.
    ENDIF.
  ENDMETHOD.

  METHOD set_items_before_edit.
    MOVE items TO me->old_items.
  ENDMETHOD.

*  METHOD SET_CARREGAMENTOS.
*    MOVE ITEMS TO GT_SHIPMENTS.
*  ENDMETHOD.

  METHOD set_planning_deleted.
    APPEND item TO me->items_deleted.
  ENDMETHOD.

  METHOD set_solicitations.
    CLEAR me->solicitations.

    LOOP AT items INTO DATA(_item) WHERE ( vbeln  = gw_order-ordem
                                     AND   posnr  = gw_order-item ).

      APPEND VALUE
          ty_solicitation( nro_sol     = _item-nro_sol
                           seq_lib     = _item-seq_lib
                           dt_liber    = _item-dt_liber
                           usuario_lib = _item-usuario_lib
                           qte_lib     = _item-qte_lib
                           dt_entrega  = _item-dt_entrega

                         ) TO me->solicitations.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_row_reference.
    MOVE row_reference TO me->row_reference.
  ENDMETHOD.

  METHOD view_notification.
    CHECK dialog_is_open = abap_false.

    DATA(_items) = me->get_plannings( ).

    CALL METHOD me->if_planning_was_saved
      EXPORTING
        items                  = _items
      EXCEPTIONS
        records_were_not_saved = 4.

    IF sy-subrc IS INITIAL.
      MOVE abap_on TO notify_view.
    ELSE.
      MESSAGE s014(zppm001) DISPLAY LIKE 'W'.
    ENDIF.
  ENDMETHOD.

  METHOD get_plannings.
    MOVE gt_plannings TO r_table.
  ENDMETHOD.

  METHOD get_items_before_edit.
    MOVE me->old_items TO r_table.
  ENDMETHOD.

  METHOD get_plannings_deleted.
    MOVE me->items_deleted TO r_table.
  ENDMETHOD.

  METHOD get_header_remessa.
    MOVE me->dd_document TO r_table.
  ENDMETHOD.

  METHOD get_planning.
    MOVE me->mw_zppt0008 TO table.
  ENDMETHOD.

  METHOD get_row_reference.
    MOVE me->row_reference TO r_value.
  ENDMETHOD.

  METHOD get_solicitations.
    MOVE me->solicitations TO table.
  ENDMETHOD.

  METHOD get_trip_guide.
    DATA obj_name TYPE thead-tdname.
    DATA lines TYPE TABLE OF tline.

    SELECT SINGLE *
      FROM zsdt0082
      INTO @DATA(_solicitation)
     WHERE nro_sol = @no_solicitation
       AND vbeln   = @order
       AND posnr   = @item
       AND status  = '1'. "//Solicitada

    IF _solicitation-nr_rot IS INITIAL.
      obj_name = _solicitation-nro_sol && _solicitation-seq.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'ROTE'
          language                = sy-langu
          name                    = obj_name
          object                  = 'ZTEXTO'
        TABLES
          lines                   = lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.
    ELSE.
      obj_name = _solicitation-nr_rot.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id        = 'ZROT'
          language  = sy-langu
          name      = obj_name
          object    = 'ZSDROTEIRO'
        TABLES
          lines     = lines
        EXCEPTIONS
          id        = 1
          language  = 2
          name      = 3
          not_found = 4
          OTHERS    = 5.
    ENDIF.

    LOOP AT lines INTO DATA(_line).
      APPEND _line-tdline TO table.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_planning_was_saved.
    LOOP AT items INTO DATA(_item).

      "//Checks if there's the record in database to save or modify it
      TRY.
          me->services->planning_production( )->select_planning(
            EXPORTING
             vbeln = _item-vbeln
             posnr = _item-posnr
             edatu = _item-edatu
          ).

          DATA(_planning) = me->services->planning_production( )->get_planning( ).
          DATA(_status)   = COND #( WHEN _item-iopus = icon_yellow_light THEN 'A' "//andamento
                                    WHEN _item-iopus = icon_green_light  THEN 'P' "//processado
                                    WHEN _item-iopus = icon_light_out    THEN ' ' "//ñ processado
                                    WHEN _item-iopus = icon_delete       THEN 'D' "//deletado
                                  ).

          IF ( _planning-edatu NE _item-edatu )
          OR ( _planning-wmeng NE _item-wmeng )
          OR ( _planning-iopus NE _status     ).
            RAISE records_were_not_saved.
          ENDIF.

        CATCH zcx_pp_services.
          RAISE records_were_not_saved.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD if_item_was_changed.
    DATA(fieldnames) = zcl_util=>get_structure_description( 'ZPPT0008' ).

    LOOP AT fieldnames ASSIGNING FIELD-SYMBOL(<fs_fields>).

      ASSIGN COMPONENT:
      <fs_fields>-fieldname OF STRUCTURE old_item TO FIELD-SYMBOL(<old_value>),
      <fs_fields>-fieldname OF STRUCTURE new_item TO FIELD-SYMBOL(<new_value>).

      IF <fs_fields>-fieldname = 'IOPUS'.
        CHECK <new_value> = icon_delete.
      ENDIF.

      CHECK ( <old_value> IS ASSIGNED AND
              <new_value> IS ASSIGNED AND
              <old_value> NE <new_value> ).
      RAISE record_was_changed.
    ENDLOOP.

  ENDMETHOD.


  METHOD refresh_items_fragments.
    REFRESH gt_plannings.
  ENDMETHOD.

  METHOD clear_data_display.
    CLEAR: me->deleted_row,
           me->items_deleted,
           me->plannings,
           me->row_reference.
  ENDMETHOD.

  METHOD selected_dados_material.
    CLEAR e_mara.
    SELECT SINGLE matnr mtart spart FROM mara INTO CORRESPONDING FIELDS OF e_mara WHERE matnr EQ i_matnr.

  ENDMETHOD.

  METHOD check_material_fert_producao.
    DATA: t_centro TYPE TABLE OF setleaf,
          r_werks  TYPE RANGE OF werks_d.

    "Selecionar dados centros do SET werks_fabrica_fert.
    SELECT *
    FROM setleaf
    INTO TABLE t_centro
      WHERE setname EQ 'MV45AFZZ_WERKS'.
    IF t_centro IS NOT INITIAL.
      r_werks =  VALUE #( FOR l IN t_centro ( low = l-valfrom sign = 'I' option = 'EQ'  ) ).
    ENDIF.

    "Validação do material.
    IF i_mara-mtart EQ 'ZFER' AND i_mara-spart EQ '02' AND i_werks IN r_werks.
      e_fert_producao = abap_true.
    ELSE.
      e_fert_producao = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD new_planning.
    DATA return_action  TYPE c.

    DATA(_item_fragments)  = me->get_plannings( ).

    DATA(fields) =
      VALUE scmg_tt_sval( ( tabname   = 'ZPPET002'
                            fieldname = 'QTD_PLANNING' )
                        ).

    CALL FUNCTION 'POPUP_GET_VALUES'
      EXPORTING
        popup_title     = 'Informe a quantidade de planejamentos'
        start_column    = 90
        start_row       = 10
      IMPORTING
        returncode      = return_action
      TABLES
        fields          = fields
      EXCEPTIONS
        error_in_fields = 1
        OTHERS          = 2.

    CHECK ( return_action IS INITIAL ).

    "// Create Entries In Alv Table
    "// The Number_fragments Variable Came From Screen
    DO ( fields[ 1 ]-value ) TIMES.
      APPEND VALUE ty_planning( vbeln = gw_order-ordem
                                posnr = gw_order-item
                                iopus = icon_light_out
                                qtd_caminhoes = |{ icon_transport } (0)|
                                style = VALUE #( ( fieldname = 'WMENG'
                                                   style     = cl_gui_alv_grid=>mc_style_enabled  )
                                                 ( fieldname = 'EDATU'
                                                   style     = cl_gui_alv_grid=>mc_style_enabled  )
                                               ) )
      TO _item_fragments.
    ENDDO.

    me->set_planning( _item_fragments ).
  ENDMETHOD.

  METHOD send_planning_to_opus.
    "//Get datas
    DATA(_plannings)        = me->get_plannings( ).
    DATA(_simulador_header) = me->services->planning_production(  )->get_simulador_venda_header( ).
    DATA(_order_business)   = me->services->planning_production(  )->get_order_business( ).
    DATA(_order_header)     = me->services->planning_production(  )->get_order_header( ).
    DATA(_order_item)       = me->services->planning_production(  )->get_order_item( ).

    CHECK ( selected_row IS NOT INITIAL ).

    CALL METHOD me->if_planning_was_saved
      EXPORTING
        items                  = _plannings
      EXCEPTIONS
        records_were_not_saved = 4.

    IF ( sy-subrc IS INITIAL ).
      cl_utils=>message_process_indicator(
        percentage = sy-tabix
        message    = 'Enviando planejamento para o OPUS...'
      ).

      LOOP AT selected_row INTO DATA(_selected_row).
        ASSIGN _plannings[ _selected_row-index ] TO FIELD-SYMBOL(<fs_planning>).

        IF ( <fs_planning>-iopus EQ icon_light_out ).

          "//Change currente line
          <fs_planning>-iopus = icon_yellow_light.
          <fs_planning>-style = VALUE #( ( fieldname = 'WMENG'
                                           style     = cl_gui_alv_grid=>mc_style_disabled )
                                         ( fieldname = 'EDATU'
                                           style     = cl_gui_alv_grid=>mc_style_disabled ) ).

          "//Send order quantity to XI
          me->services->planning_production( )->insert_order_xi(
              ordem        = <fs_planning>-vbeln
              item         = <fs_planning>-posnr
              tipo         = _order_header-auart
*              quantidade   = <fs_planning>-wmeng
              quantidade   = REDUCE zpped009( INIT i TYPE zpped009 FOR planning IN _plannings WHERE ( iopus NE icon_light_out ) NEXT i = i + planning-wmeng )
              data         = <fs_planning>-edatu
              st_atividade = _order_item-spart
              material     = _order_item-matnr
              descricao    = _order_item-arktx
              emissor      = _order_header-kunnr
              incoterms    = _order_business-inco1
              vlr_unitario = _order_item-netpr
              safra        = _simulador_header-safra
              empresa      = _order_header-vkorg
              centro       = _order_item-werks
              moeda        = _order_item-waerk
          ).

          "//Insert Planning
          me->services->planning_production( )->insert_planning_xi(
              vbeln   = <fs_planning>-vbeln
              posnr   = <fs_planning>-posnr
              edatu   = <fs_planning>-edatu
*              bmeng   = <fs_planning>-wmeng
              bmeng   = REDUCE zpped009( INIT i TYPE zpped009 FOR planning IN _plannings NEXT i = i + planning-wmeng )
              status  = zcl_pp_services=>integracao_opus-processando
          ).

          MESSAGE TEXT-015 TYPE 'S'.
          me->modify_planning( <fs_planning> ).
          me->set_planning( _plannings ).

        ELSEIF <fs_planning>-iopus EQ icon_delete.
          MESSAGE TEXT-032 TYPE 'S' DISPLAY LIKE 'W'.
        ELSE.
          MESSAGE TEXT-033 TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.
      ENDLOOP.

    ELSE.
      MESSAGE TEXT-013 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.


  METHOD update_planning_row.
    DATA(_items_planning) = me->get_plannings( ).

*    ME->SET_ITEMS_BEFORE_EDIT( _ITEMS_PLANNING ).

    LOOP AT line_changed INTO DATA(_line_changed).
      CASE _line_changed-fieldname.
        WHEN 'WMENG'.
          ASSIGN _items_planning[ _line_changed-row_id ] TO FIELD-SYMBOL(<fs_planning>).
          DATA(_item_changed) = _items_planning[ _line_changed-row_id ].

          "//Reference of error
          IF me->get_row_reference( ) = _line_changed-row_id.
            me->set_row_reference( space ).
          ENDIF.

          <fs_planning>-wmeng = _line_changed-value.

          DATA(_total_plannings) = REDUCE wmeng( INIT x = 0 FOR _item_planning IN _items_planning
                                                           NEXT x = x + _item_planning-wmeng ).


*          IF ( <FS_PLANNING>-WMENG < ( _ITEM_CHANGED-WMENG - _ITEM_CHANGED-WDISP ) ).
          IF ( <fs_planning>-wmeng < <fs_planning>-wcarr ).
            <fs_planning>-wmeng = _item_changed-wmeng.
            MESSAGE TEXT-020 TYPE 'I' DISPLAY LIKE 'E'.
          ELSEIF
             ( _total_plannings > gw_order-qtd_ordem_venda ).
            <fs_planning>-wmeng = _item_changed-wmeng.
            MESSAGE TEXT-022 TYPE 'I' DISPLAY LIKE 'E'.
          ELSE.
            IF ( _line_changed-value >= _item_changed-wmeng ).
              <fs_planning>-wdisp = <fs_planning>-wdisp + ( _line_changed-value - _item_changed-wmeng ).
            ELSE.
*--> Inicio - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
*              <fs_planning>-wdisp = <fs_planning>-wdisp - ( _item_changed-wmeng - _line_changed-value ).
              <fs_planning>-wdisp = <fs_planning>-wdisp - ( _line_changed-value -  _item_changed-wmeng ).
*<-- Fim - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
            ENDIF.
*--> Inicio - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
            IF <fs_planning>-wdisp < 0.
              <fs_planning>-wdisp = <fs_planning>-wdisp * -1.
            ENDIF.
*<-- Fim - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
            IF <fs_planning>-wcarr IS NOT INITIAL
           AND <fs_planning>-wdisp IS INITIAL.
              <fs_planning>-iopus = icon_green_light.
            ENDIF.
            gw_order-qtd_planejada  = _total_plannings.
            gw_order-qtd_disponivel = gw_order-qtd_ordem_venda - gw_order-qtd_planejada.
*--> Inicio - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
            IF gw_order-qtd_disponivel < 0.
              gw_order-qtd_disponivel = gw_order-qtd_disponivel * -1.
            ENDIF.
*<-- Fim - 16/09/2025 - ggaraujo1 - IR250119 - STEFANINI - PP - 2000051418 - IR252957 - Estorno_Caminhã
            me->set_planning( _items_planning ).
          ENDIF.

          LEAVE TO SCREEN 0001.

        WHEN 'EDATU'.
          CHECK _line_changed-value NE 0.

          IF line_exists( _items_planning[ edatu = _line_changed-value ] ).

            MESSAGE TEXT-019 TYPE 'I' DISPLAY LIKE 'E'.
            LEAVE TO SCREEN 0001.

          ELSE.
            _items_planning[ _line_changed-row_id ]-edatu = _line_changed-value.
          ENDIF.
        WHEN OTHERS.
          EXIT.
      ENDCASE.
    ENDLOOP.

    me->set_planning( _items_planning ).
  ENDMETHOD.

  METHOD info_error_message.
*
*    SPLIT me->get_splitter_field( )
*       AT '/'
*    INTO DATA(action)
*     DATA(wmeng)
*     DATA(etenr).
*
*    SHIFT etenr LEFT DELETING LEADING '0'.
*
*    "//It Shows Some Explanation About The Error In Splitter
*    "//Which Is Provided By Method Me->display_message
*    CASE action.
*      WHEN 'Father<children'.
*        cl_demo_output=>new(
*          )->write_html( |<H1 Align="center" Style="color:#c70000;">Informação</h1>|
*          )->line(
*          )->write_html( |<P><font Size="3">{ text-009 } <B>{ etenr }.</B> | &&
*                          |</Font></p>|
*          )->write_html( |<P><font Size="3">{ text-010 }.</Font></p>|
*          )->display( ).
*
*      WHEN 'Children>father'.
*        cl_demo_output=>new(
*          )->write_html( |<H1 Align="center" Style="color:#c70000;">Informação</h1>|
*          )->line(
*          )->write_html( |<P><font Size="3">{ text-005 } <B>{ etenr }</B> | &&
*                         |em <B>{ me->get_num_fragmentations( ) }</B> partes.| &&
*                         |</Font></p>|
*
*          )->write_html( |<P><font Size="3">{ text-006 } <B>{ wmeng }</B>.| &&
*                         |</Font></p>|
*          )->display( ).
*
*      WHEN 'Children=father'.
*        cl_demo_output=>new(
*          )->write_html( |<H1 Align="center" Style="color:#c70000;">Informação</h1>|
*          )->line(
*          )->write_html( |<P><font Size="3">{ text-005 } <B>{ etenr }</B> | &&
*                         |em <B>{ me->get_num_fragmentations( ) }</B> Partes.| &&
*                         |</Font></p>|
*
*          )->write_html( |<P><font Size="3">{ text-008 } <B>{ wmeng }</B>.| &&
*                         |</Font></p>|
*          )->display( ).
*
*      WHEN 'Father=children'.
*        cl_demo_output=>new(
*          )->write_html( |<H1 Align="center" Style="color:#c70000;">Informação</h1>|
*          )->line(
*          )->write_html( |<P><font Size="3">{ text-009 } <B>{ etenr }.</B> | &&
*                          |</Font></p>|
*          )->write_html( |<P><font Size="3">{ text-011 }.</Font></p>|
*          )->display( ).
*
*    ENDCASE.
  ENDMETHOD.

  METHOD save_planning.
    DATA total_available_balance TYPE vbep-wmeng.

    FIELD-SYMBOLS <fs_planning> TYPE ty_planning.

    "//Get datas
    DATA(_order_business)   = me->services->planning_production( )->get_order_business( ).
    DATA(_simulador_header) = me->services->planning_production( )->get_simulador_venda_header( ).
    DATA(_order_header)     = me->services->planning_production( )->get_order_header( ).
    DATA(_order_item)       = me->services->planning_production( )->get_order_item( ).
    DATA(_items_deleted)    = me->get_plannings_deleted( ).
    DATA(_old_items)        = me->get_items_before_edit( ).
    DATA(_items_planning)   = me->get_plannings( ).
    DATA(_component)        = CONV zppt0010-component( sy-ucomm ).

    cl_utils=>message_process_indicator(
      percentage = sy-tabix
      message    = |Salvando planejamento(s)...|
    ).

    "//Get accumulated balance
    DATA(_total_planned)         = gw_order-qtd_planejada.
    DATA(_greater_planning_date) = REDUCE sy-datum( INIT x = sy-datum
                                                     FOR _item_planning IN _items_planning
                                                    NEXT x = COND #( LET dp = _item_planning-edatu IN
                                                    WHEN dp > x THEN dp ELSE x )
                                                  ).

    IF ( gw_order-qtd_disponivel_producao IS NOT INITIAL ).
      "//Update solicitations boarding
      LOOP AT solicitations INTO DATA(_solicitation) WHERE vbeln = gw_order-ordem
                                                       AND posnr = gw_order-item.
        _solicitation-status = me->solicitation_released.

        IF ( _total_planned > 0 ).
          SUBTRACT _solicitation-qte_lib FROM _total_planned.
          _solicitation-status = me->solicitation_planned.
        ENDIF.

        me->services->planning_production(
            )->update_solicitation_boarding( _solicitation ).
      ENDLOOP.

      "//Check if there're errors in the items
      LOOP AT _items_planning ASSIGNING <fs_planning> WHERE iopus <> icon_delete.
        IF ( <fs_planning>-edatu IS INITIAL ).
          me->set_row_reference( sy-tabix ).
          MESSAGE TEXT-016 TYPE 'S' DISPLAY LIKE 'E'. RETURN.
        ELSEIF <fs_planning>-wmeng IS INITIAL.
          me->set_row_reference( sy-tabix ).
          MESSAGE TEXT-017 TYPE 'S' DISPLAY LIKE 'E'. RETURN.
        ENDIF.
      ENDLOOP.

      "//Change sales order
      CALL METHOD me->services->planning_production( )->change_sales_order(
       EXPORTING
        order      = gw_order-ordem
        item       = gw_order-item
        commit     = abap_true
        operation  = 'SAVE_PLAN'
       CHANGING
        date       = _greater_planning_date
       EXCEPTIONS
        order_not_changed = 4 ).

      IF ( sy-subrc IS INITIAL ).
        LOOP AT _items_planning ASSIGNING <fs_planning>.
          TRY.
              DATA(_old_item) = _old_items[ sy-tabix ].

              CALL METHOD me->if_item_was_changed
                EXPORTING
                  old_item           = _old_item
                  new_item           = <fs_planning>
                EXCEPTIONS
                  record_was_changed = 1.

              IF ( sy-subrc <> 0 ).
                "//Delete planning that was changed
                me->services->planning_production( )->delete_planning(
                  vbeln = _old_item-vbeln
                  posnr = _old_item-posnr
                  edatu = _old_item-edatu
                ).
                "//Block old plannings
                me->services->planning_production( )->change_plannings_xi(
                  vbeln  = _old_item-vbeln
                  posnr  = _old_item-posnr
                  edatu  = _old_item-edatu
                  bmeng  = _old_item-wmeng
                  status = zcl_pp_services=>integracao_xi-bloqueado
                ).

                IF ( <fs_planning>-iopus = icon_delete ).

                  CALL METHOD me->services->planning_production( )->set_planning_log(
                    EXPORTING
                      component = _component
                      operation = 'D'
                      old_item  = _old_item
                      new_item  = <fs_planning> ).

                  IF _old_item-iopus = icon_yellow_light.
                    "//Remove the value.
                    me->services->planning_production( )->insert_order_xi(
                      EXPORTING
                        ordem        = <fs_planning>-vbeln
                        item         = <fs_planning>-posnr
                        tipo         = _order_header-auart
*                        quantidade   = -1 * _old_item-wmeng
                        quantidade   = REDUCE zpped009( INIT i TYPE zpped009 FOR planning IN _items_planning
                                                              WHERE ( iopus NE icon_delete AND iopus NE icon_light_out ) NEXT i = i + planning-wmeng )
                        data         = <fs_planning>-edatu
                        st_atividade = _order_item-spart
                        material     = _order_item-matnr
                        descricao    = _order_item-arktx
                        emissor      = _order_header-kunnr
                        incoterms    = _order_business-inco1
                        vlr_unitario = _order_item-netpr
                        safra        = _simulador_header-safra
                        empresa      = _order_header-vkorg
                        centro       = _order_item-werks
                        moeda        = _order_item-waerk
                    ).
                  ENDIF.

                ELSEIF
                    ( <fs_planning>-iopus = icon_yellow_light ).

                  CALL METHOD me->services->planning_production( )->set_planning_log(
                    EXPORTING
                      component = _component
                      operation = 'U'
                      old_item  = _old_item
                      new_item  = <fs_planning> ).

                  "//Add or remove a value.
                  me->services->planning_production( )->insert_order_xi(
                    EXPORTING
                      ordem        = <fs_planning>-vbeln
                      item         = <fs_planning>-posnr
                      tipo         = _order_header-auart
*                      quantidade   = <fs_planning>-wmeng - _old_item-wmeng
                      quantidade   = REDUCE zpped009( INIT i TYPE zpped009 FOR planning IN _items_planning WHERE ( iopus NE icon_light_out ) NEXT i = i + planning-wmeng )
                      data         = <fs_planning>-edatu
                      st_atividade = _order_item-spart
                      material     = _order_item-matnr
                      descricao    = _order_item-arktx
                      emissor      = _order_header-kunnr
                      incoterms    = _order_business-inco1
                      vlr_unitario = _order_item-netpr
                      safra        = _simulador_header-safra
                      empresa      = _order_header-vkorg
                      centro       = _order_item-werks
                      moeda        = _order_item-waerk
                  ).
                  "//Change planning
                  me->services->planning_production( )->change_plannings_xi(
                    vbeln  = <fs_planning>-vbeln
                    posnr  = <fs_planning>-posnr
                    edatu  = <fs_planning>-edatu
                    bmeng  = <fs_planning>-wmeng
*                    bmeng  = REDUCE zpped009( INIT i TYPE zpped009 FOR planning IN _items_planning NEXT i = i + planning-wmeng )
                    status = zcl_pp_services=>integracao_xi-processando
                  ).

                ELSE.
                  CALL METHOD me->services->planning_production( )->set_planning_log(
                    EXPORTING
                      component = _component
                      operation = 'U'
                      old_item  = _old_item
                      new_item  = <fs_planning> ).
                ENDIF.

                <fs_planning>-changed_by = sy-uname.
                <fs_planning>-changed_dt = sy-datum.

              ELSE.
                <fs_planning>-iopus = _old_item-iopus.
              ENDIF.

            CATCH cx_sy_itab_line_not_found.
              <fs_planning>-created_by = sy-uname.
              <fs_planning>-created_dt = sy-datum.

              CALL METHOD me->services->planning_production( )->set_planning_log(
                EXPORTING
                  component = _component
                  operation = 'I'
                  old_item  = _old_item
                  new_item  = <fs_planning> ).
          ENDTRY.

          <fs_planning>-style = VALUE #( ( fieldname = 'WMENG' style = cl_gui_alv_grid=>mc_style_disabled )
                                         ( fieldname = 'EDATU' style = cl_gui_alv_grid=>mc_style_disabled )
                                       ).

          "//Modify planning
          CLEAR _old_item.
          me->modify_planning( <fs_planning> ).
        ENDLOOP.

        DELETE _items_planning WHERE iopus = icon_delete.

        me->set_planning( _items_planning ).
        me->set_items_before_edit( _items_planning ).

        MESSAGE s006(zppm001) WITH gw_order-ordem gw_order-item.

      ELSE.
        me->set_planning( me->get_items_before_edit( ) ).
        cl_utils=>display_messages( me->services->planning_production( )->get_messages( ) ).
      ENDIF.

    ELSE.
      me->set_planning( me->get_items_before_edit( ) ).
      MESSAGE i005(zppm001) WITH gw_order-ordem gw_order-item DISPLAY LIKE 'W'.
    ENDIF.

    me->clear_data_display( ).
  ENDMETHOD.

  METHOD modify_planning_row.
    DATA(_plannings) = me->get_plannings( ).
    DATA(_old_items) = me->get_items_before_edit( ).

    IF ( selected_row IS NOT INITIAL ).

      LOOP AT selected_row INTO DATA(_selected_row).
        ASSIGN _plannings[ _selected_row-index ] TO FIELD-SYMBOL(<fs_planning>).

        CHECK   <fs_planning>-iopus <> icon_delete.

        <fs_planning>-style = COND #( WHEN <fs_planning>-wdisp <> <fs_planning>-wmeng
                                    THEN VALUE #( ( fieldname = 'WMENG' style = cl_gui_alv_grid=>mc_style_enabled  )
                                                  ( fieldname = 'EDATU' style = cl_gui_alv_grid=>mc_style_disabled )
                                                )
                                    ELSE VALUE #( ( fieldname = 'WMENG' style = cl_gui_alv_grid=>mc_style_enabled  )
                                                  ( fieldname = 'EDATU' style = cl_gui_alv_grid=>mc_style_enabled  )
                                                )
                                  ).
      ENDLOOP.

      me->set_planning( _plannings ).
    ELSE.
      MESSAGE s023(zppm001) DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD delete_planning_row.
    DATA(_items_planning) = me->get_plannings( ).

    IF ( selected_row IS NOT INITIAL ).

      LOOP AT selected_row INTO DATA(_selected_row).
        ASSIGN _items_planning[ _selected_row-index ] TO FIELD-SYMBOL(<fs_planning>).

        CHECK <fs_planning>-iopus <> icon_delete.

        DATA(_qtd_shipments) = lines( <fs_planning>-t_shipments ).
        DATA(_qtd_transits)  = lines( <fs_planning>-t_transits ).

*        SELECT *
*          FROM ZPPT0009
*          INTO TABLE @DATA(T_0009)
*          WHERE DT_MODIFICACAO EQ @SY-DATUM
*          AND VBELN EQ @<FS_PLANNING>-VBELN.
*
*        IF SY-SUBRC IS INITIAL.
*          SORT T_0009 BY DT_MODIFICACAO HR_MODIFICACAO DESCENDING.
*          DATA(W_0009) = T_0009[ 1 ].
*      ENDIF.

        DATA(hora) = me->services->planning_production( )->get_last_time( <fs_planning>-vbeln ).

        IF NOT ( _qtd_shipments IS INITIAL )
        OR NOT ( _qtd_transits  IS INITIAL ).

          DATA(_total_shipments) = _qtd_shipments + _qtd_transits.
          MESSAGE s001(zppm001) WITH cl_utils=>convert_date_external( date = <fs_planning>-edatu ) _total_shipments DISPLAY LIKE 'E'.

        ELSE.
          IF hora < 500.
            MESSAGE s000(zppm001) WITH 'Não é possivel Excluir, Aguarde 5 minutos!' DISPLAY LIKE 'E'.
          ELSE.
            <fs_planning>-iopus = icon_delete.
            <fs_planning>-style = VALUE #( ( fieldname = 'WMENG' style = cl_gui_alv_grid=>mc_style_disabled )
                                           ( fieldname = 'EDATU' style = cl_gui_alv_grid=>mc_style_disabled )
                                         ).

            gw_order-qtd_disponivel = gw_order-qtd_disponivel + <fs_planning>-wmeng.
            gw_order-qtd_planejada  = gw_order-qtd_planejada - <fs_planning>-wmeng.
          ENDIF.
        ENDIF.
      ENDLOOP.

      me->set_planning( _items_planning ).
      LEAVE TO SCREEN 0001.

    ELSE.
      MESSAGE s024(zppm001) DISPLAY LIKE 'E'.
    ENDIF.
  ENDMETHOD.

  METHOD insert_planning.
    me->mw_zppt0008 =
    VALUE #( vbeln = items-vbeln
         posnr = items-posnr
         edatu = items-edatu
         wmeng = items-wmeng
         wdisp = items-wdisp
         iopus = SWITCH #( LET x = zcl_pp_services=>integracao_opus-nao_processado
                 IN me->mw_zppt0008-iopus
               WHEN icon_yellow_light THEN zcl_pp_services=>integracao_opus-processando
               WHEN icon_green_light  THEN zcl_pp_services=>integracao_opus-processado
               )
    ).

    INSERT zppt0008 FROM me->mw_zppt0008.
    COMMIT WORK.
  ENDMETHOD.

  METHOD modify_planning.
    IF item-iopus <> icon_delete.
      me->mw_zppt0008 =
      VALUE #( vbeln = item-vbeln
             posnr = item-posnr
             edatu = item-edatu
             wmeng = item-wmeng
             wdisp = item-wdisp
             iopus = SWITCH #( LET x = zcl_pp_services=>integracao_opus-nao_processado
                                IN item-iopus
                              WHEN icon_yellow_light THEN zcl_pp_services=>integracao_opus-processando
                              WHEN icon_green_light  THEN zcl_pp_services=>integracao_opus-processado
                              )
             createdby = item-created_by
             changedby = item-changed_by
             createddt = item-created_dt
             changeddt = item-changed_dt
           ).

      MODIFY zppt0008 FROM me->mw_zppt0008.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.

  METHOD modify_plannings.
    LOOP AT items INTO DATA(_item).
      APPEND VALUE
          zppt0008( vbeln = _item-vbeln
                    posnr = _item-posnr
                    edatu = _item-edatu
                    wmeng = _item-wmeng
                    wdisp = _item-wdisp
                    iopus = SWITCH #( LET x = zcl_pp_services=>integracao_opus-nao_processado
                                IN _item-iopus
                              WHEN icon_yellow_light THEN zcl_pp_services=>integracao_opus-processando
                              WHEN icon_green_light  THEN zcl_pp_services=>integracao_opus-processado
                                    ) ) TO me->plannings.
    ENDLOOP.

    MODIFY zppt0008 FROM TABLE plannings.
    COMMIT WORK.
  ENDMETHOD.

  METHOD update_planning.
    UPDATE zppt0008 SET (parameter) WHERE ( vbeln = vbeln )
                                AND ( posnr = posnr )
                                AND ( edatu = edatu ).
    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.

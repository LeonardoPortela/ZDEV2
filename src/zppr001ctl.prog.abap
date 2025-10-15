*&---------------------------------------------------------------------*
*&  Include           Zppr001ctl
*&---------------------------------------------------------------------*

"// Main Controller
CLASS main_controller DEFINITION
                      INHERITING FROM zsapmvc_controller.
  PUBLIC SECTION.
    "// Model Object
    DATA model TYPE REF TO main_model.

    METHODS pf_status             REDEFINITION.
    METHODS titlebar              REDEFINITION.
    METHODS user_command          REDEFINITION.
    METHODS at_exit_command       REDEFINITION.
    METHODS create_model          REDEFINITION.
    METHODS process_before_output REDEFINITION.

    METHODS set_display_dynnr
      IMPORTING i_dynpro TYPE sy-dynnr.

    METHODS set_dock_extension
      IMPORTING
        extension TYPE i.

    METHODS set_top_nodes_key
      IMPORTING node_key TYPE lvc_nkey.

    METHODS get_top_nodes_key
      RETURNING VALUE(table) TYPE lvc_t_nkey.

    METHODS get_line_dock_clicked
      RETURNING VALUE(r_table) TYPE ty_ordens.

    METHODS get_display_dynnr
      RETURNING VALUE(r_dynpro) TYPE sy-dynnr.

    METHODS get_dock_extension
      RETURNING VALUE(value) TYPE i.

    METHODS refresh_screen.

  PRIVATE SECTION.

    "// Dynpro Attributes
    DATA dynpro_display TYPE sy-dynnr VALUE '0003'.
    DATA nodes          TYPE lvc_t_nkey.
    DATA dock_extension TYPE i VALUE 5000.
    DATA outtab_line    TYPE ty_ordens.
    DATA docking        TYPE REF TO cl_gui_docking_container.
    DATA alv_tree       TYPE REF TO cl_gui_alv_tree.

    "// Events Methods
    METHODS handle_double_click FOR EVENT node_double_click OF cl_gui_alv_tree
      IMPORTING node_key.

    "// Dynpro Methods
    METHODS display.
    METHODS set_register_events.
    METHODS refresh_nodes.

    METHODS get_tree_header
      RETURNING VALUE(r_table) TYPE treev_hhdr.

    METHODS get_fieldcatalog
      RETURNING VALUE(r_table) TYPE lvc_t_fcat.
ENDCLASS.

CLASS main_controller IMPLEMENTATION.
  METHOD pf_status.
    SET PF-STATUS 'STATUS_0001'.
  ENDMETHOD.

  METHOD titlebar.
    SET TITLEBAR 'TITLE_0001'.
  ENDMETHOD.

  METHOD user_command.
    CASE ucomm.
      WHEN 'BACK'.
        LEAVE TO SCREEN 0.
      WHEN 'REFRESH'.
        cl_utils=>message_process_indicator(
          percentage = sy-tabix
          message    = |Atualizando dados...|
        ).

        edit_on  = abap_false.

        CLEAR gt_plannings.
        CLEAR gw_order.
    ENDCASE.
  ENDMETHOD.

  METHOD at_exit_command.
    CASE ucomm.
      WHEN 'BACK' OR 'CANC'.
        LEAVE PROGRAM.
    ENDCASE.
  ENDMETHOD.                    "At_exit_command

  METHOD create_model.
    TRY.
        model ?= zsapmvc_model=>create_model( model_reference = 'MAIN_MODEL' repid = sy-repid ).
      CATCH zcx_sapmvc.
    ENDTRY.
  ENDMETHOD.

  METHOD process_before_output.
    TRY.
        me->model->select_all_orders( ).
        me->display( ).

      CATCH cx_local_exception INTO DATA(data_not_found).
        data_not_found->message( type = 'S' display = 'E' ).
        LEAVE TO SCREEN 0.
    ENDTRY.

    subscreen_display = me->get_display_dynnr( ).
  ENDMETHOD.

  METHOD display.
    IF sy-ucomm = 'REFRESH'.
      me->alv_tree->free( ).
    ENDIF.

    IF ( docking IS NOT BOUND ).
      CREATE OBJECT me->docking
        EXPORTING
          side      = cl_gui_docking_container=>dock_at_left
          extension = 5000
          repid     = sy-repid
          dynnr     = '0001'.
    ENDIF.

    CREATE OBJECT me->alv_tree
      EXPORTING
        parent              = me->docking
        node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
        item_selection      = ''
        no_html_header      = 'X'
        no_toolbar          = ''.

    "// Get Field Catalog
    DATA(fieldcat) = me->get_fieldcatalog( ).
    DATA(header)   = me->get_tree_header( ).

    "// Set Tree Display
    CALL METHOD me->alv_tree->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = header
      CHANGING
        it_outtab           = gt_ordens
        it_fieldcatalog     = fieldcat.

    "// Set Ov Hierarchy
    me->model->set_hierarchy_orders(
      CHANGING
        alv_tree = me->alv_tree ).

    me->alv_tree->expand_nodes(
      me->model->get_top_nodes_key( ) ).

    "// Set Registered Events
    me->set_register_events( ).
    me->alv_tree->frontend_update( ).

    cl_gui_cfw=>flush( ).
  ENDMETHOD.

  METHOD set_display_dynnr.
    me->dynpro_display = i_dynpro.
  ENDMETHOD.

  METHOD set_dock_extension.
    me->docking->set_extension( extension ).
  ENDMETHOD.

  METHOD set_top_nodes_key.
    APPEND node_key TO me->nodes.
  ENDMETHOD.

  METHOD get_top_nodes_key.
    MOVE me->nodes TO table.
  ENDMETHOD.

  METHOD set_register_events.
    DATA lt_events TYPE cntl_simple_events.

    lt_events = VALUE #( ( eventid    = cl_gui_column_tree=>eventid_node_double_click
                           appl_event = abap_true )
                         ( eventid    = cl_gui_column_tree=>eventid_expand_no_children
                           appl_event = abap_false ) ).

    CALL METHOD me->alv_tree->set_registered_events
      EXPORTING
        events = lt_events.

    SET HANDLER me->handle_double_click FOR me->alv_tree.
  ENDMETHOD.

  METHOD refresh_nodes.
    REFRESH me->nodes.
  ENDMETHOD.

  METHOD handle_double_click.
    CALL METHOD me->alv_tree->get_outtab_line
      EXPORTING
        i_node_key    = node_key
      IMPORTING
        e_outtab_line = me->outtab_line.

    CHECK ( me->outtab_line IS NOT INITIAL ).

    CLEAR gt_plannings.
    CLEAR gt_shipments.
    CLEAR gw_order.

    CALL METHOD me->model->replanning_shipments_transit
      EXPORTING
        ordem = me->outtab_line-ordem
        item  = me->outtab_line-item.

    me->docking->set_extension( 160 ).
    me->set_display_dynnr( '0002' ).
  ENDMETHOD.

  METHOD get_display_dynnr.
    MOVE me->dynpro_display TO r_dynpro.
  ENDMETHOD.

  METHOD get_dock_extension.
    me->docking->get_extension(
      IMPORTING
        extension = value
    ).
  ENDMETHOD.

  METHOD get_fieldcatalog.
*    R_TABLE = CL_UTILS=>GET_STRUCTURE_DESCRIPTION( 'TY_ORDENS' ).

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'CENTRO'
        description = 'Centro'
        outputlen   = 10
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'EMISSOR'
        description = 'Emissor'
        no_zero     = abap_true
        outputlen   = 50
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'MATERIAL'
        description = 'Material'
        no_zero     = abap_true
        outputlen   = 50
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'QTD_ORDEM_VENDA'
        description = 'Total Solicitado'
        ref_table   = 'VBEP'
        ref_field   = 'WMENG'
        outputlen   = 20
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'QTD_PLANEJADA'
        description = 'Total Planejado'
        ref_table   = 'VBEP'
        ref_field   = 'WMENG'
        outputlen   = 20
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'QTD_DISPONIVEL'
        description = 'Total Disponível'
        ref_table   = 'VBEP'
        ref_field   = 'WMENG'
        outputlen   = 20
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'QTD_EM_TRANSITO'
        description = 'Total Em Trânsito'
        ref_table   = 'VBEP'
        ref_field   = 'WMENG'
        outputlen   = 23
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'QTD_EM_ANDAMENTO'
        description = 'Total Em Andamento'
        ref_table   = 'VBEP'
        ref_field   = 'WMENG'
        outputlen   = 28
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'QTD_FATURADA'
        description = 'Total Faturado'
        ref_table   = 'VBEP'
        ref_field   = 'WMENG'
        outputlen   = 20
      CHANGING
        r_table     = r_table.
  ENDMETHOD.

  METHOD get_line_dock_clicked.
    TRY.
        r_table = gt_ordens[ ordem = me->outtab_line-ordem item = me->outtab_line-item ].
      CATCH cx_sy_itab_line_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD refresh_screen.
    me->alv_tree->free( ).
    me->docking->free( ).

    CLEAR me->alv_tree.
    CLEAR me->docking.
  ENDMETHOD.

  METHOD get_tree_header.
    r_table = VALUE treev_hhdr( heading = TEXT-002
                                tooltip = TEXT-002
                                width   = 28 ).
  ENDMETHOD.
ENDCLASS.
"// End Of Main Controller

"// Shipments Controller
CLASS shipments_controller DEFINITION
                           INHERITING FROM zsapmvc_controller.
  PUBLIC SECTION.
    DATA model TYPE REF TO shipments_model.

    METHODS process_before_output REDEFINITION.
    METHODS create_model REDEFINITION.
    METHODS pf_status    REDEFINITION.
    METHODS user_command REDEFINITION.
    METHODS call_screen  REDEFINITION.

    METHODS set_title
      IMPORTING title TYPE lvc_title.

    METHODS set_splitter_reference
      IMPORTING
        reference TYPE REF TO cl_gui_splitter_container.

    METHODS set_source_shipments_row
      IMPORTING row TYPE lvc_index.

    METHODS get_source_shipments_row
      RETURNING VALUE(value) TYPE lvc_index.

    METHODS get_splitter_reference
      RETURNING VALUE(ref) TYPE REF TO cl_gui_splitter_container.

    METHODS refresh_source_shipments.
    METHODS display.

  PRIVATE SECTION.
    "//Attributes
    DATA alv_table          TYPE REF TO cl_gui_alv_grid.
    DATA splitter_reference TYPE REF TO cl_gui_splitter_container.
    DATA container          TYPE REF TO cl_gui_custom_container.
    DATA container_editor   TYPE REF TO cl_gui_textedit.
    DATA planning_row       TYPE lvc_index.
    DATA title              TYPE lvc_title.
    DATA layout             TYPE lvc_s_layo.
    DATA text_editor        TYPE TABLE OF ty_editor.

    METHODS get_title
      RETURNING VALUE(value) TYPE lvc_title.

    "//Methods
    METHODS get_fieldcatalog
      RETURNING VALUE(r_table) TYPE lvc_t_fcat.

    METHODS refresh_screen.

    METHODS handle_set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object.

    METHODS handle_menu_button FOR EVENT menu_button OF cl_gui_alv_grid
      IMPORTING e_object e_ucomm.

    METHODS handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS handle_hotspot FOR EVENT hotspot_click  OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no.
ENDCLASS.

CLASS shipments_controller IMPLEMENTATION.
  METHOD pf_status.
    SET PF-STATUS 'STATUS_0002'.
  ENDMETHOD.

  METHOD user_command.
    CASE ucomm.
      WHEN 'CANC'.
        LEAVE TO SCREEN 0.
      WHEN 'CONF'.
        CALL METHOD me->alv_table->get_selected_rows
          IMPORTING
            et_index_rows = DATA(selected_row).

        container_editor->get_text_as_r3table(
          IMPORTING
            table = me->text_editor ).

        me->model->print(
          selected_rows = selected_row
          print_type    = 'ETIQUETA'
        ).

        me->refresh_screen( ).
        LEAVE TO SCREEN 0.
    ENDCASE.

    "// Setup Repeat change Notification
    TRY.
        zsapmvc_view=>activate_change_notification( model_instance = me->model
                                                    view_instance  = view
                                                    timeout        = '1'
                                                    repeat         = abap_true ).
      CATCH zcx_sapmvc.
    ENDTRY.

    CLEAR text_editor.
  ENDMETHOD.

  METHOD call_screen.
    "// Call Screen <dynpro>
    CALL SCREEN dynpro STARTING AT starting_column starting_line
                       ENDING   AT ending_column ending_line.

    TRY.
        "//Get the actual Controller Instance (It's necessary after change Dynpro)
        controller = zsapmvc_controller=>get_controller( repid = sy-repid dynnr = '0002' ).

        "//Get the Actual View Instance (It's necessary after change Dynpro)
        view = zsapmvc_view=>get_view( repid = sy-repid dynnr = '0002' ).
      CATCH zcx_sapmvc.
    ENDTRY.
  ENDMETHOD.

  METHOD set_splitter_reference.
    MOVE reference TO me->splitter_reference.
  ENDMETHOD.

  METHOD set_title.
    MOVE title TO me->title.
  ENDMETHOD.

  METHOD set_source_shipments_row.
    MOVE row TO me->planning_row.
  ENDMETHOD.

  METHOD get_source_shipments_row.
    MOVE me->planning_row TO value.
  ENDMETHOD.

  METHOD get_splitter_reference.
    MOVE me->splitter_reference TO ref.
  ENDMETHOD.

  METHOD refresh_source_shipments.
    CLEAR me->planning_row.
  ENDMETHOD.

  METHOD get_title.
    MOVE me->title TO value.
  ENDMETHOD.

  METHOD display.
    me->get_splitter_reference( )->set_column_width( id = 1 width = 50 ).
    me->get_splitter_reference( )->set_column_width( id = 2 width = 50 ).

    DATA(_custom) = me->get_splitter_reference(
                        )->get_container( row = 1 column = 2 ).

    me->layout-grid_title = me->get_title( ).
    me->layout-info_fname = 'ROWCOLOR'.

    IF ( me->alv_table IS NOT BOUND ).
      "//Create Alv Object
      CREATE OBJECT me->alv_table
        EXPORTING
          i_parent = _custom.
    ENDIF.

    "//Get Field Catalog
    DATA(_fieldcat) = me->get_fieldcatalog( ).

    SET HANDLER:
    me->handle_set_toolbar  FOR me->alv_table,
    me->handle_user_command FOR me->alv_table,
    me->handle_menu_button  FOR me->alv_table.
*    ME->HANDLE_HOTSPOT      FOR ME->ALV_TABLE.

    "//Display Datas
    CALL METHOD me->alv_table->set_table_for_first_display
      EXPORTING
        is_layout       = me->layout
        i_save          = abap_on
      CHANGING
        it_outtab       = gt_shipments
        it_fieldcatalog = _fieldcat.

    me->refresh_screen( ).
  ENDMETHOD.

  METHOD get_fieldcatalog.
*    R_TABLE = CL_UTILS=>GET_STRUCTURE_DESCRIPTION( 'TY_SHIPMENTS' ).

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'PLACA'
        description = 'Placa do veículo'
        outputlen   = 16
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'ORDEMCARREG'
        description = 'Ordem carregamento'
        outputlen   = 18
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'AUFNR'
        description = 'Ordem produção'
        no_zero     = abap_true
        outputlen   = 14
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'PSMNG'
        description = 'Qtd. da ordem'
        sum         = abap_true
        outputlen   = 13
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'TXT30'
        description = 'Status'
        outputlen   = 20
      CHANGING
        r_table     = r_table.

*    LOOP AT R_TABLE ASSIGNING FIELD-SYMBOL(<FS_TABLE>).
*      CASE <FS_TABLE>-FIELDNAME.
*        WHEN 'TXT04' OR 'WEMNG' OR 'ERDAT'.
*          <FS_TABLE>-NO_OUT = ABAP_TRUE.
*        WHEN 'PSMNG'.
*          <FS_TABLE>-DO_SUM = ABAP_TRUE.
*      ENDCASE.
*    ENDLOOP.
  ENDMETHOD.

  METHOD refresh_screen.
    me->alv_table->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.

  METHOD process_before_output.

    CREATE OBJECT container
      EXPORTING
        container_name = 'CUSTOM_TEXTEDIT'.

    CREATE OBJECT container_editor
      EXPORTING
        parent            = container
        wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 76
        max_number_chars  = 100.

    CALL METHOD container_editor->set_text_as_r3table
      EXPORTING
        table = me->text_editor.

    CALL METHOD container_editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.

    CALL METHOD container_editor->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.
  ENDMETHOD.

  METHOD create_model.
    "// Create Model Instance
    TRY.
        model ?= zsapmvc_model=>create_model( model_reference = 'SHIPMENTS_MODEL' repid = sy-repid ).
      CATCH zcx_sapmvc.
    ENDTRY.
  ENDMETHOD.

  METHOD handle_set_toolbar.
    "//Set Buttons with some exceptions
    DATA(_standard_toolbars) = e_object->mt_toolbar.
    CLEAR e_object->mt_toolbar.

    DATA(_functions) = VALUE rsis_t_range(
                            ( sign = 'I' option = 'EQ' low = '&FIND'      )
                            ( sign = 'I' option = 'EQ' low = '&MB_FILTER' )
                            ( sign = 'I' option = 'EQ' low = '&SORT_ASC'  )
                            ( sign = 'I' option = 'EQ' low = '&SORT_DSC'  )
                                         ).

    APPEND VALUE #( butn_type = cntb_btype_button
                    function  = 'CLOSE'
                    icon      = icon_close
                  ) TO e_object->mt_toolbar.

    APPEND VALUE #( butn_type = cntb_btype_sep
                  ) TO e_object->mt_toolbar.

    LOOP AT _standard_toolbars INTO DATA(_toolbar).
      CHECK ( _toolbar-function IN _functions ).
      APPEND _toolbar TO e_object->mt_toolbar.
    ENDLOOP.

    APPEND VALUE #( butn_type = cntb_btype_sep
                  ) TO e_object->mt_toolbar.

    "//Check planner access;
    AUTHORITY-CHECK OBJECT 'Z_PLANNING' ID 'ACTVT' FIELD '72'.

    IF ( sy-subrc IS INITIAL ).
      APPEND VALUE #( butn_type = cntb_btype_button
                      function  = 'RELEASE_ENTRY'
                      icon      = icon_planning_in
                      text      = 'Autorizar Entrada'
                    ) TO e_object->mt_toolbar.

*-US 140426-26-06-2024-#140426-RJF-inicio
      AUTHORITY-CHECK OBJECT 'Z_PLANNING' ID 'ACTVT' FIELD '80'. "//Check planner access;
      IF sy-subrc IS INITIAL.
*-US 140426-26-06-2024-#140426-RJF-fim

        APPEND VALUE #( butn_type = cntb_btype_dropdown
                        function  = 'PRINT'
                        icon      = icon_print
                        text      = 'Imprimir'
                      ) TO e_object->mt_toolbar.
*-US 140426-26-06-2024-#140426-RJF-inicio
      ENDIF.
*-US 140426-26-06-2024-#140426-RJF-fim

*-US 140426-26-06-2024-#140426-RJF-inicio
    ELSE.
      "//Check planner access;
      AUTHORITY-CHECK OBJECT 'Z_PLANNING' ID 'ACTVT' FIELD '80'.

      IF sy-subrc IS INITIAL.
        APPEND VALUE #( butn_type = cntb_btype_dropdown
                        function  = 'PRINT'
                        icon      = icon_print
                        text      = 'Imprimir'
                      ) TO e_object->mt_toolbar.
      ENDIF.
*-US 140426-26-06-2024-#140426-RJF-fim
    ENDIF.

    APPEND VALUE #( butn_type = cntb_btype_sep ) TO e_object->mt_toolbar.

    "//Check storno access;
    AUTHORITY-CHECK OBJECT 'Z_PLANNING' ID 'ACTVT' FIELD '85'.

    IF sy-subrc IS INITIAL.
      APPEND VALUE #( butn_type = cntb_btype_dropdown
                      function  = 'STORNO'
                      icon      = icon_planning_out
                      text      = 'Estornar'
                    ) TO e_object->mt_toolbar.
    ENDIF.


    AUTHORITY-CHECK OBJECT 'Z_PLANNING' ID 'ACTVT' FIELD '85'.

    IF sy-subrc IS INITIAL.
      APPEND VALUE #( butn_type = cntb_btype_button
                      function  = 'ENVIA_SUPERV'
                      icon      = icon_ps_network_header
                      text      = 'Enviar Supervisório'
                    ) TO e_object->mt_toolbar.
    ENDIF.
  ENDMETHOD.

  METHOD handle_menu_button.
    CASE e_ucomm.
      WHEN 'PRINT'.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'PRINT_ORDEM_PRODUCAO'
            text  = '1. Ordem de Produção'.

        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'PRINT_ETIQUETA'
            text  = '2. Etiqueta SAC'.

        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'PRINT_ETIQUETA1'
            text  = '3. Etiqueta BAG'.

*        CALL METHOD E_OBJECT->ADD_FUNCTION
*          EXPORTING
*            FCODE = 'PRINT_ETIQUETA2'
*            TEXT  = '4. Etiqueta BAG A4'.

      WHEN 'STORNO'.
        CALL METHOD e_object->add_function
          EXPORTING
            fcode = 'STORNO_AUTHORIZATION'
            text  = '1. Ordem de Carregamento'.

*        CALL METHOD E_OBJECT->ADD_FUNCTION
*          EXPORTING
*            FCODE = 'STORNO_RELEASE'
*            TEXT  = '2. Ordem de Produção'.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_user_command.
    CALL METHOD me->alv_table->get_selected_rows
      IMPORTING
        et_index_rows = DATA(selected_row).

    CASE e_ucomm.
      WHEN 'CLOSE'.
        me->splitter_reference->set_column_width( id = 1 width = 100 ).
        me->refresh_source_shipments( ).
      WHEN 'RELEASE_ENTRY'.
        me->model->authorize_entry( selected_row ).
        me->refresh_screen( ).
      WHEN 'PRINT_ORDEM_PRODUCAO'.
        me->model->print(
          selected_rows = selected_row
          print_type    = 'ORDEM_PRODUCAO' ).
      WHEN 'PRINT_ETIQUETA'.
        me->model->print(
          selected_rows = selected_row
          print_type    = 'ETIQUETA' ).
      WHEN 'PRINT_ETIQUETA1'.
        me->model->print(
          selected_rows = selected_row
          print_type    = 'ETIQUETA1' ).
      WHEN 'PRINT_ETIQUETA2'.
        me->model->print(
          selected_rows = selected_row
          print_type    = 'ETIQUETA2' ).
      WHEN 'STORNO_AUTHORIZATION'.
        me->model->storno(
          selected_rows = selected_row
          storno_type   = 'AUTHORIZATION_ENTRY' ).

        me->refresh_screen( ).
      WHEN 'STORNO_RELEASE'.
        me->model->storno(
          selected_rows = selected_row
          storno_type   = 'RELEASE_PO' ).

        me->refresh_screen( ).

      WHEN 'ENVIA_SUPERV'.
        me->model->enviar_supervisorio(
          selected_rows = selected_row ).

        me->refresh_screen( ).
    ENDCASE.
  ENDMETHOD.

  METHOD handle_hotspot.
    DATA(_items) = me->model->get_shipments( ).
    DATA(_item)  = _items[ e_row_id-index ].

    CASE e_column_id.
      WHEN 'AUFNR'.

        SET PARAMETER ID 'ANR' FIELD _item-aufnr.
        CALL TRANSACTION 'COR3'.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.

"// Display Controller
CLASS display_controller DEFINITION
                         INHERITING FROM zsapmvc_controller.
  PUBLIC SECTION.
    "// Model Object
    DATA model                    TYPE REF TO display_model.
    DATA main_controller          TYPE REF TO main_controller.
    DATA shipments_controller     TYPE REF TO shipments_controller.

    METHODS pf_status             REDEFINITION.
    METHODS process_before_output REDEFINITION.
    METHODS create_model          REDEFINITION.
    METHODS user_command          REDEFINITION.
    METHODS call_screen           REDEFINITION.
*    METHODS SET_DYNPRO            REDEFINITION.

  PRIVATE SECTION.
    DATA splitter      TYPE REF TO cl_gui_splitter_container.
    DATA custom        TYPE REF TO cl_gui_custom_container.
    DATA selections    TYPE REF TO cl_salv_selections.
    DATA alv_table     TYPE REF TO cl_gui_alv_grid.
    DATA layout        TYPE lvc_s_layo.

    DATA items_solicitation TYPE ty_solicitation.

    "// Dynpro Attributes
    DATA dynpro_sample TYPE sy-dynnr VALUE '0004'.

    METHODS dependency_injection.
    METHODS display_plannings.
    METHODS display_solicitations.
    METHODS refresh_screen.

    METHODS set_action
      IMPORTING action TYPE sy-ucomm.

    METHODS get_action
      RETURNING VALUE(value) TYPE sy-ucomm.

    METHODS set_selected_row
      IMPORTING VALUE(r_value) TYPE lvc_index.

    METHODS get_splitter_reference
      RETURNING VALUE(ref) TYPE REF TO cl_gui_splitter_container.

    METHODS get_fieldcatalog
      RETURNING VALUE(r_table) TYPE lvc_t_fcat.

    METHODS handle_set_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING
        e_object.

    METHODS handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING
        e_ucomm.

    METHODS handle_salv_user_command
      FOR EVENT added_function OF cl_salv_events
      IMPORTING e_salv_function.

    METHODS handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING
        er_data_changed
        e_onf4
        e_onf4_before
        e_onf4_after
        e_ucomm.

    METHODS handle_display_shipments FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING
        e_row
        e_column
        es_row_no.
ENDCLASS.

CLASS display_controller IMPLEMENTATION.
  METHOD pf_status.
    "//Check planner access;
    AUTHORITY-CHECK OBJECT 'Z_PLANNING' ID 'ACTVT' FIELD '72'.

    IF sy-subrc IS NOT INITIAL.
      LOOP AT SCREEN.
        IF screen-name = 'SAVE_PLANNING'.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD process_before_output.
    me->dependency_injection( ).

    TRY.
        me->main_controller = CAST main_controller(
            zsapmvc_controller=>get_controller('MAIN_CONTROLLER')
        ).

        me->shipments_controller = CAST shipments_controller(
            zsapmvc_controller=>get_controller('SHIPMENTS_CONTROLLER')
        ).

      CATCH zcx_sapmvc.
    ENDTRY.

*    MODEL_NOTIFICATION ?= ME->MODEL.
*    VIEW_NOTIFICATION  ?= VIEW.

    "// Setup Repeat change Notification
    TRY.
        zsapmvc_view=>activate_change_notification( model_instance = me->model
                                                    view_instance  = view
                                                    timeout        = '30'
                                                    repeat         = abap_true ).
      CATCH zcx_sapmvc.
    ENDTRY.

    IF ( gw_order IS INITIAL ).
      gw_order = me->main_controller->get_line_dock_clicked( ).

      me->model->services->planning_production( )->select_ov_divisions(
       EXPORTING
        vbeln = gw_order-ordem
        posnr = gw_order-item
       IMPORTING
        items = DATA(_order_divisions)
      ).

      me->model->set_planning(
        EXPORTING
          items = me->main_controller->model->get_order_plannings( )
          new   = abap_true
      ).

      gw_order-qtd_disponivel_producao =
        REDUCE wmeng( INIT x = 0  FOR _division IN  _order_divisions WHERE ( lifsp <> '12' AND lifsp <> ' ' )
                                                NEXT x = x + _division-wmeng ).
    ENDIF.

*    IF ( ME->SHIPMENTS_CONTROLLER->MODEL->GET_SHIPMENTS( ) IS INITIAL ).
*      ME->SHIPMENTS_CONTROLLER->REFRESH_SOURCE_SHIPMENTS( ).
*    ENDIF.

    "//Check if had an shipment selected;
*    IF ( ME->SHIPMENTS_CONTROLLER->GET_SOURCE_SHIPMENTS_ROW( ) IS INITIAL ).
*      IF ME->GET_SPLITTER_REFERENCE( ) IS NOT INITIAL.
*        CALL METHOD ME->GET_SPLITTER_REFERENCE(
*                        )->SET_COLUMN_WIDTH( ID = 1 WIDTH = 100 ).
*      ENDIF.
*
*    ELSE.
    me->handle_display_shipments(
      e_row = VALUE #( index = me->shipments_controller->get_source_shipments_row( ) )
    ).
*    ENDIF.

    "//Display Data Items
    me->display_plannings( ).

    CALL METHOD me->set_selected_row(
           COND #( WHEN me->model->get_row_reference( ) IS NOT INITIAL
                   THEN me->model->get_row_reference( ) ) ).
  ENDMETHOD.

  METHOD create_model.
    "// Create Model Instance
    TRY.
        model ?= zsapmvc_model=>create_model( model_reference = 'DISPLAY_MODEL' repid = sy-repid ).
      CATCH zcx_sapmvc.
    ENDTRY.
  ENDMETHOD.

  METHOD user_command.
    me->set_action( ucomm ).

    CASE ucomm.
      WHEN 'INFO'.
        me->model->info_error_message( ).
      WHEN 'SELECT'.
        DATA field TYPE string.
        GET CURSOR FIELD field.

        CASE field.
          WHEN 'GW_ORDER-ORDEM'.
            SET PARAMETER ID 'AUN' FIELD gw_order-ordem.
            CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
        ENDCASE.

      WHEN 'BTN_CLOSE_VIEW'.
        me->shipments_controller->refresh_source_shipments( ).

        me->main_controller->set_display_dynnr( '0003' ).
        me->main_controller->refresh_screen( ).

        me->get_splitter_reference( )->set_column_width(
            id = 1 width = 100 ).

      WHEN 'BTN_SHOW_HIDE_VIEW'.
        DATA(_extension) = me->main_controller->get_dock_extension( ).

        me->main_controller->set_dock_extension(
          COND #( WHEN _extension >= 1300 THEN 160
                  WHEN _extension >= 160  THEN 1300
                  WHEN _extension  < 160  THEN 160 )
        ).
      WHEN 'SAVE_PLANNING'.

        DATA(hora) =
        me->model->services->planning_production(
           )->get_last_time( gw_order-ordem ).

        IF hora < 500.
          MESSAGE s000(zppm001) WITH 'Não é possivel Modificar, Aguarde 5 minutos!' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.

        "//Services
        me->model->services->planning_production(
           )->select_order_header( vbeln = gw_order-ordem ).

        me->model->services->planning_production(
           )->select_order_item( vbeln = gw_order-ordem
                                 posnr = gw_order-item ).

        me->model->services->planning_production(
           )->select_order_business( vbeln = gw_order-ordem ).

        me->model->services->planning_production(
           )->select_order_partners( ordem = gw_order-ordem ).

        me->model->services->planning_production(
           )->select_simulador_venda_header( gw_order-ordem ).
        "//

        CALL METHOD me->model->save_planning
          EXPORTING
            solicitations = main_controller->model->get_authorized_orders( ).

        TRY.
            zsapmvc_view=>activate_change_notification( model_instance = me->model
                                                        view_instance  = view
                                                        timeout        = '1'
                                                        repeat         = abap_true ).
          CATCH zcx_sapmvc.
        ENDTRY.

    ENDCASE.
  ENDMETHOD.

  METHOD call_screen.
    CALL SCREEN dynpro STARTING AT starting_column starting_line
                       ENDING   AT ending_column ending_line.
  ENDMETHOD.

*  METHOD SET_DYNPRO.
*    MOVE: REPID TO PROGRAM,
*          DYNNR TO DYNPRO.
*  ENDMETHOD.

  METHOD display_plannings.
    "//Display Plannings datas
    IF ( me->custom IS NOT BOUND ).
      CREATE OBJECT me->custom
        EXPORTING
          container_name = 'CONTAINER'.

      CREATE OBJECT me->splitter
        EXPORTING
          link_dynnr = sy-dynnr
          link_repid = sy-repid
          parent     = me->custom
          rows       = 1
          columns    = 2.

      "//Get Custom Items
      me->splitter->set_column_width( id = 1 width = 100 ).

      DATA(_custom_plannings) =
        me->splitter->get_container(
        row    = 1
        column = 1 ).
    ENDIF.

    IF ( me->alv_table IS NOT BOUND ).

      "//Get Field Catalog
      DATA(fieldcat) = me->get_fieldcatalog( ).

      "//Create Alv Object
      CREATE OBJECT me->alv_table
        EXPORTING
          i_parent = _custom_plannings.

      me->layout-sel_mode   = sy-abcde(1).
      me->layout-stylefname = 'STYLE'.
      me->layout-no_vgridln = abap_true.

      SET HANDLER:
      me->handle_set_toolbar       FOR me->alv_table,
      me->handle_user_command      FOR me->alv_table,
      me->handle_data_changed      FOR me->alv_table,
      me->handle_display_shipments FOR me->alv_table.

      "//Display Datas
      CALL METHOD me->alv_table->set_table_for_first_display
        EXPORTING
          is_layout       = me->layout
          i_save          = abap_on
        CHANGING
          it_outtab       = gt_plannings
          it_fieldcatalog = fieldcat.

      "//Register Modify Events
      CALL METHOD me->alv_table->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD me->alv_table->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ELSE.
      me->refresh_screen( ).
    ENDIF.
  ENDMETHOD.

  METHOD display_solicitations.
    DATA lo_alv     TYPE REF TO cl_salv_table.
    DATA lo_column  TYPE REF TO cl_salv_column.
    DATA lo_columns TYPE REF TO cl_salv_columns.
    DATA lo_aggregations TYPE REF TO cl_salv_aggregations.
    DATA lo_display TYPE REF TO cl_salv_display_settings.

    DATA(_items)  = me->model->get_solicitations( ).

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = _items ).

        DATA(_events)   = lo_alv->get_event( ).

        lo_display      = lo_alv->get_display_settings( ).
        selections      = lo_alv->get_selections( ).
        lo_aggregations = lo_alv->get_aggregations( ).
        lo_columns      = lo_alv->get_columns( ).

        CALL METHOD lo_alv->set_screen_popup
          EXPORTING
            start_column = 20
            end_column   = 75
            start_line   = 5
            end_line     = 15.

        lo_column ?= lo_columns->get_column( columnname = 'NRO_SOL' ).
        lo_column->set_medium_text( 'Número' ).

        lo_column ?= lo_columns->get_column( columnname = 'SEQ_LIB' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column ?= lo_columns->get_column( columnname = 'DT_LIBER' ).
        lo_column->set_medium_text( 'Data Liberação' ).

        lo_column ?= lo_columns->get_column( columnname = 'USUARIO_LIB' ).
        lo_column->set_medium_text( 'Usuário' ).

        lo_column ?= lo_columns->get_column( columnname = 'QTE_LIB' ).
        lo_column->set_medium_text( 'Qtd Liberada' ).

        lo_column ?= lo_columns->get_column( columnname = 'DT_ENTREGA' ).
        lo_column->set_medium_text( 'Data Entrega' ).

        lo_columns->set_optimize( ).

        CALL METHOD lo_aggregations->add_aggregation
          EXPORTING
            columnname  = 'QTE_LIB'
            aggregation = if_salv_c_aggregation=>total.

        CALL METHOD lo_alv->set_screen_status
          EXPORTING
            report   = sy-repid
            pfstatus = 'STATUS_0004'.

        lo_display->set_list_header( 'Histórico de solicitações de embarque' ).
        selections->set_selection_mode( if_salv_c_selection_mode=>single ).

        SET HANDLER me->handle_salv_user_command FOR _events.
        lo_alv->display( ).

      CATCH cx_salv_wrong_call
            cx_salv_data_error
            cx_salv_not_found
            cx_salv_existing
            cx_salv_msg.
    ENDTRY.
  ENDMETHOD.

  METHOD dependency_injection.
    IF ( me->shipments_controller IS NOT BOUND ).

      TRY.
          zsapmvc_controller=>create_controller(
            EXPORTING
              repid                = program
              dynnr                = '0005'
              controller_reference = 'SHIPMENTS_CONTROLLER'
          ).

        CATCH zcx_sapmvc.
      ENDTRY.

    ENDIF.
  ENDMETHOD.

  METHOD refresh_screen.
    alv_table->refresh_table_display( is_stable = VALUE #( row = abap_true col = abap_true ) ).
  ENDMETHOD.

  METHOD set_action.
    MOVE action TO me->model->action.
  ENDMETHOD.

  METHOD get_action.
    MOVE me->model->action TO value.
  ENDMETHOD.

  METHOD set_selected_row.
    CALL METHOD me->alv_table->set_selected_rows
      EXPORTING
        it_index_rows = VALUE #( ( index = r_value ) ).
  ENDMETHOD.

  METHOD get_splitter_reference.
    MOVE me->splitter TO ref.
  ENDMETHOD.

  METHOD get_fieldcatalog.
    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'IOPUS'
        description = 'Status'
        outputlen   = 6
        just        = 'C'
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'EDATU'
        description = 'Data da Remessa'
        outputlen   = 15
        edit        = abap_on
        ref_table   = 'ZPPT0008'
        ref_field   = 'EDATU'
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'WMENG'
        description = 'Qtd. Planejada'
        outputlen   = 15
        edit        = abap_on
        sum         = abap_true
        quantity    = 'KG'
        ref_table   = 'AFPO'
        ref_field   = 'WEMNG'
        just        = 'C'
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'WCARR'
        description = 'Qtd. Consumida'
        outputlen   = 15
        sum         = abap_true
        quantity    = 'KG'
        ref_table   = 'AFPO'
        ref_field   = 'WEMNG'
        just        = 'C'
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'WDISP'
        description = 'Qtd. À Carregar'
        outputlen   = 15
        sum         = abap_true
        quantity    = 'KG'
        ref_table   = 'AFPO'
        ref_field   = 'WEMNG'
        just        = 'C'
      CHANGING
        r_table     = r_table.

    CALL METHOD cl_utils=>build_fieldcatalog
      EXPORTING
        fieldname   = 'QTD_CAMINHOES'
        description = ''
        outputlen   = 7
      CHANGING
        r_table     = r_table.
  ENDMETHOD.

  METHOD handle_set_toolbar.

    "//Check planner access;
    AUTHORITY-CHECK OBJECT 'Z_PLANNING' ID 'ACTVT' FIELD '72'.

    IF sy-subrc IS INITIAL.
      "//Set Buttons That Is Going To Appear In Alv
      e_object->mt_toolbar = VALUE #( ( butn_type = 0
                                        function  = 'NEW_PLANNING'
                                        icon      = icon_create
                                        text      = 'Criar Planejamento'
                                      )
                                      "//Separator
                                      ( butn_type = 3 )
                                      ( butn_type = 0
                                        function  = 'SEND_PLANNING_TO_OPUS'
                                        icon      = icon_submit
                                        text      = 'Enviar para o Opus'
                                      )
                                      (
                                        butn_type = 0
                                        function  = 'SOLICITATIONS'
                                        icon      = icon_viewer_optical_archive
                                        text      = 'Visualizar Solicitações' )

                                      "//Separator
                                      ( butn_type = 3 )

                                      (
                                        butn_type = 0
                                        function  = 'EDIT'
                                        icon      = icon_change
                                        quickinfo = 'Editar' )

                                      ( butn_type = 0
                                        function  = 'DELETE'
                                        icon      = icon_delete_row
                                        quickinfo = 'Deletar' )
                                      ).
    ELSE.
      e_object->mt_toolbar = VALUE #( ( butn_type = 0
                                        function  = 'SOLICITATIONS'
                                        icon      = icon_viewer_optical_archive
                                        text      = 'Visualizar Solicitações' )
                                      ).
    ENDIF.
  ENDMETHOD.

  METHOD handle_user_command.
    CALL METHOD me->alv_table->get_selected_rows
      IMPORTING
        et_index_rows = DATA(selected_row).

    me->set_action( e_ucomm ).

    CASE e_ucomm.
      WHEN 'SEND_PLANNING_TO_OPUS'.
        me->model->services->planning_production(
           )->select_order_header( gw_order-ordem ).

        me->model->services->planning_production(
           )->select_order_item( vbeln = gw_order-ordem
                                 posnr = gw_order-item ).

        me->model->services->planning_production(
           )->select_order_business( gw_order-ordem ).

        me->model->services->planning_production(
           )->select_order_partners( gw_order-ordem ).

        me->model->services->planning_production(
           )->select_simulador_venda_header( gw_order-ordem ).

        me->model->send_planning_to_opus(
          selected_row = selected_row
        ).

        TRY.
            zsapmvc_view=>activate_change_notification( model_instance = me->model
                                                        view_instance  = view
                                                        timeout        = '1'
                                                        repeat         = abap_true ).
          CATCH zcx_sapmvc.
        ENDTRY.

      WHEN 'NEW_PLANNING'.
        me->model->new_planning( ).
      WHEN 'SOLICITATIONS'.
        me->model->set_solicitations(
            me->main_controller->model->get_authorized_orders( )  ).

        me->display_solicitations( ).
      WHEN 'EDIT'.
        me->model->modify_planning_row( selected_row ).
      WHEN 'DELETE'.
        SORT selected_row BY index DESCENDING.
        me->model->delete_planning_row( selected_row ).
      WHEN OTHERS.
    ENDCASE.

    IF e_ucomm NE 'SEND_PLANNING_TO_OPUS'.
      "// Setup Repeat change Notification
      TRY.
          zsapmvc_view=>activate_change_notification( model_instance = me->model
                                                      view_instance  = view
                                                      timeout        = '30'
                                                      repeat         = abap_true ).
        CATCH zcx_sapmvc.
      ENDTRY.
    ENDIF.

    me->refresh_screen( ).
  ENDMETHOD.

  METHOD handle_salv_user_command.
    CASE e_salv_function.
      WHEN 'EXIT'.
        LEAVE TO SCREEN 0.
      WHEN 'VIEW_ROTE'.
        DATA(_rows)  = selections->get_selected_rows( ).
        DATA(_items) = me->model->get_solicitations( ).

        IF _rows IS NOT INITIAL.
          LOOP AT _rows INTO DATA(_row).
            DATA(_item) = _items[ _row ].

            me->model->get_trip_guide(
              EXPORTING
                no_solicitation = _item-nro_sol
                order           = gw_order-ordem
                item            = gw_order-item
              RECEIVING
                table           = DATA(lines)
            ).
          ENDLOOP.

          IF lines IS NOT INITIAL.
            DATA(_title) = CONV sytitle( |Roteiro: Solicitação nº - { _item-nro_sol }| ).
            DATA(_mode)  = CONV xfeld( abap_true ).

            CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
              EXPORTING
                im_title        = _title
                im_display_mode = _mode
              CHANGING
                ch_text         = lines.
          ELSE.
            MESSAGE TEXT-030 TYPE 'S' DISPLAY LIKE 'E'.
          ENDIF.

        ELSE.
          MESSAGE TEXT-031 TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_data_changed.
    me->model->update_planning_row(
      line_changed = er_data_changed->mt_good_cells
      order_items  = me->main_controller->model->get_order_items( ) ).

    me->refresh_screen( ).
  ENDMETHOD.

  METHOD handle_display_shipments.
    CHECK e_row-index IS NOT INITIAL.

    DATA(_item_plannings)  = me->model->get_plannings( ).
    TRY.
        DATA(_item) = _item_plannings[ e_row-index ].

        IF NOT ( _item-t_shipments IS INITIAL )
        OR NOT ( _item-t_transits  IS INITIAL ).

          CALL METHOD me->shipments_controller->set_source_shipments_row
            EXPORTING
              row = e_row-index.

          me->shipments_controller->model->set_shipments(
            new      = abap_true
            transits = _item-t_transits
            items    = _item-t_shipments
          ).

          me->shipments_controller->set_splitter_reference(
            me->get_splitter_reference( ) ).

          me->shipments_controller->set_title(
            |{ TEXT-025 } { cl_utils=>convert_date_external( date = _item-edatu ) }|
          ).

          me->shipments_controller->display( ).

        ELSE.
          me->get_splitter_reference( )->set_column_width( id = 1 width = 100 ).
          me->shipments_controller->refresh_source_shipments( ).

          MESSAGE |{ TEXT-026 } { cl_utils=>convert_date_external( date = _item-edatu ) }.|
             TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      CATCH cx_sy_itab_line_not_found.
        me->get_splitter_reference( )->set_column_width( id = 1 width = 100 ).
    ENDTRY.
  ENDMETHOD.


ENDCLASS.

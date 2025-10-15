*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_1012 .
*----------------------------------------------------------------------*

DATA: ck_nivel_definido   TYPE c LENGTH 1,
      wa_zglt047_alterado TYPE zglt047.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_node_double_click
        FOR EVENT node_double_click
        OF cl_gui_list_tree
        IMPORTING node_key,
      handle_expand_no_children
        FOR EVENT expand_no_children
        OF cl_gui_list_tree
        IMPORTING node_key,
      handle_item_double_click
        FOR EVENT item_double_click
        OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_button_click
        FOR EVENT button_click
        OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_link_click
        FOR EVENT link_click
        OF cl_gui_list_tree
        IMPORTING node_key item_name,
      handle_checkbox_change
        FOR EVENT checkbox_change
        OF cl_gui_list_tree
        IMPORTING node_key item_name checked.

ENDCLASS.                    "LCL_APPLICATION DEFINITION



*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application IMPLEMENTATION.
  METHOD  handle_node_double_click.
    " this method handles the node double click event of the tree
    " control instance

    " show the key of the double clicked node in a dynpro field
    g_event = 'NODE_DOUBLE_CLICK'.
    g_node_key = node_key.

    CLEAR: it_nodes[].

    CALL METHOD g_tree->get_expanded_nodes
      CHANGING
        node_key_table = it_nodes.

    LOOP AT it_nodes INTO wa_nodes.
      IF wa_nodes EQ g_node_key.
        CLEAR: g_node_key.
        EXIT.
      ENDIF.
    ENDLOOP.

    CLEAR: it_nodes[].

    wa_nodes = node_key.
    APPEND wa_nodes TO it_nodes.

    IF g_node_key IS INITIAL.
      CALL METHOD g_tree->collapse_nodes
        EXPORTING
          node_key_table = it_nodes.
    ELSE.
      CALL METHOD g_tree->expand_nodes
        EXPORTING
          node_key_table = it_nodes.
    ENDIF.

  ENDMETHOD.                    "HANDLE_NODE_DOUBLE_CLICK

  METHOD  handle_item_double_click.
    " this method handles the item double click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the double clicked item in a dynpro field
    g_event = 'ITEM_DOUBLE_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  ENDMETHOD.                    "HANDLE_ITEM_DOUBLE_CLICK

  METHOD  handle_link_click.
    " this method handles the link click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked link in a dynpro field
    g_event = 'LINK_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  ENDMETHOD.                    "HANDLE_LINK_CLICK

  METHOD  handle_button_click.
    " this method handles the button click event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked button in a dynpro field
    g_event = 'BUTTON_CLICK'.
    g_node_key = node_key.
    g_item_name = item_name.
  ENDMETHOD.                    "HANDLE_BUTTON_CLICK

  METHOD  handle_checkbox_change.
    " this method handles the checkbox_change event of the tree
    " control instance

    " show the key of the node and the name of the item
    " of the clicked checkbox in a dynpro field
    g_event = 'CHECKBOX_CHANGE'.
    g_node_key = node_key.
    g_item_name = item_name.
  ENDMETHOD.                    "HANDLE_CHECKBOX_CHANGE


  METHOD handle_expand_no_children.
    DATA: node_table TYPE treev_ntab,
          node       TYPE treev_node,
          item_table TYPE item_table_type,
          item       TYPE mtreeitm.

    g_event = 'EXPAND_NO_CHILDREN'.
    g_node_key = node_key.

    CALL METHOD g_tree->add_nodes_and_items
      EXPORTING
        node_table                     = node_table
        item_table                     = item_table
        item_table_structure_name      = 'MTREEITM'
      EXCEPTIONS
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6.
    IF sy-subrc <> 0.
      MESSAGE a000.
    ENDIF.
  ENDMETHOD.                    "HANDLE_EXPAND_NO_CHILDREN

ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_DRAGDROP_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_flavor_select
        FOR EVENT on_drop_get_flavor
        OF cl_gui_list_tree
        IMPORTING node_key flavors drag_drop_object.
ENDCLASS.                    "LCL_DRAGDROP_RECEIVER DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_DRAGDROP_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD handle_flavor_select.
    DATA: wa_dest TYPE ty_zglt047_alv,
          wa_orig TYPE ty_zglt047_alv.

    READ TABLE it_zglt047_alv INTO wa_dest WITH KEY node_key = node_key.
    IF sy-subrc IS INITIAL.
      CALL METHOD g_tree->get_selected_node
        IMPORTING
          node_key                   = g_node_key
        EXCEPTIONS
          failed                     = 1
          single_node_selection_only = 2
          cntl_system_error          = 3
          OTHERS                     = 4.
      READ TABLE it_zglt047_alv INTO wa_orig WITH KEY node_key = g_node_key.
      IF sy-subrc IS INITIAL.
        IF ( wa_orig-nivelpai EQ wa_dest-nivelpai ) AND ( wa_orig-nivel NE wa_dest-nivel ).
          MOVE-CORRESPONDING wa_orig TO it_zglt047.
          it_zglt047-sqnivel = wa_dest-sqnivel.
          PERFORM ajusta_nivel USING it_zglt047.
          MODIFY zglt047 FROM it_zglt047.
          MOVE-CORRESPONDING it_zglt047 TO wa_zglt047_alterado.
          COMMIT WORK.

          CALL METHOD g_tree->delete_all_nodes.

          PERFORM atualiza_tabela_niveis USING it_zglt046_alv-versn.

          PERFORM create_and_init_tree.

          PERFORM build_node_and_item_table USING node_table item_table handle_tree.

          CALL METHOD g_tree->add_nodes_and_items
            EXPORTING
              node_table                     = node_table
              item_table                     = item_table
              item_table_structure_name      = 'MTREEITM'
            EXCEPTIONS
              failed                         = 1
              cntl_system_error              = 3
              error_in_tables                = 4
              dp_error                       = 5
              table_structure_name_not_found = 6.

          IF NOT sy-subrc IS INITIAL.
            MESSAGE a000(tree_control_msg).
          ENDIF.

          READ TABLE it_zglt047_alv INTO wa_orig
          WITH KEY versn = wa_zglt047_alterado-versn
                   nivel = wa_zglt047_alterado-nivel.
          IF sy-subrc IS INITIAL.
            CALL METHOD g_tree->set_selected_node
              EXPORTING
                node_key = wa_orig-node_key.
          ENDIF.
          CLEAR: wa_zglt047_alterado.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "HANDLE_FLAVOR_SELECT

ENDCLASS.                    "LCL_DRAGDROP_RECEIVER IMPLEMENTATION

DATA: prim_busca_alv    TYPE c LENGTH 1 VALUE abap_true,
      prim_bal_nivel    TYPE c LENGTH 1 VALUE abap_true,
      prim_bal_nivel_re TYPE c LENGTH 1 VALUE abap_true,
      g_application     TYPE REF TO lcl_application.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1012  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_1012 OUTPUT.

  "Criar ALV Tree View
  DATA: node_esp TYPE tv_nodekey.

  IF prim_busca_alv EQ abap_true.
    PERFORM atualiza_tabela_niveis USING it_zglt046_alv-versn.
    prim_busca_alv = abap_false.
  ENDIF.

  IF prim_bal_nivel EQ abap_true.
    CREATE OBJECT g_application.
    PERFORM create_and_init_tree.
    prim_bal_nivel = abap_false.
  ENDIF.

  IF prim_bal_nivel_re EQ abap_true.

    IF g_behaviour IS INITIAL.

      CREATE OBJECT g_behaviour.

      CALL METHOD g_behaviour->add
        EXPORTING
          flavor     = 'MOVER'
          dragsrc    = abap_true
          droptarget = abap_true
          effect     = cl_dragdrop=>copy.

      CALL METHOD g_behaviour->add
        EXPORTING
          flavor     = 'COPIAR'
          dragsrc    = abap_true
          droptarget = abap_true
          effect     = cl_dragdrop=>copy.

    ENDIF.

    CALL METHOD g_behaviour->get_handle
      IMPORTING
        handle = handle_tree.

    PERFORM build_node_and_item_table USING node_table item_table handle_tree.

    CALL METHOD g_tree->add_nodes_and_items
      EXPORTING
        node_table                     = node_table
        item_table                     = item_table
        item_table_structure_name      = 'MTREEITM'
      EXCEPTIONS
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6.

    IF NOT sy-subrc IS INITIAL.
      MESSAGE a000(tree_control_msg).
    ENDIF.

    CALL METHOD g_tree->expand_root_nodes.

    IF wa_zglt047_alterado IS NOT INITIAL.
      READ TABLE it_zglt047_alv
      WITH KEY versn = wa_zglt047_alterado-versn
               nivel = wa_zglt047_alterado-nivel.
      IF sy-subrc IS INITIAL.
        CALL METHOD g_tree->set_selected_node
          EXPORTING
            node_key = it_zglt047_alv-node_key.
      ENDIF.
    ENDIF.

    prim_bal_nivel_re = abap_false.

  ENDIF.

ENDMODULE.                 " STATUS_1012  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1012  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1012 INPUT.

  DATA: node_key TYPE tv_nodekey.

  CASE ok_code_1001.
    WHEN ok_expandir.
      "**********************************************************************************
      """""""" Expandir Estrutura """""""""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: it_nodes[].

      LOOP AT it_zglt047_alv.
        wa_nodes = it_zglt047_alv-node_key.
        APPEND wa_nodes TO it_nodes.
      ENDLOOP.

      CALL METHOD g_tree->expand_nodes
        EXPORTING
          node_key_table = it_nodes.
      "**********************************************************************************

    WHEN ok_collapse.
      "**********************************************************************************
      """""""" Recolher Estrutura """""""""""""""""""""""""""""""""""""""""""""""""""""""
      CALL METHOD g_tree->collapse_all_nodes.
      "**********************************************************************************

    WHEN ok_inc_no.
      "**********************************************************************************
      """""""" Incluir Nível """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CLEAR: it_zglt047.
      it_zglt047-versn = it_zglt046_alv-versn.
      CALL SCREEN 1013 STARTING AT 10 10.
      PERFORM limpar_tela_tree.
      "**********************************************************************************

    WHEN ok_editar.
      "**********************************************************************************
      """""""" Editar Nível """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      CALL METHOD g_tree->get_selected_node
        IMPORTING
          node_key                   = node_key
        EXCEPTIONS
          failed                     = 1
          single_node_selection_only = 2
          cntl_system_error          = 3
          OTHERS                     = 4.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSEIF node_key IS NOT INITIAL.
        "Alterando Nível.
        ck_editar = abap_true.
        READ TABLE it_zglt047_alv WITH KEY node_key = node_key.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING it_zglt047_alv TO it_zglt047.
          CALL SCREEN 1013 STARTING AT 10 10.
          PERFORM limpar_tela_tree.
        ELSE.
          MESSAGE s005 WITH it_zglt046_alv-versn.
        ENDIF.
      ELSE.
        MESSAGE s004 WITH it_zglt046_alv-versn.
      ENDIF.

      "**********************************************************************************

    WHEN ok_excluir.
      "**********************************************************************************
      """""""" Excluir Nível """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      CALL METHOD g_tree->get_selected_node
        IMPORTING
          node_key                   = node_key
        EXCEPTIONS
          failed                     = 1
          single_node_selection_only = 2
          cntl_system_error          = 3
          OTHERS                     = 4.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSEIF node_key IS NOT INITIAL.
        "Alterando Nível.
        READ TABLE it_zglt047_alv WITH KEY node_key = node_key.
        IF sy-subrc IS INITIAL.
          PERFORM excluir_nivel_estrutura USING it_zglt047_alv.
          CLEAR: wa_zglt047_alterado.
          IF it_zglt047_alv-nivelpai IS NOT INITIAL.
            wa_zglt047_alterado-versn = it_zglt047_alv-versn.
            wa_zglt047_alterado-nivel = it_zglt047_alv-nivelpai.
          ENDIF.
          PERFORM limpar_tela_tree.
          EXIT.
        ENDIF.

        READ TABLE it_zglt048_alv WITH KEY node_key = node_key.
        IF sy-subrc IS INITIAL.
          PERFORM excluir_nivel_somatorio USING it_zglt048_alv.
          MOVE-CORRESPONDING it_zglt048_alv TO wa_zglt047_alterado.
          PERFORM limpar_tela_tree.
          EXIT.
        ENDIF.

        READ TABLE it_zglt049_alv WITH KEY node_key = node_key.
        IF sy-subrc IS INITIAL.
          PERFORM excluir_nivel_nota USING it_zglt049_alv.
          READ TABLE it_zglt047_alv WITH KEY node_key = it_zglt049_alv-node_key_c.
          MOVE-CORRESPONDING it_zglt047_alv TO wa_zglt047_alterado.
          PERFORM limpar_tela_tree.
          EXIT.
        ENDIF.

      ELSE.
        MESSAGE s004 WITH it_zglt046_alv-versn.
      ENDIF.
      "**********************************************************************************

    WHEN ok_inc_up OR ok_inc_down OR ok_inc_right OR ok_inc_left.
      "**********************************************************************************
      """""""" Incluir Nível posição """"""""""""""""""""""""""""""""""""""""""""""""""""
      CALL METHOD g_tree->get_selected_node
        IMPORTING
          node_key                   = node_key
        EXCEPTIONS
          failed                     = 1
          single_node_selection_only = 2
          cntl_system_error          = 3
          OTHERS                     = 4.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSEIF node_key IS NOT INITIAL.
        "Incluindo
        READ TABLE it_zglt047_alv WITH KEY node_key = node_key.
        IF sy-subrc IS INITIAL.
          CLEAR: it_zglt047.
          it_zglt047-versn  = it_zglt047_alv-versn.
          ck_nivel_definido = abap_true.

          CASE ok_code_1001.
            WHEN ok_inc_up.
              it_zglt047-sqnivel  = it_zglt047_alv-sqnivel.
              it_zglt047-nivelpai = it_zglt047_alv-nivelpai.
            WHEN ok_inc_down.
              it_zglt047-sqnivel  = it_zglt047_alv-sqnivel.
              it_zglt047-nivelpai = it_zglt047_alv-nivelpai.
              ADD 1 TO it_zglt047-sqnivel.
            WHEN ok_inc_right.
              IF it_zglt047_alv-nivelsum EQ abap_true.
                CLEAR: it_zglt048.
                it_zglt048-versn = it_zglt047_alv-versn.
                it_zglt048-nivel = it_zglt047_alv-nivel.
                CALL SCREEN 1014 STARTING AT 10 10.
                PERFORM limpar_tela_tree.
                EXIT.
              ELSE.
                SELECT MAX( sqnivel ) INTO it_zglt047-sqnivel
                  FROM zglt047
                 WHERE versn    EQ it_zglt047_alv-versn
                   AND nivelpai EQ it_zglt047_alv-nivel.
                ADD 1 TO it_zglt047-sqnivel.
                it_zglt047-nivelpai = it_zglt047_alv-nivel.
              ENDIF.
            WHEN ok_inc_left.

          ENDCASE.

          CALL SCREEN 1013 STARTING AT 10 10.
          PERFORM limpar_tela_tree.
        ELSE.
          MESSAGE s005 WITH it_zglt046_alv-versn.
        ENDIF.
      ELSE.
        MESSAGE s002.
      ENDIF.
      "**********************************************************************************
    WHEN ok_add_nota.

      "**********************************************************************************
      """""""" Incluir Nota de Classificação """"""""""""""""""""""""""""""""""""""""""""
      CALL METHOD g_tree->get_selected_node
        IMPORTING
          node_key                   = node_key
        EXCEPTIONS
          failed                     = 1
          single_node_selection_only = 2
          cntl_system_error          = 3
          OTHERS                     = 4.

      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSEIF node_key IS NOT INITIAL.
        READ TABLE it_zglt047_alv WITH KEY node_key = node_key.
        IF sy-subrc IS INITIAL.
          CLEAR: it_zglt049.
          MOVE-CORRESPONDING it_zglt047_alv TO it_zglt049.
          SELECT MAX( sqnivel ) INTO it_zglt049-sqnivel
            FROM zglt049
           WHERE versn EQ it_zglt047_alv-versn
             AND nivel EQ it_zglt047_alv-nivel.
          ADD 1 TO it_zglt049-sqnivel.
          CALL SCREEN 1020 STARTING AT 10 10.
          PERFORM limpar_tela_tree.
        ELSE.
          MESSAGE s005 WITH it_zglt046_alv-versn.
        ENDIF.
      ELSE.
        MESSAGE s004 WITH it_zglt046_alv-versn.
      ENDIF.
      "**********************************************************************************

    WHEN ok_clbalanc.
      "**********************************************************************************
      """""""" Incluir Nota de Classificação """"""""""""""""""""""""""""""""""""""""""""
      CALL SCREEN 1015 STARTING AT 10 10.
      "PERFORM LIMPAR_TELA_TREE.
      "**********************************************************************************

    WHEN ok_atualiza.
      "**********************************************************************************
      """""""" Atualizar Estrutura """"""""""""""""""""""""""""""""""""""""""""""""""""""
      PERFORM atualiza_tabela_niveis USING it_zglt046_alv-versn.
      "**********************************************************************************

    WHEN ok_back.
      "**********************************************************************************
      """""""" Limpar Tela Voltar """""""""""""""""""""""""""""""""""""""""""""""""""""""
      PERFORM limpar_tela_tree.
      "**********************************************************************************

    WHEN ok_verificar.
      "**********************************************************************************
      """""""" Ativar analise de Estrutura """"""""""""""""""""""""""""""""""""""""""""""
      ck_analisa_objetos = abap_true.
      PERFORM atualiza_tabela_niveis USING it_zglt046_alv-versn.
      "**********************************************************************************

    WHEN ok_desativar.
      "**********************************************************************************
      """""""" Desativar analise de Estrutura """""""""""""""""""""""""""""""""""""""""""
      ck_analisa_objetos = abap_false.
      PERFORM atualiza_tabela_niveis USING it_zglt046_alv-versn.
      "**********************************************************************************

  ENDCASE.

ENDMODULE.                 " USER_COMMAND_1012  INPUT

*&---------------------------------------------------------------------*
*&      Form  CREATE_AND_INIT_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_and_init_tree .

  DATA: events TYPE cntl_simple_events,
        event  TYPE cntl_simple_event.

* create a container for the tree control
  CREATE OBJECT g_custom_container
    EXPORTING
      container_name              = 'TREE_CONTAINER'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  IF sy-subrc <> 0.
    MESSAGE a000(tree_control_msg).
  ENDIF.

* create a list tree
  CREATE OBJECT g_tree
    EXPORTING
      parent                      = g_custom_container
      node_selection_mode         = cl_gui_list_tree=>node_sel_mode_single
      item_selection              = 'X'
      with_headers                = ' '
    EXCEPTIONS
      cntl_system_error           = 1
      create_error                = 2
      failed                      = 3
      illegal_node_selection_mode = 4
      lifetime_error              = 5.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE a000(tree_control_msg).
  ENDIF.

* define the events which will be passed to the backend
  " node double click
  event-eventid = cl_gui_list_tree=>eventid_node_double_click.
  event-appl_event = 'X'.                                   "
  APPEND event TO events.

  " item double click
  event-eventid = cl_gui_list_tree=>eventid_item_double_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " expand no children
  event-eventid = cl_gui_list_tree=>eventid_expand_no_children.
  event-appl_event = 'X'.
  APPEND event TO events.

  " link click
  event-eventid = cl_gui_list_tree=>eventid_link_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " button click
  event-eventid = cl_gui_list_tree=>eventid_button_click.
  event-appl_event = 'X'.
  APPEND event TO events.

  " checkbox change
  event-eventid = cl_gui_list_tree=>eventid_checkbox_change.
  event-appl_event = 'X'.
  APPEND event TO events.

* assign event handlers in the application class to each desired event
  "DATA DRAGDROP TYPE REF TO LCL_DRAGDROP_RECEIVER.
  "CREATE OBJECT DRAGDROP.
  SET HANDLER g_application->handle_node_double_click  FOR g_tree.
  SET HANDLER g_application->handle_item_double_click  FOR g_tree.
  SET HANDLER g_application->handle_expand_no_children FOR g_tree.
  SET HANDLER g_application->handle_link_click         FOR g_tree.
  SET HANDLER g_application->handle_button_click       FOR g_tree.
  SET HANDLER g_application->handle_checkbox_change    FOR g_tree.
  "SET HANDLER DRAGDROP->HANDLE_FLAVOR_SELECT           FOR G_TREE.

  CALL METHOD g_tree->set_registered_events
    EXPORTING
      events                    = events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.

  IF NOT sy-subrc IS INITIAL.
    MESSAGE a000(tree_control_msg).
  ENDIF.

ENDFORM.                    " CREATE_AND_INIT_TREE

*&---------------------------------------------------------------------*
*&      Form  BUILD_NODE_AND_ITEM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_TABLE  text
*      -->P_ITEM_TABLE  text
*----------------------------------------------------------------------*
FORM build_node_and_item_table  USING node_table  TYPE treev_ntab
                                      item_table  TYPE item_table_type
                                      handle_tree TYPE i.

  DATA: node TYPE treev_node,
        item TYPE mtreeitm.

  CLEAR: node_table,
         item_table[].

  LOOP AT it_zglt047_alv.

    CLEAR: node-relatkey,
           node-relatship,
           node-expander,
           node-n_image,
           node-exp_image,
           node-dragdropid.

    IF it_zglt047_alv-nivelpai IS INITIAL.
      node-n_image   = icon_tree.
      node-exp_image = icon_previous_node.
    ENDIF.

    IF it_zglt047_alv-nivelsum EQ abap_true.
      node-n_image   = icon_sum.
      node-exp_image = icon_sum.
    ENDIF.

    node-hidden    = ' '.    " The node is visible,
    node-disabled  = ' '.    " selectable,
    node-isfolder  = 'X'.    " a folder.
    node-node_key  = it_zglt047_alv-node_key.
    IF it_zglt047_alv-node_key_c IS NOT INITIAL.
      node-relatkey   = it_zglt047_alv-node_key_c.
      node-relatship  = cl_gui_list_tree=>relat_last_child.
      node-dragdropid = handle_tree.
    ENDIF.
    APPEND node TO node_table.

    CLEAR item.
    item-node_key   = node-node_key.
    item-item_name  = '1'."p_node-node_key. " Item with name '1'
    item-class      = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment  = cl_gui_list_tree=>align_auto.
    item-font       = cl_gui_list_tree=>item_font_prop.
    item-text       = it_zglt047_alv-nivel.
    "item-usebgcolor = 'X'.
    APPEND item TO item_table.

    item-item_name = '2'."p_node-node_key. " Item with name '1'
    item-text      = it_zglt047_alv-desnvl.
    APPEND item TO item_table.

    "Inclui item de Nota """"""""""""""""""""""""""""""""""""""""""""
    "*********************************************************************
    LOOP AT it_zglt049_alv WHERE node_key_c = it_zglt047_alv-node_key.

      CLEAR: node-relatkey,
             node-relatship,
             node-expander,
             node-n_image,
             node-exp_image,
             node-dragdropid.

      node-n_image   = icon_packing."ICON_POSITIVE.
      node-exp_image = icon_unpack."ICON_POSITIVE.
      node-hidden    = ' '.    " The node is visible,
      node-disabled  = ' '.    " selectable,
      node-isfolder  = 'X'.    " a folder.
      node-node_key  = it_zglt049_alv-node_key.
      IF it_zglt049_alv-node_key_c IS NOT INITIAL.
        node-relatkey   = it_zglt049_alv-node_key_c.
        node-relatship  = cl_gui_list_tree=>relat_last_child.
        node-dragdropid = handle_tree.
      ENDIF.
      APPEND node TO node_table.

      CLEAR item.
      item-node_key   = node-node_key.
      item-item_name  = '1'."p_node-node_key. " Item with name '1'
      item-class      = cl_gui_list_tree=>item_class_text. " Text Item
      item-alignment  = cl_gui_list_tree=>align_auto.
      item-font       = cl_gui_list_tree=>item_font_prop.
      CONCATENATE it_zglt049_alv-cod_clas_not '-' it_zglt049_alv-descr_nota INTO item-text.
      "CONCATENATE IT_ZGLT049_ALV-COD_CLAS_BAL '-' IT_ZGLT049_ALV-DESCR INTO ITEM-TEXT.
      APPEND item TO item_table.

      LOOP AT it_t001_alv WHERE node_key_c = it_zglt049_alv-node_key.

        CLEAR: node-relatkey,
               node-relatship,
               node-expander,
               node-n_image,
               node-exp_image,
               node-dragdropid.

        node-n_image   = icon_private_files."ICON_POSITIVE.
        node-exp_image = icon_private_files."ICON_POSITIVE.
        node-hidden    = ' '.    " The node is visible,
        node-disabled  = ' '.    " selectable,
        node-isfolder  = 'X'.    " a folder.
        node-node_key  = it_t001_alv-node_key.
        node-relatkey  = it_t001_alv-node_key_c.
        node-relatship = cl_gui_list_tree=>relat_last_child.
        APPEND node TO node_table.

        CLEAR item.
        item-node_key   = node-node_key.
        item-item_name  = '1'."p_node-node_key. " Item with name '1'
        item-class      = cl_gui_list_tree=>item_class_text. " Text Item
        item-alignment  = cl_gui_list_tree=>align_auto.
        item-font       = cl_gui_list_tree=>item_font_prop.
        item-text       = it_t001_alv-bukrs.
        APPEND item TO item_table.

        item-item_name = '2'.
        item-text      = it_t001_alv-butxt.
        APPEND item TO item_table.

        LOOP AT it_zglt041_alv WHERE node_key_c = it_t001_alv-node_key.

          CLEAR: node-relatkey,    "Special case: A root node has no parent
                 node-relatship,   "node.
                 node-expander,
                 node-n_image,
                 node-exp_image,
                 node-dragdropid.    " see below

          node-n_image   = icon_outbox.
          node-exp_image = icon_outbox.
          node-hidden    = ' '.    " The node is visible,
          node-disabled  = ' '.    " selectable,
          node-isfolder  = 'X'.    " a folder.
          node-node_key  = it_zglt041_alv-node_key.
          node-relatkey  = it_zglt041_alv-node_key_c.
          node-relatship = cl_gui_list_tree=>relat_last_child.
          APPEND node TO node_table.

          CLEAR item.
          item-node_key   = node-node_key.
          item-item_name  = '1'."p_node-node_key. " Item with name '1'
          item-class      = cl_gui_list_tree=>item_class_text. " Text Item
          item-alignment  = cl_gui_list_tree=>align_auto.
          item-font       = cl_gui_list_tree=>item_font_prop.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = it_zglt041_alv-saknr
            IMPORTING
              output = it_zglt041_alv-saknr.

          item-text       = it_zglt041_alv-saknr.
          APPEND item TO item_table.

          item-item_name  = '2'.
          item-text       = it_zglt041_alv-texto.
          APPEND item TO item_table.

        ENDLOOP.

      ENDLOOP.

      "Objetos de Custo """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "************************************************************************************
      LOOP AT it_049clm_alv WHERE node_key_pai EQ it_zglt049_alv-node_key.

        "Nota de Classificação Possui Tipo de centro de custo
        "**********************************************************************************
        IF it_049clm_alv-node_key_c IS NOT INITIAL.
          PERFORM add_node_objeto_custo TABLES it_049c_alv
                                         USING node_table
                                               item_table
                                               it_zglt049_alv-node_key
                                               it_049clm_alv.
        ENDIF.

        "Nota de Classificação Possui Centro de Lucro
        "**********************************************************************************
        IF it_049clm_alv-node_key_l IS NOT INITIAL.
          PERFORM add_node_objeto_lucro TABLES it_049l_alv
                                         USING node_table
                                               item_table
                                               it_zglt049_alv-node_key
                                               it_049clm_alv.
        ENDIF.

        "Nota de Classificação Possui Grupo de Mercadoria
        "**********************************************************************************
        IF it_049clm_alv-node_key_m IS NOT INITIAL.
          PERFORM add_node_objeto_mercadoria TABLES it_049m_alv
                                         USING node_table
                                               item_table
                                               it_zglt049_alv-node_key
                                               it_049clm_alv.
        ENDIF.

      ENDLOOP.
      "************************************************************************************

    ENDLOOP.

    "Inclui item de somatório """"""""""""""""""""""""""""""""""""""""""""
    "*********************************************************************
    LOOP AT it_zglt048_alv WHERE node_key_c = it_zglt047_alv-node_key.

      CLEAR: node-relatkey,    "Special case: A root node has no parent
             node-relatship,   "node.
             node-expander,
             node-n_image,
             node-exp_image,
             node-dragdropid.    " see below

      node-n_image   = icon_rating_positive."ICON_POSITIVE.
      node-exp_image = icon_rating_positive."ICON_POSITIVE.
      node-hidden    = ' '.    " The node is visible,
      node-disabled  = ' '.    " selectable,
      node-isfolder  = 'X'.    " a folder.
      node-node_key  = it_zglt048_alv-node_key.
      IF it_zglt048_alv-node_key_c IS NOT INITIAL.
        node-relatkey  = it_zglt048_alv-node_key_c.
        node-relatship = cl_gui_list_tree=>relat_last_child.
      ENDIF.
      APPEND node TO node_table.

      CLEAR item.
      item-node_key   = node-node_key.
      item-item_name  = '1'."p_node-node_key. " Item with name '1'
      item-class      = cl_gui_list_tree=>item_class_text. " Text Item
      item-alignment  = cl_gui_list_tree=>align_auto.
      item-font       = cl_gui_list_tree=>item_font_prop.
      item-style      = cl_gui_list_tree=>style_emphasized_c.
      item-text       = it_zglt048_alv-nivelsum.
      "ITEM-USEBGCOLOR = 'X'.
      "item-usebgcolor = 'X'.
      APPEND item TO item_table.

      item-item_name = '2'."p_node-node_key. " Item with name '1'
      item-text      = it_zglt048_alv-desnvl.
      APPEND item TO item_table.

    ENDLOOP.
    "*********************************************************************

  ENDLOOP.

ENDFORM.                    " BUILD_NODE_AND_ITEM_TABLE

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TABELA_NIVEIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZGLT046_ALV_VERSN  text
*----------------------------------------------------------------------*
FORM atualiza_tabela_niveis USING  p_versn TYPE versn_011.

  IF prim_bal_nivel_re EQ abap_false.
    CALL METHOD g_tree->free.
    prim_bal_nivel_re = abap_true.
  ENDIF.

  IF prim_bal_nivel EQ abap_false.
    CALL METHOD g_custom_container->free.
    prim_bal_nivel = abap_true.
  ENDIF.

  PERFORM atualiza_tabelas_consultas USING p_versn space space. " '0000'.

ENDFORM.                    " ATUALIZA_TABELA_NIVEIS

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_NIVEL_ESTRUTURA
*&---------------------------------------------------------------------*
*       Exclui Nível de Estrutura de Balanço Patrimonial
*----------------------------------------------------------------------*
*      -->P_ZGLT047_ALV - Nó de estrutura de Balanço Patrimonial
*----------------------------------------------------------------------*
FORM excluir_nivel_estrutura  USING p_zglt047_alv TYPE ty_zglt047_alv.

  DATA: answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = 'Atenção!'
      textline1 = 'Todo o nível será excluido!'
      textline2 = 'Deseja realmente excluir?'
    IMPORTING
      answer    = answer.

  IF answer = 'J'.
    PERFORM exclui_nivel USING p_zglt047_alv-versn p_zglt047_alv-nivel abap_true.
    MESSAGE s006 WITH p_zglt047_alv-nivel p_zglt047_alv-versn.
  ENDIF.

ENDFORM.                    " EXCLUIR_NIVEL_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  EXCLUI_NIVEL
*&---------------------------------------------------------------------*
*       Exclui Nível Solicitado
*----------------------------------------------------------------------*
*      -->P_VERSN  - Versão da Estrutura
*      -->P_NIVEL  - Nível da Estrutura
*----------------------------------------------------------------------*
FORM exclui_nivel  USING  p_versn LIKE zglt047-versn
                          p_nivel LIKE zglt047-nivel
                          p_base  TYPE char1.

  DATA: it_niveis_filhos  TYPE TABLE OF zglt047 WITH HEADER LINE,
        wa_niveis_excluir TYPE zglt047,
        lc_posicao        TYPE zdesqnivel.

  SELECT * INTO TABLE it_niveis_filhos
    FROM zglt047
   WHERE versn    EQ p_versn
     AND nivelpai EQ p_nivel.

  LOOP AT it_niveis_filhos.
    PERFORM exclui_nivel USING it_niveis_filhos-versn it_niveis_filhos-nivel abap_false.
  ENDLOOP.

  IF p_base EQ abap_true.
    SELECT SINGLE * INTO wa_niveis_excluir
      FROM zglt047
     WHERE versn EQ p_versn
       AND nivel EQ p_nivel.
  ENDIF.

  "Exclui Nível de Soma se foi utilizado o Nível como Soma
  DELETE FROM zglt048 WHERE versn EQ p_versn AND nivel EQ p_nivel.

  "Exclui Físico Notas do Nível
  DELETE FROM zglt049 WHERE versn EQ p_versn AND nivel EQ p_nivel.

  "Exclui Físico o Nível da Estrutura
  DELETE FROM zglt047 WHERE versn EQ p_versn AND nivel EQ p_nivel.

  IF ( p_base EQ abap_true ) AND ( wa_niveis_excluir-nivelpai IS NOT INITIAL ).

    CLEAR: it_niveis_filhos[].

    SELECT * INTO TABLE it_niveis_filhos
      FROM zglt047
     WHERE versn    EQ p_versn
       AND nivelpai EQ wa_niveis_excluir-nivelpai
     ORDER BY sqnivel.

    lc_posicao = 0.
    LOOP AT it_niveis_filhos.
      ADD 1 TO lc_posicao.
      IF it_niveis_filhos-sqnivel NE lc_posicao.
        it_niveis_filhos-sqnivel = lc_posicao.
        MODIFY zglt047 FROM it_niveis_filhos.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " EXCLUI_NIVEL

*&---------------------------------------------------------------------*
*&      Form  LIMPAR_TELA_TREE
*&---------------------------------------------------------------------*
*       Limpar objetos da tela de apresentação da ALV TREE
*----------------------------------------------------------------------*
FORM limpar_tela_tree .
  ck_editar         = abap_false.
  ck_nivel_definido = abap_false.
  prim_busca_alv    = abap_true.
  prim_bal_nivel    = abap_true.
  prim_bal_nivel_re = abap_true.
  CALL METHOD g_tree->free.
  CALL METHOD g_custom_container->free.
ENDFORM.                    " LIMPAR_TELA_TREE

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_NIVEL_SOMATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZGLT048_ALV  text
*----------------------------------------------------------------------*
FORM excluir_nivel_somatorio  USING  p_zglt048_alv TYPE ty_zglt048_alv.

  DATA: answer TYPE c LENGTH 1.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = 'Atenção!'
      textline1 = 'Todo o nível será excluido!'
      textline2 = 'Deseja realmente excluir?'
    IMPORTING
      answer    = answer.

  IF answer = 'J'.
    DELETE FROM zglt048
     WHERE versn    EQ p_zglt048_alv-versn
       AND nivel    EQ p_zglt048_alv-nivel
       AND nivelsum EQ p_zglt048_alv-nivelsum.
    MESSAGE s007 WITH p_zglt048_alv-nivelsum p_zglt048_alv-nivel p_zglt048_alv-versn.
  ENDIF.

ENDFORM.                    " EXCLUIR_NIVEL_SOMATORIO

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_NIVEL_NOTA
*&---------------------------------------------------------------------*
*       Exclui Classificação de Nota do Nível
*----------------------------------------------------------------------*
*      -->P_ZGLT049_ALV  Classificação do balanço do Nó de Estrutura de Balanço Patr.
*----------------------------------------------------------------------*
FORM excluir_nivel_nota  USING p_zglt049_alv TYPE ty_zglt049_alv.

  DATA: answer           TYPE c LENGTH 1,
        it_niveis_filhos TYPE TABLE OF zglt049 WITH HEADER LINE,
        lc_posicao       TYPE zdesqnivel.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel     = 'Atenção!'
      textline1 = 'O Nota de Classificação será excluido da Estrutura!'
      textline2 = 'Deseja realmente excluir?'
    IMPORTING
      answer    = answer.

  IF answer = 'J'.
    DELETE FROM zglt049
     WHERE versn        EQ p_zglt049_alv-versn
       AND nivel        EQ p_zglt049_alv-nivel
       AND cod_clas_bal EQ p_zglt049_alv-cod_clas_bal
       AND cod_clas_not EQ p_zglt049_alv-cod_clas_not.

    SELECT * INTO TABLE it_niveis_filhos
      FROM zglt049
     WHERE versn EQ p_zglt049_alv-versn
       AND nivel EQ p_zglt049_alv-nivel
     ORDER BY sqnivel.

    lc_posicao = 0.
    LOOP AT it_niveis_filhos.
      ADD 1 TO lc_posicao.
      IF it_niveis_filhos-sqnivel NE lc_posicao.
        it_niveis_filhos-sqnivel = lc_posicao.
        MODIFY zglt049 FROM it_niveis_filhos.
      ENDIF.
    ENDLOOP.

    MESSAGE s008 WITH p_zglt049_alv-cod_clas_not p_zglt049_alv-nivel p_zglt049_alv-versn.
  ENDIF.

ENDFORM.                    " EXCLUIR_NIVEL_NOTA

*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TABELAS_CONSULTAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM atualiza_tabelas_consultas USING p_versn   TYPE versn_011
                                      p_bukrs01 TYPE bukrs
                                      p_bukrs02 TYPE bukrs.
  "P_GJAHR01 TYPE GJAHR. "/Modificação CS2017000372

  DATA: lc_posicao         TYPE i,
        vg_sytabix         TYPE sytabix,
        v_bukrs            TYPE bukrs,
        it_zglt047_alv_aux TYPE TABLE OF ty_zglt047_alv  WITH HEADER LINE,
        it_tipo_c          TYPE TABLE OF tkt05 WITH HEADER LINE,
        it_tipo_l          TYPE TABLE OF cepct WITH HEADER LINE,
        it_tipo_m          TYPE TABLE OF t023t WITH HEADER LINE,
        it_zglt041_aux     TYPE TABLE OF zglt041 WITH HEADER LINE,
        vg_linhas          TYPE i.

  CLEAR: it_zglt047[],
         it_zglt047_alv[],
         it_zglt048[],
         it_zglt048_alv[],
         it_zglt049[],
         it_zglt049_alv[],
         it_t001_alv[],
         it_zglt041_alv[],
         it_zglt039[],
         it_zglt041[],
         it_049_cn[],
         it_049_ln[],
         it_049_mn[],
         it_dre_c[],
         it_dre_l[],
         it_dre_m[],
         it_049clm_alv[],
         it_049c_alv[],
         it_049l_alv[],
         it_049m_alv[].

  PERFORM mostra_texto USING 'Pesquisa: Nós de estrutura de Balanço Patrimonial'.

  SELECT * INTO TABLE it_zglt047
    FROM zglt047
   WHERE versn EQ p_versn
   ORDER BY nivelpai sqnivel.

  PERFORM mostra_texto USING 'Pesquisa: Nós de Linha Somatório'.

  SELECT * INTO TABLE it_zglt048
    FROM zglt048
   WHERE versn EQ p_versn.

  SORT it_zglt048 BY versn nivel.

  PERFORM mostra_texto USING 'Pesquisa: Classificação do balanço'.

  SELECT * INTO TABLE it_zglt049
    FROM zglt049
   WHERE versn EQ p_versn
   ORDER BY versn nivel sqnivel.

  SORT it_zglt049 BY versn nivel sqnivel.

  PERFORM mostra_texto USING 'Pesquisa: Cadastro de Classificação do balanço'.

  SELECT * INTO TABLE it_zglt039
    FROM zglt039
     FOR ALL ENTRIES IN it_zglt049
   WHERE codigo   EQ it_zglt049-cod_clas_bal
     AND cod_nota EQ it_zglt049-cod_clas_not.

  SORT it_zglt039 BY codigo cod_nota.

  PERFORM mostra_texto USING 'Pesquisa: Reconciliação Contábil – Parâmetros Gerais'.

  SELECT * INTO TABLE it_zglt041
    FROM zglt041
   ORDER BY bukrs saknr.

  IF ( p_bukrs01 NE space ) AND ( p_bukrs02 NE space ) AND ( p_bukrs01 IS NOT INITIAL ) AND ( p_bukrs02 IS NOT INITIAL ).
    DELETE it_zglt041 WHERE bukrs NE p_bukrs01 AND bukrs NE p_bukrs02.
  ELSEIF ( p_bukrs01 NE space ) AND ( p_bukrs01 IS NOT INITIAL ).
    DELETE it_zglt041 WHERE bukrs NE p_bukrs01.
  ENDIF.

*  IF P_GJAHR01 IS NOT INITIAL.  "/Modificação CS2017000372
*    DELETE IT_ZGLT041 WHERE GJAHR NE P_GJAHR01.
*  ENDIF.

  SORT it_zglt041 BY bukrs saknr.

  PERFORM mostra_texto USING 'Pesquisa: Empresas'.

  SELECT * INTO TABLE it_t001
    FROM t001
     FOR ALL ENTRIES IN it_zglt041
   WHERE bukrs EQ it_zglt041-bukrs.

  SORT it_t001 BY bukrs.

  PERFORM mostra_texto USING 'Pesquisa: Contas do Razão (plano de contas: denominação)'.

  SELECT * INTO TABLE it_skat
    FROM skat
     FOR ALL ENTRIES IN it_t001
   WHERE spras EQ sy-langu
     AND ktopl EQ it_t001-ktopl.

  SORT it_skat BY ktopl saknr.


  "Estrutura for de DRE. """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "Buscar Objetos de Custo """""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "*********************************************************************************
  IF ( it_zglt046_alv-versnt EQ '2' OR it_zglt046_alv-versnt EQ '3' ) AND
     ( ck_analisa_objetos EQ abap_true ).

    PERFORM mostra_texto USING 'Pesquisa: Nó de Estrutura de Balanço Patrimonial - Custo'.

    SELECT *
      INTO TABLE it_049_cn
      FROM zglt049cn
       FOR ALL ENTRIES IN it_zglt049
     WHERE versn        EQ p_versn
       AND cod_clas_bal EQ it_zglt049-cod_clas_bal
       AND cod_clas_not EQ it_zglt049-cod_clas_not.

    PERFORM mostra_texto USING 'Pesquisa: Nó de Estrutura de Balanço Patrimonial - Lucro'.

    SELECT *
      INTO TABLE it_049_ln
      FROM zglt049ln
       FOR ALL ENTRIES IN it_zglt049
     WHERE versn        EQ p_versn
       AND cod_clas_bal EQ it_zglt049-cod_clas_bal
       AND cod_clas_not EQ it_zglt049-cod_clas_not.

    PERFORM mostra_texto USING 'Pesquisa: Nó de Estrutura de Balanço Patrimonial - G. Mercadoria'.

    SELECT *
      INTO TABLE it_049_mn
      FROM zglt049mn
       FOR ALL ENTRIES IN it_zglt049
     WHERE versn        EQ p_versn
       AND cod_clas_bal EQ it_zglt049-cod_clas_bal
       AND cod_clas_not EQ it_zglt049-cod_clas_not.

    PERFORM mostra_texto USING 'Pesquisa: DRE - Tipo Custo'.
    SELECT * INTO TABLE it_dre_c FROM zgl015_dre_est04.

*---> 04/07/2023 - Migração S4 - WS
    SORT it_dre_c BY ktopl saknr kosar.
*<--- 04/07/2023 - Migração S4 - WS

    PERFORM mostra_texto USING 'Pesquisa: DRE - Lucro'.
    SELECT * INTO TABLE it_dre_l FROM zgl015_dre_est05.

*---> 04/07/2023 - Migração S4 - WS
    SORT it_dre_l BY ktopl saknr kokrs prctr.
*<--- 04/07/2023 - Migração S4 - WS

    PERFORM mostra_texto USING 'Pesquisa: DRE - G. Mercadoria'.
    SELECT * INTO TABLE it_dre_m FROM zgl015_dre_est06.

*---> 04/07/2023 - Migração S4 - WS
    SORT it_dre_m BY ktopl saknr matkl.
*<--- 04/07/2023 - Migração S4 - WS

    DELETE ADJACENT DUPLICATES FROM it_dre_c COMPARING ktopl saknr kosar. "#EC CI_SORTED
    DELETE ADJACENT DUPLICATES FROM it_dre_l COMPARING ktopl saknr kokrs prctr. "#EC CI_SORTED
    DELETE ADJACENT DUPLICATES FROM it_dre_m COMPARING ktopl saknr matkl. "#EC CI_SORTED

    PERFORM mostra_texto USING 'Pesquisa: DRE - Categorias de centros de custos - textos'.
    SELECT * INTO TABLE it_tipo_c
      FROM tkt05
       FOR ALL ENTRIES IN it_dre_c
     WHERE spras EQ sy-langu
       AND kosar EQ it_dre_c-kosar.

    PERFORM mostra_texto USING 'Pesquisa: DRE - Textos de dados mestre de centro de lucro'.
    SELECT * INTO TABLE it_tipo_l
      FROM cepct
       FOR ALL ENTRIES IN it_dre_l
     WHERE spras EQ sy-langu
       AND kokrs EQ it_dre_l-kokrs
       AND prctr EQ it_dre_l-prctr.

    PERFORM mostra_texto USING 'Pesquisa: Denominações para grupos de mercadoria'.
    SELECT * INTO TABLE it_tipo_m
      FROM t023t
       FOR ALL ENTRIES IN it_dre_m
     WHERE spras EQ sy-langu
       AND matkl EQ it_dre_m-matkl.

    SORT it_tipo_c BY kosar.
    SORT it_tipo_l BY kokrs prctr.
    SORT it_tipo_m BY matkl.
  ENDIF.
  "*********************************************************************************

  lc_posicao = 0.

  " Ajuste de Níveis """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "********************************************************************************
  PERFORM mostra_texto USING 'Ajuste de Níveis'.

  LOOP AT it_zglt047.
    CLEAR: it_zglt047_alv.
    ADD 1 TO lc_posicao.
    MOVE lc_posicao TO it_zglt047_alv-node_key.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = it_zglt047_alv-node_key
      IMPORTING
        output = it_zglt047_alv-node_key.

    MOVE-CORRESPONDING it_zglt047 TO it_zglt047_alv.
    APPEND it_zglt047_alv.
  ENDLOOP.
  "********************************************************************************


  " Ajuste de Itens de Somatório """"""""""""""""""""""""""""""""""""""""""""""""""
  "********************************************************************************
  PERFORM mostra_texto USING 'Ajuste de Itens de Somatório'.

  LOOP AT it_zglt048.
    CLEAR: it_zglt048_alv.
    ADD 1 TO lc_posicao.
    MOVE lc_posicao TO it_zglt048_alv-node_key.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = it_zglt048_alv-node_key
      IMPORTING
        output = it_zglt048_alv-node_key.

    MOVE-CORRESPONDING it_zglt048 TO it_zglt048_alv.

    "Busca Pai do Nível Somatório
    READ TABLE it_zglt047_alv
    WITH KEY versn = it_zglt048_alv-versn
             nivel = it_zglt048_alv-nivel.

    IF sy-subrc IS INITIAL.
      it_zglt048_alv-node_key_c = it_zglt047_alv-node_key.
    ENDIF.

    "Busca Descrição do Nível Somatório
    READ TABLE it_zglt047_alv
    WITH KEY versn = it_zglt048_alv-versn
             nivel = it_zglt048_alv-nivelsum.

    IF sy-subrc IS INITIAL.
      it_zglt048_alv-desnvl = it_zglt047_alv-desnvl.
    ENDIF.

    APPEND it_zglt048_alv.
  ENDLOOP.
  "********************************************************************************


  " Ajuste de Itens de Nota de Classificação """"""""""""""""""""""""""""""""""""""
  "********************************************************************************
  PERFORM mostra_texto USING 'Ajuste de Itens de Nota de Classificação'.

  DESCRIBE TABLE it_zglt049 LINES vg_linhas.

  LOOP AT it_zglt049.
    CLEAR: it_zglt049_alv.

    PERFORM mostra_texto_p
      USING 'Ajuste de Itens de Nota de Classificação' vg_linhas sy-tabix.

    ADD 1 TO lc_posicao.
    MOVE lc_posicao TO it_zglt049_alv-node_key.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = it_zglt049_alv-node_key
      IMPORTING
        output = it_zglt049_alv-node_key.

    MOVE-CORRESPONDING it_zglt049 TO it_zglt049_alv.

    "Busca Pai do Nível Somatório
    READ TABLE it_zglt047_alv
    WITH KEY versn = it_zglt049_alv-versn
             nivel = it_zglt049_alv-nivel.

    IF sy-subrc IS INITIAL.
      it_zglt049_alv-node_key_c = it_zglt047_alv-node_key.
    ENDIF.

    READ TABLE it_zglt039
    WITH KEY codigo   = it_zglt049_alv-cod_clas_bal
             cod_nota = it_zglt049_alv-cod_clas_not.

    IF sy-subrc IS INITIAL.
      it_zglt049_alv-descr      = it_zglt039-descr.
      it_zglt049_alv-descr_nota = it_zglt039-descr_nota.
    ENDIF.

    APPEND it_zglt049_alv.

    " Ajuste de Itens de Nota de Classificação """"""""""""""""""""""""""""""""""""""
    "********************************************************************************
    CLEAR: v_bukrs.

    MOVE it_zglt041[] TO it_zglt041_aux[].

    LOOP AT it_zglt041 WHERE cod_clas_bal  EQ it_zglt049_alv-cod_clas_bal
                         AND cod_clas_not2 EQ it_zglt049_alv-cod_clas_not.

      IF v_bukrs <> it_zglt041-bukrs.
        v_bukrs = it_zglt041-bukrs.

        READ TABLE it_t001_alv WITH KEY nivel        = it_zglt049_alv-nivel
                                        bukrs        = it_zglt041-bukrs
                                        cod_clas_bal = it_zglt049_alv-cod_clas_bal
                                        cod_clas_not = it_zglt049_alv-cod_clas_not.
        IF sy-subrc IS NOT INITIAL.
          CLEAR: it_t001_alv.
          ADD 1 TO lc_posicao.
          MOVE lc_posicao TO it_t001_alv-node_key.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = it_t001_alv-node_key
            IMPORTING
              output = it_t001_alv-node_key.

          it_t001_alv-bukrs = it_zglt041-bukrs.
          READ TABLE it_t001 WITH KEY bukrs = it_zglt041-bukrs.
          it_t001_alv-butxt        = it_t001-butxt.
          it_t001_alv-ktopl        = it_t001-ktopl.
          it_t001_alv-nivel        = it_zglt049_alv-nivel.
          it_t001_alv-cod_clas_bal = it_zglt049_alv-cod_clas_bal.
          it_t001_alv-cod_clas_not = it_zglt049_alv-cod_clas_not.
          it_t001_alv-node_key_c   = it_zglt049_alv-node_key.
          APPEND it_t001_alv.
        ENDIF.
      ENDIF.

      ADD 1 TO lc_posicao.
      MOVE lc_posicao TO it_zglt041_alv-node_key.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = it_zglt041_alv-node_key
        IMPORTING
          output = it_zglt041_alv-node_key.

      it_zglt041_alv-node_key_c = it_t001_alv-node_key.

      MOVE-CORRESPONDING it_zglt041 TO it_zglt041_alv.

      READ TABLE it_skat WITH KEY ktopl = it_t001_alv-ktopl
                                  saknr = it_zglt041-saknr BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        it_zglt041_alv-texto = it_skat-txt50.
      ENDIF.

      APPEND it_zglt041_alv.

    ENDLOOP.
    "********************************************************************************

    "Ajuste de Objetos de Custo""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "********************************************************************************
    CLEAR: it_049clm_alv.
    it_049clm_alv-node_key_pai = it_zglt049_alv-node_key.

    ADD 1 TO lc_posicao.
    MOVE lc_posicao TO it_049clm_alv-node_key.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = it_049clm_alv-node_key
      IMPORTING
        output = it_049clm_alv-node_key.

    LOOP AT it_zglt041 WHERE cod_clas_bal  EQ it_zglt049_alv-cod_clas_bal
                         AND cod_clas_not2 EQ it_zglt049_alv-cod_clas_not.

      "Centro de Custo """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "*****************************************************************************
      LOOP AT it_dre_c WHERE saknr EQ it_zglt041-saknr.

        IF it_049clm_alv-node_key_c IS INITIAL.
          ADD 1 TO lc_posicao.
          MOVE lc_posicao TO it_049clm_alv-node_key_c.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = it_049clm_alv-node_key_c
            IMPORTING
              output = it_049clm_alv-node_key_c.
        ENDIF.

        READ TABLE it_049c_alv
          WITH KEY nivel        = it_zglt049_alv-nivel
                   cod_clas_bal = it_zglt049_alv-cod_clas_bal
                   cod_clas_not = it_zglt049_alv-cod_clas_not
                   kosar        = it_dre_c-kosar.

        IF sy-subrc IS NOT INITIAL.
          CLEAR: it_049c_alv.
          MOVE-CORRESPONDING it_zglt049_alv TO it_049c_alv.

          ADD 1 TO lc_posicao.
          MOVE lc_posicao TO it_049c_alv-node_key.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = it_049c_alv-node_key
            IMPORTING
              output = it_049c_alv-node_key.

          it_049c_alv-node_key_c = it_049clm_alv-node_key_c.
          it_049c_alv-kosar      = it_dre_c-kosar.
          it_049c_alv-contido    = abap_false.
          it_049c_alv-duplicado  = abap_false.

          READ TABLE it_049_cn
          WITH KEY nivel        = it_049c_alv-nivel
                   cod_clas_bal = it_049c_alv-cod_clas_bal
                   cod_clas_not = it_049c_alv-cod_clas_not
                   kosar        = it_049c_alv-kosar.

          IF sy-subrc IS INITIAL.
            it_049c_alv-contido    = abap_true.
          ENDIF.

          IF it_049c_alv-contido EQ abap_true.
            LOOP AT it_zglt041_aux WHERE saknr EQ it_zglt041-saknr
                                     AND ( cod_clas_bal  NE it_049c_alv-cod_clas_bal OR
                                           cod_clas_not2 NE it_049c_alv-cod_clas_not ).
              READ TABLE it_049_cn
              WITH KEY nivel        = it_049c_alv-nivel
                       cod_clas_bal = it_zglt041_aux-cod_clas_bal
                       cod_clas_not = it_zglt041_aux-cod_clas_not2
                       kosar        = it_049c_alv-kosar.

              IF sy-subrc IS INITIAL.
                it_049c_alv-duplicado = abap_true.
              ENDIF.
            ENDLOOP.
          ENDIF.

          READ TABLE it_tipo_c WITH KEY kosar = it_049c_alv-kosar BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            it_049c_alv-ktext      = it_tipo_c-ktext.
          ENDIF.

          APPEND it_049c_alv.
        ENDIF.
      ENDLOOP.
      "*****************************************************************************

      "Centro de Lucro """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "*****************************************************************************
      LOOP AT it_dre_l WHERE saknr EQ it_zglt041-saknr.

        IF it_049clm_alv-node_key_l IS INITIAL.
          ADD 1 TO lc_posicao.
          MOVE lc_posicao TO it_049clm_alv-node_key_l.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = it_049clm_alv-node_key_l
            IMPORTING
              output = it_049clm_alv-node_key_l.

        ENDIF.

        READ TABLE it_049l_alv
          WITH KEY nivel        = it_zglt049_alv-nivel
                   cod_clas_bal = it_zglt049_alv-cod_clas_bal
                   cod_clas_not = it_zglt049_alv-cod_clas_not
                   kokrs        = it_dre_l-kokrs
                   prctr        = it_dre_l-prctr.

        IF sy-subrc IS NOT INITIAL.
          CLEAR: it_049l_alv.
          MOVE-CORRESPONDING it_zglt049_alv TO it_049l_alv.

          ADD 1 TO lc_posicao.
          MOVE lc_posicao TO it_049l_alv-node_key.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = it_049l_alv-node_key
            IMPORTING
              output = it_049l_alv-node_key.

          it_049l_alv-node_key_c = it_049clm_alv-node_key_l.
          it_049l_alv-kokrs      = it_dre_l-kokrs.
          it_049l_alv-prctr      = it_dre_l-prctr.
          it_049l_alv-contido    = abap_false.
          it_049c_alv-duplicado  = abap_false.

          READ TABLE it_049_ln
          WITH KEY nivel        = it_049l_alv-nivel
                   cod_clas_bal = it_049l_alv-cod_clas_bal
                   cod_clas_not = it_049l_alv-cod_clas_not
                   kokrs        = it_049l_alv-kokrs
                   prctr        = it_049l_alv-prctr.

          IF sy-subrc IS INITIAL.
            it_049l_alv-contido    = abap_true.
          ENDIF.

          IF it_049l_alv-contido EQ abap_true.
            LOOP AT it_zglt041_aux WHERE saknr EQ it_zglt041-saknr
                                     AND ( cod_clas_bal  NE it_049l_alv-cod_clas_bal OR
                                           cod_clas_not2 NE it_049l_alv-cod_clas_not ).
              READ TABLE it_049_ln
              WITH KEY nivel        = it_049l_alv-nivel
                       cod_clas_bal = it_zglt041_aux-cod_clas_bal
                       cod_clas_not = it_zglt041_aux-cod_clas_not2
                       kokrs        = it_049l_alv-kokrs
                       prctr        = it_049l_alv-prctr.

              IF sy-subrc IS INITIAL.
                it_049l_alv-duplicado = abap_true.
              ENDIF.
            ENDLOOP.
          ENDIF.

          READ TABLE it_tipo_l WITH KEY kokrs = it_049l_alv-kokrs
                                        prctr = it_049l_alv-prctr
                                        BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            it_049l_alv-ltext      = it_tipo_l-ltext.
          ENDIF.

          APPEND it_049l_alv.
        ENDIF.
      ENDLOOP.
      "*****************************************************************************

      "Grupo de Material """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "*****************************************************************************
      LOOP AT it_dre_m WHERE saknr EQ it_zglt041-saknr.

        IF it_049clm_alv-node_key_m IS INITIAL.
          ADD 1 TO lc_posicao.
          MOVE lc_posicao TO it_049clm_alv-node_key_m.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = it_049clm_alv-node_key_m
            IMPORTING
              output = it_049clm_alv-node_key_m.

        ENDIF.

        READ TABLE it_049m_alv
          WITH KEY nivel        = it_zglt049_alv-nivel
                   cod_clas_bal = it_zglt049_alv-cod_clas_bal
                   cod_clas_not = it_zglt049_alv-cod_clas_not
                   matkl        = it_dre_m-matkl.

        IF sy-subrc IS NOT INITIAL.
          CLEAR: it_049m_alv.
          MOVE-CORRESPONDING it_zglt049_alv TO it_049m_alv.

          ADD 1 TO lc_posicao.
          MOVE lc_posicao TO it_049m_alv-node_key.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = it_049m_alv-node_key
            IMPORTING
              output = it_049m_alv-node_key.

          it_049m_alv-node_key_c = it_049clm_alv-node_key_m.
          it_049m_alv-matkl      = it_dre_m-matkl.
          it_049m_alv-contido    = abap_false.
          it_049c_alv-duplicado  = abap_false.

          READ TABLE it_049_mn
          WITH KEY nivel        = it_049m_alv-nivel
                   cod_clas_bal = it_049m_alv-cod_clas_bal
                   cod_clas_not = it_049m_alv-cod_clas_not
                   matkl        = it_049m_alv-matkl.

          IF sy-subrc IS INITIAL.
            it_049m_alv-contido    = abap_true.
          ENDIF.

          IF it_049m_alv-contido EQ abap_true.
            LOOP AT it_zglt041_aux WHERE saknr EQ it_zglt041-saknr
                                     AND ( cod_clas_bal  NE it_049m_alv-cod_clas_bal OR
                                           cod_clas_not2 NE it_049m_alv-cod_clas_not ).
              READ TABLE it_049_mn
              WITH KEY nivel        = it_049m_alv-nivel
                       cod_clas_bal = it_zglt041_aux-cod_clas_bal
                       cod_clas_not = it_zglt041_aux-cod_clas_not2
                       matkl        = it_049m_alv-matkl.

              IF sy-subrc IS INITIAL.
                it_049m_alv-duplicado = abap_true.
              ENDIF.
            ENDLOOP.
          ENDIF.

          READ TABLE it_tipo_m WITH KEY matkl = it_049m_alv-matkl BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            it_049m_alv-wgbez60    = it_tipo_m-wgbez60.
          ENDIF.

          APPEND it_049m_alv.
        ENDIF.
      ENDLOOP.
      "*****************************************************************************

    ENDLOOP.

    IF it_049clm_alv-node_key_c IS NOT INITIAL OR
       it_049clm_alv-node_key_l IS NOT INITIAL OR
       it_049clm_alv-node_key_m IS NOT INITIAL.
      APPEND it_049clm_alv.
    ELSE.
      ADD -1 TO lc_posicao.
    ENDIF.
    "********************************************************************************

  ENDLOOP.
  "********************************************************************************

  " Ajuste de Níveis Pais """""""""""""""""""""""""""""""""""""""""""""""""""""""""
  "********************************************************************************
  PERFORM mostra_texto USING 'Ajuste de Níveis Pais'.

  MOVE it_zglt047_alv[] TO it_zglt047_alv_aux[].
  LOOP AT it_zglt047_alv.
    vg_sytabix = sy-tabix.
    IF it_zglt047_alv-nivelpai IS NOT INITIAL.
      READ TABLE it_zglt047_alv_aux WITH KEY nivel = it_zglt047_alv-nivelpai.
      IF sy-subrc IS INITIAL.
        it_zglt047_alv-node_key_c = it_zglt047_alv_aux-node_key.
        MODIFY it_zglt047_alv INDEX vg_sytabix TRANSPORTING node_key_c.
      ENDIF.
    ENDIF.
  ENDLOOP.
  "********************************************************************************

  SORT it_zglt047_alv BY node_key node_key_c.

ENDFORM.                    " ATUALIZA_TABELAS_CONSULTAS

*&---------------------------------------------------------------------*
*&      Form  ADD_NODE_OBJETO_CUSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_node_objeto_custo  TABLES p_049c_alv STRUCTURE it_049c_alv
                            USING  node_table TYPE treev_ntab
                                   item_table TYPE item_table_type
                                   p_key_c    TYPE tv_nodekey
                                   p_clm_alv  TYPE ty_049clm_alv.

  DATA: node       TYPE treev_node,
        item       TYPE mtreeitm,
        wa_049_obj TYPE ty_049c_alv.

  node-n_image   = icon_biw_application."ICON_POSITIVE.
  node-exp_image = icon_biw_application."ICON_POSITIVE.
  node-hidden    = ' '.    " The node is visible,
  node-disabled  = ' '.    " selectable,
  node-isfolder  = 'X'.    " a folder.
  node-node_key  = p_clm_alv-node_key_c.
  node-relatkey  = p_clm_alv-node_key_pai.
  node-relatship = cl_gui_list_tree=>relat_last_child.
  APPEND node TO node_table.

  CLEAR item.
  item-node_key   = node-node_key.
  item-item_name  = '1'."p_node-node_key. " Item with name '1'
  item-class      = cl_gui_list_tree=>item_class_text. " Text Item
  item-alignment  = cl_gui_list_tree=>align_auto.
  item-font       = cl_gui_list_tree=>item_font_prop.
  item-text       = 'Tipo de Centro de Custo'.
  APPEND item TO item_table.

  LOOP AT p_049c_alv INTO wa_049_obj WHERE node_key_c EQ p_clm_alv-node_key_c.

    IF wa_049_obj-duplicado EQ abap_true.
      node-n_image   = icon_led_red.
      node-exp_image = icon_led_red.
    ELSEIF wa_049_obj-contido EQ abap_true.
      node-n_image   = icon_led_green.
      node-exp_image = icon_led_green.
    ELSE.
      node-n_image   = icon_led_inactive.
      node-exp_image = icon_led_inactive.
    ENDIF.
    node-hidden    = ' '.    " The node is visible,
    node-disabled  = ' '.    " selectable,
    node-isfolder  = ' '.    " a folder.
    node-node_key  = wa_049_obj-node_key.
    node-relatkey  = wa_049_obj-node_key_c.
    node-relatship = cl_gui_list_tree=>relat_last_child.
    APPEND node TO node_table.

    CLEAR item.
    item-node_key   = node-node_key.
    item-item_name  = '1'."p_node-node_key. " Item with name '1'
    item-class      = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment  = cl_gui_list_tree=>align_auto.
    item-font       = cl_gui_list_tree=>item_font_prop.
    CONCATENATE wa_049_obj-kosar '-' wa_049_obj-ktext INTO item-text SEPARATED BY space.
    APPEND item TO item_table.

  ENDLOOP.

ENDFORM.                    " ADD_NODE_OBJETO_CUSTO

*&---------------------------------------------------------------------*
*&      Form  ADD_NODE_OBJETO_LUCRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_node_objeto_lucro  TABLES p_049l_alv STRUCTURE it_049l_alv
                            USING  node_table TYPE treev_ntab
                                   item_table TYPE item_table_type
                                   p_key_c    TYPE tv_nodekey
                                   p_clm_alv  TYPE ty_049clm_alv.

  DATA: node       TYPE treev_node,
        item       TYPE mtreeitm,
        wa_049_obj TYPE ty_049l_alv.

  node-n_image   = icon_biw_info_area."ICON_POSITIVE.
  node-exp_image = icon_biw_info_area."ICON_POSITIVE.
  node-hidden    = ' '.    " The node is visible,
  node-disabled  = ' '.    " selectable,
  node-isfolder  = 'X'.    " a folder.
  node-node_key  = p_clm_alv-node_key_l.
  node-relatkey  = p_clm_alv-node_key_pai.
  node-relatship = cl_gui_list_tree=>relat_last_child.
  APPEND node TO node_table.

  CLEAR item.
  item-node_key   = node-node_key.
  item-item_name  = '1'."p_node-node_key. " Item with name '1'
  item-class      = cl_gui_list_tree=>item_class_text. " Text Item
  item-alignment  = cl_gui_list_tree=>align_auto.
  item-font       = cl_gui_list_tree=>item_font_prop.
  item-text       = 'Centro de Lucro'.
  APPEND item TO item_table.

  LOOP AT p_049l_alv INTO wa_049_obj WHERE node_key_c EQ p_clm_alv-node_key_l.

    IF wa_049_obj-duplicado EQ abap_true.
      node-n_image   = icon_led_red.
      node-exp_image = icon_led_red.
    ELSEIF wa_049_obj-contido EQ abap_true.
      node-n_image   = icon_led_green.
      node-exp_image = icon_led_green.
    ELSE.
      node-n_image   = icon_led_inactive.
      node-exp_image = icon_led_inactive.
    ENDIF.
    node-hidden    = ' '.    " The node is visible,
    node-disabled  = ' '.    " selectable,
    node-isfolder  = ' '.    " a folder.
    node-node_key  = wa_049_obj-node_key.
    node-relatkey  = wa_049_obj-node_key_c.
    node-relatship = cl_gui_list_tree=>relat_last_child.
    APPEND node TO node_table.

    CLEAR item.
    item-node_key   = node-node_key.
    item-item_name  = '1'."p_node-node_key. " Item with name '1'
    item-class      = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment  = cl_gui_list_tree=>align_auto.
    item-font       = cl_gui_list_tree=>item_font_prop.
    CONCATENATE wa_049_obj-prctr '-' wa_049_obj-ltext INTO item-text SEPARATED BY space.
    APPEND item TO item_table.

  ENDLOOP.

ENDFORM.                    " ADD_NODE_OBJETO_LUCRO

*&---------------------------------------------------------------------*
*&      Form  ADD_NODE_OBJETO_MERCADORIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM add_node_objeto_mercadoria TABLES p_049m_alv STRUCTURE it_049m_alv
                            USING  node_table TYPE treev_ntab
                                   item_table TYPE item_table_type
                                   p_key_c    TYPE tv_nodekey
                                   p_clm_alv  TYPE ty_049clm_alv.

  DATA: node       TYPE treev_node,
        item       TYPE mtreeitm,
        wa_049_obj TYPE ty_049m_alv.

  node-n_image   = icon_biw_info_cube."ICON_POSITIVE.
  node-exp_image = icon_biw_info_cube."ICON_POSITIVE.
  node-hidden    = ' '.    " The node is visible,
  node-disabled  = ' '.    " selectable,
  node-isfolder  = 'X'.    " a folder.
  node-node_key  = p_clm_alv-node_key_m.
  node-relatkey  = p_clm_alv-node_key_pai.
  node-relatship = cl_gui_list_tree=>relat_last_child.
  APPEND node TO node_table.

  CLEAR item.
  item-node_key   = node-node_key.
  item-item_name  = '1'."p_node-node_key. " Item with name '1'
  item-class      = cl_gui_list_tree=>item_class_text. " Text Item
  item-alignment  = cl_gui_list_tree=>align_auto.
  item-font       = cl_gui_list_tree=>item_font_prop.
  item-text       = 'Grupo de Mercadoria'.
  APPEND item TO item_table.

  LOOP AT p_049m_alv INTO wa_049_obj WHERE node_key_c EQ p_clm_alv-node_key_m.

    IF wa_049_obj-duplicado EQ abap_true.
      node-n_image   = icon_led_red.
      node-exp_image = icon_led_red.
    ELSEIF wa_049_obj-contido EQ abap_true.
      node-n_image   = icon_led_green.
      node-exp_image = icon_led_green.
    ELSE.
      node-n_image   = icon_led_inactive.
      node-exp_image = icon_led_inactive.
    ENDIF.
    node-hidden    = ' '.    " The node is visible,
    node-disabled  = ' '.    " selectable,
    node-isfolder  = ' '.    " a folder.
    node-node_key  = wa_049_obj-node_key.
    node-relatkey  = wa_049_obj-node_key_c.
    node-relatship = cl_gui_list_tree=>relat_last_child.
    APPEND node TO node_table.

    CLEAR item.
    item-node_key   = node-node_key.
    item-item_name  = '1'."p_node-node_key. " Item with name '1'
    item-class      = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment  = cl_gui_list_tree=>align_auto.
    item-font       = cl_gui_list_tree=>item_font_prop.
    CONCATENATE wa_049_obj-matkl '-' wa_049_obj-wgbez60 INTO item-text SEPARATED BY space.
    APPEND item TO item_table.

  ENDLOOP.

ENDFORM.                    " ADD_NODE_OBJETO_MERCADORIA

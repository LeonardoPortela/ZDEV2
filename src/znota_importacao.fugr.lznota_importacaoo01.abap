*----------------------------------------------------------------------*
***INCLUDE LZNOTA_IMPORTACAOO01 .
*----------------------------------------------------------------------*
  TYPES: BEGIN OF ty_nota_import,
           o_vbc      TYPE znota_import_ii-o_vbc,
           o_vdespadu TYPE znota_import_ii-o_vdespadu,
           o_vii      TYPE znota_import_ii-o_vii,
           o_viof     TYPE znota_import_ii-o_viof,
         END OF ty_nota_import.

  DATA: it_notas     TYPE TABLE OF ty_nota_import,
        wl_notas_imp TYPE ty_nota_import,
        wa_notas     TYPE ty_nota_import,
        wa_grava     TYPE znota_import_ii,
        wl_notas_aux TYPE znota_import_ii.

  DATA: g_custom_container TYPE REF TO cl_gui_custom_container,
        "G_TREE TYPE REF TO CL_GUI_LIST_TREE.
        g_tree             TYPE REF TO cl_gui_alv_tree,
        wl_fcat            TYPE lvc_s_fcat,
        gt_fcat_0001       TYPE TABLE OF lvc_s_fcat.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE status_0001 OUTPUT.

    IF imp_dynnr_000 IS INITIAL.
      imp_dynnr_000 = c_0002.
    ENDIF.

    SET PF-STATUS 'PF_IMP'.
    SET TITLEBAR 'TL_IMP'.
    PERFORM cria_dock.

  ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_DOCK
*&---------------------------------------------------------------------*
  FORM cria_dock .

    DATA: node_table  TYPE treev_ntab,
          item_table  TYPE item_table_type,
          events      TYPE cntl_simple_events,
          event       TYPE cntl_simple_event,
          vg_node_key TYPE tv_nodekey,
          l_header    TYPE treev_hhdr.

    IF cria_dock_tela IS INITIAL.

      CLEAR: g_tree, g_custom_container,
             events, wl_fcat , gt_fcat_0001,
             it_menu_tree, wa_menu_tree, l_header.

      FREE: g_tree, g_custom_container.
      FREE: it_menu_tree, wa_menu_tree.

      CALL METHOD cl_gui_cfw=>flush.
      CALL METHOD cl_gui_cfw=>remove_object
        EXPORTING
          ref = g_tree.

      CREATE OBJECT g_custom_container
        EXPORTING
          container_name              = 'TREECONTEXTO'
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.

      "Old 04.02.2016 >>>>>>
*    CREATE OBJECT G_TREE
*      EXPORTING
*        PARENT                      = G_CUSTOM_CONTAINER
*        NODE_SELECTION_MODE         = CL_GUI_LIST_TREE=>NODE_SEL_MODE_SINGLE
*        ITEM_SELECTION              = 'X'
*        WITH_HEADERS                = ' '
*      EXCEPTIONS
*        CNTL_SYSTEM_ERROR           = 1
*        CREATE_ERROR                = 2
*        FAILED                      = 3
*        ILLEGAL_NODE_SELECTION_MODE = 4
*        LIFETIME_ERROR              = 5.
      "<<<<<

      CREATE OBJECT g_tree
        EXPORTING
          parent              = g_custom_container "OBJ_DOCKING
          node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
          item_selection      = ' '
          no_html_header      = 'X'
          no_toolbar          = 'X'.


      l_header-heading = 'Notas'.
      l_header-tooltip = 'Notas'.
      l_header-width   = 20.

      wl_fcat-fieldname = ' '.
      wl_fcat-no_out    = 'X'.
      wl_fcat-key       = ' '.
      APPEND wl_fcat TO gt_fcat_0001.
      CLEAR wl_fcat.

      CALL METHOD g_tree->set_table_for_first_display
        EXPORTING
          is_hierarchy_header = l_header
        CHANGING
          it_outtab           = it_menu_tree
          it_fieldcatalog     = gt_fcat_0001.

      CALL METHOD g_tree->delete_all_nodes.
      PERFORM create_hierarchy.

      CLEAR event.
      event-eventid = cl_gui_column_tree=>eventid_node_double_click.
      event-appl_event = 'X'.
      APPEND event TO events.

      event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
      APPEND event TO events.
      CLEAR event.

      CALL METHOD g_tree->set_registered_events
        EXPORTING
          events                    = events
        EXCEPTIONS
          cntl_error                = 1
          cntl_system_error         = 2
          illegal_event_combination = 3.

      "<<<<<<

      "SET HANDLER G_APPLICATION->HANDLE_ITEM_DOUBLE_CLICK FOR G_TREE.

      "PERFORM BUILD_NODE_AND_ITEM_TABLE USING NODE_TABLE ITEM_TABLE.

*    CALL METHOD G_TREE->ADD_NODES_AND_ITEMS
*      EXPORTING
*        NODE_TABLE                     = NODE_TABLE
*        ITEM_TABLE                     = ITEM_TABLE
*        ITEM_TABLE_STRUCTURE_NAME      = 'MTREEITM'
*      EXCEPTIONS
*        FAILED                         = 1
*        CNTL_SYSTEM_ERROR              = 3
*        ERROR_IN_TABLES                = 4
*        DP_ERROR                       = 5
*        TABLE_STRUCTURE_NAME_NOT_FOUND = 6.
*


      READ TABLE it_menu_tree INTO wa_menu_tree WITH KEY node_pai = vg_docnum.
      IF sy-subrc = 0.
        vg_node_key = wa_menu_tree-node_pai_key.
      ENDIF.


      CALL METHOD g_tree->expand_node
        EXPORTING
          i_node_key          = vg_node_key                   "#EC NOTEXT
        EXCEPTIONS
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          node_not_found      = 4
          cannot_expand_leaf  = 5.

      cria_dock_tela = c_x.

    ENDIF.

    SET HANDLER obj_tree_event_receiver->handle_double_click FOR g_tree.
    CALL METHOD g_tree->frontend_update.
    CALL METHOD g_tree->update_calculations.



  ENDFORM.                    " CRIA_DOCK

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE user_command_exit INPUT.
    LEAVE TO SCREEN 0.
  ENDMODULE.                 " USER_COMMAND_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE user_command_0001 INPUT.

    DATA: return_code TYPE i.

    CALL METHOD cl_gui_cfw=>dispatch
      IMPORTING
        return_code = return_code.

    IF return_code <> cl_gui_cfw=>rc_noevent.
      " a control event occured => exit PAI
      CLEAR ok_code.
      EXIT.
    ENDIF.

    CASE ok_code.
      WHEN ok_nova_di.
        ok_di_altera = c_x.
        IF imp_dynnr_002 EQ c_2001.
          CLEAR znota_import.
          znota_import-docnum     = vg_docnum.
          znota_import-itmnum     = vg_itmnum.
          znota_import-paisdesenb = 'BR'.
          CALL SCREEN 2002 STARTING AT 20 10 ENDING AT 80 21.
        ELSE.
          CLEAR znota_import_ad.
          MOVE-CORRESPONDING znota_import TO znota_import_ad.
          znota_import_ad-cfabricante = znota_import-cexportador.
          CALL SCREEN 2004 STARTING AT 20 10 ENDING AT 80 21.
        ENDIF.
      WHEN 'IMPORT'. "163040 CS2024001206 Dados importação em massa PSA
        PERFORM ALV_LM.
    ENDCASE.

  ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Form  BUILD_NODE_AND_ITEM_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_TABLE  text
*      -->P_ITEM_TABLE  text
*----------------------------------------------------------------------*
  FORM build_node_and_item_table
    USING
      node_table TYPE treev_ntab
      item_table TYPE item_table_type.

    DATA: node        TYPE treev_node,
          item        TYPE mtreeitm,
          vg_node_key TYPE tv_nodekey,
          vg_node_rel TYPE tv_nodekey.

    vg_node_key = vg_docnum.

    node-node_key = vg_node_key.

    CLEAR node-relatkey.
    CLEAR node-relatship.

    node-hidden = ' '.
    node-disabled = ' '.
    node-isfolder = 'X'.
    CLEAR node-n_image.
    CLEAR node-exp_image.
    node-expander = 'X'.
    APPEND node TO node_table.

    CLEAR item.
    item-node_key = vg_node_key.
    item-item_name = vg_node_key.
    item-class = cl_gui_list_tree=>item_class_text. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-font = cl_gui_list_tree=>item_font_prop.
    item-text = vg_docnum.
    APPEND item TO item_table.

    CLEAR: it_j_1bnflin.

    SELECT * INTO TABLE it_j_1bnflin
      FROM j_1bnflin
     WHERE docnum EQ vg_docnum.

    LOOP AT it_j_1bnflin INTO wa_j_1bnflin.

      CLEAR node.
      vg_node_rel = wa_j_1bnflin-itmnum.

      CLEAR node.
      node-node_key = vg_node_rel.
      node-relatkey = vg_node_key.
      node-relatship = cl_gui_list_tree=>relat_last_child.
      APPEND node TO node_table.

      CLEAR item.
      item-node_key = vg_node_rel.
      item-item_name = vg_node_rel.
      item-class = cl_gui_list_tree=>item_class_text. " Text Item
      item-alignment = cl_gui_list_tree=>align_auto.
      item-font = cl_gui_list_tree=>item_font_prop.
      CONCATENATE wa_j_1bnflin-itmnum '-' wa_j_1bnflin-maktx INTO item-text.
      APPEND item TO item_table.

      IF vg_itmnum IS INITIAL.
        vg_itmnum = wa_j_1bnflin-itmnum.
        "concatenate wa_j_1bnflin-itmnum '-' wa_j_1bnflin-maktx into vg_item_selecionado.
        "perform pesquisa_di using vg_docnum vg_itmnum.
        PERFORM atualizada_telas_item USING node-node_key. "VG_NODE_REL.
      ENDIF.

    ENDLOOP.

  ENDFORM.                    " BUILD_NODE_AND_ITEM_TABLE

*&---------------------------------------------------------------------*
*&      Form  ATUALIZADA_TELAS_ITEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM atualizada_telas_item  USING    p_node_key  TYPE tv_nodekey.
    "P_ITEM_NAME TYPE TV_NODEKEY.

    MOVE: p_node_key TO wa_j_1bnflin-itmnum,
          vg_docnum  TO wa_j_1bnflin-docnum.

    READ TABLE it_j_1bnflin INTO wa_j_1bnflin WITH KEY itmnum = wa_j_1bnflin-itmnum.

    IF sy-subrc IS INITIAL.
      CONCATENATE wa_j_1bnflin-itmnum '-' wa_j_1bnflin-maktx INTO vg_item_selecionado.
      vg_itmnum = wa_j_1bnflin-itmnum.
      CLEAR: it_znota_import.

      SELECT SINGLE * INTO znota_import_ii
        FROM znota_import_ii
       WHERE docnum EQ wa_j_1bnflin-docnum
         AND itmnum EQ wa_j_1bnflin-itmnum.

      IF NOT sy-subrc IS INITIAL.
        znota_import_ii-docnum = wa_j_1bnflin-docnum.
        znota_import_ii-itmnum = wa_j_1bnflin-itmnum.
      ENDIF.

      SELECT *
        FROM znota_import_ii
        INTO CORRESPONDING FIELDS OF TABLE it_notas
       WHERE docnum EQ wa_j_1bnflin-docnum
         AND itmnum EQ wa_j_1bnflin-itmnum.

      IF sy-subrc = 0.
        LOOP AT it_notas INTO wl_notas_imp.
          MOVE-CORRESPONDING wl_notas_imp TO wa_notas.
        ENDLOOP.
      ENDIF.


      PERFORM pesquisa_di USING wa_j_1bnflin-docnum wa_j_1bnflin-itmnum.
    ELSE.
      CLEAR: vg_item_selecionado.
    ENDIF.

  ENDFORM.                    " ATUALIZADA_TELAS_ITEM

*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
  FORM z_estrutura_fieldcat TABLES it_catalogo TYPE lvc_t_fcat
                             USING p_tab_name
                                   p_fieldname
                                   p_texto_grande
                                   p_hot
                                   p_posicao
                                   p_outputlen
                                   p_fix_column
                                   p_convexit
                                   p_do_sum
                                   p_icon
                                   p_just
                                   p_emphasize.

    DATA: imp_catalog TYPE lvc_s_fcat.

    CLEAR imp_catalog.
    imp_catalog-tabname     = p_tab_name.
    imp_catalog-fieldname   = p_fieldname.
    imp_catalog-scrtext_l   = p_texto_grande.
    imp_catalog-scrtext_m   = p_texto_grande.
    imp_catalog-scrtext_s   = p_texto_grande.
    imp_catalog-hotspot     = p_hot.
    imp_catalog-col_pos     = p_posicao.
    imp_catalog-outputlen   = p_outputlen.
    imp_catalog-fix_column  = p_fix_column.
    imp_catalog-convexit    = p_convexit.
    imp_catalog-do_sum      = p_do_sum.
    imp_catalog-icon        = p_icon.
    imp_catalog-just        = p_just.
    imp_catalog-emphasize   = p_emphasize.
    APPEND imp_catalog TO it_catalogo.
  ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE user_command_0003 INPUT.

    CASE ok_code.
      WHEN ok_ii_salvar.
        IF ( NOT znota_import_ii-docnum IS INITIAL ) AND ( NOT znota_import_ii-itmnum IS INITIAL ).
          wa_grava-docnum     = vg_docnum.
          wa_grava-itmnum     = vg_itmnum.
          MOVE-CORRESPONDING wa_notas TO wa_grava.
*        WA_GRAVA-O_VBC      = WA_NOTAS-O_VBC.
*        WA_GRAVA-O_VDESPADU = WA_NOTAS-O_VDESPADU.
*        WA_GRAVA-O_VII      = WA_NOTAS-O_VII.
*        WA_GRAVA-O_VIOF     = WA_NOTAS-O_VIOF.

          MODIFY znota_import_ii FROM wa_grava.
          COMMIT WORK.
        ENDIF.

    ENDCASE.

  ENDMODULE.                 " USER_COMMAND_0003  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE status_0003 OUTPUT.

    LOOP AT SCREEN.
      IF ( screen-name(15) EQ 'ZNOTA_IMPORT_II' ).
        IF ( vg_item_selecionado IS INITIAL ).
          screen-output = '1'.
          screen-input  = '0'.
        ELSE.
          screen-output = '1'.
          screen-input  = '1'.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDMODULE.                 " STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  CREATE_HIERARCHY
*&---------------------------------------------------------------------*
  FORM create_hierarchy .

    DATA: p_relat_key    TYPE lvc_nkey,
          p_pai_key      TYPE lvc_nkey,
          p_filho_key    TYPE lvc_nkey,
          l_node_text    TYPE lvc_value,
          p_node_key     TYPE tv_nodekey,
          p_add_node_pai TYPE c.

    CLEAR: it_menu_tree, it_j_1bnflin, wa_menu_tree, vg_itmnum,
           p_pai_key ,   p_filho_key, l_node_text, p_node_key,
           p_add_node_pai.

    SELECT * INTO TABLE it_j_1bnflin
      FROM j_1bnflin
     WHERE docnum EQ vg_docnum.

    LOOP AT it_j_1bnflin INTO wa_j_1bnflin.

      wa_menu_tree-node_pai         = vg_docnum.
      wa_menu_tree-node_image_pai   = icon_closed_folder.
      CONCATENATE wa_j_1bnflin-itmnum '-' wa_j_1bnflin-maktx INTO wa_menu_tree-node_filho.
      APPEND wa_menu_tree TO it_menu_tree.

      IF vg_itmnum IS INITIAL.
        vg_itmnum = wa_j_1bnflin-itmnum.
        MOVE wa_menu_tree-node_filho(6) TO p_node_key.
        PERFORM atualizada_telas_item USING p_node_key.
      ENDIF.

    ENDLOOP.

    LOOP AT it_menu_tree INTO wa_menu_tree.
      IF NOT wa_menu_tree IS INITIAL.

        IF p_add_node_pai IS INITIAL.
          p_add_node_pai = 'X'.
          PERFORM add_pai USING space
                                wa_menu_tree-node_image_pai
                                wa_menu_tree-node_pai
                                CHANGING p_pai_key.
        ENDIF.

        "ON CHANGE OF WA_MENU_TREE-NODE_FILHO.
        PERFORM add_filho USING p_pai_key
                              wa_menu_tree-node_image_filho
                              wa_menu_tree-node_filho
                             CHANGING p_filho_key.
        "ENDON.

        wa_menu_tree-node_filho_key = p_filho_key.
        wa_menu_tree-node_pai_key   = p_pai_key.
        MODIFY it_menu_tree FROM wa_menu_tree.
        CLEAR wa_menu_tree.
      ENDIF.
    ENDLOOP.



  ENDFORM.                    " CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*&      Form  ADD_PAI
*&---------------------------------------------------------------------*
  FORM add_pai USING    p_relat_key TYPE lvc_nkey
                        p_node_image TYPE tv_image
                        p_node_pai  TYPE char50
               CHANGING p_pai_key   TYPE lvc_nkey.

    DATA: l_node_text   TYPE lvc_value,
          l_layout_node TYPE lvc_s_layn.

    l_node_text = p_node_pai.
    l_layout_node-n_image = p_node_image.

    CALL METHOD g_tree->add_node
      EXPORTING
        i_relat_node_key = p_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
        is_node_layout   = l_layout_node
      IMPORTING
        e_new_node_key   = p_pai_key.

  ENDFORM.                    "ADD_PAI
*&---------------------------------------------------------------------*
*&      FORM  ADD_FILHO
*&---------------------------------------------------------------------*
  FORM add_filho USING  p_pai_key    TYPE lvc_nkey
                        p_node_image TYPE tv_image
                        p_node_filho TYPE char50
               CHANGING p_filho_key  TYPE lvc_nkey.

    DATA: l_node_text   TYPE lvc_value,
          l_layout_node TYPE lvc_s_layn.

    l_node_text   = p_node_filho.
    l_layout_node-n_image = p_node_image.

    CALL METHOD g_tree->add_node
      EXPORTING
        i_relat_node_key = p_pai_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
        is_node_layout   = l_layout_node
      IMPORTING
        e_new_node_key   = p_filho_key.

  ENDFORM.                    "ADD_FILHO

  MODULE buid_options OUTPUT.

    DATA: values  TYPE vrm_values WITH HEADER LINE.

    IF init IS INITIAL.

      REFRESH: values.
      CLEAR: values.

      values-text = 'Marítima'.
      values-key  = '1'.
      APPEND values.

      values-text = 'Fluvial'.
      values-key  = '2'.
      APPEND values.

      values-text = 'Lacustre'.
      values-key  = '3'.
      APPEND values.

      values-text = 'Aérea'.
      values-key  = '4'.
      APPEND values.

      values-text = 'Postal '.
      values-key  = '5'.
      APPEND values.

      values-text = 'Ferroviária'.
      values-key  = '6'.
      APPEND values.

      values-text = 'Rodoviária'.
      values-key  = '7'.
      APPEND values.

      values-text = 'Conduto / Rede Transmissão'.
      values-key  = '8'.
      APPEND values.

      values-text = 'Meios Próprios'.
      values-key  = '9'.
      APPEND values.

      values-text = 'Entrada / Saída ficta'.
      values-key  = '10'.
      APPEND values.

      values-text = 'Courier'.
      values-key  = '11'.
      APPEND values.

      values-text = 'Em Mãos'.
      values-key  = '12'.
      APPEND values.

      values-text = 'Por reboque'.
      values-key  = '13'.
      APPEND values.

      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id              = 'ZNOTA_IMPORT-TPVIATRANSP'
          values          = values[]
        EXCEPTIONS
          id_illegal_name = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      REFRESH values.
      values-text = 'Importação por conta própria'.
      values-key  = '1'.
      APPEND values.

      values-text = 'Importação por conta e ordem'.
      values-key  = '2'.
      APPEND values.

      values-text = 'Importação por encomenda'.
      values-key  = '3'.
      APPEND values.

      CALL FUNCTION 'VRM_SET_VALUES'
        EXPORTING
          id              = 'ZNOTA_IMPORT-TPINTERMEDIO'
          values          = values[]
        EXCEPTIONS
          id_illegal_name = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      init = c_x.

    ENDIF.

  ENDMODULE.                 " BUID_OPTIONS  OUTPUT

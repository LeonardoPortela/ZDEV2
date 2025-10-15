************************************************************************
* A M A G G I  E X P O R T A Ç Ã  O  E  I M P O R T A Ç Ã O  L T D A.  *
*                                                                      *
************************************************************************
* Responsável ...: Amaggi Importação & Exportação Ltda                 *
* Data desenv ...: 19.06.2008                                          *
* Tipo de prg ...: Include                                             *
* Objetivo    ...: Relatório de aprensantação da DRE                   *
*                                                                      *
************************************************************************
* Data Modif    Autor                Descriçao            Request      *
************************************************************************
* 26.06.2008    Michely              Criação              DEVK904304   *
* 31.07.2008    Marcus.Barbara       Alteração            DEVK904591   *
* 16.12.2008    Marcus.Barbara       Alteração            DEVK905347   *
* 03.03.2009    Marcus.Barbara       Alteração            DEVK905577   *
* 06.03.2009    Marcus.Barbara       Alteração            DEVK905609   *
* 10.03.2009    Marcus.Barbara       Alteração            DEVK905623   *
* 10.03.2009    Marcus.Barbara       Alteração            DEVK905625   *
*                                                                      *
************************************************************************
***INCLUDE ZGL004_INCLUDE .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Metodo e clases do evendo duplo clique
*&---------------------------------------------------------------------*

CLASS lcl_tree_event_receiver DEFINITION.

  PUBLIC SECTION.

    METHODS handle_item_double_click
    FOR EVENT item_double_click OF cl_gui_alv_tree
    IMPORTING node_key
      fieldname.

ENDCLASS.                    "lcl_tree_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_tree_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_tree_event_receiver IMPLEMENTATION.
  METHOD handle_item_double_click.
    DATA: lt_selected_nodes TYPE lvc_t_nkey,
          l_node_key        TYPE lvc_nkey,
          l_fieldname       TYPE lvc_fname.

    REFRESH: lt_selected_nodes.
    CALL METHOD tree1->get_selected_nodes
      CHANGING
        ct_selected_nodes = lt_selected_nodes.
    IF lt_selected_nodes[] IS INITIAL.
      CALL METHOD tree1->get_selected_item
        IMPORTING
          e_selected_node = l_node_key
          e_fieldname     = l_fieldname.
    ELSE.
      READ TABLE lt_selected_nodes INTO l_node_key INDEX 1.
    ENDIF.
    IF NOT l_node_key IS INITIAL.
      PERFORM f_tree_double_click USING l_node_key.
    ENDIF.
  ENDMETHOD.                    "handle_item_double_click
ENDCLASS.                    "lcl_tree_event_receiver IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Form  F_INICIA_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_inicia_tree .

  DATA: vl_tree_cont_name         TYPE c LENGTH 30,
        vl_custom_container       TYPE REF TO cl_gui_custom_container,
        vl_list_commentary        TYPE slis_t_listheader,
        vl_logo                   TYPE sdydo_value,
        vl_variant                TYPE disvariant,
        vl_hierarchy_header       TYPE treev_hhdr.

  SORT it_dados BY ordnv txt50 ltext.

* Crio a fieldcatalog para a structure do relatório
  PERFORM f_fieldcatalog.

* Crio a container para alv-tree
  vl_tree_cont_name = 'TREE1'.

  IF sy-batch IS INITIAL.
    IF vl_custom_container IS INITIAL.
      CREATE OBJECT vl_custom_container
        EXPORTING
          container_name              = vl_tree_cont_name
        EXCEPTIONS
          cntl_error                  = 1
          cntl_system_error           = 2
          create_error                = 3
          lifetime_error              = 4
          lifetime_dynpro_dynpro_link = 5.
      IF sy-subrc <> 0.
        MESSAGE x208(00) WITH 'ERROR'.                      "#EC NOTEXT
      ENDIF.
    ENDIF.
  ENDIF.

* Crio tree control
  IF tree1 IS INITIAL.
    CREATE OBJECT tree1
      EXPORTING
        parent                      = vl_custom_container
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = 'X'
        no_html_header              = ''
        no_toolbar                  = ''
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        illegal_node_selection_mode = 5
        failed                      = 6
        illegal_column_name         = 7.
    IF sy-subrc <> 0.
      MESSAGE x208(00) WITH 'ERROR'.                        "#EC NOTEXT
    ENDIF.
  ENDIF.

  PERFORM f_hierarchy_header_ CHANGING vl_hierarchy_header.

* Monto cabeçalho do ALV
  PERFORM f_cabecalho USING vl_list_commentary.

  vl_variant-report = sy-repid.

* create emty tree-control
  CALL METHOD tree1->set_table_for_first_display
    EXPORTING
      is_hierarchy_header = vl_hierarchy_header
      it_list_commentary  = vl_list_commentary
*      i_logo              = vl_logo
      i_background_id     = 'ALV_BACKGROUND'
      i_save              = 'A'
      is_variant          = vl_variant
    CHANGING
      it_outtab           = it_alvtree"table must be emty !!
      it_fieldcatalog     = it_fieldcatalog.

* create hierarchy
*  perform f_create_hierarchy.
  PERFORM select_data_and_fill_col_tree.

* add own functioncodes to the toolbar
  PERFORM f_change_toolbar.

* register events
  PERFORM f_register_events.
*
** adjust column_width
** call method tree1->COLUMN_OPTIMIZE.
ENDFORM.                    " F_INICIA_TREE
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_fieldcatalog .
  DATA: wa_fieldcatalog  TYPE lvc_s_fcat.

* busco fieldcatalog
  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZST_GL004_DADOS'
    CHANGING
      ct_fieldcat      = it_fieldcatalog.

*  sort it_fieldcatalog by scrtext_l.

* Altero fieldcatalog
  LOOP AT it_fieldcatalog INTO wa_fieldcatalog.
    CASE wa_fieldcatalog-fieldname.
      WHEN 'NIVEL'  OR 'SAKNR' OR 'DESNVL' OR 'ORDNV' OR
           'TLEVEL' OR 'TXT50' OR 'KOSTL'  OR 'AUFNR' OR
           'PRCTR'  OR 'LTEXT' OR 'NVL1' OR 'NVL2' OR
           'NVL3'   OR 'NVL4'  OR 'NVL5' OR 'NVL6' OR
           'NVL7'   OR 'NVL8'  OR 'NVL9' OR 'NVL10'.
        wa_fieldcatalog-no_out = 'X'.
        wa_fieldcatalog-key    = ''.
      WHEN 'QTD_TON'.
        wa_fieldcatalog-reptext   = '[Mês]Ton'.
        wa_fieldcatalog-do_sum    = 'X'.
        wa_fieldcatalog-valexi    = 'X'.
        wa_fieldcatalog-outputlen = 35.
      WHEN 'VLR_REA'.
        wa_fieldcatalog-reptext = '[Mês]Realizado'.
        wa_fieldcatalog-do_sum = 'X'.
        wa_fieldcatalog-valexi = 'X'.
        wa_fieldcatalog-outputlen = 40.
      WHEN 'ANA_VLR'.
        wa_fieldcatalog-reptext   = '[Mês]Análise Vlr/Ton'.
        wa_fieldcatalog-outputlen = 10.
      WHEN 'QTD_ACM'.
        wa_fieldcatalog-reptext = '[Acumulado]Ton'.
        wa_fieldcatalog-do_sum = 'X'.
        wa_fieldcatalog-valexi = 'X'.
        wa_fieldcatalog-outputlen = 35.
      WHEN 'VLR_ACM'.
        wa_fieldcatalog-reptext = '[Acumulado]Realizado'.
        wa_fieldcatalog-do_sum = 'X'.
        wa_fieldcatalog-valexi = 'X'.
        wa_fieldcatalog-outputlen = 40.
      WHEN 'ANA_ACM'.
        wa_fieldcatalog-reptext = '[Acumulado]Análise Vlr/Ton'.
        wa_fieldcatalog-outputlen = 10.
    ENDCASE.

    MODIFY it_fieldcatalog FROM wa_fieldcatalog.
  ENDLOOP.
ENDFORM.                    " F_FIELDCATALOG

*&---------------------------------------------------------------------*
*&      Form  F_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_cabecalho  USING    p_list_commentary TYPE slis_t_listheader.
  DATA: vl_line          TYPE slis_listheader,
        vl_txt           TYPE c LENGTH 50,
        vl_empresa       TYPE c LENGTH 50,
        vl_data          TYPE c LENGTH 14,
        vl_hora          TYPE c LENGTH 8.

  CALL FUNCTION 'MONTH_NAMES_GET'
    EXPORTING
      language    = sy-langu
    TABLES
      month_names = it_mes.

  READ TABLE it_mes INTO wa_mes WITH KEY mnr = wa_dre_005-monat.
  vl_txt = wa_mes-ltx.
  SELECT SINGLE butxt
    FROM t001
    INTO vl_empresa
   WHERE bukrs EQ wa_dre_005-bukrs.

* Primeira linha: TYPE H
  CONCATENATE vg_titulo '['
              vl_txt '-' wa_dre_005-gjahr ']' INTO vl_txt.

  CLEAR vl_line.
  vl_line-typ  = 'H'.
  vl_line-info = vl_txt.
  APPEND vl_line TO p_list_commentary.
  CLEAR vl_line.
  vl_line-typ  = 'S'.
  vl_line-key = vl_empresa.
  APPEND vl_line TO p_list_commentary.

  CLEAR vl_line.
  vl_line-typ  = 'S'.
  vl_line-key  = 'Moeda:'.
  vl_line-info = wa_dre_001-waers.
  APPEND vl_line TO p_list_commentary.

  CONCATENATE wa_dre_005-datum+6(2)
              '.' wa_dre_005-datum+4(2)
              '.' wa_dre_005-datum(4) INTO vl_data.

  CONCATENATE wa_dre_005-uzeit(2)
              ':' wa_dre_005-uzeit+2(2)
              ':' wa_dre_005-uzeit+4(2) INTO vl_hora.

  CONCATENATE  wa_dre_005-uname
              'em' vl_data
              '-' vl_hora INTO vl_txt
              SEPARATED BY space.
  CLEAR vl_line.
  vl_line-typ  = 'S'.
  vl_line-key  = 'Gerado por:'.
  vl_line-info = vl_txt.
  APPEND vl_line TO p_list_commentary.

*  p_logo = 'ENJOYSAP_LOGO'.
ENDFORM.                    " F_CABECALHO

*&---------------------------------------------------------------------*
*&      Form  F_EXIT_PROGRAM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_exit_program .
  CALL METHOD tree1->free.
  CALL METHOD go_tbm->save_state.
ENDFORM.                    " F_EXIT_PROGRAM
*&---------------------------------------------------------------------*
*&      Form  F_CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_change_toolbar.
  DATA:
        lt_button_alv  TYPE ttb_button,
        lt_button_appl TYPE ttb_button.

* get toolbar control
  CALL METHOD tree1->get_toolbar_object
    IMPORTING
      er_toolbar = mr_toolbar.

  CHECK NOT mr_toolbar IS INITIAL.

  lt_button_alv = mr_toolbar->m_table_button.

  CALL METHOD mr_toolbar->delete_all_buttons
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.

* Add Standard Button to toolbar (for Delete Subtree)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = 'DELETE'
      icon      = '@18@'
      butn_type = cntb_btype_button
      text      = ''
      quickinfo = 'Delete subtree'.                         "#EC NOTEXT

* add Dropdown Button to toolbar (for Insert Line)
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = 'INSERT_LC'
      icon      = '@17@'
      butn_type = cntb_btype_dropdown
      text      = ''
      quickinfo = 'Insert Line'.                            "#EC NOTEXT

* add seperator to toolbar
  CALL METHOD mr_toolbar->add_button
    EXPORTING
      fcode     = ''
      icon      = ''
      butn_type = cntb_btype_sep
      text      = ''
      quickinfo = 'This is a Seperator'.                    "#EC NOTEXT

  lt_button_appl = mr_toolbar->m_table_button.

  INSERT LINES OF lt_button_appl INTO lt_button_alv INDEX 1.

  CALL METHOD mr_toolbar->delete_all_buttons
    EXCEPTIONS
      cntl_error = 1
      OTHERS     = 2.

  CALL METHOD mr_toolbar->add_button_group
    EXPORTING
      data_table       = lt_button_alv
    EXCEPTIONS
      dp_error         = 1
      cntb_error_fcode = 2
      OTHERS           = 3.

* set event-handler for toolbar-control
  CREATE OBJECT toolbar_event_receiver.

  SET HANDLER: toolbar_event_receiver->on_function_selected FOR mr_toolbar,
  toolbar_event_receiver->on_toolbar_dropdown  FOR mr_toolbar.

  CREATE OBJECT go_tbm
    EXPORTING
      io_alv_tree = tree1.

  go_tbm->reorganize( ).

ENDFORM.                    " F_CHANGE_TOOLBAR
*&---------------------------------------------------------------------*
*&      Form  F_REGISTER_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_register_events .
* define the events which will be passed to the backend
  DATA: lt_events TYPE cntl_simple_events,
        l_event TYPE cntl_simple_event.

* define the events which will be passed to the backend
  CALL METHOD tree1->get_registered_events
    IMPORTING
      events = lt_events.

  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  APPEND l_event TO lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_checkbox_change.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_header_context_men_req.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_node_context_menu_req.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_item_context_menu_req.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_header_click.
**  append l_event to lt_events.
**  l_event-eventid = cl_gui_column_tree=>eventid_item_keypress.
**  append l_event to lt_events.
**
  CALL METHOD tree1->set_registered_events
    EXPORTING
      events                    = lt_events
    EXCEPTIONS
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  IF sy-subrc <> 0.
    MESSAGE x208(00) WITH 'ERROR'.                          "#EC NOTEXT
  ENDIF.
**
*** set Handler
  DATA: l_event_receiver TYPE REF TO lcl_tree_event_receiver.
*  create object l_event_receiver.
*  set handler l_event_receiver->handle_item_double_click
*  for tree1.
**  set handler l_event_receiver->handle_node_ctmenu_selected
**  for tree1.
**  set handler l_event_receiver->handle_item_ctmenu_request
**  for tree1.
**  set handler l_event_receiver->handle_item_ctmenu_selected
**  for tree1.
***  *§4d. Register events on backend (ABAP Objects event handling)
  CREATE OBJECT l_event_receiver.
  SET HANDLER l_event_receiver->handle_item_double_click FOR tree1.
ENDFORM.                    " F_REGISTER_EVENTS

*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
MODULE pbo OUTPUT.
  IF sy-dynnr = 1000.
    IF ( sy-ucomm EQ 'BACK') OR ( sy-ucomm EQ 'EXIT' ) OR ( sy-ucomm EQ 'CANC' ).
      LEAVE SCREEN .
    ELSEIF sy-ucomm EQ 'EXEC'.
      CALL SCREEN 100.
    ELSE.
      SET PF-STATUS 'TELA_1000'.
    ENDIF.
  ENDIF.

  IF sy-dynnr = 100.
    IF ok_code IS INITIAL.

      SET TITLEBAR 'TITULO'.
      SET PF-STATUS 'TELA_2000'.
      PERFORM f_inicia_tree.
      CALL METHOD cl_gui_cfw=>flush
        EXCEPTIONS
          cntl_system_error = 1
          cntl_error        = 2.

    ELSEIF ( ok_code EQ 'BACK' ) OR ( ok_code EQ 'EXIT') OR ( ok_code EQ 'CANC' ).
      SET PF-STATUS 'TELA_1000'.
      LEAVE TO SCREEN 1000.
*      CALL SELECTION-SCREEN 1000.
*      IF ( OK_CODE EQ 'BACK' ) OR ( OK_CODE EQ 'EXIT') OR ( OK_CODE EQ 'CANC').
*        LEAVE PROGRAM.
*      ENDIF.
    ENDIF.
  ENDIF.
ENDMODULE.                 " PBO  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
MODULE pai INPUT.

  IF sy-dynnr = 100.
    IF ( ok_code EQ 'BACK' ) OR ( ok_code EQ 'EXIT') OR ( ok_code EQ 'CANC').
      CLEAR sy-ucomm.
      PERFORM f_exit_program.
*      SET PF-STATUS 'TELA_1000'.
*      LEAVE SCREEN.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  IF sy-dynnr = 1000.
    IF ( sy-ucomm EQ 'BACK') OR ( sy-ucomm EQ 'EXIT' ) OR ( sy-ucomm EQ 'CANC' ).
      LEAVE PROGRAM.
    ELSEIF sy-ucomm EQ 'EXEC'.
      CLEAR: ok_code.
      CALL SCREEN 100.
    ENDIF.
  ENDIF.

ENDMODULE.                 " PAI  INPUT
*&---------------------------------------------------------------------*
*&      Form  SELECT_DATA_AND_FILL_COL_TREE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM select_data_and_fill_col_tree.
  DATA: l_node_text      TYPE lvc_value,
        l_nvl1_key       TYPE lvc_nkey,
        l_nvl2_key       TYPE lvc_nkey,
        l_nvl3_key       TYPE lvc_nkey,
        l_nvl4_key       TYPE lvc_nkey,
        l_nvl5_key       TYPE lvc_nkey,
        l_nvl6_key       TYPE lvc_nkey,
        l_nvl7_key       TYPE lvc_nkey,
        l_nvl8_key       TYPE lvc_nkey,
        l_nvl9_key       TYPE lvc_nkey,
        l_nvl10_key      TYPE lvc_nkey,
        l_root_key       TYPE lvc_nkey,
        l_sak_key        TYPE lvc_nkey,
        l_aux_key        TYPE lvc_nkey,
        l_text_key       TYPE lvc_nkey,
        lt_layout_item   TYPE lvc_t_layi,
        vl_lgnvl         LIKE zgl002_dre_est-lgnvl,
        vl_idex          TYPE sy-tabix,
        texto(50).

  CLEAR vl_lgnvl.
  LOOP AT it_dados INTO wa_dados.
    vl_idex = sy-tabix.

    ON CHANGE OF wa_dados-nvl1.
      IF wa_dados-nvl1 GT 0.
        PERFORM create_item_layouts CHANGING lt_layout_item.
        l_node_text = wa_dados-desnvl.

        READ TABLE it_dre_008 INTO wa_dre_008 WITH KEY nivel = wa_dados-nivel.
        IF sy-subrc EQ 0.
          CALL METHOD tree1->add_node
            EXPORTING
              i_relat_node_key = l_root_key
              i_relationship   = cl_gui_column_tree=>relat_last_child
              i_node_text      = l_node_text
              is_outtab_line   = wa_dados
              it_item_layout   = lt_layout_item
            IMPORTING
              e_new_node_key   = l_nvl1_key.
        ELSE.
          CALL METHOD tree1->add_node
            EXPORTING
              i_relat_node_key = l_root_key
              i_relationship   = cl_gui_column_tree=>relat_last_child
              i_node_text      = l_node_text
              it_item_layout   = lt_layout_item
            IMPORTING
              e_new_node_key   = l_nvl1_key.
        ENDIF.
        l_aux_key = l_nvl1_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl2.
      IF wa_dados-nvl2 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl1_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl2_key.
        l_aux_key = l_nvl2_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl3.
      IF wa_dados-nvl3 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl2_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl3_key.
        l_aux_key = l_nvl3_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl4.
      IF wa_dados-nvl4 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl3_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl4_key.
        l_aux_key = l_nvl4_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl5.
      IF wa_dados-nvl5 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl4_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl5_key.
        l_aux_key = l_nvl5_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl6.
      IF wa_dados-nvl6 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl5_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl6_key.
        l_aux_key = l_nvl6_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl7.
      IF wa_dados-nvl7 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl6_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl7_key.
        l_aux_key = l_nvl7_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl8.
      IF wa_dados-nvl8 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl7_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl8_key.
        l_aux_key = l_nvl8_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl9.
      IF wa_dados-nvl9 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl8_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl9_key.
        l_aux_key = l_nvl9_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-nvl10.
      IF wa_dados-nvl10 GT 0.
        l_node_text = wa_dados-desnvl.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_nvl9_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_nvl10_key.
        l_aux_key = l_nvl10_key.
        wa_dados-nkey = l_aux_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    ON CHANGE OF wa_dados-saknr.
      IF wa_dados-txt50 IS NOT INITIAL.
        PERFORM create_item_layouts CHANGING lt_layout_item.
        CONCATENATE wa_dados-saknr+4 '-' wa_dados-txt50 INTO texto.
        l_node_text = texto.
        CALL METHOD tree1->add_node
          EXPORTING
            i_relat_node_key = l_aux_key
            i_relationship   = cl_gui_column_tree=>relat_last_child
            i_node_text      = l_node_text
            it_item_layout   = lt_layout_item
          IMPORTING
            e_new_node_key   = l_sak_key.
        wa_dados-nkey = l_sak_key.
        APPEND wa_dados TO it_alvtree.
      ENDIF.
    ENDON.

    IF wa_dados-ltext IS NOT INITIAL.
      PERFORM create_item_layouts CHANGING lt_layout_item.
      WRITE wa_dados-ltext TO l_node_text.
      CALL METHOD tree1->add_node
        EXPORTING
          i_relat_node_key = l_sak_key
          i_relationship   = cl_gui_column_tree=>relat_last_child
          is_outtab_line   = wa_dados
          i_node_text      = l_node_text
          it_item_layout   = lt_layout_item
        IMPORTING
          e_new_node_key   = l_text_key.
      wa_dados-nkey = l_text_key.
      APPEND wa_dados TO it_alvtree.
    ENDIF.

  ENDLOOP.

  CALL METHOD tree1->update_calculations.
  CALL METHOD tree1->frontend_update.
ENDFORM.                    " SELECT_DATA_AND_FILL_COL_TREE
*&---------------------------------------------------------------------*
*&      Form  CREATE_ITEM_LAYOUTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LT_LAYOUT_ITEM  text
*----------------------------------------------------------------------*
FORM create_item_layouts  CHANGING pt_item_layout TYPE lvc_t_layi.
  DATA: ls_fieldcatalog TYPE lvc_s_fcat,
        ls_item_layout TYPE lvc_s_layi.

  CLEAR pt_item_layout.
  LOOP AT it_fieldcatalog INTO ls_fieldcatalog.
    CLEAR ls_item_layout.
    IF ls_fieldcatalog-no_out EQ space.
      ls_item_layout-fieldname = ls_fieldcatalog-fieldname.
      APPEND ls_item_layout TO pt_item_layout.
    ENDIF.

  ENDLOOP.

  CLEAR ls_item_layout.
  ls_item_layout-fieldname = tree1->c_hierarchy_column_name.
  APPEND ls_item_layout TO pt_item_layout.
ENDFORM.                    " CREATE_ITEM_LAYOUTS
*&---------------------------------------------------------------------*
*&      Form  F_HIERARCHY_HEADER_
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_VL_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
FORM f_hierarchy_header_  CHANGING p_hierarchy_header TYPE treev_hhdr.
*  data: vl_header        type treev_hhdr.

  p_hierarchy_header-heading = 'Itens'.
  p_hierarchy_header-tooltip = 'Itens'.
  p_hierarchy_header-width = 35.
  p_hierarchy_header-width_pix = ''.
ENDFORM.                    " F_HIERARCHY_HEADER_

*&---------------------------------------------------------------------*
*&      Form  F_TREE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_L_NODE_KEY  text
*----------------------------------------------------------------------*
FORM f_tree_double_click  USING p_node_key TYPE lvc_nkey.

  DATA: wa_alvtree       TYPE zst_gl004_dados,
        ls_exit          TYPE slis_exit_by_user.

  READ TABLE it_alvtree INTO wa_alvtree WITH KEY nkey = p_node_key.
  IF sy-subrc = 0.

    CLEAR: it_bdcdata.

    IF wa_alvtree-saknr IS NOT INITIAL.
      IF ( wa_alvtree-kostl IS NOT INITIAL ). "Centro Custo KSB1
        PERFORM f_tree_ksb1 USING wa_alvtree-kostl txdataini txdatafim.
      ELSEIF ( wa_alvtree-aufnr IS NOT INITIAL ). "Ordem Interna KOB1
        PERFORM f_tree_kob1 USING wa_alvtree-aufnr txdataini txdatafim.
      ELSEIF ( wa_alvtree-prctr IS NOT INITIAL ). "Centro de lucro KE5Z
        PERFORM f_tree_ke5z USING wa_alvtree-prctr wa_alvtree-saknr wa_dre_005-bukrs wa_dre_005-monat wa_dre_005-gjahr.
      ELSE.
        PERFORM f_tree_faglb03 USING wa_alvtree-saknr wa_dre_005-bukrs wa_dre_005-gjahr.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " F_TREE_DOUBLE_CLICK

*&---------------------------------------------------------------------*
*&      Form  F_INSERT_SHDB
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM f_insert_shdb  USING value(p_code)
                          value(p_fnam)
                          value(p_fval).

  CLEAR wa_bdcdata.

  wa_bdcdata-dynbegin = p_code.

  IF (  p_code EQ c_mark  ).
    wa_bdcdata-program  = p_fnam.
    wa_bdcdata-dynpro   = p_fval.
  ELSE.
    wa_bdcdata-fnam     = p_fnam.
    wa_bdcdata-fval     = p_fval.
  ENDIF.

  APPEND wa_bdcdata TO it_bdcdata.

ENDFORM.                    "f_insert_shdb

*&---------------------------------------------------------------------*
*&      Form  F_TREE_KE5Z
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALVTREE  text
*      -->P_WA_DRE_005_BUKRS  text
*      -->P_WA_DRE_005_MONAT  text
*      -->P_WA_DRE_005_GJAHR  text
*----------------------------------------------------------------------*
FORM f_tree_ke5z  USING    p_wa_alvtree_prctr
                           p_wa_alvtree_saknr
                           p_wa_dre_005_bukrs
                           p_wa_dre_005_monat
                           p_wa_dre_005_gjahr.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'KE5Z'
    EXCEPTIONS
      ok     = 1
      not_ok = 2.
  IF sy-subrc = 2.
    MESSAGE e077(s#) WITH 'KE5Z'.
  ENDIF.


  PERFORM f_insert_shdb USING: 'X' 'RCOPCA02'	'1000',
                               ' ' 'BDC_CURSOR' 'P_VARI',
                               ' ' 'BDC_OKCODE' '=ONLI',
                               ' ' 'RRCTY-LOW'  '0',
                               ' ' 'RVERS-LOW'  '0',
                               ' ' 'KOKRS-LOW'  'MAGI',
                               ' ' 'BUKRS-LOW'  p_wa_dre_005_bukrs,
                               ' ' 'POPER-LOW'  p_wa_dre_005_monat,
                               ' ' 'RYEAR-LOW'  p_wa_dre_005_gjahr,
                               ' ' 'PRCTR-LOW'  p_wa_alvtree_prctr,
                               ' ' 'RACCT-LOW'  p_wa_alvtree_saknr,
                               ' ' 'P_VARI'	'/DRE_CL',
                               'X' 'RCOPCA02'	'1000',
                               ' ' 'BDC_OKCODE'	'/EENDE',
                               ' ' 'BDC_CURSOR'	'RRCTY-LOW'.

  CALL TRANSACTION 'KE5Z' USING it_bdcdata
                           MODE 'E'
                       MESSAGES INTO it_message.

ENDFORM.                    " F_TREE_KE5Z

*&---------------------------------------------------------------------*
*&      Form  F_TREE_FAGLB03
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALVTREE_SAKNR  text
*      -->P_WA_DRE_005_BUKRS  text
*      -->P_WA_DRE_005_GJAHR  text
*----------------------------------------------------------------------*
FORM f_tree_faglb03  USING    p_wa_alvtree_saknr
                              p_wa_dre_005_bukrs
                              p_wa_dre_005_gjahr.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'FAGLB03'
    EXCEPTIONS
      ok     = 1
      not_ok = 2.
  IF sy-subrc = 2.
    MESSAGE e077(s#) WITH 'FAGLB03'.
  ENDIF.
  SET PARAMETER ID: 'ACC' FIELD p_wa_alvtree_saknr,
                    'BUK' FIELD p_wa_dre_005_bukrs,
                    'GJR' FIELD p_wa_dre_005_gjahr.
  CALL TRANSACTION 'FAGLB03' AND SKIP FIRST SCREEN.

ENDFORM.                    " F_TREE_FAGLB03

*&---------------------------------------------------------------------*
*&      Form  F_TREE_KOB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALVTREE_AUFNR  text
*      -->P_TXDATAINI  text
*      -->P_TXDATAFIM  text
*----------------------------------------------------------------------*
FORM f_tree_kob1  USING    p_wa_alvtree_aufnr
                           p_txdataini
                           p_txdatafim.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'KOB1'
    EXCEPTIONS
      ok     = 1
      not_ok = 2.
  IF sy-subrc = 2.
    MESSAGE e077(s#) WITH 'KOB1'.
  ENDIF.

  PERFORM f_insert_shdb USING: 'X' 'SAPLSPO4'	'0300',
                               ' ' 'BDC_CURSOR'	'SVALD-VALUE(01)',
                               ' ' 'BDC_OKCODE'	'=FURT',
                               ' ' 'SVALD-VALUE(01)'  'MAGI',
                               'X' 'RKAEP000'	'0110',
                               ' ' 'BDC_CURSOR'	'P_DISVAR',
                               ' ' 'BDC_OKCODE'	'=ONLI',
                               ' ' 'AUFNR-LOW' p_wa_alvtree_aufnr,
                               ' ' 'R_BUDAT-LOW' p_txdataini,
                               ' ' 'R_BUDAT-HIGH'	p_txdatafim,
                               ' ' 'P_DISVAR'	'/B_ORDEM06',
                               'X' 'SAPLSLVC_FULLSCREEN' '0500',
                               ' ' 'BDC_OKCODE'	'=&F12',
                               'X' 'SAPLSPO1'	'0100',
                               ' ' 'BDC_OKCODE'	'=YES',
                               'X' 'RKAEP000'	'0110',
                               ' ' 'BDC_OKCODE'	'/EENDE',
                               ' ' 'BDC_CURSOR'	'AUFNR-LOW'.

  CALL TRANSACTION 'KOB1' USING it_bdcdata
                           MODE 'E'
                       MESSAGES INTO it_message.

ENDFORM.                    " F_TREE_KOB1

*&---------------------------------------------------------------------*
*&      Form  F_TREE_KSB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ALVTREE_KOSTL  text
*      -->P_TXDATAINI  text
*      -->P_TXDATAFIM  text
*----------------------------------------------------------------------*
FORM f_tree_ksb1  USING    p_wa_alvtree_kostl
                           p_txdataini
                           p_txdatafim.

  CALL FUNCTION 'AUTHORITY_CHECK_TCODE'
    EXPORTING
      tcode  = 'KSB1'
    EXCEPTIONS
      ok     = 1
      not_ok = 2.
  IF sy-subrc = 2.
    MESSAGE e077(s#) WITH 'KSB1'.
  ENDIF.


  PERFORM f_insert_shdb USING: 'X' 'SAPLSPO4'	'0300',
                               ' ' 'BDC_CURSOR'	'SVALD-VALUE(01)',
                               ' ' 'BDC_OKCODE'	'=FURT',
                               ' ' 'SVALD-VALUE(01)'  'MAGI',
                               'X' 'RKAEP000'	'0100',
                               ' ' 'BDC_OKCODE'	'/EE'.

  CALL TRANSACTION 'KSB1' USING it_bdcdata
                           MODE 'N'
                       MESSAGES INTO it_message.

  CLEAR it_bdcdata.

  PERFORM f_insert_shdb USING: 'X' 'RKAEP000'  '0100',
                               ' ' 'BDC_CURSOR'  'P_DISVAR',
                               ' ' 'KOSTL-LOW' p_wa_alvtree_kostl,
                               ' ' 'R_BUDAT-LOW'  p_txdataini,
                               ' ' 'R_BUDAT-HIGH'	p_txdatafim,
                               ' ' 'P_DISVAR'  '/B_CUSTOS3',
                               ' ' 'BDC_OKCODE'  '=ONLI',
                               'X' 'RKAEP000'  '0100',
                               ' ' 'BDC_OKCODE'  '/EENDE'.

  CALL TRANSACTION 'KSB1' USING it_bdcdata
                           MODE 'E'
                       MESSAGES INTO it_message.

ENDFORM.                    " F_TREE_KSB1

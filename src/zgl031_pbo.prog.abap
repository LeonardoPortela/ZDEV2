*&---------------------------------------------------------------------*
*&  Include           ZGL031_PBO
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2JVI |12/05/2025 |Ajuste Campo Competencia para  &*
*&                                    |range para os tipos 17, 29, 21 &*
*&                                    |e '22'                         &*
*&                                    |Chamado: 164255.               &*
*&--------------------------------------------------------------------&*
MODULE status_0100 OUTPUT.
  DATA: BEGIN OF tl_ucomm OCCURS 0,
          ucomm TYPE  sy-ucomm,
        END OF tl_ucomm.

  IF autorizado IS INITIAL.
    APPEND VALUE #( ucomm = c_tp_seguro ) TO tl_ucomm[].
    APPEND VALUE #( ucomm = 'SAVE' ) TO tl_ucomm[].
  ENDIF.

  SET PF-STATUS '0100' EXCLUDING tl_ucomm.
  SET TITLEBAR '0100'.

  CASE screen_item.
    WHEN c_screen_0120.
      SET TITLEBAR '0120'.
    WHEN c_screen_0130.
      SET TITLEBAR '0130'.
  ENDCASE.

  CASE screen_principal.
    WHEN c_screen_0150.
      SET TITLEBAR '0150'.
    WHEN c_screen_0160.
      SET TITLEBAR '0160'.
    WHEN c_screen_0170.
      SET TITLEBAR '0170'.
  ENDCASE.

ENDMODULE.                 " STATUS_0100  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0100 OUTPUT.
  DATA: l_event                 TYPE cntl_simple_event,
        lt_events               TYPE cntl_simple_events,
        obj_event_click         TYPE REF TO lcl_event_handler,
        obj_tree_event_receiver TYPE REF TO lcl_event_handler,
        it_node_key             TYPE lvc_t_nkey.

  CREATE OBJECT obj_tree_event_receiver.

  IF obj_custom_tree IS INITIAL.
    CREATE OBJECT obj_docking
      EXPORTING
        side      = cl_gui_docking_container=>dock_at_left
        extension = 250
        repid     = sy-repid
        dynnr     = '0100'.

    CREATE OBJECT obj_custom_tree
      EXPORTING
        container_name              = 'CUSTOM_TREE'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_tree
      EXPORTING
        parent              = obj_docking
        node_selection_mode = cl_gui_column_tree=>node_sel_mode_single
        item_selection      = ''
        no_html_header      = 'X'
        no_toolbar          = 'X'.

    DATA l_header TYPE treev_hhdr.
    PERFORM build_header CHANGING l_header.

    wl_fcat-fieldname = ' '.
    wl_fcat-no_out    = 'X'.
    wl_fcat-key       = ' '.
    APPEND wl_fcat TO gt_fcat_0100.
    CLEAR wl_fcat.

    CALL METHOD obj_alv_tree->set_table_for_first_display
      EXPORTING
        is_hierarchy_header = l_header
*       I_SAVE              = 'A'
      CHANGING
        it_outtab           = gt_menu_tree
        it_fieldcatalog     = gt_fcat_0100.

    PERFORM create_hierarchy TABLES it_node_key.

    CLEAR l_event.
    l_event-eventid    = cl_gui_column_tree=>eventid_node_double_click.
    l_event-appl_event = 'X'.
    APPEND l_event TO lt_events.

    l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
    APPEND l_event TO lt_events.
    CLEAR l_event.

    CALL METHOD obj_alv_tree->set_registered_events
      EXPORTING
        events                    = lt_events
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3.

    SET HANDLER obj_tree_event_receiver->handle_double_click FOR obj_alv_tree.
    CALL METHOD obj_alv_tree->frontend_update.

    CALL METHOD obj_alv_tree->expand_nodes
      EXPORTING
        it_node_key = it_node_key.

*   Traz o item "Contas a Pagar e Receber" selecionado.

    l_active_node = 2.
    APPEND l_active_node TO lt_nodes_select.

    CALL METHOD obj_alv_tree->set_selected_nodes
      EXPORTING
        it_selected_nodes = lt_nodes_select.

    CREATE OBJECT obj_event_click.
    obj_event_click->handle_double_click( EXPORTING node_key = l_active_node ).

  ENDIF.

ENDMODULE.                 " PBO_0100  OUTPUT

*----------------------------------------------------------------------*
*  MODULE PBO_0110 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0110 OUTPUT.

  IF ( obj_custom_txt IS INITIAL ).

    CREATE OBJECT obj_custom_txt
      EXPORTING
        container_name = 'CUSTOM_TEXTEDIT'.

    CREATE OBJECT obj_custom_editor
      EXPORTING
        parent            = obj_custom_txt
        wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 76
        max_number_chars  = 250.

    CALL METHOD obj_custom_editor->set_toolbar_mode
      EXPORTING
        toolbar_mode = cl_gui_textedit=>false.

    CALL METHOD obj_custom_editor->set_statusbar_mode
      EXPORTING
        statusbar_mode = cl_gui_textedit=>false.

  ENDIF.

  IF ( op_modo = c_search AND autorizado EQ abap_true ) OR  ( edit = abap_true ).

    CALL METHOD obj_custom_editor->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
  ELSE.
    CALL METHOD obj_custom_editor->set_readonly_mode
      EXPORTING
        readonly_mode = custom_mode.
  ENDIF.


  CALL METHOD obj_custom_editor->get_text_as_r3table
    IMPORTING
      table = gt_editor.

  CALL METHOD obj_custom_editor->set_text_as_r3table
    EXPORTING
      table = gt_editor.
ENDMODULE.                 " PBO_0110  OUTPUT

MODULE iniciar_tela_0110 OUTPUT.

  IF ( sy-calld = 'X' ) AND ( wl_cabecalho_0110-seq_lcto IS INITIAL ).
    GET PARAMETER ID 'SLCTO' FIELD  wl_cabecalho_0110-seq_lcto.
  ENDIF.

  custom_mode = COND #( WHEN autorizado IS INITIAL THEN 1 ELSE 0 ).

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  TRATAR_CAMPOS_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tratar_campos_0110 OUTPUT.

  LOOP AT gt_fields INTO wl_fields.

    LOOP AT SCREEN.
*      CHECK ( SCREEN-GROUP2 = WL_FIELDS-GROUP2 ). "//Screen group2

      IF screen-group1  EQ wl_fields-group1 AND
         screen-group1  NE space.

        screen-input        = wl_fields-value.
        screen-invisible    = wl_fields-invisible.
        MODIFY SCREEN.

      ELSEIF screen-group2 EQ wl_fields-group2
         AND screen-group2 NE space.

        screen-input        = wl_fields-value.
        screen-invisible    = wl_fields-invisible.
        MODIFY SCREEN.

      ELSEIF screen-name     EQ wl_fields-campo
         AND wl_fields-campo NE space.

        screen-input        = wl_fields-value.
        screen-invisible    = wl_fields-invisible.
        MODIFY SCREEN.

      ENDIF.

      IF screen-name = 'WL_CABECALHO_0110-REF_SEQ_LCTO'.
        screen-required = 0.
        MODIFY SCREEN.
      ENDIF.

*  ENDIF.

    ENDLOOP.

*    IF ( OP_MODO = C_TIPO_MOEDA )
*    OR ( OP_MODO = C_ENTER      ).
*
*      LOOP AT SCREEN.
*        CHECK ( SCREEN-GROUP2 = WL_FIELDS-GROUP2 ). "//Screen group2
*        SCREEN-INPUT        = WL_FIELDS-VALUE.
*        SCREEN-INVISIBLE    = WL_FIELDS-INVISIBLE.
*        MODIFY SCREEN.
*      ENDLOOP.
*
*    ELSE.
*
*      LOOP AT SCREEN.
*        CHECK ( SCREEN-GROUP1 = WL_FIELDS-GROUP1 ). "//Screen group1
*        SCREEN-INPUT        = WL_FIELDS-VALUE.
*        SCREEN-INVISIBLE    = WL_FIELDS-INVISIBLE.
*        MODIFY SCREEN.
*      ENDLOOP.
*    ENDIF.
  ENDLOOP.

*  IF wl_cabecalho_0110-tp_opr = 'P'.
*    LOOP AT SCREEN.
*      IF screen-name = 'WL_CABECALHO_0110-REF_SEQ_LCTO'.
*        screen-input = 1.
*        MODIFY SCREEN.
*      ENDIF.
*    ENDLOOP.
*  ENDIF.

  IF autorizado IS INITIAL.
    LOOP AT SCREEN.
      IF screen-name EQ 'SAVE'.
        BREAK-POINT.
      ENDIF.
      IF screen-group1 = 'B1' OR screen-group1 = 'GR1'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " TRATAR_CAMPOS_0120  OUTPUT


*----------------------------------------------------------------------*
*  MODULE SET_SELECTED_ROWS OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE set_selected_rows OUTPUT.
*  CHECK ( WL_CELL IS NOT INITIAL ).
  REFRESH gt_selected_rows.

  wl_selected_rows = wl_cell-row_id.
  APPEND wl_selected_rows TO gt_selected_rows.

  IF ( screen_item = c_screen_0120 ).

    CALL METHOD obj_alv_0120->set_current_cell_via_id
      EXPORTING
        is_row_id    = gv_row
        is_column_id = gv_col.

    CALL METHOD obj_alv_0120->set_selected_rows
      EXPORTING
        it_index_rows = gt_selected_rows.
  ELSE.

    CALL METHOD obj_alv_0130->set_current_cell_via_id
      EXPORTING
        is_row_id    = gv_row
        is_column_id = gv_col.

    CALL METHOD obj_alv_0130->set_selected_rows
      EXPORTING
        it_index_rows = gt_selected_rows.
  ENDIF.

  CLEAR: gt_selected_rows, wl_cell, gv_col, gv_row.
ENDMODULE.                 " SET_SELECTED_ROWS  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  PBO_0120  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0120 OUTPUT.
  DATA: gt_sort   TYPE lvc_t_sort,
        vg_branch TYPE char01.

  CLEAR: gt_fcat_0120, wl_layout.


  PERFORM alv_preenche_cat USING:
  1  'STATUS'         TEXT-i06 '6'   ''  ''  ''  'X' ''  '' '' ''               ''                   '' '' '',
  2  'NRO_PARC'       TEXT-i07 '11'  ''  ''  ''  ''  ''  '' '' ''               ''                   '' '' '',
  3  'FILIAL'         TEXT-i08 '6'   ''  ''  ''  ''  'X' '' '' ''               'GT_SAIDA_0120' 'X' '' '',
  4  'TAXA_CAMBIO'    TEXT-i09 '11'  ''  ''  ''  ''  ' ' '' 'ZGLT067'           'WKURS' 'GT_SAIDA_0120' '' '' '',
  5  'VLR_PREMIO_USD' TEXT-i10 '10'  ''  ''  'X' ''  'X' '' 'ZGLT067'           'VLR_PREMIO_BRL' 'GT_SAIDA_0120' '' '' '',
  6  'VLR_PREMIO_BRL' TEXT-i11 '10'  ''  ''  'X' ''  'X' '' 'ZGLT067'           'VLR_PREMIO_BRL' 'GT_SAIDA_0120' '' '' '',
  7  'DT_VENC'        TEXT-i12 '14'  ''  ''  ''  ''  'X' '' 'ZEQUI_EMPRESTIMO'  'ERDAT' 'GT_SAIDA_0120' '' '' '',
  8  'PAIS_PGTO'      TEXT-i13 '9'   ''  ''  ''  ''  'X' '' ''                  ''  'GT_SAIDA_0120' 'X' '' '',
  9  'BCO_EMPRESA'    TEXT-i14 '11'  ''  ''  ''  ''  'X' '' ''                  ''  'GT_SAIDA_0120' 'X' '' '',
  10 'BCO_PARCEIRO'   TEXT-i15 '12'  ''  ''  ''  ''  'X' '' ''                  ''  'GT_SAIDA_0120' 'X' '' '',
  11 'FORMA_PGTO'     TEXT-i16 '10'  ''  ''  ''  ''  'X' '' ''                  ''  'GT_SAIDA_0120' 'X' '' '',
  12 'BLOQ_PGTO'      TEXT-i17 '9'   ''  ''  ''  ''  'X' '' ''                  ''  'GT_SAIDA_0120' 'X' '' '',
  13 'LOTE'           TEXT-i18 '8'   'X' ''  ''  ''  ''  '' ''                  ''  '' '' '' '',
  15 'NRO_DOCUMENTO'  TEXT-i19 '13'  'X' ''  ''  ''  ''  '' ''                  ''  '' '' '' '',
  16 'DOC_CONTABIL'   TEXT-i20 '12'  'X' ''  ''  ''  ''  '' ''                  ''  '' '' '' '',
  17 'COD_BARRAS'     TEXT-i38 '48'  ''  ''  ''  ''  'X' '' ''                  ''  '' '' '' ''.

  wl_layout-grid_title = 'Contas a Pagar e Receber'.

  IF ( obj_custom_0120 IS INITIAL ).
    CREATE OBJECT obj_custom_0120
      EXPORTING
        container_name              = 'CUSTOM_0120'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0120
      EXPORTING
        i_parent          = obj_custom_0120
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  PERFORM register_f4_for_fields.
  PERFORM exclude_tb_functions CHANGING wl_exclude.

  wl_layout-stylefname  = 'ESTILO'.
  "WL_LAYOUT-NO_ROWMARK  = 'X'.
  wl_stable-row         = 'X'.
  wl_stable-col         = 'X'.
  wl_variant_0120-report = sy-repid.

  SET HANDLER:
      lcl_event_toolbar=>set_toolbar
      lcl_event_toolbar=>get_ucomm
      lcl_event_handler=>on_data_changed_finished
      lcl_event_handler=>on_data_changed
      lcl_event_handler=>on_onf4
      lcl_event_handler=>on_double_click
      lcl_event_handler=>on_click
  FOR obj_alv_0120.

  CALL METHOD obj_alv_0120->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      it_toolbar_excluding          = wl_exclude
      is_variant                    = wl_variant_0120
      i_save                        = 'A'
    CHANGING
      it_sort                       = gt_sort
      it_outtab                     = gt_saida_0120
      it_fieldcatalog               = gt_fcat_0120
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0120->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0120->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
ENDMODULE.                 " PBO_0120  OUTPUT


*----------------------------------------------------------------------*
*  MODULE PBO_0130 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0130 OUTPUT.
  CLEAR: gt_fcat_0130, wl_layout.

  DATA obj_toolbar_0130 TYPE REF TO lcl_event_toolbar.

  PERFORM alv_preenche_cat USING:
 1 'NR_ITEM'          'Item'             '6'  '' ''  '' '' ' ' '' '' '' 'GT_SAIDA_0130' 'X' '' '',
 1 'ST_BAIXA'         'Baixa'            '5'  '' ''  '' '' ' ' '' '' '' 'GT_SAIDA_0130' ' ' '' '',
 2 'FILIAL'           'Divisão'          '6'  '' ''  '' '' 'X' '' '' '' 'GT_SAIDA_0130' 'X' '' '',
 3 'CHASSI'           'Chassi'           '25' '' ''  '' '' ' ' '' '' '' 'GT_SAIDA_0130' 'X' '' '',
 4 'NR_SERIE'         'Nr. Série'        '18' '' ''  '' '' ' ' '' '' '' 'GT_SAIDA_0130' 'X' '' '',
 5 'IMOBILIZADO'      'Imobilizado'      '11' '' 'X' '' '' 'X' '' 'ZGLT068' 'ANLN1' 'GT_SAIDA_0130' 'X' '' '',
 6 'SUBNUMERO'        'Sub-número'       '10' '' 'X' '' '' 'X' '' 'ZGLT068' 'ANLN2' '' '' '' '',
 7 'MERCADORIA'       'Mercadoria'       '10' '' 'X' '' '' 'X' '' 'ZGLT068' 'MATNR' 'GT_SAIDA_0130' 'X' '' '',
 8 'DESCR_BENS'       'Desc. Bens'       '30' '' ''  '' '' 'X' '' 'ZGLT068' 'DESCR_BENS' '' '' '' '',
 9 'CENTRO_CUSTO'     'Centro custo'     '12' '' ''  '' '' 'X' '' '' '' 'GT_SAIDA_0130' 'X' '' '',
 10 'AUFNR'           'Ordem'            '12' '' ''  '' '' 'X' '' '' '' 'GT_SAIDA_0130' 'X' '' '',
 11 'VORNR'           'Operação'         '08' '' ''  '' '' 'X' '' '' '' 'GT_SAIDA_0130' 'X' '' '',
 12 'UF'               'UF'              '03' '' ''  '' '' 'X' '' '' '' 'GT_SAIDA_0130' 'X' '' '',
 13 'TAXA_CAMBIO'     'Taxa Cambio'      '11' '' ''  '' '' '' ''  '' '' '' '' '' '',
 14 'VLR_PREMIO_USD'  'Vlr prêmio US$'   '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0130' '' '' '',
 15 'VLR_PREMIO_BRL'  'Vlr prêmio R$'    '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0130' '' '' '',
 16 'VLR_AJ_PREM_USD' 'Vlr ajuste US$'   '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0130' '' '' '',
 17 'VLR_AJ_PREM_BRL' 'Vlr ajuste R$'    '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0130' '' '' '',
 18 'VLR_RISCO_USD'   'Vlr risco US$'    '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0130' '' '' '',
 19 'VLR_RISCO_BRL'   'Vlr risco R$'     '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0130' '' '' '',
 20 'DT_INIC_VIGENC'  'Dt vigência'      '12' '' ''  '' '' ''  '' '' '' '' '' '' '',
 21 'DT_BAIXA'        'Data baixa'       '10' '' ''  '' '' ' ' '' 'ZGLT068' 'DT_BAIXA' 'GT_SAIDA_0130' '' '' '',
 22 'CLAU_BENEF'      'Cl. beneficiaria' '12' '' ''  '' '' 'X' '' '' '' '' '' '1' '',
 23 'BANCO'           'Banco'            '20' '' ''  '' '' 'X' ''  '' '' '' '' '' ''.

  IF ( obj_custom_0130 IS INITIAL ).
    CLEAR: wl_layout.

    CREATE OBJECT obj_custom_0130
      EXPORTING
        container_name              = 'CUSTOM_0130'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0130
      EXPORTING
        i_parent          = obj_custom_0130
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    PERFORM register_f4_for_fields.
    PERFORM set_dropdown_table.
  ENDIF.

  "WL_LAYOUT-NO_ROWMARK = 'X'.
  wl_layout-grid_title = 'Bens Assegurados'.
  wl_layout-stylefname = 'ESTILO'.
  wl_stable-row        = 'X'.
  wl_stable-col        = 'X'.
  wl_variant_0130-report = sy-repid.

  PERFORM exclude_tb_functions CHANGING wl_exclude.

  SET HANDLER:
      lcl_event_toolbar=>set_toolbar
      lcl_event_toolbar=>get_ucomm
      lcl_event_handler=>on_data_changed
      lcl_event_handler=>on_data_changed_finished
      lcl_event_handler=>on_onf4
  FOR obj_alv_0130.

  CALL METHOD obj_alv_0130->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      it_toolbar_excluding          = wl_exclude
      is_variant                    = wl_variant_0130
      i_save                        = 'A'
      "I_DEFAULT                     = 'X'
    CHANGING
      it_outtab                     = gt_saida_0130
      it_fieldcatalog               = gt_fcat_0130
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0130->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0130->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
ENDMODULE.                 " PBO_0130  OUTPUT



*----------------------------------------------------------------------*
*  MODULE PBO_0150 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE pbo_0150 OUTPUT.

  DATA gt_filter   TYPE lvc_t_filt.
  CLEAR wl_layout.
**<<<------"164255 - NMS - INI------>>>
* Valida se campo alterado para ajuste de tela campo competencia.
  PERFORM zf_fill_tipo.
**<<<------"164255 - NMS - FIM------>>>
  REFRESH gt_fcat_0150.
  PERFORM alv_preenche_cat USING:
 1 'MARK'          ''                 ''   ''  '' '' ''   'X' '' ''         ''            '' '' '' 'X',
 2 'STATUS'         'Status'          '6'  'X' '' '' 'X'  ''  '' ''         ''            '' '' '' '',
 3 'SEQ_LCTO'       'Seq. Lcto'       '9'  ''  '' '' ''   ''  '' 'ZGLT050'  'SEQ_LCTO'    'GT_SAIDA_0150' '' '' '',
 4 'NR_ITEM'        'Item'            '6'  ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
 5 'NRO_PARC'       'Nº Parcela'      '9'  ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
 6 'NRO_APOLICE'    'Nro Apolice'     '10' ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
 7 'DOC_CONTABIL'   'Doc Contábil'    '11' 'X' '' '' ''   ''  '' ''         ''            '' '' '' '',
 8 'WERKS'          'Filial'          '6'  ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
 9 'ANLN1'          'Imobilizado'     '11' ''  '' '' ''   ''  '' 'ZGLT068'  'ANLN1'       '' '' '' '',
10 'ANLN2'          'Sub-número'      '10' ''  '' '' ''   ''  '' 'ZGLT068'  'ANLN2'       '' '' '' '',
11 'MATNR'          'Material'        '10' ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
12 'DESCR_BENS'     'Desc Bens'       '30' ''  '' '' ''   ''  '' 'ZGLT068'  'DESCR_BENS'  '' '' '' '',
13 'KOSTL'          'Centro Custo'    '12' ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
14 'WKURS'          'Taxa Câmbio'     '11' ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
15 'VLR_PREMIO_USD' 'Valor US$'       '9'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
16 'VLR_PREMIO_BRL' 'Valor R$'        '8'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
17 'DT_APROPR'      'Dt Apropriação'  '12' ''  '' '' ''   'X' '' ''         ''            ''  '' '' '',
18 'LOTE'           'Lote'            '8'  'X' '' '' ''   ''  '' ''         ''            '' '' '' '',
19 'DOC_LCTO'       'Doc Lançamento'  '14' 'X' '' '' ''   ''  '' ''         ''            '' '' '' ''.

  IF ( obj_custom_0150 IS INITIAL ).
    CREATE OBJECT obj_custom_0150
      EXPORTING
        container_name              = 'CUSTOM_0150'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0150
      EXPORTING
        i_parent          = obj_custom_0150
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  PERFORM exclude_tb_functions CHANGING wl_exclude.

  wl_layout-grid_title = 'Gerar Apropriação'.
  wl_stable-row        = 'X'.
  wl_stable-col        = 'X'.
  wl_variant_0150-report = sy-repid.

  SET HANDLER:
    lcl_event_toolbar=>set_toolbar    FOR obj_alv_0150,
    lcl_event_toolbar=>get_ucomm      FOR obj_alv_0150,
    lcl_event_handler=>on_click       FOR obj_alv_0150.

  CALL METHOD obj_alv_0150->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      it_toolbar_excluding          = wl_exclude
      is_variant                    = wl_variant_0150
      i_save                        = 'A'
      i_default                     = 'X'
    CHANGING
      it_outtab                     = gt_saida_0150
      it_fieldcatalog               = gt_fcat_0150
      it_filter                     = gt_filter[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

*  call method obj_alv_0150->list_processing_events
*    exporting
*      i_event_name = 'TOP_OF_PAGE'
*      i_dyndoc_id  = obj_dyndoc_id.
ENDMODULE.                 " PBO_0150  OUTPUT

MODULE pbo_0170 OUTPUT.
  CLEAR wl_layout.

  REFRESH gt_fcat_0170.
  PERFORM alv_preenche_cat USING:

   01 'MARK'              ''                  ''    ''  '' '' ''   'X' '' ''         ''            '' '' '' 'X',
   02 'STATUS'            'Status'            '06'  ''  '' '' 'X'  ''  '' ''         ''            '' '' '' '',
   03 'SEQ_LCTO'          'Seq. Lcto'         '09'  ''  '' '' ''   ''  '' 'ZGLT050'  'SEQ_LCTO'    'GT_SAIDA_0170' '' '' '',
   04 'NR_ITEM'           'Item'              '06'  ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
   06 'NRO_APOLICE'       'Nro Apolice'       '10'  ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
   07 'DOC_CONTABIL'      'Doc Contábil'      '11'  'X'  '' '' ''   ''  '' ''         ''            '' '' '' '',
   08 'WERKS'             'Filial'            '06'  ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
   09 'ANLN1'             'Imobilizado'       '11'  ''  '' '' ''   ''  '' 'ZGLT068'  'ANLN1'       '' '' '' '',
   10 'ANLN2'             'Sub-número'        '10'  ''  '' '' ''   ''  '' 'ZGLT068'  'ANLN2'       '' '' '' '',
   11 'MATNR'             'Material'          '10'  ''  '' '' ''   ''  '' ''         ''            '' '' '' '',
   12 'DESCR_BENS'        'Desc Bens'         '30'  ''  '' '' ''   ''  '' 'ZGLT068'  'DESCR_BENS'  '' '' '' '',
   13 'KOSTL'             'Centro Custo'      '12'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   14 'WKURS'             'Taxa Câmbio'       '11'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   17 'VLR_PREMIO_USD'    'Valor US$'         '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   18 'VLR_PREMIO_BRL'    'Valor R$'          '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   19 'VLR_BX_USD'        'Vlr Bx. US$'       '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   20 'VLR_BX_BRL'        'Vlr Bx. R$'        '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   21 'APROP_USD'         'Vlr Aprop. US$'    '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   22 'APROP_BRL'         'Vlr Aprop. R$'     '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   23 'SALDO_USD'         'Saldo US$'         '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   24 'SALDO_BRL'         'Saldo R$'          '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   25 'VLR_AJUSTE_USD'    'Vlr.Ajuste US$'    '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   26 'VLR_AJUSTE_BRL'    'Vlr.Ajuste R$'     '13'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   27 'NRO_APROP'         'Nro.Aprop.'        '10'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   28 'PERC_APROP'        'Perc.Aprop.'       '11'  ''  '' '' ''   ''  '' ''         ''            ''  '' '' '',
   29 'DT_LCTO_CTB_AJUS'  'Dt.Ajuste Aprop.'  '12'  ''  '' '' ''   'X' '' ''         ''            ''  '' '' '',
   29 'LOTE_AJUS'         'Lote'              '8'   'X' '' '' ''   ''  '' ''         ''            '' '' ''  '',
   30 'DOC_LCTO_AJUS'     'Doc Lançamento'    '14'  'X' '' '' ''   ''  '' ''         ''            '' '' ''  ''.

  IF ( obj_custom_0170 IS INITIAL ).
    CREATE OBJECT obj_custom_0170
      EXPORTING
        container_name              = 'CUSTOM_0170'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0170
      EXPORTING
        i_parent          = obj_custom_0170
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  PERFORM exclude_tb_functions CHANGING wl_exclude.

  wl_stable-row        = 'X'.
  wl_stable-col        = 'X'.
  wl_variant_0170-report = sy-repid.

  wl_layout-grid_title = 'Gerar Ajuste Apropriação'.

  SET HANDLER:
    lcl_event_toolbar=>set_toolbar FOR obj_alv_0170,
    lcl_event_toolbar=>get_ucomm   FOR obj_alv_0170,
    lcl_event_handler=>on_click    FOR obj_alv_0170.

  CALL METHOD obj_alv_0170->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      it_toolbar_excluding          = wl_exclude
      is_variant                    = wl_variant_0170
      i_save                        = 'A'
      i_default                     = 'X'
    CHANGING
      it_outtab                     = gt_saida_0170
      it_fieldcatalog               = gt_fcat_0170
      it_filter                     = gt_filter[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

*  call method obj_alv_0150->list_processing_events
*    exporting
*      i_event_name = 'TOP_OF_PAGE'
*      i_dyndoc_id  = obj_dyndoc_id.
ENDMODULE.                 " PBO_0150  OUTPUT

*----------------------------------------------------------------------*
*  MODULE STATUS_0200 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR '0200'.

ENDMODULE.                 " STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0200 OUTPUT.
  CLEAR: gt_fcat_0200.

  PERFORM alv_preenche_cat USING:
  1 'STATUS'   ''       '2' 'X' '' '' '' '' '' '' '' '' '' '' '',
  2 'MSG_ERRO' TEXT-i21 '70' '' '' '' '' '' '' '' '' '' '' '' ''.

  IF ( obj_custom_0200 IS INITIAL ).
    CREATE OBJECT obj_custom_0200
      EXPORTING
        container_name              = 'CUSTOM_0200'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0200
      EXPORTING
        i_parent          = obj_custom_0200
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    wl_layout-zebra      = 'X'.
  ENDIF.

  APPEND cl_gui_alv_grid=>mc_fc_excl_all TO wl_exclude.

  SET HANDLER:
    lcl_event_toolbar=>set_toolbar     FOR obj_alv_0200,
    lcl_event_toolbar=>get_ucomm       FOR obj_alv_0200.

  CALL METHOD obj_alv_0200->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      it_toolbar_excluding          = wl_exclude
*     IS_VARIANT                    = GT_VARIANT
      i_save                        = 'A'
    CHANGING
      it_outtab                     = gt_saida_0200
      it_fieldcatalog               = gt_fcat_0200
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
ENDMODULE.                 " PBO_0200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0131  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0131 OUTPUT.
  SET PF-STATUS '0131'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0132  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0132 OUTPUT.
  SET PF-STATUS '0132'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PBO_0132  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo_0132 OUTPUT.

  CLEAR: gt_fcat_0132, wl_layout.

  DATA obj_toolbar_0132 TYPE REF TO lcl_event_toolbar.

  PERFORM alv_preenche_cat USING:
  1 'CHECK'           'Baixa'            '05' '' ''  '' '' 'X' 'X' '' '' 'GT_SAIDA_0132' '' '' '',
  1 'NR_ITEM'          'Item'             '6'  '' ''  '' '' ' ' '' '' '' 'GT_SAIDA_0132' 'X' '' '',
  2 'FILIAL'           'Divisão'          '6'  '' ''  '' '' 'X' '' '' '' 'GT_SAIDA_0132' 'X' '' '',
  3 'CHASSI'           'Chassi'           '25' '' ''  '' '' ' ' '' '' '' 'GT_SAIDA_0132' 'X' '' '',
  4 'NR_SERIE'         'Nr. Série'        '10' '' ''  '' '' ' ' '' '' '' 'GT_SAIDA_0132' 'X' '' '',
  5 'IMOBILIZADO'      'Imobilizado'      '11' '' 'X' '' '' 'X' '' '' '' 'GT_SAIDA_0132' 'X' '' '',
  6 'SUBNUMERO'        'Sub-número'       '10' '' 'X' '' '' 'X' '' '' '' '' '' '' '',
  7 'MERCADORIA'       'Mercadoria'       '10' '' 'X' '' '' 'X' '' '' '' 'GT_SAIDA_0132' 'X' '' '',
  8 'DESCR_BENS'       'Desc. Bens'       '30' '' ''  '' '' 'X' '' '' '' '' '' '' '',
  9 'CENTRO_CUSTO'     'Centro custo'     '12' '' ''  '' '' 'X' '' 'ANLZ' 'KOSTL' 'GT_SAIDA_0132' '' '' '',
  10 'UF'               'UF'               '3'  '' ''  '' '' 'X' '' '' '' 'GT_SAIDA_0132' 'X' '' '',
  11 'TAXA_CAMBIO'     'Taxa Cambio'      '11' '' ''  '' '' '' '' '' '' '' '' '' '',
  12 'VLR_PREMIO_USD'  'Vlr prêmio US$'   '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0132' '' '' '',
  13 'VLR_PREMIO_BRL'  'Vlr prêmio R$'    '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0132' '' '' '',
  14 'VLR_AJ_PREM_USD' 'Vlr ajuste US$'   '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0132' '' '' '',
  15 'VLR_AJ_PREM_BRL' 'Vlr ajuste R$'    '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0132' '' '' '',
  16 'VLR_RISCO_USD'   'Vlr risco US$'    '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0132' '' '' '',
  17 'VLR_RISCO_BRL'   'Vlr risco R$'     '13' '' ''  '' '' 'X' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0132' '' '' '',
  18 'DT_INIC_VIGENC'  'Dt vigência'      '12' '' ''  '' '' ''  '' '' '' '' '' '' '',
  19 'CLAU_BENEF'      'Cl. beneficiaria' '12' '' ''  '' '' 'X' '' '' '' '' '' '1' '',
  21 'BANCO'           'Banco'            '20' '' ''  '' '' 'X' ''  '' '' '' '' '' '',
  22 'DT_BAIXA'        'Data baixa'       '10' '' ''  '' '' ''  '' 'ZGLT068' 'DT_BAIXA' 'GT_SAIDA_0132' '' '' ''.

  IF ( obj_custom_0132 IS INITIAL ).
    CLEAR: wl_layout.

    CREATE OBJECT obj_custom_0132
      EXPORTING
        container_name              = 'CUSTOM_0132'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0132
      EXPORTING
        i_parent          = obj_custom_0132
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    "PERFORM REGISTER_F4_FOR_FIELDS.
    "PERFORM SET_DROPDOWN_TABLE.
  ENDIF.

*  wl_layout-no_rowmark = 'X'.
  wl_layout-stylefname = 'ESTILO'.
  wl_layout-sel_mode   = 'A'.
  wl_stable-row        = 'X'.
  wl_stable-col        = 'X'.
  wl_variant_0132-report = sy-repid.


  PERFORM exclude_tb_functions CHANGING wl_exclude.

  SET HANDLER:
      lcl_event_toolbar=>set_toolbar
      lcl_event_toolbar=>get_ucomm
      lcl_event_handler=>on_data_changed
      lcl_event_handler=>on_data_changed_finished
      lcl_event_handler=>on_onf4
  FOR obj_alv_0132.

  CALL METHOD obj_alv_0132->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      it_toolbar_excluding          = wl_exclude
      is_variant                    = wl_variant_0132
      i_save                        = 'A'
    CHANGING
      it_outtab                     = gt_saida_0132
      it_fieldcatalog               = gt_fcat_0132
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

  CALL METHOD obj_alv_0132->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD obj_alv_0132->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0133  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0133 OUTPUT.
  SET PF-STATUS '0133'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.

MODULE pbo_0160 OUTPUT.

  CLEAR: wl_layout, gt_filter.

  REFRESH gt_fcat_0160.
  PERFORM alv_preenche_cat USING:
  1  'STATUS'         TEXT-i06 '6'   ''  ''  ''  'X' ''  ''  '' ''  ''                   '' '' '',
  1  'SEQ_LCTO'       TEXT-i22 '08'  ''  ''  ''  ''  ''  ''  '' ''  ''                   '' '' '',
  2  'NRO_PARC'       TEXT-i07 '11'  ''  ''  ''  ''  ''  ''  '' ''  ''                   '' '' '',
  3  'FILIAL'         TEXT-i08 '6'   ''  ''  ''  ''  ' ' '' '' ''  'GT_SAIDA_0120' 'X' '' '',
  4  'TAXA_CAMBIO'    TEXT-i09 '11'  ''  ''  ''  ''  ' ' '' 'ZGLT067' 'WKURS' 'GT_SAIDA_0120' '' '' '',
  5  'VLR_PREMIO_USD' TEXT-i10 '10'  ''  ''  'X' ''  ' ' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0120' '' '' '',
  6  'VLR_PREMIO_BRL' TEXT-i11 '10'  ''  ''  'X' ''  ' ' '' 'ZGLT067' 'VLR_PREMIO_BRL' 'GT_SAIDA_0120' '' '' '',
  7  'DT_VENC'        TEXT-i12 '14'  ''  ''  ''  ''  ' ' '' 'ZEQUI_EMPRESTIMO' 'ERDAT' 'GT_SAIDA_0120' '' '' '',
  8  'PAIS_PGTO'      TEXT-i13 '9'   ''  ''  ''  ''  ' ' '' '' ''  'GT_SAIDA_0120' 'X' '' '',
  9  'BCO_EMPRESA'    TEXT-i14 '11'  ''  ''  ''  ''  ' ' '' '' ''  'GT_SAIDA_0120' 'X' '' '',
  10 'BCO_PARCEIRO'   TEXT-i15 '12'  ''  ''  ''  ''  ' ' '' '' ''  'GT_SAIDA_0120' 'X' '' '',
  11 'FORMA_PGTO'     TEXT-i16 '10'  ''  ''  ''  ''  ' ' '' '' ''  'GT_SAIDA_0120' 'X' '' '',
  12 'BLOQ_PGTO'      TEXT-i17 '9'   ''  ''  ''  ''  ' ' '' '' ''  'GT_SAIDA_0120' 'X' '' '',
  13 'LOTE'           TEXT-i18 '8'   'X'  ''  ''  ''  ''  '' '' ''  '' '' '' '',
  15 'NRO_DOCUMENTO'  TEXT-i19 '13'  'X' ''  ''  ''  ''  '' '' ''  '' '' '' '',
  16 'DOC_CONTABIL'   TEXT-i20 '12'  'X' ''  ''  ''  ''  '' '' ''  '' '' '' ''.

  IF ( obj_custom_0160 IS INITIAL ).
    CREATE OBJECT obj_custom_0160
      EXPORTING
        container_name              = 'CUSTOM_0160'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CREATE OBJECT obj_alv_0160
      EXPORTING
        i_parent          = obj_custom_0160
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  CLEAR: wl_exclude.
  PERFORM exclude_tb_functions CHANGING wl_exclude.

  wl_stable-row        = 'X'.
  wl_stable-col        = 'X'.
  wl_variant_0160-report = sy-repid.

  wl_layout-sel_mode   = 'A'.
  wl_layout-grid_title = 'Gerar C.Pagar/Receber'.

  SET HANDLER:
    lcl_event_toolbar=>set_toolbar FOR obj_alv_0160,
    lcl_event_toolbar=>get_ucomm   FOR obj_alv_0160,
    lcl_event_handler=>on_click    FOR obj_alv_0160.

  CALL METHOD obj_alv_0160->set_table_for_first_display
    EXPORTING
      is_layout                     = wl_layout
      it_toolbar_excluding          = wl_exclude
      is_variant                    = wl_variant_0160
      i_save                        = 'A'
      i_default                     = 'X'
    CHANGING
      it_outtab                     = gt_saida_0160
      it_fieldcatalog               = gt_fcat_0160
      it_filter                     = gt_filter[]
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.

ENDMODULE.

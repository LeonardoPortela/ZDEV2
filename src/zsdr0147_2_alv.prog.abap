*&---------------------------------------------------------------------*
*& Include ZFIR0100_ALV
*&---------------------------------------------------------------------*
FORM alv1.

* grid 1
  TRY.
      cl_salv_table=>factory( EXPORTING
                                  r_container    = painel1 "spl_right
                                IMPORTING
                                  r_salv_table   = gr_table1
                                CHANGING
                                  t_table        = t_saida1 ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

*... §3.1 activate ALV generic Functions
  lr_functions1 = gr_table1->get_functions( ).
  lr_functions1->set_all( abap_true ).


  "lr_functions->set_group_aggregation( space ).
  "lr_functions->set_group_layout( 'X' ).

**... §3.2 include own functions
  DATA: l_text_ref  TYPE string,
        l_icon_ref  TYPE string,
        l_text_rep  TYPE string,
        l_icon_rep  TYPE string,
        l_text_del  TYPE string,
        l_icon_del  TYPE string,
        l_text_edit TYPE string,
        l_icon_edit TYPE string,
        l_text_save TYPE string,
        l_icon_save TYPE string,
        l_text_VER  TYPE string,
        l_icon_VER  TYPE string.

  l_text_edit = 'Editar'.
  l_icon_edit = icon_edit_file.
  l_text_save = 'Salvar'.
  l_icon_save = icon_save_as_template.
  l_text_del = 'Deletar'.
  l_icon_del = icon_delete.
  l_text_ref = 'Atualizar'.
  l_icon_ref = icon_refresh.
  l_text_rep = 'Reprocessar'.
  l_icon_rep = icon_operation.
  l_text_ver = 'Verificar'.
  l_icon_ver = icon_set_state.

  TRY.

*      lr_functions->add_function(
*        name     = 'EDIT'
*        icon     = l_icon_edit
*        text     = l_text_edit
*        tooltip  = 'Edit Item'
*        position = if_salv_c_function_position=>right_of_salv_functions ).

*      lr_functions->add_function(
*        name     = 'SAVE'
*        icon     = l_icon_save
*        text     = l_text_save
*        tooltip  = 'Save Item'
*        position = if_salv_c_function_position=>right_of_salv_functions ).

      "Caso precise refazer a operação por completo
*      lr_functions->add_function(
*        name     = 'DELETE'
*        icon     = l_icon_del
*        text     = l_text_del
*        tooltip  = 'Deleta Dados'
*        position = if_salv_c_function_position=>right_of_salv_functions ).

*      lr_functions->add_function(
*      name     = 'REPROCESS'
*      icon     = l_icon_rep
*      text     = l_text_rep
*      tooltip  = 'Reprocessar Dados'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

*      lr_functions->add_function(
*      name     = 'REFRESH'
*      icon     = l_icon_ref
*      text     = l_text_ref
*      tooltip  = 'Atualizar Dados'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

*      lr_functions->add_function(
*      name     = 'VERIFICAR'
*      icon     = l_icon_ver
*      text     = l_text_ver
*      tooltip  = 'Verificar Contas'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

  lr_columns1 = gr_table1->get_columns( ).
  lr_columns_tb1 = gr_table1->get_columns( ).

  TRY.
      gr_table1->get_sorts( )->add_sort(
                             columnname = 'ID_SEQ'
                             subtotal   = 'X'
                           ).

      gr_table1->get_aggregations( )->add_aggregation( columnname = 'QTDPEDID'  ).

    CATCH cx_root.                                      "#EC NO_HANDLER
  ENDTRY.


  PERFORM organizar_colunas.
  lr_columns1->set_optimize( abap_false ).
  PERFORM configurar_colunas. "USING lr_columns1.



  " F4 DDIC

*   DATA lv_ddic TYPE salv_s_ddic_reference.
*
**   TRY.
**       lr_column1 ?= lr_columns1->get_column( columnname = 'NM_INDIC' ).
**       lv_ddic = VALUE #( table = 'ZFIT0201' field = 'NM_INDIC').
**       lr_column1->set_ddic_reference( lv_ddic  ). "EXPORTING value = lv_ddic
**       lr_column1->set_f4(  if_salv_c_bool_sap=>true ).
**     CATCH cx_salv_not_found.
**   ENDTRY.
*
**... §4 set hotspot column
*  TRY.
*      lr_column1_tb ?= lr_columns1_tb->get_column( 'STATUS' ).
*      lr_column1_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.
*
*  TRY.
*      lr_column1_tb ?= lr_columns1_tb->get_column( 'OBJ_KEY' ).
*      lr_column1_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.

  "... §6 RG_ATUALIZADOer to the events of cl_salv_table
  DATA: lr_events1 TYPE REF TO cl_salv_events_table.

  lr_events1 = gr_table1->get_event( ).

  CREATE OBJECT gr_events1.

*... §6.1 RG_ATUALIZADOer to the event USER_COMMAND
  SET HANDLER gr_events1->on_user_command1 FOR lr_events1.

  SET HANDLER gr_events1->on_before_user_command1 FOR lr_events1.

  SET HANDLER gr_events1->on_after_user_command1 FOR lr_events1.

  SET HANDLER gr_events1->on_link_click1 FOR lr_events1.

*... set list title

  l_title1 = |Composição de Lote|.
  lr_display_settings1 = gr_table1->get_display_settings( ).
  lr_display_settings1->set_list_header_size( '0' ). "0=l, 1=s, 2=m
  lr_display_settings1->set_list_header( l_title1 ).

  lr_display_settings1->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display_settings1->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
  lr_display_settings1->set_list_header( l_title1 ).

  "Enable Zebra Layout
  lr_display_settings1->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable cell selection mode
  lr_selections1 = gr_table1->get_selections( ).
  lr_selections1->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  lv_key1-report = sy-repid.
  lr_layout1 = gr_table1->get_layout( ).
  lr_layout1->set_key( lv_key1 ).
  lr_layout1->set_save_restriction( cl_salv_selections=>single ). "cl_salv_selections=>multiple , if_salv_c_layout=>restrict_none
  lr_layout1->set_default( abap_true ).

*... §7 display the table
  gr_table1->display( ).

ENDFORM.

FORM configurar_colunas. "USING lr_columns1 TYPE REF TO cl_salv_columns.

  ".. Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)lr_column1
  TRY.
      " Mostra Coluna

      lr_column1 = lr_columns1->get_column( 'STATUS' ).
      lr_column1->set_short_text( 'Status' ).
      lr_column1->set_medium_text( 'Status' ).
      lr_column1->set_long_text( 'Status' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '05' ).

      lr_column1 = lr_columns1->get_column( 'ID_SEQ' ).
      lr_column1->set_short_text( 'Lote' ).
      lr_column1->set_medium_text( 'Lote' ).
      lr_column1->set_long_text( 'Lote' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '10' ).

      lr_column1 = lr_columns1->get_column( 'BUKRS' ).
      lr_column1->set_short_text( 'Emp.Ped.' ).
      lr_column1->set_medium_text( 'Empresa Ped.' ).
      lr_column1->set_long_text( 'Empresa Ped.' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '12' ).

      lr_column1 = lr_columns1->get_column( 'WERKS' ).
      lr_column1->set_short_text( 'Fil.Ped.' ).
      lr_column1->set_medium_text( 'Filial.Ped.' ).
      lr_column1->set_long_text( 'Filial.Ped.' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '10' ).

      lr_column1 = lr_columns1->get_column( 'EBELN' ).
      lr_column1->set_short_text( 'Nr.Pedido' ).
      lr_column1->set_medium_text( 'Nr.Pedido' ).
      lr_column1->set_long_text( 'Nr.Pedido' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '12' ).

      lr_column1 = lr_columns1->get_column( 'EBELP' ).
      lr_column1->set_short_text( 'Item Ped.' ).
      lr_column1->set_medium_text( 'Item Ped.' ).
      lr_column1->set_long_text( 'Item Ped.' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '09' ).

      lr_column1 = lr_columns1->get_column( 'MATNR' ).
      lr_column1->set_short_text( 'CdMatPed' ).
      lr_column1->set_medium_text( 'Cód.Mat.Ped.' ).
      lr_column1->set_long_text( 'Cód.Mat.Ped.' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '16' ).

      lr_column1 = lr_columns1->get_column( 'MAKTX' ).
      lr_column1->set_short_text( 'DescMatPed' ).
      lr_column1->set_medium_text( 'Desc.Mat.Ped.' ).
      lr_column1->set_long_text( 'Descr.Mat.Ped.' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '30' ).

      lr_column1 = lr_columns1->get_column( 'MATKL' ).
      lr_column1->set_short_text( 'GrpMercPed' ).
      lr_column1->set_medium_text( 'Grp.Merc.Ped.' ).
      lr_column1->set_long_text( 'Grp.Merc.Ped.' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '12' ).

      lr_column1 = lr_columns1->get_column( 'QTDPEDID' ).
      lr_column1->set_short_text( 'Qtd.Ped.' ).
      lr_column1->set_medium_text( 'Qtd.Ped.' ).
      lr_column1->set_long_text( 'Qtd.Ped.' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '15' ).
      "lr_column1->set_decimals( '2' ).


*      lr_column1 = lr_columns1->get_column( 'QTDFATUR' ).
*      lr_column1->set_short_text( 'Qtd.Fat.' ).
*      lr_column1->set_medium_text( 'Qtd.Fat.' ).
*      lr_column1->set_long_text( 'Qtd.Fatura' ).
*      lr_column1->set_optimized( abap_false ).
*      lr_column1->set_output_length( '15' ).
*      "lr_column1->set_decimals( '2' ).
*
*      lr_column1 = lr_columns1->get_column( 'QTDSALDO' ).
*      lr_column1->set_short_text( 'Qtd.Saldo' ).
*      lr_column1->set_medium_text( 'Qtd.Saldo' ).
*      lr_column1->set_long_text( 'Qtd.Saldo' ).
*      lr_column1->set_optimized( abap_false ).
*      lr_column1->set_output_length( '15' ).
*      "lr_column1->set_decimals( '2' ).
*
*      lr_column1 = lr_columns1->get_column( 'QTDSOBRA' ).
*      lr_column1->set_short_text( 'Qtd.Sobra' ).
*      lr_column1->set_medium_text( 'Qtd.Sobra' ).
*      lr_column1->set_long_text( 'Qtd.Sobra' ).
*      lr_column1->set_optimized( abap_false ).
*      lr_column1->set_output_length( '15' ).
*      "lr_column1->set_decimals( '2' ).

      lr_column1 = lr_columns1->get_column( 'OPERACAO' ).
      lr_column1->set_short_text( 'Operação' ).
      lr_column1->set_medium_text( 'Operação' ).
      lr_column1->set_long_text( 'Operação' ).
      lr_column1->set_optimized( abap_false ).
      lr_column1->set_output_length( '10' ).

      " Esconde Coluna

      lr_column1 = lr_columns1->get_column('OPERACAO').
      lr_column1->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO lex_not_found1.
      "write some error handling
  ENDTRY.

ENDFORM.

FORM organizar_colunas.
  lr_columns1->set_column_position(  columnname = 'STATUS'       position = 1 ).
  lr_columns1->set_column_position(  columnname = 'ID_SEQ'       position = 2 ).
  lr_columns1->set_column_position(  columnname = 'BUKRS'        position = 3 ).
  lr_columns1->set_column_position(  columnname = 'WERKS'        position = 4 ).
  lr_columns1->set_column_position(  columnname = 'EBELN'        position = 5 ).
  lr_columns1->set_column_position(  columnname = 'EBELP'        position = 6 ).
  lr_columns1->set_column_position(  columnname = 'MATNR'        position = 7 ).
  lr_columns1->set_column_position(  columnname = 'MAKTX'        position = 8 ).
  lr_columns1->set_column_position(  columnname = 'MATKL'        position = 9 ).
  lr_columns1->set_column_position(  columnname = 'QTDPEDID'   position = 10 ).
ENDFORM.

FORM show_function_info USING i_function TYPE salv_de_function  i_text TYPE string.
  DATA: l_string TYPE string.
  "concatenate i_text i_function into l_string separated by space.
  "message i000(0k) with l_string.

  CASE i_function.
      "     Make ALV as Editable ALV
*    WHEN 'EDIT'.
*      ls_api = gr_table1->extended_grid_api( ).
*      ls_edit = ls_api->editable_restricted( ).
*
*      TRY.
*          ls_edit->set_attributes_for_columnname(
*            EXPORTING
*              columnname              = 'SEQ'
*              all_cells_input_enabled = abap_true
*          ).
*
*          ls_edit->set_attributes_for_columnname(
*      EXPORTING
*        columnname = 'NM_ITEM'
*        all_cells_input_enabled = abap_false
*      ).
*          ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'DENS'
*all_cells_input_enabled = abap_true
*).
*
*          ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'TIPO'
*all_cells_input_enabled = abap_true
*).
*        CATCH cx_root.
*
*      ENDTRY.
*
*
*      ls_edit->validate_changed_data(
*    ).
*      gr_table1->refresh( ).
*
*
*

    WHEN 'REFRESH'.
      PERFORM pega_dados.
      gr_table1->refresh( ).
*      gr_table2->refresh( ).
    WHEN 'VOLTAA'.
*      SUBMIT zfir0100_volta_a
*      AND RETURN.
    WHEN 'REPROCESS'.
*      SUBMIT zfir0100_processa
*      WITH p_emp = p_emp
*      WITH p_ano = p_ano
*      WITH p_mes = p_mes
*      AND RETURN.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'EXIT'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDFORM.

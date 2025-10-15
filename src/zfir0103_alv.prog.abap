*&---------------------------------------------------------------------*
*& Include ZFIR0103_ALV
*&---------------------------------------------------------------------*
FORM alv1.

* grid 1
  TRY.
      cl_salv_table=>factory( EXPORTING
                                  r_container    = painel1 "spl_right
                                IMPORTING
                                  r_salv_table   = gr_table
                                CHANGING
                                  t_table        = it_saida[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.


*...Add DDIC reference for F4 generating
  DATA gr_salv_func TYPE REF TO cl_salv_functions .
  gr_salv_func = gr_table->get_functions( ).
  gr_salv_func->set_all( abap_true ).

*... §3.1 activate ALV generic Functions
  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( abap_true ).

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
*
*      lr_functions->add_function(
*        name     = 'SAVE'
*        icon     = l_icon_save
*        text     = l_text_save
*        tooltip  = 'Save Item'
*        position = if_salv_c_function_position=>right_of_salv_functions ).
*
      "Caso precise refazer a operação por completo
*      lr_functions->add_function(
*        name     = 'DELETE'
*        icon     = l_icon_del
*        text     = l_text_del
*        tooltip  = 'Deleta Dados'
*        position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
      name     = 'REPROCESS'
      icon     = l_icon_rep
      text     = l_text_rep
      tooltip  = 'Reprocessar Dados'
      position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
      name     = 'REFRESH'
      icon     = l_icon_ref
      text     = l_text_ref
      tooltip  = 'Atualizar Dados'
      position = if_salv_c_function_position=>right_of_salv_functions ).

*      lr_functions->add_function(
*      name     = 'VERIFICAR'
*      icon     = l_icon_ver
*      text     = l_text_ver
*      tooltip  = 'Verificar Contas'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

  lr_columns = gr_table->get_columns( ).
  lr_columns_tb = gr_table->get_columns( ).

  PERFORM organizar_colunas.
  lr_columns->set_optimize( abap_false ).
  PERFORM configurar_colunas. "USING lr_columns.

*      " F4 DDIC

*   DATA lv_ddic TYPE salv_s_ddic_reference.
*
**   TRY.
**       lr_column ?= lr_columns->get_column( columnname = 'NM_INDIC' ).
**       lv_ddic = VALUE #( table = 'ZFIT0201' field = 'NM_INDIC').
**       lr_column->set_ddic_reference( lv_ddic  ). "EXPORTING value = lv_ddic
**       lr_column->set_f4(  if_salv_c_bool_sap=>true ).
**     CATCH cx_salv_not_found.
**   ENDTRY.
*
**... §4 set hotspot column
  TRY.
      lr_column_tb ?= lr_columns_tb->get_column( 'STATUS' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  TRY.
      lr_column_tb ?= lr_columns_tb->get_column( 'OBJ_KEY' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  "... §6 RG_ATUALIZADOer to the events of cl_salv_table
  DATA: lr_events TYPE REF TO cl_salv_events_table.

  lr_events = gr_table->get_event( ).

  CREATE OBJECT gr_events.

*... §6.1 RG_ATUALIZADOer to the event USER_COMMAND
  SET HANDLER gr_events->on_user_command FOR lr_events.

  SET HANDLER gr_events->on_before_user_command FOR lr_events.

  SET HANDLER gr_events->on_after_user_command FOR lr_events.

  SET HANDLER gr_events->on_link_click FOR lr_events.

*... set list title

  l_title = |Lançamentos 50 para 0L que nao existem|.
  lr_display_settings = gr_table->get_display_settings( ).
  lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
  lr_display_settings->set_list_header( l_title ).

  lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
  lr_display_settings->set_list_header( l_title ).

  "Enable Zebra Layout
  lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable cell selection mode
  lr_selections = gr_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  lv_key-report = sy-repid.
  lr_layout = gr_table->get_layout( ).
  lr_layout->set_key( lv_key ).
  lr_layout->set_save_restriction( cl_salv_selections=>single ). "cl_salv_selections=>multiple , if_salv_c_layout=>restrict_none
  lr_layout->set_default( abap_true ).

*... §7 display the table
  gr_table->display( ).

ENDFORM.

FORM configurar_colunas. "USING lr_columns TYPE REF TO cl_salv_columns.

*  ".. Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)lr_column
  TRY.

      lr_column = lr_columns->get_column( 'SEQITEM' ).
      lr_column->set_short_text( 'Tot.Itens' ).
      lr_column->set_medium_text( 'Tot.Itens' ).
      lr_column->set_long_text( 'Tot.Itens' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '4' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      lr_column = lr_columns->get_column( 'OBJ_KEY' ).
      lr_column->set_short_text( 'ObjectKey' ).
      lr_column->set_medium_text( 'Object Key' ).
      lr_column->set_long_text( 'Object Key' ).
      lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'BSCHL' ).
      "lr_column->set_short_text( 'ChaveLanc' ).
      "lr_column->set_medium_text( 'Chave lançamento' ).
      "lr_column->set_long_text( 'Chave de lançamento' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      lr_column = lr_columns->get_column( 'GSBER' ).
      lr_column->set_short_text( 'Divisão' ).
      lr_column->set_medium_text( 'Divisão' ).
      lr_column->set_long_text( 'Divisão' ).
      lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      lr_column = lr_columns->get_column( 'BUKRS' ).
      lr_column->set_short_text( 'Empresa' ).
      lr_column->set_medium_text( 'Empresa' ).
      lr_column->set_long_text( 'Empresa' ).
      lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      lr_column = lr_columns->get_column( 'BLDAT' ).
      lr_column->set_short_text( 'Data.Doc.' ).
      lr_column->set_medium_text( 'Data Documento' ).
      lr_column->set_long_text( 'Data do Documento' ).
      lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      lr_column = lr_columns->get_column( 'BUDAT' ).
      lr_column->set_short_text( 'DtLançDoc' ).
      lr_column->set_medium_text( 'Dt. lançamento doc.' ).
      lr_column->set_long_text( 'Data de lançamento no documento' ).
      lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      lr_column = lr_columns->get_column( 'GJAHR' ).
      lr_column->set_short_text( 'Ano' ).
      lr_column->set_medium_text( 'Ano' ).
      lr_column->set_long_text( 'Ano' ).
      lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'MONAT' ).
      "lr_column->set_short_text( 'Per.Cont.' ).
      "lr_column->set_medium_text( 'Período contábil' ).
      "lr_column->set_long_text( 'Período contábil' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'BLART' ).
      "lr_column->set_short_text( 'Tipo.doc.' ).
      "lr_column->set_medium_text( 'Tipo documento' ).
      "lr_column->set_long_text( 'Tipo de documento' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'HKONT' ).
      "lr_column->set_short_text( 'Cont.Razão' ).
      "lr_column->set_medium_text( 'Conta Razão' ).
      "lr_column->set_long_text( 'Conta do Razão' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'WRBTR' ).
      "lr_column->set_short_text( 'MontMoeDoc' ).
      "lr_column->set_medium_text( 'Mont.Moeda.Doc.' ).
      "lr_column->set_long_text( 'Montante moeda do Documento' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'WAERS' ).
      "lr_column->set_short_text( 'Moeda.Doc.' ).
      "lr_column->set_medium_text( 'Moeda Documento' ).
      "lr_column->set_long_text( 'Moeda do Documento' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'SGTXT' ).
      "lr_column->set_short_text( 'Texto' ).
      "lr_column->set_medium_text( 'Texto' ).
      "lr_column->set_long_text( 'Texto' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'KOSTL' ).
      "lr_column->set_short_text( 'Cent.Custo' ).
      "lr_column->set_medium_text( 'Centro Custo' ).
      "lr_column->set_long_text( 'Centro de Custo' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'PRCTR' ).
      "lr_column->set_short_text( 'Cent.Lucro' ).
      "lr_column->set_medium_text( 'Centro de Lucro' ).
      "lr_column->set_long_text( 'Centro de Lucro' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'WAERS_I' ).
      "lr_column->set_short_text( 'Moed.Int.' ).
      "lr_column->set_medium_text( 'Moeda Interna' ).
      "lr_column->set_long_text( '  Moeda Interna' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'DMBTR' ).
      "lr_column->set_short_text( 'MontMoeInt' ).
      "lr_column->set_medium_text( 'Mont. Moeda Interna' ).
      "lr_column->set_long_text( 'Montante Moeda Interna' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'WAERS_F' ).
      "lr_column->set_short_text( 'Moed.Dolar' ).
      "lr_column->set_medium_text( 'Moeda Dolar' ).
      "lr_column->set_long_text( 'Moeda Dolar' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      "lr_column = lr_columns->get_column( 'DMBE2' ).
      "lr_column->set_short_text( 'MontMoeDol' ).
      "lr_column->set_medium_text( 'Montante Moeda Dolar' ).
      "lr_column->set_long_text( 'Montante Moeda Dolar' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      " Esconde Coluna

      "lr_column->set_visible( if_salv_c_bool_sap=>false ).
      "lr_column = lr_columns->get_column( 'RG_ATUALIZADO' ).
      "lr_column->set_short_text( 'Reg.Lido' ).
      "lr_column->set_medium_text( 'RG_ATUALIZADOro Lido' ).
      "lr_column->set_long_text( 'RG_ATUALIZADOro Lido' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).

      " Esconde Coluna

      "lr_column->set_visible( if_salv_c_bool_sap=>false ).

      "lr_column = lr_columns->get_column( 'RLDNR' ).
      "lr_column->set_short_text( 'Ledger' ).
      "lr_column->set_medium_text( 'Ledger' ).
      "lr_column->set_long_text( 'Ledger' ).
      "lr_column->set_optimized( abap_true ).
      "lr_column->set_output_length( '20' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_xls }| ).


      lr_column = lr_columns->get_column( 'STATUS' ).
      lr_column->set_short_text( 'Status' ).
      lr_column->set_medium_text( 'Status' ).
      lr_column->set_long_text( 'Status' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '6' ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_tooltip( value = |{ icon_status }| ).

      lr_column = lr_columns->get_column( 'XBLNR' ).
      lr_column->set_short_text( 'Doc.Cont' ).
      lr_column->set_medium_text( 'Documento Contabil' ).
      lr_column->set_long_text( 'Documento Contabil' ).
      lr_column->set_optimized( abap_false ).
      "lr_column->set_output_length( '6' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_status }| ).

      lr_column = lr_columns->get_column( 'BELNR' ).
      lr_column->set_short_text( 'Doc.Orig.' ).
      lr_column->set_medium_text( 'Doc.Origem.' ).
      lr_column->set_long_text( 'Documento Origem.' ).
      lr_column->set_optimized( abap_false ).
      "lr_column->set_output_length( '6' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column->set_tooltip( value = |{ icon_status }| ).

      " Esconde Coluna
      lr_column = lr_columns->get_column( 'MANDT' ).
      lr_column = lr_columns->get_column( 'RLDNR' ).
      lr_column = lr_columns->get_column( 'RG_ATUALIZADO' ).
      lr_column = lr_columns->get_column( 'BSCHL' ).
      lr_column = lr_columns->get_column( 'MONAT'   ).
      lr_column = lr_columns->get_column( 'BLART'   ).
      lr_column = lr_columns->get_column( 'HKONT'   ).
      lr_column = lr_columns->get_column( 'WRBTR'   ).
      lr_column = lr_columns->get_column( 'WAERS'   ).
      lr_column = lr_columns->get_column( 'SGTXT'   ).
      lr_column = lr_columns->get_column( 'KOSTL'   ).
      lr_column = lr_columns->get_column( 'PRCTR'   ).
      lr_column = lr_columns->get_column( 'WAERS_I' ).
      lr_column = lr_columns->get_column( 'DMBTR'   ).
      lr_column = lr_columns->get_column( 'WAERS_F' ).
      lr_column = lr_columns->get_column( 'DMBE2'   ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO lex_not_found.
      "write some error handling
  ENDTRY.

ENDFORM.

FORM organizar_colunas.
  lr_columns->set_column_position(  columnname = 'SEQITEM'      position = 1 ).
  lr_columns->set_column_position(  columnname = 'STATUS'   position = 2 ).
  lr_columns->set_column_position(  columnname = 'XBLNR'    position = 3 ).
  lr_columns->set_column_position(  columnname = 'BELNR'    position = 4 ).
  lr_columns->set_column_position(  columnname = 'OBJ_KEY'  position = 5 ).
  "lr_columns->set_column_position(  columnname = 'BSCHL'    position = 6 ).
  lr_columns->set_column_position(  columnname = 'GSBER'    position = 7 ).
  lr_columns->set_column_position(  columnname = 'BUKRS'    position = 8 ).
  lr_columns->set_column_position(  columnname = 'BLDAT'    position = 9 ).
  lr_columns->set_column_position(  columnname = 'BUDAT'    position = 10 ).
  lr_columns->set_column_position(  columnname = 'GJAHR'    position = 11 ).
  "lr_columns->set_column_position(  columnname = 'MONAT'    position = 12 ).
  "lr_columns->set_column_position(  columnname = 'BLART'    position = 13 ).
  "lr_columns->set_column_position(  columnname = 'HKONT'    position = 14 ).
  "lr_columns->set_column_position(  columnname = 'WRBTR'    position = 15 ).
  "lr_columns->set_column_position(  columnname = 'WAERS'    position = 16 ).
  "lr_columns->set_column_position(  columnname = 'SGTXT'    position = 17 ).
  "lr_columns->set_column_position(  columnname = 'KOSTL'    position = 18 ).
  "lr_columns->set_column_position(  columnname = 'PRCTR'    position = 19 ).
  "lr_columns->set_column_position(  columnname = 'WAERS_I'  position = 20 ).
  "lr_columns->set_column_position(  columnname = 'DMBTR'    position = 21 ).
  "lr_columns->set_column_position(  columnname = 'WAERS_F'  position = 22 ).
  "lr_columns->set_column_position(  columnname = 'DMBE2'    position = 23 ).
ENDFORM.

FORM show_function_info USING i_function TYPE salv_de_function  i_text TYPE string.
  DATA: l_string TYPE string.
  "concatenate i_text i_function into l_string separated by space.
  "message i000(0k) with l_string.

  CASE i_function.
      "     Make ALV as Editable ALV
*    WHEN 'EDIT'.
*      ls_api = gr_table->extended_grid_api( ).
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
*      gr_table->refresh( ).
*
*
*

    WHEN 'REFRESH'.
      PERFORM pega_dados.
      gr_table->refresh( ).
      gr_table2->refresh( ).
    WHEN 'VOLTAA'.
      SUBMIT ZFIR0100_volta_a
      AND RETURN.
    WHEN 'REPROCESS'.
      SUBMIT ZFIR0103_processa
      WITH p_emp = p_emp
      WITH p_ano = p_ano
      WITH p_mes = p_mes
      AND RETURN.
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

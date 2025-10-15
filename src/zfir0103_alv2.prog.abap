*&---------------------------------------------------------------------*
*& Include ZFIR0103_ALV
*&---------------------------------------------------------------------*
FORM alv2.

* grid 2
  TRY.
      cl_salv_table=>factory( EXPORTING
                                  r_container    = painel2"spl_left
                                IMPORTING
                                  r_salv_table   = gr_table2
                                CHANGING
                                  t_table        = it_saida2[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

*...Add DDIC reference for F4 generating
  DATA gr_salv_func TYPE REF TO cl_salv_functions .
  gr_salv_func = gr_table2->get_functions( ).
  gr_salv_func->set_all( abap_true ).

*... §3.1 activate ALV generic Functions
  lr_functions2 = gr_table2->get_functions( ).
  lr_functions2->set_all( abap_true ).

  "lr_functions2->set_group_aggregation( space ).
  "lr_functions2->set_group_layout( 'X' ).

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

*      lr_functions2->add_function(
*        name     = 'EDIT'
*        icon     = l_icon_edit
*        text     = l_text_edit
*        tooltip  = 'Edit Item'
*        position = if_salv_c_function_position=>right_of_salv_functions ).
*
*      lr_functions2->add_function(
*        name     = 'SAVE'
*        icon     = l_icon_save
*        text     = l_text_save
*        tooltip  = 'Save Item'
*        position = if_salv_c_function_position=>right_of_salv_functions ).
*
      "Caso precise refazer a operação por completo
*      lr_functions2->add_function(
*        name     = 'DELETE'
*        icon     = l_icon_del
*        text     = l_text_del
*        tooltip  = 'Deleta Dados'
*        position = if_salv_c_function_position=>right_of_salv_functions ).

*    lr_functions2->add_function(
*    name     = 'REPROCESS'
*    ICON     = l_icon_rep
*    TEXT     = l_text_rep
*    tooltip  = 'Reprocessar Dados'
*    POSITION = if_salv_c_function_position=>right_of_salv_functions ).

*      lr_functions2->add_function(
*      name     = 'REFRESH'
*      icon     = l_icon_ref
*      text     = l_text_ref
*      tooltip  = 'Atualizar Dados'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

*      lr_functions2->add_function(
*      name     = 'VERIFICAR'
*      icon     = l_icon_ver
*      text     = l_text_ver
*      tooltip  = 'Verificar Contas'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

  lr_columns2 = gr_table2->get_columns( ).
  lr_columns_tb2 = gr_table2->get_columns( ).

  PERFORM organizar_colunas2.
  lr_columns2->set_optimize( abap_false ).
  PERFORM configurar_colunas2. "USING lr_columns2.

*      " F4 DDIC

*   DATA lv_ddic TYPE salv_s_ddic_reference.
*
**   TRY.
**       lr_column2 ?= lr_columns2->get_column( columnname = 'NM_INDIC' ).
**       lv_ddic = VALUE #( table = 'ZFIT0201' field = 'NM_INDIC').
**       lr_column2->set_ddic_reference( lv_ddic  ). "EXPORTING value = lv_ddic
**       lr_column2->set_f4(  if_salv_c_bool_sap=>true ).
**     CATCH cx_salv_not_found.
**   ENDTRY.

**... §4 set hotspot column
*  TRY.
*    lr_column_tb2 ?= lr_columns_tb2->get_column( 'STATUS' ).
*    lr_column_tb2 ?= lr_columns_tb2->get_column( 'OBJ_KEY' ).
*    lr_column_tb2->set_cell_type( if_salv_c_cell_type=>hotspot ).
*  CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.


  "... §6 RG_ATUALIZADOer to the events of cl_salv_table
  DATA: lr_events2 TYPE REF TO cl_salv_events_table.

  lr_events2 = gr_table2->get_event( ).

  CREATE OBJECT gr_events2.

*... §6.1 RG_ATUALIZADOer to the event USER_COMMAND
  SET HANDLER gr_events2->on_user_command FOR lr_events2.

  SET HANDLER gr_events2->on_before_user_command FOR lr_events2.

  SET HANDLER gr_events2->on_after_user_command FOR lr_events2.

  SET HANDLER gr_events2->on_link_click FOR lr_events2.

*... set list title

  l_title2 = |Relatório dos Itens|.
  lr_display_settings2 = gr_table2->get_display_settings( ).
  lr_display_settings2->set_list_header_size( '10' ). "0=l, 1=s, 2=m
  lr_display_settings2->set_list_header( l_title2 ).

  lr_display_settings2->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display_settings2->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
  lr_display_settings2->set_list_header( l_title2 ).

  "Enable Zebra Layout
  lr_display_settings2->set_striped_pattern( cl_salv_display_settings=>true ).

* Enable cell selection mode
  lr_selections2 = gr_table2->get_selections( ).
  lr_selections2->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  lv_key2-report = sy-repid.
  lr_layout2 = gr_table2->get_layout( ).
  lr_layout2->set_key( lv_key2 ).
  lr_layout2->set_save_restriction( cl_salv_selections=>single ). "cl_salv_selections=>multiple , if_salv_c_layout=>restrict_none
  lr_layout2->set_default( abap_true ).

*... §7 display the table
  gr_table2->display( ).

ENDFORM.

FORM configurar_colunas2. "USING lr_columns2 TYPE REF TO cl_salv_columns.

*  ".. Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)lr_column2
  TRY.

      lr_column2 = lr_columns2->get_column( 'SEQITEM' ).
      lr_column2->set_short_text( 'Seq.Itens' ).
      lr_column2->set_medium_text( 'Seq.Itens' ).
      lr_column2->set_long_text( 'Seq.Itens' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '4' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'OBJ_KEY' ).
      lr_column2->set_short_text( 'ObjectKey' ).
      lr_column2->set_medium_text( 'Object Key' ).
      lr_column2->set_long_text( 'Object Key' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'BSCHL' ).
      lr_column2->set_short_text( 'ChaveLanc' ).
      lr_column2->set_medium_text( 'Chave lançamento' ).
      lr_column2->set_long_text( 'Chave de lançamento' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'GSBER' ).
      lr_column2->set_short_text( 'Divisão' ).
      lr_column2->set_medium_text( 'Divisão' ).
      lr_column2->set_long_text( 'Divisão' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'BUKRS' ).
      lr_column2->set_short_text( 'Empresa' ).
      lr_column2->set_medium_text( 'Empresa' ).
      lr_column2->set_long_text( 'Empresa' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'BLDAT' ).
      lr_column2->set_short_text( 'Data.Doc.' ).
      lr_column2->set_medium_text( 'Data Documento' ).
      lr_column2->set_long_text( 'Data do Documento' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'BUDAT' ).
      lr_column2->set_short_text( 'DtLançDoc' ).
      lr_column2->set_medium_text( 'Dt. lançamento doc.' ).
      lr_column2->set_long_text( 'Data de lançamento no documento' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'GJAHR' ).
      lr_column2->set_short_text( 'Ano' ).
      lr_column2->set_medium_text( 'Ano' ).
      lr_column2->set_long_text( 'Ano' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'MONAT' ).
      lr_column2->set_short_text( 'Per.Cont.' ).
      lr_column2->set_medium_text( 'Período contábil' ).
      lr_column2->set_long_text( 'Período contábil' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'BLART' ).
      lr_column2->set_short_text( 'Tipo.doc.' ).
      lr_column2->set_medium_text( 'Tipo documento' ).
      lr_column2->set_long_text( 'Tipo de documento' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'HKONT' ).
      lr_column2->set_short_text( 'Cont.Razão' ).
      lr_column2->set_medium_text( 'Conta Razão' ).
      lr_column2->set_long_text( 'Conta do Razão' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'WRBTR' ).
      lr_column2->set_short_text( 'MontMoeDoc' ).
      lr_column2->set_medium_text( 'Mont.Moeda.Doc.' ).
      lr_column2->set_long_text( 'Montante moeda do Documento' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'WAERS' ).
      lr_column2->set_short_text( 'Moeda.Doc.' ).
      lr_column2->set_medium_text( 'Moeda Documento' ).
      lr_column2->set_long_text( 'Moeda do Documento' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'SGTXT' ).
      lr_column2->set_short_text( 'Texto' ).
      lr_column2->set_medium_text( 'Texto' ).
      lr_column2->set_long_text( 'Texto' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'KOSTL' ).
      lr_column2->set_short_text( 'Cent.Custo' ).
      lr_column2->set_medium_text( 'Centro Custo' ).
      lr_column2->set_long_text( 'Centro de Custo' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'PRCTR' ).
      lr_column2->set_short_text( 'Cent.Lucro' ).
      lr_column2->set_medium_text( 'Centro de Lucro' ).
      lr_column2->set_long_text( 'Centro de Lucro' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'WAERS_I' ).
      lr_column2->set_short_text( 'Moed.Int.' ).
      lr_column2->set_medium_text( 'Moeda Interna' ).
      lr_column2->set_long_text( '  Moeda Interna' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'DMBTR' ).
      lr_column2->set_short_text( 'MontMoeInt' ).
      lr_column2->set_medium_text( 'Mont. Moeda Interna' ).
      lr_column2->set_long_text( 'Montante Moeda Interna' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'WAERS_F' ).
      lr_column2->set_short_text( 'Moed.Dolar' ).
      lr_column2->set_medium_text( 'Moeda Dolar' ).
      lr_column2->set_long_text( 'Moeda Dolar' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).

      lr_column2 = lr_columns2->get_column( 'DMBE2' ).
      lr_column2->set_short_text( 'MontMoeDol' ).
      lr_column2->set_medium_text( 'MontMoeDol').
      lr_column2->set_long_text( 'MontMoeDol' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).


      lr_column2 = lr_columns2->get_column( 'RG_ATUALIZADO' ).
      lr_column2->set_short_text( 'Reg.Atual.' ).
      lr_column2->set_medium_text( 'Reg.Atual.' ).
      lr_column2->set_long_text( 'Reg.Atual.' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).


      lr_column2 = lr_columns2->get_column( 'RLDNR' ).
      lr_column2->set_short_text( 'Ledger' ).
      lr_column2->set_medium_text( 'Ledger' ).
      lr_column2->set_long_text( 'Ledger' ).
      lr_column2->set_optimized( abap_true ).
      "lr_column2->set_output_length( '20' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_xls }| ).


      lr_column2 = lr_columns2->get_column( 'STATUS' ).
      lr_column2->set_short_text( 'Status' ).
      lr_column2->set_medium_text( 'Status' ).
      lr_column2->set_long_text( 'Status' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '6' ).
      lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      lr_column2->set_tooltip( value = |{ icon_status2 }| ).

      lr_column2 = lr_columns2->get_column( 'XBLNR' ).
      lr_column2->set_short_text( 'Doc.Cont' ).
      lr_column2->set_medium_text( 'Documento Contabil' ).
      lr_column2->set_long_text( 'Documento Contabil' ).
      lr_column2->set_optimized( abap_false ).
      "lr_column2->set_output_length( '6' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_status2 }| ).

      lr_column2 = lr_columns2->get_column( 'BELNR' ).
      lr_column2->set_short_text( 'Doc.Orig.' ).
      lr_column2->set_medium_text( 'Doc.Origem.' ).
      lr_column2->set_long_text( 'Documento Origem' ).
      lr_column2->set_optimized( abap_false ).
      "lr_column2->set_output_length( '6' ).
      "lr_column2->set_alignment( if_salv_c_alignment=>centered ).
      "lr_column2->set_tooltip( value = |{ icon_status2 }| ).

      " Esconde Coluna
      lr_column2 = lr_columns2->get_column( 'MANDT' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO lex_not_found2.
      "write some error handling
  ENDTRY.

ENDFORM.

FORM organizar_colunas2.
  lr_columns2->set_column_position(  columnname = 'SEQITEM'  position = 1 ).
  lr_columns2->set_column_position(  columnname = 'STATUS'   position = 2 ).
  lr_columns2->set_column_position(  columnname = 'XBLNR'    position = 3 ).
  lr_columns2->set_column_position(  columnname = 'BELNR'    position = 4 ).
  lr_columns2->set_column_position(  columnname = 'OBJ_KEY'  position = 5 ).
  lr_columns2->set_column_position(  columnname = 'BSCHL'    position = 6 ).
  lr_columns2->set_column_position(  columnname = 'GSBER'    position = 7 ).
  lr_columns2->set_column_position(  columnname = 'BUKRS'    position = 8 ).
  lr_columns2->set_column_position(  columnname = 'BLDAT'    position = 9 ).
  lr_columns2->set_column_position(  columnname = 'BUDAT'    position = 10 ).
  lr_columns2->set_column_position(  columnname = 'GJAHR'    position = 11 ).
  lr_columns2->set_column_position(  columnname = 'MONAT'    position = 12 ).
  lr_columns2->set_column_position(  columnname = 'BLART'    position = 13 ).
  lr_columns2->set_column_position(  columnname = 'HKONT'    position = 14 ).
  lr_columns2->set_column_position(  columnname = 'WRBTR'    position = 15 ).
  lr_columns2->set_column_position(  columnname = 'WAERS'    position = 16 ).
  lr_columns2->set_column_position(  columnname = 'SGTXT'    position = 17 ).
  lr_columns2->set_column_position(  columnname = 'KOSTL'    position = 18 ).
  lr_columns2->set_column_position(  columnname = 'PRCTR'    position = 19 ).
  lr_columns2->set_column_position(  columnname = 'WAERS_I'  position = 20 ).
  lr_columns2->set_column_position(  columnname = 'DMBTR'    position = 21 ).
  lr_columns2->set_column_position(  columnname = 'WAERS_F'  position = 22 ).
  lr_columns2->set_column_position(  columnname = 'DMBE2'    position = 23 ).
ENDFORM.

FORM show_function_info2 USING i_function2 TYPE salv_de_function  i_text2 TYPE string.
  DATA: l_string2 TYPE string.
  "concatenate i_text2 i_function2 into l_string2 separated by space.
  "message i000(0k) with l_string2.

  CASE i_function2.
      "     Make ALV as Editable ALV
*    WHEN 'EDIT'.
*      ls_api2 = gr_table2->extended_grid_api( ).
*      ls_edit2 = ls_api2->editable_restricted( ).
*
*      TRY.
*          ls_edit2->set_attributes_for_columnname(
*            EXPORTING
*              columnname              = 'SEQ'
*              all_cells_input_enabled = abap_true
*          ).
*
*          ls_edit2->set_attributes_for_columnname(
*      EXPORTING
*        columnname = 'NM_ITEM'
*        all_cells_input_enabled = abap_false
*      ).
*          ls_edit2->set_attributes_for_columnname(
*EXPORTING
*columnname = 'DENS'
*all_cells_input_enabled = abap_true
*).
*
*          ls_edit2->set_attributes_for_columnname(
*EXPORTING
*columnname = 'TIPO'
*all_cells_input_enabled = abap_true
*).
*        CATCH cx_root.
*
*      ENDTRY.
*
*
*      ls_edit2->validate_changed_data(
*    ).
*      gr_table2->refresh( ).
*
*
*

*    WHEN 'REFRESH'.
*      gr_table->refresh( ).
*      gr_table2->refresh( ).
*    WHEN 'VOLTAA'.
*      SUBMIT ZFIR0100_volta_a
*      AND RETURN.
    WHEN 'ONLI'.
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

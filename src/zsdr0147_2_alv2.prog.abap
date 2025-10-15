*&---------------------------------------------------------------------*
*& Include ZFIR0100_ALV
*&---------------------------------------------------------------------*
FORM alv2.

* grid 2
  TRY.
      cl_salv_table=>factory( EXPORTING r_container  = painel2 "spl_left
                              IMPORTING r_salv_table = gr_table2
                              CHANGING  t_table      = t_saida2 ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

*...Add DDIC reference for F4 generating
*  DATA gr_salv_func2 TYPE REF TO cl_salv_functions_list .
*  gr_salv_func2 = gr_table2->get_functions( ).
*  gr_salv_func2->set_all( abap_true ).

*... §3.1 activate ALV generic Functions
  lr_functions2 = gr_table2->get_functions( ).
  lr_functions2->set_all( abap_true ). "Abap_true libera todos os campos  Toolbar

**... §3.2 include own functions
  DATA:
*        l_text_ref     TYPE string,
*        l_icon_ref     TYPE string,
*        l_text_rep     TYPE string,
*        l_icon_rep     TYPE string,
*        l_text_del     TYPE string,
*        l_icon_del     TYPE string,
    l_text_edit    TYPE string,
    l_icon_edit    TYPE string,
*        l_text_save    TYPE string,
*        l_icon_save    TYPE string,
*        l_text_VER     TYPE string,
*        l_icon_VER     TYPE string,
    l_text_fat_del TYPE string,
    l_icon_fat_del TYPE string,
    l_text_fat_ins TYPE string,
    l_icon_fat_ins TYPE string,
    l_text_fat_est TYPE string,
    l_icon_fat_est TYPE string,
    l_text_fat_ger TYPE string,
    l_icon_fat_ger TYPE string.

  l_text_edit = 'Editar'.
  l_icon_edit = icon_edit_file.
*  l_text_save = 'Salvar'.
*  l_icon_save = icon_save_as_template.
*  l_text_del = 'Deletar'.
*  l_icon_del = icon_delete.
*  l_text_ref = 'Atualizar'.
*  l_icon_ref = icon_refresh.
*  l_text_rep = 'Reprocessar'.
*  l_icon_rep = icon_operation.
*  l_text_ver = 'Verificar'.
*  l_icon_ver = icon_set_state.
  l_text_fat_del = ''.
  l_icon_fat_del = icon_delete_row.
  l_text_fat_ins = ''.
  l_icon_fat_ins = icon_insert_row.
  l_text_fat_est = 'Estornar'.
  l_icon_fat_est = icon_storno.
  l_text_fat_ger = 'Gerar'.
  l_icon_fat_ger = icon_transport.



*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'DELETE_ROW'.   "fcode
*    mt_toolbar-icon = '@B_DELR@'.
*    mt_toolbar-quickinfo = 'Eliminar linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.
  TRY.

*      lr_functions2->add_function(
*        name     = 'EDIT'
*        icon     = l_icon_edit
*        text     = l_text_edit
*        tooltip  = 'Edit Item'
*        position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions2->add_function(
        name     = 'INSERT_ROW'
        icon     = l_icon_fat_ins
        text     = l_text_fat_ins
        tooltip  = 'Inserir Fatura'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions2->add_function(
        name     = 'DELETE_ROW'
        icon     = l_icon_fat_del
        text     = l_text_fat_del
        tooltip  = 'Deletar Linha'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions2->add_function(
        name     = 'FATURAR'
        icon     = l_icon_fat_ger
        text     = l_text_fat_ger
        tooltip  = 'Gerar'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions2->add_function(
        name     = 'ESTORNAR'
        icon     = l_icon_fat_est
        text     = l_text_fat_est
        tooltip  = 'Estornar'
        position = if_salv_c_function_position=>right_of_salv_functions ).

*      lr_functions2->add_function(
*        name     = 'SAVE'
*        icon     = l_icon_save
*        text     = l_text_save
*        tooltip  = 'Save Item'
*        position = if_salv_c_function_position=>right_of_salv_functions ).
*
*      lr_functions2->add_function(
*        name     = 'DELETE'
*        icon     = l_icon_del
*        text     = l_text_del
*        tooltip  = 'Deleta Dados'
*        position = if_salv_c_function_position=>right_of_salv_functions ).
*
*      lr_functions2->add_function(
*      name     = 'REPROCESS'
*      icon     = l_icon_rep
*      text     = l_text_rep
*      tooltip  = 'Reprocessar Dados'
*      position = if_salv_c_function_position=>right_of_salv_functions ).
*
*      lr_functions2->add_function(
*      name     = 'REFRESH'
*      icon     = l_icon_ref
*      text     = l_text_ref
*      tooltip  = 'Atualizar Dados'
*      position = if_salv_c_function_position=>right_of_salv_functions ).
*
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

  PERFORM organizar_colunas2. "USING lr_columns2.
  lr_columns2->set_optimize( abap_false ). "Todos!
  PERFORM configurar_colunas2. "USING lr_columns2.


*      " F4 DDIC

  DATA lv_ddic TYPE salv_s_ddic_reference.


  TRY.
      lr_column_tb2 ?= lr_columns_tb2->get_column( columnname = 'AUART' ).
      lv_ddic = VALUE #( table  = 'ZSDT0306_FAT'  field = 'AUART').
      lr_column_tb2->set_ddic_reference( EXPORTING value = lv_ddic ).
      lr_column_tb2->set_f4( if_salv_c_bool_sap=>true ).

      lr_column_tb2 ?= lr_columns_tb2->get_column( columnname = 'WAERK' ).
      lv_ddic = VALUE #( table  = 'ZSDT0306_FAT'  field = 'WAERK').
      lr_column_tb2->set_ddic_reference( EXPORTING value = lv_ddic ).
      lr_column_tb2->set_f4( if_salv_c_bool_sap=>true ).

      lr_column_tb2 ?= lr_columns_tb2->get_column( columnname = 'OPERACAO' ).
      lv_ddic = VALUE #( table  = 'ZSDT0306_FAT'  field = 'OPERACAO').
      lr_column_tb2->set_ddic_reference( EXPORTING value = lv_ddic ).
      lr_column_tb2->set_f4( if_salv_c_bool_sap=>true ).

    CATCH cx_root.                                      "#EC NO_HANDLER
  ENDTRY.


**... §4 set hotspot column
  TRY.
      lr_column_tb2 ?= lr_columns_tb2->get_column( columnname = 'DOCNUM_NF' ).
      lr_column_tb2->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

  "... §6 RG_ATUALIZADOer to the events of cl_salv_table
  DATA: lr_events2 TYPE REF TO cl_salv_events_table.

  lr_events2 = gr_table2->get_event( ).

  CREATE OBJECT gr_events2.

*... §6.1 RG_ATUALIZADOer to the event USER_COMMAND
  SET HANDLER gr_events2->on_user_command2 FOR lr_events2.

  SET HANDLER gr_events2->on_before_user_command2 FOR lr_events2.

  SET HANDLER gr_events2->on_after_user_command2 FOR lr_events2.

  SET HANDLER gr_events2->on_link_click2 FOR lr_events2.

  SET HANDLER gr_events2->toolbar2 FOR ALL INSTANCES ACTIVATION 'X'.

  SET HANDLER gr_events2->on_handle_data_changed2 FOR ALL INSTANCES.

  SET HANDLER gr_events2->on_data_changed_finished2 FOR ALL INSTANCES.


*... set list title

  l_title2 = |Fatura NF Serviço|.
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
  lr_layout2->set_default( if_salv_c_bool_sap=>true ).

*... §7 display the table
  gr_table2->display( ).

  "PERFORM make_alv2.

ENDFORM.

FORM configurar_colunas2."USING lr_columns2 TYPE REF TO cl_salv_columns.

  ".. Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)lr_column2
  TRY.

      lr_column2 = lr_columns2->get_column( 'ID_SEQ' ).
      lr_column2->set_short_text( 'Lote' ).
      lr_column2->set_medium_text( 'Lote' ).
      lr_column2->set_long_text( 'Lote' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '6' ).

      lr_column2 = lr_columns2->get_column( 'CL_CODIGO' ).
      lr_column2->set_short_text( 'Cliente' ).
      lr_column2->set_medium_text( 'Cliente' ).
      lr_column2->set_long_text( 'Cliente' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).
      lr_column2->set_edit_mask( value = '__________' ).

      lr_column2 = lr_columns2->get_column( 'BUKRS_FAT' ).
      lr_column2->set_short_text( 'Emp.Srv.' ).
      lr_column2->set_medium_text( 'Empresa Serv' ).
      lr_column2->set_long_text( 'Empresa Serv' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '8' ).

      lr_column2 = lr_columns2->get_column( 'WERKS_FAT' ).
      lr_column2->set_short_text( 'Fil.Srv.' ).
      lr_column2->set_medium_text( 'Filial Srv.' ).
      lr_column2->set_long_text( 'Filial Srv.' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '8' ).

      lr_column2 = lr_columns2->get_column( 'QTDPEDID' ).
      lr_column2->set_short_text( 'Qtd.Ped.' ).
      lr_column2->set_medium_text( 'Qtd.Ped.' ).
      lr_column2->set_long_text( 'Qtd.Ped.' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '15' ).


      lr_column2 = lr_columns2->get_column( 'QTDFATUR' ).
      lr_column2->set_short_text( 'Qtd.Fat.' ).
      lr_column2->set_medium_text( 'Qtd.Fat.' ).
      lr_column2->set_long_text( 'Qtd.Fat.' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '15' ).


      lr_column2 = lr_columns2->get_column( 'QTDSALDO' ).
      lr_column2->set_short_text( 'Qtd.Saldo' ).
      lr_column2->set_medium_text( 'Qtd.Saldo' ).
      lr_column2->set_long_text( 'Qtd.Saldo' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '15' ).


      lr_column2 = lr_columns2->get_column( 'QTDSOBRA' ).
      lr_column2->set_short_text( 'Qtd.Sobra' ).
      lr_column2->set_medium_text( 'Qtd.Sobra' ).
      lr_column2->set_long_text( 'Qtd.Sobra' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).


      lr_column2 = lr_columns2->get_column( 'NAVIO' ).
      lr_column2->set_short_text( 'Navio' ).
      lr_column2->set_medium_text( 'Navio' ).
      lr_column2->set_long_text( 'Navio' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '12' ).

      lr_column2 = lr_columns2->get_column( 'LOCAL_OPERACAO' ).
      lr_column2->set_short_text( 'Loc.Oper.' ).
      lr_column2->set_medium_text( 'Loc.Operação' ).
      lr_column2->set_long_text( 'Loc.Operação' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '12' ).

      lr_column2 = lr_columns2->get_column( 'DT_FATURA' ).
      lr_column2->set_short_text( 'Dt.Fatura' ).
      lr_column2->set_medium_text( 'Dt.Fatura' ).
      lr_column2->set_long_text( 'Dt.Fatura' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).

      lr_column2 = lr_columns2->get_column( 'WAERK' ).
      lr_column2->set_short_text( 'Moeda.Ct.' ).
      lr_column2->set_medium_text( 'Moeda.Ct.' ).
      lr_column2->set_long_text( 'Moeda.Ct.' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '7' ).

      lr_column2 = lr_columns2->get_column( 'NETPR' ).
      lr_column2->set_short_text( 'Vlr.Unit.' ).
      lr_column2->set_medium_text( 'Vlr.Unit.' ).
      lr_column2->set_long_text( 'Vlr.Unit.' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).

      lr_column2 = lr_columns2->get_column( 'TAX_DOLAR' ).
      lr_column2->set_short_text( 'Tx.OV' ).
      lr_column2->set_medium_text( 'Tx.OV' ).
      lr_column2->set_long_text( 'Tx.OV' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).

      lr_column2 = lr_columns2->get_column( 'VLR_USD' ).
      lr_column2->set_short_text( 'Vlr.USD' ).
      lr_column2->set_medium_text( 'Vlr.USD' ).
      lr_column2->set_long_text( 'Vlr.USD' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).

      lr_column2 = lr_columns2->get_column( 'VLR_BRL' ).
      lr_column2->set_short_text( 'Vlr.BRL' ).
      lr_column2->set_medium_text( 'Vlr.BRL' ).
      lr_column2->set_long_text( 'Vlr.BRL' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).

      lr_column2 = lr_columns2->get_column( 'NR_OV' ).
      lr_column2->set_short_text( 'Nr.OV' ).
      lr_column2->set_medium_text( 'Nr.OV' ).
      lr_column2->set_long_text( 'Nr.OV' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).

      lr_column2 = lr_columns2->get_column( 'DOCNUM_NF' ).
      lr_column2->set_short_text( 'Nr.NF' ).
      lr_column2->set_medium_text( 'Nr.NF' ).
      lr_column2->set_long_text( 'Nr.NF' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).

      lr_column2 = lr_columns2->get_column( 'FATURA' ).
      lr_column2->set_short_text( 'Fatura' ).
      lr_column2->set_medium_text( 'Fatura' ).
      lr_column2->set_long_text( 'Fatura' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '10' ).

      lr_column2 = lr_columns2->get_column( 'AUART' ).
      lr_column2->set_short_text( 'Tp.OV' ).
      lr_column2->set_medium_text( 'Tp.OV' ).
      lr_column2->set_long_text( 'Tp.OV' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '8' ).

      lr_column2 = lr_columns2->get_column( 'OPERACAO' ).
      lr_column2->set_short_text( 'Oper' ).
      lr_column2->set_medium_text( 'Operaão' ).
      lr_column2->set_long_text( 'Operação' ).
      lr_column2->set_optimized( abap_false ).
      lr_column2->set_output_length( '4' ).

*      " Esconde Coluna
      lr_column2 = lr_columns2->get_column( 'DT_RECREG' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'MANDT' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'DATA_REGISTRO' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'HORA_REGISTRO' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'USUARIO' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'BUKRS_PED' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'WERKS_PED' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'MATNR_OV' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'MATKL' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'STATUS' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'VKORG' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'KUNNR' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'EBELN' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'EBELP' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'MENGE' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'MEINS' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'GEWEI' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'PEDIDOS' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'CENTRO_FAT_SERV' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'KURST' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'VTWEG' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'SPART' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'ZTERM' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'VKAUS' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'MATNR' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'BUKRS_FAT' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).
      lr_column2 = lr_columns2->get_column( 'WERKS_FA' ).
      lr_column2->set_visible( if_salv_c_bool_sap=>false ).


    CATCH cx_salv_not_found INTO lex_not_found2.

  ENDTRY.



ENDFORM.

FORM organizar_colunas2."USING lr_columns2 TYPE REF TO cl_salv_columns.
  lr_columns2->set_column_position(  columnname =  'ID_SEQ'               position = 1 ).
  lr_columns2->set_column_position(  columnname =  'NR_OV'                position = 2 ).
  lr_columns2->set_column_position(  columnname =  'DOCNUM_NF'            position = 3 ).
  lr_columns2->set_column_position(  columnname =  'FATURA'               position = 4 ).
  lr_columns2->set_column_position(  columnname =  'CL_CODIGO'            position = 5 ).
  lr_columns2->set_column_position(  columnname =  'BUKRS_FAT'            position = 6 ).
  lr_columns2->set_column_position(  columnname =  'WERKS_FAT'            position = 7 ).
  lr_columns2->set_column_position(  columnname =  'OPERACAO'             position = 8 ).
  lr_columns2->set_column_position(  columnname =  'QTDPEDID'             position = 9 ).
  lr_columns2->set_column_position(  columnname =  'QTDFATUR'             position = 10 ).
  lr_columns2->set_column_position(  columnname =  'QTDSALDO'             position = 11 ).
  lr_columns2->set_column_position(  columnname =  'QTDSOBRA'             position = 12 ).
  lr_columns2->set_column_position(  columnname =  'NAVIO'                position = 13 ).
  lr_columns2->set_column_position(  columnname =  'LOCAL_OPERACAO'       position = 14 ).
  lr_columns2->set_column_position(  columnname =  'DT_FATURA'            position = 15 ).
  lr_columns2->set_column_position(  columnname =  'AUART'                position = 16 ).
  lr_columns2->set_column_position(  columnname =  'WAERK'                position = 17 ).
  lr_columns2->set_column_position(  columnname =  'NETPR'                position = 18 ).
  lr_columns2->set_column_position(  columnname =  'TAX_DOLAR'            position = 19 ).
  lr_columns2->set_column_position(  columnname =  'VLR_USD'              position = 20 ).
  lr_columns2->set_column_position(  columnname =  'VLR_BRL'              position = 21 ).


ENDFORM.

FORM show_function_info2 USING i_function2 TYPE salv_de_function  i_text2 TYPE string.

  IF i_function2 IS NOT INITIAL.
    CASE i_function2.
      WHEN 'INSERT_ROW'.

        PERFORM popup_insert_row.
        PERFORM  pega_dados.
        gr_table2->refresh( ).
      WHEN 'DELETE_ROW'.
        CLEAR: lt_rows2, ls_row2.
        lt_rows2 = gr_table2->get_selections( )->get_selected_rows( ).

        DESCRIBE TABLE lt_rows2 LINES DATA(linhas).

        IF linhas > 0.
          LOOP AT lt_rows2 INTO ls_row2.

            READ TABLE t_saida2 ASSIGNING FIELD-SYMBOL(<_get>) INDEX ls_row2.
            IF <_get>-nr_ov IS INITIAL.
              DELETE zsdt0306_fat FROM <_get>.
              DELETE t_saida2 INDEX ls_row2.
              COMMIT WORK.
            ELSE.
              MESSAGE 'Não é possivel excluir o item com Lote e Faturamentos Gerados!' TYPE 'E' DISPLAY LIKE 'I'.
              _erro = 'X'.
              EXIT.
            ENDIF.

          ENDLOOP.
        ELSE.
          MESSAGE 'Selecione uma linha!' TYPE 'E' DISPLAY LIKE 'I'.
          _erro = 'X'.
          EXIT.
        ENDIF.


        PERFORM  pega_dados.
        gr_table2->refresh( ).
      WHEN 'FATURAR'.
        PERFORM faturar.
        PERFORM  pega_dados.
        gr_table2->refresh( ).
      WHEN 'ESTORNAR'.
        PERFORM estornar.
        PERFORM  pega_dados.
        gr_table2->refresh( ).
*      WHEN 'ONLI'.
*      WHEN 'BACK'.
*        SET SCREEN 0.
*        LEAVE SCREEN.
*      WHEN 'CANCEL'.
*        SET SCREEN 0.
*        LEAVE SCREEN.
*      WHEN 'EXIT'.
*        SET SCREEN 0.
*        LEAVE SCREEN.
    ENDCASE.
    gr_table2->refresh( ).
  ENDIF.
ENDFORM.

FORM make_alv2_row_cell.

  ls_api2 = gr_table2->extended_grid_api( ).
  ls_edit2 = ls_api2->editable_restricted( ).

  gr_table2->refresh( ).
ENDFORM.

FORM make_alv2.

  ls_api2 = gr_table2->extended_grid_api( ).
  ls_edit2 = ls_api2->editable_restricted( ).

  TRY.

      ls_edit2->set_attributes_for_columnname(
        EXPORTING
          columnname              = 'QTDFATUR'
          all_cells_input_enabled = abap_true
      ).
      ls_edit2->set_attributes_for_columnname(
        EXPORTING
          columnname              = 'QTDSOBRA'
          all_cells_input_enabled = abap_true
      ).
      ls_edit2->set_attributes_for_columnname(
        EXPORTING
          columnname              = 'NAVIO'
          all_cells_input_enabled = abap_true
      ).
      ls_edit2->set_attributes_for_columnname(
        EXPORTING
          columnname              = 'LOCAL_OPERACAO'
          all_cells_input_enabled = abap_true
      ).
      ls_edit2->set_attributes_for_columnname(
        EXPORTING
          columnname              = 'DT_FATURA'
          all_cells_input_enabled = abap_true
      ).

      ls_edit2->set_attributes_for_columnname(
        EXPORTING
          columnname              = 'AUART'
          all_cells_input_enabled = abap_true
      ).
      ls_edit2->set_attributes_for_columnname(
        EXPORTING
          columnname              = 'WAERK'
          all_cells_input_enabled = abap_true
      ).
    CATCH cx_root.
  ENDTRY.
  ls_edit2->validate_changed_data( ).
  gr_table2->refresh( ).
ENDFORM.

FORM resultado_total_alv1 .

  CLEAR: w_saida1.
  DESCRIBE TABLE t_saida1 LINES DATA(lv_tab_lines).

  READ TABLE t_saida1 INTO w_saida1 INDEX 1.

  IF lv_tab_lines >= 1.
    DATA: aux_qtdfattot TYPE p DECIMALS 3.
    DATA: aux_qtdpedtot TYPE p DECIMALS 3.
    DATA: aux_qtdsobtot TYPE p DECIMALS 3.
    CLEAR: aux_qtdfattot,aux_qtdpedtot,aux_qtdsobtot.



    LOOP AT t_saida1 ASSIGNING FIELD-SYMBOL(<soma_qtd_ped>).
      aux_qtdpedtot = aux_qtdpedtot + <soma_qtd_ped>-qtdpedid.
    ENDLOOP.

    LOOP AT t_saida2 ASSIGNING FIELD-SYMBOL(<soma_qtd_fat>) WHERE id_seq = w_saida1-id_seq .
      aux_qtdfattot = aux_qtdfattot + <soma_qtd_fat>-qtdfatur.
      aux_qtdsobtot = aux_qtdsobtot + <soma_qtd_fat>-qtdsobra.
    ENDLOOP.

    CLEAR: w_saida1-qtdpedid.
    "valor atualizado!
    w_saida1-qtdpedid = ( aux_qtdpedtot - aux_qtdfattot ) + aux_qtdsobtot.

  ENDIF.
ENDFORM.


FORM get_values_alv2.

  CLEAR: lr_scell2,
  lr_scells2,
  lr_scolumns2,
  lr_srows2,
  lr_smode2,
  linhas.

  lr_scell2 = gr_table2->get_selections( )->get_current_cell( ).
  lr_scells2 = gr_table2->get_selections( )->get_selected_cells( ).
  lr_scolumns2 = gr_table2->get_selections( )->get_selected_columns( ).
  lr_srows2 = gr_table2->get_selections( )->get_selected_rows( ).
  lr_smode2 = gr_table2->get_selections( )->get_selection_mode( ).

  DESCRIBE TABLE lr_srows2 LINES linhas.

ENDFORM.

FORM faturar.

*  DATA: lc_dados   TYPE zsde0183,    "*-CS2025000025-#164218-27.01.2025-JT-inicio
*        lc_retorno TYPE zsdt0370_t,  "*-CS2025000025-#164218-27.01.2025-JT-inicio
*        wc_retorno TYPE zsdt0370.    "*-CS2025000025-#164218-27.01.2025-JT-inicio

  FREE: t_success
  ,t_billing
  ,t_return
  ,it_zlest0055
  ,t_saida2_aux
  ,t_saida2_ori
  ,t_saida2_ov
  ,r_vlr_brl[].

  PERFORM resultado_total_alv1.
  PERFORM get_values_alv2.
  CLEAR: w_saida2,vbeln_fatura
  ,wa_j_1bnflin
  ,wl_lifnr
  ,l_itm_number
  ,msg
  ,_dia
  ,r_auart_ztrg
  ,w_zsdt0225
  ,w_zsdt0307
  ,r_matkl
  ,t_matkl
  ,wa_zlest0055
  ,l_pedidos
  ,l_erro
  ,l_tabix
  ,w_saida2_aux
  ,w_saida2_ori
  ,w_saida2_ov
  ,w_stable
  ,v_vlr_brl.



  IF     lr_scell2-row = 0.
    MESSAGE 'Selecionar uma Linha!' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.
  ELSEIF linhas > 1.
    MESSAGE 'Selecionar apenas uma Linha para gerar OV!' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.
  ENDIF.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = 50
      text       = 'Gerando Ordem de Venda / Fatura...'.

  CLEAR: w_zsdt0225, w_zsdt0307.

  FREE: wl_header_in,
  wl_header_inx,
  it_itemdata,
  it_items_inx,
  tl_schedules_in,
  it_condition,
  it_partner,
  it_return.

*---------------------------------
*- validacoes
*---------------------------------
  READ TABLE t_saida2 INTO w_saida2 INDEX lr_scell2-row.

  IF w_saida1-matnr IS NOT INITIAL."147507 Correção de erro ao criar fatura na ZSDT0158 PSA
    SELECT SINGLE *
FROM zsdt0307
INTO w_zsdt0307
WHERE emp_pedido   = w_saida2-bukrs_ped
AND emp_fat_serv = w_saida2-bukrs_fat.

    IF sy-subrc = 0.
      wl_lifnr = |{ w_zsdt0307-centro_fat_serv ALPHA = IN }|.
      SELECT SINGLE * FROM lfa1 INTO @DATA(w_lfa1) WHERE lifnr EQ @wl_lifnr.
      SELECT SINGLE * FROM kna1 INTO @DATA(w_kna1) WHERE kunnr EQ @w_saida2-cl_codigo.

      SELECT SINGLE *
  FROM j_1btxiss
  INTO @DATA(w_j_1btxiss)
      WHERE country    EQ 'BR'
      AND gruop      EQ '73'
      AND taxjurcode EQ @w_kna1-txjcd  "( domicílio fiscal do cliente da OV ) - SP 3522307
      AND value      EQ @w_lfa1-txjcd  "( domicilio fiscal do centro emissor do serviço )  - AM 1301902
      AND validto    <= @w_saida2-dt_fatura    "( data final  da  vigencia)
      AND validfrom  >= @w_saida2-dt_fatura.  "(  data início de vigência)

      IF sy-subrc <> 0.
        DATA(l0) = |Mat: Alíquota do ISS  não parametrizada { w_kna1-txjcd } - { w_lfa1-txjcd } . Abrir uma FI|.
        "APPEND VALUE #( message = l0 status = icon_led_red ) TO it_err[].
        MESSAGE l0 TYPE 'I'.
        EXIT.
        _erro = 'X'.
      ENDIF.
    ENDIF.
  ENDIF.


  IF w_saida2-matnr_ov IS NOT INITIAL.
    MESSAGE 'Já existe OV e Faturas Geradas!' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.
  ENDIF.

  l_id_seq = w_saida2-id_seq.

  IF w_saida2-waerk IS INITIAL.
    MESSAGE 'Moeda Fatura não encontrada, favor parametrizar na transação ZLES0075.'TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.
  ENDIF.

  IF w_saida2-navio IS INITIAL.
    MESSAGE 'Informar o Navio!' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.
  ENDIF.

  IF w_saida2-local_operacao IS INITIAL.
    MESSAGE 'Informar o Local de Operação!' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.
  ENDIF.

  SELECT SINGLE *
  FROM zsdt0307
  INTO w_zsdt0307
  WHERE emp_pedido   = w_saida2-bukrs_ped
  AND emp_fat_serv = w_saida2-bukrs_fat.

  IF sy-subrc = 0.
    wl_lifnr = |{ w_zsdt0307-centro_fat_serv ALPHA = IN }|.
  ENDIF.

  FREE: r_matkl.
  t_matkl-sign   = 'I'.
  t_matkl-option = 'CP'.
  t_matkl-low    = '*' && w_saida2-matkl && '*'.
  APPEND t_matkl TO r_matkl.

  SELECT *
   FROM zlest0055
   WHERE kunnr    EQ @w_saida2-kunnr
    AND auart    EQ @w_saida2-auart
    AND matkl    IN @r_matkl
    AND dt_fim   >= @w_saida2-dt_fatura
    AND waerk    >= @w_saida2-waerk
    AND status   EQ '1'
    AND vkorg    EQ @w_zsdt0307-emp_fat_serv
    AND operacao EQ @w_saida2-operacao "#161385 / Renato
   INTO TABLE @it_zlest0055.

  CLEAR: wa_zlest0055.
  READ TABLE it_zlest0055 INTO wa_zlest0055 INDEX 1.

  LOOP AT t_saida2  INTO w_saida2 WHERE id_seq = l_id_seq.
    w_saida2-vkorg     = wa_zlest0055-vkorg.
    w_saida2-waerk_fatura = wa_zlest0055-waerk_fatura. "#161385 / RGA
*    w_saida2-vtweg     = wa_zlest0055-vtweg.
*    w_saida2-spart     = wa_zlest0055-spart.
*    w_saida2-netpr     = wa_zlest0055-netpr.
    MODIFY t_saida2 FROM w_saida2 INDEX sy-tabix.
    COMMIT WORK.
  ENDLOOP.

  CLEAR _dia.

  _dia = w_saida2-dt_fatura+6(2).
  IF ( _dia >= 01 ) AND  (  _dia <= 15 ).
    w_saida2-zterm    = wa_zlest0055-zterm.
  ELSEIF ( _dia >= 16 ) AND  (  _dia <= 31 ).
    w_saida2-zterm    = wa_zlest0055-zterm2.
  ENDIF.

  SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1) WHERE lifnr EQ @wl_lifnr.
  SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1) WHERE kunnr EQ @w_saida2-cl_codigo.

*----------------------
* pedido recebedor mercadoria
*----------------------
  vpurch_no_s              = w_saida2-id_seq.

*----------------------
* monta cabecalho
*----------------------
  wl_header_in-sales_org   = w_saida2-vkorg. "(org. de venda)
  wl_header_in-distr_chan  = w_saida2-vtweg. "(canal distr.)
  wl_header_in-currency    = wa_zlest0055-waerk_fatura." #161385 / RG w_saida2-waerk. "(moeda.) " gw_saida2_ger_ov-waerk 12.12.16
  wl_header_in-pymt_meth   = 'P'.
  wl_header_in-division    = w_saida2-spart. "(setor atividade)
  wl_header_in-doc_type    = w_saida2-auart. "(tipo de ordem)
  wl_header_in-pmnttrms    = w_saida2-zterm. "ZTERM.
  wl_header_in-exrate_fi   = w_saida2-tax_dolar."(taxa dolar)
  wl_header_in-bill_date   = w_saida2-dt_fatura.
  wl_header_in-purch_no_c  = vpurch_no_s. "'.'.
  wl_header_in-purch_no_s  = vpurch_no_s.
  wl_header_in-fix_val_dy  = w_saida2-dt_fatura. "VALDT Data efetiva fixa
  wl_header_in-pymt_meth   = ''. "ZLSCH. Forma de pagamento
  wl_header_in-dlvschduse  = wa_zlest0055-vkaus. "VKAUS. Código de utilização
  wl_header_in-incoterms1  = 'SRV'.
  wl_header_in-incoterms2  = 'Serviço'.

*----------------------
* Set utilizado por odens ZTRG criadas pela transação ZSDT0158
*----------------------
  SELECT *
  FROM setleaf
  INTO TABLE @DATA(it_set_ztrg)
        WHERE setname = 'MAGGI_VF01_TP_OV'.

  LOOP AT it_set_ztrg INTO DATA(wa_set_ztrg).
    APPEND VALUE rsdsselopt( option = wa_set_ztrg-valoption  sign = wa_set_ztrg-valsign low = wa_set_ztrg-valfrom )
    TO r_auart_ztrg.
  ENDLOOP.

  IF w_saida2-auart IN r_auart_ztrg.
    wl_header_in-req_date_h = w_saida2-dt_fatura.
    wl_header_in-price_date  =  w_saida2-dt_fatura.
  ENDIF.

*------------------------------------------------
* itens da OV
*------------------------------------------------
  FREE: t_saida2_ov, "lc_dados,
  l_pedidos,
  l_erro.

  LOOP AT t_saida2 INTO w_saida2 WHERE id_seq = l_id_seq.

    l_tabix = sy-tabix.

    DATA: wl_zlest0059 TYPE zlest0059.
    CLEAR: wl_zlest0059.
    SELECT SINGLE *
    FROM zlest0059
    INTO @wl_zlest0059
    WHERE bukrs       EQ @w_saida2-bukrs_fat
    AND auart       EQ @w_saida2-auart
    AND po_embarque EQ @abap_off
    AND po_destino  EQ @abap_off.

    IF sy-subrc <> 0.
      l_erro = abap_true.
      MESSAGE 'Material não encontrado para Tipo Venda'  TYPE 'E' DISPLAY LIKE 'I'.
      _erro = 'X'.
      EXIT.
    ELSE.
      w_saida2-matnr_ov     = wl_zlest0059-matnr.
    ENDIF.

    MODIFY t_saida2      FROM w_saida2 INDEX l_tabix.
    COMMIT WORK.

    w_saida2_ov-id_seq      = w_saida2-id_seq.
    w_saida2_ov-matnr_ov    = w_saida2-matnr_ov.
    "somente a quantidade a ser criada
    IF w_saida2-nr_ov IS INITIAL. "#180457 - RG - 23.05.2025
      w_saida2_ov-menge       = w_saida2-qtdfatur. "w_saida2-menge
    ENDIF.

    w_saida2_ov-vlr_brl     = w_saida2-vlr_brl.
    w_saida2_ov-vlr_usd     = w_saida2-vlr_usd.
    COLLECT w_saida2_ov  INTO t_saida2_ov.
  ENDLOOP.

  CHECK l_erro = abap_false.

  t_saida2_ori[] = t_saida2[].

  LOOP AT t_saida2_ov   INTO w_saida2_ov.

    FREE: l_pedidos.

    LOOP AT t_saida2    INTO w_saida2 WHERE id_seq   = w_saida2_ov-id_seq
    AND matnr_ov = w_saida2_ov-matnr_ov.
      w_saida2-menge       = w_saida2_ov-menge.
      l_pedidos           = l_pedidos && w_saida2-ebeln && '/' && w_saida2-ebelp && '-'.
      MODIFY t_saida2   FROM w_saida2 INDEX sy-tabix.
      COMMIT WORK.
    ENDLOOP.

    CLEAR: vlr_icms, vlr_pis, vlr_cofins, vlr_iss, vlr_liquido.

    PERFORM f_get_set.

    IF w_saida2-vlr_brl > r_vlr_brl-low.
      l_erro = abap_true.
      t_saida2[] = t_saida2_ori[].
      msg    = 'Valor BRL não pode ser maior que R$'.
      CONCATENATE msg v_vlr_brl INTO  msg SEPARATED BY space.
      MESSAGE msg  TYPE 'E' DISPLAY LIKE 'I'.
      _erro = 'X'.
      EXIT.
    ENDIF.

*-CS2025000025-#164218-27.01.2025-JT-inicio
    SELECT *
    FROM zsdt0008
    INTO TABLE @DATA(it_zsdt0008)
          WHERE auart      EQ @w_saida2-auart
          AND vkaus      EQ @wa_zlest0055-vkaus
          AND uf_centro  EQ @wa_lfa1-regio
          AND uf_cliente EQ @wa_kna1-regio
          AND mwsk1      EQ 'SD'
          AND ownpr      NE 'X'.

*    lc_dados-auart-valor      = w_saida2-auart.
*    lc_dados-vkaus-valor      = wa_zlest0055-vkaus.
*    lc_dados-mwsk1-valor      = 'SD'.
*    lc_dados-uf_centro-valor  = wa_lfa1-regio.
*    lc_dados-uf_cliente-valor = wa_kna1-regio.
*    lc_dados-ownpr-regra      = 'NE'.
*    lc_dados-ownpr-valor      = 'X'.
*    lc_dados-bukrs_emit-valor = w_saida2-bukrs_fat.
**   lc_dados-bukrs_toma-valor = w_saida-bukrs_fat.
*    lc_dados-kunnr-valor      = w_saida2-cl_codigo.
*    lc_dados-werks-valor      = w_saida2-werks_fat.
*    lc_dados-matnr-valor      = w_saida2-matnr.
*
*    lc_retorno = zcl_impostos=>get_tax_imposto( i_dados = lc_dados i_todos = abap_true ).
*
*    READ TABLE lc_retorno INTO wc_retorno INDEX 1.
*-CS2025000025-#164218-27.01.2025-JT-fim

    IF sy-subrc EQ 0.
      SELECT *
      FROM j_1btxsdc
      INTO TABLE @DATA(it_j_1btxsdc)
            FOR ALL ENTRIES IN @it_zsdt0008           "*-CS2025000025-#164218-27.01.2025-JT
            WHERE taxcode   EQ @it_zsdt0008-j_1btxsdc "*-CS2025000025-#164218-27.01.2025-JT
            AND custusage EQ 1.

      IF sy-subrc EQ 0.
        LOOP AT it_j_1btxsdc INTO DATA(wa_j_1btxsdc).

          CLEAR: vlr_icms, vlr_pis, vlr_cofins.

          IF wa_j_1btxsdc-icms EQ 'X'.
            SELECT SINGLE *
            FROM j_1btxic1
            INTO @DATA(wa_j_1btxic1)
                  WHERE land1    EQ 'BR'
                  AND shipfrom EQ @wa_lfa1-regio
                  AND shipto   EQ @wa_kna1-regio.

            CASE w_saida2-waerk.
              WHEN 'BRL'.
                vlr_icms = ( w_saida2-vlr_brl * wa_j_1btxic1-rate  ) / 100.
              WHEN 'USD'.
                vlr_icms = ( w_saida2-vlr_usd * wa_j_1btxic1-rate  ) / 100.
            ENDCASE.
          ENDIF.

          IF wa_j_1btxsdc-pis EQ 'X'.
            DATA: wa_j_1btxpis TYPE j_1btxpis.
            SELECT SINGLE *
            FROM j_1btxpis
            INTO @wa_j_1btxpis
            WHERE country    EQ 'BR'
            AND gruop      EQ '72'
            AND value      EQ @w_saida2-werks_fat
            AND validto    <= @w_saida2-dt_fatura
            AND validfrom  >= @w_saida2-dt_fatura.

            CASE w_saida2-waerk.
              WHEN 'BRL'.
                vlr_pis = ( w_saida2-vlr_brl *  wa_j_1btxpis-rate  ) / 100.
              WHEN 'USD'.
                vlr_pis = ( w_saida2-vlr_usd *  wa_j_1btxpis-rate  ) / 100.
            ENDCASE.
          ENDIF.

          IF ( wa_j_1btxsdc-cofins EQ 'X' ).

            DATA: wa_j_1btxcof TYPE j_1btxcof.

            SELECT SINGLE *
            FROM j_1btxcof
            INTO @wa_j_1btxcof
            WHERE country   EQ 'BR'
            AND gruop     EQ '71'
            AND value     EQ @w_saida2-werks_fat
            AND validto   <= @w_saida2-dt_fatura
            AND validfrom >= @w_saida2-dt_fatura.

            CASE w_saida2-waerk.
              WHEN: 'BRL'.
                vlr_cofins  = ( w_saida2-vlr_brl * wa_j_1btxcof-rate ) / 100.
              WHEN: 'USD'.
                vlr_cofins  = ( w_saida2-vlr_usd * wa_j_1btxcof-rate ) / 100.
            ENDCASE.
          ENDIF.

          CASE w_saida2-waerk.
            WHEN: 'BRL'.
              vlr_liquido = w_saida2-vlr_brl - vlr_pis - vlr_cofins - vlr_icms.
            WHEN: 'USD'.
              vlr_liquido = w_saida2-vlr_usd - vlr_pis - vlr_cofins - vlr_icms.
          ENDCASE.
        ENDLOOP.
      ENDIF.
    ELSE.
      FREE: it_j_1btxsdc.

      SELECT *
      FROM j_1bsdica
      INTO TABLE @DATA(it_j_1bsdica)
            WHERE auart EQ @w_saida2-auart.

      SELECT *
      FROM j_1btxsdc
      INTO TABLE  it_j_1btxsdc
      FOR ALL ENTRIES IN it_j_1bsdica
      WHERE taxcode  EQ it_j_1bsdica-txsdc
      AND custusage EQ 1.

      IF sy-subrc EQ 0.
        CLEAR: wa_j_1btxpis, wa_j_1btxcof, vl_data, vl_validto, vl_validfrom.

        CONCATENATE w_saida2-dt_fatura+6(2) '.'  w_saida2-dt_fatura+4(2) '.' w_saida2-dt_fatura(4) INTO vl_data.

        CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
          EXPORTING
            input  = vl_data
          IMPORTING
            output = vl_validto.

        vl_validfrom  = vl_validto.

        LOOP AT it_j_1btxsdc INTO wa_j_1btxsdc.

          IF wa_j_1btxsdc-iss EQ 'X'.
            SELECT SINGLE *
            FROM j_1btxiss
            INTO @DATA(wa_j_1btxiss)
                  WHERE country    EQ 'BR'
                  AND gruop      EQ '73'
                  AND taxjurcode EQ @wa_kna1-txjcd
                  AND value      EQ @wa_lfa1-txjcd
                  AND validto    <= @vl_validto
                  AND validfrom  >= @vl_validfrom.

            CASE w_saida2-waerk_fatura. " #161385 / RG
              WHEN: 'BRL'.
                vlr_iss   = (  w_saida2-vlr_brl * wa_j_1btxiss-rate ) / 100.
              WHEN: 'USD'.
                vlr_iss    = ( w_saida2-vlr_usd * wa_j_1btxiss-rate ) / 100.
            ENDCASE.
          ENDIF.

          IF wa_j_1btxsdc-pis EQ 'X'.
            SELECT SINGLE *
            FROM j_1btxpis
            INTO wa_j_1btxpis
            WHERE country   EQ 'BR'
            AND gruop     EQ '72'
            AND value     EQ w_saida2-werks_fat
            AND validto   <= vl_validto
            AND validfrom >= vl_validfrom.

            CASE w_saida2-waerk_fatura." #161385 / RG
              WHEN: 'BRL'.
                vlr_pis     = ( w_saida2-vlr_brl * wa_j_1btxpis-rate ) / 100.
              WHEN: 'USD'.
                vlr_pis     = ( w_saida2-vlr_usd * wa_j_1btxpis-rate ) / 100.
            ENDCASE.
          ENDIF.

          IF wa_j_1btxsdc-cofins EQ 'X'.
            SELECT SINGLE *
            FROM j_1btxcof
            INTO wa_j_1btxcof
            WHERE country    EQ 'BR'
            AND gruop      EQ '71'
            AND value      EQ w_saida2-werks_fat
            AND validto    <= vl_validto
            AND validfrom  >= vl_validfrom.

            CASE w_saida2-waerk_fatura. " #161385 / RG  "gw_saida2_ger_ov-waerk 12.12.16
              WHEN: 'BRL'.
                vlr_cofins  = ( w_saida2-vlr_brl * wa_j_1btxcof-rate ) / 100.
              WHEN: 'USD'.
                vlr_cofins  = ( w_saida2-vlr_usd * wa_j_1btxcof-rate ) / 100.
            ENDCASE.
          ENDIF.
        ENDLOOP.

        CASE w_saida2-waerk_fatura. " #161385 / RG  "gw_saida2_ger_ov-waerk 12.12.16
          WHEN: 'BRL'.
            vlr_liquido = w_saida2-vlr_brl - vlr_iss - vlr_pis - vlr_cofins.
          WHEN: 'USD'.
            vlr_liquido = w_saida2-vlr_usd - vlr_iss - vlr_pis - vlr_cofins.
        ENDCASE.
      ENDIF.
    ENDIF.

    CLEAR: wa_itemdata, wl_items_inx, wl_schedules_in, wa_condition.


    DATA: wmara TYPE mara.
    CLEAR wmara.

    SELECT SINGLE *
    FROM mara
    INTO @wmara
    WHERE matnr EQ @w_saida2-matnr_ov.

    IF sy-subrc <> 0.
      CLEAR wmara.
    ENDIF.

    l_itm_number            = l_itm_number + 10.

    wa_itemdata-itm_number  = l_itm_number.
    wa_itemdata-material    = w_saida2-matnr_ov.
    wa_itemdata-plant       = w_zsdt0307-centro_fat_serv.
    wa_itemdata-division    = w_saida2-spart. "(setor atividade)
    wa_itemdata-target_qty  = 1.
    wa_itemdata-target_qu   = wmara-meins.
    wa_itemdata-sales_unit  = wmara-meins.
    wa_itemdata-gross_wght  = w_saida2-menge.
    wa_itemdata-net_weight  = w_saida2-menge.
    wa_itemdata-untof_wght  = wmara-gewei.
    wa_itemdata-fix_val_dy  = w_saida2-dt_fatura.
    wa_itemdata-price_date  = w_saida2-dt_fatura.
    wa_itemdata-ex_rate_fi  = w_saida2-tax_dolar.
    wa_itemdata-dlvschduse  = wa_zlest0055-vkaus.
    wa_itemdata-incoterms1  = 'SRV'.
    wa_itemdata-incoterms2  = 'Serviço'.
    wa_itemdata-purch_no_c  = l_pedidos. "w_saida2-ebeln && '/' && w_saida2-ebelp.


    DATA: wa_knvv TYPE knvv.
    CLEAR: wa_knvv.

    SELECT SINGLE *
    FROM knvv
    INTO @wa_knvv
    WHERE kunnr  EQ @w_saida2-kunnr
    AND vkorg  EQ @w_saida2-vkorg
    AND vtweg  EQ @w_saida2-vtweg
    AND spart  EQ @w_saida2-spart.

    IF sy-subrc <> 0.
      CLEAR wa_knvv.
    ENDIF.

    wa_knvv-kdgrp = |{ wa_knvv-kdgrp ALPHA = IN }|.

    DATA: wa_marc TYPE marc.
    CLEAR: wa_marc.

    SELECT SINGLE *
    FROM marc
    INTO @wa_marc
    WHERE matnr EQ @w_saida2-matnr_ov
    AND werks EQ @w_zsdt0307-centro_fat_serv.

    DATA: wa_mbew TYPE mbew.
    CLEAR: wa_mbew.

    SELECT SINGLE *
    FROM mbew
    INTO @wa_mbew
    WHERE matnr EQ @w_saida2-matnr_ov
    AND bwkey EQ @w_zsdt0307-centro_fat_serv.

    IF wa_lfa1-regio EQ wa_kna1-regio.

      DATA: wa_1bapn TYPE j_1bapn.
      CLEAR: wa_1bapn.

      SELECT SINGLE *
      FROM j_1bapn
      INTO @wa_1bapn
      WHERE direct EQ '2'
      AND dstcat EQ '0'
      AND indus3 EQ @wa_marc-indus
      AND itmtyp EQ 'ZH'
      AND ownpro EQ ' '
      AND matuse EQ @wa_mbew-mtuse
      AND indus1 EQ ' '.
    ELSE.
      SELECT SINGLE *
      FROM j_1bapn
      INTO wa_1bapn
      WHERE direct EQ '2'
      AND dstcat EQ '1'
      AND indus3 EQ wa_marc-indus
      AND itmtyp EQ 'ZH'
      AND ownpro EQ ' '
      AND matuse EQ wa_mbew-mtuse
      AND indus1 EQ ' '.
    ENDIF.

    wa_itemdata-cfop_long      = wa_1bapn-cfop.
    APPEND wa_itemdata        TO it_itemdata.

    wl_items_inx-itm_number    = l_itm_number.
    wl_items_inx-target_qty    = 'X'.
    APPEND wl_items_inx       TO it_items_inx.

    wl_schedules_in-itm_number = l_itm_number.
    wl_schedules_in-req_qty    = 1.
    APPEND wl_schedules_in    TO  tl_schedules_in.

    wa_condition-itm_number    = l_itm_number.
    wa_condition-cond_type     = 'PR00'.

    IF vlr_liquido IS NOT INITIAL.
      wa_condition-cond_value  =  vlr_liquido.
    ELSE.
      IF w_saida2-waerk_fatura = 'BRL'. " #161385 / RG
        wa_condition-cond_value = w_saida2-vlr_brl.
      ELSE.
        wa_condition-cond_value = w_saida2-vlr_usd.
      ENDIF.
    ENDIF.

*    wa_condition-currency      = w_saida2-waerk.
    wa_condition-currency      = w_saida2-waerk_fatura. "// wbarbosa 220525
    wa_condition-cond_unit     = wmara-meins.
    APPEND  wa_condition      TO it_condition.
  ENDLOOP.

  CHECK l_erro = abap_false.

*----------------------
* parceiros
*----------------------
  wa_partner-partn_role = 'AG'.
  wa_partner-partn_numb = w_saida2-cl_codigo.
  APPEND wa_partner TO it_partner.

  wa_partner-partn_role = 'RE'.
  wa_partner-partn_numb = w_saida2-cl_codigo.
  APPEND wa_partner TO it_partner.

  wa_partner-partn_role = 'RG'.
  wa_partner-partn_numb = w_saida2-cl_codigo.
  APPEND wa_partner TO it_partner.

  wa_partner-partn_role = 'WE'.
  wa_partner-partn_numb = w_saida2-cl_codigo.
  APPEND wa_partner TO it_partner.

*-------------------------------------------
* Criar OV
*-------------------------------------------
  CALL FUNCTION 'SD_SALESDOCUMENT_CREATE'
    EXPORTING
      sales_header_in     = wl_header_in
      sales_header_inx    = wl_header_inx
    IMPORTING
      salesdocument_ex    = vbeln_ov
    TABLES
      return              = it_return
      sales_items_in      = it_itemdata
      sales_items_inx     = it_items_inx
      sales_partners      = it_partner
      sales_schedules_in  = tl_schedules_in
      sales_conditions_in = it_condition.

  READ TABLE it_return INTO wa_return WITH KEY type = 'E'.

  IF sy-subrc IS INITIAL.
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

    FREE : tg_msg_ret, wa_return, wl_msg_ret.

    LOOP AT it_return INTO wa_return WHERE type <> 'S'.
      wl_msg_ret-msg = wa_return-message.
      APPEND wl_msg_ret TO tg_msg_ret.
    ENDLOOP.

    CHECK tg_msg_ret[] IS NOT INITIAL.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = 'X'
        i_repid       = sy-repid
        i_pressed_tab = 'TABSTRIP-ACTIVETAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.

    LOOP AT t_saida2  INTO w_saida2 WHERE id_seq = l_id_seq.
      w_saida2-status    = icon_led_red.
      MODIFY t_saida2 FROM w_saida2 INDEX sy-tabix TRANSPORTING status.
      COMMIT WORK.
    ENDLOOP.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = 'X'.

    SELECT *
    FROM vbuv
    INTO TABLE tl_vbuv
    WHERE vbeln EQ vbeln_ov.

    IF sy-subrc IS INITIAL.
      LOOP AT  tl_vbuv INTO wl_vbuv.
        CLEAR: wl_fieldname, wl_text.

        wl_fieldname = wl_vbuv-fdnam.

        CALL FUNCTION 'RM_DDIC_TEXTS_GET'
          EXPORTING
            i_name                = wl_fieldname
            i_type                = 'DTEL'
            i_langu               = sy-langu
          IMPORTING
            e_ddtxt               = wl_text
          EXCEPTIONS
            objtype_not_supported = 1
            illegal_input         = 2
            OTHERS                = 3.

        IF sy-subrc <> 0.
          CONCATENATE 'Dados incompletos na O.V: ' wl_vbuv-fdnam INTO wl_msg_ret-msg SEPARATED BY space.
        ELSE.
          CONCATENATE 'Dados incompletos na O.V: ' wl_text INTO wl_msg_ret-msg SEPARATED BY space.
        ENDIF.
        APPEND wl_msg_ret TO tg_msg_ret.
      ENDLOOP.

      CHECK tg_msg_ret[] IS NOT INITIAL.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = 'X'
          i_repid       = sy-repid
          i_pressed_tab = 'TABSTRIP-ACTIVETAB'
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.

      wl_erro = 'X'.
    ENDIF.

*-------------------------------------------
*-- Elimina OV
*-------------------------------------------
    CASE wl_erro.

      WHEN 'X'.
        REFRESH it_return.
        CLEAR: wl_header_in.
        wl_header_inx2-updateflag = 'D'.

        CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            salesdocument    = vbeln_ov
            order_header_inx = wl_header_inx2
          TABLES
            return           = it_return.

        READ TABLE it_return INTO wa_return WITH KEY type = 'S'.

        IF sy-subrc = 0.
          PERFORM get_values_alv2.
          CLEAR: w_saida2.
          READ TABLE t_saida2 INTO w_saida2 INDEX lr_scell2-row.

          LOOP AT t_saida2 INTO w_saida2 WHERE id_seq = l_id_seq.
            CLEAR: w_saida2-auart,
            w_saida2-tax_dolar,
            w_saida2-vlr_usd,
            w_saida2-vlr_brl,
            w_saida2-waerk,
            w_saida2-waerk.
            w_saida2-status    = icon_led_red.
            MODIFY t_saida2 FROM w_saida2 INDEX sy-tabix TRANSPORTING status auart tax_dolar vlr_usd vlr_brl waerk.
            COMMIT WORK.
          ENDLOOP.

          gr_table2->refresh(
            s_stable = w_stable
*           refresh_mode = if_salv_c_refresh=>soft
          ).

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.

      WHEN OTHERS.
        vbeln_fatura = vbeln_ov.

        "Set utilizado por odens ZTRG criadas pela transação ZSDT0158
        IF w_saida2-auart IN r_auart_ztrg.
          t_billing = VALUE #( (  ref_doc     = vbeln_fatura
          ref_doc_ca  = 'C'
          bill_date   = w_saida2-dt_fatura  )  ). "sy-datum  )  ).

        ELSE.
          t_billing = VALUE #( ( ref_doc      = vbeln_fatura
          ref_doc_ca   = 'C'
          bill_date    = sy-datum  )  ).
        ENDIF.

*-------------------------------------------
*------ Criar fatura
*-------------------------------------------
        CALL FUNCTION 'BAPI_BILLINGDOC_CREATEMULTIPLE' "#EC CI_USAGE_OK[2438131]
          TABLES
            billingdatain = t_billing
            return        = t_return
            success       = t_success.

        IF t_success IS NOT INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          TRY.
              vbeln_fatura = t_success[ 1 ]-bill_doc.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          wl_erro =  abap_true.
        ENDIF.

        WAIT UP TO 12 SECONDS.

        CLEAR: wa_j_1bnflin.

        SELECT SINGLE *
        FROM j_1bnflin
        INTO wa_j_1bnflin
        WHERE refkey EQ vbeln_fatura.

        IF sy-subrc EQ 0.
          w_saida2-nr_ov = vbeln_ov.
          w_saida2-docnum_nf = wa_j_1bnflin-docnum.
          w_saida2-fatura = vbeln_fatura.

          MODIFY zsdt0306_fat FROM w_saida2 .
          COMMIT WORK.
          IF sy-subrc <> 0.
          ENDIF.

          DATA: dtnow TYPE sy-datum.
          DATA: hrnow TYPE sy-uzeit.
          DATA: data_reg TYPE timestamp.
          DATA: usr_reg TYPE sy-uname.
          dtnow = sy-datum.
          hrnow = sy-uzeit.
          data_reg = CONV timestamp( dtnow && hrnow ).
          usr_reg = sy-uname.

          DATA(aux_docnum) = w_saida2-docnum_nf.
          CONDENSE aux_docnum NO-GAPS.
          CLEAR:w_0225.
          w_0225-mandt          = sy-mandt.
          w_0225-id_seq         = w_saida2-id_seq.
          w_0225-lote           = abap_false.
          w_0225-bukrs          = w_saida2-bukrs_ped.
          w_0225-werks          = w_saida2-werks_ped.
          w_0225-operacao       = w_saida2-operacao.
          w_0225-ano_viagem     = w_saida2-dt_fatura+0(4).
          w_0225-cl_codigo      = w_saida2-cl_codigo.
          w_0225-safra          = w_saida1-safra. "abap_false.  "*-IR194132-29.10.2024-#156085-JT
          w_0225-cod_material   = w_saida2-matnr.
          w_0225-dt_recreg      = w_saida2-dt_recreg.
          w_0225-tp_class       = abap_false.
          w_0225-nr_dco         = abap_false.
          w_0225-po_embarque    = abap_false.
          w_0225-po_destino     = abap_false.
          w_0225-dt_fatura      = w_saida2-dt_fatura.
          w_0225-auart          = w_saida2-auart.
          w_0225-bukrs_serv     = w_saida2-bukrs_fat.
          w_0225-werks_serv     = w_saida2-werks_fat.
          w_0225-waerk          = w_saida2-waerk.
          w_0225-netpr          = w_saida2-netpr.
          w_0225-tax_dolar      = w_saida2-tax_dolar.
          w_0225-vlr_usd        = w_saida2-vlr_usd.
          w_0225-vlr_brl        = w_saida2-vlr_brl.
          w_0225-vkorg          = w_saida2-vkorg.
          w_0225-vtweg          = w_saida2-vtweg.
          w_0225-spart          = w_saida2-spart.
          w_0225-matnr_ov       = w_saida2-matnr_ov.
          w_0225-zterm          = w_saida2-zterm.
          w_0225-nr_ov          = w_saida2-nr_ov.
          w_0225-fatura         = w_saida2-fatura.
          w_0225-docnum         = aux_docnum.
          w_0225-waerk_fatura   = w_saida2-waerk_fatura. "#161385 / RG
          w_0225-navio          = w_saida2-navio.
          w_0225-local_operacao = w_saida2-local_operacao.
          w_0225-usuario        = usr_reg.
          w_0225-data_registro  = dtnow.
          w_0225-hora_registro  = hrnow.


          MODIFY zsdt0225    FROM w_0225.
          COMMIT WORK.
          MESSAGE 'Ordem de Venda Gerada com Sucesso!'  TYPE 'S'.
          _erro = 'X'.
          gr_table2->refresh(
            s_stable = w_stable ).
        ENDIF.
    ENDCASE.

  ENDIF.

ENDFORM.

FORM estornar.
  PERFORM get_values_alv2.
  CLEAR: w_saida2.

  IF     lr_scell2-row  = 0.
    MESSAGE 'Selecionar uma Linha!' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.
  ELSEIF linhas  > 1.
    MESSAGE 'Selecionar apenas uma Linha para Estorno!' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.
  ELSEIF linhas  = 1.

    READ TABLE t_saida2 INTO w_saida2 INDEX lr_scell2-row.

    IF w_saida2-id_seq IS NOT INITIAL.

      SELECT SINGLE doc_znfw FROM zsdt0225
        WHERE id_seq = @w_saida2-id_seq
        AND dt_recreg = @w_saida2-dt_recreg
        INTO @DATA(aux_doc_znfw).

      IF  aux_doc_znfw IS NOT INITIAL.
        MESSAGE 'Nota fiscal registrada na empresa tomadora do serviço, solicite estorno!' TYPE 'E' DISPLAY LIKE 'I'.
        _erro = 'X'.
        EXIT.
      ENDIF.

      IF w_saida2-fatura IS INITIAL OR
         w_saida2-nr_ov  IS INITIAL OR
         w_saida2-docnum_nf IS INITIAL.

        MESSAGE 'Não existe faturamento a estornar. Pode apenas eliminar a linha' TYPE 'E' DISPLAY LIKE 'I'.
        _erro = 'X'.
        EXIT.

      ENDIF.



      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
        EXPORTING
          percentage = 50
          text       = 'Cancelando Fatura...'.

      DATA : t_success    TYPE TABLE OF bapivbrksuccess,
             t_return     TYPE TABLE OF bapireturn1,
             w_erro       TYPE char1,
             estornado    TYPE c,
             is_cancelled TYPE bapivbrkout,
             vdocnum_est  TYPE j_1bdocnum,
             w_header_in  TYPE bapisdh1,
             w_header_inx TYPE bapisdh1x,
             vl_bill_doc  TYPE bapivbrksuccess-bill_doc.


      FREE: t_return, t_success, w_erro, estornado.
*
**---------------------------------
**- validacoes
**---------------------------------
*
      l_id_seq = w_saida2-id_seq.
      w_erro   = abap_false.


      vl_bill_doc = CONV #( w_saida2-fatura ).

      CALL FUNCTION 'BAPI_BILLINGDOC_GETDETAIL'
        EXPORTING
          billingdocument       = vl_bill_doc
        IMPORTING
          billingdocumentdetail = is_cancelled
*         RETURN                =
        .


      IF is_cancelled-cancelled IS INITIAL.
        CALL FUNCTION 'ZBAPI_BILLINGDOC_CANCEL1'
          EXPORTING
            billingdocument = w_saida2-fatura
          TABLES
            return          = t_return
            success         = t_success.
      ENDIF.

      IF t_success[] IS NOT INITIAL OR  is_cancelled IS NOT INITIAL.

        SELECT SINGLE docnum
        FROM j_1bnflin
        INTO @DATA(_docnum)
              WHERE docnum EQ @w_saida2-docnum_nf.

        SELECT SINGLE candat
        FROM j_1bnfdoc
        INTO @DATA(_vcandat)
              WHERE docnum EQ @_docnum.

        IF _vcandat IS INITIAL.
          SELECT SINGLE docnum
          FROM j_1bnfe_active
          INTO @DATA(v_docnum)
                WHERE docnum EQ @_docnum
                AND docsta EQ '1'
                AND cancel EQ ''.

          IF sy-subrc NE 0.
            CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
              EXPORTING
                doc_number               = _docnum
                ref_type                 = space
                ref_key                  = space
                can_dat                  = sy-datum
              IMPORTING
                doc_number               = vdocnum_est
              EXCEPTIONS
                document_not_found       = 1
                cancel_not_possible      = 2
                nf_cancel_type_not_found = 3
                database_problem         = 4
                docum_lock               = 5
                nfe_cancel_simulation    = 6
                OTHERS                   = 7.

            IF sy-subrc EQ 0.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = abap_true.
            ELSE.
              w_erro = abap_true.
            ENDIF.
          ELSE.
            w_erro = abap_true.
          ENDIF.

          IF w_erro IS NOT INITIAL.
            APPEND VALUE #( msg = |Impossível estorno de fatura { w_saida2-fatura }. Danfe, não estornada| ) TO tg_msg_ret.
          ENDIF.
        ENDIF.
      ELSE.
        w_erro = abap_true.
      ENDIF.

      WAIT UP TO 2 SECONDS.

      DELETE t_return WHERE type NE 'E' .

      IF t_return[] IS INITIAL AND w_erro IS NOT INITIAL.

        LOOP AT t_return INTO DATA(wa).
          SELECT *
          FROM t100
          INTO TABLE @DATA(it_t100)
                WHERE sprsl EQ 'PT'
                AND arbgb EQ @wa-id
                AND msgnr EQ @wa-number.

          CHECK it_t100 IS NOT  INITIAL.

          LOOP AT it_t100 INTO DATA(wa_t100).
            CONCATENATE w_saida2-fatura '-' wa_t100-text INTO wl_msg_ret-msg SEPARATED BY space.
            APPEND wl_msg_ret TO tg_msg_ret.
          ENDLOOP.

          IF NOT ( tg_msg_ret[] IS INITIAL ).
            CALL FUNCTION 'Z_DOC_CHECK_NEW'
              EXPORTING
                i_screen      = '100'
                i_show        = 'X'
                i_repid       = sy-repid
                i_pressed_tab = 'TABSTRIP-ACTIVETAB'
                i_set_field   = 'X_FIELD'
              IMPORTING
                e_messagem    = wg_mensagem
              TABLES
                it_msgs       = tg_msg_ret.
          ELSE.
            estornado = abap_true.
          ENDIF.
        ENDLOOP.
      ELSE.
        estornado = abap_true.
      ENDIF.

      CASE estornado.
        WHEN abap_true.
          FREE: w_header_in, w_header_inx, it_return.

*----------------------------
*---- bloquear OV
*----------------------------
          w_header_in-bill_block  = '03'.
          w_header_inx-bill_block = abap_true.
          w_header_inx-updateflag = 'U'.

          CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              salesdocument    = w_saida2-nr_ov
              order_header_in  = w_header_in
              order_header_inx = w_header_inx
            TABLES
              return           = it_return.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

      ENDCASE.

      IF estornado = abap_true.
        DELETE FROM zsdt0225 WHERE id_seq = w_saida2-id_seq AND dt_recreg = w_saida2-dt_recreg.
* INICIO IR220071 - GGARAUJO1 - STEFANINI
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
* FIM IR220071 - GGARAUJO1 - STEFANINI
        DELETE FROM zsdt0306_fat WHERE id_seq = w_saida2-id_seq AND dt_recreg = w_saida2-dt_recreg.
* INICIO IR220071 - GGARAUJO1 - STEFANINI
        IF sy-subrc IS INITIAL.
          COMMIT WORK.
        ENDIF.
* FIM IR220071 - GGARAUJO1 - STEFANINI
        MESSAGE 'Documentos foram Estornados!' TYPE 'E' DISPLAY LIKE 'I'.
        _erro = 'X'.
      ELSE.
        MESSAGE 'Não há documentos a Estornar!' TYPE 'E' DISPLAY LIKE 'I'.
        _erro = 'X'.
      ENDIF.
    ELSE.
      MESSAGE 'errrrroooooo!' TYPE 'E' DISPLAY LIKE 'I'.
      _erro = 'X'.
    ENDIF.

  ENDIF.
ENDFORM.

FORM f_get_set.

  DATA: valor         TYPE p DECIMALS 2,
        text_out(255) TYPE c.

  FREE: r_matkl_sel.

  SELECT *
  FROM tvarvc
  INTO TABLE @DATA(t_tvarvc)
        WHERE name = 'MAGGI_GR_FERTILIZANTES'.

  LOOP AT t_tvarvc  INTO DATA(w_tvarvc).
    r_matkl_sel-sign   = w_tvarvc-sign.
    r_matkl_sel-option = w_tvarvc-opti.
    r_matkl_sel-low    = w_tvarvc-low.
    APPEND r_matkl_sel.
  ENDLOOP.


  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'ZSDR0108_DT_FATURA'
      class         = '0000'
    TABLES
      set_values    = it_value
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  CHECK it_value IS NOT INITIAL.

  CLEAR r_dt_fatura.

  LOOP AT it_value.
    r_dt_fatura-sign   = 'I'.
    r_dt_fatura-option = 'GE'.
    r_dt_fatura-low    = it_value-from.
    APPEND r_dt_fatura.
  ENDLOOP.

  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr         = 'MAGGI_ZSDT0158_VLR'
      class         = '0000'
    TABLES
      set_values    = it_value_01
    EXCEPTIONS
      set_not_found = 1
      OTHERS        = 2.

  CHECK it_value_01 IS NOT INITIAL.

  CLEAR: r_vlr_brl, v_vlr_brl.

  LOOP AT it_value_01.
    r_vlr_brl-sign    = 'I'.
    r_vlr_brl-option  = 'EQ'.

    text_out = it_value_01-from.
    DO 5  TIMES.
      REPLACE  ',00' WITH ' ' INTO text_out.
    ENDDO.

    CONDENSE text_out NO-GAPS.
    MOVE text_out TO valor.
    r_vlr_brl-low = valor.
    v_vlr_brl = r_vlr_brl-low.
    CONDENSE v_vlr_brl NO-GAPS.
    APPEND r_vlr_brl.
  ENDLOOP.

ENDFORM.

FORM popup_insert_row .
  DATA:_qtdtotal TYPE p DECIMALS 3.
  DATA:_qtdpedid TYPE p DECIMALS 3.
  DATA:_qtdfatur TYPE p DECIMALS 3.
  DATA:_qtdsobra TYPE p DECIMALS 3.
  CLEAR: isval, isval[],_qtdtotal,w_saida1,w_saida2.
  PERFORM resultado_total_alv1.

  IF t_saida2[] IS NOT INITIAL.

    DESCRIBE TABLE t_saida2 LINES DATA(max_lines).

    READ TABLE t_saida2 INTO DATA(get_max_row) INDEX max_lines.
    _qtdtotal = get_max_row-qtdsaldo.

    IF get_max_row-nr_ov IS INITIAL.
      MESSAGE 'Existem faturamentos em aberto!' TYPE 'E' DISPLAY LIKE 'I'.
      _erro = 'X'.
      EXIT.
    ENDIF.

  ELSE.

  ENDIF.

  IF w_saida1-qtdpedid <= 0.

    MESSAGE 'Não existe Saldo para Lançamento' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.

  ELSE.

    IF w_saida1-qtdpedid > 0.
      _qtdpedid = w_saida1-qtdpedid.
    ENDIF.

    IF w_saida2-qtdfatur > 0.
      _qtdfatur = w_saida2-qtdfatur.
    ENDIF.
    IF w_saida2-qtdsobra > 0.
      _qtdsobra = w_saida2-qtdsobra.
    ENDIF.

  ENDIF.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'QTDPEDID'.
  isval-fieldtext = 'Qtd Saldo:'.
  isval-value = _qtdpedid.
  APPEND isval TO isval.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'QTDFATUR'.
  isval-fieldtext = 'Qtd Faturada:'.
  isval-field_obl = 'X'..
  isval-value = _qtdfatur.
  APPEND isval TO isval.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'QTDSOBRA'.
  isval-fieldtext = 'Qtd Sobra:'.
  isval-field_obl = ' '.
  isval-value = _qtdsobra.
  APPEND isval TO isval.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'NAVIO'.
  isval-fieldtext = 'Navio:'.
  isval-field_obl = 'X'.
  isval-value = w_saida2-navio.
  APPEND isval TO isval.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'LOCAL_OPERACAO'.
  isval-fieldtext = 'Local Operação:'.
  isval-field_obl = 'X'.
  isval-value = w_saida2-local_operacao.
  APPEND isval TO isval.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'DT_FATURA'.
  isval-fieldtext = 'Data:'.
  isval-field_obl = 'X'.
  isval-value = w_saida2-dt_fatura.
  APPEND isval TO isval.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'AUART'.
  isval-fieldtext = 'Tipo:'.
  isval-field_obl = 'X'.
  isval-value = w_saida2-auart.
  APPEND isval TO isval.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'WAERK'.
  isval-fieldtext = 'Moeda:'.
  isval-field_obl = 'X'.
  isval-value = w_saida2-waerk.
  APPEND isval TO isval.

  isval-tabname   = 'ZSDT0306_FAT'.
  isval-fieldname = 'OPERACAO'.
  isval-fieldtext = 'Operação:'.
  isval-field_obl = 'X'.
  isval-value = w_saida2-operacao.
  APPEND isval TO isval.

  LOOP AT isval ASSIGNING FIELD-SYMBOL(<isval>)  .

    IF <isval>-fieldname = 'QTDPEDID' OR <isval>-fieldname = 'QTDFATUR' OR <isval>-fieldname = 'QTDSOBRA' OR <isval>-fieldname = 'DT_FATURA'.
      CONDENSE <isval>-value NO-GAPS.
      IF <isval>-value = '0.000' OR <isval>-value = '00000000'.
        CLEAR:<isval>-value.
      ENDIF.
    ENDIF.


    IF <isval>-fieldname = 'QTDPEDID'.
      <isval>-field_attr = '03'.
    ELSE.
      <isval>-field_attr = '00'.
    ENDIF.
  ENDLOOP.

  icon_name =  '@8W@'.
  repid = sy-repid.

  CALL FUNCTION 'POPUP_GET_VALUES_USER_BUTTONS'
    EXPORTING
*     F1_FORMNAME       = ' '
*     F1_PROGRAMNAME    = ' '
*     F4_FORMNAME       = ' '
*     F4_PROGRAMNAME    = ' '
      formname          = 'HANDLE_PUSH_BUTTON'
      programname       = repid
      popup_title       = 'Registro de Saidas'
      ok_pushbuttontext = 'Preparar Faturamento'
      icon_ok_push      = icon_name
*     QUICKINFO_OK_PUSH = ' '
*     FIRST_PUSHBUTTON  = ' '
*     ICON_BUTTON_1     =
*     QUICKINFO_BUTTON_1              = ' '
*     SECOND_PUSHBUTTON = ' '
*     ICON_BUTTON_2     =
*     QUICKINFO_BUTTON_2              = ' '
*     START_COLUMN      = '5'
*     START_ROW         = '5'
*     NO_CHECK_FOR_FIXED_VALUES       = ' '
*    IMPORTING
*     returncode        = lr_RETURNCODE
    TABLES
      fields            = isval
* EXCEPTIONS
*     ERROR_IN_FIELDS   = 1
*     OTHERS            = 2
    .
  IF sy-subrc = 0 .

    IF sy-ucomm = 'CANC'.
      CLEAR: t_saida2[],w_saida2.
    ELSEIF sy-ucomm = 'FURT'.
      PERFORM valida.
      IF _erro <> 'X'.

        PERFORM prepara.

      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

FORM handle_push_button TABLES fields STRUCTURE sval
USING ok_code
CHANGING error STRUCTURE svale
  show_popup.

* Do something.
  READ TABLE fields INDEX 1.
  SHIFT fields-value LEFT DELETING LEADING space.


  IF  fields[] IS NOT INITIAL.

    FREE: t_saida2.
    PERFORM resultado_total_alv1.

    APPEND INITIAL LINE TO t_saida2.
    LOOP AT t_saida2 ASSIGNING FIELD-SYMBOL(<_complete_saida2>).

    ENDLOOP.

    LOOP AT fields[] ASSIGNING FIELD-SYMBOL(<_move>).
      IF <_move>-value IS NOT INITIAL.
        CASE <_move>-fieldname.
          WHEN 'QTDPEDID'.
            CLEAR: _value,new_value.
            _value = <_move>-value.
            CONDENSE _value NO-GAPS.
            new_value = _value.
            w_saida2-qtdpedid = new_value.
          WHEN 'QTDFATUR'.
            CLEAR: _value,new_value.
            _value = <_move>-value.
            CONDENSE _value NO-GAPS.
            new_value = _value.
            w_saida2-qtdfatur = new_value.

            IF w_saida2-qtdfatur <= 0.
              MESSAGE 'Quantidade Faturada Deve ser maior que 0!' TYPE 'E' DISPLAY LIKE 'I'.
              _erro = 'X'.
              EXIT.
            ENDIF.
          WHEN 'QTDSOBRA'.
            CLEAR: _value,new_value.
            _value = <_move>-value.
            CONDENSE _value NO-GAPS.
            new_value = _value.
            w_saida2-qtdsobra = new_value.
          WHEN 'NAVIO'.
            w_saida2-navio = <_move>-value.
          WHEN 'LOCAL_OPERACAO'.
            w_saida2-local_operacao = <_move>-value.
          WHEN 'DT_FATURA'.
            w_saida2-dt_fatura = <_move>-value.
          WHEN 'AUART'.
            w_saida2-auart = <_move>-value.
          WHEN 'WAERK'.
            w_saida2-waerk = <_move>-value.
          WHEN 'OPERACAO'.
            w_saida2-operacao = <_move>-value.
        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.


FORM valida.

  DATA: qtd_total  TYPE zsdt0306_fat-qtdfatur.

  IF w_saida2-qtdpedid IS NOT INITIAL.
    CLEAR:qtd_total,_erro.
    qtd_total = ( ( w_saida2-qtdpedid + ( w_saida2-qtdfatur ) * -1 ) + w_saida2-qtdsobra ).
    DATA(msg_qtd_erro) = |Quantidade Faturada excede O Total!|.
    IF qtd_total < 0.
      MESSAGE msg_qtd_erro TYPE 'E' DISPLAY LIKE 'I'.
      PERFORM popup_insert_row.
      _erro = 'X'.
    ELSE.

      CLEAR:w_saida2-qtdsaldo.
      w_saida2-qtdsaldo = ( ( w_saida2-qtdpedid - w_saida2-qtdfatur ) + w_saida2-qtdsobra ).
      IF w_saida2-qtdsaldo < 0.
        MESSAGE msg_qtd_erro TYPE 'E' DISPLAY LIKE 'I'.
        PERFORM popup_insert_row.
        _erro = 'X'.
      ENDIF.
    ENDIF.
  ELSE.

    MESSAGE 'Não existe Saldo para Lançamento' TYPE 'E' DISPLAY LIKE 'I'.
    _erro = 'X'.
    EXIT.

  ENDIF.


ENDFORM.

FORM prepara.

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.

  DATA: w_zlest0055 TYPE zlest0055.
  DATA: aux_matkl TYPE zlest0055-matkl.

  READ TABLE t_zsdt0306 INTO DATA(w_zsdt0306) INDEX 1.

  CLEAR: aux_matkl,w_zlest0055.

  w_saida2-id_seq = w_saida1-id_seq.
  w_saida2-bukrs_fat = w_saida1-bukrs_fat.
  w_saida2-werks_fat = w_saida1-werks_fat.
  w_saida2-qtdpedid = w_saida1-qtdpedid.

  CONDENSE w_saida1-werks NO-GAPS.
  UNPACK  w_saida1-werks TO w_saida2-cl_codigo.

  CONDENSE w_saida1-matkl NO-GAPS.
  UNPACK w_saida1-matkl TO aux_matkl.

  w_saida2-matnr = w_saida1-matnr.
  w_saida2-matkl = w_saida1-matkl.
  w_saida2-bukrs_ped = w_saida1-bukrs.
  w_saida2-werks_ped = w_saida1-werks.

  SELECT SINGLE * FROM zlest0055
  WHERE 1 = 1
  AND status = '1'
  AND operacao = @w_saida2-operacao
  AND vkorg = @w_saida2-bukrs_fat
  AND waerk = @w_saida2-waerk
  AND kunnr = @w_saida2-cl_codigo
  AND matkl = @aux_matkl
  AND auart = @w_saida2-auart
  INTO @w_zlest0055.

  IF w_zlest0055-kunnr IS INITIAL.

    SELECT SINGLE * FROM kna1 INTO @DATA(wl_kna1) WHERE kunnr EQ @w_saida2-cl_codigo.

    DATA(l_kunnr_ped) = wl_kna1-kunnr.

    CONCATENATE wl_kna1-stcd1(8) '%' INTO l_stcd_str.

    SELECT *
      FROM kna1
      INTO TABLE @DATA(tl_kna1)
     WHERE stcd1 LIKE @l_stcd_str.

    CHECK NOT tl_kna1[] IS INITIAL.

    SORT tl_kna1[] BY stcd1 DESCENDING.
    DELETE tl_kna1[] WHERE stcd1+8(4) <> '0001'.

    READ TABLE tl_kna1[] INTO DATA(wkna1) INDEX 1.


**********************************************************************

    IF w_saida2-bukrs_fat IS INITIAL.
      SELECT *
        FROM zsdt0307
        INTO TABLE @DATA(t_0307)
       WHERE emp_pedido  EQ @w_saida2-bukrs_ped.
    ELSE.
      SELECT *
        FROM zsdt0307
        INTO TABLE t_0307
       WHERE emp_pedido   EQ w_saida2-bukrs_ped
         AND emp_fat_serv EQ w_saida2-bukrs_fat.
    ENDIF.

    IF sy-subrc <> 0.
      FREE: t_0307.
    ENDIF.

    IF lines( t_0307 ) > 1.
      FREE: t_lista.
      LOOP AT t_0307 INTO DATA(w_0307).
        DATA(l_linha) = |{ w_0307-emp_pedido } / { w_0307-emp_fat_serv } / { w_0307-centro_fat_serv }|.
        APPEND VALUE #( selflag = abap_off varoption = l_linha inactive = abap_off ) TO t_lista.
      ENDLOOP.

      l_texto1 = 'Há mais de uma Empresa Fat.Servico'.
      l_texto2 = 'para a Empresa Pedido:' && w_saida2-bukrs_ped.
      CALL FUNCTION 'Z_POPUP_TO_DECIDE_LIST'
        EXPORTING
          cursorline  = 1
          start_col   = 70
          start_row   = 05
          show_row    = 12
          textline1   = l_texto1
          textline2   = l_texto2
          titel       = 'Fatura Serviço - Parametros Pedido'
          coltitle    = 'Empr.Pedido / Empr.Fat.Serv / Centro Fat.Serv'
        IMPORTING
          answer      = l_resp
        TABLES
          im_ex_liste = t_lista
        EXCEPTIONS
          no_entries  = 1
          OTHERS      = 2.

      IF l_resp <> 'S'.
        FREE t_0307.
      ELSE.
        READ TABLE t_lista INTO DATA(w_lista) WITH KEY selflag = abap_true.
        DELETE t_0307 WHERE emp_fat_serv <> w_lista-varoption+7(4).
      ENDIF.
    ELSEIF lines( t_0307 ) = 1.
    ENDIF.


    IF t_0307[] IS INITIAL.
      l_erro        = abap_true.
      MESSAGE 'Empresa Fatura Serviço não encontrado,Favor realize o parametro na transação ZSDT0158 !'  TYPE 'E' DISPLAY LIKE 'I'.
      _erro = 'X'.
      EXIT.
    ENDIF.

    READ TABLE t_0307 INTO w_0307 INDEX 1.

    CLEAR w_zlest0055.

    FREE: r_matkl.
    t_matkl-sign   = 'I'.
    t_matkl-option = 'CP'.
    t_matkl-low    = '*' && w_saida2-matkl && '*'.
    APPEND t_matkl TO r_matkl.

    SELECT SINGLE *
      FROM zlest0055
      INTO w_zlest0055
     WHERE kunnr   EQ l_kunnr_ped
       AND auart   EQ w_saida2-auart
       AND matkl   IN r_matkl
       AND dt_fim  GE w_saida2-dt_fatura
       AND waerk   EQ w_saida2-waerk
       AND status  EQ '1'
       AND vkorg   EQ w_0307-emp_fat_serv
       AND operacao = w_saida2-operacao.

    IF sy-subrc <> 0.
      SELECT SINGLE *
        FROM zlest0055
        INTO w_zlest0055
       WHERE kunnr   EQ wkna1-kunnr
         AND auart   EQ w_saida2-auart
         AND matkl   IN r_matkl
         AND dt_fim  GE w_saida2-dt_fatura
         AND waerk   EQ w_saida2-waerk
         AND status  EQ '1'
         AND vkorg   EQ w_0307-emp_fat_serv
         AND operacao = w_saida2-operacao.
    ENDIF.
    IF sy-subrc <> 0.
      DATA: it_msg_erro TYPE STANDARD TABLE OF tline INITIAL SIZE 0.
      FREE: it_msg_erro[].
      DATA l_single_gpmat TYPE txt20.
      CLEAR:l_single_gpmat.
      CONCATENATE LINES OF r_matkl INTO l_single_gpmat SEPARATED BY ','.
      DATA(l1) = |Tarifa não encontrada na transação ZLES0075!|.
      DATA(l2) = |Cliente: { wkna1-kunnr }|.
      DATA(l3) = |Tipo Ov: { w_saida2-auart }|.
      DATA(l4) = |MoedaFat: { w_saida2-waerk }|.
      DATA(l5) = |DtFat: { w_saida2-dt_fatura }|.
      DATA(l6) = |Oper: { w_saida2-operacao }|.
      DATA(l7) = |EmpFat: { w_0307-emp_fat_serv }|.
      DATA(l8) = |GpMat: { l_single_gpmat }|.
      DATA l_message01(255) TYPE c.                                          "*-166474-19.02.2025-JT-inicio
      CONCATENATE l1 l2 l3 l4 l5 l6 l7 l8 INTO l_message01 SEPARATED BY '|'. "','. "*-166474-19.02.2025-JT-inicio
      APPEND VALUE #( message = l_message01 status = icon_led_red ) TO it_err[].
      PERFORM popup_error. "*-166474-19.02.2025-JT-inicio
    ENDIF.

**********************************************************************

  ENDIF.

  IF w_zlest0055 IS NOT INITIAL.

    SELECT SINGLE * FROM mara WHERE matnr = @w_saida2-matnr INTO @DATA(lr_mara) .
    SELECT SINGLE * FROM zsdt0307 WHERE emp_pedido   = @w_saida1-bukrs AND emp_fat_serv = @w_saida2-bukrs_fat INTO @DATA(lr_zsdt0307).

    w_saida2-menge = w_saida2-qtdfatur.
    w_saida2-kunnr = w_zlest0055-kunnr.
    w_saida2-gewei = lr_mara-gewei.
    w_saida2-meins = lr_mara-meins.
    w_saida2-vkaus = w_zlest0055-vkaus.
    w_saida2-vtweg = w_zlest0055-vtweg.
    w_saida2-spart = w_zlest0055-spart.
    w_saida2-zterm = w_zlest0055-zterm.
    w_saida2-netpr = w_zlest0055-netpr.
    w_saida2-kurst = w_zlest0055-kurst.
    w_saida2-centro_fat_serv = lr_zsdt0307-centro_fat_serv.
    w_saida2-pedidos = w_saida2-pedidos && w_zsdt0306-ebeln && '/' && w_zsdt0306-ebelp && '-'.

    CASE  w_saida2-waerk.
      WHEN 'BRL'.
        CLEAR: l_vlr_brl,l_vlr_brl.
        l_vlr_brl = ( ( w_saida2-qtdfatur / 1000 ) *  w_saida2-netpr  ).
        l_gdatu   =  w_saida2-dt_fatura.

        obj_zcl_util_sd->set_data(    EXPORTING i_data  = l_gdatu ).
        obj_zcl_util_sd->set_kurst(   EXPORTING i_kurst = w_saida2-kurst ).
        obj_zcl_util_sd->set_waerk(   EXPORTING i_waerk = w_saida2-waerk ).
        obj_zcl_util_sd->set_tcurr(   EXPORTING i_tcurr = 'USD' ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = l_ukurs ).

        IF l_ukurs IS NOT INITIAL.
          w_saida2-tax_dolar  = ( l_ukurs * -1 ).
          l_vlr_usd           = ( l_vlr_brl /  ( l_ukurs * -1 ) ).
          w_saida2-vlr_brl    = l_vlr_brl.
          w_saida2-vlr_usd    = l_vlr_usd.
        ELSE.
          MESSAGE 'Taxa do câmbio não cadastrada.' TYPE 'E' DISPLAY LIKE 'I'.
          _erro = 'X'.
          EXIT.
        ENDIF.

      WHEN 'USD'.
        CLEAR: l_vlr_brl,l_vlr_brl.
        l_vlr_usd = ( ( w_saida2-qtdfatur / 1000 ) *  w_zlest0055-netpr  ).
        l_gdatu   =  w_saida2-dt_fatura.

        obj_zcl_util_sd->set_data(    EXPORTING i_data  = l_gdatu ).
        obj_zcl_util_sd->set_kurst(   EXPORTING i_kurst = w_saida2-kurst ).
        obj_zcl_util_sd->set_waerk(   EXPORTING i_waerk = w_saida2-waerk ).
        obj_zcl_util_sd->set_tcurr(   EXPORTING i_tcurr = 'BRL' ).
        obj_zcl_util_sd->taxa_cambio( RECEIVING e_ukurs = l_ukurs ).

        IF l_ukurs IS NOT INITIAL.
          l_vlr_brl           = l_vlr_usd * l_ukurs.
          w_saida2-tax_dolar  = l_ukurs.
          w_saida2-vlr_brl    = l_vlr_brl.
          w_saida2-vlr_usd    = l_vlr_usd.
        ELSE.
          MESSAGE 'Taxa do câmbio não cadastrada.'TYPE 'E' DISPLAY LIKE 'I'.
          _erro = 'X'.
          EXIT.
        ENDIF.
        CLEAR: l_vlr_brl,l_vlr_brl.
    ENDCASE.

    DATA: dtnow TYPE sy-datum.
    DATA: hrnow TYPE sy-uzeit.
    DATA: data_reg TYPE timestamp.
    DATA: usr_reg TYPE sy-uname.
    dtnow = sy-datum.
    hrnow = sy-uzeit.
    data_reg = CONV timestamp( dtnow && hrnow ).
    usr_reg = sy-uname.

    w_saida2-usuario = usr_reg.
    w_saida2-hora_registro = hrnow.
    w_saida2-data_registro = dtnow.
    w_saida2-dt_recreg = data_reg.

    MODIFY zsdt0306_fat FROM w_saida2 .
    COMMIT WORK.
    IF sy-subrc <> 0.
    ENDIF.

  ENDIF.

  IF w_saida1-matnr IS NOT INITIAL."147507 Correção de erro ao criar fatura na ZSDT0158 PSA
    SELECT SINGLE *
FROM zsdt0307
INTO w_zsdt0307
WHERE emp_pedido   = w_saida2-bukrs_ped
AND emp_fat_serv = w_saida2-bukrs_fat.

    IF sy-subrc = 0.
      wl_lifnr = |{ w_zsdt0307-centro_fat_serv ALPHA = IN }|.
      SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1) WHERE lifnr EQ @wl_lifnr.
      SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1) WHERE kunnr EQ @w_saida2-cl_codigo.

      SELECT SINGLE *
  FROM j_1btxiss
  INTO @DATA(wa_j_1btxiss)
      WHERE country    EQ 'BR'
      AND gruop      EQ '73'
      AND taxjurcode EQ @wa_kna1-txjcd  "( domicílio fiscal do cliente da OV ) - SP 3522307
      AND value      EQ @wa_lfa1-txjcd  "( domicilio fiscal do centro emissor do serviço )  - AM 1301902
      AND validto    <= @w_saida2-dt_fatura    "( data final  da  vigencia)
      AND validfrom  >= @w_saida2-dt_fatura.  "(  data início de vigência)

      IF sy-subrc <> 0.
        DATA(l0) = |Mat: Alíquota do ISS  não parametrizada { wa_kna1-txjcd } - { wa_lfa1-txjcd } . Abrir uma FI|.
        "APPEND VALUE #( message = l0 status = icon_led_red ) TO it_err[].
        MESSAGE l0 TYPE 'I'.
        _erro = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

*  IF it_err[] IS NOT INITIAL.
*    PERFORM popup_error.
*    FREE: it_err.
*ENDIF.

ENDFORM.

FORM uuid.
  CLEAR: lv_guid.
* Generate a new UUID
  CALL FUNCTION 'GUID_CREATE'
    IMPORTING
      ev_guid_16 = lv_guid.
ENDFORM.

FORM popup_error .
  DATA: go_alv  TYPE REF TO cl_salv_table,
        t_mesg  TYPE TABLE OF string,
        w_erros TYPE ty_err,
        t_erros TYPE STANDARD TABLE OF ty_err INITIAL SIZE 0.

*-166474-19.02.2025-JT-inicio
  READ TABLE it_err INTO wa_err_saida INDEX 1.
  SPLIT wa_err_saida-message AT '|' INTO TABLE t_mesg.

  LOOP AT t_mesg   INTO DATA(w_mesg).
    w_erros-status    = icon_led_red.
    w_erros-message   = w_mesg.
    APPEND w_erros   TO t_erros.
  ENDLOOP.
*-166474-19.02.2025-JT-fim

  TRY.
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table      = t_erros ). "it_err[] ). "*-166474-19.02.2025-JT-inicio
    CATCH cx_salv_msg.

  ENDTRY.

  "***MOSTRA
  DATA(go_column) = go_alv->get_columns( )->get_column( 'STATUS' ).
  go_column->set_short_text( 'Status' ).
  go_column->set_medium_text( 'Status' ).
  go_column->set_long_text( 'Status' ).
  go_column->set_optimized( abap_false ).
  go_column->set_alignment( if_salv_c_alignment=>centered ).
  go_column->set_output_length( '6' ).

  go_column = go_alv->get_columns( )->get_column( 'MESSAGE' ).
  go_column->set_short_text( 'MSG' ).
  go_column->set_medium_text( 'Menssagem' ).
  go_column->set_long_text( 'Menssagem' ).
  go_column->set_optimized( abap_false ).
  go_column->set_alignment( if_salv_c_alignment=>left ).
  go_column->set_output_length( '150' ).

  DATA go_title              TYPE lvc_title.
  go_title = |Erros!|.
  DATA(go_display_settings) = go_alv->get_display_settings( ).
  go_display_settings->set_list_header_size( '10' ). " 0=l, 1=s, 2=m
  go_display_settings->set_list_header( go_title ).

  go_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
  go_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
  go_display_settings->set_list_header( go_title ).

*-166474-19.02.2025-JT-inicio
  DATA(ltot) = lines( it_err[] ).
* DATA(ltot) = lines( t_erros[] ).
*-166474-19.02.2025-JT-fim

  DATA total TYPE i.

  total = ltot + 10.

*-166474-19.02.2025-JT-inicio
*  go_alv->set_screen_popup(
*    start_column = 1
*    end_column   = 100
*    start_line   = 1
*    end_line     = total ).
  go_alv->set_screen_popup(
    start_column = 50
    start_line   = 05
    end_column   = 130
    end_line     = 15 ).
*-166474-19.02.2025-JT-fim

  go_alv->display( ).

ENDFORM.

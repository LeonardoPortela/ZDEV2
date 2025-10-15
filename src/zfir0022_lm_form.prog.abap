*&---------------------------------------------------------------------*
*& Include          ZFIR0022_LM_FORM
*&---------------------------------------------------------------------*

FORM make_container_lm. " CONTAINER ALTERAÇÃO EM MASSA
  CREATE OBJECT container_main
    EXPORTING
      container_name = 'MAIN_CONT'
      lifetime       = container_main->lifetime_dynpro.

* Cria Splitter Container
  CREATE OBJECT painel_control
    EXPORTING
      parent  = container_main
      rows    = 1
      columns = 1
      align   = 15.

* Exibe Painel 1
  CALL METHOD painel_control->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = painel1.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = painel1
          container_name = 'MAIN_CONT'
        IMPORTING
          r_salv_table   = gr_table
        CHANGING
          t_table        = it_lanc_mass[] ).
    CATCH cx_salv_msg .                                 "#EC NO_HANDLER
  ENDTRY.

  " ... §3.1 activate ALV generic Functions
  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( abap_true ). " Abap_true libera todos os campos  Toolbar

  " ... §3.2 include own functions
  DATA l_text_save TYPE string.
  DATA l_icon_save TYPE string.
  DATA l_icon_calc TYPE string.
  DATA l_text_calc TYPE string.
  DATA l_icon_atu TYPE string.
  DATA l_text_atu TYPE string.
  l_text_save = 'Gravar'.
  l_icon_save = icon_system_save.
  l_text_calc = 'Calcular'.
  l_icon_calc = icon_refresh.
  l_text_atu = 'Atualizar'.
  l_icon_atu = icon_refresh.
  TRY.

      lr_functions->add_function( name     = 'SALVAR_MASSA'
                                  icon     = l_icon_save
                                  text     = l_text_save
                                  tooltip  = 'Salvar'
                                  position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function( name     = 'CALCULAR_ALV'
                                  icon     = l_icon_calc
                                  text     = l_text_calc
                                  tooltip  = 'Calcular'
                                  position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

  lr_columns = gr_table->get_columns( ).
  lr_columns_tb = gr_table->get_columns( ).
  lr_cols_aggreg = gr_table->get_aggregations( ).

  lr_columns->set_optimize( abap_false ). " Todos!

  " CONFIGURA COLUNAS - Alterar Nome Coluna (Pequeno,Médio Grande)lr_column2
  PERFORM colunasmostraesconde.
  PERFORM colunasorganiza.
  PERFORM colunasf4.
  " PERFORM colunasSoma.

*... §4 set hotspot column
  TRY.
      lr_column_tb ?= lr_columns_tb->get_column( 'VALID' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).

      lr_column_tb ?= lr_columns_tb->get_column( 'CHECK_CALC' ).
      lr_column_tb->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found  .                          "#EC NO_HANDLER
  ENDTRY.

  "... §6 RG_ATUALIZADOer to the events of cl_salv_table
  DATA lr_events TYPE REF TO cl_salv_events_table.
  lr_events = gr_table->get_event( ).

  CREATE OBJECT gr_events.

  " ... §6.1 RG_ATUALIZADOer to the event USER_COMMAND
  SET HANDLER gr_events->on_user_command  FOR lr_events.
  SET HANDLER gr_events->on_before_user_command FOR lr_events.
  SET HANDLER gr_events->on_after_user_command FOR lr_events.
  SET HANDLER gr_events->on_hotspot_click FOR lr_events.
  SET HANDLER gr_events->make_toolbar FOR ALL INSTANCES.
  SET HANDLER gr_events->on_data_changed_finished FOR ALL INSTANCES ACTIVATION 'X'.

  " ... set list title

  l_title = |Fila de Lançamento - Ordem de Venda { ordemvenda }|.
  lr_display_settings = gr_table->get_display_settings( ).
  lr_display_settings->set_list_header_size( '10' ). " 0=l, 1=s, 2=m
  lr_display_settings->set_list_header( l_title ).

  lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
  lr_display_settings->set_list_header( l_title ).

  " Enable Zebra Layout
  lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).

  " Enable cell selection mode
  lr_selections = gr_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  " Enable the save layout buttons
  lv_key-report = sy-repid.
  lr_layout = gr_table->get_layout( ).
  lr_layout->set_key( lv_key ).
  lr_layout->set_save_restriction( cl_salv_selections=>single ). " cl_salv_selections=>multiple , if_salv_c_layout=>restrict_none
  lr_layout->set_default( if_salv_c_bool_sap=>true ).

  " ... §7 display the table
  gr_table->display( ).

  PERFORM make_table_alv.
ENDFORM.

FORM show_function_info USING i_function TYPE sy-ucomm  i_text TYPE string.

  IF i_function IS INITIAL.
    RETURN.
  ENDIF.
  " Botões da Grid
  CASE i_function.
    WHEN 'SALVAR_MASSA'.
      PERFORM validacoes.
      gr_table->refresh( ).
      CLEAR:wa_error.
      READ TABLE it_lanc_mass INTO wa_error WITH KEY valid = icon_led_red.

      IF wa_error-valid = icon_led_red.
********************************************************************** ALV POP-UP
        TYPES: BEGIN OF ty_err,
                 status  TYPE zfie0026-valid,
                 linha   TYPE i,
                 message TYPE string,
               END OF ty_err.

        DATA it_err TYPE STANDARD TABLE OF ty_err INITIAL SIZE 0.
        DATA wa_err_saida TYPE ty_err.
        FREE: it_err.
        CLEAR: wa_err_saida.
        LOOP AT it_lanc_mass INTO DATA(wa_mass).
          IF wa_mass-valid = icon_led_red.
            wa_err_saida-status = wa_mass-valid.
            wa_err_saida-linha = sy-tabix.
            wa_err_saida-message = wa_mass-error.
            APPEND wa_err_saida TO it_err.
          ENDIF.
          CLEAR: wa_err_saida.
        ENDLOOP.

        DATA go_alv TYPE REF TO cl_salv_table.


        TRY.
            cl_salv_table=>factory(
              IMPORTING
                r_salv_table = go_alv
              CHANGING
                t_table      = it_err[] ).
          CATCH cx_salv_msg.

        ENDTRY.

        TRY.

            "***MOSTRA
            DATA(go_column) = go_alv->get_columns( )->get_column( 'STATUS' ).
            go_column->set_short_text( 'Status' ).
            go_column->set_medium_text( 'Status' ).
            go_column->set_long_text( 'Status' ).
            go_column->set_optimized( abap_false ).
            go_column->set_alignment( if_salv_c_alignment=>centered ).
            go_column->set_output_length( '6' ).

            go_column = go_alv->get_columns( )->get_column( 'LINHA' ).
            go_column->set_short_text( 'Linha' ).
            go_column->set_medium_text( 'Linha' ).
            go_column->set_long_text( 'Linha' ).
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

          CATCH cx_root.

        ENDTRY.

        DATA go_title              TYPE lvc_title.
        go_title = |Lançamentos com Erros! { ordemvenda }|.
        DATA(go_display_settings) = go_alv->get_display_settings( ).
        go_display_settings->set_list_header_size( '10' ). " 0=l, 1=s, 2=m
        go_display_settings->set_list_header( go_title ).

        go_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
        go_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
        go_display_settings->set_list_header( go_title ).

        DATA(ltot) = lines( it_err[] ).
        DATA total TYPE i.

        total = ltot + 10.

        go_alv->set_screen_popup(
          start_column = 1
          end_column   = 100
          start_line   = 1
          end_line     = total ).

        go_alv->display( ).

**********************************************************************
        EXIT.
        FREE: it_err.
      ELSE.
        PERFORM salvarmassa.
      ENDIF.
    WHEN 'CALCULAR_ALV'.
*      CLEAR: p_calc.
*      p_calc = 'X'.
      PERFORM calcular.
      lcl_handle_events=>on_data_changed_finished(
        e_modified = 'X'
      ).
    WHEN 'ATUALIZAR_ALV'.
      gr_table->refresh( ).
  ENDCASE.
ENDFORM.

FORM make_table_alv.
  ls_api = gr_table->extended_grid_api( ).
  ls_edit = ls_api->editable_restricted( ).

  TRY.

      ls_edit->set_attributes_for_columnname( columnname              = 'REC_VLR_TOTAL'
                                              all_cells_input_enabled = abap_true ).
      ls_edit->set_attributes_for_columnname( columnname              = 'DATA_PGTO'
                                              all_cells_input_enabled = abap_true ).
      ls_edit->set_attributes_for_columnname( columnname              = 'NUM_COMP_ADIANT'
                                              all_cells_input_enabled = abap_true ).
      ls_edit->set_attributes_for_columnname( columnname              = 'MONT_RBDO'
                                              all_cells_input_enabled = abap_true ).
      ls_edit->set_attributes_for_columnname( columnname              = 'VLR_MULTA_RBDO'
                                              all_cells_input_enabled = abap_true ).
      ls_edit->set_attributes_for_columnname( columnname              = 'VLR_JUROS_RBDO'
                                              all_cells_input_enabled = abap_true ).
      ls_edit->set_attributes_for_columnname( columnname              = 'FORMA_PAG'
                                              all_cells_input_enabled = abap_true ).
      ls_edit->set_attributes_for_columnname( columnname              = 'ZTERM'
                                              all_cells_input_enabled = abap_true ).
      ls_edit->set_attributes_for_columnname( columnname              = 'OBSERVACAO'
                                              all_cells_input_enabled = abap_true ).
    CATCH cx_salv_not_found.
  ENDTRY.
  ls_edit->validate_changed_data( ).
  gr_table->refresh( ).
ENDFORM.

FORM action_process.
  " Botões do Programa Cabeçalho
  CASE sy-ucomm.
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

FORM colunasmostraesconde.
  TRY.
      "***MOSTRA
      lr_column = lr_columns->get_column( 'VALID' ).
      lr_column->set_short_text( 'Status' ).
      lr_column->set_medium_text( 'Status' ).
      lr_column->set_long_text( 'Status' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).

*"// INICIO WBARBOSA 24/12/2024 - US-115811 BUG-162073
      lr_column = lr_columns->get_column( 'NFENUM' ).
      lr_column->set_short_text( 'Nfe'  ).
      lr_column->set_medium_text( 'Nfe' ).
      lr_column->set_long_text( 'Nfe' ).
      lr_column->set_optimized( abap_true ).

      lr_column = lr_columns->get_column( 'BELNR' ).
      lr_column->set_short_text( CONV #( 'Doc.Contábil'  ) ).
      lr_column->set_medium_text( 'Doc.Contábil' ).
      lr_column->set_long_text( 'Doc.Contábil' ).
      lr_column->set_optimized( abap_true ).

      lr_column = lr_columns->get_column( 'VALDT' ).
      lr_column->set_short_text( CONV #( 'Data vencimento'  ) ).
      lr_column->set_medium_text( 'Data vencimento' ).
      lr_column->set_long_text( 'Data vencimento' ).
      lr_column->set_optimized( abap_true ).

      lr_column = lr_columns->get_column( 'VALOR' ).
      lr_column->set_short_text( 'Valor' ).
      lr_column->set_medium_text( 'Valor' ).
      lr_column->set_long_text( 'Valor' ).
      lr_column->set_tooltip( 'Valor'  ).
      lr_column->set_optimized( abap_true ).

      lr_column = lr_columns->get_column( 'TOTAL_RECEBIDO' ).
      lr_column->set_short_text( CONV #( 'Saldo à Receber' ) ).
      lr_column->set_medium_text( 'Saldo à Receber' ).
      lr_column->set_long_text( 'Saldo à Receber' ).
      lr_column->set_optimized( abap_true ).

      lr_column = lr_columns->get_column( 'SALDO_FINAN' ).
      lr_column->set_short_text( CONV #( 'Saldo Financeiro'  ) ).
      lr_column->set_medium_text( 'Saldo Financeiro' ).
      lr_column->set_long_text( 'Saldo Financeiro' ).
      lr_column->set_optimized( abap_true ).
*"// FIM WBARBOSA 24/12/2024 - US-115811 BUG-162073

      lr_column = lr_columns->get_column( 'DATA_PGTO' ).
*      lr_column->set_short_text( 'Dt.Pgto'  ).
      lr_column->set_medium_text( 'Data de Pagto'  ).
      lr_column->set_long_text( 'Data de Pagamento' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '10' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'DATA_VENC' ).
*      lr_column->set_short_text( 'Dt. Venc.'  ).
      lr_column->set_medium_text( 'Data de Venc.'  ).
      lr_column->set_long_text( 'Data de Vencimento' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '10' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'MOEDA' ).
      lr_column->set_short_text( 'Moeda'  ).
      lr_column->set_medium_text( 'Moeda do Doc.' ).
      lr_column->set_long_text( 'Moeda do Documento' ).
      lr_column->set_optimized( abap_true ).
      " lr_column->set_output_length( '10' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'TAXA' ).
      lr_column->set_short_text( 'Ptax'  ).
      lr_column->set_medium_text( 'Ptax' ).
      lr_column->set_long_text( 'Ptax' ).
      lr_column->set_optimized( abap_true ).
      lr_column->set_decimals( '5' ).
      lr_column->set_output_length( '15' ).
      lr_column->set_edit_mask( value = '0,00000' ).

      lr_column = lr_columns->get_column( 'MONT_RBDO' ).
      lr_column->set_short_text( 'Mont.Rec.'  ).
      lr_column->set_medium_text( 'Mont.Recebido' ).
      lr_column->set_long_text( 'Montante Recebido' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '12' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'NUM_COMP_ADIANT' ).
      lr_column->set_short_text( 'NºCp/Adt'  ).
      lr_column->set_medium_text( 'N° Comp./Adiant.' ).
      lr_column->set_long_text( 'N° Compra/Adiantamento' ).
      lr_column->set_optimized( abap_false ).
      " lr_column->set_output_length( '10' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'VLR_MULTA_RBDO' ).
      lr_column->set_short_text( 'Multa'  ).
      lr_column->set_medium_text( 'Multa' ).
      lr_column->set_long_text( 'Multa' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '10' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'VLR_JUROS_RBDO' ).
      lr_column->set_short_text( 'Juros'  ).
      lr_column->set_medium_text( 'Juros' ).
      lr_column->set_long_text( 'Juros' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '10' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'MONT_MOEDA' ).
      lr_column->set_short_text( 'Mont.OV'  ).
      lr_column->set_medium_text( 'Montante OV' ).
      lr_column->set_long_text( 'Montante da OV' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '10' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'FORMA_PAG' ).
      lr_column->set_short_text( 'F.Pgto'  ).
      lr_column->set_medium_text( 'Forma de Pgto' ).
      lr_column->set_long_text( 'Forma de Pagamento' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'ZTERM' ).
      lr_column->set_short_text( 'C.Pgto'  ).
      lr_column->set_medium_text( 'Cond. de Pgto' ).
      lr_column->set_long_text( 'Condição de Pagamento' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '6' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'OBSERVACAO' ).
      lr_column->set_short_text( 'Obs.'  ).
      lr_column->set_medium_text( 'Observação' ).
      lr_column->set_long_text( 'Observação' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_output_length( '100' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'REC_VLR_TOTAL' ).
      lr_column->set_short_text( 'Rec.Tot.' ).
      lr_column->set_medium_text( 'Rec.Tot.' ).
      lr_column->set_long_text( 'Recebimento Total' ).
      lr_column->set_optimized( abap_true ).
      " lr_column->set_output_length( '12' ).
      " lr_column->set_edit_mask( value = '__________' ).

      lr_column = lr_columns->get_column( 'CHECK_CALC' ).
      lr_column->set_short_text( 'Chk.Calc.' ).
      lr_column->set_medium_text( 'Check Calc.' ).
      lr_column->set_long_text( 'Check Calculo' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_output_length( '8' ).
      " lr_column->set_edit_mask( value = '__________' ).

      "***Esconde

      lr_column = lr_columns->get_column( 'BUKRS' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'STATUS' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'DOC_FATURA' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'BELNR' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'VALDT' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'REFKEY' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'SEQ' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'MONT_MI' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'RAZAO_ESPECIAL' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'AJUSTE' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'VLR_DESC_MULT' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'VLR_DESC_JROS' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'VLR_MULTA_CALC' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'VLR_JUROS_CALC' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'VBELN' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'MONT_MOEDA_PARC' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'MONT_MOEDA_FIX' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'AUART' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'NETWR' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).

      lr_column = lr_columns->get_column( 'ERROR' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).


    CATCH cx_salv_not_found INTO lex_not_found.
  ENDTRY.
ENDFORM.

FORM colunasorganiza.
  " ORGANIZA COLUNAS
  lr_columns->set_column_position( columnname = 'VALID'
                                   position   = 1 ).
  lr_columns->set_column_position( columnname = 'REC_VLR_TOTAL'
                                   position   = 2 ).
  lr_columns->set_column_position( columnname = 'NFENUM'
                                   position   = 3 ).
  lr_columns->set_column_position( columnname = 'DATA_PGTO'
                                   position   = 4 ).
  lr_columns->set_column_position( columnname = 'DATA_VENC'
                                   position   = 5 ).
  lr_columns->set_column_position( columnname = 'MOEDA'
                                   position   = 6 ).
  lr_columns->set_column_position( columnname = 'TAXA'
                                   position   = 7 ).
  lr_columns->set_column_position( columnname = 'VALOR'
                                   position   = 8 ).
  lr_columns->set_column_position( columnname = 'TOTAL_RECEBIDO'
                                   position   = 9 ).
  lr_columns->set_column_position( columnname = 'SALDO_FINAN'
                                   position   = 10 ).
  lr_columns->set_column_position( columnname = 'MONT_RBDO'
                                   position   = 11 ).
  lr_columns->set_column_position( columnname = 'NUM_COMP_ADIANT'
                                   position   = 12 ).
  lr_columns->set_column_position( columnname = 'CHECK_CALC'
                                   position   = 13 ).
  lr_columns->set_column_position( columnname = 'VLR_MULTA_RBDO'
                                   position   = 14 ).
  lr_columns->set_column_position( columnname = 'VLR_JUROS_RBDO'
                                   position   = 15 ).
  lr_columns->set_column_position( columnname = 'MONT_MOEDA'
                                   position   = 16 ).
  lr_columns->set_column_position( columnname = 'FORMA_PAG'
                                   position   = 17 ).
  lr_columns->set_column_position( columnname = 'ZTERM'
                                   position   = 18 ).
  lr_columns->set_column_position( columnname = 'OBSERVACAO'
                                   position   = 19 ).
ENDFORM.

FORM colunasf4.
*      " F4 DDIC
  DATA lv_ddic TYPE salv_s_ddic_reference.

  TRY.
      lr_column_tb ?= lr_columns_tb->get_column( columnname = 'REC_VLR_TOTAL' ).
      lv_ddic = VALUE #( table = 'ZFIT0026'
                         field = 'REC_VLR_TOTAL' ).
      lr_column_tb->set_ddic_reference( value = lv_ddic ).
      lr_column_tb->set_f4( if_salv_c_bool_sap=>true ).
      lr_column_tb ?= lr_columns_tb->get_column( columnname = 'FORMA_PAG' ).
      lv_ddic = VALUE #( table = 'ZFIT0026'
                         field = 'FORMA_PAG' ).
      lr_column_tb->set_ddic_reference( value = lv_ddic ).
      lr_column_tb->set_f4( if_salv_c_bool_sap=>true ).
      lr_column_tb ?= lr_columns_tb->get_column( columnname = 'ZTERM' ).
      lv_ddic = VALUE #( table = 'ZFIT0026'
                         field = 'ZTERM' ).
      lr_column_tb->set_ddic_reference( value = lv_ddic ).
      lr_column_tb->set_f4( if_salv_c_bool_sap=>true ).

    CATCH cx_root ##NO_HANDLER.
  ENDTRY.
ENDFORM.

FORM mapalvscreen.
  CLEAR:
l_s_cell,
l_t_cell,
l_t_colum,
l_t_row,
l_s_mode.

  l_s_cell  = gr_table->get_selections( )->get_current_cell( ).
  l_t_cell  = gr_table->get_selections( )->get_selected_cells( ).
  l_t_colum = gr_table->get_selections( )->get_selected_columns( ).
  l_t_row   = gr_table->get_selections( )->get_selected_rows( ).
  l_s_mode  = gr_table->get_selections( )->get_selection_mode( ).
ENDFORM.

FORM jurosmulta.
  MOVE-CORRESPONDING <_valida> TO wa_lanc.
  MOVE-CORRESPONDING <_valida> TO wa_edit.

  IF wa_edit-data_pgto <> '00000000' AND wa_edit-data_venc <> '00000000' AND wa_edit-mont_rbdo <> ' '.

    " Mercado Interno
    SELECT SINGLE * FROM zsdt0053
      INTO @DATA(wa_zsdt0053)
      WHERE vbeln = @wa_edit-vbeln.

    IF sy-subrc = 0.
      SELECT SINGLE * FROM zsdt0051
        INTO @DATA(wa_zsdt0051)
        WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.
    ENDIF.

    " Insumos
    SELECT SINGLE * FROM zsdt0090
      INTO @DATA(wa_zsdt0090)
      WHERE vbeln = @wa_edit-vbeln.

    IF sy-subrc = 0.
      SELECT SINGLE * FROM zsdt0040
        INTO @DATA(wa_zsdt0040)
        WHERE doc_simulacao = @wa_zsdt0090-doc_simulacao.
    ELSE.

      SELECT SINGLE * FROM zsdt0041
        INTO @DATA(wa_zsdt0041)
        WHERE vbeln = @wa_edit-vbeln.

      IF sy-subrc = 0.
        SELECT SINGLE * FROM zsdt0040
          INTO @wa_zsdt0040
          WHERE doc_simulacao = @wa_zsdt0041-doc_simulacao.
      ENDIF.
    ENDIF.

    vlr_multa = 0.
    vlr_juros = 0.

    zcl_miro=>get_proximo_dia_util( EXPORTING  i_data_base        = wa_edit-data_venc
                                               i_signum           = '+'
                                               i_ck_data_zles0145 = abap_true
                                    RECEIVING  r_data             = v_data_aux
                                    EXCEPTIONS erro               = 1
                                               OTHERS             = 2 ).

    p_mi = 'X'.

    IF p_mi IS NOT INITIAL.

*      IF wa_edit-data_pgto > v_data_aux OR v_mont_aux > wa_edit-mont_rbdo AND wa_edit-data_pgto > v_data_aux.
      IF p_calc = 'X'.

        DATA(d_atraso) = ( wa_edit-data_pgto - wa_edit-data_venc ).

        " =====================================================================================================
        CLEAR fat_prop.
        fat_prop = ( wa_zsdt0051-tx_juros / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

        IF wa_edit-rec_vlr_total = abap_true. " Se o campo recebimento total for marcado, considerar o juros com base no total da OV.
          IF wa_edit-total_recebido IS NOT INITIAL. " Se o valor da fatura estiver preenchido, fazer o calculo valor da referencia.
            " VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).

            CLEAR: vlr_juros,
                   vlr_jros,
                   prop_multa,
                   vlr_mult.
            vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  " Valor do juros com base no valor total da OV.
            vlr_mult = ( wa_edit-total_recebido * wa_zsdt0051-tx_multa ) / 100.

            vlr_juros = vlr_jros.
            vlr_multa = vlr_mult.
          ELSE.

            CLEAR: vlr_juros,
                   vlr_jros,
                   prop_multa,
                   vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  " Valor do juros com base no valor total da OV.
            vlr_mult = ( wa_lanc-mont_moeda_parc * wa_zsdt0051-tx_multa ) / 100.

            vlr_juros = vlr_jros.
            vlr_multa = vlr_mult.
          ENDIF.

        ELSE.

          CLEAR: vlr_juros,
                 vlr_jros,
                 prop_multa,
                 vlr_mult.
          vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  " Valor do juros com base no valor total da OV.
          vlr_mult = ( wa_lanc-mont_moeda_fix * wa_zsdt0051-tx_multa ) / 100.

          CLEAR total_ov.
          total_ov = ( vlr_jros + wa_lanc-mont_moeda_fix + vlr_mult ). " Total da OV + juros.

          prop_juros = ( vlr_jros / total_ov ) * 100. " Porcentagem proporcional ao valor do juros
          prop_multa = ( vlr_mult / total_ov ) * 100. " Porcentagem proporcional da multa.

          CLEAR total_ov.

          vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
          vlr_multa = ( prop_multa * wa_edit-mont_rbdo ) / 100.
        ENDIF.

        " =======================================================================================================

        wa_edit-vlr_multa_rbdo = vlr_multa.
        wa_edit-vlr_juros_rbdo = vlr_juros.

        wa_edit-vlr_multa_calc = vlr_multa.
        wa_edit-vlr_juros_calc = vlr_juros.

        wa_edit-mont_moeda     = ( wa_edit-mont_rbdo - wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo ).
        "calc = abap_true.
      ELSE.

        d_atraso = ( wa_edit-data_pgto - wa_edit-data_venc ).

        " =====================================================================================================
        CLEAR fat_prop.
        fat_prop = ( wa_zsdt0051-tx_juros / 360 ) * d_atraso. " Achar o fator de proporção ( Dias no ano x taxa de juros x dias em atraso ).

        IF wa_edit-rec_vlr_total = abap_true.
          IF wa_edit-total_recebido IS NOT INITIAL. " Se o valor da fatura estiver preenchido, fazer o calculo com base neste valor.
            " VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
            CLEAR: vlr_juros,
                   vlr_jros,
                   prop_multa,
                   vlr_mult.
            vlr_jros = ( wa_edit-total_recebido * fat_prop ) / 100.  " Valor do juros com base no valor total da OV.
            vlr_mult = ( wa_edit-total_recebido * wa_zsdt0051-tx_multa ) / 100.

            vlr_juros = vlr_jros.
            vlr_multa = vlr_mult.
          ELSE.

            " VLR_JUROS = ( ( WA_EDIT-MONT_RBDO * D_ATRASO * ( WA_ZSDT0051-TX_JUROS / 360 ) ) / 100 ).
            CLEAR: vlr_juros,
                   vlr_jros,
                   prop_multa,
                   vlr_mult.
            vlr_jros = ( wa_lanc-mont_moeda_parc * fat_prop ) / 100.  " Valor do juros com base no valor total da OV.
            vlr_mult = ( wa_lanc-mont_moeda_parc * wa_zsdt0051-tx_multa ) / 100.

            vlr_juros = vlr_jros.
            vlr_multa = vlr_mult.

          ENDIF.

        ELSE.

          CLEAR: vlr_juros,
                 vlr_jros,
                 prop_multa,
                 vlr_mult.
          vlr_jros = ( wa_lanc-mont_moeda_fix * fat_prop ) / 100.  " Valor do juros com base no valor total da OV.
          vlr_mult = ( wa_lanc-mont_moeda_fix * wa_zsdt0051-tx_multa ) / 100.

          total_ov = ( vlr_jros + wa_lanc-mont_moeda_fix + vlr_mult ). " Total da OV + juros.
          prop_juros = ( vlr_jros / total_ov ) * 100. " Porcentagem proporcional ao valor do juros
          prop_multa = ( vlr_mult / total_ov ) * 100. " Porcentagem proporcional da multa.
          CLEAR total_ov.

          vlr_juros = ( prop_juros * wa_edit-mont_rbdo ) / 100.
          vlr_multa = ( prop_multa * wa_edit-mont_rbdo ) / 100.

        ENDIF.

        " =======================================================================================================
        " Se for ajuste não calcular juros nem multa.
        IF wa_edit-ajuste IS NOT INITIAL.
          vlr_juros = ' '.
          vlr_multa = ' '.
        ENDIF.

        " Alterando valor do Montante Recebido ou executar a opção recebimento total.
        IF mont_rbdo_anter <> wa_edit-mont_rbdo OR sy-ucomm = 'CHECK_AJUST'.
          mont_rbdo_anter        = wa_edit-mont_rbdo.
          wa_edit-vlr_juros_calc = vlr_juros.
          wa_edit-vlr_multa_calc = vlr_multa.
          wa_edit-vlr_juros_rbdo = vlr_juros.
          wa_edit-vlr_multa_rbdo = vlr_multa.

        ENDIF.

        " Alterando valor do juros
        IF wa_edit-vlr_juros_rbdo = wa_edit-vlr_juros_calc.
          wa_edit-vlr_juros_rbdo = vlr_juros.
        ENDIF.

        " Alterando data de pagamento
        IF dt_pgto_anter <> wa_edit-data_pgto.
          dt_pgto_anter = wa_edit-data_pgto.

          wa_edit-vlr_juros_rbdo = vlr_juros.
          wa_edit-vlr_juros_calc = vlr_juros.

          wa_edit-vlr_multa_rbdo = vlr_multa.
          wa_edit-vlr_multa_calc = vlr_multa.
        ENDIF.
        wa_edit-mont_moeda = ( wa_edit-mont_rbdo - wa_edit-vlr_multa_rbdo - wa_edit-vlr_juros_rbdo ).
      ENDIF.

      CLEAR v_mont_aux.
      v_mont_aux = wa_edit-mont_rbdo.

    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING wa_edit TO <_valida>.


ENDFORM.

FORM colunassoma.
  TRY.
      lr_col_aggreg ?= lr_cols_aggreg->add_aggregation( columnname = 'VALOR' ).
      lr_col_aggreg ?= lr_cols_aggreg->add_aggregation( columnname = 'TOTAL_RECEBIDO' ).
      lr_col_aggreg ?= lr_cols_aggreg->add_aggregation( columnname = 'MONT_RBDO'  ).
      lr_col_aggreg ?= lr_cols_aggreg->add_aggregation( columnname = 'MONT_MOEDA' ).
    CATCH cx_root.
  ENDTRY.
ENDFORM.

FORM salvarmassa.

  IF wa_edit-bukrs IS INITIAL.
    MOVE-CORRESPONDING <_valida> TO wa_edit.
  ENDIF.

  LOOP AT it_lanc_mass ASSIGNING <_valida> WHERE valid = icon_led_green.

    CLEAR wa_lanc.
    MOVE-CORRESPONDING <_valida> TO wa_lanc.
    MOVE-CORRESPONDING <_valida> TO wa_edit.
    DATA l_status         TYPE c LENGTH 1.
    DATA l_message        TYPE c LENGTH 64.
    DATA txtopen          TYPE c LENGTH 1.
    DATA block            TYPE c LENGTH 1.
    DATA rbutton_acerto   TYPE c LENGTH 1.
    DATA rbutton_deposito TYPE c LENGTH 1.
    DATA p_erro           TYPE c LENGTH 1.

    DATA p_zid            TYPE numc10.
    DATA cont             TYPE i.
    DATA zfit0026_wa      TYPE zfit0026.
    DATA soma             TYPE zfit0026-mont_moeda.
    DATA obj_key          TYPE zfit0026-obj_key.

    DATA texto_01         TYPE c LENGTH 100        VALUE 'Valor do montante menor do que o valor'.
    DATA texto_02         TYPE c LENGTH 100        VALUE 'do lançamento!'.

    DATA texto_03         TYPE c LENGTH 100        VALUE 'Não existe montante suficiente'.
    DATA texto_04         TYPE c LENGTH 100        VALUE 'para esse lançamento'.

    DATA razao_especial   TYPE c LENGTH 1.

    DATA it_zib_contabil_chv TYPE STANDARD TABLE OF zib_contabil_chv.
    DATA wa_zib_contabil_chv TYPE zib_contabil_chv.
    DATA it_z0159            TYPE STANDARD TABLE OF zsdt0159.
    DATA wa_z0159            TYPE zsdt0159.

    DATA vl_gdatu         TYPE gdatu_inv.

    DATA vsoma            TYPE zfit0026-mont_moeda.
    DATA c_vbeln          TYPE zfit0026-vbeln.
    DATA c_bukrs          TYPE zfit0026-bukrs.


    CLEAR p_erro.
    c_vbeln = wa_lanc-vbeln.
    c_bukrs = wa_lanc-bukrs_vf.

    FREE: l_status,
          l_message.

    DATA obj_zcl_util_sd TYPE REF TO zcl_util_sd.
    CREATE OBJECT obj_zcl_util_sd.

    CLEAR vl_gdatu.

    IF wa_lanc-doc_fatura IS NOT INITIAL.

      SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname data_registro bukrs obj_key
             docnum zterm doc_fatura data_pgto vlr_multa_rbdo vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
             observacao ajuste rec_vlr_total vlr_desc_jros vlr_desc_mult
        FROM zfit0026
        INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
        WHERE vbeln      = wa_edit-vbeln
          AND doc_fatura = wa_edit-doc_fatura.

    ELSE.
      SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname data_registro bukrs obj_key
             docnum zterm doc_fatura data_pgto vlr_multa_rbdo vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
             observacao ajuste rec_vlr_total vlr_desc_jros vlr_desc_mult
        FROM zfit0026
        INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
        WHERE vbeln = wa_edit-vbeln.
    ENDIF.

    " Totalizar montante recebido.
    LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = c_vbeln.
      vsoma += wa_lanc_ver-mont_moeda.
    ENDLOOP.

    IF wa_edit-doc_fatura IS NOT INITIAL.
      wa_lanc-mont_moeda_fix = wa_edit-total_recebido.
    ELSE.
      wa_lanc-mont_moeda_fix = wa_lanc-mont_moeda_parc.
    ENDIF.

    IF wa_edit-ajuste <> 'X' AND wa_lanc-docnum = 0.     " Comentado Aoenning
      IF wa_edit-mont_moeda > wa_lanc-mont_moeda_fix.
        IF wa_edit-doc_fatura IS INITIAL.
          MESSAGE e888(sabapdocu) WITH 'Ordem de venda não possue saldo para o lançamento'.
          EXIT.
        ELSE.
          MESSAGE e888(sabapdocu) WITH 'Referencia não possue saldo para o lançamento'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    " Só pode ser negativo, quando for lançamento de ajuste.
    IF wa_edit-mont_moeda < 0.
      IF wa_edit-ajuste <> 'X'.
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH 'Valor não pode ser Negativo!'.
        txtopen = abap_true.
        p_erro = abap_true.
        EXIT.
      ENDIF.
    ENDIF.

    IF wa_edit-data_pgto = ' '.
      MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH 'Falta preencher o campo data de pagamento!'.
      txtopen = abap_true.
      p_erro = abap_true.
      EXIT.
    ENDIF.

    IF wa_edit-rec_vlr_total IS NOT INITIAL.

      IF wa_edit-valor IS NOT INITIAL.
        IF wa_edit-total_recebido <> wa_edit-mont_moeda.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH 'Montante da OV deve ser igual ao montante parcial da referencia'.
          p_erro = abap_true.
          txtopen = abap_true.
          EXIT.
        ENDIF.

        IF wa_edit-mont_moeda > wa_edit-total_recebido.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH 'Montante é maior que o valor da referencia'.
          p_erro = abap_true.
          txtopen = abap_true.
          EXIT.
        ENDIF.

      ELSE.

        IF wa_edit-mont_moeda > wa_lanc-mont_moeda_parc.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH 'Montante é maior que o valor da OV'.
          p_erro = abap_true.
          txtopen = abap_true.
          EXIT.
        ENDIF.

        IF wa_edit-mont_moeda <> wa_lanc-mont_moeda_parc.
          MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH 'Montante da OV deve ser igual ao montante parcial'.
          p_erro = abap_true.
          txtopen = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_edit-data_pgto IS NOT INITIAL.
      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = wa_edit-bukrs
          i_data   = wa_edit-data_pgto
        IMPORTING
          e_status = l_status
          e_messa  = l_message
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF l_status = 'E'.
        DATA(l_mess1) = l_message(30).
        DATA(l_mess2) = l_message+30(34).
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH l_mess1 l_mess2.
        txtopen = abap_true.
        p_erro = abap_true.
        EXIT.
      ENDIF.
    ENDIF.


    IF     (    ( wa_edit           IS INITIAL )
             OR ( wa_edit-data_venc IS INITIAL )
             OR ( wa_edit-moeda     IS INITIAL )
             OR ( wa_edit-forma_pag IS INITIAL )
             OR ( wa_edit-taxa      IS INITIAL )
             OR ( wa_edit-mont_rbdo IS INITIAL )
             OR ( wa_edit-zterm     IS INITIAL ) )
       AND ( wa_edit-moeda <> 'BRL' ).

      MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH 'Obrigatório preencher todos os campos!'.
      txtopen = abap_true.
      p_erro = abap_true.
      EXIT.

    ELSE.

      IF wa_edit-data_pgto IS INITIAL.
        block = abap_false.
        MESSAGE 'Favor informar Data Pagamento!' TYPE 'E'.
        p_erro = abap_true.
        EXIT.
      ENDIF.

      IF wa_edit-mont_rbdo IS INITIAL.
        block = abap_false.
        MESSAGE 'Favor informar Montante Recebido!' TYPE 'E'.
        p_erro = abap_true.
        EXIT.
      ENDIF.

      IF ( wa_edit-ajuste <> 'X' ) AND ( wa_edit-mont_moeda > wa_lanc-mont_moeda_fix ).
        CLEAR wa_edit.
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH texto_01 texto_02.
      ELSEIF ( wa_edit-ajuste <> 'X' ) AND ( wa_edit-mont_moeda > wa_lanc-mont_moeda_parc ) AND ( cont <> 0 ).
        CLEAR wa_edit.
        MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH texto_03 texto_04.
      ELSE.
*
        CLEAR obj_key.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZID_LANC'
          IMPORTING
            number      = p_zid.

        SELECT vbeln seq FROM zfit0026
          INTO TABLE it_cont_seq
          WHERE vbeln = <_valida>-vbeln
          ORDER BY seq DESCENDING.

        READ TABLE it_cont_seq INTO wa_cont_seq INDEX 1.

        zfit0026_wa-zid_lanc   = p_zid.
        zfit0026_wa-seq        = wa_cont_seq-seq + 1.
        zfit0026_wa-vbeln      = wa_edit-vbeln.
        zfit0026_wa-data_venc  = wa_edit-data_venc.
        zfit0026_wa-bukrs      = wa_edit-bukrs.
        zfit0026_wa-zterm      = wa_edit-zterm.
        zfit0026_wa-observacao = wa_edit-observacao. "157529 CS2023000317 - Erros na transação ZFIS26 PSA
        zfit0026_wa-moeda      = wa_edit-moeda.

        zfit0026_wa-mont_moeda = wa_edit-mont_moeda.
        zfit0026_wa-ajuste     = wa_edit-ajuste.

        IF zfit0026_wa-moeda = 'BRL'.
          IF wa_edit-ajuste IS INITIAL.
            zfit0026_wa-taxa = 1.
          ELSE.
            obj_zcl_util_sd->set_kurst( i_kurst = 'G' ).
            obj_zcl_util_sd->set_waerk( i_waerk = 'USD' ).
            obj_zcl_util_sd->set_tcurr( i_tcurr = 'BRL' ).

            vl_gdatu = wa_edit-data_pgto.
            obj_zcl_util_sd->set_data( vl_gdatu ).
            zfit0026_wa-taxa = obj_zcl_util_sd->taxa_cambio_dia( ).

            IF ( zfit0026_wa-taxa IS INITIAL ) OR ( zfit0026_wa-moeda = 'BRL' ). " alteracao feita por Alexandre Rimini 04.04.2023 - antes era  IF zfit0026_wa-taxa IS INITIAL.
              zfit0026_wa-taxa = 1.
            ENDIF.
          ENDIF.
          zfit0026_wa-mont_mi = wa_edit-mont_moeda * zfit0026_wa-taxa.
        ELSE.
          zfit0026_wa-taxa    = wa_edit-taxa.
          zfit0026_wa-mont_mi = wa_edit-mont_moeda * zfit0026_wa-taxa.
        ENDIF.

        IF rbutton_acerto = 'X'.
          zfit0026_wa-razao_especial = 'G'.
        ELSEIF rbutton_deposito = 'X'.
          zfit0026_wa-razao_especial = 'L'.
        ENDIF.

        zfit0026_wa-forma_pag = wa_edit-forma_pag.
        IF zfit0026_wa-ajuste = 'X'.
          zfit0026_wa-status = 'A'.
        ELSE.
          zfit0026_wa-status = space.
        ENDIF.
        zfit0026_wa-uname         = sy-uname.
        zfit0026_wa-data_registro = sy-datum.

        zfit0026_wa-data_pgto     = wa_edit-data_pgto.
        zfit0026_wa-mont_rbdo     = wa_edit-mont_rbdo.
        zfit0026_wa-doc_fatura    = wa_edit-doc_fatura.
        zfit0026_wa-rec_vlr_total = wa_edit-rec_vlr_total.

        IF wa_edit-ajuste IS NOT INITIAL AND wa_edit-mont_moeda = 0.
          zfit0026_wa-vlr_desc_jros = wa_edit-vlr_juros_rbdo.
          zfit0026_wa-vlr_desc_mult = wa_edit-vlr_multa_rbdo.
        ELSE.
          zfit0026_wa-vlr_juros_rbdo = wa_edit-vlr_juros_rbdo.
          zfit0026_wa-vlr_multa_rbdo = wa_edit-vlr_multa_rbdo.
        ENDIF.

        IF wa_edit-ajuste IS INITIAL AND wa_edit-mont_moeda <> 0. "
          zfit0026_wa-vlr_juros_calc = wa_edit-vlr_juros_calc.
          zfit0026_wa-vlr_multa_calc = wa_edit-vlr_multa_calc.
        ELSE.
          zfit0026_wa-vlr_juros_calc = ' '.
          zfit0026_wa-vlr_multa_calc = ' '.
        ENDIF.

        CONCATENATE zfit0026_wa-vbeln zfit0026_wa-seq sy-datum(4) INTO obj_key.

        zfit0026_wa-obj_key = obj_key.

        INSERT INTO zfit0026 VALUES zfit0026_wa.

        CLEAR: it_lanc[],
               wa_lanc,
               it_lanc_ver[],
               wa_lanc_ver.

        SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname data_registro bukrs obj_key
               docnum zterm doc_fatura data_pgto vlr_multa_rbdo vlr_juros_rbdo mont_rbdo vlr_multa_calc
               vlr_juros_calc razao_especial observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros
          FROM zfit0026
          INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
          WHERE vbeln = wa_edit-vbeln.

        SORT it_lanc_ver BY seq.

        IF sy-subrc = 0.

          CLEAR soma.

          LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = wa_edit-vbeln.

            MOVE-CORRESPONDING wa_lanc_ver TO wa_lanc.

            wa_lanc-edit    = icon_change.
            wa_lanc-excluir = icon_delete.
            IF wa_lanc_ver-ajuste <> 'X'.
              wa_lanc-gera  = icon_activity.
              wa_lanc-estor = icon_reject.
            ELSE.
              CLEAR: wa_lanc-gera,
                     wa_lanc-estor.
            ENDIF.
            wa_lanc-bukrs_vf = wa_lanc_ver-bukrs.
            wa_lanc-ajuste   = wa_lanc_ver-ajuste.

            CASE wa_lanc-razao_especial.
              WHEN 'G'.
                wa_lanc-razao = 'Acerto'.
              WHEN 'L'.
                wa_lanc-razao = 'Depósito'.
            ENDCASE.

            CASE wa_lanc_ver-status.
              WHEN 'P'.
                wa_lanc-status_doc = icon_generate.
              WHEN 'E'.
                wa_lanc-status_doc = icon_led_red.
              WHEN 'G'.
                wa_lanc-status_doc = icon_led_green.
              WHEN 'X'.
                wa_lanc-status_doc = icon_booking_stop.
              WHEN 'A'.
                wa_lanc-status_doc = icon_budget_update.
              WHEN OTHERS.
                wa_lanc-status_doc = icon_led_yellow.
            ENDCASE.

            wa_lanc-mont_moeda_fix = <_valida>-netwr.

            wa_lanc-observacao     = wa_lanc_ver-observacao.

            soma += wa_lanc_ver-mont_moeda.

            APPEND wa_lanc TO it_lanc.

          ENDLOOP.

          IF <_valida>-auart IN r_devo_recu.
            wa_lanc-mont_moeda_parc  = wa_lanc-mont_moeda_fix - abs( soma ).
            wa_lanc-mont_moeda_parc *= -1.
          ELSE.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.
          ENDIF.

          CLEAR: wa_edit,
                 it_edit[].


        ENDIF.
      ENDIF.

      IF p_erro IS INITIAL.

        DATA lt_zsdt0315 TYPE TABLE OF zsdt0315.

        CALL FUNCTION 'ZSDMF_GRAVA_REG_ZSDT0315'
          EXPORTING
            iv_vbeln             = zfit0026_wa-vbeln
            iv_waers             = zfit0026_wa-moeda
            iv_valor_ov          = wa_lanc-mont_moeda_fix
            iv_liqui             = wa_lanc-mont_moeda
            iv_commit            = 'X'
          TABLES
            et_zsdt0315          = lt_zsdt0315
          EXCEPTIONS
            ov_100_liquidada     = 1
            vlr_desm_maior_ov    = 2
            desm_e_liqui         = 3
            vlr_liqui_maior_ov   = 4
            informar_ov_desmem   = 5
            vlr_ov_desatualizado = 6
            ov_nova_existente    = 7
            OTHERS               = 8.

        IF sy-subrc <> 0.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDLOOP.

  IF sy-subrc <> 0.
    MESSAGE s888(sabapdocu) DISPLAY LIKE 'I' WITH 'Não Existem dados para salvar!'.
    EXIT.
  ELSE.

    IF p_erro IS INITIAL.
      SET SCREEN 0.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.

FORM calcular.
  CLEAR: p_calc.
  LOOP AT it_lanc_mass ASSIGNING <_valida> WHERE data_pgto <> '00000000'
                                              AND mont_rbdo IS NOT INITIAL
                                              AND forma_pag IS NOT INITIAL
                                              AND zterm IS NOT INITIAL.
                                              "AND observacao IS NOT INITIAL. BUG - 174094 - CBRAND

    CASE <_valida>-check_calc.
      WHEN 'C'.

      WHEN 'M'.

      WHEN ''.
        CLEAR: p_calc.
        p_calc = 'X'.
        <_valida>-check_calc = 'C'.
        IF <_valida>-data_pgto > <_valida>-data_venc. "PSA 163127 - CS2023000317 - ZFIS26 - JUROS E MULTA VENCIMENTO FUTURO
        PERFORM jurosmulta.
        ENDIF.
        CLEAR: p_calc.
    ENDCASE.



  ENDLOOP.


*  LOOP AT it_lanc_mass ASSIGNING <_valida>.
*
*    IF     <_valida>-rec_vlr_total    = abap_false
*       AND <_valida>-data_pgto        = '00000000'
*       AND <_valida>-mont_rbdo       IS INITIAL
*       AND <_valida>-num_comp_adiant IS INITIAL
*       AND <_valida>-vlr_multa_rbdo  IS INITIAL
*       AND <_valida>-vlr_juros_rbdo  IS INITIAL
*       AND <_valida>-forma_pag       IS INITIAL
*       AND <_valida>-zterm           IS INITIAL
*       AND <_valida>-observacao      IS INITIAL
*       AND <_valida>-check_calc      IS INITIAL.
*      <_valida>-valid = icon_led_yellow.
*
*    ELSE.
*
*      IF <_valida>-rec_vlr_total = 'x'.
*        <_valida>-rec_vlr_total = 'X'.
*      ENDIF.
*
*      IF     <_valida>-data_pgto      <> '00000000'
*         AND <_valida>-mont_rbdo      > 0
*         AND <_valida>-forma_pag      IS NOT INITIAL
*         AND <_valida>-zterm          IS NOT INITIAL
*         AND <_valida>-check_calc     IS INITIAL.
*
*        <_valida>-valid = icon_led_green.
*
*        "155236 - CS202300317 - ZFIS26 - Lçto adiantamento Clientes Mercado Interno
*        IF <_valida>-data_pgto > <_valida>-data_venc.
*          PERFORM jurosMulta.
*        ELSE.
*          "Valida a passagem do calculo dos juros e multa
*          <_valida>-check_calc = 'C'.
*
*          CHECK <_valida>-check_calc = 'C'.
*          CLEAR: p_calc.
*        ENDIF .
*
*      ELSE.
*        <_valida>-valid = icon_led_red.
*      ENDIF.
*
*    ENDIF.
*
*  ENDLOOP.
ENDFORM.

FORM validacoes.
  LOOP AT it_lanc_mass ASSIGNING <_valida>.
    IF     <_valida>-rec_vlr_total    = abap_false
       AND <_valida>-data_pgto        = '00000000'
       AND <_valida>-mont_rbdo       IS INITIAL
       AND <_valida>-num_comp_adiant IS INITIAL
       AND <_valida>-vlr_multa_rbdo  IS INITIAL
       AND <_valida>-vlr_juros_rbdo  IS INITIAL
       AND <_valida>-forma_pag       IS INITIAL
       AND <_valida>-zterm           IS INITIAL
       "AND <_valida>-observacao      IS INITIAL BUG - 174094 - CBRAND
       AND <_valida>-check_calc      IS INITIAL.
      <_valida>-valid = icon_led_yellow.
    ELSE.
      IF     <_valida>-data_pgto <> '00000000'
         AND <_valida>-mont_rbdo  > 0
         AND <_valida>-forma_pag <> ''
         AND <_valida>-zterm     <> ''.
        IF <_valida>-rec_vlr_total IS NOT INITIAL.

          IF <_valida>-valor IS NOT INITIAL.
            IF <_valida>-total_recebido <> <_valida>-mont_moeda.
              <_valida>-error = 'Montante da OV deve ser igual ao montante parcial da referencia!'.
              <_valida>-valid = icon_led_red.
            ENDIF.

            IF <_valida>-mont_moeda > <_valida>-total_recebido.
              <_valida>-error = 'Montante da OV deve ser igual ao montante parcial da referencia!'.
              <_valida>-valid = icon_led_red.
            ENDIF.

          ELSE.

            IF <_valida>-mont_moeda > wa_lanc-mont_moeda_parc.
              <_valida>-error = 'Montante da OV deve ser igual ao montante parcial da referencia!'.
              <_valida>-valid = icon_led_red.
            ENDIF.

            IF <_valida>-mont_moeda <> wa_lanc-mont_moeda_parc.
              <_valida>-error = 'Montante da OV deve ser igual ao montante parcial da referencia!'.
              <_valida>-valid = icon_led_red.
            ENDIF.
          ENDIF.
        ENDIF.

        IF <_valida>-valid <> icon_led_red AND <_valida>-valid <> icon_led_yellow.
          <_valida>-valid = icon_led_green.
          CLEAR: <_valida>-error.
        ENDIF.

        IF <_valida>-check_calc = abap_false.
          <_valida>-error = 'É necessário Clicar no Botão Calcular!'.
          <_valida>-valid = icon_led_red.
        ENDIF.

      ELSE.
        <_valida>-error = 'Existem campo que faltam preenchimento!'.
        <_valida>-valid = icon_led_red.
      ENDIF.

    ENDIF.

    <_valida>-mont_moeda      = ( <_valida>-mont_rbdo - <_valida>-vlr_multa_rbdo - <_valida>-vlr_juros_rbdo ).

  ENDLOOP.

ENDFORM.

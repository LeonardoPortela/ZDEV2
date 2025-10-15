*&---------------------------------------------------------------------*
*& Include          ZMMR189_p21_P1
*&---------------------------------------------------------------------*

CLASS p21_lcl_handle_events DEFINITION DEFERRED.
DATA p21_gr_events TYPE REF TO p21_lcl_handle_events.

CLASS p21_lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command_p21 FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_before_user_command_p21 FOR EVENT before_salv_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_after_user_command_p21 FOR EVENT after_salv_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_link_click_p21 FOR EVENT link_click OF cl_salv_events_table IMPORTING row column sender,
      on_aft_event_p21 FOR EVENT after_salv_function OF cl_salv_events_table IMPORTING e_salv_function sender,
      on_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid IMPORTING sender,
      on_handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm
          sender.

ENDCLASS.

CLASS p21_lcl_handle_events IMPLEMENTATION.

  METHOD on_handle_data_changed.
    "BREAK-POINT.
  ENDMETHOD.

  METHOD on_after_refresh.
    "BREAK-POINT.
  ENDMETHOD.

  METHOD on_aft_event_p21.
    PERFORM show_function_info_p21 USING e_salv_function TEXT-i08.
  ENDMETHOD.

  METHOD on_user_command_p21.
    PERFORM show_function_info_p21 USING e_salv_function TEXT-i08.
  ENDMETHOD.

  METHOD on_before_user_command_p21.
    PERFORM show_function_info_p21 USING e_salv_function TEXT-i09.
  ENDMETHOD.

  METHOD on_after_user_command_p21.
    PERFORM show_function_info_p21 USING e_salv_function TEXT-i10.
  ENDMETHOD.

  METHOD on_link_click_p21.
    READ TABLE it_dados_p21 INDEX row INTO DATA(ls_dados_p21).
    IF sy-subrc = 0.

      CLEAR: vlr_indic.
      vlr_indic = ls_dados_p21-cd_indic.

      IF column = 'CCUSTOS'.
        SUBMIT zmmr189_ccust
        WITH p_cdind = vlr_indic
        AND RETURN.
      ELSEIF column = 'ITENS'.
        SUBMIT zmmr189_itens
        WITH p_cdind = vlr_indic
        AND RETURN.
      ELSEIF column = 'DEPARA'.
        SUBMIT zmmr189_dep
        WITH p_ano = p_ano
        WITH p_cdind = vlr_indic
        AND RETURN.
      ELSEIF column = 'RELATORIO'.
        PERFORM get_dados_mseg.
        "cl_demo_output=>display( resultado ).
        IF resultado IS NOT INITIAL.
          cl_demo_output=>display( resultado ).
        ELSE.
          DATA(i_msg) = |Não existem dados para O relatório!|.
          MESSAGE i_msg TYPE 'I'.
        ENDIF.

      ELSEIF column = 'EXPORT'.
        PERFORM dados_ksb1.
        PERFORM exp_excel.
      ELSEIF column = 'API'.

      ELSEIF column = 'GRUPOS'.
        SUBMIT zmmr189_grp
        WITH p_cdind = vlr_indic
        AND RETURN.
      ELSEIF column = 'CLCUSTOS'.
        SUBMIT zmmr189_clcust
        WITH p_cdind = vlr_indic
        AND RETURN.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  "on_link_click
ENDCLASS.
**&---------------------------------------------------------------------*
**&      Form  show_function_info
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
FORM show_function_info_p21 USING p21_i_function TYPE salv_de_function
      p21_i_text     TYPE string.
  DATA: p21_l_string TYPE string.
  "concatenate i_text i_function into l_string separated by space.
  "message i000(0k) with l_string.

  CASE p21_i_function.
*     Make ALV as Editable ALV
    WHEN 'EDIT'.
      p21_ls_api = p21_gr_table->extended_grid_api( ).
      p21_ls_edit = p21_ls_api->editable_restricted( ).

      TRY.
          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'CD_INDIC'
            all_cells_input_enabled = abap_true
            ).
          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'NM_INDIC'
            all_cells_input_enabled = abap_true
            ).

          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'CCUSTOS'
            all_cells_input_enabled = abap_true
            ).


          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'CLCUSTOS'
            all_cells_input_enabled = abap_true
            ).
          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'ITENS'
            all_cells_input_enabled = abap_true
            ).

          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'DEPARA'
            all_cells_input_enabled = abap_true
            ).
          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'RELATORIO'
            all_cells_input_enabled = abap_true
            ).

          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'ANO'
            all_cells_input_enabled = abap_false
            ).
          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'EXPORT'
            all_cells_input_enabled = abap_true
            ).

*           p21_ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'API'
*all_cells_input_enabled = abap_true
*).
*
*           p21_ls_edit->set_attributes_for_columnname(
*EXPORTING
*columnname = 'ENVIADOS'
*all_cells_input_enabled = abap_true
*).

          p21_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'GRUPOS'
            all_cells_input_enabled = abap_true
            ).

        CATCH cx_salv_not_found.
      ENDTRY.

      p21_ls_edit->validate_changed_data(
      ).
      PERFORM atuliza_p21_p1.

    WHEN 'SAVE'.

      PERFORM check_values.

*       TRY.
*           MODIFY zfit0201 FROM TABLE it_dados_p21.
*           COMMIT WORK.
*         CATCH cx_salv_not_found.
*       ENDTRY.

      PERFORM atuliza_p21_p1.

    WHEN 'DELETE'.


      DATA: p21_lo_selections TYPE REF TO cl_salv_selections.
      DATA p21_lt_rows TYPE salv_t_row.
      DATA p21_ls_row TYPE int4.
      DATA: p21_msg_text TYPE char45.

      p21_lt_rows = p21_gr_table->get_selections( )->get_selected_rows( ).

      IF p21_lt_rows IS INITIAL.
        p21_msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE p21_msg_text TYPE 'I'.

      ELSE.

        LOOP AT p21_lt_rows INTO p21_ls_row.

          READ TABLE it_dados_p21 INDEX p21_ls_row.
          DELETE FROM zfit0201 WHERE cd_indic = it_dados_p21-cd_indic.
          COMMIT WORK.

        ENDLOOP.

      ENDIF.
      PERFORM atuliza_p21_p1.

    WHEN 'REFRESH'.
      PERFORM atuliza_p21_p1.


  ENDCASE.



ENDFORM.                    " show_function_info
*
**&---------------------------------------------------------------------*
**&      Form  set_columns_technical
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
FORM set_columns_technical_p21 USING p21_ir_columns TYPE REF TO cl_salv_columns.

*...Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)p21_lr_column
  TRY.

      p21_lr_column = p21_ir_columns->get_column( 'CD_INDIC' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'Cod.ID.SIS' ).
      p21_lr_column->set_medium_text( 'Codt.ID.SIS' ).
      p21_lr_column->set_long_text( 'Cód. Ident. SIS' ).

      p21_lr_column = p21_ir_columns->get_column( 'NM_INDIC' ).
      p21_lr_column->set_optimized( abap_true ).
      p21_lr_column->set_short_text( 'NM.ID.SIS' ).
      p21_lr_column->set_medium_text( 'Desc.ID.SIS' ).
      p21_lr_column->set_long_text( 'Identificador SIS' ).

      p21_lr_column = p21_ir_columns->get_column( 'CCUSTOS' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'C.Custo' ).
      p21_lr_column->set_medium_text( 'C.Custo' ).
      p21_lr_column->set_long_text( 'Centro.Custo' ).
      p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
      p21_lr_column->set_tooltip( value = |{ icon_change }| ).

      p21_lr_column = p21_ir_columns->get_column( 'CLCUSTOS' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'CL.Custo' ).
      p21_lr_column->set_medium_text( 'CL.Custo' ).
      p21_lr_column->set_long_text( 'Classe.Custo' ).
      p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
      p21_lr_column->set_tooltip( value = |{ icon_change }| ).

      p21_lr_column = p21_ir_columns->get_column( 'ITENS' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'Itens' ).
      p21_lr_column->set_medium_text( 'Itens' ).
      p21_lr_column->set_long_text( 'Itens' ).
      p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
      p21_lr_column->set_tooltip( value = |{ icon_change }| ).

      p21_lr_column = p21_ir_columns->get_column( 'DEPARA' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'Depara' ).
      p21_lr_column->set_medium_text( 'Depara' ).
      p21_lr_column->set_long_text( 'Depara' ).
      p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
      p21_lr_column->set_tooltip( value = |{ icon_change }| ).

      p21_lr_column = p21_ir_columns->get_column( 'RELATORIO' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'Relatório' ).
      p21_lr_column->set_medium_text( 'Relatório' ).
      p21_lr_column->set_long_text( 'Relatório' ).
      p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
      p21_lr_column->set_tooltip( value = |{ icon_change }| ).

      p21_lr_column = p21_ir_columns->get_column( 'ANO' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'Ano' ).
      p21_lr_column->set_medium_text( 'Ano' ).
      p21_lr_column->set_long_text( 'Ano' ).
      p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
      "p21_lr_column->set_tooltip( value = |{ icon_change }| ).

      p21_lr_column = p21_ir_columns->get_column( 'EXPORT' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'Exportar' ).
      p21_lr_column->set_medium_text( 'Exportar' ).
      p21_lr_column->set_long_text( 'Exportar' ).
      p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
      p21_lr_column->set_tooltip( value = |{ icon_xls }| ).

*       p21_lr_column = p21_ir_columns->get_column( 'API' ).
*       p21_lr_column->set_optimized( abap_false ).
*       p21_lr_column->set_short_text( 'API' ).
*       p21_lr_column->set_medium_text( 'API' ).
*       p21_lr_column->set_long_text( 'API' ).
*       p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
*       p21_lr_column->set_tooltip( value = |{ icon_add_row }| ).

*       p21_lr_column = p21_ir_columns->get_column( 'ENVIADOS' ).
*       p21_lr_column->set_optimized( abap_false ).
*       p21_lr_column->set_short_text( 'Enviados' ).
*       p21_lr_column->set_medium_text( 'Enviados' ).
*       p21_lr_column->set_long_text( 'Enviados' ).
*       p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
*       p21_lr_column->set_tooltip( value = |{ icon_view_list }| ).

      p21_lr_column = p21_ir_columns->get_column( 'GRUPOS' ).
      p21_lr_column->set_optimized( abap_false ).
      p21_lr_column->set_short_text( 'Grupos' ).
      p21_lr_column->set_medium_text( 'Grupos' ).
      p21_lr_column->set_long_text( 'Grupos' ).
      p21_lr_column->set_alignment( if_salv_c_alignment=>centered ).
      p21_lr_column->set_tooltip( value = |{ icon_change }| ).

      p21_lr_column = p21_ir_columns->get_column( 'API' ).
      p21_lr_column->set_visible( if_salv_c_bool_sap=>false ).
      p21_lr_column = p21_ir_columns->get_column( 'ENVIADOS' ).
      p21_lr_column->set_visible( if_salv_c_bool_sap=>false ).

      p21_lr_column = p21_ir_columns->get_column( 'MANDT' ).
      p21_lr_column->set_visible( if_salv_c_bool_sap=>false ).
      p21_lr_column = p21_ir_columns->get_column( 'OID' ).
      p21_lr_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO p21_lex_not_found.
      "write some error handling
  ENDTRY.


ENDFORM.

FORM container_main_p21.

  "Busca os dados
  PERFORM seleciona_dados_p21.
  "Chama o container da cl_salv
  PERFORM docker_p2.

*...Add DDIC reference for F4 generating

  DATA p21_gr_salv_func TYPE REF TO cl_salv_functions .
  p21_gr_salv_func = p21_gr_table->get_functions( ).
  p21_gr_salv_func->set_all( abap_false ).

*... §3.1 activate ALV generic Functions

  p21_lr_functions = p21_gr_table->get_functions( ).
  p21_lr_functions->set_all( abap_false ).

*... §3.2 include own functions
  p21_l_text_edit = 'Editar'.
  p21_l_icon_edit = icon_edit_file.
  p21_l_text_save = 'Salvar'.
  p21_l_icon_save = icon_save_as_template.
  p21_l_text_del = 'Remover'.
  p21_l_icon_del = icon_delete.
  p21_l_text_ref = 'Atualizar'.
  p21_l_icon_ref = icon_refresh.
*   p21_l_text_fat = 'Conversão'.
*   p21_l_icon_fat = icon_equipment.
  TRY.
      p21_lr_functions->add_function(
      name     = 'EDIT'
      icon     = p21_l_icon_edit
      text     = p21_l_text_edit
      tooltip  = 'Edit Item'
      position = if_salv_c_function_position=>right_of_salv_functions ).

      p21_lr_functions->add_function(
      name     = 'SAVE'
      icon     = p21_l_icon_save
      text     = p21_l_text_save
      tooltip  = 'Save Item'
      position = if_salv_c_function_position=>right_of_salv_functions ).

      p21_lr_functions->add_function(
      name     = 'DELETE'
      icon     = p21_l_icon_del
      text     = p21_l_text_del
      tooltip  = 'Deleta Dados'
      position = if_salv_c_function_position=>right_of_salv_functions ).

      p21_lr_functions->add_function(
      name     = 'REFRESH'
      icon     = p21_l_icon_ref
      text     = p21_l_text_ref
      tooltip  = 'Atualizar Dados'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

*... set the columns technical
  DATA: p21_lr_columns    TYPE REF TO cl_salv_columns,
        p21_lr_column     TYPE REF TO cl_salv_column_table,
        p21_lr_selections TYPE REF TO cl_salv_selections.

  DATA: p21_lr_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        p21_lv_key    TYPE salv_s_layout_key.

  p21_lr_columns = p21_gr_table->get_columns( ).

** Column name
*    lv_grMATNR = 'MATNR'.
*
** Table name for DDIC reference
*    ls_grMATNR_ref-table = 'YMAR_CT_GRBEW'.
** Field in the above mentioned table for DDIC reference
*    ls_grMATNR_ref-field = 'YY_LE_GRBEW'.
*
*
*    ls_col_grbew = p21_lr_columns->get_column(
*            EXPORTING
*              columnname = lv_grbew
*          ).
*    ls_col_grbew->set_ddic_reference(
*            EXPORTING
*              value = ls_grbew_ref
*
*         ).

  p21_lr_columns->set_optimize( abap_false ).

  PERFORM set_columns_technical_p21 USING p21_lr_columns.

*... §4 set hotspot column
  TRY.
      p21_lr_column ?= p21_lr_columns->get_column( 'ITENS' ).
      p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      p21_lr_column ?= p21_lr_columns->get_column( 'CCUSTOS' ).
      p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      p21_lr_column ?= p21_lr_columns->get_column( 'CLCUSTOS' ).
      p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      p21_lr_column ?= p21_lr_columns->get_column( 'DEPARA' ).
      p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      p21_lr_column ?= p21_lr_columns->get_column( 'RELATORIO' ).
      p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      p21_lr_column ?= p21_lr_columns->get_column( 'GRUPOS' ).
      p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
      p21_lr_column ?= p21_lr_columns->get_column( 'EXPORT' ).
      p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*       p21_lr_column ?= p21_lr_columns->get_column( 'API' ).
*       p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*       p21_lr_column ?= p21_lr_columns->get_column( 'ENVIADOS' ).
*       p21_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

*... §6 register to the events of cl_salv_table
  DATA p21_lr_events TYPE REF TO cl_salv_events_table.

  p21_lr_events = p21_gr_table->get_event( ).

  CREATE OBJECT p21_gr_events.

*... §6.1 register to the event USER_COMMAND
  SET HANDLER p21_gr_events->on_user_command_p21 FOR p21_lr_events.

  SET HANDLER p21_gr_events->on_before_user_command_p21 FOR p21_lr_events.

  SET HANDLER p21_gr_events->on_after_user_command_p21 FOR p21_lr_events.

  SET HANDLER p21_gr_events->on_link_click_p21  FOR p21_lr_events.

  SET HANDLER p21_gr_events->on_after_refresh FOR ALL INSTANCES ACTIVATION 'X'.

  SET HANDLER p21_gr_events->on_handle_data_changed FOR ALL INSTANCES ACTIVATION 'X'.
*... set list title
  DATA: p21_lr_display_settings TYPE REF TO cl_salv_display_settings,
        p21_l_title             TYPE lvc_title.

  p21_l_title = |Cadastro de Indicadores - { p_ano }|.
  p21_lr_display_settings = p21_gr_table->get_display_settings( ).
  p21_lr_display_settings->set_list_header_size( '1' ).
  p21_lr_display_settings->set_list_header( p21_l_title ).


* Enable cell selection mode
  p21_lr_selections = p21_gr_table->get_selections( ).
  p21_lr_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

* Enable the save layout buttons
  p21_lv_key-report = sy-repid.
  p21_lr_layout = p21_gr_table->get_layout( ).
  p21_lr_layout->set_key( p21_lv_key ).
  p21_lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  p21_lr_layout->set_default( abap_true ).

*... §7 display the table
  p21_gr_table->display( ).

ENDFORM.

FORM atuliza_p21_p1.
  REFRESH it_dados_p21.
  PERFORM seleciona_dados_p21.
  p21_gr_table->refresh( ).
  "p21_gr_table->display( ).


ENDFORM.

FORM pega_dados_cell_column.

  CLEAR: p21p1_oo1,p21p1_oo2,p21p1_oo3,p21p1_oo4,p21p1_oo5.

  p21p1_oo1 = p21_gr_table->get_selections( )->get_current_cell( ).
  p21p1_oo2 = p21_gr_table->get_selections( )->get_selected_cells( ).
  p21p1_oo3 = p21_gr_table->get_selections( )->get_selected_columns( ).
  p21p1_oo4 = p21_gr_table->get_selections( )->get_selected_rows( ).
  p21p1_oo5 = p21_gr_table->get_selections( )->get_selection_mode( ).

ENDFORM.


FORM check_values.

  LOOP AT it_dados_p21 ASSIGNING FIELD-SYMBOL(<it_dados_p21>).

    DATA : l_dupl TYPE i.
    CLEAR: l_dupl.
    LOOP AT it_dados_p21 ASSIGNING FIELD-SYMBOL(<duplicados>) WHERE cd_indic = <it_dados_p21>-cd_indic.
      l_dupl = l_dupl + 1.
    ENDLOOP.

    IF l_dupl > 1.
      DELETE it_dados_p21 WHERE cd_indic = <it_dados_p21>-cd_indic.
    ELSE.
      IF <it_dados_p21>-cd_indic IS INITIAL.
      ENDIF.
      IF <it_dados_p21>-oid IS INITIAL.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = <it_dados_p21>-oid.
      ENDIF.
      MODIFY zfit0201 FROM <it_dados_p21>.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM dados_ksb1.

  PERFORM get_dados_mseg.

  IF Resultado IS NOT INITIAL.

    TYPES: BEGIN OF ty_saida1,
             werks    TYPE zsis_ksb1-werks,
             ebtxt    TYPE zsis_ksb1-ebtxt,
             matnr    TYPE zsis_ksb1-matnr,
             bukrs    TYPE zsis_ksb1-bukrs,
             meinb    TYPE zsis_ksb1-meinb,
             mbgbtr   TYPE menge_d,
             perid(6) TYPE c,
           END OF ty_saida1.


    DATA: it_saida1 TYPE STANDARD TABLE OF ty_saida1 WITH HEADER LINE.
    DATA aux_conv TYPE decfloat34.
    DATA CONV_mbgbtr TYPE decfloat34.

    LOOP AT resultado ASSIGNING FIELD-SYMBOL(<resultado>).

      DATA: l_biomassa TYPE p.
      CLEAR: l_biomassa,aux_conv.
      PACK <resultado>-matnr TO l_biomassa.

      CASE l_biomassa.
          "MST , M3 OU ST
        WHEN 420035 OR 47071 OR 119622 OR 393108 OR 435684 OR 397567.
          <resultado>-meinb = 'GCM'.
          aux_conv = '0.65'.
          CONV_mbgbtr = <resultado>-mbgbtr * aux_conv.
          <resultado>-mbgbtr = CONV_mbgbtr.
        WHEN 266316 OR 47072 OR 271610 OR 37828 OR 37827 OR 37828 OR 86183.
          <resultado>-meinb = 'GCM'.
          aux_conv = '0.608'.
          CONV_mbgbtr = <resultado>-mbgbtr * aux_conv.
          <resultado>-mbgbtr = CONV_mbgbtr.
        WHEN 429791.
          <resultado>-meinb = 'GCM'.
          aux_conv = '0.608'.
          CONV_mbgbtr = <resultado>-mbgbtr * aux_conv.
          <resultado>-mbgbtr = CONV_mbgbtr.
        WHEN 240357 OR 87406 OR 120883.
          <resultado>-meinb = 'GCM'.
          aux_conv = '0.3'.
          CONV_mbgbtr = <resultado>-mbgbtr * aux_conv.
          <resultado>-mbgbtr = CONV_mbgbtr.
      ENDCASE.

      DATA: l_glp TYPE p.
      CLEAR: l_glp.
      PACK <resultado>-matnr TO l_glp.

      CASE l_glp.
        WHEN 28995 OR 140405 OR 3103 OR 33199 OR 9633 OR 110900 OR 140405. "13.
          CLEAR: <resultado>-meinb,<resultado>-mbgbtr.
          <resultado>-meinb = 'KG'.
          <resultado>-mbgbtr = 13.
        WHEN 4687 OR 28994 OR 289059 OR 8256 . "45
          CLEAR: <resultado>-meinb,<resultado>-mbgbtr.
          <resultado>-meinb = 'KG'.
          <resultado>-mbgbtr = 45.
        WHEN 283492 OR 113096 OR 113292 OR  138451. "20
          CLEAR: <resultado>-meinb,<resultado>-mbgbtr.
          <resultado>-meinb = 'KG'.
          <resultado>-mbgbtr = 20.
      ENDCASE.

      SELECT SINGLE cd_item FROM zfit0202 WHERE cd_indic = @vlr_indic AND matnr = @<resultado>-matnr INTO @DATA(aux_cd_item) .

      CLEAR: <resultado>-matnr.

      <resultado>-matnr = aux_cd_item. "ALTERA PARA O CD_ITEM

      "VERIFICA SE PRECISA DE FATOR DE CONVERSÃO

      IF <resultado>-meinb = 'ST' OR <resultado>-meinb = 'STM' OR <resultado>-meinb = 'M3' OR <resultado>-meinb = 'KG' . "Todos serão convertidos para Toneladas

        DATA: vlr_conversao TYPE decfloat34.

        IF <resultado>-meinb = 'KG' AND l_glp IS INITIAL.
          CLEAR: vlr_conversao.
          vlr_conversao = ( <resultado>-mbgbtr / 1000 ).
          CLEAR: <resultado>-mbgbtr.
          <resultado>-mbgbtr = vlr_conversao.

        ELSE.

          SELECT SINGLE dens FROM zfit0205 WHERE cd_item = @aux_cd_item INTO @DATA(aux_fat_conv).

          IF  aux_fat_conv IS NOT INITIAL. "VERIFICA SE O ITEM PODE TER CONVERSÃO

            IF <resultado>-meinb = 'M3' OR <resultado>-meinb = 'ST' OR <resultado>-meinb = 'MT'.
              CLEAR: vlr_conversao.
              vlr_conversao = ( <resultado>-mbgbtr * aux_fat_conv ).
              CLEAR: <resultado>-mbgbtr.
              <resultado>-mbgbtr = vlr_conversao.
            ENDIF.

          ENDIF.

          CLEAR: <resultado>-meinb.
          <resultado>-meinb = 'T'.
        ENDIF.
      ENDIF.

      MOVE-CORRESPONDING <resultado> TO it_saida1.
      it_saida1-perid = <resultado>-budat+0(6). "Periodo budat+0(6) Ano-Mes ou budat+0(4) Ano
      APPEND it_saida1.
    ENDLOOP.

    CLEAR: resultado.

    SORT it_saida1[] BY bukrs werks perid matnr.

    TYPES: BEGIN OF ty_saida2,
             werks    TYPE t001w-werks,
             matnr    TYPE zsis_ksb1-matnr,
             meinb    TYPE zsis_ksb1-meinb,
             mbgbtr   TYPE menge_d,
             perid(6) TYPE c,
           END OF ty_saida2.

    DATA it_saida2 TYPE STANDARD TABLE OF ty_saida2 WITH HEADER LINE.
    DATA aux_mbgbtr TYPE menge_d.

    LOOP AT it_saida1[] ASSIGNING FIELD-SYMBOL(<saida1>)
    GROUP BY ( werks = <saida1>-werks perid = <saida1>-perid matnr = <saida1>-matnr meinb = <saida1>-meinb )
    ASCENDING
    ASSIGNING FIELD-SYMBOL(<group>).
      CLEAR: aux_mbgbtr,it_saida2.
      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<grupo_saida1>).
        aux_mbgbtr = <grupo_saida1>-mbgbtr + aux_mbgbtr.
      ENDLOOP.

      it_saida2-werks = <group>-werks.
      it_saida2-perid = <group>-perid.
      it_saida2-matnr = <group>-matnr.
      it_saida2-meinb = <group>-meinb.
      it_saida2-mbgbtr = aux_mbgbtr.

      APPEND it_saida2.

    ENDLOOP.

    CLEAR: it_saida1[].

  ENDIF.

  LOOP AT it_saida2[] ASSIGNING FIELD-SYMBOL(<saida2>).

*         periodo  TYPE zfie0204-col_a,
*         unidade  TYPE zfie0204-col_b,
*         regfont  TYPE zfie0204-col_c,
*         descfont TYPE zfie0204-col_d,
*         combutil TYPE zfie0204-col_e,
*         qtdcons  TYPE zfie0204-col_f,

    SELECT SINGLE a~nm_item FROM zfit0200 AS a
    WHERE a~cd_indic = @vlr_indic AND  a~cd_item = @<saida2>-matnr INTO @it_saida-col_e .

    it_saida-col_a = |{ <saida2>-perid+0(4) }-{ <saida2>-perid+4(2) }|."periodo Ano-Mes

    SELECT SINGLE name1 FROM t001w INTO @DATA(aux_nmfilial) WHERE werks = @<saida2>-werks .
    CONDENSE <saida2>-werks NO-GAPS.
    REPLACE |  | IN aux_nmfilial WITH | |.
    it_saida-col_b = |{ <saida2>-werks } - { aux_nmfilial }|.
    it_saida-col_f = <saida2>-mbgbtr.
    it_saida-col_c = 'Tabela MSEG'.
    it_saida-col_d = 'Consumo de materiais da Tabela MSEG'.

    REPLACE ALL OCCURRENCES OF |.| IN it_saida-col_f WITH |,|.

    APPEND it_saida.

  ENDLOOP.

  CLEAR: it_saida2[].

  SORT it_saida[] BY col_b col_a col_e ASCENDING.

ENDFORM.

FORM exp_excel.

  TYPES: BEGIN OF ty_itab_line,
           cola TYPE char50,
           colb TYPE char50,
           colc TYPE char50,
           cold TYPE char50,
           cole TYPE char50,
           colf TYPE char50,
         END OF ty_itab_line.

  DATA: v_cola TYPE char50,
        v_colb TYPE char50,
        v_colc TYPE char50,
        v_cold TYPE char50,
        v_cole TYPE char50,
        v_colf TYPE char50.


  DATA lt_itab TYPE TABLE OF ty_itab_line.
  DATA lv_xstring TYPE xstring.
  DATA lv_subrc TYPE sy-subrc.
  DATA aux_indic_cd(6) TYPE c.
  DATA aux_indic_nm(50) TYPE c.

  DATA: go_alv        TYPE REF TO cl_salv_table.
  DATA: lr_columns    TYPE REF TO cl_salv_columns.
  DATA: lr_column     TYPE REF TO cl_salv_column.

  CLEAR: v_cola,v_colb,v_colc,v_cold,v_cole,v_colf,
  aux_indic_cd,aux_indic_nm,
  lt_itab,lv_xstring,lv_subrc.

  CASE vlr_indic.
    WHEN '005583'.
      v_cola = 'Período (AAAA-MM ou AAAA)'.
      v_colb  = 'Unidade'.
      v_colc  = 'Registro da fonte'.
      v_cold  = 'Descrição da fonte'.
      v_cole  = 'Combustível utilizado'.
      v_colf  = 'Quantidade consumida'.
    WHEN '005658'.
      v_cola = 'Período (AAAA-MM ou AAAA)'.
      v_colb  = 'Unidade'.
      v_colc  = 'Registro da frota'.
      v_cold  = 'Descrição da fonte'.
      v_cole  = 'Tipo de combustível'.
      v_colf  = 'Consumo anual'.
    WHEN '006710'.
      v_cola = 'Período (AAAA-MM ou AAAA)'.
      v_colb  = 'Unidade'.
      v_colc  = 'Registro da frota'.
      v_cold  = 'Descrição da frota'.
      v_cole  = 'Tipo de combustível'.
      v_colf  = 'Consumo de combustível - Anual'.
    WHEN '005638'.
      v_cola = 'Período (AAAA-MM ou AAAA)'.
      v_colb  = 'Unidade'.
      v_colc  = 'Registro da frota'..
      v_cold  = 'Descrição da frota'.
      v_cole  = 'Tipo de combustível'.
      v_colf  = 'Consumo de combustível - Anual'.
  ENDCASE.

  SELECT SINGLE nm_indic FROM zfit0201 WHERE cd_indic = @vlr_indic INTO @aux_indic_nm.

  aux_indic_cd = vlr_indic.
  CONDENSE aux_indic_cd NO-GAPS.

  APPEND VALUE #( cola = v_cola colb = v_colb colc = v_colc cold = v_cold cole = v_cole colf = v_colf ) TO lt_itab.
  LOOP AT it_saida[] ASSIGNING FIELD-SYMBOL(<saida>).
    APPEND VALUE #( cola = <saida>-col_a colb = <saida>-col_b colc = <saida>-col_c cold = <saida>-col_d cole = <saida>-col_e colf = <saida>-col_f ) TO lt_itab.
  ENDLOOP.

  CLEAR: it_saida[],go_alv.

  CALL FUNCTION 'SCMS_TEXT_TO_XSTRING'
    EXPORTING
      first_line = 0
      last_line  = 0
      mimetype   = ' '
    IMPORTING
      buffer     = lv_xstring
    TABLES
      text_tab   = lt_itab
    EXCEPTIONS
      failed     = 1
      OTHERS     = 2.
  IF sy-subrc <> 0.

    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA:
    go_xls TYPE REF TO cl_salv_table,
    lx_xml TYPE  xstring.

  CLEAR: go_xls,lx_xml.

  TRY.
      cl_salv_table=>factory(
      IMPORTING
        r_salv_table = go_xls
      CHANGING
        t_table      = lt_itab ).   "lt_data is your internal table

**Customiza Colunas
      lr_columns = go_xls->get_columns( ).
      "lr_columns->set_optimize( ' ' ).

**Colunas
      lr_column ?= lr_columns->get_column( 'COLA' ).
      "lr_column->set_optimized( abap_false ).
      "lr_column->set_short_text( 'Indicador Name:' ).
      "lr_column->set_medium_text( 'Indicador Name:' ).
      lr_column->set_long_text( 'Indicador Name:' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_output_length( '30' ).

      lr_column = lr_columns->get_column( 'COLB' ).
      lr_column->set_optimized( abap_false ).
      "lr_column->set_short_text( 'Escopo 1 - Combustão estacionária' ).
      "lr_column->set_medium_text( 'Combustão estacionária' ).
      lr_column->set_long_text( |{ aux_indic_nm }| ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_output_length( '50' ).

      lr_column = lr_columns->get_column( 'COLC' ).
      "lr_column->set_optimized( abap_false ).
      "lr_column->set_short_text( '5583' ).
      "lr_column->set_medium_text( '5583' ).
      lr_column->set_long_text( |{ aux_indic_cd }| ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_output_length( '50' ).

      lr_column = lr_columns->get_column( 'COLD' ).
      "lr_column->set_optimized( abap_false ).
      "lr_column->set_short_text( '4' ).
      lr_column->set_medium_text( '4' ).
      lr_column->set_long_text( '4' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_output_length( '50' ).

      lr_column = lr_columns->get_column( 'COLE' ).
      "lr_column->set_optimized( abap_false ).
      "lr_column->set_short_text( 'Comb.Util' ).
      "lr_column->set_medium_text( 'Comb.Utilizado' ).
      lr_column->set_long_text( '' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_output_length( '20' ).

      lr_column = lr_columns->get_column( 'COLF' ).
      "lr_column->set_optimized( abap_false ).
      "lr_column->set_short_text( '' ).
      "lr_column->set_medium_text( '' ).
      lr_column->set_long_text( '' ).
      "lr_column->set_alignment( if_salv_c_alignment=>centered ).
      lr_column->set_output_length( '20' ).

*... set list title
      DATA: lr_display_settings TYPE REF TO cl_salv_display_settings,
            l_title             TYPE lvc_title.

      l_title = |{ aux_indic_nm }_{ vlr_indic }_{ sy-datum }_{ sy-uzeit }|.
      lr_display_settings = go_xls->get_display_settings( ).
      lr_display_settings->set_list_header_size( '1' ).
      lr_display_settings->set_list_header( l_title ).
      lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
      lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
      lr_display_settings->set_list_header( l_title ).

    CATCH cx_root.
  ENDTRY.

  lx_xml = go_xls->to_xml( xml_type = '10' ). "XLSX

  CALL FUNCTION 'XML_EXPORT_DIALOG'
    EXPORTING
      i_xml                      = lx_xml
      i_default_extension        = 'XLSX'
      i_initial_directory        = ''
      i_default_file_name        = |{ l_title }.xlsx |
      i_mask                     = 'Excel (*.XLSX)|*.XLSX'
    EXCEPTIONS
      application_not_executable = 1
      OTHERS                     = 2.
ENDFORM.

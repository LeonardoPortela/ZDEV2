*&---------------------------------------------------------------------*
*& Include          ZMMR189_C1_P1
*&---------------------------------------------------------------------*
"CLASS p01_lcl_handle_events DEFINITION DEFERRED.

CLASS p01_lcl_handle_events DEFINITION.
  PUBLIC SECTION.

    METHODS:
      on_user_command_p01 FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_before_user_command_p01 FOR EVENT before_salv_function OF cl_salv_events
        IMPORTING e_salv_function,
      on_after_user_command_p01 FOR EVENT after_salv_function OF cl_salv_events
        IMPORTING e_salv_function.
*      on_after_refresh_p01 FOR EVENT after_refresh OF cl_gui_alv_grid
*        IMPORTING
*          sender.
ENDCLASS.


CLASS p01_lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command_p01.
    PERFORM show_function_info_p01 USING e_salv_function TEXT-i08.
  ENDMETHOD.

  METHOD on_before_user_command_p01.
    PERFORM show_function_info_p01 USING e_salv_function TEXT-i09.
  ENDMETHOD.

  METHOD on_after_user_command_p01.
    PERFORM show_function_info_p01 USING e_salv_function TEXT-i10.
  ENDMETHOD.

*  METHOD on_after_refresh_p01.
*
*    DATA: ls_layout TYPE lvc_s_layo,
*          lt_fcat   TYPE lvc_t_fcat.
*    FIELD-SYMBOLS: <ls_fcat> LIKE LINE OF lt_fcat.
*    TRY .
*
*        SET HANDLER on_after_refresh_p01
*          FOR ALL INSTANCES
*          ACTIVATION 'X'.
*
*
*        sender->get_frontend_fieldcatalog( IMPORTING et_fieldcatalog = lt_fcat ).
*        LOOP AT lt_fcat ASSIGNING <ls_fcat>.
*          IF <ls_fcat>-fieldname EQ 'CD_INDIC'.
*
*            "PERFORM completa_campos.
*
*          ENDIF.
*        ENDLOOP.
*        sender->set_frontend_fieldcatalog( lt_fcat ).
*
*        sender->set_frontend_layout( ls_layout ).
*        sender->set_ready_for_input( 1 ).
*      CATCH cx_salv_error.
*    ENDTRY.
*  ENDMETHOD.

ENDCLASS.
*&---------------------------------------------------------------------*
*&      Form  show_function_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM show_function_info_p01 USING p01_i_function TYPE salv_de_function
                              p01_i_text     TYPE string.
  DATA: p01_l_string TYPE string.
  "concatenate i_text i_function into l_string separated by space.
  "message i000(0k) with l_string.

  CASE p01_i_function.
*     Make ALV as Editable ALV
    WHEN 'EDIT_p01'.
      p01_ls_api = p01_gr_table->extended_grid_api( ).
      p01_ls_edit = p01_ls_api->editable_restricted( ).

      TRY.
          p01_ls_edit->set_attributes_for_columnname(
      EXPORTING
        columnname = 'CD_INDIC'
        all_cells_input_enabled = abap_true
      ).
          p01_ls_edit->set_attributes_for_columnname(
            EXPORTING
              columnname = 'NM_INDIC'
              all_cells_input_enabled = abap_false
              ).
          p01_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'CD_ITEM'
            all_cells_input_enabled = abap_true
            ).
          p01_ls_edit->set_attributes_for_columnname(
          EXPORTING
            columnname = 'NM_ITEM'
            all_cells_input_enabled = abap_false
            ).
        CATCH cx_salv_not_found.
      ENDTRY.

      p01_ls_edit->validate_changed_data(
    ).

      PERFORM p01_atualiza_P1.

    WHEN 'SAVE_p01'.
      TRY.
          p01_ls_edit->validate_changed_data(
                IMPORTING
                  is_input_data_valid = DATA(p01_s)
                ).
        CATCH cx_salv_not_found.
      ENDTRY.

      PERFORM p01_atualiza_P1.


* Input is valid, we can save the data
      IF p01_s = 'X'.
* You will need to implement your own save method, based on your own data
        " save( ).
      ENDIF.


    WHEN 'IMPORT_p01'.

      SELECT DISTINCT a~maktx, a~matnr,c~wgbez60,b~matkl
               FROM makt AS a
               INNER JOIN mara AS b ON b~matnr = a~matnr
               INNER JOIN t023t AS c ON c~matkl = b~matkl AND c~spras = 'P'
               WHERE a~spras = 'P'
               AND b~matkl IN ('658845','555720','658830','658840','658810','658815','658820','658710','658850','658720','557245','658835' )
               AND a~matnr NOT IN ( SELECT matnr FROM zfit0202 where matnr is not null )
               INTO TABLE @it_materiais.

      CHECK sy-subrc = 0.

      SORT it_materiais BY matnr ASCENDING.

      LOOP AT it_materiais ASSIGNING FIELD-SYMBOL(<it_materiais>).

        it_dados_p01-matnr = <it_materiais>-matnr.
        it_dados_p01-maktx = <it_materiais>-maktx.
        it_dados_p01-matkl = <it_materiais>-matkl.
        it_dados_p01-wgbez60 = <it_materiais>-wgbez60.

        APPEND it_dados_p01.

      ENDLOOP.

      SORT it_dados_p01[] BY matnr ASCENDING.

      DELETE ADJACENT DUPLICATES FROM it_dados_p01[] COMPARING matnr.

      SORT it_dados_p01[] BY matkl ASCENDING.

      MODIFY zfit0202 FROM TABLE it_dados_p01[].
      COMMIT WORK.


      CLEAR: it_dados_p01.
      FREE: it_dados_p01[],it_materiais[].

      PERFORM p01_atualiza_P1.

      "cl_demo_output=>display( it_dados_p01 ).

    WHEN 'DELETE_p01'.

      DATA: p01_lo_selections TYPE REF TO cl_salv_selections.
      DATA p01_lt_rows TYPE salv_t_row.
      DATA p01_ls_row TYPE int4.
      DATA: p01_msg_text TYPE char45.

      p01_lt_rows = p01_gr_table->get_selections( )->get_selected_rows( ).

      IF p01_lt_rows IS INITIAL.
        p01_msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE p01_msg_text TYPE 'I'.

      ELSE.

        LOOP AT p01_lt_rows INTO p01_ls_row.

          READ TABLE it_saida_p01 INDEX p01_ls_row.
          DELETE FROM zfit0202 WHERE matnr = it_saida_p01-matnr.
          COMMIT WORK.

        ENDLOOP.

      ENDIF.

      PERFORM p01_atualiza_P1.

    WHEN 'REFRESH_p01'.
      PERFORM   p01_atualiza_P1.
  ENDCASE.

ENDFORM.                    " show_function_info

*&---------------------------------------------------------------------*
*&      Form  set_columns_technical
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM set_columns_technical_p01 USING p01_ir_columns TYPE REF TO cl_salv_columns.

*...Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)p01_lr_column
  TRY.
      p01_lr_column = p01_ir_columns->get_column( 'MAKTX' ).
      p01_lr_column->set_short_text( 'Desc.Mat.' ).
      p01_lr_column->set_medium_text( 'Descrição Material' ).
      p01_lr_column->set_long_text( 'Desccrição Material' ).

      p01_lr_column = p01_ir_columns->get_column( 'MATNR' ).
      p01_lr_column->set_short_text( 'Cod.Mat.' ).
      p01_lr_column->set_medium_text( 'Código Material' ).
      p01_lr_column->set_long_text( 'Código Material' ).

      p01_lr_column = p01_ir_columns->get_column( 'MATKL' ).
      p01_lr_column->set_short_text( 'Cod.Grupo' ).
      p01_lr_column->set_medium_text( 'Código Grupo' ).
      p01_lr_column->set_long_text( 'Código Grupo' ).

      p01_lr_column = p01_ir_columns->get_column( 'WGBEZ60' ).
      p01_lr_column->set_short_text( 'Desc.Grupo' ).
      p01_lr_column->set_medium_text( 'Descrição Grupo' ).
      p01_lr_column->set_long_text( 'Descrição Grupo' ).

      p01_lr_column = p01_ir_columns->get_column( 'CD_INDIC' ).
      p01_lr_column->set_short_text( 'Cod.ID.SIS' ).
      p01_lr_column->set_medium_text( 'Codt.ID.SIS' ).
      p01_lr_column->set_long_text( 'Cód. Ident. SIS' ).

      p01_lr_column = p01_ir_columns->get_column( 'NM_INDIC' ).
      p01_lr_column->set_short_text( 'NM.ID.SIS' ).
      p01_lr_column->set_medium_text( 'Desc.ID.SIS' ).
      p01_lr_column->set_long_text( 'Identificador SIS' ).

      p01_lr_column = p01_ir_columns->get_column( 'CD_ITEM' ).
      p01_lr_column->set_short_text( 'Cód.Item' ).
      p01_lr_column->set_medium_text( 'Cód. Item SIS' ).
      p01_lr_column->set_long_text( 'Código Item SIS' ).

      p01_lr_column = p01_ir_columns->get_column( 'NM_ITEM' ).
      p01_lr_column->set_short_text( 'Desc.Item' ).
      p01_lr_column->set_medium_text( 'Desc. Item SIS' ).
      p01_lr_column->set_long_text( 'Descrição Item SIS' ).

      p01_lr_column = p01_ir_columns->get_column( 'MANDT' ).
      p01_lr_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO p01_lex_not_found.
      "write some error handling
  ENDTRY.

ENDFORM.

FORM container_main_p1.

  PERFORM dados_saida.

  PERFORM DOCKER_P1. "AQUI FICA A CL_SALV

*...Add DDIC reference for F4 generating


  DATA p01_gr_salv_func TYPE REF TO cl_salv_functions .
  p01_gr_salv_func = p01_gr_table->get_functions( ).
  p01_gr_salv_func->set_all( abap_true ).

*... §3.1 activate ALV generic Functions

  p01_lr_functions = p01_gr_table->get_functions( ).
  p01_lr_functions->set_all( abap_true ).

*... §3.2 include own functions
  p01_l_text_EDIT = 'Editar'.
  p01_l_icon_EDIT = icon_edit_file.
  p01_l_text_SAVE = 'Salvar'.
  p01_l_icon_SAVE = icon_save_as_template.
  p01_l_text_getitem = 'Importar'.
  p01_l_icon_getitem = icon_import.
  p01_l_text_DEL = 'Remove'.
  p01_l_icon_DEL = icon_delete.
  p01_l_text_ref = 'Atualizar'.
  p01_l_icon_ref = icon_refresh.
  TRY.
      p01_lr_functions->add_function(
        name     = 'EDIT_p01'
        icon     = p01_l_icon_EDIT
        text     = p01_l_text_EDIT
        tooltip  = 'Edit Item'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      p01_lr_functions->add_function(
       name     = 'SAVE_p01'
       icon     = p01_l_icon_SAVE
       text     = p01_l_text_SAVE
       tooltip  = 'Save Item'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      p01_lr_functions->add_function(
       name     = 'IMPORT_p01'
       icon     = p01_l_icon_getitem
       text     = p01_l_text_getitem
       tooltip  = 'Buscar Itens'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      p01_lr_functions->add_function(
      name     = 'DELETE_p01'
      icon     = p01_l_icon_del
      text     = p01_l_text_del
      tooltip  = 'Deletar Itens'
      position = if_salv_c_function_position=>right_of_salv_functions ).

      p01_lr_functions->add_function(
name     = 'REFRESH_p01'
icon     = p01_l_icon_REF
text     = p01_l_text_REF
tooltip  = 'Atualizar Grid'
position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

*... set the columns technical
  DATA: p01_lr_columns    TYPE REF TO cl_salv_columns,
        p01_lr_column     TYPE REF TO cl_salv_column_table,
        p01_lR_selections TYPE REF TO cl_salv_selections.

  DATA: p01_lR_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        p01_lv_key    TYPE salv_s_layout_key.

  p01_lr_columns = p01_gr_table->get_columns( ).


  p01_lr_columns->set_column_position(  columnname = 'MATNR' position = 1 ).
  p01_lr_columns->set_column_position(  columnname = 'MAKTX' position = 2 ).
  p01_lr_columns->set_column_position(  columnname = 'WGBEZ60' position = 3 ).
  p01_lr_columns->set_column_position(  columnname = 'MATKL' position = 4 ).
  p01_lr_columns->set_column_position(  columnname = 'CD_INDIC' position = 5 ).
  p01_lr_columns->set_column_position(  columnname = 'NM_INDIC' position = 6 ).
  p01_lr_columns->set_column_position(  columnname = 'CD_ITEM' position = 7 ).
  p01_lr_columns->set_column_position(  columnname = 'NM_ITEM' position = 8 ).

  " F4 DDIC
  DATA p01_lv_ddic TYPE salv_s_ddic_reference.

  TRY.
      p01_lr_column ?= p01_lr_columns->get_column( columnname = 'NM_INDIC' ).
      p01_lv_ddic = VALUE #( table = 'ZFIT0200' field = 'NM_INDIC').
      p01_lr_column->set_ddic_reference( p01_lv_ddic  ). "EXPORTING value = C22_lv_ddic
      p01_lr_column->set_f4(  if_salv_c_bool_sap=>true ).

      p01_lr_column ?= p01_lr_columns->get_column( columnname = 'NM_ITEM' ).
      p01_lv_ddic = VALUE #( table = 'ZFIT0200' field = 'NM_ITEM').
      p01_lr_column->set_ddic_reference( p01_lv_ddic  ). "EXPORTING value = C22_lv_ddic
      p01_lr_column->set_f4(  if_salv_c_bool_sap=>true ).



    CATCH cx_salv_not_found.
  ENDTRY.

  p01_lr_columns->set_optimize( abap_true ).

  PERFORM set_columns_technical_p01 USING p01_lr_columns.

*... §4 set hotspot column
  TRY.
      p01_lr_column ?= p01_lr_columns->get_column( 'MATNR' ).
      p01_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

*... §6 register to the events of cl_salv_table
  DATA: p01_lr_events TYPE REF TO cl_salv_events_table.

  p01_lr_events = p01_gr_table->get_event( ).

  DATA: p01_gr_events TYPE REF TO p01_lcl_handle_events.

  CREATE OBJECT p01_gr_events.

*... §6.1 register to the event USER_COMMAND
  SET HANDLER p01_gr_events->on_user_command_p01 FOR p01_lr_events.
  SET HANDLER p01_gr_events->on_before_user_command_p01 FOR p01_lr_events.
  SET HANDLER p01_gr_events->on_after_user_command_p01 FOR p01_lr_events.
  "SET HANDLER p01_gr_events->on_after_refresh_p01 FOR ALL INSTANCES.

*... set list title
  DATA: p01_lr_display_settings TYPE REF TO cl_salv_display_settings,
        p01_l_title             TYPE lvc_title.

  p01_l_title = 'Depara de Materiais e Indicadores / Itens'.
  p01_lr_display_settings = p01_gr_table->get_display_settings( ).
  p01_lr_display_settings->set_list_header( p01_l_title ).

* Enable cell selection mode
  p01_lR_selections = p01_gr_table->get_selections( ).
  p01_lR_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  p01_lv_key-report = sy-repid.
  p01_lR_layout = p01_gr_table->get_layout( ).
  p01_lR_layout->set_key( p01_lv_key ).
  p01_lR_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  p01_lR_layout->set_default( abap_true ).

*... §7 display the table
  p01_gr_table->display( ).

ENDFORM.

FORM p01_atualiza_P1.

  REFRESH it_dados_p01.
  PERFORM dados_saida.
  p01_gr_table->refresh( ).
  p01_gr_table->display( ).

ENDFORM.

FORM dados_saida.

  SELECT * FROM zfit0202 INTO TABLE @it_saida_p01 where matnr is not null.

ENDFORM.

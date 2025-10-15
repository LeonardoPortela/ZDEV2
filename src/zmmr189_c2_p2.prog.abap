*&---------------------------------------------------------------------*
*& Include          ZMMR189_C2_P2
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       CLASS cl_event_handler DEFINITION
*----------------------------------------------------------------------*

REPORT ZMMR189_C2_P2.

CLASS cl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS on_before_salv_function_p22         " BEFORE_SALV_FUNCTION
      FOR EVENT if_salv_events_functions~before_salv_function
      OF cl_salv_events_table
      IMPORTING e_salv_function.

    CLASS-METHODS on_after_salv_function_p22          " AFTER_SALV_FUNCTION
      FOR EVENT if_salv_events_functions~after_salv_function
      OF cl_salv_events_table
      IMPORTING e_salv_function.

    CLASS-METHODS on_added_function_p22               " ADDED_FUNCTION
      FOR EVENT if_salv_events_functions~added_function
      OF cl_salv_events_table
      IMPORTING e_salv_function.

    CLASS-METHODS on_top_of_page_p22                  " TOP_OF_PAGE
      FOR EVENT if_salv_events_list~top_of_page
      OF cl_salv_events_table
      IMPORTING r_top_of_page
                page
                table_index.

    CLASS-METHODS on_end_of_page_p22                  " END_OF_PAGE
      FOR EVENT if_salv_events_list~end_of_page
      OF cl_salv_events_table
      IMPORTING r_end_of_page
                page.

    CLASS-METHODS on_double_click_p22                 " DOUBLE_CLICK
      FOR EVENT if_salv_events_actions_table~double_click
      OF cl_salv_events_table
      IMPORTING row
                column.

    CLASS-METHODS on_link_click_p22                   " LINK_CLICK
      FOR EVENT if_salv_events_actions_table~link_click
      OF cl_salv_events_table
      IMPORTING row
                column.



ENDCLASS.                    "cl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_event_handler IMPLEMENTATION.


  METHOD on_before_salv_function_p22.
    PERFORM show_function_info_p22 USING e_salv_function.
  ENDMETHOD.                    "on_before_salv_function

  METHOD on_after_salv_function_p22.
    PERFORM show_function_info_p22 USING e_salv_function.

*    sender->set_delay_change_selection(
*      exporting
*        time   =  100    " Time in Milliseconds
*      exceptions
*        error  = 1
*        others = 2
*    ).
*    if sy-subrc <> 0.
*     message id sy-msgid type sy-msgty number sy-msgno
*                with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    endif.
*
*    sender->register_delayed_event(
*      exporting
*        i_event_id =  sender->mc_evt_delayed_change_select
*      exceptions
*        error      = 1
*        others     = 2
*    ).
*    if sy-subrc <> 0.


  ENDMETHOD.                    "on_after_salv_function

  METHOD on_added_function_p22.
    PERFORM show_function_info_p22 USING e_salv_function.
  ENDMETHOD.                    "on_added_function

  METHOD on_top_of_page_p22.
    BREAK-POINT.
  ENDMETHOD.                    "on_top_of_page

  METHOD on_end_of_page_p22.
    BREAK-POINT.
  ENDMETHOD.                    "on_end_of_page

  METHOD on_double_click_p22.
    BREAK-POINT.

    IF row IS NOT INITIAL.

    ENDIF.

    IF column IS NOT INITIAL.

    ENDIF..


  ENDMETHOD.                    "on_double_click

  METHOD on_link_click_p22.
    BREAK-POINT.
  ENDMETHOD.                    "on_link_click

ENDCLASS.



**&---------------------------------------------------------------------*
**&      Form  show_function_info
**&---------------------------------------------------------------------*
**       text

FORM show_function_info_p22 USING p22_i_function.
  DATA: p22_l_string TYPE string.

  CASE p22_i_function.
*     Make ALV as Editable ALV
    WHEN 'EDIT_p22'.
      p22_ls_api = p22_gr_table->extended_grid_api( ).
      p22_ls_edit = p22_ls_api->editable_restricted( ).

      TRY.
          p22_ls_edit->set_attributes_for_columnname(
      EXPORTING
        columnname = 'CD_INDIC'
        all_cells_input_enabled = abap_true
      ).
          p22_ls_edit->set_attributes_for_columnname(
            EXPORTING
              columnname = 'NM_INDIC'
              all_cells_input_enabled = abap_true
              ).
          p22_ls_edit->set_attributes_for_columnname(
 EXPORTING
   columnname = 'CD_ITEM'
   all_cells_input_enabled = abap_true
   ).
          p22_ls_edit->set_attributes_for_columnname(
EXPORTING
columnname = 'NM_ITEM'
all_cells_input_enabled = abap_true
).
        CATCH cx_salv_not_found.
      ENDTRY.

      p22_ls_edit->validate_changed_data(
    ).
      PERFORM atuliza_c2_p2.



    WHEN 'SAVE_p22'.

      TRY.
          MODIFY zfit0200 FROM TABLE it_dados_p22.
          COMMIT WORK.
        CATCH cx_salv_not_found.
      ENDTRY.

      PERFORM atuliza_c2_p2.

    WHEN 'DELETE_p22'.


      DATA: p22_lo_selections TYPE REF TO cl_salv_selections.
      DATA p22_lt_rows TYPE salv_t_row.
      DATA p22_ls_row TYPE int4.
      DATA: p22_msg_text TYPE char45.

      p22_lt_rows = p22_gr_table->get_selections( )->get_selected_rows( ).

      IF p22_lt_rows IS INITIAL.
        p22_msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE p22_msg_text TYPE 'I'.

      ELSE.

        LOOP AT p22_lt_rows INTO p22_ls_row.

          READ TABLE it_dados_p22 INDEX p22_ls_row.
          DELETE FROM zfit0200 WHERE cd_indic = it_dados_p22-cd_indic AND cd_item = it_dados_p22-cd_item.
          COMMIT WORK.

        ENDLOOP.

      ENDIF.
      PERFORM atuliza_c2_p2.

    WHEN 'REFRESH_p22'.
      PERFORM atuliza_c2_p2.

          WHEN '&F03' OR '&ENTER'. " Check for Enter key press
      BREAK-POINT.
    WHEN OTHERS.
      BREAK-POINT.

  ENDCASE.



ENDFORM.                    " show_function_info
*
**&---------------------------------------------------------------------*
**&      Form  set_columns_technical
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
FORM set_columns_technical_p22 USING p22_ir_columns TYPE REF TO cl_salv_columns.

*...Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)p22_lr_column
  TRY.

      p22_lr_column = p22_ir_columns->get_column( 'CD_INDIC' ).
      p22_lr_column->set_short_text( 'COD IND' ).
      p22_lr_column->set_medium_text( 'COD IND' ).
      p22_lr_column->set_long_text( 'COD IND' ).

      p22_lr_column = p22_ir_columns->get_column( 'NM_INDIC' ).
      p22_lr_column->set_short_text( 'NM IND' ).
      p22_lr_column->set_medium_text( 'NM IND' ).
      p22_lr_column->set_long_text( 'NM IND' ).

      p22_lr_column = p22_ir_columns->get_column( 'CD_ITEM' ).
      p22_lr_column->set_short_text( 'COD ITEN' ).
      p22_lr_column->set_medium_text( 'COD ITEN' ).
      p22_lr_column->set_long_text( 'COD ITEN' ).

      p22_lr_column = p22_ir_columns->get_column( 'NM_ITEM' ).
      p22_lr_column->set_short_text( 'NM ITEM' ).
      p22_lr_column->set_medium_text( 'NM ITEM' ).
      p22_lr_column->set_long_text( 'NM ITEM' ).

      p22_lr_column = p22_ir_columns->get_column( 'MANDT' ).
      p22_lr_column->set_visible( if_salv_c_bool_sap=>false ).
      p22_lr_column = p22_ir_columns->get_column( 'NM_INDIC' ).
      p22_lr_column->set_visible( if_salv_c_bool_sap=>false ).
    CATCH cx_salv_not_found INTO p22_lex_not_found.
      "write some error handling
  ENDTRY.

ENDFORM.

FORM container_main_p22.

  PERFORM seleciona_dados_p22.
  PERFORM docker_p22.


*...Add DDIC reference for F4 generating

  DATA p22_gr_salv_func TYPE REF TO cl_salv_functions .
  p22_gr_salv_func = p22_gr_table->get_functions( ).
  p22_gr_salv_func->set_all( abap_true ).

*... §3.1 activate ALV generic Functions

  p22_lr_functions = p22_gr_table->get_functions( ).
  p22_lr_functions->set_all( abap_true ).

*... §3.2 include own functions
  p22_l_text_EDIT = 'Editar'.
  p22_l_icon_EDIT = icon_edit_file.
  p22_l_text_SAVE = 'Salvar'.
  p22_l_icon_SAVE = icon_save_as_template.
  p22_l_text_DEL = 'Remover'.
  p22_l_icon_DEL = icon_delete.
  p22_l_text_REF = 'Atualizar'.
  p22_l_icon_REF = icon_refresh.
  TRY.
      p22_lr_functions->add_function(
        name     = 'EDIT_p22'
        icon     = p22_l_icon_EDIT
        text     = p22_l_text_EDIT
        tooltip  = 'Edit Item'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      p22_lr_functions->add_function(
       name     = 'SAVE_p22'
       icon     = p22_l_icon_SAVE
       text     = p22_l_text_SAVE
       tooltip  = 'Save Item'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      p22_lr_functions->add_function(
       name     = 'DELETE_p22'
       icon     = p22_l_icon_del
       text     = p22_l_text_del
       tooltip  = 'Deleta Dados'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      p22_lr_functions->add_function(
 name     = 'REFRESH_p22'
 icon     = p22_l_icon_REF
 text     = p22_l_text_REF
 tooltip  = 'Atualizar Dados'
 position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

*... set the columns technical
  DATA: p22_lr_columns    TYPE REF TO cl_salv_columns,
        p22_lr_column     TYPE REF TO cl_salv_column_table,
        p22_lR_selections TYPE REF TO cl_salv_selections.

  DATA: p22_lR_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        p22_lv_key    TYPE salv_s_layout_key.

  p22_lr_columns = p22_gr_table->get_columns( ).

  p22_lr_columns->set_column_position(  columnname = 'CD_INDIC' position = 1 ).
  p22_lr_columns->set_column_position(  columnname = 'NM_INDIC' position = 2 ).
  p22_lr_columns->set_column_position(  columnname = 'CD_ITEM' position = 3 ).
  p22_lr_columns->set_column_position(  columnname = 'NM_ITEM' position = 4 ).

  p22_lr_columns->set_optimize( abap_true ).

  PERFORM set_columns_technical_p22 USING p22_lr_columns.

*... F4 DDIC

  DATA p22_lv_ddic TYPE salv_s_ddic_reference.

  TRY.
      p22_lr_column ?= p22_lr_columns->get_column( columnname = 'NM_INDIC' ).
      p22_lv_ddic = VALUE #( table = 'ZFIT0201' field = 'NM_INDIC').
      p22_lr_column->set_ddic_reference( p22_lv_ddic  ). "EXPORTING value = p22_lv_ddic
      p22_lr_column->set_f4(  if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.
  ENDTRY.


*... §4 set hotspot column
  TRY.
      p22_lr_column ?= p22_lr_columns->get_column( 'MATNR' ).
      p22_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
  ENDTRY.

*... §6 register to the events of cl_salv_table
  DATA p22_lr_events TYPE REF TO cl_salv_events_table.
  p22_lr_events = p22_gr_table->get_event( ).




*... §6.1 register to the event USER_COMMAND
  SET HANDLER cl_event_handler=>on_before_salv_function_p22 FOR p22_lr_events.
  SET HANDLER cl_event_handler=>on_after_salv_function_p22 FOR p22_lr_events.
  SET HANDLER cl_event_handler=>on_added_function_p22 FOR p22_lr_events.
  SET HANDLER cl_event_handler=>on_top_of_page_p22 FOR p22_lr_events.
  SET HANDLER cl_event_handler=>on_end_of_page_p22 FOR p22_lr_events.
  SET HANDLER cl_event_handler=>on_double_click_p22 FOR p22_lr_events.
  SET HANDLER cl_event_handler=>on_link_click_p22 FOR p22_lr_events.


*... set list title
  DATA: p22_lr_display_settings TYPE REF TO cl_salv_display_settings,
        p22_l_title             TYPE lvc_title.

  p22_l_title = 'Cadastro de Itens por Indicador'.
  p22_lr_display_settings = p22_gr_table->get_display_settings( ).
  p22_lr_display_settings->set_list_header_size( '1' ).
  p22_lr_display_settings->set_list_header( p22_l_title ).


* Enable cell selection mode
  p22_lR_selections = p22_gr_table->get_selections( ).
  p22_lR_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  p22_lv_key-report = sy-repid.
  p22_lR_layout = p22_gr_table->get_layout( ).
  p22_lR_layout->set_key( p22_lv_key ).
  p22_lR_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  p22_lR_layout->set_default( abap_true ).

    "just to triger handler
    p22_gr_table->refresh( ).

*... §7 display the table
  p22_gr_table->display( ).

ENDFORM.

FORM atuliza_c2_p2.
  REFRESH it_dados_p22.
  PERFORM seleciona_dados_p22.
  p22_gr_table->refresh( ).
  p22_gr_table->display( ).
ENDFORM.

FORM p22_pega_dados_cell_column.

*  CLEAR: c2p2_oo1,c2p2_oo2,c2p2_oo3,c2p2_oo4,c2p2_oo5.
*
*  c2p2_oo1 = p22_gr_table->get_selections( )->get_current_cell( ).
*  c2p2_oo2 = p22_gr_table->get_selections( )->get_selected_cells( ).
*  c2p2_oo3 = p22_gr_table->get_selections( )->get_selected_columns( ).
*  c2p2_oo4 = p22_gr_table->get_selections( )->get_selected_rows( ).
*  c2p2_oo5 = p22_gr_table->get_selections( )->get_selection_mode( ).

ENDFORM.


FORM completa_campos.
  LOOP AT it_dados_p22 ASSIGNING FIELD-SYMBOL(<p22_it_dados_p22>).

    IF <p22_it_dados_p22>-cd_indic IS NOT INITIAL.

      SELECT SINGLE nm_indic FROM zfit0201
        INTO <p22_it_dados_p22>-nm_indic
        WHERE cd_indic = <p22_it_dados_p22>-cd_indic.

    ENDIF.

  ENDLOOP.
ENDFORM.

 FORM seleciona_dados_p22.
  SELECT * FROM zfit0200 INTO TABLE @it_dados_p22 WHERE cd_indic IS NOT NULL.
ENDFORM.

FORM docker_p22.
  TRY.
      cl_salv_table=>factory(
*        EXPORTING
*      r_container = painel1
*      container_name = 'CONTAINER'
        IMPORTING
          r_salv_table   = p22_gr_table
        CHANGING
          t_table        = it_dados_p22[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

  ENDFORM.

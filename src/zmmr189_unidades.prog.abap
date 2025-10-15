*&---------------------------------------------------------------------*
*& Report ZMMR189_unidades
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr189_unidades.

"TABLES: ZFIT0204.

DATA: it_dados_p22 TYPE STANDARD TABLE OF zfit0204 WITH HEADER LINE.
DATA: p22_g_okcode TYPE sy-ucomm.
DATA: p22_lr_column TYPE REF TO cl_salv_column.
DATA p22_ir_columns           TYPE REF TO cl_salv_columns_table.
DATA p22_lex_not_found        TYPE REF TO cx_salv_not_found.
DATA: p22_gr_table   TYPE REF TO cl_salv_table.
DATA: p22_ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
      p22_ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.
DATA: p22_lr_functions TYPE REF TO cl_salv_functions,
      p22_l_text_ref   TYPE string,
      p22_l_icon_ref   TYPE string,
      p22_l_text_del   TYPE string,
      p22_l_icon_del   TYPE string,
      p22_l_text_edit  TYPE string,
      p22_l_icon_edit  TYPE string,
      p22_l_text_save  TYPE string,
      p22_l_icon_save  TYPE string,
      p22_l_text_bck   TYPE string,
      p22_l_icon_bck   TYPE string,
      p22_l_text_can   TYPE string,
      p22_l_icon_can   TYPE string.

DATA:
  container_main TYPE REF TO cl_gui_custom_container,
  painel_control TYPE REF TO cl_gui_splitter_container,
  painel1        TYPE REF TO cl_gui_container,
  painel2        TYPE REF TO cl_gui_container.

*SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE t1.
*
*  SELECT-OPTIONS p_cdind FOR zfit0204-cd_indic NO INTERVALS.
*
*SELECTION-SCREEN END OF BLOCK a.


START-OF-SELECTION.
*  cl_demo_output=>display( it_saida ).
  PERFORM container_main_p22.
  PERFORM display_grid_unidades.

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
              columnname              = 'CD_UNID'
              all_cells_input_enabled = abap_true
          ).

          p22_ls_edit->set_attributes_for_columnname(
            EXPORTING
              columnname              = 'NM_UNID'
              all_cells_input_enabled = abap_true
          ).
        CATCH cx_salv_not_found.
      ENDTRY.

      p22_ls_edit->validate_changed_data(
    ).
      PERFORM atuliza_c2_p2.



    WHEN 'SAVE_p22'.

      PERFORM check_values.

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
          DELETE FROM zfit0204 WHERE cd_unid = it_dados_p22-cd_unid.
          COMMIT WORK.

        ENDLOOP.

      ENDIF.
      PERFORM atuliza_c2_p2.

    WHEN 'REFRESH_p22'.
      PERFORM atuliza_c2_p2.

    WHEN '&F03' OR '&ENTER'. " Check for Enter key press
      BREAK-POINT.

    WHEN 'CANCEL_p22'.
      LEAVE PROGRAM.
    WHEN 'BACK_p22'.
      SET SCREEN 0.
      LEAVE SCREEN.

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

      p22_lr_column = p22_ir_columns->get_column( 'CD_UNID' ).
      p22_lr_column->set_short_text( 'COD UNID' ).
      p22_lr_column->set_medium_text( 'COD UNID' ).
      p22_lr_column->set_long_text( 'COD UNID' ).

      p22_lr_column = p22_ir_columns->get_column( 'NM_UNID' ).
      p22_lr_column->set_short_text( 'NM UNID' ).
      p22_lr_column->set_medium_text( 'NM UNID' ).
      p22_lr_column->set_long_text( 'NM UNID' ).

      p22_lr_column = p22_ir_columns->get_column( 'OID' ).
      p22_lr_column->set_visible( if_salv_c_bool_sap=>false ).

      p22_lr_column = p22_ir_columns->get_column( 'MANDT' ).
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
  p22_l_text_edit = 'Editar'.
  p22_l_icon_edit = icon_edit_file.
  p22_l_text_save = 'Salvar'.
  p22_l_icon_save = icon_save_as_template.
  p22_l_text_del = 'Remover'.
  p22_l_icon_del = icon_delete.
  p22_l_text_ref = 'Atualizar'.
  p22_l_icon_ref = icon_refresh.
  p22_l_text_bck = 'Voltar'.
  p22_l_icon_bck = icon_retrieve.
  p22_l_text_can = 'Cancelar'.
  p22_l_icon_can = icon_cancel.
  TRY.
      p22_lr_functions->add_function(
        name     = 'EDIT_p22'
        icon     = p22_l_icon_edit
        text     = p22_l_text_edit
        tooltip  = 'Edit Item'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      p22_lr_functions->add_function(
        name     = 'SAVE_p22'
        icon     = p22_l_icon_save
        text     = p22_l_text_save
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
        icon     = p22_l_icon_ref
        text     = p22_l_text_ref
        tooltip  = 'Atualizar Dados'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      p22_lr_functions->add_function(
        name     = 'BACK_p22'
        icon     = p22_l_icon_bck
        text     = p22_l_text_bck
        tooltip  = 'Voltar'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      p22_lr_functions->add_function(
        name     = 'CANCEL_p22'
        icon     = p22_l_icon_can
        text     = p22_l_text_can
        tooltip  = 'Cancelar'
        position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

*... set the columns technical
  DATA: p22_lr_columns    TYPE REF TO cl_salv_columns,
        p22_lr_column     TYPE REF TO cl_salv_column_table,
        p22_lr_selections TYPE REF TO cl_salv_selections.

  DATA: p22_lr_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        p22_lv_key    TYPE salv_s_layout_key.

  p22_lr_columns = p22_gr_table->get_columns( ).

  p22_lr_columns->set_column_position(  columnname = 'CD_UNID' position = 1 ).
  p22_lr_columns->set_column_position(  columnname = 'NM_UNID' position = 2 ).


  p22_lr_columns->set_optimize( abap_true ).

  PERFORM set_columns_technical_p22 USING p22_lr_columns.

*... F4 DDIC

  DATA p22_lv_ddic TYPE salv_s_ddic_reference.

*  TRY.
*      p22_lr_column ?= p22_lr_columns->get_column( columnname = 'NM_INDIC' ).
*      p22_lv_ddic = VALUE #( table = 'ZFIT0201' field = 'NM_INDIC').
*      p22_lr_column->set_ddic_reference( p22_lv_ddic  ). "EXPORTING value = p22_lv_ddic
*      p22_lr_column->set_f4(  if_salv_c_bool_sap=>true ).
*    CATCH cx_salv_not_found.
*  ENDTRY.


*... §4 set hotspot column
*  TRY.
*      p22_lr_column ?= p22_lr_columns->get_column( 'MATNR' ).
*      p22_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.

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
  p22_lr_selections = p22_gr_table->get_selections( ).
  p22_lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  p22_lv_key-report = sy-repid.
  p22_lr_layout = p22_gr_table->get_layout( ).
  p22_lr_layout->set_key( p22_lv_key ).
  p22_lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  p22_lr_layout->set_default( abap_true ).

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

    IF <p22_it_dados_p22>-cd_unid IS NOT INITIAL.



    ENDIF.

  ENDLOOP.
ENDFORM.

FORM seleciona_dados_p22.
  SELECT * FROM zfit0204 INTO TABLE @it_dados_p22 WHERE cd_unid <> ''.
ENDFORM.

FORM docker_p22.
  TRY.
      cl_salv_table=>factory(
        EXPORTING
          r_container    = painel1
          container_name = 'CONTAINER'
        IMPORTING
          r_salv_table   = p22_gr_table
        CHANGING
          t_table        = it_dados_p22[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

ENDFORM.

FORM display_grid_unidades.

  CALL SCREEN 0200.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  zmmr189_unidades_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zmmr189_unidades_pbo OUTPUT.
  PERFORM zmmr189_unidades_pbo.
ENDMODULE.                 " zmmr189_unidades_pbo  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  zmmr189_unidades_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zmmr189_unidades_pai INPUT.
  PERFORM zmmr189_unidades_pai.
ENDMODULE.                 " zmmr189_unidades_pai  INPUT

*&---------------------------------------------------------------------*
*&      Form  zmmr189_unidades_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zmmr189_unidades_pbo .

  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.
  PERFORM docker.
  PERFORM container_main_p22.

ENDFORM.                                                    " zmmr189_unidades_pbo

*&---------------------------------------------------------------------*
*&      Form  zmmr189_unidades_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zmmr189_unidades_pai .

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'RELATORIO'.
      "PERFORM select_data.
  ENDCASE.

ENDFORM.

FORM docker.
  CREATE OBJECT container_main
    EXPORTING
      container_name = 'CONTAINERP22'
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

** Exibe Painel 2 "hABILITA E DIVIDE A TELA PARA 2 PARTE ABAIXO DO PAINEL 1
*    CALL METHOD painel_control->get_container
*      EXPORTING
*        row       = 2
*        column    = 1
*      RECEIVING
*        container = painel_2.

ENDFORM.

FORM check_values.

  LOOP AT it_dados_p22 ASSIGNING FIELD-SYMBOL(<it_dados_p22>).

    DATA : l_dupl TYPE i.
    CLEAR: l_dupl.
    LOOP AT it_dados_p22 ASSIGNING FIELD-SYMBOL(<duplicados>) WHERE cd_unid = <it_dados_p22>-cd_unid.
      l_dupl = l_dupl + 1.
    ENDLOOP.

    IF l_dupl > 1.
      DELETE it_dados_p22 WHERE cd_unid = <it_dados_p22>-cd_unid.
    ELSE.
      IF <it_dados_p22>-cd_unid IS INITIAL.
        "<it_dados_p22>-cd_indic = p_cdind-low.
      ENDIF.
      IF <it_dados_p22>-oid IS INITIAL.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = <it_dados_p22>-oid.
      ENDIF.
      MODIFY zfit0204 FROM <it_dados_p22>.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

MODULE status_0200 OUTPUT.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.
    CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

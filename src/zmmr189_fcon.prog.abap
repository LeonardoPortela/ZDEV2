REPORT zmmr189_fcon.

TABLES: zfit0205.
DATA : oid TYPE numc4.
DATA: it_dados TYPE STANDARD TABLE OF zfit0205 WITH HEADER LINE.
DATA: wa_dados TYPE zfit0205.
DATA: aux_dados TYPE zfit0205.
DATA: g_okcode TYPE sy-ucomm.
DATA: lr_column TYPE REF TO cl_salv_column.
DATA ir_columns           TYPE REF TO cl_salv_columns_table.
DATA lex_not_found        TYPE REF TO cx_salv_not_found.
DATA: gr_table   TYPE REF TO cl_salv_table.
DATA: ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
      ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.
DATA: lr_functions TYPE REF TO cl_salv_functions,
      l_text_ref   TYPE string,
      l_icon_ref   TYPE string,
      l_text_del   TYPE string,
      l_icon_del   TYPE string,
      l_text_edit  TYPE string,
      l_icon_edit  TYPE string,
      l_text_save  TYPE string,
      l_icon_save  TYPE string,
      l_text_bck   TYPE string,
      l_icon_bck   TYPE string,
      l_text_can   TYPE string,
      l_icon_can   TYPE string.

DATA:
  container_main TYPE REF TO cl_gui_custom_container,
  painel_control TYPE REF TO cl_gui_splitter_container,
  painel1        TYPE REF TO cl_gui_container,
  painel2        TYPE REF TO cl_gui_container.

DATA: lo_selections TYPE REF TO cl_salv_selections.
DATA lt_rows TYPE salv_t_row.
DATA ls_row TYPE int4.
DATA: msg_text TYPE char45.

*SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
*
*  SELECT-OPTIONS p_cdind FOR zfiT0205-cd_indic NO INTERVALS.
*
*SELECTION-SCREEN END OF BLOCK a.


START-OF-SELECTION.
  PERFORM container_main.
  PERFORM display_grid_unidades.

CLASS cl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS on_before_salv_function         " BEFORE_SALV_FUNCTION
      FOR EVENT if_salv_events_functions~before_salv_function
      OF cl_salv_events_table
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_after_salv_function          " AFTER_SALV_FUNCTION
      FOR EVENT if_salv_events_functions~after_salv_function
      OF cl_salv_events_table
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_added_function               " ADDED_FUNCTION
      FOR EVENT if_salv_events_functions~added_function
      OF cl_salv_events_table
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_top_of_page                  " TOP_OF_PAGE
      FOR EVENT if_salv_events_list~top_of_page
      OF cl_salv_events_table
      IMPORTING r_top_of_page page table_index .

    CLASS-METHODS on_end_of_page                  " END_OF_PAGE
      FOR EVENT if_salv_events_list~end_of_page
      OF cl_salv_events_table
      IMPORTING r_end_of_page page sender.

    CLASS-METHODS on_double_click                 " DOUBLE_CLICK
      FOR EVENT if_salv_events_actions_table~double_click
      OF cl_salv_events_table
      IMPORTING column row sender.

    CLASS-METHODS on_link_click                   " LINK_CLICK
      FOR EVENT if_salv_events_actions_table~link_click
      OF cl_salv_events_table
      IMPORTING row column sender.

    CLASS-METHODS added_function  FOR EVENT if_salv_events_functions~added_function
      OF cl_salv_events_table
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_press  FOR EVENT if_salv_events_tree~keypress
      OF cl_salv_events_tree
      IMPORTING key node_key sender.


ENDCLASS.                    "cl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_event_handler IMPLEMENTATION.


  METHOD on_before_salv_function.
    PERFORM show_function_info USING e_salv_function.
  ENDMETHOD.                    "on_before_salv_function

  METHOD on_after_salv_function.
    PERFORM show_function_info USING e_salv_function.

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

  METHOD on_added_function.
    PERFORM show_function_info USING e_salv_function.
  ENDMETHOD.                    "on_added_function

  METHOD on_top_of_page.
    "BREAK-POINT.
  ENDMETHOD.                    "on_top_of_page

  METHOD on_end_of_page.
    "BREAK-POINT.
  ENDMETHOD.                    "on_end_of_page

  METHOD on_double_click.
    "BREAK-POINT.

    IF row IS NOT INITIAL.

    ENDIF.

    IF column IS NOT INITIAL.

    ENDIF.


  ENDMETHOD.                    "on_double_click

  METHOD on_link_click.
    "BREAK-POINT.
  ENDMETHOD.

  METHOD added_function.          .

    DATA(zz) = 'X'.

  ENDMETHOD.

  METHOD on_press.          .

    DATA(zzy) = 'X'.

  ENDMETHOD.

ENDCLASS.



**&---------------------------------------------------------------------*
**&      Form  show_function_info
**&---------------------------------------------------------------------*
**       text

FORM show_function_info USING i_function.
  DATA: l_string TYPE string.

  CASE i_function.
*     Make ALV as Editable ALV
    WHEN 'EDIT'.
      ls_api = gr_table->extended_grid_api( ).
      ls_edit = ls_api->editable_restricted( ).

      TRY.
          ls_edit->set_attributes_for_columnname(
      EXPORTING
        columnname = 'CD_ITEM'
        all_cells_input_enabled = abap_true
      ).

          ls_edit->set_attributes_for_columnname(
      EXPORTING
        columnname = 'NM_ITEM'
        all_cells_input_enabled = abap_false
      ).
          ls_edit->set_attributes_for_columnname(
EXPORTING
columnname = 'DENS'
all_cells_input_enabled = abap_true
).

          ls_edit->set_attributes_for_columnname(
EXPORTING
columnname = 'TIPO'
all_cells_input_enabled = abap_true
).
      ENDTRY.

      ls_edit->validate_changed_data(
    ).
      PERFORM atuliza.



    WHEN 'SAVE'.

      PERFORM completa_campos.

      PERFORM check_values.

      PERFORM atuliza.

    WHEN 'DELETE'.

      lt_rows = gr_table->get_selections( )->get_selected_rows( ).

      IF lt_rows IS INITIAL.
        msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE msg_text TYPE 'I'.

      ELSE.

        LOOP AT lt_rows INTO ls_row.
          READ TABLE it_dados INTO wa_dados INDEX ls_row.
          DELETE zfit0205 FROM wa_dados.
          COMMIT WORK.
        ENDLOOP.

      ENDIF.
      PERFORM atuliza.

    WHEN 'REFRESH'.
      PERFORM atuliza.

    WHEN '&F03' OR '&ENTER'. " Check for Enter key press
      "BREAK-POINT.

    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN OTHERS.
      "BREAK-POINT.

  ENDCASE.



ENDFORM.                    " show_function_info
*
**&---------------------------------------------------------------------*
**&      Form  set_columns_technical
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*


FORM container_main.

  PERFORM seleciona_dados.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
      r_container = painel1
      container_name = 'CONTAINER'
        IMPORTING
          r_salv_table   = gr_table
        CHANGING
          t_table        = it_dados[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

*Enable function buttons

  DATA gr_salv_func TYPE REF TO cl_salv_functions .
  gr_salv_func = gr_table->get_functions( ).
  gr_salv_func->set_all( abap_true ).

*... §3.1 activate ALV generic Functions

  lr_functions = gr_table->get_functions( ).
  lr_functions->set_all( abap_true ).

*... §3.2 include own functions
  l_text_edit = 'Editar'.
  l_icon_edit = icon_edit_file.
  l_text_save = 'Salvar'.
  l_icon_save = icon_save_as_template.
  l_text_del = 'Remover'.
  l_icon_del = icon_delete.
  l_text_ref = 'Atualizar'.
  l_icon_ref = icon_refresh.
  l_text_bck = 'Voltar'.
  l_icon_bck = icon_retrieve.
  l_text_can = 'Cancelar'.
  l_icon_can = icon_cancel.
  TRY.
      lr_functions->add_function(
        name     = 'EDIT'
        icon     = l_icon_edit
        text     = l_text_edit
        tooltip  = 'Edit Item'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
       name     = 'SAVE'
       icon     = l_icon_save
       text     = l_text_save
       tooltip  = 'Save Item'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
       name     = 'DELETE'
       icon     = l_icon_del
       text     = l_text_del
       tooltip  = 'Deleta Dados'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
 name     = 'REFRESH'
 icon     = l_icon_ref
 text     = l_text_ref
 tooltip  = 'Atualizar Dados'
 position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
      name     = 'BACK'
      icon     = l_icon_bck
      text     = l_text_bck
      tooltip  = 'Voltar'
      position = if_salv_c_function_position=>right_of_salv_functions ).

      lr_functions->add_function(
      name     = 'CANCEL'
      icon     = l_icon_can
      text     = l_text_can
      tooltip  = 'Cancelar'
      position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

*... set the columns technical
  DATA: lr_columns    TYPE REF TO cl_salv_columns,
        lr_column     TYPE REF TO cl_salv_column,
        lr_selections TYPE REF TO cl_salv_selections.

  DATA: lr_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  lr_columns = gr_table->get_columns( ).

  lr_columns->set_column_position(  columnname = 'CD_ITEM'  position = 1 ).
  lr_columns->set_column_position(  columnname = 'NM_ITEM'  position = 2 ).
  lr_columns->set_column_position(  columnname = 'DENS'  position = 3 ).
  lr_columns->set_column_position(  columnname = 'TIPO'  position = 4 ).

  "lr_columns->apply_ddic_structure( name = 'SFLIGHT' ).
  "lr_columns->set_key_fixation( ).


  lr_column = lr_columns->get_column( 'CD_ITEM' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'CODIGO' ).
  lr_column->set_medium_text( 'CODIGO' ).
  lr_column->set_long_text( 'CODIGO' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'NM_ITEM' ).
  lr_column->set_optimized( abap_true ).
  lr_column->set_short_text( 'ITEM' ).
  lr_column->set_medium_text( 'ITEM' ).
  lr_column->set_long_text( 'ITEM' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'DENS' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'DENSIDADE' ).
  lr_column->set_medium_text( 'DENSIDADE' ).
  lr_column->set_long_text( 'DENSIDADE' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'TIPO' ).
  lr_column->set_optimized( abap_false ).
  lr_column->set_short_text( 'TIPO' ).
  lr_column->set_medium_text( 'TIPO' ).
  lr_column->set_long_text( 'TIPO' ).
  "lr_column->set_alignment( if_salv_c_alignment=>centered ).

  lr_column = lr_columns->get_column( 'MANDT' ).
  lr_column->set_visible( if_salv_c_bool_sap=>false ).

  lr_column = lr_columns->get_column( 'OID' ).
  lr_column->set_visible( if_salv_c_bool_sap=>false ).

*... F4 DDIC

  DATA lv_ddic TYPE salv_s_ddic_reference.

*  TRY.
*      lr_column ?= lr_columns->get_column( columnname = 'CD_ITEM' ).
*      lv_ddic = VALUE #( table = 'ZFIT0201' field = 'CD_ITEM').
*      lr_column->set_ddic_reference( lv_ddic  ). "EXPORTING value = lv_ddic
*      lr_column->set_F(  if_salv_c_bool_sap=>true ).
*    CATCH cx_salv_not_found.
*  ENDTRY.


*... §4 set hotspot column
*  TRY.
*      lr_column ?= lr_columns->get_column( 'MATNR' ).
*      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.

*... §6 register to the events of cl_salv_table
  "DATA lr_tree TYPE REF TO cl_salv_events_tree.
  DATA lr_events TYPE REF TO cl_salv_events_table.
  lr_events = gr_table->get_event( ).
  DATA(lr_tree) = gr_table->get_functions( ).

*... §6.1 register to the event USER_COMMAND
  SET HANDLER cl_event_handler=>on_before_salv_function FOR lr_events.
  SET HANDLER cl_event_handler=>on_after_salv_function FOR lr_events.
  SET HANDLER cl_event_handler=>on_added_function FOR lr_events.
  SET HANDLER cl_event_handler=>on_top_of_page FOR lr_events.
  SET HANDLER cl_event_handler=>on_end_of_page FOR lr_events.
  SET HANDLER cl_event_handler=>on_double_click FOR lr_events.
  SET HANDLER cl_event_handler=>on_link_click FOR lr_events.
  SET HANDLER cl_event_handler=>added_function FOR lr_events.
  SET HANDLER cl_event_handler=>on_press FOR ALL INSTANCES.


*... set list title
  DATA: lr_display_settings TYPE REF TO cl_salv_display_settings,
        l_title             TYPE lvc_title.

*  l_title = |Cadastro de unidades por Indicador - { p_cdind-low } |.
*  lr_display_settings = gr_table->get_display_settings( ).
*  lr_display_settings->set_list_header_size( '1' ).
*  lr_display_settings->set_list_header( l_title ).
*  lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
*  lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
*  lr_display_settings->set_list_header( l_title ).


* Enable cell selection mode
  lr_selections = gr_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  lv_key-report = sy-repid.
  lr_layout = gr_table->get_layout( ).
  lr_layout->set_key( lv_key ).
  lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  lr_layout->set_default( abap_true ).

  "just to triger handler
  gr_table->refresh( ).

*... §7 display the table
  gr_table->display( ).

ENDFORM.

FORM atuliza.
  REFRESH it_dados.
  PERFORM seleciona_dados.
  gr_table->refresh( ).
  gr_table->display( ).
ENDFORM.

FORM pega_dados_cell_column.

*  CLEAR: c2p2_oo1,c2p2_oo2,c2p2_oo3,c2p2_oo4,c2p2_oo5.
*
*  c2p2_oo1 = gr_table->get_selections( )->get_current_cell( ).
*  c2p2_oo2 = gr_table->get_selections( )->get_selected_cells( ).
*  c2p2_oo3 = gr_table->get_selections( )->get_selected_columns( ).
*  c2p2_oo4 = gr_table->get_selections( )->get_selected_rows( ).
*  c2p2_oo5 = gr_table->get_selections( )->get_selection_mode( ).

ENDFORM.


FORM completa_campos.

*  LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<it_dados>).
*    IF <it_dados>-cd_ IS INITIAL.
*      <it_dados>-cd_indic = p_cdind.
*    ENDIF.
*  ENDLOOP.

  LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<it_dados>).
    IF <it_dados>-cd_item IS NOT INITIAL.
      SELECT SINGLE nm_item FROM zfit0200
        INTO <it_dados>-nm_item
        WHERE cd_item = <it_dados>-cd_item.
    ENDIF.
  ENDLOOP.
ENDFORM.

FORM seleciona_dados.
  SELECT * FROM zfit0205 INTO TABLE @it_dados." WHERE cd_indic = @p_cdind-low .
ENDFORM.


FORM display_grid_unidades.

  CALL SCREEN 0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ZMMR189_FCON_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zmmr189_fcon_pbo OUTPUT.
  PERFORM zmmr189_fcon_pbo.
ENDMODULE.                 " ZMMR189_FCON_pbo  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ZMMR189_FCON_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zmmr189_fcon_pai INPUT.
  PERFORM zmmr189_fcon_pai.
ENDMODULE.                 " ZMMR189_FCON_pai  INPUT

*&---------------------------------------------------------------------*
*&      Form  ZMMR189_FCON_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zmmr189_fcon_pbo .

  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.
  PERFORM docker.
  PERFORM container_main.

ENDFORM.                                                    " ZMMR189_FCON_pbo

*&---------------------------------------------------------------------*
*&      Form  ZMMR189_FCON_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zmmr189_fcon_pai .

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDFORM.

FORM docker.
  CREATE OBJECT container_main
    EXPORTING
      container_name = 'CONTAINER'
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

  LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<it_dados>).

    DATA : l_dupl TYPE i.
    CLEAR: l_dupl.
    LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<duplicados>) WHERE cd_item = <it_dados>-cd_item.
      l_dupl = l_dupl + 1.
    ENDLOOP.

    IF l_dupl > 1.
      DELETE it_dados WHERE cd_item = <it_dados>-cd_item.
    ELSE.
*      IF <it_dados>-cd_indic IS INITIAL.
*        <it_dados>-cd_indic = p_cdind-low.
*      ENDIF.
      IF <it_dados>-oid IS INITIAL.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = <it_dados>-oid.
      ENDIF.
      MODIFY zfit0205 FROM <it_dados>.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
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

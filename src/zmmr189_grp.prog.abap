REPORT zmmr189_grp.

TABLES: zfit0206.
DATA : oid TYPE numc4.
DATA: it_dados TYPE STANDARD TABLE OF zfit0206 WITH HEADER LINE.
DATA: wa_dados TYPE zfit0206.
DATA: aux_dados TYPE zfit0206.
DATA: P23_g_okcode TYPE sy-ucomm.
DATA: P23_lr_column TYPE REF TO cl_salv_column.
DATA P23_ir_columns           TYPE REF TO cl_salv_columns_table.
DATA P23_lex_not_found        TYPE REF TO cx_salv_not_found.
DATA: P23_gr_table   TYPE REF TO cl_salv_table.
DATA: P23_ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
      P23_ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.
DATA: P23_lr_functions TYPE REF TO cl_salv_functions,
      P23_l_text_ref   TYPE string,
      P23_l_icon_ref   TYPE string,
      P23_l_text_del   TYPE string,
      P23_l_icon_del   TYPE string,
      P23_l_text_edit  TYPE string,
      P23_l_icon_edit  TYPE string,
      P23_l_text_save  TYPE string,
      P23_l_icon_save  TYPE string,
      P23_l_text_bck   TYPE string,
      P23_l_icon_bck   TYPE string,
      P23_l_text_can   TYPE string,
      P23_l_icon_can   TYPE string.

DATA:
  container_main TYPE REF TO cl_gui_custom_container,
  painel_control TYPE REF TO cl_gui_splitter_container,
  painel1        TYPE REF TO cl_gui_container,
  painel2        TYPE REF TO cl_gui_container.

DATA: P23_lo_selections TYPE REF TO cl_salv_selections.
DATA P23_lt_rows TYPE salv_t_row.
DATA P23_ls_row TYPE int4.
DATA: P23_msg_text TYPE char45.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS p_cdind FOR zfit0206-cd_indic NO INTERVALS.

SELECTION-SCREEN END OF BLOCK a.


START-OF-SELECTION.
  PERFORM container_main_P23.
  PERFORM display_grid_MATKL.

CLASS cl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS on_before_salv_function_P23         " BEFORE_SALV_FUNCTION
      FOR EVENT if_salv_events_functions~before_salv_function
      OF cl_salv_events_table
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_after_salv_function_P23          " AFTER_SALV_FUNCTION
      FOR EVENT if_salv_events_functions~after_salv_function
      OF cl_salv_events_table
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_added_function_P23               " ADDED_FUNCTION
      FOR EVENT if_salv_events_functions~added_function
      OF cl_salv_events_table
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_top_of_page_P23                  " TOP_OF_PAGE
      FOR EVENT if_salv_events_list~top_of_page
      OF cl_salv_events_table
      IMPORTING r_top_of_page page table_index .

    CLASS-METHODS on_end_of_page_P23                  " END_OF_PAGE
      FOR EVENT if_salv_events_list~end_of_page
      OF cl_salv_events_table
      IMPORTING r_end_of_page page sender.

    CLASS-METHODS on_double_click_P23                 " DOUBLE_CLICK
      FOR EVENT if_salv_events_actions_table~double_click
      OF cl_salv_events_table
      IMPORTING column row sender.

    CLASS-METHODS on_link_click_P23                   " LINK_CLICK
      FOR EVENT if_salv_events_actions_table~link_click
      OF cl_salv_events_table
      IMPORTING row column sender.

    CLASS-METHODS added_function_P23  FOR EVENT if_salv_events_functions~added_function
      OF cl_salv_events_table
      IMPORTING e_salv_function sender.

    CLASS-METHODS on_press_P23  FOR EVENT if_salv_events_tree~keypress
      OF cl_salv_events_tree
      IMPORTING key node_key sender.


ENDCLASS.                    "cl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_event_handler IMPLEMENTATION.


  METHOD on_before_salv_function_P23.
    PERFORM show_function_info_P23 USING e_salv_function.
  ENDMETHOD.                    "on_before_salv_function

  METHOD on_after_salv_function_P23.
    PERFORM show_function_info_P23 USING e_salv_function.

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

  METHOD on_added_function_P23.
    PERFORM show_function_info_P23 USING e_salv_function.
  ENDMETHOD.                    "on_added_function

  METHOD on_top_of_page_P23.
    "BREAK-POINT.
  ENDMETHOD.                    "on_top_of_page

  METHOD on_end_of_page_P23.
    "BREAK-POINT.
  ENDMETHOD.                    "on_end_of_page

  METHOD on_double_click_P23.
    "BREAK-POINT.

    IF row IS NOT INITIAL.

    ENDIF.

    IF column IS NOT INITIAL.

    ENDIF.


  ENDMETHOD.                    "on_double_click

  METHOD on_link_click_P23.
    "BREAK-POINT.
  ENDMETHOD.

  METHOD added_function_P23.          .

    DATA(zz) = 'X'.

  ENDMETHOD.

  METHOD on_press_P23.          .

    DATA(zzy) = 'X'.

  ENDMETHOD.

ENDCLASS.



**&---------------------------------------------------------------------*
**&      Form  show_function_info
**&---------------------------------------------------------------------*
**       text

FORM show_function_info_P23 USING P23_i_function.
  DATA: P23_l_string TYPE string.

  CASE P23_i_function.
*     Make ALV as Editable ALV
    WHEN 'EDIT_P23'.
      P23_ls_api = P23_gr_table->extended_grid_api( ).
      P23_ls_edit = P23_ls_api->editable_restricted( ).

      TRY.
          P23_ls_edit->set_attributes_for_columnname(
      EXPORTING
        columnname = 'CD_INDIC'
        all_cells_input_enabled = abap_false
      ).
          P23_ls_edit->set_attributes_for_columnname(
            EXPORTING
              columnname = 'MATKL'
              all_cells_input_enabled = abap_true
              ).
        CATCH cx_salv_not_found.
      ENDTRY.

      P23_ls_edit->validate_changed_data(
    ).
      PERFORM atuliza_c2_p2.



    WHEN 'SAVE_P23'.

      PERFORM check_values.
*      "LOOP AT P23_lt_rows INTO P23_ls_row.
*      LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<it_dados>).
*
*        IF <it_dados>-oid IS INITIAL .
*          PERFORM gera_oid.
*          <it_dados>-oid = oid.
*          CLEAR: oid.
*        ENDIF.
*
*        MODIFY zfit0206 FROM wa_dados.
*        COMMIT WORK.
*
*      ENDLOOP.
*      "READ TABLE it_dados INTO wa_dados INDEX 1.


      "ENDLOOP.


*      TRY.
*          MODIFY zfit0206 FROM TABLE it_dados.
*          COMMIT WORK.
*        CATCH cx_salv_not_found.
*      ENDTRY.



      PERFORM atuliza_c2_p2.

    WHEN 'DELETE_P23'.

      P23_lt_rows = P23_gr_table->get_selections( )->get_selected_rows( ).

      IF P23_lt_rows IS INITIAL.
        P23_msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE P23_msg_text TYPE 'I'.

      ELSE.

        LOOP AT P23_lt_rows INTO P23_ls_row.
          READ TABLE it_dados INTO wa_dados INDEX P23_ls_row.
          DELETE zfit0206 FROM wa_dados.
          COMMIT WORK.
        ENDLOOP.

      ENDIF.
      PERFORM atuliza_c2_p2.

    WHEN 'REFRESH_P23'.
      PERFORM atuliza_c2_p2.

    WHEN '&F03' OR '&ENTER'. " Check for Enter key press
      "BREAK-POINT.

    WHEN 'CANCEL_P23'.
      LEAVE PROGRAM.
    WHEN 'BACK_P23'.
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
FORM set_columns_technical_P23 USING P23_ir_columns TYPE REF TO cl_salv_columns.

*...Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)P23_lr_column
  TRY.

      P23_lr_column = P23_ir_columns->get_column( 'CD_INDIC' ).
      P23_lr_column->set_optimized( abap_false ).
      P23_lr_column->set_short_text( 'INIDICADOR' ).
      P23_lr_column->set_medium_text( 'INIDICADOR' ).
      P23_lr_column->set_long_text( 'INIDICADOR' ).
      P23_lr_column->set_alignment( if_salv_c_alignment=>centered ).

      P23_lr_column = P23_ir_columns->get_column( 'MATKL' ).
      P23_lr_column->set_optimized( abap_false ).
      P23_lr_column->set_short_text( 'GRUPO' ).
      P23_lr_column->set_medium_text( 'GRUPO' ).
      P23_lr_column->set_long_text( 'GRUPO' ).

      P23_lr_column = P23_ir_columns->get_column( 'MANDT' ).
      P23_lr_column->set_visible( if_salv_c_bool_sap=>false ).

      P23_lr_column = P23_ir_columns->get_column( 'OID' ).
      P23_lr_column->set_visible( if_salv_c_bool_sap=>false ).


    CATCH cx_salv_not_found INTO P23_lex_not_found.
      "write some error handling
  ENDTRY.

ENDFORM.

FORM container_main_P23.

  PERFORM seleciona_dados_P23.
  PERFORM docker_P23.


*Enable function buttons

  DATA P23_gr_salv_func TYPE REF TO cl_salv_functions .
  P23_gr_salv_func = P23_gr_table->get_functions( ).
  P23_gr_salv_func->set_all( abap_false ).

*... §3.1 activate ALV generic Functions

  P23_lr_functions = P23_gr_table->get_functions( ).
  P23_lr_functions->set_all( abap_true ).

*... §3.2 include own functions
  P23_l_text_edit = 'Editar'.
  P23_l_icon_edit = icon_edit_file.
  P23_l_text_save = 'Salvar'.
  P23_l_icon_save = icon_save_as_template.
  P23_l_text_del = 'Remover'.
  P23_l_icon_del = icon_delete.
  P23_l_text_ref = 'Atualizar'.
  P23_l_icon_ref = icon_refresh.
  P23_l_text_bck = 'Voltar'.
  P23_l_icon_bck = icon_retrieve.
  P23_l_text_can = 'Cancelar'.
  P23_l_icon_can = icon_cancel.
  TRY.
      P23_lr_functions->add_function(
        name     = 'EDIT_P23'
        icon     = P23_l_icon_edit
        text     = P23_l_text_edit
        tooltip  = 'Edit Item'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      P23_lr_functions->add_function(
       name     = 'SAVE_P23'
       icon     = P23_l_icon_save
       text     = P23_l_text_save
       tooltip  = 'Save Item'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      P23_lr_functions->add_function(
       name     = 'DELETE_P23'
       icon     = P23_l_icon_del
       text     = P23_l_text_del
       tooltip  = 'Deleta Dados'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      P23_lr_functions->add_function(
 name     = 'REFRESH_P23'
 icon     = P23_l_icon_ref
 text     = P23_l_text_ref
 tooltip  = 'Atualizar Dados'
 position = if_salv_c_function_position=>right_of_salv_functions ).

*      P23_lr_functions->add_function(
*      name     = 'BACK_P23'
*      icon     = P23_l_icon_bck
*      text     = P23_l_text_bck
*      tooltip  = 'Voltar'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

*      P23_lr_functions->add_function(
*      name     = 'CANCEL_P23'
*      icon     = P23_l_icon_can
*      text     = P23_l_text_can
*      tooltip  = 'Cancelar'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

*... set the columns technical
  DATA: P23_lr_columns    TYPE REF TO cl_salv_columns,
        P23_lr_column     TYPE REF TO cl_salv_column_table,
        P23_lr_selections TYPE REF TO cl_salv_selections.

  DATA: P23_lr_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        P23_lv_key    TYPE salv_s_layout_key.

  P23_lr_columns = P23_gr_table->get_columns( ).

  P23_lr_columns->set_column_position(  columnname = 'CD_INDIC' position = 1 ).
  P23_lr_columns->set_column_position(  columnname = 'MATKL' position = 2 ).

  "P23_lr_columns->apply_ddic_structure( name = 'SFLIGHT' ).
  "P23_lr_columns->set_key_fixation( ).

  P23_lr_columns->set_optimize( abap_true ).

  PERFORM set_columns_technical_P23 USING P23_lr_columns.

*... F4 DDIC

  DATA P23_lv_ddic TYPE salv_s_ddic_reference.

*  TRY.
*      P23_lr_column ?= P23_lr_columns->get_column( columnname = 'MATKL' ).
*      P23_lv_ddic = VALUE #( table = 'CSKB' field = 'MATKL').
*      P23_lr_column->set_ddic_reference( P23_lv_ddic  ). "EXPORTING value = P23_lv_ddic
*      P23_lr_column->set_f4(  if_salv_c_bool_sap=>true ).
*    CATCH cx_salv_not_found.
*  ENDTRY.


*... §4 set hotspot column
*  TRY.
*      P23_lr_column ?= P23_lr_columns->get_column( 'MATNR' ).
*      P23_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.

*... §6 register to the events of cl_salv_table
  "DATA P23_lr_tree TYPE REF TO cl_salv_events_tree.
  DATA P23_lr_events TYPE REF TO cl_salv_events_table.
  P23_lr_events = P23_gr_table->get_event( ).
  DATA(P23_lr_tree) = P23_gr_table->get_functions( ).

*... §6.1 register to the event USER_COMMAND
  SET HANDLER cl_event_handler=>on_before_salv_function_P23 FOR P23_lr_events.
  SET HANDLER cl_event_handler=>on_after_salv_function_P23 FOR P23_lr_events.
  SET HANDLER cl_event_handler=>on_added_function_P23 FOR P23_lr_events.
  SET HANDLER cl_event_handler=>on_top_of_page_P23 FOR P23_lr_events.
  SET HANDLER cl_event_handler=>on_end_of_page_P23 FOR P23_lr_events.
  SET HANDLER cl_event_handler=>on_double_click_P23 FOR P23_lr_events.
  SET HANDLER cl_event_handler=>on_link_click_P23 FOR P23_lr_events.
  SET HANDLER cl_event_handler=>added_function_P23 FOR P23_lr_events.
  SET HANDLER cl_event_handler=>on_press_P23 FOR ALL INSTANCES.


*... set list title
  DATA: P23_lr_display_settings TYPE REF TO cl_salv_display_settings,
        P23_l_title             TYPE lvc_title.

  P23_l_title = |Cadastro de MATKL por Indicador - { p_cdind-low } |.
  P23_lr_display_settings = P23_gr_table->get_display_settings( ).
  P23_lr_display_settings->set_list_header_size( '1' ).
  P23_lr_display_settings->set_list_header( P23_l_title ).
  P23_lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
  P23_lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
  P23_lr_display_settings->set_list_header( P23_l_title ).


* Enable cell selection mode
  P23_lr_selections = P23_gr_table->get_selections( ).
  P23_lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  P23_lv_key-report = sy-repid.
  P23_lr_layout = P23_gr_table->get_layout( ).
  P23_lr_layout->set_key( P23_lv_key ).
  P23_lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  P23_lr_layout->set_default( abap_true ).

  "just to triger handler
  P23_gr_table->refresh( ).

*... §7 display the table
  P23_gr_table->display( ).

ENDFORM.

FORM atuliza_c2_p2.
  REFRESH it_dados.
  PERFORM seleciona_dados_P23.
  P23_gr_table->refresh( ).
  P23_gr_table->display( ).
ENDFORM.

FORM P23_pega_dados_cell_column.

*  CLEAR: c2p2_oo1,c2p2_oo2,c2p2_oo3,c2p2_oo4,c2p2_oo5.
*
*  c2p2_oo1 = P23_gr_table->get_selections( )->get_current_cell( ).
*  c2p2_oo2 = P23_gr_table->get_selections( )->get_selected_cells( ).
*  c2p2_oo3 = P23_gr_table->get_selections( )->get_selected_columns( ).
*  c2p2_oo4 = P23_gr_table->get_selections( )->get_selected_rows( ).
*  c2p2_oo5 = P23_gr_table->get_selections( )->get_selection_mode( ).

ENDFORM.


FORM completa_campos.

  LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<P23_it_dados>).
    IF <P23_it_dados>-cd_indic IS INITIAL.
      <P23_it_dados>-cd_indic = p_cdind.
    ENDIF.
  ENDLOOP.

*  LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<P23_it_dados>).
*    IF <P23_it_dados>-cd_indic IS NOT INITIAL.
*      SELECT SINGLE nm_indic FROM zfit0201
*        INTO <P23_it_dados>-nm_indic
*        WHERE cd_indic = <P23_it_dados>-cd_indic.
*    ENDIF.
*  ENDLOOP.
ENDFORM.

FORM seleciona_dados_P23.
  SELECT * FROM zfit0206 INTO TABLE @it_dados WHERE cd_indic = @p_cdind-low .
ENDFORM.

FORM docker_P23.

  TRY.
      cl_salv_table=>factory(
        EXPORTING
      r_container = painel1
      container_name = 'CONTAINER'
        IMPORTING
          r_salv_table   = P23_gr_table
        CHANGING
          t_table        = it_dados[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

ENDFORM.

FORM display_grid_MATKL.

  CALL SCREEN 0200.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  zmmr189_MATKL_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zmmr189_MATKL_pbo OUTPUT.
  PERFORM zmmr189_MATKL_pbo.
ENDMODULE.                 " zmmr189_MATKL_pbo  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  zmmr189_MATKL_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zmmr189_MATKL_pai INPUT.
  PERFORM zmmr189_MATKL_pai.
ENDMODULE.                 " zmmr189_MATKL_pai  INPUT

*&---------------------------------------------------------------------*
*&      Form  zmmr189_MATKL_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zmmr189_MATKL_pbo .

  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.
  PERFORM docker.
  PERFORM container_main_P23.

ENDFORM.                                                    " zmmr189_MATKL_pbo

*&---------------------------------------------------------------------*
*&      Form  zmmr189_MATKL_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zmmr189_MATKL_pai .

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
      container_name = 'CONTAINERP23'
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
    LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<duplicados>) WHERE MATKL = <it_dados>-MATKL.
      l_dupl = l_dupl + 1.
    ENDLOOP.

    IF l_dupl > 1.
      DELETE it_dados WHERE MATKL = <it_dados>-MATKL.
    ELSE.
      IF <it_dados>-cd_indic IS INITIAL.
        <it_dados>-cd_indic = p_cdind-low.
      ENDIF.
      IF <it_dados>-oid IS INITIAL.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = <it_dados>-oid.
      ENDIF.
      MODIFY zfit0206 FROM <it_dados>.
    ENDIF.
  ENDLOOP.
ENDFORM.

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

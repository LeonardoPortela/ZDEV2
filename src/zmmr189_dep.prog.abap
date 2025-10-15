*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Report ZMMR189_DEP
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmmr189_dep.


TABLES:zfit0202.
TYPES: BEGIN OF ty_dados,
         matnr   TYPE mseg-matnr,
         maktx   TYPE makt-maktx,
         wgbez60 TYPE t023t-wgbez60,
         erfme   TYPE mseg-erfme,
       END OF ty_dados.

DATA: it_dados_p24 TYPE STANDARD TABLE OF zfit0202 WITH HEADER LINE.
"DATA: resultado   TYPE STANDARD TABLE OF zsis_ksb1 INITIAL SIZE 0.
"DATA: resultado2   TYPE STANDARD TABLE OF zsis_ksb1 INITIAL SIZE 0.
DATA: it_dados TYPE STANDARD TABLE OF ty_dados WITH HEADER LINE.
DATA: it_dados2 TYPE STANDARD TABLE OF ty_dados WITH HEADER LINE.
DATA: it_dados_final TYPE STANDARD TABLE OF ty_dados WITH HEADER LINE.
DATA: p24_g_okcode TYPE sy-ucomm.
DATA: p24_lr_column TYPE REF TO cl_salv_column.
DATA p24_lv_ddic TYPE salv_s_ddic_reference.
DATA p24_ir_columns           TYPE REF TO cl_salv_columns_table.
DATA p24_lex_not_found        TYPE REF TO cx_salv_not_found.
DATA: p24_gr_table   TYPE REF TO cl_salv_table.
DATA: p24_ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
      p24_ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.
DATA: p24_lr_functions TYPE REF TO cl_salv_functions,
      p24_l_text_ref   TYPE string,
      p24_l_icon_ref   TYPE string,
      p24_l_text_del   TYPE string,
      p24_l_icon_del   TYPE string,
      p24_l_text_edit  TYPE string,
      p24_l_icon_edit  TYPE string,
      p24_l_text_save  TYPE string,
      p24_l_icon_save  TYPE string,
      p24_l_text_bck   TYPE string,
      p24_l_icon_bck   TYPE string,
      p24_l_text_can   TYPE string,
      p24_l_icon_can   TYPE string,
      p24_l_text_get   TYPE string,
      p24_l_icon_get   TYPE string.

DATA: p24_lo_grid        TYPE REF TO cl_salv_form_layout_grid, " Variables for header
      p24_lo_layout_logo TYPE REF TO cl_salv_form_layout_logo,
      p24_lo_content     TYPE REF TO cl_salv_form_element,
      p24_lv_title       TYPE string,
      p24_lv_rows        TYPE string.

DATA: p_nmindc TYPE string.
DATA: it_f1_item TYPE STANDARD TABLE OF zfit0200 INITIAL SIZE 0 .

DATA:
  container_main TYPE REF TO cl_gui_custom_container,
  painel_control TYPE REF TO cl_gui_splitter_container,
  painel1        TYPE REF TO cl_gui_container,
  painel2        TYPE REF TO cl_gui_container.

DATA: it_kostl TYPE STANDARD TABLE OF zfit0203 INITIAL SIZE 0.
DATA: lr_kostl TYPE RANGE OF zfit0203-kostl.
DATA: it_kstar TYPE STANDARD TABLE OF zfit0207 INITIAL SIZE 0.
DATA: lr_kstar TYPE RANGE OF zfit0207-kstar.

SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS p_cdind FOR zfit0202-cd_indic NO INTERVALS.
  PARAMETERS: p_ano TYPE gjahr.

SELECTION-SCREEN END OF BLOCK a.


START-OF-SELECTION.

  "  cl_demo_output=>display( it_dados_p24 ).
  PERFORM container_main_p24.
  PERFORM display_grid_dep.



CLASS cl_event_handler DEFINITION.

  PUBLIC SECTION.

    CLASS-METHODS on_before_salv_function_p24         " BEFORE_SALV_FUNCTION
      FOR EVENT if_salv_events_functions~before_salv_function
      OF cl_salv_events_table
      IMPORTING e_salv_function.

    CLASS-METHODS on_after_salv_function_p24          " AFTER_SALV_FUNCTION
      FOR EVENT if_salv_events_functions~after_salv_function
      OF cl_salv_events_table
      IMPORTING e_salv_function.

    CLASS-METHODS on_added_function_p24               " ADDED_FUNCTION
      FOR EVENT if_salv_events_functions~added_function
      OF cl_salv_events_table
      IMPORTING e_salv_function.

    CLASS-METHODS on_top_of_page_p24                  " TOP_OF_PAGE
      FOR EVENT if_salv_events_list~top_of_page
      OF cl_salv_events_table
      IMPORTING r_top_of_page
                page
                table_index.

    CLASS-METHODS on_end_of_page_p24                  " END_OF_PAGE
      FOR EVENT if_salv_events_list~end_of_page
      OF cl_salv_events_table
      IMPORTING r_end_of_page
                page.

    CLASS-METHODS on_double_click_p24                 " DOUBLE_CLICK
      FOR EVENT if_salv_events_actions_table~double_click
      OF cl_salv_events_table
      IMPORTING row
                column.

    CLASS-METHODS on_link_click_p24                   " LINK_CLICK
      FOR EVENT if_salv_events_actions_table~link_click
      OF cl_salv_events_table
      IMPORTING row
                column.

    CLASS-METHODS on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

    CLASS-METHODS on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells sender.


    CLASS-METHODS on_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid
      IMPORTING sender.

    CLASS-METHODS on_onf1 FOR EVENT onf1 OF cl_gui_alv_grid
      IMPORTING e_fieldname er_event_data es_row_no sender.

    CLASS-METHODS on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
      IMPORTING e_display e_fieldname e_fieldvalue er_event_data es_row_no et_bad_cells sender.

    CLASS-METHODS on_after_user_command FOR EVENT after_user_command OF cl_gui_alv_grid IMPORTING e_not_processed e_saved e_ucomm sender.
    CLASS-METHODS on_before_user_command FOR EVENT before_user_command OF cl_gui_alv_grid IMPORTING e_ucomm sender.

    CLASS-METHODS on_cc_on_dynpro_load FOR EVENT cc_on_dynpro_load OF cl_gui_cfw IMPORTING dynnr modal_level program stacklevel.

    CLASS-METHODS on_button_click FOR EVENT button_click OF cl_gui_alv_grid IMPORTING es_col_id es_row_no sender.
    CLASS-METHODS on_menu_button FOR EVENT changed OF cl_ctmenu IMPORTING sender.

    CLASS-METHODS on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_interactive e_object sender.

    CLASS-METHODS on_wd_function FOR EVENT if_salv_wd_config~changed OF cl_salv_wd_function IMPORTING class method sender sender_object value.


ENDCLASS.                    "cl_event_handler DEFINITION


*----------------------------------------------------------------------*
*       CLASS cl_event_handler IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS cl_event_handler IMPLEMENTATION.

  METHOD on_before_salv_function_p24.
    PERFORM show_function_info_p24 USING e_salv_function.
  ENDMETHOD.                    "on_before_salv_function

  METHOD on_after_salv_function_p24.
    PERFORM show_function_info_p24 USING e_salv_function.
  ENDMETHOD.

  METHOD on_added_function_p24.
    PERFORM show_function_info_p24 USING e_salv_function.
  ENDMETHOD.                    "on_added_function

  METHOD on_top_of_page_p24.
    DATA(x) = 1.
  ENDMETHOD.                    "on_top_of_page

  METHOD on_end_of_page_p24.
    DATA(x) = 1.
  ENDMETHOD.                    "on_end_of_page

  METHOD on_double_click_p24.
    DATA(x) = 1.

    IF row IS NOT INITIAL.

    ENDIF.

    IF column IS NOT INITIAL.

    ENDIF.

  ENDMETHOD.                    "on_double_click

  METHOD on_link_click_p24.
    DATA(x) = 1.
  ENDMETHOD.              "on_link_click

  METHOD on_data_changed.
    DATA(on_f4) = e_onf4.
    DATA(aaa) = e_onf4_after.
    DATA(aaaa) = e_onf4_before.
    DATA(aaaaa) = e_ucomm.
    DATA(aaaaaa) = er_data_changed.
    DATA(aaaaaaa) = sender.

    IF on_f4 = 'X'.

      TYPES: BEGIN OF ty_item,
               cd_indic TYPE zfit0200-cd_indic,
               cd_item  TYPE zfit0200-cd_item,
               nm_item  TYPE zfit0200-nm_item,
             END OF ty_item.

      DATA: lt_item TYPE TABLE OF ty_item,
            ls_item TYPE ty_item.

      CLEAR: lt_item,ls_item, it_dados_p24-cd_item, it_dados_p24-nm_item.


      " Implement logic to fetch data (e.g., from MARA table)
      SELECT cd_indic,cd_item,nm_item FROM zfit0200 WHERE cd_indic = @p_cdind-low INTO TABLE @lt_item.

      " Provide the data to the F4 help
      LOOP AT lt_item INTO ls_item.
        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield  = 'CD_ITEM'
          TABLES
            value_tab = lt_item
          EXCEPTIONS
            OTHERS    = 1.

        IF sy-subrc <> 0.
          " Handle errors if needed
        ENDIF.
      ENDLOOP.

    ENDIF.


  ENDMETHOD.


  METHOD on_data_changed_finished.

  ENDMETHOD.

  METHOD on_after_refresh.

  ENDMETHOD.

  METHOD on_onf1.

  ENDMETHOD.

  METHOD on_onf4.

  ENDMETHOD.


  METHOD on_after_user_command.

  ENDMETHOD.

  METHOD on_before_user_command.

  ENDMETHOD.

  METHOD on_cc_on_dynpro_load.

  ENDMETHOD.

  METHOD on_button_click.

  ENDMETHOD.

  METHOD on_menu_button.

  ENDMETHOD.

  METHOD on_toolbar.

  ENDMETHOD.

  METHOD on_wd_function.

  ENDMETHOD.


ENDCLASS.


**&---------------------------------------------------------------------*
**&      Form  show_function_info
**&---------------------------------------------------------------------*
**       text

FORM show_function_info_p24 USING p24_i_function.
  DATA: p24_l_string TYPE string.

  CASE p24_i_function.
*     Make ALV as Editable ALV
    WHEN 'EDIT_P24'.
      p24_ls_api = p24_gr_table->extended_grid_api( ).
      p24_ls_edit = p24_ls_api->editable_restricted( ).

      TRY.
          p24_ls_edit->set_attributes_for_columnname(
      EXPORTING
        columnname = 'CD_INDIC'
        all_cells_input_enabled = abap_false
      ).
          p24_ls_edit->set_attributes_for_columnname(
 EXPORTING
   columnname = 'CD_ITEM'
   all_cells_input_enabled = abap_true
   ).
          p24_ls_edit->set_attributes_for_columnname(
EXPORTING
columnname = 'NM_ITEM'
all_cells_input_enabled = abap_false
).
        CATCH cx_salv_not_found.
      ENDTRY.

      p24_ls_edit->validate_changed_data(
    ).
      PERFORM atuliza_c2_p2.



    WHEN 'SAVE_P24'.

      PERFORM check_values.

      PERFORM atuliza_c2_p2.

    WHEN 'DELETE_P24'.


      DATA: p24_lo_selections TYPE REF TO cl_salv_selections.
      DATA p24_lt_rows TYPE salv_t_row.
      DATA p24_ls_row TYPE int4.
      DATA: p24_msg_text TYPE char45.

      p24_lt_rows = p24_gr_table->get_selections( )->get_selected_rows( ).

      IF p24_lt_rows IS INITIAL.
        p24_msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE p24_msg_text TYPE 'I'.

      ELSE.

        LOOP AT p24_lt_rows INTO p24_ls_row.

          READ TABLE it_dados_p24 INDEX p24_ls_row.
          DELETE FROM zfit0202 WHERE cd_indic = @it_dados_p24-cd_indic AND cd_item = @it_dados_p24-cd_item AND oid = @it_dados_p24-oid.
          COMMIT WORK.

        ENDLOOP.

      ENDIF.
      CLEAR: p24_lt_rows,p24_lo_selections,p24_ls_row,p24_msg_text.
      PERFORM atuliza_c2_p2.

    WHEN 'REFRESH_P24'.
      PERFORM atuliza_c2_p2.

    WHEN '&F03' OR '&ENTER'. " Check for Enter key press
      "BREAK-POINT.

    WHEN 'CANCEL_P24'.
      LEAVE PROGRAM.
    WHEN 'BACK_P24'.
      SET SCREEN 0.
      LEAVE SCREEN.

    WHEN 'IMPORT_P24'.
      PERFORM import_data.
    WHEN OTHERS.
      "BREAK-POINT.

  ENDCASE.



ENDFORM.                    " show_function_info


FORM container_main_p24.

  PERFORM seleciona_dados_p24.
  PERFORM docker_p24.


  DATA p24_gr_salv_func TYPE REF TO cl_salv_functions .
  p24_gr_salv_func = p24_gr_table->get_functions( ).
  p24_gr_salv_func->set_all( abap_true ).

*... §3.1 activate ALV generic Functions

  p24_lr_functions = p24_gr_table->get_functions( ).
  p24_lr_functions->set_all( abap_true ).

*... §3.2 include own functions
  p24_l_text_edit = 'Editar'.
  p24_l_icon_edit = icon_edit_file.
  p24_l_text_save = 'Salvar'.
  p24_l_icon_save = icon_save_as_template.
  p24_l_text_del = 'Remover'.
  p24_l_icon_del = icon_delete.
  p24_l_text_ref = 'Atualizar'.
  p24_l_icon_ref = icon_refresh.
  p24_l_text_bck = 'Voltar'.
  p24_l_icon_bck = icon_retrieve.
  p24_l_text_can = 'Cancelar'.
  p24_l_icon_can = icon_cancel.
  p24_l_text_get = 'Importar'.
  p24_l_icon_get = icon_import.

  TRY.
      p24_lr_functions->add_function(
        name     = 'EDIT_P24'
        icon     = p24_l_icon_edit
        text     = p24_l_text_edit
        tooltip  = 'EDIT ITEM'
        position = if_salv_c_function_position=>right_of_salv_functions ).

      p24_lr_functions->add_function(
       name     = 'SAVE_P24'
       icon     = p24_l_icon_save
       text     = p24_l_text_save
       tooltip  = 'SAVE ITEM'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      p24_lr_functions->add_function(
 name     = 'IMPORT_P24'
 icon     = p24_l_icon_get
 text     = p24_l_text_get
 tooltip  = 'BUSCAR ITENS'
 position = if_salv_c_function_position=>right_of_salv_functions ).

      p24_lr_functions->add_function(
       name     = 'DELETE_P24'
       icon     = p24_l_icon_del
       text     = p24_l_text_del
       tooltip  = 'DELETA DADOS'
       position = if_salv_c_function_position=>right_of_salv_functions ).

      p24_lr_functions->add_function(
 name     = 'REFRESH_P24'
 icon     = p24_l_icon_ref
 text     = p24_l_text_ref
 tooltip  = 'ATUALIZAR DADOS'
 position = if_salv_c_function_position=>right_of_salv_functions ).

*      p24_lr_functions->add_function(
*      name     = 'BACK_P24'
*      icon     = p24_l_icon_bck
*      text     = p24_l_text_bck
*      tooltip  = 'VOLTAR'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

*      p24_lr_functions->add_function(
*      name     = 'CANCEL_P24'
*      icon     = p24_l_icon_can
*      text     = p24_l_text_can
*      tooltip  = 'CANCELAR'
*      position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

*... set the columns technical
  DATA: p24_lr_columns    TYPE REF TO cl_salv_columns,
        p24_lr_column     TYPE REF TO cl_salv_column_table,
        p24_lr_selections TYPE REF TO cl_salv_selections.

  DATA: p24_lr_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        p24_lv_key    TYPE salv_s_layout_key.

  p24_lr_columns = p24_gr_table->get_columns( ).

  p24_lr_columns->set_column_position(  columnname = 'CD_INDIC' position = 1 ).
  p24_lr_columns->set_column_position(  columnname = 'CD_ITEM' position = 2 ).
  p24_lr_columns->set_column_position(  columnname = 'NM_ITEM' position = 3 ).
  p24_lr_columns->set_column_position(  columnname = 'MATNR' position = 4 ).
  p24_lr_columns->set_column_position(  columnname = 'MAKTX' position = 5 ).
  p24_lr_columns->set_column_position(  columnname = 'WGBEZ60' position = 6 ).
  p24_lr_columns->set_column_position(  columnname = 'UN' position = 7 ).

  "p24_lr_columns->set_optimize( ).

  PERFORM set_columns_technical_p24 USING p24_lr_columns.


*... F4 DDIC

  DATA p24_lv_ddic TYPE salv_s_ddic_reference.

  TRY.
      p24_lr_column ?= p24_lr_columns->get_column( columnname = 'CD_ITEM' ).
      p24_lv_ddic = VALUE #( table = 'ZFIT0200' field = 'CD_ITEM').
      p24_lr_column->set_ddic_reference( p24_lv_ddic  ). "EXPORTING value = P24_lv_ddic
      p24_lr_column->set_f4(  if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.
  ENDTRY.

*... §4 set hotspot column
*  TRY.
*      p24_lr_column = p24_lr_columns->get_column( 'MATNR' ).
*      p24_lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*    CATCH cx_salv_not_found.
*                                                        "#EC NO_HANDLER
*  ENDTRY.

*... §6 register to the events of cl_salv_table
  DATA p24_lr_events TYPE REF TO cl_salv_events_table.
  DATA p24_lr_events_grid TYPE REF TO cl_gui_alv_grid.
  p24_lr_events = p24_gr_table->get_event( ).


*... §6.1 register to the event USER_COMMAND
  SET HANDLER cl_event_handler=>on_before_salv_function_p24 FOR p24_lr_events.
  SET HANDLER cl_event_handler=>on_after_salv_function_p24 FOR p24_lr_events.
  SET HANDLER cl_event_handler=>on_added_function_p24 FOR p24_lr_events.
  SET HANDLER cl_event_handler=>on_top_of_page_p24 FOR p24_lr_events.
  SET HANDLER cl_event_handler=>on_end_of_page_p24 FOR p24_lr_events.
  SET HANDLER cl_event_handler=>on_double_click_p24 FOR p24_lr_events.
  SET HANDLER cl_event_handler=>on_link_click_p24 FOR p24_lr_events.
  SET HANDLER cl_event_handler=>on_data_changed FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_data_changed_finished FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_onf1 FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_onf4 FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_after_refresh FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_after_user_command FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_before_user_command FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_button_click FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_menu_button FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_toolbar FOR ALL INSTANCES.
  SET HANDLER cl_event_handler=>on_wd_function FOR ALL INSTANCES.



*... set list title
  DATA: p24_lr_display_settings TYPE REF TO cl_salv_display_settings,
        p24_l_title             TYPE lvc_title.

  p24_l_title = |Depara SAP x SIS Indicador - { p_cdind-low } { p_nmindc }|.
  p24_lr_display_settings = p24_gr_table->get_display_settings( ).
  p24_lr_display_settings->set_list_header_size( '10' ).
  p24_lr_display_settings->set_list_header( p24_l_title ).


* Enable cell selection mode
  p24_lr_selections = p24_gr_table->get_selections( ).
  p24_lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

* Enable the save layout buttons
  p24_lv_key-report = sy-repid.
  p24_lr_layout = p24_gr_table->get_layout( ).
  p24_lr_layout->set_key( p24_lv_key ).
  p24_lr_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
  p24_lr_layout->set_default( abap_true ).

* Create header
  DESCRIBE TABLE it_dados_p24[] LINES p24_lv_rows.
  CONCATENATE 'Number of lv_rows: ' p24_lv_rows INTO p24_lv_title SEPARATED BY space.

  CREATE OBJECT p24_lo_grid.
  CREATE OBJECT p24_lo_layout_logo.
  p24_lo_grid->create_label( row = 1 column = 1 text = p24_lv_title tooltip = p24_lv_title ).
  p24_lo_layout_logo->set_left_content( p24_lo_grid ).
  p24_lo_content = p24_lo_layout_logo.
  p24_gr_table->set_top_of_list( p24_lo_content ).



  "just to triger handler
  p24_gr_table->refresh( ).

*... §7 display the table
  p24_gr_table->display( ).

ENDFORM.

FORM atuliza_c2_p2.
  REFRESH it_dados_p24.
  PERFORM seleciona_dados_p24.
  p24_gr_table->refresh( ).
  p24_gr_table->display( ).
ENDFORM.

FORM p24_pega_dados_cell_column.

*  CLEAR: c2p2_oo1,c2p2_oo2,c2p2_oo3,c2p2_oo4,c2p2_oo5.
*
*  c2p2_oo1 = p24_gr_table->get_selections( )->get_current_cell( ).
*  c2p2_oo2 = p24_gr_table->get_selections( )->get_selected_cells( ).
*  c2p2_oo3 = p24_gr_table->get_selections( )->get_selected_columns( ).
*  c2p2_oo4 = p24_gr_table->get_selections( )->get_selected_rows( ).
*  c2p2_oo5 = p24_gr_table->get_selections( )->get_selection_mode( ).

ENDFORM.


FORM completa_campos.
  LOOP AT it_dados_p24 ASSIGNING FIELD-SYMBOL(<p24_it_dados_p24>).

*    IF <p24_it_dados_p24>-cd_indic IS NOT INITIAL.
*
*      SELECT SINGLE nm_indic FROM zfit0201
*        INTO <p24_it_dados_p24>-nm_indic
*        WHERE cd_indic = <p24_it_dados_p24>-cd_indic.
*
*    ENDIF.

  ENDLOOP.
ENDFORM.

FORM seleciona_dados_p24.
  CLEAR: p_nmindc.
  SELECT SINGLE nm_indic FROM zfit0201 INTO @p_nmindc WHERE cd_indic IN @p_cdind.

  CLEAR: it_dados_p24.
  SELECT * FROM zfit0202 INTO TABLE @it_dados_p24 WHERE cd_indic IN @p_cdind.

  CLEAR: it_f1_item.
  SELECT * FROM zfit0200 WHERE cd_indic = @p_cdind-low INTO TABLE @it_f1_item.


ENDFORM.

FORM docker_p24.
  TRY.
      cl_salv_table=>factory(
        EXPORTING
      r_container = painel1
      container_name = 'CONTAINER'
        IMPORTING
          r_salv_table   = p24_gr_table
        CHANGING
          t_table        = it_dados_p24[] ).
    CATCH cx_salv_msg.                                  "#EC NO_HANDLER
  ENDTRY.

ENDFORM.

FORM display_grid_dep.

  CALL SCREEN 0200.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  zmmr189_DEP_pbo  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zmmr189_dep_pbo OUTPUT.
  PERFORM zmmr189_dep_pbo.
ENDMODULE.                 " zmmr189_DEP_pbo  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  zmmr189_DEP_pai  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE zmmr189_dep_pai INPUT.
  PERFORM zmmr189_dep_pai.
ENDMODULE.                 " zmmr189_DEP_pai  INPUT

*&---------------------------------------------------------------------*
*&      Form  zmmr189_DEP_pbo
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zmmr189_dep_pbo .

  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.
  PERFORM docker.
  PERFORM container_main_p24.

ENDFORM.                                                    " zmmr189_DEP_pbo

*&---------------------------------------------------------------------*
*&      Form  zmmr189_DEP_pai
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zmmr189_dep_pai .

  CASE sy-ucomm.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL' OR 'EXIT'.
      LEAVE PROGRAM.
*    WHEN 'RELATORIO'.
*      PERFORM select_data.
  ENDCASE.

ENDFORM.

FORM docker.
  CREATE OBJECT container_main
    EXPORTING
      container_name = 'CONTAINERP24'
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

FORM import_data.

*  CLEAR: it_kostl.
*  SELECT DISTINCT * FROM zfit0203 WHERE cd_indic = @p_cdind-low INTO TABLE @it_kostl.
*  CLEAR: lr_kostl.
*
*  lr_kostl = VALUE #( FOR wa_params_cc IN it_kostl ( option = 'EQ' sign = 'I' low = wa_params_cc-kostl ) ).
*
*  SORT lr_kostl[] ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM lr_kostl[].
*
*  CLEAR: it_kstar.
*
*  SELECT DISTINCT * FROM zfit0207 WHERE cd_indic = @p_cdind-low INTO TABLE @it_kstar.
*
*  CLEAR: lr_kstar.
*
*  lr_kstar = VALUE #( FOR wa_params_clc IN it_kstar ( option = 'EQ' sign = 'I' low = wa_params_clc-kstar ) ).
*
*  SORT lr_kstar[] ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM lr_kstar[].

*  DATA: it_MATKL TYPE STANDARD TABLE OF zfit0206 INITIAL SIZE 0.
*  CLEAR: it_MATKL.
*
*  SELECT DISTINCT * FROM zfit0206 WHERE cd_indic = @p_cdind-low INTO TABLE @it_MATKL.

*  DATA: lr_MATKL TYPE RANGE OF zfit0206-matkl.
*  CLEAR: lr_MATKL.
*
*  lr_MATKL = VALUE #( FOR wa_params_GRP IN it_MATKL ( option = 'EQ' sign = 'I' low = wa_params_GRP-matkl ) ).
*
*  SORT lr_MATKL[] ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM lr_MATKL[].
*
*  DATA: lr_MATNR TYPE RANGE OF mara-matnr.
*  CLEAR: lr_MATNR.
*
*  SELECT DISTINCT
*    matnr
*  INTO TABLE @DATA(IT_matnr)
*  FROM mara WHERE matkl IN @lr_MATKL[].
*
*  lr_MATNR = VALUE #( FOR wa_params_MAT IN IT_matnr ( option = 'EQ' sign = 'I' low = wa_params_MAT-matnr ) ).
*  SORT lr_MATNR[] ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM lr_MATNR[].
*
*     DATA: p_i_matnr TYPE RANGE OF pkosa-matnr WITH HEADER LINE.
*
*   LOOP AT lr_MATNR[] ASSIGNING FIELD-SYMBOL(<mat>).
*     p_i_matnr = <mat>-low.
*     APPEND p_i_matnr.
*   ENDLOOP.


  DATA: so_data    TYPE RANGE OF budat,
        wa_data    LIKE LINE OF so_data,
        s_ccusto   TYPE RANGE OF kostl,
        s_clcusto  TYPE RANGE OF kstar,
        p_kagru    TYPE kagru,
        wa_ccusto  LIKE LINE OF s_ccusto,
        wa_clcusto LIKE LINE OF s_ccusto.

  "w_resultado TYPE zsis_ksb1.


  DATA: lr_data            TYPE REF TO data,
        lr_data_line       TYPE REF TO data,
        lr_data_descr      TYPE REF TO cl_abap_datadescr,
        lr_data_line_descr TYPE REF TO cl_abap_datadescr.

  DATA: rkaep000 TYPE REF TO cl_salv_bs_runtime_info.


  DATA: l_budat1  TYPE sy-datum,
        l_budat2  TYPE sy-datum,
        l_tcode   TYPE sy-tcode,
        l_kokrs   TYPE string, "kokrs,
        lyear     TYPE p,
        lt_data   TYPE STANDARD TABLE OF abaplist WITH HEADER LINE,
        lt_result TYPE STANDARD TABLE OF abaplist WITH HEADER LINE.

  FIELD-SYMBOLS: <lt_data>      TYPE ANY TABLE,
                 <lt_data_line> TYPE ANY TABLE,
                 <ls_data>      TYPE any,
                 <ls_data_line> TYPE any.

* Data Declaration
  DATA:lt_listobject TYPE TABLE OF abaplist,
       l_tab_lines   TYPE i,
       lv_list_id    TYPE string.

  CLEAR: l_budat1, l_budat2,l_tcode,l_kokrs,lt_data,lt_result.

  l_budat1 = |{ p_ano }0101|.
  l_budat2 = |{ p_ano }1231|.
  l_tcode = 'KSB1'.
  l_kokrs = 'MAGI'.

  TYPES: BEGIN OF ty_resultado,
           matnr  TYPE mseg-matnr,
           werks  TYPE mseg-werks,
           bukrs  TYPE mseg-bukrs,
           mjahr  TYPE mseg-mjahr,
           kstar  TYPE mseg-sakto,
           kostl  TYPE mseg-pprctr,
           meinb  TYPE mseg-meins,
           mbgbtr TYPE mseg-menge,
           budat  TYPE mseg-budat_mkpf,
           ebtxt  TYPE makt-maktx,
         END OF ty_resultado.

  DATA: resultado   TYPE STANDARD TABLE OF ty_resultado INITIAL SIZE 0,
        w_resultado TYPE ty_resultado.

SELECT
  DISTINCT
  ",aa~matkl
   a~matnr
  ,a~werks
  ,a~bukrs
  ,a~mjahr
  ,a~sakto AS kstar
  ,aaaa~kostl AS KOSTL
  ,a~meins AS meinb
  ,a~menge AS mbgbtr
  ,a~budat_mkpf AS budat
  ,aaa~maktx AS ebtxt
  ",'261' as tipo
  FROM mseg  AS a
  LEFT JOIN mara AS aa ON a~matnr = aa~matnr
  LEFT JOIN makt AS aaa ON a~matnr = aaa~matnr
  LEFT JOIN caufv AS aaaa ON a~aufnr = aaaa~aufnr
  WHERE 1 = 1
  AND LTRIM( a~sakto,'0' ) IN ( SELECT DISTINCT LTRIM( kstar,'0' ) AS kstar FROM zfit0207 WHERE cd_indic = @p_cdind-low )
  AND LTRIM( aaaa~kostl,'0' ) IN ( SELECT DISTINCT LTRIM( kostl,'0' ) AS KOSTL FROM zfit0203 WHERE cd_indic = @p_cdind-low )
  AND LTRIM( aa~matkl,'0' ) IN ( SELECT DISTINCT LTRIM( matkl,'0' ) AS matkl FROM zfit0206 WHERE cd_indic = @p_cdind-low )
  "AND a~matnr IN ( SELECT DISTINCT matnr FROM zfit0202 WHERE cd_indic = @p_cdind-low )
  AND a~mjahr = @p_ano
  AND a~budat_mkpf BETWEEN @l_budat1 AND @l_budat2
  AND a~kokrs = @l_kokrs
  AND a~bwart = '261'
  AND a~mblnr NOT IN (
  SELECT DISTINCT b~smbln FROM mseg AS b
  WHERE 1 = 1
  AND b~smbln = a~mblnr
  AND b~bwart = '262' )
  INTO TABLE @DATA(resultado_261)."@resultado.

  APPEND LINES OF resultado_261 TO resultado.


  SELECT
  DISTINCT
  ",aa~matkl
  a~matnr
  ,a~werks
  ,a~bukrs
  ,a~mjahr
  ,a~sakto AS kstar
  ,a~kostl
  ,a~meins AS meinb
  ,a~menge AS mbgbtr
  ,a~budat_mkpf AS budat
  ,aaa~maktx AS ebtxt
  ",'201' as tipo
  FROM mseg  AS a
  LEFT JOIN mara AS aa ON a~matnr = aa~matnr
  LEFT JOIN makt AS aaa ON a~matnr = aaa~matnr
  LEFT JOIN caufv AS aaaa ON a~aufnr = aaaa~aufnr
  WHERE 1 = 1
  AND LTRIM( a~sakto,'0' ) IN ( SELECT DISTINCT LTRIM( kstar,'0' ) AS kstar FROM zfit0207 WHERE cd_indic = @p_cdind-low )
  AND LTRIM( A~kostl,'0' ) IN ( SELECT DISTINCT LTRIM( kostl,'0' ) AS KOSTL FROM zfit0203 WHERE cd_indic = @p_cdind-low )
  AND LTRIM( aa~matkl,'0' ) IN ( SELECT DISTINCT LTRIM( matkl,'0' ) AS matkl FROM zfit0206 WHERE cd_indic = @p_cdind-low )
  "AND a~matnr IN ( SELECT DISTINCT matnr FROM zfit0202 WHERE cd_indic = @p_cdind-low )
  AND a~mjahr = @p_ano
  AND a~budat_mkpf BETWEEN @l_budat1 AND @l_budat2
  AND a~kokrs = @l_kokrs
  AND a~bwart = '201'
  AND a~mblnr NOT IN (
  SELECT DISTINCT b~smbln FROM mseg AS b
  WHERE 1 = 1
  AND b~smbln = a~mblnr
  AND b~bwart = '202')
  INTO TABLE @DATA(resultado_201)."@resultado.

  APPEND LINES OF resultado_201 TO resultado.


  SELECT
  DISTINCT
  ",aa~matkl
   a~matnr
  ,a~werks
  ,a~bukrs
  ,a~mjahr
  ,a~sakto AS kstar
  ,a~kostl
  ,a~meins AS meinb
  ,a~menge AS mbgbtr
  ,a~budat_mkpf AS budat
  ,aaa~maktx AS ebtxt
  ",'101' as tipo
  FROM mseg  AS a
  LEFT JOIN mara AS aa ON a~matnr = aa~matnr
  LEFT JOIN makt AS aaa ON a~matnr = aaa~matnr
  LEFT JOIN caufv AS aaaa ON a~aufnr = aaaa~aufnr
  WHERE 1 = 1
  AND LTRIM( a~sakto,'0' ) IN ( SELECT DISTINCT LTRIM( kstar,'0' ) AS kstar FROM zfit0207 WHERE cd_indic = @p_cdind-low )
  AND LTRIM( A~kostl,'0' ) IN ( SELECT DISTINCT LTRIM( kostl,'0' ) AS KOSTL FROM zfit0203 WHERE cd_indic = @p_cdind-low )
  AND LTRIM( aa~matkl,'0' ) IN ( SELECT DISTINCT LTRIM( matkl,'0' ) AS matkl FROM zfit0206 WHERE cd_indic = @p_cdind-low )
  "AND a~matnr IN ( SELECT DISTINCT matnr FROM zfit0202 WHERE cd_indic = @p_cdind-low )
  AND a~mjahr = @p_ano
  AND a~budat_mkpf BETWEEN @l_budat1 AND @l_budat2
  AND a~kokrs = @l_kokrs
  AND a~bwart = '101'
  AND a~mblnr NOT IN (
  SELECT DISTINCT b~smbln FROM mseg AS b
  WHERE 1 = 1
  AND b~smbln = a~mblnr
  AND b~bwart = '102' )
  AND A~kostl <> ''
  INTO TABLE @DATA(resultado_101)."@resultado.

  APPEND LINES OF resultado_101 TO resultado.


*  cl_salv_bs_runtime_info=>set(
*       EXPORTING display = abap_false
*                 metadata = abap_true
*                 data = abap_true ).
*
*  SUBMIT rkaep000
*  WITH kostl IN lr_kostl[]
*  WITH kstar IN lr_kstar[]
*  WITH r_budat BETWEEN l_budat1 AND l_budat2
*  WITH p_tcode = l_tcode
*  WITH p_kokrs =  l_kokrs
*  WITH p_maxsel EQ 999999
*  EXPORTING LIST TO MEMORY
*    AND RETURN.
*
*  TRY.
*      cl_salv_bs_runtime_info=>get_data_ref(
*          IMPORTING r_data_descr      = lr_data_descr
*                    r_data_line_descr = lr_data_line_descr ).
*    CATCH cx_root.
*
*  ENDTRY.
*
*
*  IF lr_data_descr IS NOT INITIAL.
*    CREATE DATA lr_data TYPE HANDLE lr_data_descr.
*
*    ASSIGN lr_data->* TO <lt_data>.
*
*    TRY.
*        cl_salv_bs_runtime_info=>get_data(
*        IMPORTING
*        t_data = <lt_data>
*        ).
*
*      CATCH cx_root.
*
*    ENDTRY.
*
*  ELSE.
*
*    EXIT.
*
*    MESSAGE 'Não foram encontrados resultados!' TYPE 'I'.
*
*  ENDIF.
*
*
*  cl_salv_bs_runtime_info=>clear_all( ).
*
*  ASSIGN lr_data->* TO <ls_data>.
*
*
*  LOOP AT <lt_data> ASSIGNING <ls_data>.
*    MOVE-CORRESPONDING <ls_data> TO w_resultado.
*    IF w_resultado-meinb IS NOT INITIAL AND w_resultado-matnr IN lr_matnr[].
*      APPEND w_resultado TO resultado.
*    ENDIF.
*    CLEAR w_resultado.
*  ENDLOOP.
*
*  DATA: lr_aufnr TYPE RANGE OF aufnr WITH HEADER LINE.
*
*  SORT lr_aufnr[] BY  low ASCENDING.
*  DELETE ADJACENT DUPLICATES FROM lr_aufnr[] COMPARING low.
*
*  CLEAR: l_tcode.
*
*  IF lr_aufnr[] IS NOT INITIAL.
*
*    CLEAR: <lt_data>,lr_data,lr_data_descr, lr_data_line, lr_data_line_descr, w_resultado.
*
*    l_budat1 = |{ p_ano }0101|.
*    l_budat2 = |{ p_ano }1231|.
*    l_tcode = 'KOB1'.
*    l_kokrs = 'MAGI'.

*    cl_salv_bs_runtime_info=>set(
*         EXPORTING display = abap_false
*                   metadata = abap_true
*                   data = abap_true ).
*
*    SUBMIT rkaep000
*    WITH aufnr IN lr_aufnr[]
*    WITH r_budat BETWEEN l_budat1 AND l_budat2
*    WITH p_tcode = l_tcode
*    WITH p_kokrs =  l_kokrs
*    WITH p_maxsel EQ 999999
*    EXPORTING LIST TO MEMORY
*      AND RETURN.
*
*    TRY.
*        cl_salv_bs_runtime_info=>get_data_ref(
*            IMPORTING r_data_descr      = lr_data_descr
*                      r_data_line_descr = lr_data_line_descr ).
*      CATCH cx_root.
*
*    ENDTRY.
*
*
*    IF lr_data_descr IS NOT INITIAL.
*      CREATE DATA lr_data TYPE HANDLE lr_data_descr.
*
*      ASSIGN lr_data->* TO <lt_data>.
*
*      TRY.
*          cl_salv_bs_runtime_info=>get_data(
*          IMPORTING
*          t_data = <lt_data>
*          ).
*
*        CATCH cx_root.
*
*      ENDTRY.
*
*    ELSE.
*
*      EXIT.
*
*      MESSAGE 'Não foram encontrados resultados!' TYPE 'I'.
*
*    ENDIF.
*
*
*    cl_salv_bs_runtime_info=>clear_all( ).
*
*    ASSIGN lr_data->* TO <ls_data>.
*
*    LOOP AT <lt_data> ASSIGNING <ls_data>.
*      MOVE-CORRESPONDING <ls_data> TO w_resultado.
*      IF w_resultado-meinb IS NOT INITIAL AND w_resultado-matnr IN lr_matnr[].
*        APPEND w_resultado TO resultado.
*      ENDIF.
*      CLEAR w_resultado.
*    ENDLOOP.
*
*  ENDIF.
*
***********************************************************************
*  DELETE resultado WHERE matnr = abap_false.
*  DELETE resultado WHERE matnr = ''.
*  DELETE Resultado WHERE meinb = '' .
*  DELETE Resultado WHERE meinb = abap_false.
*
*  CLEAR: <lt_data>,lr_data,lr_data_descr, lr_data_line, lr_data_line_descr, w_resultado.
*
*  DATA: aux_werks TYPE werks_d.
*  DATA: aux_j1branch TYPE t001w-j_1bbranch.

  CLEAR: it_dados.

  LOOP AT resultado ASSIGNING FIELD-SYMBOL(<reult>).
    it_dados-matnr = <reult>-matnr.
    SELECT SINGLE a~maktx FROM makt AS a
            INTO  it_dados-maktx
WHERE a~matnr = <reult>-matnr.
    SELECT SINGLE b~wgbez60 FROM mara AS a
      INNER JOIN t023t AS b ON a~matkl = b~matkl AND b~spras = 'P'
      INTO  it_dados-wgbez60
  WHERE a~matnr = <reult>-matnr.

    it_dados-erfme = <reult>-meinb.
    APPEND it_dados.
  ENDLOOP.

  MOVE it_dados[] TO it_dados_final[].

  CLEAR: it_dados[].

  SORT it_dados_final BY matnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM it_dados_final COMPARING matnr.


  CLEAR: resultado.

  LOOP AT it_dados_final[] ASSIGNING FIELD-SYMBOL(<it_fdados>).
    it_dados_p24-matnr = <it_fdados>-matnr.
    it_dados_p24-cd_indic = p_cdind-low.
    it_dados_p24-maktx = <it_fdados>-maktx.
    it_dados_p24-wgbez60 = <it_fdados>-wgbez60.
    it_dados_p24-un =  <it_fdados>-erfme.

    APPEND it_dados_p24.
  ENDLOOP.

  IF  it_dados_p24-oid IS INITIAL.
    CLEAR: it_dados_p24-cd_item, it_dados_p24-nm_item.
  ENDIF.


  p24_gr_table->refresh( ).
  p24_gr_table->display( ).

ENDFORM.

FORM check_values.

  LOOP AT it_dados_p24 ASSIGNING FIELD-SYMBOL(<it_dados_p24>).

    DATA : l_dupl TYPE i.
    CLEAR: l_dupl.
*    LOOP AT it_dados_p24 ASSIGNING FIELD-SYMBOL(<duplicados>) WHERE cd_indic = p_cdind-low AND  cd_item = <it_dados_p24>-cd_item.
*      l_dupl = l_dupl + 1.
*    ENDLOOP.

    IF l_dupl > 1.
      "DELETE it_dados_p24 WHERE cd_indic = p_cdind-low AND cd_item = <it_dados_p24>-cd_item.
    ELSE.

      IF <it_dados_p24>-cd_indic IS NOT INITIAL AND <it_dados_p24>-cd_item IS NOT INITIAL.
        DATA: aux_nmitem TYPE zfit0200-nm_item.
        CLEAR: aux_nmitem.
        SELECT SINGLE nm_item FROM zfit0200 INTO @aux_nmitem WHERE cd_indic IN @p_cdind AND cd_item = @<it_dados_p24>-cd_item.
        <it_dados_p24>-nm_item = aux_nmitem.
      ENDIF.

      IF <it_dados_p24>-oid IS INITIAL.
        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_16 = <it_dados_p24>-oid.
      ENDIF.

      IF <it_dados_p24>-cd_indic IS NOT INITIAL AND <it_dados_p24>-cd_item IS NOT INITIAL AND <it_dados_p24>-oid IS NOT INITIAL.
        "Somente gravar se tiver OID
        MODIFY zfit0202 FROM <it_dados_p24>.
      ENDIF.

    ENDIF.

  ENDLOOP.
ENDFORM.

FORM set_columns_technical_p24 USING p24_ir_columns TYPE REF TO cl_salv_columns.
*...Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)P23_lr_column
  TRY.
      p24_lr_column = p24_ir_columns->get_column( 'MAKTX' ).
      p24_lr_column->set_optimized( abap_true ).
      p24_lr_column->set_short_text( 'Desc.Mat.' ).
      p24_lr_column->set_medium_text( 'Descrição Material' ).
      p24_lr_column->set_long_text( 'Desccrição Material' ).
      "p24_lr_column->set_output_length('5').

      p24_lr_column = p24_ir_columns->get_column( 'MATNR' ).
      p24_lr_column->set_optimized( abap_true ).
      p24_lr_column->set_short_text( 'Cod.Mat.' ).
      p24_lr_column->set_medium_text( 'Código Material' ).
      p24_lr_column->set_long_text( 'Código Material' ).
      "p24_lr_column->set_output_length('10').

      p24_lr_column = p24_ir_columns->get_column( 'WGBEZ60' ).
      p24_lr_column->set_optimized( abap_false ).
      p24_lr_column->set_short_text( 'Desc.Grupo' ).
      p24_lr_column->set_medium_text( 'Descrição Grupo' ).
      p24_lr_column->set_long_text( 'Descrição Grupo' ).
      p24_lr_column->set_output_length('30').

      p24_lr_column = p24_ir_columns->get_column( 'CD_INDIC' ).
      p24_lr_column->set_optimized( abap_false ).
      p24_lr_column->set_short_text( 'Cod.ID.SIS' ).
      p24_lr_column->set_medium_text( 'Codt.ID.SIS' ).
      p24_lr_column->set_long_text( 'Cód. Ident. SIS' ).
      p24_lr_column->set_output_length('10').

      p24_lr_column = p24_ir_columns->get_column( 'CD_ITEM' ).
      p24_lr_column->set_optimized( abap_false ).
      p24_lr_column->set_short_text( 'Cód.Item' ).
      p24_lr_column->set_medium_text( 'Cód. Item SIS' ).
      p24_lr_column->set_long_text( 'Código Item SIS' ).
      p24_lr_column->set_output_length('10').

      p24_lr_column = p24_ir_columns->get_column( 'NM_ITEM' ).
      p24_lr_column->set_optimized( abap_true ).
      p24_lr_column->set_short_text( 'NM ITEM' ).
      p24_lr_column->set_medium_text( 'NOME ITEM SIS' ).
      p24_lr_column->set_long_text( 'NOME ITEM SIS' ).
      "p24_lr_column->set_output_length('30').

      p24_lr_column = p24_ir_columns->get_column( 'OID' ).
      p24_lr_column->set_visible( if_salv_c_bool_sap=>false ).

      p24_lr_column = p24_ir_columns->get_column( 'MANDT' ).
      p24_lr_column->set_visible( if_salv_c_bool_sap=>false ).

    CATCH cx_salv_not_found INTO p24_lex_not_found.
      "write some error handling
  ENDTRY.

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

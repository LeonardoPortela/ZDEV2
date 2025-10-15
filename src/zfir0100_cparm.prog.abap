*&---------------------------------------------------------------------*
*& Report ZFIR0100_CPARM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfir0100_cparm.


DATA : oid  TYPE numc4.
DATA: it_dados TYPE STANDARD TABLE OF zfit0100_cont WITH HEADER LINE.
DATA: wa_dados TYPE zfit0100_cont.
DATA: aux_dados TYPE zfit0100_cont.
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
      l_icon_can   TYPE string,
      l_text_nov   TYPE string,
      l_icon_nov   TYPE string.

DATA:
  container_main TYPE REF TO cl_gui_custom_container,
  painel_control TYPE REF TO cl_gui_splitter_container,
  painel1        TYPE REF TO cl_gui_container,
  painel2        TYPE REF TO cl_gui_container.

DATA: lo_selections TYPE REF TO cl_salv_selections.
DATA lt_rows TYPE salv_t_row.
DATA ls_row TYPE int4.
DATA: msg_text TYPE char45.

DATA LV_smode TYPE string.
DATA LV_ccell TYPE salv_s_cell.
DATA LV_scellS TYPE salv_T_cell.
DATA LV_scolumnS TYPE salv_t_column.
DATA LV_srows TYPE salv_t_row.

*SELECTION-SCREEN BEGIN OF BLOCK a WITH FRAME TITLE TEXT-001.
*
*  SELECT-OPTIONS p_cdind FOR ZFIT0100_CONT-cd_indic NO INTERVALS.
*
*SELECTION-SCREEN END OF BLOCK a.


START-OF-SELECTION.
  PERFORM container_main.
  PERFORM display_grid.

  "CLASSE
  CLASS lcl_handle_events DEFINITION DEFERRED.
  DATA gr_events TYPE REF TO lcl_handle_events.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS:
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_before_user_command FOR EVENT before_salv_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_after_user_command FOR EVENT after_salv_function OF cl_salv_events IMPORTING e_salv_function sender.
ENDCLASS.                    "cl_event_handler DEFINITION

CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i08.
  ENDMETHOD.

  METHOD on_before_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i09.
  ENDMETHOD.

  METHOD on_after_user_command.
    PERFORM show_function_info USING e_salv_function TEXT-i10.
  ENDMETHOD.

ENDCLASS.

FORM show_function_info USING i_function TYPE salv_de_function
                              i_text     TYPE string.
  DATA: l_string TYPE string.
  "concatenate i_text i_function into l_string separated by space.
  "message i000(0k) with l_string.

  CASE i_function.
*     Make ALV as Editable ALV
    WHEN 'EDIT'.
      ls_api = gr_table->extended_grid_api( ).
      ls_edit = ls_api->editable_restricted( ).

      TRY.
          ls_edit->set_attributes_for_columnname(
      EXPORTING
        columnname = 'BUKRS'
        all_cells_input_enabled = abap_true
      ).
          ls_edit->set_attributes_for_columnname(
            EXPORTING
              columnname = 'RACCT'
              all_cells_input_enabled = abap_true
              ).
        CATCH cx_salv_not_found.
      ENDTRY.

      ls_edit->validate_changed_data(
    ).
      PERFORM atuliza.

    WHEN 'SAVE'.

      PERFORM pega_dados_cell_column.

        LOOP AT lv_srows INTO ls_row.
          READ TABLE it_dados INTO wa_dados INDEX ls_row.

         CONDENSE wa_dados-racct NO-GAPS.
          UNPACK wa_dados-racct TO wa_dados-racct.

         CONDENSE wa_dados-bukrs NO-GAPS.
          UNPACK wa_dados-bukrs TO wa_dados-bukrs.

          MODIFY zfit0100_cont FROM wa_dados.
          COMMIT WORK.
        ENDLOOP.

      PERFORM atuliza.

    WHEN 'DELETE'.

      PERFORM pega_dados_cell_column.

      IF lv_srows IS INITIAL.
        msg_text = 'Selecione ao manos uma linha!'.
        MESSAGE msg_text TYPE 'I'.

      ELSE.

        LOOP AT lv_srows INTO ls_row.
          READ TABLE it_dados INTO wa_dados INDEX ls_row.
          DELETE zfit0100_cont FROM wa_dados.
          COMMIT WORK.
        ENDLOOP.

      ENDIF.
      PERFORM atuliza.

    WHEN 'REFRESH'.
      PERFORM atuliza.


    WHEN 'NOVO'.


      DATA : AUX_zfit0100_cont TYPE zfit0100_cont.
      DATA : IT_zfit0100_cont TYPE STANDARD TABLE OF zfit0100_cont WITH HEADER LINE .
      DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.

      DATA: ivals  TYPE STANDARD TABLE OF sval WITH HEADER LINE.
      DATA: xvals  TYPE sval.

      xvals-tabname   = 'ZFIT0100_CONT'.
      xvals-fieldname = 'BUKRS'.
      xvals-field_obl = 'X'.
      APPEND xvals TO ivals[].

      xvals-tabname   = 'ZFIT0100_CONT'.
      xvals-fieldname = 'RACCT'.
      xvals-field_obl = 'X'.
      APPEND xvals TO ivals[].




      DATA w_retn TYPE string.
      CLEAR: w_retn.

      CALL FUNCTION 'POPUP_GET_VALUES'
        EXPORTING
          "no_value_check  = 'X'
          popup_title     = 'Cadastro de Contas'
*         START_COLUMN    = '5'
*         START_ROW       = '5'
        IMPORTING
          returncode      = w_retn
        TABLES
          fields          = ivals[]
        EXCEPTIONS
          error_in_fields = 1
          OTHERS          = 2.
      IF sy-subrc <> 0.
      ENDIF.

      LOOP AT ivals[] ASSIGNING FIELD-SYMBOL(<field>) GROUP BY <field>-fieldname .
        CASE <field>-fieldname.
          WHEN 'RACCT'.
            IT_zfit0100_cont-racct = <field>-value.
          WHEN 'BUKRS'.
            IT_zfit0100_cont-bukrs = <field>-value.
        ENDCASE.
      ENDLOOP.



      SELECT SINGLE * FROM zfit0100_cont
    WHERE 1 = 1
    AND racct = @IT_zfit0100_cont-racct
        AND bukrs = @IT_zfit0100_cont-bukrs
        INTO @AUX_zfit0100_cont.

      MODIFY zfit0100_cont FROM AUX_zfit0100_cont.
      COMMIT WORK.


      PERFORM atuliza.

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
FORM set_columns_technical USING ir_columns TYPE REF TO cl_salv_columns.

*...Colunas ALV - Alterar Nome Coluna (Pequeno,Médio Grande)lr_column
  TRY.

      lr_column = ir_columns->get_column( 'BUKRS' ).
      lr_column->set_optimized( abap_false ).
      lr_column->set_short_text( 'Empresa' ).
      lr_column->set_medium_text( 'Empresa' ).
      lr_column->set_long_text( 'Empresa' ).
      lr_column->set_alignment( if_salv_c_alignment=>centered ).

      lr_column = ir_columns->get_column( 'RACCT' ).
      lr_column->set_short_text( 'Conta' ).
      lr_column->set_medium_text( 'Conta' ).
      lr_column->set_long_text( 'Conta' ).

      lr_column = ir_columns->get_column( 'MANDT' ).
      lr_column->set_visible( if_salv_c_bool_sap=>false ).


    CATCH cx_salv_not_found INTO lex_not_found.
      "write some error handling
  ENDTRY.

ENDFORM.

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
  l_text_nov = 'Novo'.
  l_icon_nov = icon_add_row.
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
 name     = 'NOVO'
 icon     = l_icon_nov
 text     = l_text_nov
 tooltip  = 'Novo Item'
 position = if_salv_c_function_position=>right_of_salv_functions ).

    CATCH cx_salv_existing cx_salv_wrong_call.
  ENDTRY.

*... set the columns technical
  DATA: lr_columns    TYPE REF TO cl_salv_columns,
        lr_column     TYPE REF TO cl_salv_column_table,
        lr_selections TYPE REF TO cl_salv_selections.

  DATA: lr_layout TYPE REF TO cl_salv_layout, " Variables for enabling Save button
        lv_key    TYPE salv_s_layout_key.

  lr_columns = gr_table->get_columns( ).

  lr_columns->set_column_position(  columnname = 'BUKRS' position = 1 ).
  lr_columns->set_column_position(  columnname = 'RACCT' position = 2 ).

  lr_columns->apply_ddic_structure( name = 'RACCT' ).


  lr_columns->set_optimize( abap_true ).

  PERFORM set_columns_technical USING lr_columns.

*... F4 DDIC

  DATA lv_ddic TYPE salv_s_ddic_reference.

  TRY.
      lr_column ?= lr_columns->get_column( columnname = 'BUKRS' ).
      lv_ddic = VALUE #( table = 'ZFIT0100_CONT' field = 'BUKRS').
      lr_column->set_ddic_reference( lv_ddic  ). "EXPORTING value = lv_ddic
      lr_column->set_f4(  if_salv_c_bool_sap=>true ).

      lr_column ?= lr_columns->get_column( columnname = 'RACCT' ).
      lv_ddic = VALUE #( table = 'ZFIT0100_CONT' field = 'RACCT').
      lr_column->set_ddic_reference( lv_ddic  ). "EXPORTING value = lv_ddic
      lr_column->set_f4(  if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.
  ENDTRY.


*... §4 set hotspot column
*  TRY.
*      lr_column ?= lr_columns->get_column( 'MATNR' ).
*      lr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*    CATCH cx_salv_not_found.                            "#EC NO_HANDLER
*  ENDTRY.

  "... §6 register to the events of cl_salv_table
  DATA lr_events TYPE REF TO cl_salv_events_table.
  lr_events = gr_table->get_event( ).

  CREATE OBJECT gr_events.

*... §6.1 register to the event USER_COMMAND

  SET HANDLER gr_events->on_user_command FOR lr_events.
  SET HANDLER gr_events->on_before_user_command FOR lr_events.
  SET HANDLER gr_events->on_after_user_command FOR lr_events.

*... set list title
  DATA: lr_display_settings TYPE REF TO cl_salv_display_settings,
        l_title             TYPE lvc_title.

  l_title = |Cadastro de Contas|.
  lr_display_settings = gr_table->get_display_settings( ).
  lr_display_settings->set_list_header_size( '1' ).
  lr_display_settings->set_list_header( l_title ).
  lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
  lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
  lr_display_settings->set_list_header( l_title ).


* Enable cell selection mode
  lr_selections = gr_table->get_selections( ).
  lr_selections->set_selection_mode( if_salv_c_selection_mode=>cell ). "linha por linha row_column

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
  PERFORM seleciona_dados.
  gr_table->refresh( ).
  gr_table->display( ).
ENDFORM.

FORM pega_dados_cell_column.
  gr_table->get_metadata( ). " Call this method before getting selected rows

  CLEAR: LV_scellS,LV_scolumns,LV_srows,LV_ccell,LV_smode.

  LV_smode = gr_table->get_selections( )->get_selection_mode( ).
  LV_ccell = gr_table->get_selections( )->get_current_cell( ).
  LV_scellS = gr_table->get_selections( )->get_selected_cells( ).
  LV_scolumns = gr_table->get_selections( )->get_selected_columns( ).
  LV_srows = gr_table->get_selections( )->get_selected_rows( ).

ENDFORM.


FORM completa_campos.

  LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<it_dados>).
*    IF <it_dados>-cd_indic IS INITIAL.
*      "<it_dados>-cd_indic = p_cdind.
*    ENDIF.
  ENDLOOP.

*  LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<it_dados>).
*    IF <it_dados>-cd_indic IS NOT INITIAL.
*      SELECT SINGLE nm_indic FROM zfit0201
*        INTO <it_dados>-nm_indic
*        WHERE cd_indic = <it_dados>-cd_indic.
*    ENDIF.
*  ENDLOOP.
ENDFORM.

FORM seleciona_dados.
  SELECT * FROM zfit0100_cont INTO TABLE @it_dados.
ENDFORM.



FORM display_grid.

  CALL SCREEN 0100.

ENDFORM.


FORM gerate_grid .

  PERFORM docker.
  PERFORM container_main.

ENDFORM.                                                    " ZFIR0100_CPARM_pbo


FORM press_key .

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
*    LOOP AT it_dados ASSIGNING FIELD-SYMBOL(<duplicados>) WHERE kostl = <it_dados>-kostl.
*      l_dupl = l_dupl + 1.
*    ENDLOOP.

*    IF l_dupl > 1.
*      DELETE it_dados WHERE kostl = <it_dados>-kostl.
*    ELSE.
*      IF <it_dados>-cd_indic IS INITIAL.
*        "<it_dados>-cd_indic = p_cdind-low.
*      ENDIF.
*      IF <it_dados>-oid IS INITIAL.
*        CALL FUNCTION 'GUID_CREATE'
*          IMPORTING
*            ev_guid_16 = <it_dados>-oid.
*      ENDIF.
*      MODIFY ZFIT0100_CONT FROM <it_dados>.
*    ENDIF.
  ENDLOOP.
ENDFORM.


MODULE pbo OUTPUT.
  PERFORM out.
ENDMODULE.

MODULE pai INPUT.

ENDMODULE.

MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN '&F03' OR '&ENTER'. " Check for Enter key press
      "BREAK-POINT.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'BACK'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.


FORM out.
  SET PF-STATUS 'ZSTATUS'.
  SET TITLEBAR 'ZTITULO'.
  PERFORM gerate_grid.
ENDFORM.

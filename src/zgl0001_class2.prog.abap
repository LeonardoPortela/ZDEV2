

CLASS lo_report2 DEFINITION DEFERRED.
DATA: lo_report2 TYPE REF TO lo_report2.

CLASS lcl_listener2 DEFINITION.
  PUBLIC SECTION.
    INTERFACES :
      if_salv_gui_om_edit_strct_lstr.
ENDCLASS.

CLASS lcl_listener2 IMPLEMENTATION.
  METHOD if_salv_gui_om_edit_strct_lstr~on_check_changed_data.
    o_ui_data_modify->get_ui_changes( IMPORTING t_modified_cells = DATA(lt_modified) ).
  ENDMETHOD.
ENDCLASS.


CLASS lo_report2 DEFINITION.
  PUBLIC SECTION .

    DATA: container_main TYPE REF TO cl_gui_custom_container,
          painel_control TYPE REF TO cl_gui_splitter_container,
          painel1        TYPE REF TO cl_gui_container,
          painel2        TYPE REF TO cl_gui_container.

    DATA: o_alv     TYPE REF TO cl_salv_table.
    METHODS:
      get_data,
      generate_output,
      set_HANDLER
        CHANGING co_alV    TYPE REF TO cl_salv_table
                 co_report TYPE REF TO lo_report2,
      set_refresh
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      set_columns_build
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      set_pf_status
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      set_layout
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      set_edit_alv
        CHANGING
          co_alV TYPE REF TO cl_salv_table,
      on_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive
          sender.

    METHODS: on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function sender,
      on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_change_data FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

ENDCLASS.
CLASS lo_report2 IMPLEMENTATION.
  METHOD set_refresh.
    lo_report2->get_data( ).
    co_alv->refresh( ).
  ENDMETHOD.

  METHOD on_change_data.

    DATA: it_changed TYPE STANDARD TABLE OF zfise46 INITIAL SIZE 0.
    "CLEAR: it_changed,wa_saida.

    DATA(inserted_tab) = er_data_changed->mt_inserted_rows.
    DATA(deleted_tab)  = er_data_changed->mt_deleted_rows.

    FIELD-SYMBOLS: <itab>        TYPE ANY TABLE,
                   <struct>      TYPE any,
                   <it_mod_rows> TYPE ANY TABLE,
                   <wa_mod_rows> TYPE any.


    DATA: lt_good_cells TYPE lvc_t_modi,
          ls_good_cell  TYPE lvc_s_modi.


    ASSIGN er_data_changed->mp_mod_rows->* TO <it_mod_rows>.
    MOVE-CORRESPONDING <it_mod_rows> TO it_changed.

    lt_good_cells = er_data_changed->mt_good_cells.

  ENDMETHOD.

  METHOD set_columns_build .
*...Get all the Columns
    DATA: lo_cols        TYPE REF TO cl_salv_columns,
          lo_cols_ref    TYPE        salv_t_column_ref,
          lo_cols_list   TYPE REF TO cl_salv_column_list,
          lo_col_list    LIKE LINE OF lo_cols_ref,
          lo_column      TYPE REF TO cl_salv_column,
          ls_ddic_f4_ref TYPE salv_s_ddic_reference.

    lo_cols = co_alv->get_columns( ).
    "lo_cols->set_optimize( abap_false ).

    lo_cols_ref    = lo_cols->get( ).


    TRY.

        LOOP AT lo_cols_ref INTO lo_col_list.
          lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
          CLEAR: ls_ddic_f4_ref.
          CASE lo_col_list-columnname.
            WHEN 'RACCT' .
              lo_cols_list->set_short_text( 'Cont.Razão' ).
              lo_cols_list->set_medium_text( 'Conta Razão'  ).
              lo_cols_list->set_long_text( 'Conta Razão'  ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'MATNR' .
              lo_cols_list->set_short_text( 'Material' ).
              lo_cols_list->set_medium_text( 'Material' ).
              lo_cols_list->set_long_text( 'Material' ).
              lo_cols_list->set_output_length( '25' ).
            WHEN 'MSL'.
              lo_cols_list->set_short_text( 'Qtd' ).
              lo_cols_list->set_medium_text( 'Qtd' ).
              lo_cols_list->set_long_text( 'Qtd' ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'TSL' .
              lo_cols_list->set_short_text( 'Mont.MI' ).
              lo_cols_list->set_medium_text( 'Montante MI' ).
              lo_cols_list->set_long_text( 'Montante MI' ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'KSL' .
              lo_cols_list->set_short_text( 'Mont.MI2' ).
              lo_cols_list->set_medium_text( 'Montante MI2' ).
              lo_cols_list->set_long_text( 'Montante MI2' ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'PRECOB' .
              lo_cols_list->set_short_text( 'Preço BRL' ).
              lo_cols_list->set_medium_text( 'Preço BRL' ).
              lo_cols_list->set_long_text( 'Preço BRL' ).
              lo_cols_list->set_output_length( '15' ).

            WHEN 'PRECO' .
              lo_cols_list->set_short_text( 'Preço USD' ).
              lo_cols_list->set_medium_text( 'Preço USD' ).
              lo_cols_list->set_long_text( 'Preço USD' ).
              lo_cols_list->set_output_length( '15' ).

            WHEN OTHERS.
              lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
          ENDCASE.
        ENDLOOP.
      CATCH cx_salv_not_found.

    ENDTRY.

    lo_cols->set_column_position( columnname = 	'RACCT'	  position  = 01   ).
    lo_cols->set_column_position( columnname = 	'MATNR'	  position  = 02   ).
    lo_cols->set_column_position( columnname = 	'MSL'	    position  = 03   ).
    lo_cols->set_column_position( columnname = 	'TSL'	    position  = 04   ).
    lo_cols->set_column_position( columnname = 	'KSL'	    position  = 05   ).
    lo_cols->set_column_position( columnname =  'PRECOB'  position  = 06   ).
*    lo_cols->set_column_position( columnname =   'PRECO'   position  = 07   ).
    lo_cols->set_column_position( columnname = 	'PRECO_DOLAR'	  position  = 07   ).
    lo_cols->set_column_position( columnname =  'PRECO_REAL'    position  = 08   ).
    lo_cols->set_column_position( columnname = 	'PRECO_TOTAL'	  position  = 09   ).


  ENDMETHOD.

  METHOD set_edit_alv.
    DATA:ls_api  TYPE REF TO if_salv_gui_om_extend_grid_api,
         ls_edit TYPE REF TO if_salv_gui_om_edit_restricted.

    DATA: lv_ref_table  TYPE REF TO cl_abap_tabledescr,
          lv_ref_struct TYPE REF TO cl_abap_structdescr.

    ls_api = o_alv->extended_grid_api( ).
    ls_edit = ls_api->editable_restricted( ).
    lv_ref_table  ?= cl_abap_tabledescr=>describe_by_data( it_saida2 ).
    lv_ref_struct ?= lv_ref_table->get_table_line_type( ).
    DATA(lt_details)   = lv_ref_struct->components.

    LOOP AT lt_details ASSIGNING FIELD-SYMBOL(<_details>).
      ls_edit->set_attributes_for_columnname(
        EXPORTING
          columnname              = <_details>-name
          all_cells_input_enabled = abap_true
      ).

    ENDLOOP.

    DATA(mo_listener) = NEW lcl_listener1( ).
    ls_edit->set_listener( mo_listener ).
    ls_edit->validate_changed_data(
).

    o_alv->refresh( ).
  ENDMETHOD.

  METHOD get_data.

    PERFORM get_dados_alv2.


  ENDMETHOD.


  METHOD set_pf_status.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.
    lo_functions = co_alv->get_functions( ).
    lo_functions->set_all( abap_true ).
    "lo_functions->set_default( abap_true ).

*    TRY.
*        lo_functions->add_function( name     = 'GRAVAR'
*                                    icon     = '@2L@'
*                                    text     = 'Gravar'
*                                    tooltip  = 'Gravar em Tabela'
*                                    position = if_salv_c_function_position=>right_of_salv_functions ).
*
*
*      CATCH cx_root.
*
*    ENDTRY.


  ENDMETHOD.

  METHOD on_user_command.

    DATA: lo_selections TYPE REF TO cl_salv_selections.
    DATA lt_rows TYPE salv_t_row.
    DATA lt_columns TYPE salv_t_column.
    DATA lt_cells TYPE salv_t_cell.
    DATA qtd_rows TYPE int4.

    FREE: lt_rows.
    CLEAR: qtd_rows.

    lo_selections = o_alv->get_selections( ).
    lt_rows = lo_selections->get_selected_rows( ).
    qtd_rows = lines( lt_rows ).


    CASE e_salv_function.

      WHEN 'GRAVAR'.

        IF it_saida2 IS NOT INITIAL.
          LOOP AT it_saida2 ASSIGNING FIELD-SYMBOL(<_read>).
            "MODIFY zmmt0185 FROM <_read>.
          ENDLOOP.
        ELSE.
          MESSAGE 'Teste' TYPE 'I'.
        ENDIF.

      WHEN 'DELETE_ROW'.

        IF qtd_rows > 0.
          LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
            READ TABLE it_saida2 ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
            "DELETE zmmt0185 FROM <_del>.
          ENDLOOP.

        ELSE.
          MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
          EXIT.
        ENDIF.

        CALL METHOD set_refresh CHANGING co_alv = o_alv.

      WHEN 'REFRESH_ROW'.
        CALL METHOD set_refresh CHANGING co_alv = o_alv.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.

  METHOD on_toolbar.

    DATA : mt_toolbar TYPE stb_button.

    CLEAR mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    APPEND mt_toolbar TO e_object->mt_toolbar.

    LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
      "3 DESABILITA E 0 HABILITA
      IF  <fs_tollbar>-function EQ '&LOCAL&COPY_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&CREATE_ROW'.
        "<fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
        <fs_tollbar>-butn_type = '3'.
      ENDIF.
      IF <fs_tollbar>-function EQ '&REFRESH'.
        <fs_tollbar>-function = 'REFRESH_ROW'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
        <fs_tollbar>-function = 'DELETE_ROW'.
      ENDIF.
    ENDLOOP.

*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'INSERT_ROW'.   "fcode
*    mt_toolbar-icon = '@B_INSR@'.
*    mt_toolbar-quickinfo = 'Inserir linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.

  METHOD generate_output.

    container_main = NEW cl_gui_custom_container(
      parent         = cl_gui_container=>default_screen
      "lifetime       =  cl_gui_container=>lifetime_dynpro
      container_name = 'CONTAINER2'
    ).

*** Cria Splitter Container
*  CREATE OBJECT painel_control
*    EXPORTING
*      parent  = container_main
*      rows    = 1
*      columns = 1
*      align   = 70.
**
*** Exibe Painel 1
*  CALL METHOD painel_control->get_container
*    EXPORTING
*      row       = 1
*      column    = 1
*    RECEIVING
*      container = painel1.

    DATA: lx_msg TYPE REF TO cx_salv_msg.


    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = container_main
            container_name = 'CONTAINER2'
          IMPORTING
            r_salv_table   = o_alv
          CHANGING
            t_table        = it_saida2 ).
      CATCH cx_salv_msg INTO lx_msg.
    ENDTRY.

    CALL METHOD set_pf_status
      CHANGING
        co_alv = o_alv.

    CALL METHOD set_layout
      CHANGING
        co_alv = o_alv.

    CALL METHOD set_HANDLER
      CHANGING
        co_alv    = o_alv
        co_report = lo_report2.

    CALL METHOD me->set_columns_build
      CHANGING
        co_alv = o_alv.

*    DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
*    DATA l_title              TYPE lvc_title.
*    l_title = |Nome Relatório|.
*    lr_display_settings = o_alv->get_display_settings( ).
*    lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
*    lr_display_settings->set_list_header( l_title ).
*    lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
*    lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).
*    lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ). "Enable Zebra Layout
    DATA lr_selections        TYPE REF TO cl_salv_selections.
* Enable cell selection mode
    lr_selections = o_alv->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

    o_alv->display( ).

*    CALL METHOD set_edit_alv "Este metodo precisa ser após a saida do display para que ele possa dar um refresh
*      CHANGING
*        co_alv = o_alv.


  ENDMETHOD.

  METHOD set_HANDLER.
*
*...HotSpot
    DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,
          lo_col_tab  TYPE REF TO cl_salv_column_table,
          lo_events   TYPE REF TO cl_salv_events_table.

    lo_cols_tab = co_alv->get_columns( ).
    lo_events = co_alv->get_event( ).

*   event handler
    SET HANDLER co_report->on_link_click FOR lo_events.
    SET HANDLER co_report->on_user_command FOR lo_events.
    SET HANDLER co_report->on_toolbar FOR ALL INSTANCES ACTIVATION 'X'.
    SET HANDLER co_report->on_change_data FOR ALL INSTANCES ACTIVATION 'X'.
    "SET HANDLER co_report->on_after_refresh FOR ALL INSTANCES ACTIVATION 'X'.
  ENDMETHOD.

  METHOD on_link_click.

*    DATA: wa_saida TYPE zpme0087.
*    CLEAR: wa_saida.
*
*    IF column = ''.
*      "READ TABLE lo_report2->it_saida2 INTO wa_saida INDEX row.
*
*    ENDIF.

  ENDMETHOD.


  METHOD set_layout.
*
    DATA: lo_layout  TYPE REF TO cl_salv_layout,
          lf_variant TYPE slis_vari,
          ls_key     TYPE salv_s_layout_key.
*   get layout object
    lo_layout = co_alv->get_layout( ).
*   set Layout save restriction
*   1. Set Layout Key .. Unique key identifies the Differenet ALVs
    ls_key-report = sy-repid.
    lo_layout->set_key( ls_key ).
*   2. Remove Save layout the restriction.
    lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   set initial Layout
    lf_variant = 'DEFAULT'.
    lo_layout->set_initial_layout( lf_variant ).
  ENDMETHOD.

ENDCLASS.

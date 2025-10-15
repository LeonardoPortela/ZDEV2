
    DATA: container_main TYPE REF TO cl_gui_custom_container,
          painel_control TYPE REF TO cl_gui_splitter_container,
          painel1        TYPE REF TO cl_gui_container,
          painel2        TYPE REF TO cl_gui_container.
    CLASS lcl_report DEFINITION DEFERRED.
    DATA: lo_report TYPE REF TO lcl_report.
    CLASS lcl_report DEFINITION.
      PUBLIC SECTION .

        DATA: o_alv  TYPE REF TO cl_salv_table,
              t_salv TYPE STANDARD TABLE OF REF TO cl_salv_table.
        METHODS:
          get_data,
          generate_output,
          set_HANDLER
            CHANGING co_alV    TYPE REF TO cl_salv_table
                     co_report TYPE REF TO lcl_report,
          set_refresh,
          "CHANGING co_alv TYPE REF TO cl_salv_table,
          set_columns_build
            CHANGING
              co_alv TYPE REF TO cl_salv_table,
          set_columns_position
            CHANGING
              co_alv TYPE REF TO cl_salv_table,
          set_f4
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
    CLASS lcl_report IMPLEMENTATION.
      METHOD set_refresh.
        lo_report->get_data( ).
        o_alv->refresh( ).
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
*
*...Get all the Columns
        DATA: lo_cols   TYPE REF TO cl_salv_columns,
              lo_column TYPE REF TO cl_salv_column.

        lo_cols = co_alv->get_columns( ).
        lo_cols->set_optimize( abap_false ).


        DATA: lo_cols_ref  TYPE        salv_t_column_ref,
              lo_cols_list TYPE REF TO cl_salv_column_list,
              lo_col_list  LIKE LINE OF lo_cols_ref.

        lo_cols = o_alv->get_columns( ).
        "lo_cols_c1->set_optimize( abap_false ).
        DATA: ls_ddic_f4_ref TYPE salv_s_ddic_reference.

        lo_cols_ref    = lo_cols->get( ).

        TRY.

            LOOP AT lo_cols_ref INTO lo_col_list.
              lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
              CASE lo_col_list-columnname.
                WHEN 'BUKRS' .
                  lo_cols_list->set_short_text( 'Empresa' ).
                  lo_cols_list->set_medium_text( 'Empresa' ).
                  lo_cols_list->set_long_text( 'Empresa' ).
                  lo_cols_list->set_optimized( abap_false ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'WERKS'.
                  lo_cols_list->set_short_text( 'Filial' ).
                  lo_cols_list->set_medium_text( 'Filial' ).
                  lo_cols_list->set_long_text( 'Filial' ).
                  lo_cols_list->set_optimized( abap_false ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'GJAHR'.
                  lo_cols_list->set_short_text( 'Ano' ).
                  lo_cols_list->set_medium_text( 'Ano' ).
                  lo_cols_list->set_long_text( 'Ano' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '6' ).
                WHEN 'MONAT'.
                  lo_cols_list->set_short_text( 'Mês' ).
                  lo_cols_list->set_medium_text( 'Mês' ).
                  lo_cols_list->set_long_text( 'Mês' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '6' ).
                WHEN 'SALDO_FIXO'.
                  lo_cols_list->set_short_text( 'Saldo Fixo' ).
                  lo_cols_list->set_medium_text( 'Saldo Fixo' ).
                  lo_cols_list->set_long_text( 'Saldo Fixo' ).
                  lo_cols_list->set_optimized( abap_false ).
                  "lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '15' ).
                WHEN OTHERS.
                  lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
              ENDCASE.
            ENDLOOP.
          CATCH cx_salv_not_found.
        ENDTRY.


      ENDMETHOD.

      METHOD set_edit_alv.
*    DATA ls_api               TYPE REF TO if_salv_gui_om_extend_grid_api.
*    DATA ls_edit              TYPE REF TO if_salv_gui_om_edit_restricted.
*
*    ls_api = co_alv->extended_grid_api( ).
*    ls_edit = ls_api->editable_restricted( ).
*
*    TRY.
*
*        ls_edit->set_attributes_for_columnname( columnname              = 'BUKRS'
*                                                all_cells_input_enabled = abap_true ).
*      CATCH cx_salv_not_found.
*    ENDTRY.
*    ls_edit->validate_changed_data( ).
*    co_alv->refresh( ).
      ENDMETHOD.

      METHOD get_data.

        FREE: it_saida.

        SELECT DISTINCT *
        FROM
          zfit0216
          WHERE 1 = 1
*          AND saknr IN @p_SAKNR
          INTO TABLE @DATA(it_sap).

        MOVE-CORRESPONDING it_sap TO it_saida.


      ENDMETHOD.


      METHOD set_pf_status.

        DATA: lo_functions TYPE REF TO cl_salv_functions_list.
        lo_functions = co_alv->get_functions( ).
        lo_functions->set_all( abap_true ).
        "lo_functions->set_default( abap_true ).

        TRY.
            lo_functions->add_function( name     = 'NOVO'
                                        icon     = '@B_INSR@'
                                        text     = 'Novo'
                                        tooltip  = 'Novo'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

            lo_functions->add_function( name     = 'DELETAR'
                                        icon     = '@B_DELR@'
                                        text     = 'Delete'
                                        tooltip  = 'Delete'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).


            lo_functions->add_function( name     = 'ATUALIZAR'
                                        icon     = '@B_REFR@'
                                        text     = 'Atualizar'
                                        tooltip  = 'Atualizar'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

          CATCH cx_root.

        ENDTRY.


      ENDMETHOD.

      METHOD set_columns_position.

        DATA: lo_cols TYPE REF TO cl_salv_columns.


*        lo_cols->set_column_position( columnname = 'CHAVE_NFE_SAP'      position  =  1   ).
*        lo_cols->set_column_position( columnname = 'EMPRESA'            position  = 2   ).
*        lo_cols->set_column_position( columnname = 'FILIAL'             position  = 3   ).

      ENDMETHOD.


      METHOD set_f4.
*
        DATA: lo_cols TYPE REF TO cl_salv_columns.
        lo_cols = co_alv->get_columns( ).

        DATA: lo_column TYPE REF TO cl_salv_column.
        DATA: lo_column_tab TYPE REF TO cl_salv_column_table.

*      " F4 DDIC

*    DATA lv_ddic TYPE salv_s_ddic_reference.
*
*    TRY.
*
*        lo_column_tab ?= lo_cols->get_column( columnname = 'BUKRS' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFDOC'  field = 'BUKRS').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).
*
*        lo_column_tab ?= lo_cols->get_column( columnname = 'BRANCH' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFDOC'  field = 'BRANCH').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).
*
*        lo_column_tab ?= lo_cols->get_column( columnname = 'NBM' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFLIN'  field = 'NBM').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).
*
*      CATCH cx_root.                                    "#EC NO_HANDLER
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

          WHEN 'NOVO'.
            CLEAR: wa_saida.
            CALL SCREEN 0200 STARTING AT 10 10 ENDING AT 50 15.
*            IF it_saida IS NOT INITIAL.
*              LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_read>).
*                "MODIFY zmmt0185 FROM <_read>.
*              ENDLOOP.
*            ELSE.
*              MESSAGE 'Teste' TYPE 'I'.
*            ENDIF.

          WHEN 'DELETAR'.
            IF qtd_rows > 0.
              DATA: wa_zfit0216 TYPE zfit0216.
              LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
                READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
                CLEAR: wa_zfit0216.
                MOVE-CORRESPONDING <_del> TO wa_zfit0216.
                DELETE zfit0216 FROM wa_zfit0216.
              ENDLOOP.
              lo_report->set_refresh( ).
            ELSE.
              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
              EXIT.
            ENDIF.
          WHEN 'ATUALIZAR'.
            CALL METHOD set_refresh." CHANGING co_alv = o_alv.
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
          container_name = 'CONTAINER'
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
                container_name = 'CONTAINER'
              IMPORTING
                r_salv_table   = o_alv
              CHANGING
                t_table        = it_saida ).
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
            co_report = lo_report.

*    CALL METHOD set_f4
*      CHANGING
*        co_alv = o_alv.

        CALL METHOD set_columns_position
          CHANGING
            co_alv = o_alv.

        CALL METHOD me->set_columns_build
          CHANGING
            co_alv = o_alv.

        DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
        DATA l_title              TYPE lvc_title.
        l_title = |Controle de Saldo|.
        lr_display_settings = o_alv->get_display_settings( ).
        lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
        lr_display_settings->set_list_header( l_title ).
        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
        lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).

        "Enable Zebra Layout
        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
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
*      "READ TABLE lo_report->it_saida INTO wa_saida INDEX row.
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

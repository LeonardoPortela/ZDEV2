
    DATA: container_main TYPE REF TO cl_gui_custom_container,
          painel_control TYPE REF TO cl_gui_splitter_container,
          painel1        TYPE REF TO cl_gui_container,
          painel2        TYPE REF TO cl_gui_container,
          lo_cols        TYPE REF TO cl_salv_columns,
          lo_cols_ref    TYPE        salv_t_column_ref,
          lo_cols_list   TYPE REF TO cl_salv_column_list,
          lo_col_list    LIKE LINE OF lo_cols_ref,
          lo_column      TYPE REF TO cl_salv_column,
          ls_ddic_f4_ref TYPE salv_s_ddic_reference.
    CLASS lcl_report DEFINITION DEFERRED.
    DATA: lo_report TYPE REF TO lcl_report.
    CLASS lcl_listener DEFINITION.
      PUBLIC SECTION.
        INTERFACES if_salv_gui_om_edit_strct_lstr.
    ENDCLASS.

    CLASS lcl_listener IMPLEMENTATION.

      METHOD if_salv_gui_om_edit_strct_lstr~on_f4_request.
      ENDMETHOD.

      METHOD if_salv_gui_om_edit_strct_lstr~on_check_changed_data.


*        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*          EXPORTING
*            functioncode           = '=ENT'
*          EXCEPTIONS
*            function_not_supported = 1
*            OTHERS                 = 2.

        o_ui_data_modify->get_ui_changes( IMPORTING t_deleted_rows = DATA(lt_deleted_rows) ).
        o_ui_data_modify->get_ui_changes( IMPORTING t_good_cells = DATA(lt_good_cells) ).
        o_ui_data_modify->get_ui_changes( IMPORTING t_inserted_rows = DATA(lt_inserted_rows) ).
        o_ui_data_modify->get_ui_changes( IMPORTING t_modified_cells = DATA(lt_modified_cells) ).
        o_ui_data_modify->get_ui_changes( IMPORTING rt_modified_data_rows = DATA(lt_modified_data_rows) ).

        LOOP AT lt_modified_cells ASSIGNING FIELD-SYMBOL(<_get_mod>).
          READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_it_saida_mod>) INDEX <_get_mod>-row_id.

          CASE <_get_mod>-fieldname.
            WHEN 'SUPL_ORC'.
              o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
                                                   fieldname  = 'SUPL_ORC'
                                                   cell_value = 'X' ).
            WHEN 'APROVADOR' OR 'USUA_SUBST'. "Trata usuarios
              CONDENSE <_get_mod>-value NO-GAPS.

              DATA(_campo) = <_get_mod>-fieldname.

              IF <_get_mod>-value IS NOT INITIAL.

                SELECT SINGLE * FROM usr21 WHERE bname = @<_get_mod>-value INTO @DATA(ls_USR21).

                IF sy-subrc = 0.
                  o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
                                                       fieldname  = _campo
                                                       cell_value = ls_USR21-bname ).
                  IF _campo = 'USUA_SUBST'.
                    DATA(_usua_subst) = ls_USR21-bname.
                  ENDIF.
                ELSE.
                  DATA(msg_erro_usr21) = |Nome do { _campo } não foi encontrado!|.
                  MESSAGE msg_erro_usr21 TYPE 'I' DISPLAY LIKE 'I'.
                  o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
                                                       fieldname  = _campo
                                                       cell_value = '' ).
                ENDIF.
              ENDIF.
          ENDCASE.
        ENDLOOP.



      ENDMETHOD.
    ENDCLASS.
    CLASS lcl_report DEFINITION.
      PUBLIC SECTION .

        DATA: "o_alv    TYPE REF TO cl_salv_table,
              t_salv   TYPE STANDARD TABLE OF REF TO cl_salv_table.
        METHODS:
          get_data,
          generate_output,
          set_HANDLER
            CHANGING co_alV    TYPE REF TO cl_salv_table
                     co_report TYPE REF TO lcl_report,
          set_refresh,
*            CHANGING
*              co_alv TYPE REF TO cl_salv_table,
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

        METHODS: on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function sender.

    ENDCLASS.
    CLASS lcl_report IMPLEMENTATION.
      METHOD set_refresh.
        lo_report->get_data( ).
        o_alv->refresh( ).
      ENDMETHOD.

      METHOD set_columns_build .

        lo_cols = o_alv->get_columns( ).
        "lo_cols->set_optimize( abap_false ).

        lo_cols_ref    = lo_cols->get( ).


        TRY.

            LOOP AT lo_cols_ref INTO lo_col_list.
              lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
              CLEAR: ls_ddic_f4_ref.
              CASE lo_col_list-columnname.
                WHEN 'BUKRS' .
                  lo_cols_list->set_short_text( 'Empresa' ).
                  lo_cols_list->set_medium_text( 'Empresa'  ).
                  lo_cols_list->set_long_text( 'Empresa'  ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'CENTRO_DESP' .
                  lo_cols_list->set_short_text( 'Centro' ).
                  lo_cols_list->set_medium_text( 'Centro'  ).
                  lo_cols_list->set_long_text( 'Centro'  ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'SUPL_ORC'.
                  lo_cols_list->set_short_text( 'Supl/Orça' ).
                  lo_cols_list->set_medium_text( 'Supl./Orça.'  ).
                  lo_cols_list->set_long_text( 'Supl./Orça.'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'NIVEL' .
                  lo_cols_list->set_short_text( 'Nível' ).
                  lo_cols_list->set_medium_text( 'Nível' ).
                  lo_cols_list->set_long_text( 'Nível'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '6' ).
                WHEN 'APROVADOR' .
                  lo_cols_list->set_short_text( 'Aprovador' ).
                  lo_cols_list->set_medium_text( 'Aprovador'  ).
                  lo_cols_list->set_long_text( 'Aprovador'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '15' ).
                WHEN 'PERMIT' .
                  lo_cols_list->set_short_text( 'Permit' ).
                  lo_cols_list->set_medium_text( 'Permit'  ).
                  lo_cols_list->set_long_text( 'Permit'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '12' ).
                WHEN 'WAERS' .
                  lo_cols_list->set_short_text( 'Moeda' ).
                  lo_cols_list->set_medium_text( 'Moeda'  ).
                  lo_cols_list->set_long_text( 'Moeda'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '6' ).
                WHEN 'VALOR_DE' .
                  lo_cols_list->set_short_text( 'Valor de' ).
                  lo_cols_list->set_medium_text( 'Valor de' ).
                  lo_cols_list->set_long_text( 'Valor de'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '15' ).
                WHEN 'VALOR_ATE' .
                  lo_cols_list->set_short_text( 'Valor até' ).
                  lo_cols_list->set_medium_text( 'Valor até' ).
                  lo_cols_list->set_long_text( 'Valor até'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '15' ).
                WHEN 'USUA_SUBST' .
                  lo_cols_list->set_short_text( 'Ap.Subst' ).
                  lo_cols_list->set_medium_text( 'Aprov.Substituto' ).
                  lo_cols_list->set_long_text(  'Aprov.Substituto'  ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '15' ).
                WHEN 'DATA_LIM' .
                  lo_cols_list->set_short_text( 'Data Final' ).
                  lo_cols_list->set_medium_text( 'Data Final'  ).
                  lo_cols_list->set_long_text( 'Data Final'  ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN OTHERS.
                  lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
              ENDCASE.
            ENDLOOP.
          CATCH cx_salv_not_found.

        ENDTRY.

        lo_cols->set_column_position( columnname = 'BUKRS'            position  =       01  ).
        lo_cols->set_column_position( columnname = 'CENTRO_DESP'      position  =       02  ).
        lo_cols->set_column_position( columnname = 'SUPL_ORC'         position  =       03  ).
        lo_cols->set_column_position( columnname = 'NIVEL'            position  =       04  ).
        lo_cols->set_column_position( columnname = 'APROVADOR'        position  =       05  ).
        lo_cols->set_column_position( columnname = 'PERMIT'           position  =       06  ).
        lo_cols->set_column_position( columnname = 'WAERS'            position  =       07  ).
        lo_cols->set_column_position( columnname = 'VALOR_DE'         position  =       08  ).
        lo_cols->set_column_position( columnname = 'VALOR_ATE'        position  =       09  ).
        lo_cols->set_column_position( columnname = 'USUA_SUBST'       position  =       10  ).
        lo_cols->set_column_position( columnname = 'DATA_LIM'         position  =       11  ).


      ENDMETHOD.

      METHOD set_edit_alv.

        DATA ls_api               TYPE REF TO if_salv_gui_om_extend_grid_api.
        DATA ls_edit              TYPE REF TO if_salv_gui_om_edit_restricted.

        ls_api = o_alv->extended_grid_api( ).
        ls_edit = ls_api->editable_restricted( ).

        ls_edit->set_t_celltab_columnname( t_celltab_columnname = 'CELLTAB' ).

        DATA(mo_listener) = NEW lcl_listener( ).
        ls_edit->set_listener( mo_listener ).
        ls_edit->validate_changed_data( ).
        co_alv->refresh( ).


      ENDMETHOD.

      METHOD get_data.


      ENDMETHOD.


      METHOD set_pf_status.

        DATA: lo_functions TYPE REF TO cl_salv_functions_list.
        lo_functions = co_alv->get_functions( ).
        lo_functions->set_all( abap_true ).
        lo_functions->set_default( abap_true ).

        TRY.

            lo_functions->add_function( name     = 'GRAVAR'
                                        icon     = '@2L@'
                                        text     = 'Gravar'
                                        tooltip  = 'Gravar em Tabela'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

          CATCH cx_root.

        ENDTRY.


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
          WHEN 'INSERT_ROW'.

            CLEAR: wa_saida.

            READ TABLE it_saida2 INTO wa_saida2 INDEX 1.

            wa_saida-bukrs = wa_saida2-bukrs.
            wa_saida-centro_desp =  wa_saida2-centro_desp.
            wa_saida-supl_orc =  'X'.

            APPEND wa_saida TO it_saida ASSIGNING FIELD-SYMBOL(<_put_celltab>).
            <_put_celltab>-celltab = lt_modify_celltab."lt_celltab. "aQUI VC EDITA UMA LINHA
            SORT it_saida ASCENDING.

            o_alv->refresh( ).

          WHEN 'GRAVAR'.
            PERFORM gravar.

        ENDCASE.

      ENDMETHOD.

      METHOD on_toolbar.

        CASE p_tipo.
          WHEN 'E'.
            LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
              "3 DESABILITA E 0 HABILITA
              IF <fs_tollbar>-function = '&LOCAL&INSERT_ROW'
                OR <fs_tollbar>-function = '&LOCAL&APPEND'
                OR <fs_tollbar>-function = '&LOCAL&INSERT_ROW'
                OR <fs_tollbar>-function = '&LOCAL&DELETE_ROW'
                OR <fs_tollbar>-function = '&LOCAL&COPY_ROW'.
                <fs_tollbar>-butn_type = '3'.
              ENDIF.
            ENDLOOP.
          WHEN 'I' or 'C'.
            LOOP AT e_object->mt_toolbar ASSIGNING <fs_tollbar>.
              "3 DESABILITA E 0 HABILITA
              IF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
                <fs_tollbar>-function = 'INSERT_ROW'.
              ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
                <fs_tollbar>-butn_type = '3'.
              ENDIF.
            ENDLOOP.
        ENDCASE.

*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'INSERT_ROW'.   "fcode
*    mt_toolbar-icon = '@B_INSR@'.
*    mt_toolbar-quickinfo = 'Inserir linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.

      ENDMETHOD.

      METHOD generate_output.

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


*        container_main = NEW cl_gui_custom_container(
*          parent         = cl_gui_container=>default_screen
*          "lifetime       =  cl_gui_container=>lifetime_dynpro
*          container_name = 'CONTAINER'
*        ).

        DATA: lx_msg TYPE REF TO cx_salv_msg.


        TRY.
            cl_salv_table=>factory(
              EXPORTING
*               r_container    = container_main
*               container_name = 'CONTAINER'
                r_container    = painel1
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

        CALL METHOD me->set_columns_build
          CHANGING
            co_alv = o_alv.

        DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
        DATA l_title              TYPE lvc_title.
        "l_title = |Nome Relatório|.
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

        CALL METHOD set_edit_alv "Este metodo precisa ser após a saida do display para que ele possa dar um refresh
          CHANGING
            co_alv = o_alv.


      ENDMETHOD.

      METHOD set_HANDLER.

        DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,
              lo_col_tab  TYPE REF TO cl_salv_column_table,
              lo_events   TYPE REF TO cl_salv_events_table.

        lo_cols_tab = co_alv->get_columns( ).
        lo_events = co_alv->get_event( ).

        SET HANDLER co_report->on_user_command FOR lo_events.
        SET HANDLER co_report->on_toolbar FOR ALL INSTANCES ACTIVATION 'X'.

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

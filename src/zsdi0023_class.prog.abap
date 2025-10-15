
    data: container_main type ref to cl_gui_custom_container,
          painel_control type ref to cl_gui_splitter_container,
          painel1        type ref to cl_gui_container,
          painel2        type ref to cl_gui_container,
          lo_cols        type ref to cl_salv_columns,
          lo_cols_ref    type        salv_t_column_ref,
          lo_cols_list   type ref to cl_salv_column_list,
          lo_col_list    like line of lo_cols_ref,
          lo_column      type ref to cl_salv_column,
          ls_ddic_f4_ref type salv_s_ddic_reference.
    class lcl_report definition deferred.
    data: lo_report type ref to lcl_report.
    class lcl_listener definition.
      public section.
        interfaces if_salv_gui_om_edit_strct_lstr.
    endclass.

    class lcl_listener implementation.

      method if_salv_gui_om_edit_strct_lstr~on_f4_request.
      endmethod.

      method if_salv_gui_om_edit_strct_lstr~on_check_changed_data.

        o_ui_data_modify->get_ui_changes( importing t_deleted_rows = data(lt_deleted_rows) ).
        o_ui_data_modify->get_ui_changes( importing t_good_cells = data(lt_good_cells) ).
        o_ui_data_modify->get_ui_changes( importing t_inserted_rows = data(lt_inserted_rows) ).
        o_ui_data_modify->get_ui_changes( importing t_modified_cells = data(lt_modified_cells) ).
        o_ui_data_modify->get_ui_changes( importing rt_modified_data_rows = data(lt_modified_data_rows) ).

        loop at lt_modified_cells assigning field-symbol(<_get_mod>).
          read table it_saida assigning field-symbol(<_it_saida_mod>) index <_get_mod>-row_id.

          case <_get_mod>-fieldname.
              "Exemplo
*            WHEN 'SUPL_ORC'.
*              o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                   fieldname  = 'SUPL_ORC'
*                                                   cell_value = 'X' ).
*            WHEN 'APROVADOR' OR 'USUA_SUBST'. "Trata usuarios
*              CONDENSE <_get_mod>-value NO-GAPS.
*
*              DATA(_campo) = <_get_mod>-fieldname.
*
*              IF <_get_mod>-value IS NOT INITIAL.
*
*                SELECT SINGLE * FROM usr21 WHERE bname = @<_get_mod>-value INTO @DATA(ls_USR21).
*
*                IF sy-subrc = 0.
*                  o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                       fieldname  = _campo
*                                                       cell_value = ls_USR21-bname ).
*                  IF _campo = 'USUA_SUBST'.
*                    DATA(_usua_subst) = ls_USR21-bname.
*                  ENDIF.
*                ELSE.
*                  DATA(msg_erro_usr21) = |Nome do { _campo } não foi encontrado!|.
*                  MESSAGE msg_erro_usr21 TYPE 'I' DISPLAY LIKE 'I'.
*                  o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                       fieldname  = _campo
*                                                       cell_value = '' ).
*                ENDIF.
*              ENDIF.
            when 'CH_MODELO'.
            when 'AUART'.
            when 'VKAUS'.
            when 'BRSCH'.
            when 'UF_CENTRO'.
            when 'UF_CLIENTE'.
            when 'CITYC'.
            when 'MWSK1'.
            when 'OWNPR'.
            when 'SHTYP'.
            when 'TDLNR'.
            when 'BUKRS_TOMA'.
            when 'BUKRS_EMIT'.
            when 'MTORG'.
            when 'MATNR'.
            when 'MATKL'.
            when 'EXTWG'.
            when 'STEUC'.
            when 'J_1BTXSDC'.
            when 'J_1BTAXLW1'.
            when 'J_1BTAXLW2'.
            when 'J_1BTAXLW4'.
            when 'J_1BTAXLW5'.
          endcase.
        endloop.



      endmethod.
    endclass.
    class lcl_report definition.
      public section .

        methods:
          get_data,
          generate_output,
          set_HANDLER,
          set_refresh,
          set_columns_build,
          set_pf_status,
          set_layout,
          set_edit_alv,
          on_toolbar
            for event toolbar of cl_gui_alv_grid
            importing
              e_object
              e_interactive
              sender,
          on_user_command for event added_function of cl_salv_events importing e_salv_function sender,
          on_link_click for event link_click of cl_salv_events_table importing row column.

    endclass.
    class lcl_report implementation.
      method set_refresh.
        lo_report->get_data( ).
        o_alv->refresh( ).
      endmethod.

      method set_columns_build .

        lo_cols = o_alv->get_columns( ).
        "lo_cols->set_optimize( abap_false ).

        lo_cols_ref    = lo_cols->get( ).


        try.

            loop at lo_cols_ref into lo_col_list.
              lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
              clear: ls_ddic_f4_ref.
              case lo_col_list-columnname.
                when 'AUART' .
                  lo_cols_list->set_short_text( 'TpDocVend' ).
                  lo_cols_list->set_medium_text( 'Tipo Doc. Vendas' ).
                  lo_cols_list->set_long_text( 'Tipo Doc. Vendas' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'VKAUS' .
                  lo_cols_list->set_short_text( 'Utilização' ).
                  lo_cols_list->set_medium_text( 'Utilização' ).
                  lo_cols_list->set_long_text( 'Utilização' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'BRSCH'.
                  lo_cols_list->set_short_text( 'Setor Ind.' ).
                  lo_cols_list->set_medium_text( 'Setor Industrial' ).
                  lo_cols_list->set_long_text( 'Setor Industrial' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'UF_CENTRO' .
                  lo_cols_list->set_short_text( 'UF Emissor' ).
                  lo_cols_list->set_medium_text( 'UF Emissor' ).
                  lo_cols_list->set_long_text( 'UF Emissor' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'UF_CLIENTE' .
                  lo_cols_list->set_short_text( 'UF Recept.' ).
                  lo_cols_list->set_medium_text( 'UF Receptor' ).
                  lo_cols_list->set_long_text( 'UF Receptor' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'CITYC' .
                  lo_cols_list->set_short_text( 'Cd. Cidade' ).
                  lo_cols_list->set_medium_text( 'Cód. Cidade' ).
                  lo_cols_list->set_long_text( 'Cód. Cidade' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'MWSK1' .
                  lo_cols_list->set_short_text( 'Tipo IVA' ).
                  lo_cols_list->set_medium_text( 'Tipo IVA' ).
                  lo_cols_list->set_long_text( 'Tipo IVA' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'OWNPR' .
                  lo_cols_list->set_short_text( 'Produção' ).
                  lo_cols_list->set_medium_text( 'Produção' ).
                  lo_cols_list->set_long_text( 'Produção' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'SHTYP' .
                  lo_cols_list->set_short_text( 'Tp. Transp' ).
                  lo_cols_list->set_medium_text( 'Tipo Transp.' ).
                  lo_cols_list->set_long_text( 'Tipo transporte' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'TDLNR' .
                  lo_cols_list->set_short_text( 'Ag. Frete' ).
                  lo_cols_list->set_medium_text( 'Agente de Frete' ).
                  lo_cols_list->set_long_text( 'Agente de Frete' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'BUKRS_TOMA' .
                  lo_cols_list->set_short_text( 'BUKRS_TOMA' ).
                  lo_cols_list->set_medium_text( 'Emp.Tomador' ).
                  lo_cols_list->set_long_text( 'Emp.Tomador' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'BUKRS_EMIT' .
                  lo_cols_list->set_short_text( 'BUKRS_EMIT' ).
                  lo_cols_list->set_medium_text( 'Emp.Emitente' ).
                  lo_cols_list->set_long_text( 'Emp.Emitente' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'MTORG' .
                  lo_cols_list->set_short_text( 'OrigMater.' ).
                  lo_cols_list->set_medium_text( 'Origem de Material' ).
                  lo_cols_list->set_long_text( 'Origem de Material' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'MATNR' .
                  lo_cols_list->set_short_text( 'CdMaterial' ).
                  lo_cols_list->set_medium_text( 'Cód. Material' ).
                  lo_cols_list->set_long_text( 'Cód. Material' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'MATKL' .
                  lo_cols_list->set_short_text( 'GpMercado' ).
                  lo_cols_list->set_medium_text( 'Grupo Mercadoria' ).
                  lo_cols_list->set_long_text( 'Grupo Mercadoria' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'EXTWG' .
                  lo_cols_list->set_short_text( 'GpMatExt.' ).
                  lo_cols_list->set_medium_text( 'Gp Material Ext.' ).
                  lo_cols_list->set_long_text( 'Gp Material Ext.' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'STEUC' .
                  lo_cols_list->set_short_text( 'NCM' ).
                  lo_cols_list->set_medium_text( 'NCM' ).
                  lo_cols_list->set_long_text( 'NCM' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'J_1BTXSDC' .
                  lo_cols_list->set_short_text( 'Cd Imp. SD' ).
                  lo_cols_list->set_medium_text( 'Cd Imp. SD' ).
                  lo_cols_list->set_long_text( 'Cd Imp. SD' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'J_1BTAXLW1 ' .
                  lo_cols_list->set_short_text( 'Txt ICMS' ).
                  lo_cols_list->set_medium_text( 'Txt ICMS' ).
                  lo_cols_list->set_long_text( 'Txt ICMS' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'J_1BTAXLW2 ' .
                  lo_cols_list->set_short_text( 'Txt IPI' ).
                  lo_cols_list->set_medium_text( 'Txt IPI' ).
                  lo_cols_list->set_long_text( 'Txt IPI' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'J_1BTAXLW4 ' .
                  lo_cols_list->set_short_text( 'LeiCOFINS' ).
                  lo_cols_list->set_medium_text( 'Lei trib. COFINS' ).
                  lo_cols_list->set_long_text( 'Lei tributária COFINS' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                when 'J_1BTAXLW5 ' .
                  lo_cols_list->set_short_text( 'LeitribPIS' ).
                  lo_cols_list->set_medium_text( 'Lei tributária PIS' ).
                  lo_cols_list->set_long_text( 'Lei tributária PIS' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).

                when others.
                  lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
              endcase.
            endloop.
          catch cx_salv_not_found.

        endtry.

        lo_cols->set_column_position( columnname = 'AUART' position = 01 ).
        lo_cols->set_column_position( columnname = 'BUKRS_EMIT' position = 02 ).
        lo_cols->set_column_position( columnname = 'MTORG' position = 03 ).
        lo_cols->set_column_position( columnname = 'STEUC' position = 04 ).
        lo_cols->set_column_position( columnname = 'MATNR' position = 05 ).
        lo_cols->set_column_position( columnname = 'MATKL' position = 06 ).
        lo_cols->set_column_position( columnname = 'EXTWG' position = 07 ).
        lo_cols->set_column_position( columnname = 'VKAUS' position = 08 ).
        lo_cols->set_column_position( columnname = 'BRSCH' position = 09 ).
        lo_cols->set_column_position( columnname = 'UF_CENTRO' position = 10 ).
        lo_cols->set_column_position( columnname = 'UF_CLIENTE' position = 11 ).
        lo_cols->set_column_position( columnname = 'CITYC' position = 12 ).
        lo_cols->set_column_position( columnname = 'MWSK1' position = 13 ).
        lo_cols->set_column_position( columnname = 'OWNPR' position = 14 ).
        lo_cols->set_column_position( columnname = 'SHTYP' position = 15 ).
        lo_cols->set_column_position( columnname = 'TDLNR' position = 16 ).
        lo_cols->set_column_position( columnname = 'BUKRS_TOMA' position = 17 ).
        lo_cols->set_column_position( columnname = 'J_1BTXSDC' position = 18 ).
        lo_cols->set_column_position( columnname = 'J_1BTAXLW2' position = 19 ).
        lo_cols->set_column_position( columnname = 'J_1BTAXLW1' position = 20 ).
        lo_cols->set_column_position( columnname = 'J_1BTAXLW4' position = 21 ).
        lo_cols->set_column_position( columnname = 'J_1BTAXLW5' position = 22 ).


      endmethod.

      method set_edit_alv.

*        DATA ls_api               TYPE REF TO if_salv_gui_om_extend_grid_api.
*        DATA ls_edit              TYPE REF TO if_salv_gui_om_edit_restricted.
*
*        ls_api = o_alv->extended_grid_api( ).
*        ls_edit = ls_api->editable_restricted( ).
*        TRY.
*
*            LOOP AT lo_cols_ref INTO lo_col_list.
*              lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
*              CLEAR: ls_ddic_f4_ref.
*
*              ls_edit->set_attributes_for_columnname( columnname              = lo_col_list-columnname
*                                                      all_cells_input_enabled = abap_true ).
*
*            ENDLOOP.
*
*          CATCH cx_salv_not_found.
*        ENDTRY.
*
*        ls_edit->validate_changed_data( ).
*
*
*        co_alv->refresh( ).


        data ls_api               type ref to if_salv_gui_om_extend_grid_api.
        data ls_edit              type ref to if_salv_gui_om_edit_restricted.

        ls_api = o_alv->extended_grid_api( ).
        ls_edit = ls_api->editable_restricted( ).

        ls_edit->set_t_celltab_columnname( t_celltab_columnname = 'CELLTAB' ).

        data(mo_listener) = new lcl_listener( ).
        ls_edit->set_listener( mo_listener ).
        ls_edit->validate_changed_data( ).
        o_alv->refresh( ).



      endmethod.

      method get_data.


      endmethod.


      method set_pf_status.

        data: lo_functions type ref to cl_salv_functions_list.
        lo_functions = o_alv->get_functions( ).
        lo_functions->set_all( abap_true ).
        lo_functions->set_default( abap_true ).

        try.

            lo_functions->add_function( name     = 'GRAVAR'
                                        icon     = '@2L@'
                                        text     = 'Gravar'
                                        tooltip  = 'Gravar em Tabela'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

          catch cx_root.

        endtry.


      endmethod.

      method on_user_command.

        data: lo_selections type ref to cl_salv_selections.
        data lt_rows type salv_t_row.
        data lt_columns type salv_t_column.
        data lt_cells type salv_t_cell.
        data qtd_rows type int4.

        free: lt_rows.
        clear: qtd_rows.

        lo_selections = o_alv->get_selections( ).
        lt_rows = lo_selections->get_selected_rows( ).
        qtd_rows = lines( lt_rows ).


*        CASE e_salv_function.
*
*          WHEN 'SAVE'. "GRAVAR
*            CLEAR:wa_saida.
*            IF it_saida IS NOT INITIAL.
*              LOOP AT it_saida INTO wa_saida.
*                MODIFY ZSDT0370 FROM wa_saida.
*                COMMIT WORK.
*              ENDLOOP.
*              FREE: it_saida.
*              CLEAR:wa_saida.
*              LEAVE PROGRAM.
*            ELSE.
*              MESSAGE 'Não Existem Dados a Serem Salvos!' TYPE 'I'.
*            ENDIF.
*
*          WHEN OTHERS.
*        ENDCASE.

*          WHEN 'DELETE_ROW'.
*
*            IF qtd_rows > 0.
*              LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
*                READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
*                "DELETE zmmt0185 FROM <_del>.
*              ENDLOOP.
*
*            ELSE.
*              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
*              EXIT.
*            ENDIF.
*
*            CALL METHOD set_refresh CHANGING co_alv = o_alv.
*
*          WHEN 'REFRESH_ROW'.
*            CALL METHOD set_refresh CHANGING co_alv = o_alv.
*          WHEN OTHERS.
*        ENDCASE.


        case e_salv_function.
          when 'INSERT_ROW'.

            clear: wa_saida.

*            READ TABLE it_saida2 INTO wa_saida2 INDEX 1.

*            wa_saida-bukrs = wa_saida2-bukrs.
*            wa_saida-centro_desp =  wa_saida2-centro_desp.
*            wa_saida-supl_orc =  'X'.

            append wa_saida to it_saida assigning field-symbol(<_put_celltab>).
            <_put_celltab>-celltab = lt_modify_celltab."lt_celltab. "aQUI VC EDITA UMA LINHA
            sort it_saida ascending.

            o_alv->refresh( ).

          when 'GRAVAR'.
            perform gravar.
        endcase.

      endmethod.

      method on_toolbar.

*        CASE p_tipo.
*          WHEN 'E'.
*            LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
*              "3 DESABILITA E 0 HABILITA
*              IF <fs_tollbar>-function = '&LOCAL&INSERT_ROW'
*                OR <fs_tollbar>-function = '&LOCAL&APPEND'
*                OR <fs_tollbar>-function = '&LOCAL&INSERT_ROW'
*                OR <fs_tollbar>-function = '&LOCAL&DELETE_ROW'
*                OR <fs_tollbar>-function = '&LOCAL&COPY_ROW'.
*                <fs_tollbar>-butn_type = '3'.
*              ENDIF.
*            ENDLOOP.
*          WHEN 'I' or 'C'.
        loop at e_object->mt_toolbar assigning field-symbol(<fs_tollbar>).
          "3 DESABILITA E 0 HABILITA
          if <fs_tollbar>-function eq '&LOCAL&INSERT_ROW'.
            <fs_tollbar>-function = 'INSERT_ROW'.
          elseif <fs_tollbar>-function eq '&LOCAL&APPEND'.
            <fs_tollbar>-butn_type = '3'.
          endif.
        endloop.
*        ENDCASE.

*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'INSERT_ROW'.   "fcode
*    mt_toolbar-icon = '@B_INSR@'.
*    mt_toolbar-quickinfo = 'Inserir linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.

      endmethod.

      method generate_output.

        create object container_main
          exporting
            container_name = 'CONTAINER'
            lifetime       = container_main->lifetime_dynpro.

* Cria Splitter Container
        create object painel_control
          exporting
            parent  = container_main
            rows    = 1
            columns = 1
            align   = 15.

* Exibe Painel 1
        call method painel_control->get_container
          exporting
            row       = 1
            column    = 1
          receiving
            container = painel1.


*        container_main = NEW cl_gui_custom_container(
*          parent         = cl_gui_container=>default_screen
*          "lifetime       =  cl_gui_container=>lifetime_dynpro
*          container_name = 'CONTAINER'
*        ).

        data: lx_msg type ref to cx_salv_msg.


        try.
            cl_salv_table=>factory(
              exporting
*               r_container    = container_main
*               container_name = 'CONTAINER'
                r_container    = painel1
                container_name = 'CONTAINER'
              importing
                r_salv_table   = o_alv
              changing
                t_table        = it_saida ).
          catch cx_salv_msg into lx_msg.
        endtry.

        call method set_pf_status.

        call method set_layout.

        call method set_HANDLER.

        call method me->set_columns_build.

        data lr_display_settings  type ref to cl_salv_display_settings.
        data l_title              type lvc_title.
        "l_title = |Nome Relatório|.
        lr_display_settings = o_alv->get_display_settings( ).
        lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
        lr_display_settings->set_list_header( l_title ).
        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
        lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).

        "Enable Zebra Layout
        lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
        data lr_selections        type ref to cl_salv_selections.
* Enable cell selection mode
        lr_selections = o_alv->get_selections( ).
        lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

        o_alv->display( ).

        call method set_edit_alv. "Este metodo precisa ser após a saida do display para que ele possa dar um refresh



      endmethod.

      method set_HANDLER.
*
*...HotSpot
        data: lo_cols_tab type ref to cl_salv_columns_table,
              lo_col_tab  type ref to cl_salv_column_table,
              lo_events   type ref to cl_salv_events_table.

        lo_cols_tab = o_alv->get_columns( ).
        lo_events = o_alv->get_event( ).

*   event handler
        set handler lo_report->on_link_click for lo_events.
        set handler lo_report->on_user_command for lo_events.
        set handler lo_report->on_toolbar for all instances activation 'X'.
      endmethod.

      method on_link_click.

*    DATA: wa_saida TYPE zpme0087.
*    CLEAR: wa_saida.
*
*    IF column = ''.
*      "READ TABLE lo_report->it_saida INTO wa_saida INDEX row.
*
*    ENDIF.

      endmethod.


      method set_layout.
*
        data: lo_layout  type ref to cl_salv_layout,
              lf_variant type slis_vari,
              ls_key     type salv_s_layout_key.
*   get layout object
        lo_layout = o_alv->get_layout( ).
*   set Layout save restriction
*   1. Set Layout Key .. Unique key identifies the Differenet ALVs
        ls_key-report = sy-repid.
        lo_layout->set_key( ls_key ).
*   2. Remove Save layout the restriction.
        lo_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).
*   set initial Layout
        lf_variant = 'DEFAULT'.
        lo_layout->set_initial_layout( lf_variant ).
      endmethod.

    endclass.

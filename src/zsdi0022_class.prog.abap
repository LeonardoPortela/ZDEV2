
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
    CLASS lcl_report DEFINITION.
      PUBLIC SECTION .

        DATA: o_alv     TYPE REF TO cl_salv_table,
              it_saida  TYPE STANDARD TABLE OF zsdt0370 INITIAL SIZE 0,
              it_saida2 TYPE STANDARD TABLE OF zsdt0370 INITIAL SIZE 0,
              wa_saida  TYPE zsdt0370,
              t_salv    TYPE STANDARD TABLE OF REF TO cl_salv_table.
        METHODS:
          get_data,
          generate_output,
          set_HANDLER
            CHANGING co_alV    TYPE REF TO cl_salv_table
                     co_report TYPE REF TO lcl_report,
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
    CLASS lcl_report IMPLEMENTATION.
      METHOD set_refresh.
        lo_report->get_data( ).
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

        lo_cols = o_alv->get_columns( ).
        "lo_cols->set_optimize( abap_false ).

        lo_cols_ref    = lo_cols->get( ).


        TRY.

            LOOP AT lo_cols_ref INTO lo_col_list.
              lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
              CLEAR: ls_ddic_f4_ref.
              CASE lo_col_list-columnname.
                WHEN 'AUART' .
                  lo_cols_list->set_short_text( 'TpDocVend' ).
                  lo_cols_list->set_medium_text( 'Tipo Doc. Vendas' ).
                  lo_cols_list->set_long_text( 'Tipo Doc. Vendas' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                  lo_cols_list->set_f4( if_salv_c_bool_sap=>false ).

                WHEN 'BUKRS_EMIT' .
                  lo_cols_list->set_short_text( 'BUKRS_EMIT' ).
                  lo_cols_list->set_medium_text( 'Emp.Emitente' ).
                  lo_cols_list->set_long_text( 'Emp.Emitente' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'MTORG' .
                  lo_cols_list->set_short_text( 'OrigMater.' ).
                  lo_cols_list->set_medium_text( 'Origem de Material' ).
                  lo_cols_list->set_long_text( 'Origem de Material' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'STEUC' .
                  lo_cols_list->set_short_text( 'NCM' ).
                  lo_cols_list->set_medium_text( 'NCM' ).
                  lo_cols_list->set_long_text( 'NCM' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'MATNR' .
                  lo_cols_list->set_short_text( 'CdMaterial' ).
                  lo_cols_list->set_medium_text( 'Cód. Material' ).
                  lo_cols_list->set_long_text( 'Cód. Material' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'MATKL' .
                  lo_cols_list->set_short_text( 'GpMercado' ).
                  lo_cols_list->set_medium_text( 'Grupo Mercadoria' ).
                  lo_cols_list->set_long_text( 'Grupo Mercadoria' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'EXTWG' .
                  lo_cols_list->set_short_text( 'GpMatExt.' ).
                  lo_cols_list->set_medium_text( 'Gp Material Ext.' ).
                  lo_cols_list->set_long_text( 'Gp Material Ext.' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'VKAUS' .
                  lo_cols_list->set_short_text( 'Utilização' ).
                  lo_cols_list->set_medium_text( 'Utilização' ).
                  lo_cols_list->set_long_text( 'Utilização' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'BRSCH'.
                  lo_cols_list->set_short_text( 'Setor Ind.' ).
                  lo_cols_list->set_medium_text( 'Setor Industrial' ).
                  lo_cols_list->set_long_text( 'Setor Industrial' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'UF_CENTRO' .
                  lo_cols_list->set_short_text( 'UF Emissor' ).
                  lo_cols_list->set_medium_text( 'UF Emissor' ).
                  lo_cols_list->set_long_text( 'UF Emissor' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'UF_CLIENTE' .
                  lo_cols_list->set_short_text( 'UF Recept.' ).
                  lo_cols_list->set_medium_text( 'UF Receptor' ).
                  lo_cols_list->set_long_text( 'UF Receptor' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'CITYC' .
                  lo_cols_list->set_short_text( 'Cd. Cidade' ).
                  lo_cols_list->set_medium_text( 'Cód. Cidade' ).
                  lo_cols_list->set_long_text( 'Cód. Cidade' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'MWSK1' .
                  lo_cols_list->set_short_text( 'Tipo IVA' ).
                  lo_cols_list->set_medium_text( 'Tipo IVA' ).
                  lo_cols_list->set_long_text( 'Tipo IVA' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'OWNPR' .
                  lo_cols_list->set_short_text( 'Produção' ).
                  lo_cols_list->set_medium_text( 'Produção' ).
                  lo_cols_list->set_long_text( 'Produção' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'SHTYP' .
                  lo_cols_list->set_short_text( 'Tp. Transp' ).
                  lo_cols_list->set_medium_text( 'Tipo Transp.' ).
                  lo_cols_list->set_long_text( 'Tipo transporte' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'TDLNR' .
                  lo_cols_list->set_short_text( 'Ag. Frete' ).
                  lo_cols_list->set_medium_text( 'Agente de Frete' ).
                  lo_cols_list->set_long_text( 'Agente de Frete' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'BUKRS_TOMA' .
                  lo_cols_list->set_short_text( 'BUKRS_TOMA' ).
                  lo_cols_list->set_medium_text( 'Emp.Tomador' ).
                  lo_cols_list->set_long_text( 'Emp.Tomador' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'J_1BTXSDC' .
                  lo_cols_list->set_short_text( 'Cd Imp. SD' ).
                  lo_cols_list->set_medium_text( 'Cd Imp. SD' ).
                  lo_cols_list->set_long_text( 'Cd Imp. SD' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'J_1BTAXLW2 ' .
                  lo_cols_list->set_short_text( 'Txt IPI' ).
                  lo_cols_list->set_medium_text( 'Txt IPI' ).
                  lo_cols_list->set_long_text( 'Txt IPI' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'J_1BTAXLW1 ' .
                  lo_cols_list->set_short_text( 'Txt ICMS' ).
                  lo_cols_list->set_medium_text( 'Txt ICMS' ).
                  lo_cols_list->set_long_text( 'Txt ICMS' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'J_1BTAXLW4 ' .
                  lo_cols_list->set_short_text( 'LeiCOFINS' ).
                  lo_cols_list->set_medium_text( 'Lei trib. COFINS' ).
                  lo_cols_list->set_long_text( 'Lei tributária COFINS' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'J_1BTAXLW5 ' .
                  lo_cols_list->set_short_text( 'LeitribPIS' ).
                  lo_cols_list->set_medium_text( 'Lei tributária PIS' ).
                  lo_cols_list->set_long_text( 'Lei tributária PIS' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN OTHERS.
                  lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
              ENDCASE.
            ENDLOOP.
          CATCH cx_salv_not_found.

        ENDTRY.

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


      ENDMETHOD.

      METHOD set_edit_alv.

        DATA ls_api               TYPE REF TO if_salv_gui_om_extend_grid_api.
        DATA ls_edit              TYPE REF TO if_salv_gui_om_edit_restricted.

        ls_api = o_alv->extended_grid_api( ).
        ls_edit = ls_api->editable_restricted( ).
        TRY.

            LOOP AT lo_cols_ref INTO lo_col_list.
              lo_cols_list ?= lo_col_list-r_column.    "Narrow casting
              CLEAR: ls_ddic_f4_ref.

              ls_edit->set_attributes_for_columnname( columnname              = lo_col_list-columnname
                                                      all_cells_input_enabled = abap_true ).

            ENDLOOP.

          CATCH cx_salv_not_found.
        ENDTRY.

        ls_edit->validate_changed_data( ).


        co_alv->refresh( ).



      ENDMETHOD.

      METHOD get_data.

        FREE: it_saida.

        SELECT *
        FROM
          zsdt0370
          INTO TABLE @it_saida.
        SORT  it_saida DESCENDING BY ch_modelo auart.

      ENDMETHOD.


      METHOD set_pf_status.

        DATA: lo_functions TYPE REF TO cl_salv_functions_list.
        lo_functions = co_alv->get_functions( ).
        lo_functions->set_all( abap_true ).
        lo_functions->set_default( abap_true ).

        TRY.

            lo_functions->add_function( name     = 'INSERT_NEW'
                                        icon     = '@17@'
                                        text     = 'Inserir'
                                        tooltip  = 'Inserir'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

            lo_functions->add_function( name     = 'DELETE_ROW'
                                        icon     = '@18@'
                                        text     = 'Deletar'
                                        tooltip  = 'Deletar'
                                        position = if_salv_c_function_position=>right_of_salv_functions ).

            lo_functions->add_function( name     = 'COPY_NEW'
                                        icon     = '@14@'
                                        text     = 'Copiar'
                                        tooltip  = 'Copiar'
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

          WHEN 'COPY_NEW'.
            FREE: it_saida2.
            IF qtd_rows >= 1.
              FREE: it_saida2.
              CLEAR: wa_saida.
              LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row_copy>).
                READ TABLE it_saida INTO wa_saida INDEX <row_copy>.
                APPEND wa_saida TO it_saida2.
                CLEAR: wa_saida.
              ENDLOOP.
              FREE: it_saida.

              "Exportando do programa ZSDI0022 para ZSDI0023.
              EXPORT it_saida2 = it_saida2 TO MEMORY ID 'ZSDI0023'.
              SUBMIT zsdi0023 AND RETURN .

              CALL METHOD set_refresh CHANGING co_alv = o_alv.
            ELSE.
              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
              EXIT.
            ENDIF.

          WHEN 'INSERT_NEW'.
            FREE MEMORY ID 'ZSDI0023'.
            FREE: it_saida2.
            SUBMIT zsdi0023 AND RETURN .
          WHEN 'DELETE_ROW'.

            IF qtd_rows > 0.
              DATA: lv_answer TYPE c.
              CALL FUNCTION 'POPUP_TO_CONFIRM'
                EXPORTING
                  titlebar              = 'Exclusão de seleção'
                  text_question         = 'Deseja excluir as linhas selecionadas?'
                  text_button_1         = 'Sim'
                  text_button_2         = 'Não'
                  default_button        = '2'
                  display_cancel_button = ' '
                IMPORTING
                  answer                = lv_answer
                EXCEPTIONS
                  text_not_found        = 1
                  OTHERS                = 2.

              IF lv_answer = '1'.
                LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row_del>).
                  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_del>) INDEX <row_del>.
                  DELETE zsdt0370 FROM <_del>.
                  COMMIT WORK.
                ENDLOOP.
              ELSE.

              ENDIF.

            ELSE.
              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
              EXIT.
            ENDIF.

            CALL METHOD set_refresh CHANGING co_alv = o_alv.

          WHEN 'REFRESH_NEW'.
            CALL METHOD set_refresh CHANGING co_alv = o_alv.
          WHEN OTHERS.
        ENDCASE.

      ENDMETHOD.

      METHOD on_toolbar.

*        DATA : mt_toolbar TYPE stb_button.
*
*        CLEAR mt_toolbar.
*        mt_toolbar-butn_type = '3'.   "separator
*        APPEND mt_toolbar TO e_object->mt_toolbar.
*
*        LOOP AT e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_tollbar>).
*          "3 DESABILITA E 0 HABILITA
*          IF  <fs_tollbar>-function EQ '&LOCAL&COPY_ROW'.
*            <fs_tollbar>-butn_type = '3'.
*          ELSEIF <fs_tollbar>-function EQ '&LOCAL&CREATE_ROW'.
*            "<fs_tollbar>-butn_type = '3'.
*          ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
*            <fs_tollbar>-butn_type = '3'.
*          ENDIF.
*          IF <fs_tollbar>-function EQ '&REFRESH'.
*            <fs_tollbar>-function = 'REFRESH_ROW'.
*          ELSEIF <fs_tollbar>-function EQ '&LOCAL&DELETE_ROW'.
*            <fs_tollbar>-function = 'DELETE_ROW'.
*          ENDIF.
*        ENDLOOP.
*
**    CLEAR mt_toolbar.
**    mt_toolbar-butn_type = '0'.   "normal Button
**    mt_toolbar-function = 'INSERT_ROW'.   "fcode
**    mt_toolbar-icon = '@B_INSR@'.
**    mt_toolbar-quickinfo = 'Inserir linha'.
**    APPEND mt_toolbar TO e_object->mt_toolbar.

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

*        if o_alv is not initial.
*          set handler on_f4 for o_alv.
*        endif.

        o_alv->display( ).

*        CALL METHOD set_edit_alv "Este metodo precisa ser após a saida do display para que ele possa dar um refresh
*          CHANGING
*            co_alv = o_alv.


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

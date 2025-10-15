
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
        DATA: o_alv  TYPE REF TO cl_salv_table,
              t_salv TYPE STANDARD TABLE OF REF TO cl_salv_table.
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

*      METHOD on_hotspot.
**        DATA: ls_data TYPE zmy_table.
**        READ TABLE gt_data INTO ls_data INDEX row.
**        IF sy-subrc = 0 AND column = 'ACTION'.  " Ensure it's the button column
**          MESSAGE 'Button Clicked!' TYPE 'I'.
**          " Perform action based on row data
**        ENDIF.
*      ENDMETHOD.

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
                  "lo_cols_list->set_specific_group( id = 'GRP' ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'SUPL_ORC'.
                  lo_cols_list->set_short_text( 'Supl/Orça' ).
                  lo_cols_list->set_medium_text( 'Supl./Orça.'  ).
                  lo_cols_list->set_long_text( 'Supl./Orça.'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                  lo_cols_list->set_output_length( '10' ).
                WHEN 'NIVEL' .
                  lo_cols_list->set_short_text( 'Nível' ).
                  lo_cols_list->set_medium_text( 'Nível' ).
                  lo_cols_list->set_long_text( 'Nível'   ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
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
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
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
                WHEN 'APROCC' .
                  lo_cols_list->set_short_text( 'Aprov/C.C' ).
                  lo_cols_list->set_medium_text( 'Aprov. Centro Custo'  ).
                  lo_cols_list->set_long_text( 'Aprov. Centro Custo'  ).
                  lo_cols_list->set_alignment( if_salv_c_alignment=>centered ).
                  lo_cols_list->set_cell_type( if_salv_c_cell_type=>hotspot ).
                  "lo_cols_list->set_specific_group( id = 'GRP' ).
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
        lo_cols->set_column_position( columnname = 'APROCC'           position  =       12  ).


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
          zpmr0002
                    WHERE bukrs IN @p_bukrs
          AND centro_desp IN @p_cdesp
          INTO TABLE @DATA(it_dados).

        IF it_dados IS NOT INITIAL.

          MOVE-CORRESPONDING it_dados TO it_saida.

          SORT it_saida BY bukrs ASCENDING centro_desp ASCENDING nivel ASCENDING.

          SELECT * FROM zpmr0011
          FOR ALL ENTRIES IN @it_saida
          WHERE bukrs = @it_saida-bukrs
          AND centro_desp = @it_saida-centro_desp
          INTO TABLE @DATA(it_zpmr0011).

          LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).

            READ TABLE it_zpmr0011 INTO DATA(wa_zpmr0011) WITH KEY bukrs = <fs_saida>-bukrs centro_desp = <fs_saida>-centro_desp.

            IF sy-subrc = 0.
              <fs_saida>-aprocc = icon_change.
            ELSE.
              <fs_saida>-aprocc = icon_positive.
            ENDIF.

          ENDLOOP.

          SORT it_saida BY bukrs ASCENDING centro_desp ASCENDING nivel ASCENDING.

        ENDIF.


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

            lo_functions->add_function( name     = 'EDIT_ROW'
                                        icon     = '@0Z@'
                                        text     = 'Editar'
                                        tooltip  = 'Editar'
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
          WHEN 'EDIT_ROW'.
            FREE: it_saida2.
            IF qtd_rows >= 1.
              FREE: it_saida2.
              CLEAR: wa_saida.
              LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row_edit>).
                READ TABLE it_saida INTO wa_saida INDEX <row_edit>.
                MOVE-CORRESPONDING wa_saida TO wa_saida2.
                APPEND wa_saida2 TO it_saida2.
                CLEAR: wa_saida,wa_saida2.
              ENDLOOP.
              FREE: it_saida.

              "Exportando do programa ZPMR0091 para ZPMR0092.

              LOOP AT it_saida2 ASSIGNING FIELD-SYMBOL(<fs_saida2>).
                <fs_saida2>-action = 'EDIT'.
              ENDLOOP.

              EXPORT it_saida2 = it_saida2 TO MEMORY ID 'ZPMR0092'.
              SUBMIT zpmr0092 AND RETURN .

              CALL METHOD set_refresh CHANGING co_alv = o_alv.
            ELSE.
              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
              EXIT.
            ENDIF.

          WHEN 'COPY_NEW'.
            FREE: it_saida2.
            IF qtd_rows >= 1.
              FREE: it_saida2.
              CLEAR: wa_saida.
              LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<row_copy>).
                READ TABLE it_saida INTO wa_saida INDEX <row_copy>.
                MOVE-CORRESPONDING wa_saida TO wa_saida2.
                APPEND wa_saida2 TO it_saida2.
                CLEAR: wa_saida,wa_saida2.
              ENDLOOP.
              FREE: it_saida.

              "Exportando do programa ZPMR0091 para ZPMR0092.
              LOOP AT it_saida2 ASSIGNING <fs_saida2>.
                <fs_saida2>-action = 'COPY'.
              ENDLOOP.
              EXPORT it_saida2 = it_saida2 TO MEMORY ID 'ZPMR0092'.
              SUBMIT zpmr0092 AND RETURN .

              CALL METHOD set_refresh CHANGING co_alv = o_alv.
            ELSE.
              MESSAGE 'Selecione ao menos uma linha!' TYPE 'I' DISPLAY LIKE 'I'.
              EXIT.
            ENDIF.

          WHEN 'INSERT_NEW'.
            FREE: it_saida2.

            DATA p_erro TYPE string.
            CLEAR:p_erro.
            IF qtd_rows = 0.
              CLEAR: p_bukrs,p_cdesp.
              IF p_bukrs IS INITIAL AND p_cdesp IS INITIAL .
                "CALL SCREEN 1000 STARTING AT 10 5 ENDING AT 50 15.

                DATA: lt_fields     TYPE TABLE OF sval,  " Table for popup fields
                      ls_fields     TYPE sval,           " Work area for field definition
                      lv_returncode TYPE c.          " Return code from popup

                ls_fields-tabname   = 'T001'.       " Table name
                ls_fields-fieldname = 'BUKRS'.      " Field name
                ls_fields-field_obl = 'X'.          " Mandatory field
                APPEND ls_fields TO lt_fields.

                " Define the second field: Plant (WERKS from MARC)
                ls_fields-tabname   = 'T001W'.
                ls_fields-fieldname = 'WERKS'.
                ls_fields-field_obl = 'X'.         " Optional field
                APPEND ls_fields TO lt_fields.

                " Call the popup function module
                CALL FUNCTION 'POPUP_GET_VALUES'
                  EXPORTING
                    popup_title     = 'Filtro'
                    start_column    = '5'              " Position of popup
                    start_row       = '5'
                  IMPORTING
                    returncode      = lv_returncode    " User action (OK or Cancel)
                  TABLES
                    fields          = lt_fields
                  EXCEPTIONS
                    error_in_fields = 1
                    OTHERS          = 2.

                " Check if the popup was successful
                IF sy-subrc = 0.
                  " Check user action
                  IF lv_returncode = 'A'.
                    MESSAGE 'É necessário informar uma empresa e centro para continuar!' TYPE 'I'.
                    EXIT.
                  ELSE.
                    " Retrieve and display entered values
                    READ TABLE lt_fields INTO ls_fields INDEX 1. " Material
                    SELECT SINGLE bukrs FROM t001 WHERE bukrs = @ls_fields-value INTO @p_bukrs-low.
                    IF sy-subrc = 0 .
                      READ TABLE lt_fields INTO ls_fields INDEX 2. " Plant
                      SELECT SINGLE werks FROM t001w WHERE vkorg = @p_bukrs-low AND werks = @ls_fields-value INTO @p_cdesp-low.
                      IF sy-subrc <> 0.
                        MESSAGE 'O centro informado não tem relação com a empresa informada!' TYPE 'I' DISPLAY LIKE 'I'.
                        EXIT.
                      ENDIF.
                    ELSE.
                      MESSAGE 'Verifique o valor da empresa informada!' TYPE 'I' DISPLAY LIKE 'I'.
                      EXIT.
                    ENDIF.


                  ENDIF.

                ELSEIF ( p_bukrs IS NOT INITIAL AND p_cdesp IS NOT INITIAL ) OR ( p_bukrs IS INITIAL AND p_cdesp IS NOT INITIAL ).
                  CLEAR:p_erro.
                ELSE.
                  p_erro = |Selecione ao menos uma linha!|.
                ENDIF.
              ENDIF.
            ELSEIF qtd_rows = 1.
              CLEAR:wa_saida.
              READ TABLE lt_rows ASSIGNING FIELD-SYMBOL(<row_insert>) INDEX 1.
              READ TABLE it_saida INTO wa_saida INDEX <row_insert>.
              p_bukrs-low = wa_saida-bukrs.
              p_cdesp-low = wa_saida-centro_desp.
            ELSE.
              p_erro = |Selecione ao menos uma linha!|.
            ENDIF.


            IF p_erro IS INITIAL.
              FREE: it_saida2.
              FREE MEMORY ID 'ZPMR0092'.

              "Exportando do programa ZPMR0091 para ZPMR0092.
              CLEAR: wa_saida2.

              wa_saida2-bukrs = p_bukrs-low.
              wa_saida2-centro_desp = p_cdesp-low.
              wa_saida2-action = 'INSERT'.
              APPEND wa_saida2 TO it_saida2.
              CLEAR: wa_saida2.

              EXPORT it_saida2 = it_saida2 TO MEMORY ID 'ZPMR0092'.
              SUBMIT zpmr0092 AND RETURN .
              FREE: it_saida2.
              FREE MEMORY ID 'ZPMR0094'.


              CALL METHOD set_refresh CHANGING co_alv = o_alv.
            ELSE.
              MESSAGE p_erro TYPE 'I' DISPLAY LIKE 'I'.
              EXIT.
            ENDIF.

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
                  DELETE zpmr0002 FROM <_del>.
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

        DATA: lo_cols_sorts TYPE REF TO cl_salv_sorts.

        lo_cols_sorts = o_alv->get_sorts( ).
        lo_cols_sorts->add_sort( columnname = 'BUKRS' subtotal = abap_true ).
        lo_cols_sorts->add_sort( columnname = 'CENTRO_DESP' subtotal = abap_true ).


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
        SET HANDLER co_report->on_link_click FOR lo_events. "(hotspot)
        SET HANDLER co_report->on_user_command FOR lo_events.
        SET HANDLER co_report->on_toolbar FOR ALL INSTANCES ACTIVATION 'X'.
        SET HANDLER co_report->on_change_data FOR ALL INSTANCES ACTIVATION 'X'.
      ENDMETHOD.

      METHOD on_link_click."(hotspot)
        DATA: lt_seltab TYPE TABLE OF rsparams,
              ls_seltab LIKE LINE OF lt_seltab.
        CLEAR: wa_saida.
        IF column = 'APROCC'.
          READ TABLE it_saida INTO wa_saida INDEX row.
          IF sy-subrc = 0.
            FREE: lt_seltab.

            CLEAR: ls_seltab.
            ls_seltab-selname = 'P_BUKRS'.
            ls_seltab-kind    = 'S'.
            ls_seltab-sign    = 'I'.
            ls_seltab-option  = 'EQ'.
            ls_seltab-low     = wa_saida-bukrs.
            ls_seltab-high    = wa_saida-bukrs.
            APPEND ls_seltab TO lt_seltab.

            CLEAR: ls_seltab.
            ls_seltab-selname = 'P_CDESP'.
            ls_seltab-kind    = 'S'.
            ls_seltab-sign    = 'I'.
            ls_seltab-option  = 'EQ'.
            ls_seltab-low     = wa_saida-centro_desp.
            ls_seltab-high    = wa_saida-centro_desp.
            APPEND ls_seltab TO lt_seltab.

            CLEAR: ls_seltab.

            SUBMIT zpmr0093
            WITH SELECTION-TABLE lt_seltab
            AND RETURN.
            FREE: lt_seltab.

*            CASE wa_saida-aprocc.
*              WHEN icon_change.
*                SUBMIT zpmr0093
*                WITH SELECTION-TABLE lt_seltab
*                AND RETURN.
*                FREE: lt_seltab.
*              WHEN icon_positive.
*                FREE: it_export.
*                FREE MEMORY ID 'ZPMR0094'.
*                "Exportando do programa ZPMR0091 para ZPMR0094.
*                CLEAR: wa_export.
*                wa_export-bukrs = wa_saida-bukrs.
*                wa_export-centro_desp = wa_saida-centro_desp.
*                APPEND wa_export TO it_export.
*                CLEAR: wa_export.
*                EXPORT it_import = it_export TO MEMORY ID 'ZPMR0094'.
*                SUBMIT zpmr0094 AND RETURN .
*                FREE: it_export.
*                FREE MEMORY ID 'ZPMR0094'.
*              WHEN OTHERS.
*            ENDCASE.
            CALL METHOD set_refresh CHANGING co_alv = o_alv.
          ENDIF.
        ENDIF.
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

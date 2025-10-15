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

*    LOOP AT lt_modified_cells ASSIGNING FIELD-SYMBOL(<_get_mod>).
*      READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_it_saida_mod>) INDEX <_get_mod>-row_id.
*
*      CASE <_get_mod>-fieldname.
*        WHEN 'KOSTL'.
*          DATA: _kostl TYPE kostl.
*          CONDENSE <_get_mod>-value NO-GAPS.
*
*          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*            EXPORTING
*              input  = <_get_mod>-value
*            IMPORTING
*              output = _kostl.
*
*          SELECT SINGLE * FROM csks
*          WHERE kokrs = 'MAGI'
*          AND datbi >= @sy-datum
*          AND bukrs = @<_it_saida_mod>-bukrs
*          AND gsber = @<_it_saida_mod>-centro_desp
*          AND kostl = @_kostl
*          INTO @DATA(ls_csks).
*
*          IF sy-subrc <> 0.
*            MESSAGE 'Centro de Custo não é válido para esta empresa e filial!' TYPE 'I' DISPLAY LIKE 'I'.
*            o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                 fieldname  = 'KOSTL'
*                                                 cell_value = '' ).
*            o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                 fieldname  = 'KTEXT'
*                                                 cell_value = '' ).
*          ELSE.
*            o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                 fieldname  = 'KOSTL'
*                                                 cell_value = ls_csks-kostl ).
*
*            SELECT SINGLE * FROM cskt WHERE kostl = @ls_csks-kostl AND spras = 'P' INTO @DATA(ls_cskt).
*            o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                 fieldname  = 'KTEXT'
*                                                 cell_value = ls_cskt-ktext ).
*          ENDIF.
*
*        WHEN 'APROVADOR' OR 'USUA_SUBST'. "Trata usuarios
*          CONDENSE <_get_mod>-value NO-GAPS.
*
*          DATA(_campo) = <_get_mod>-fieldname.
*
*          IF <_get_mod>-value IS NOT INITIAL.
*
*            SELECT SINGLE * FROM usr21 WHERE bname = @<_get_mod>-value INTO @DATA(ls_USR21).
*
*            IF sy-subrc = 0.
*              o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                   fieldname  = _campo
*                                                   cell_value = ls_USR21-bname ).
*              IF _campo = 'USUA_SUBST'.
*                DATA(_usua_subst) = ls_USR21-bname.
*              ENDIF.
*            ELSE.
*              DATA(msg_erro_usr21) = |Nome do { _campo } não foi encontrado!|.
*              MESSAGE msg_erro_usr21 TYPE 'I' DISPLAY LIKE 'I'.
*              o_ui_data_modify->modify_cell_value( row_id     = <_get_mod>-row_id
*                                                   fieldname  = _campo
*                                                   cell_value = '' ).
*            ENDIF.
*          ENDIF.
*      ENDCASE.
*    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      generate_output,
      set_refresh,
      set_copy,
      set_columns_build,
      set_pf_status,
      set_layout,
      set_edit_alv,
      on_toolbar
        FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive,
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function.
ENDCLASS.
CLASS lcl_report IMPLEMENTATION.

  METHOD set_copy.
    o_alv->refresh( ).
  ENDMETHOD.
  METHOD set_refresh.
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
            WHEN 'DOCNUM' .
              lo_cols_list->set_short_text( 'Nº doc' ).
              lo_cols_list->set_medium_text( 'Nº doc'  ).
              lo_cols_list->set_long_text( 'Nº doc'  ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '10' ).
            WHEN 'ITMNUM' .
              lo_cols_list->set_short_text( 'NºMNum' ).
              lo_cols_list->set_medium_text( 'NºMNum' ).
              lo_cols_list->set_long_text( 'NºMNum' ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '06' ).
            WHEN 'ITDIDOC'.
              lo_cols_list->set_short_text( 'Nº IDoc' ).
              lo_cols_list->set_medium_text( 'Nº IDoc'  ).
              lo_cols_list->set_long_text( 'Nº IDoc'    ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '06' ).
            WHEN 'NR_ADICAO'.
              lo_cols_list->set_short_text( 'NAdic' ).
              lo_cols_list->set_medium_text( 'NAdic'  ).
              lo_cols_list->set_long_text( 'NAdic'    ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '5' ).
            WHEN 'NR_SEQ_ADICAO' .
              lo_cols_list->set_short_text( 'Seq.' ).
              lo_cols_list->set_medium_text( 'Seq.' ).
              lo_cols_list->set_long_text( 'Seq.'   ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '5' ).
            WHEN 'CFABRICANTE' .
              lo_cols_list->set_short_text( 'Fabricante' ).
              lo_cols_list->set_medium_text( 'Fabricante' ).
              lo_cols_list->set_long_text( 'Fabricante'  ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '20' ).
            WHEN 'VLR_DESCONTO' .
              lo_cols_list->set_short_text( 'Vlr Liq.' ).
              lo_cols_list->set_medium_text( 'Vlr Liq.'  ).
              lo_cols_list->set_long_text( 'Vlr Liq.'   ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '15' ).
            WHEN 'NR_PED_COMPRA' .
              lo_cols_list->set_short_text( 'NPedComp' ).
              lo_cols_list->set_medium_text( 'NPedComp'  ).
              lo_cols_list->set_long_text( 'NPedComp'   ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '15' ).
            WHEN 'NR_PED_COMPRA_IT' .
              lo_cols_list->set_short_text( 'IPedComp' ).
              lo_cols_list->set_medium_text( 'Item Ped.Comp.' ).
              lo_cols_list->set_long_text( 'Item Ped.Comp.'   ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '06' ).
            WHEN 'NR_DRAWBACK' .
              lo_cols_list->set_short_text( 'NDrawback' ).
              lo_cols_list->set_medium_text( 'Nº Drawback' ).
              lo_cols_list->set_long_text( 'Nº Drawback'   ).
              lo_cols_list->set_alignment( if_salv_c_alignment=>left ).
              lo_cols_list->set_output_length( '20' ).
            WHEN OTHERS.
              lo_cols_list->set_visible( if_salv_c_bool_sap=>false ).
          ENDCASE.
        ENDLOOP.
      CATCH cx_salv_not_found.

    ENDTRY.

    lo_cols->set_column_position( columnname =  'DOCNUM'  position  =         01  ).
    lo_cols->set_column_position( columnname =  'ITMNUM'  position  =         02  ).
    lo_cols->set_column_position( columnname =  'ITDIDOC' position  =         03  ).
    lo_cols->set_column_position( columnname =  'NR_ADICAO' position  =         04  ).
    lo_cols->set_column_position( columnname =  'NR_SEQ_ADICAO' position  =         05  ).
    lo_cols->set_column_position( columnname =  'CFABRICANTE' position  =         06  ).
    lo_cols->set_column_position( columnname =  'VLR_DESCONTO'  position  =         07  ).
    lo_cols->set_column_position( columnname =  'NR_PED_COMPRA' position  =         08  ).
    lo_cols->set_column_position( columnname =  'NR_PED_COMPRA_IT'  position  =         09  ).
    lo_cols->set_column_position( columnname =  'NR_DRAWBACK' position  =         10  ).


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
    o_alv->refresh( ).


  ENDMETHOD.


  METHOD set_pf_status.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.
    lo_functions = o_alv->get_functions( ).
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

        "READ TABLE it_saida INTO wa_saida2 INDEX 1.

        APPEND wa_saida TO it_saida ASSIGNING FIELD-SYMBOL(<_put_celltab>).
        <_put_celltab>-celltab = lt_modify_celltab."lt_celltab. "aQUI VC EDITA UMA LINHA
        SORT it_saida ASCENDING.

        o_alv->refresh( ).

      WHEN 'GRAVAR'.
        PERFORM gravar.
    ENDCASE.

  ENDMETHOD.

  METHOD on_toolbar.

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

*      IF <fs_tollbar>-function EQ '&LOCAL&INSERT_ROW'.
*        <fs_tollbar>-function = 'INSERT_ROW'.
*      ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
*        <fs_tollbar>-butn_type = '3'.
*      ENDIF.


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

** Cria Splitter Container
*        CREATE OBJECT painel_control
*          EXPORTING
*            parent  = container_main
*            rows    = 1
*            columns = 1
*            align   = 15.
*
** Exibe Painel 1
*        CALL METHOD painel_control->get_container
*          EXPORTING
*            row       = 1
*            column    = 1
*          RECEIVING
*            container = painel1.


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



    CALL METHOD set_pf_status.

    CALL METHOD set_layout.

    CALL METHOD set_columns_build.

    DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
    DATA l_title              TYPE lvc_title.
    "l_title = |Nome Relatório|.
    lr_display_settings = o_alv->get_display_settings( ).
    lr_display_settings->set_list_header_size( '10' ). "0=l, 1=s, 2=m
    lr_display_settings->set_list_header( l_title ).
    lr_display_settings->set_striped_pattern( cl_salv_display_settings=>true ).
    lr_display_settings->set_fit_column_to_table_size( cl_salv_display_settings=>true ).

    "Enable Zebra Layout
    lr_display_settings->set_striped_pattern( cl_salv_display_settings=>false ).
    DATA lr_selections        TYPE REF TO cl_salv_selections.
* Enable cell selection mode
    lr_selections = o_alv->get_selections( ).
    lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).


    lo_events = o_alv->get_event( ).

*   event handler
    SET HANDLER lo_report->on_user_command FOR ALL INSTANCES ACTIVATION 'X'."FOR lo_events.
    SET HANDLER lo_report->on_toolbar FOR ALL INSTANCES ACTIVATION 'X'.

    o_alv->display( ).

    CALL METHOD set_edit_alv. "Este metodo precisa ser após a saida do display para que ele possa dar um refresh

  ENDMETHOD.

  METHOD set_layout.
*
    DATA: lo_layout  TYPE REF TO cl_salv_layout,
          lf_variant TYPE slis_vari,
          ls_key     TYPE salv_s_layout_key.
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
  ENDMETHOD.

ENDCLASS.

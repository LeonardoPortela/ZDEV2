*&---------------------------------------------------------------------*
*& Include          ZMMR204_PARAMS_CLASS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include          ZPMR0087_CLA
*&---------------------------------------------------------------------*

*----------------------------------------------------------------*
* CLASS DEFINITION                                                     *
*----------------------------------------------------------------------*
* Eventhandler

CLASS lcl_report DEFINITION DEFERRED.
DATA: lo_report TYPE REF TO lcl_report.
CLASS lcl_report DEFINITION.
  PUBLIC SECTION.
    DATA: o_alv    TYPE REF TO cl_salv_table,
          it_saida TYPE STANDARD TABLE OF zmmt0187 INITIAL SIZE 0,
          wa_saida TYPE zmmt0187.
    METHODS:
      check_permission,
      set_refresh
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      get_data,
      generate_output,
      set_col_modify
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      set_columns
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      set_pf_status
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      set_layout
        CHANGING
          co_alv TYPE REF TO cl_salv_table,
      on_user_command FOR EVENT added_function OF cl_salv_events IMPORTING e_salv_function sender,
      set_HANDLER CHANGING co_alV   TYPE REF TO cl_salv_table co_report TYPE REF TO lcl_report,
      set_edit_alv CHANGING co_alV   TYPE REF TO cl_salv_table,
      on_link_click FOR EVENT link_click OF cl_salv_events_table IMPORTING row column,
      on_change_data FOR EVENT data_changed OF cl_gui_alv_grid IMPORTING e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender,
      "on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid IMPORTING e_modified et_good_cells.
      "on_after_refresh FOR EVENT after_refresh OF cl_gui_alv_grid IMPORTING sender,
      on_toolbar      FOR EVENT toolbar      OF cl_gui_alv_grid
        IMPORTING
          e_object
          e_interactive
          sender.

*    CLASS-METHODS get_row
*      CHANGING
*        co_alv        TYPE REF TO cl_salv_table
*      RETURNING
*        VALUE(e_rows) TYPE salv_t_row.


ENDCLASS.


CLASS lcl_report IMPLEMENTATION.

  METHOD set_refresh.
    lo_report->get_data( ).
    co_alv->refresh( ).
  ENDMETHOD.

  METHOD on_change_data.

*    DATA: it_changed TYPE STANDARD TABLE OF zmmt0185 INITIAL SIZE 0.
*    CLEAR: it_changed,wa_saida.
*
*    DATA(inserted_tab) = er_data_changed->mt_inserted_rows.
*    DATA(deleted_tab)  = er_data_changed->mt_deleted_rows.
*
*    FIELD-SYMBOLS: <itab>        TYPE ANY TABLE,
*                   <struct>      TYPE any,
*                   <it_mod_rows> TYPE ANY TABLE,
*                   <wa_mod_rows> TYPE any.
*
*
*    DATA: lt_good_cells TYPE lvc_t_modi,
*          ls_good_cell  TYPE lvc_s_modi.
*
*
*    ASSIGN er_data_changed->mp_mod_rows->* TO <it_mod_rows>.
*    MOVE-CORRESPONDING <it_mod_rows> TO it_changed.
*
*    lt_good_cells = er_data_changed->mt_good_cells.
*
*
*
**    BREAK-POINT.
**
**    LOOP AT it_changed INTO wa_saida.
**
**      SELECT SINGLE * FROM p_businessplace_vh WHERE bukrs = @wa_saida-bukrs AND branch = @wa_saida-branch INTO @DATA(lr_validacao).
**
**      IF sy-subrc <> 0.
**        MESSAGE 'Filial Não pertence a empresa!' TYPE 'I'.
***        LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<_clear>).
***          IF <_clear>-fieldname = 'BRANCH'.
***          ENDIF.
***        ENDLOOP.
**      ENDIF.
**
**    ENDLOOP.

  ENDMETHOD.

  METHOD set_col_modify.
*
*...Get all the Columns
    DATA: lo_cols TYPE REF TO cl_salv_columns.
    lo_cols = co_alv->get_columns( ).

*
*...Process individual columns
    DATA: lo_column TYPE REF TO cl_salv_column.
    DATA: lo_column_tab TYPE REF TO cl_salv_column_table.

    TRY.

        "***ESCONDE
        lo_column ?= lo_cols->get_column( 'MANDT' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column ?= lo_cols->get_column( 'ZDT_ATUAL' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column ?= lo_cols->get_column( 'ZHR_ATUAL' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column ?= lo_cols->get_column( 'USNAM' ).
        lo_column->set_visible( if_salv_c_bool_sap=>false ).

        "***MOSTRA
        lo_column ?= lo_cols->get_column( 'EMP_ID' ).
        lo_column->set_short_text( 'Empresa' ).
        lo_column->set_medium_text( 'Empresa' ).
        lo_column->set_long_text( 'Empresa' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '30' ).


      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.


*  METHOD get_row.
*    "DATA: lo_selections TYPE REF TO cl_salv_selections.
*    "  DATA lt_rows TYPE salv_t_row.
*    " DATA lt_columns TYPE salv_t_column.
**        DATA lt_cells TYPE salv_t_cell.
**        DATA qtd_rows TYPE int4.
**
*    e_rows = co_alv->get_selections( )->get_selected_rows( ).
**        lt_columns = co_alv->get_selections( )->get_selected_columns( ).
**        lt_cells = co_alv->get_selections( )->get_selected_cells( ).
**        qtd_rows = lines( lt_rows ).
*
** chamada no programa
**    DATA lt_rows TYPE salv_t_row.
**    DATA qtd_row TYPE int4.
**
**    lt_rows = lcl_report=>get_row( CHANGING co_alv =  o_alv ).
**    qtd_row = lines( lt_rows ).
*
*  ENDMETHOD.

  METHOD set_edit_alv.
    DATA ls_api               TYPE REF TO if_salv_gui_om_extend_grid_api.
    DATA ls_edit              TYPE REF TO if_salv_gui_om_edit_restricted.

    ls_api = co_alv->extended_grid_api( ).
    ls_edit = ls_api->editable_restricted( ).

    TRY.

*        ls_edit->set_attributes_for_columnname( columnname              = 'BUKRS'
*                                                all_cells_input_enabled = abap_true ).
*        ls_edit->set_attributes_for_columnname( columnname              = 'BRANCH'
*                                                all_cells_input_enabled = abap_true ).
        ls_edit->set_attributes_for_columnname( columnname              = 'EMP_ID'
                                                all_cells_input_enabled = abap_true ).
      CATCH cx_salv_not_found.
    ENDTRY.
    ls_edit->validate_changed_data( ).
    co_alv->refresh( ).
  ENDMETHOD.

*  METHOD on_after_refresh.
*    DATA: lo_grid TYPE REF TO cl_gui_alv_grid.
*    DATA: ls_layout TYPE lvc_s_layo.
*    DATA: lo_salv TYPE REF TO cl_salv_table.
*
*    BREAK-POINT.
*
**    TRY .
**        LOOP AT t_salv INTO lo_salv.
**          BREAK-POINT.
**          "lo_grid = zcl_test_np_salv_model=>get_grid( lo_salv ).
**          CHECK lo_grid EQ sender.
**
**          "deregister the event handler
**          SET HANDLER me->on_after_refresh
**            FOR ALL INSTANCES
**            ACTIVATION space.
**
**          "Set editable
**          ls_layout-edit = 'X'.
**          lo_grid->set_frontend_layout( ls_layout ).
**          lo_grid->set_ready_for_input( 1 ).
**        ENDLOOP.
**      CATCH cx_salv_error.
**    ENDTRY.
*  ENDMETHOD.

  METHOD get_data.

    FREE: it_saida.
    SELECT * FROM zmmt0187 INTO TABLE @it_saida.

  ENDMETHOD.
  METHOD generate_output.

    DATA(container) =
      NEW cl_gui_custom_container(
      parent         = cl_gui_container=>default_screen
      container_name = 'CONTAINER_01' ).


    DATA: lx_msg TYPE REF TO cx_salv_msg.
    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = container
            container_name = 'CONTAINER_01'
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

    CALL METHOD set_columns
      CHANGING
        co_alv = o_alv.

    CALL METHOD set_col_modify
      CHANGING
        co_alv = o_alv.



    DATA lr_display_settings  TYPE REF TO cl_salv_display_settings.
    DATA l_title              TYPE lvc_title.
    l_title = |Cadastro Empresas Linkana|.
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
*
*...HotSpot
    DATA: lo_cols_tab TYPE REF TO cl_salv_columns_table,
          lo_col_tab  TYPE REF TO cl_salv_column_table.
*
*   get Columns object
    lo_cols_tab = co_alv->get_columns( ).
    "Ativa Hotspot
*    TRY.
*        lo_col_tab ?= lo_cols_tab->get_column( 'LIQ_STATUS' ).
*      CATCH cx_salv_not_found.
*    ENDTRY.
**
**   Set the HotSpot for VBELN Column
*    TRY.
*        CALL METHOD lo_col_tab->set_cell_type
*          EXPORTING
*            value = if_salv_c_cell_type=>hotspot.
*        .
*      CATCH cx_salv_data_error .
*    ENDTRY.
*
*...Events
    DATA: lo_events TYPE REF TO cl_salv_events_table.
*
*   all events
    lo_events = o_alv->get_event( ).
*
*   event handler
    SET HANDLER co_report->on_link_click FOR lo_events.
    SET HANDLER co_report->on_user_command FOR lo_events.
    SET HANDLER co_report->on_toolbar FOR ALL INSTANCES ACTIVATION 'X'.
    SET HANDLER co_report->on_change_data FOR ALL INSTANCES ACTIVATION 'X'.
    "SET HANDLER co_report->on_after_refresh FOR ALL INSTANCES ACTIVATION 'X'.
  ENDMETHOD.

  METHOD on_link_click.

    DATA: wa_saida TYPE zpme0087.
    CLEAR: wa_saida.

    IF column = ''.
      "READ TABLE lo_report->it_saida INTO wa_saida INDEX row.
    ENDIF.

  ENDMETHOD.



  METHOD set_pf_status.

    DATA: lo_functions TYPE REF TO cl_salv_functions_list.
    lo_functions = co_alv->get_functions( ).
    lo_functions->set_all( abap_false ).
    "lo_functions->set_default( abap_true ).

    TRY.
        lo_functions->add_function( name     = 'GRAVAR'
                                    icon     = '@2L@'
                                    text     = 'Gravar'
                                    tooltip  = 'Gravar em Tabela'
                                    position = if_salv_c_function_position=>right_of_salv_functions ).


      CATCH cx_root.

    ENDTRY.


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

  METHOD set_columns.
*
*...Get all the Columns
    DATA: lo_cols TYPE REF TO cl_salv_columns.
    lo_cols = co_alv->get_columns( ).
*
*   set the Column optimization
    "lo_cols->set_optimize( 'X' ).
*
*...Process individual columns
    DATA: lo_column TYPE REF TO cl_salv_column.
    DATA: lo_column_tab TYPE REF TO cl_salv_column_table.

*    lo_cols->set_column_position( columnname = 'BUKRS'         position   = 01 ).
*    lo_cols->set_column_position( columnname = 'BRANCH'        position   = 02 ).
    lo_cols->set_column_position( columnname = 'EMP_ID'           position   = 01 ).


*      " F4 DDIC

*    DATA lv_ddic TYPE salv_s_ddic_reference.
*
*    TRY.

*        lo_column_tab ?= lo_cols->get_column( columnname = 'BUKRS' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFDOC'  field = 'BUKRS').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).
*
*        lo_column_tab ?= lo_cols->get_column( columnname = 'BRANCH' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFDOC'  field = 'BRANCH').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).

*        lo_column_tab ?= lo_cols->get_column( columnname = 'NBM' ).
*        lv_ddic = VALUE #( table  = 'J_1BNFLIN'  field = 'NBM').
*        lo_column_tab->set_ddic_reference( EXPORTING value = lv_ddic ).
*        lo_column_tab->set_f4( if_salv_c_bool_sap=>true ).
*
*      CATCH cx_root.                                    "#EC NO_HANDLER
*    ENDTRY.


  ENDMETHOD.                    "SET_COLUMNS

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
        DATA: lt_tab     TYPE esp1_message_tab_type,
              ls_tab     TYPE esp1_message_wa_type,
              lv_pattern TYPE string VALUE '^(?:\d{4}/){1,19}\d{4}$',
              lv_matched TYPE string.
        CLEAR: lt_tab,ls_tab.

        ls_tab-msgid  = 'E4'.
        ls_tab-msgno  = '000'.

        IF it_saida IS NOT INITIAL.
          LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_read>).

            SELECT SINGLE * FROM zmmt0187 WHERE emp_id = @<_read>-emp_id INTO @DATA(lr_zmmt0187).
            IF sy-subrc <> 0.
              lv_matched = <_read>-emp_id.
              REPLACE ALL OCCURRENCES OF REGEX lv_pattern IN lv_matched WITH ''.
              IF lv_matched IS INITIAL AND <_read>-emp_id IS NOT INITIAL.
                MODIFY zmmt0187 FROM <_read>.
                IF sy-subrc = 0.
                  ls_tab-msgty  = 'S'.
                  ls_tab-msgv1  = |Linha: { sy-tabix } - Empresa { <_read>-emp_id } Gravada com sucesso!|.
                  ls_tab-lineno = 3.
                  APPEND ls_tab TO lt_tab.
                ELSE.
                  ls_tab-msgty  = 'E'.
                  ls_tab-msgv1  = |Linha: { sy-tabix } - Falha ao Gravar a Empresa { <_read>-emp_id } !|.
                  ls_tab-lineno = 1.
                  APPEND ls_tab TO lt_tab.
                ENDIF.
              ELSE.
                ls_tab-msgty  = 'E'.
                ls_tab-msgv1  = |Linha: { sy-tabix } - ERRO: O padrão precisa ser ????/????...: { <_read>-emp_id } !|.
                ls_tab-lineno = 1.
                APPEND ls_tab TO lt_tab.
              ENDIF.
            ENDIF.
          ENDLOOP.

        ELSE.
          MESSAGE 'Gravado com sucesso, sem novos dados.' TYPE 'S'.
        ENDIF.

        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = lt_tab.

        CALL METHOD set_refresh CHANGING co_alv = o_alv.

      WHEN 'DELETE_ROW'.

        IF qtd_rows > 0.
          LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
            READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
            DELETE zmmt0187 FROM <_del>.
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
      IF <fs_tollbar>-function EQ '&CHECK'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&CUT'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&COPY'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&PASTE'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&UNDO'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&APPEND'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&LOCAL&COPY_ROW'.
        <fs_tollbar>-butn_type = '3'.
      ELSEIF <fs_tollbar>-function EQ '&MB_VARIANT'.
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
*
  ENDMETHOD.

  METHOD check_permission.

    DATA : lt_return TYPE TABLE OF bapiret2,
           lt_parameter TYPE TABLE OF bapiparam.



      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
        EXPORTING
          username = sy-uname
        TABLES
          return   = lt_return
          parameter = lt_parameter.

      IF sy-subrc = 0.
        READ TABLE lt_parameter INTO DATA(ls_parameter) WITH KEY parid = 'ZLINKANA_ACESSO'.
          IF sy-subrc = 0.
            IF ls_parameter-parva = 'X'.
            ELSE.
              MESSAGE 'Acesso não preenchido corretamente (X), abrir SA.' TYPE 'E'.
            ENDIF.
          ELSE.
            MESSAGE 'Não possui acesso a transação, abrir SA.' TYPE 'E'.
          ENDIF.
      ENDIF.

   ENDMETHOD.


ENDCLASS.

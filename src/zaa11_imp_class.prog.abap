CLASS lcl_report DEFINITION DEFERRED.
DATA: lo_report TYPE REF TO lcl_report.
CLASS lcl_report DEFINITION.
  PUBLIC SECTION .
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
*
*...Get all the Columns
    DATA: lo_cols   TYPE REF TO cl_salv_columns,
          lo_column TYPE REF TO cl_salv_column.

    lo_cols = co_alv->get_columns( ).
    "lo_cols->set_optimize( abap_false ).

    TRY.

*            lo_column = lo_cols->get_column( 'BASE_DIFAL_SAP' ).
*            lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column ?= lo_cols->get_column( 'ANLN1' ).
        lo_column->set_short_text( 'NºImobil.' ).
        lo_column->set_medium_text( 'Nº Imobilizado' ).
        lo_column->set_long_text( 'Nº Imobilizado' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '10' ).

        lo_column ?= lo_cols->get_column( 'ANLN2' ).
        lo_column->set_short_text( 'Sub Nº' ).
        lo_column->set_medium_text( 'Sub Nº' ).
        lo_column->set_long_text( 'Sub Nº' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '10' ).

        lo_column ?= lo_cols->get_column( 'ZIMOB_V' ).
        lo_column->set_short_text( 'Status' ).
        lo_column->set_medium_text( 'Status' ).
        lo_column->set_long_text( 'Status' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '6' ).

        lo_column ?= lo_cols->get_column( 'ZIMOB_P' ).
        lo_column->set_short_text( 'Plaqueta' ).
        lo_column->set_medium_text( 'Plaqueta' ).
        lo_column->set_long_text( 'Plaqueta' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '8' ).

        lo_column ?= lo_cols->get_column( 'OBSERV' ).
        lo_column->set_short_text( 'OBS.' ).
        lo_column->set_medium_text( 'Observação' ).
        lo_column->set_long_text( 'Observação' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '50' ).

        lo_column ?= lo_cols->get_column( 'IVENTARIADO' ).
        lo_column->set_short_text( 'Iventari.' ).
        lo_column->set_medium_text( 'Iventariado' ).
        lo_column->set_long_text( 'Iventariado' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '10' ).

        lo_column ?= lo_cols->get_column( 'IMG' ).
        lo_column->set_short_text( 'Imagem' ).
        lo_column->set_medium_text( 'Imagem' ).
        lo_column->set_long_text( 'Imagem' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '200' ).

        lo_column ?= lo_cols->get_column( 'INVENTARIANTE' ).
        lo_column->set_short_text( 'Inventar.' ).
        lo_column->set_medium_text( 'Inventariante' ).
        lo_column->set_long_text( 'Inventariante' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '50' ).


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

    SORT it_saida BY zimob_v ASCENDING.

    DELETE it_saida WHERE zimob_v = 4.

    SELECT kokrs, kostl, datbi, datab, bkzkp, pkzkp, bukrs, bkzks, pkzks
      FROM csks
      INTO TABLE @DATA(lt_csks)
      WHERE bukrs = @lv_bukrs
      AND   datbi = '99991231'
      AND  bkzkp NE 'X'
      AND  pkzkp NE 'X'
      AND  bkzks NE 'X'
      AND  pkzks NE 'X'.

    IF sy-subrc = 0.
      SORT lt_csks BY kostl.
    ENDIF.

    LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_ajusta01>).

      IF <_ajusta01>-anln1 IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <_ajusta01>-anln1
          IMPORTING
            output = <_ajusta01>-anln1.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <_ajusta01>-anln2
          IMPORTING
            output = <_ajusta01>-anln2.

        "Verifica se o centro de custo está ativo
        SELECT *
          FROM zaa005
          FOR ALL ENTRIES IN @lt_csks
          WHERE anln1 = @<_ajusta01>-anln1
          AND   anln2 = @<_ajusta01>-anln2
          AND   bukrs = @lt_csks-bukrs
          AND   gjahr = @sy-datum+0(4)
          AND   kostl = @lt_csks-kostl
          INTO TABLE @DATA(lt_val_centro).

          IF SY-SUBRC = 0.
            READ TABLE lt_val_centro ASSIGNING FIELD-SYMBOL(<lfs_val_centro>) INDEX 1.
            IF sy-subrc = 0.
              DATA(lv_centro) = <lfs_val_centro>-kostl.
              else.
                lv_centro = ' '.
            ENDIF.
          ENDIF.

          "Se existir é que já ouve um iventário = 1
          SELECT SINGLE COUNT( * ) AS qtd
            FROM zaa005
            WHERE anln1 = @<_ajusta01>-anln1
            AND   anln2 = @<_ajusta01>-anln2
            AND   bukrs = @lv_bukrs
            AND   gjahr = @sy-datum+0(4)
            and   kostl = @lv_centro
            INTO @<_ajusta01>-iventariado.
          "Senão existir é novo = 0
          IF sy-subrc <> 0.
            <_ajusta01>-iventariado = 0.
          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDMETHOD.


    METHOD set_pf_status.

      DATA: lo_functions TYPE REF TO cl_salv_functions_list.
      lo_functions = co_alv->get_functions( ).
      lo_functions->set_all( abap_true ).
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

        WHEN 'GRAVAR'.
          DATA: _ano TYPE gjahr.
          _ano = sy-datum+0(4).
          LOOP AT it_saida ASSIGNING FIELD-SYMBOL(<_saida>).

            CLEAR: wa_zaa005.
            FREE: it_zaa005.

            IF <_saida>-iventariado = 0 .
              SELECT * FROM zaa005 WHERE anln1 = @<_saida>-anln1 AND anln2 = @<_saida>-anln2  INTO TABLE @it_zaa005.
                SORT it_zaa005 BY gjahr DESCENDING.
                DELETE ADJACENT DUPLICATES FROM it_zaa005 COMPARING anln1.
                LOOP AT it_zaa005 INTO wa_zaa005.
                  wa_zaa005 = _ano.
                ENDLOOP.
              ELSE.
                SELECT SINGLE * FROM zaa005 WHERE anln1 = @<_saida>-anln1 AND anln2 = @<_saida>-anln2 AND gjahr = @_ano INTO @wa_zaa005.
                ENDIF.

                MOVE-CORRESPONDING <_saida> TO wa_zaa005.


                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                  EXPORTING
                    input  = wa_zaa005-anln2
                  IMPORTING
                    output = wa_zaa005-anln2.


                MODIFY zaa005 FROM wa_zaa005.

                COMMIT WORK.

              ENDLOOP.

              FREE: it_saida.

              LEAVE TO SCREEN 1000.


            WHEN 'DELETE_ROW'.

              IF qtd_rows > 0.
                LOOP AT lt_rows ASSIGNING FIELD-SYMBOL(<_index>).
                  READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<_del>) INDEX <_index>.
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

          DATA: container_main TYPE REF TO cl_gui_custom_container,
                painel_control TYPE REF TO cl_gui_splitter_container,
                painel1        TYPE REF TO cl_gui_container,
                painel2        TYPE REF TO cl_gui_container.

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
          l_title = |Nome Relatório|.
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


CLASS lcl_import DEFINITION DEFERRED.
DATA: lo_import TYPE REF TO lcl_import.
CLASS lcl_import DEFINITION.
PUBLIC SECTION .
  CLASS-METHODS:
    model IMPORTING i_file TYPE string EXPORTING e_tzaa005 TYPE STANDARD TABLE ,
    get_file.

ENDCLASS.

CLASS lcl_import IMPLEMENTATION.
METHOD get_file.
  DATA: lt_file   TYPE filetable,
        lv_action TYPE i,
        lv_rc     TYPE i,
        nm_file   TYPE string.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      file_filter             = |xlsx (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_excel }|
    CHANGING
      file_table              = lt_file
      rc                      = lv_rc
      user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5
  ).
  IF lv_action = cl_gui_frontend_services=>action_ok.

    IF lv_rc = 1.
      READ TABLE lt_file INDEX 1 INTO nm_file.
      IF sy-subrc = 0.
        model( i_file =  nm_file ).
      ENDIF.
    ENDIF.
  ENDIF.
ENDMETHOD.


METHOD model.

  TYPES: BEGIN OF ty_EXCEL,
           a TYPE string, "Imobilizado  -> A
           b TYPE string, "sub numero 2 -> B
           c TYPE string, "Status -> C
           d TYPE string, "Plaqueta -> D
           e TYPE string, "Observação -> E
           f TYPE string, "Usuario -> F
           g TYPE string, "Foto -> G
         END OF ty_EXCEL.

  TYPES: BEGIN OF ty_import,
           anln1             TYPE anln1, "Imobilizado  -> A
           anln2             TYPE anln2, "Imobilizado 2 -> B
           zimob_v           TYPE zimob_v, "Status -> C
           zimob_p           TYPE zimob_p, "Placa de Ativo -> D
           observ            TYPE badi_char255, "Observação -> E/F
           img               TYPE zaa005-img, "Observação -> G
           inventariante(50) TYPE c,
         END OF ty_import.

  DATA: lt_itab   TYPE solix_tab,
        ls_EXCEL  TYPE ty_EXCEL,
        lt_excel  TYPE STANDARD TABLE OF ty_EXCEL INITIAL SIZE 0,
        ls_import TYPE ty_import,
        lt_import TYPE STANDARD TABLE OF ty_import INITIAL SIZE 0.
  "Get Data.
  cl_gui_frontend_services=>gui_upload( EXPORTING  filename = i_file    " Name of file
                                                   filetype = 'BIN'    " File Type (ASCII, Binary)
                                        CHANGING   data_tab = lt_itab    " Transfer table for file contents
                                        EXCEPTIONS OTHERS   = 1
                                                  ).
  IF sy-subrc <> 0.
    RETURN.

  ELSE.

    FIELD-SYMBOLS:
<lfs_tab>    TYPE STANDARD TABLE.
* <lfs_row> type ANY.

    "Get excel reference.
    TRY .
        DATA(lo_excel_ref) = NEW cl_fdt_xl_spreadsheet( document_name = i_file
                                                        xdocument     = cl_bcs_convert=>solix_to_xstring( lt_itab ) ).

      CATCH cx_fdt_excel_core.
        RETURN.
    ENDTRY .
    "GEt work sheets.
    lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names( IMPORTING worksheet_names = DATA(worksheet_names) ).
    "Get first work sheet (You can iterate over multiple if there are more than one work sheets.
    DATA(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( worksheet_names[ 1 ] ).

    ASSIGN lo_data_ref->* TO  <lfs_tab>.

    MOVE-CORRESPONDING <lfs_tab> TO lt_excel.

    LOOP AT lt_excel ASSIGNING FIELD-SYMBOL(<_move_tab>) FROM 2.

      CONDENSE <_move_tab>-a NO-GAPS.
      CONDENSE <_move_tab>-b NO-GAPS.
      CONDENSE <_move_tab>-c NO-GAPS.
      CONDENSE <_move_tab>-d NO-GAPS.

      ls_import-anln1 = <_move_tab>-a.
      ls_import-anln2 = <_move_tab>-b.
      ls_import-zimob_v = <_move_tab>-c.
      ls_import-zimob_p = <_move_tab>-d.
      ls_import-observ = <_move_tab>-e.
      ls_import-inventariante = <_move_tab>-f.
      ls_import-img = <_move_tab>-g.

      APPEND ls_import TO lt_import.
      CLEAR:
      ls_import.
    ENDLOOP.

  ENDIF.

  MOVE-CORRESPONDING lt_import TO it_SAIDA.

ENDMETHOD.
ENDCLASS.

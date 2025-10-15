class lcl_report definition deferred.
data: lo_report type ref to lcl_report.
class lcl_report definition.
  public section .
    methods:
      modify_data,
      generate_output,
      set_HANDLER
        changing co_alV    type ref to cl_salv_table
                 co_report type ref to lcl_report,
      set_refresh
        changing
          co_alv type ref to cl_salv_table,
      set_columns_build
        changing
          co_alv type ref to cl_salv_table,
      set_columns_position
        changing
          co_alv type ref to cl_salv_table,
      set_f4
        changing
          co_alv type ref to cl_salv_table,
      set_pf_status
        changing
          co_alv type ref to cl_salv_table,
      set_layout
        changing
          co_alv type ref to cl_salv_table,
      set_edit_alv
        changing
          co_alV type ref to cl_salv_table,
      on_toolbar
        for event toolbar of cl_gui_alv_grid
        importing
          e_object
          e_interactive
          sender.

    methods: on_user_command for event added_function of cl_salv_events importing e_salv_function sender,
      on_link_click for event link_click of cl_salv_events_table importing row column,
      on_change_data for event data_changed of cl_gui_alv_grid importing e_onf4 e_onf4_after e_onf4_before e_ucomm er_data_changed sender.

endclass.
class lcl_report implementation.

  method set_refresh.
    co_alv->refresh( ).
  endmethod.

  method on_change_data.

    data: it_changed type standard table of zfise46 initial size 0.
    "CLEAR: it_changed,wa_saida.

    data(inserted_tab) = er_data_changed->mt_inserted_rows.
    data(deleted_tab)  = er_data_changed->mt_deleted_rows.

    field-symbols: <itab>        type any table,
                   <struct>      type any,
                   <it_mod_rows> type any table,
                   <wa_mod_rows> type any.


    data: lt_good_cells type lvc_t_modi,
          ls_good_cell  type lvc_s_modi.


    assign er_data_changed->mp_mod_rows->* to <it_mod_rows>.
    move-corresponding <it_mod_rows> to it_changed.

    lt_good_cells = er_data_changed->mt_good_cells.

  endmethod.

  method set_columns_build .
*
*...Get all the Columns
    data: lo_cols   type ref to cl_salv_columns,
          lo_column type ref to cl_salv_column.

    lo_cols = co_alv->get_columns( ).
    "lo_cols->set_optimize( abap_false ).

    try.

*            lo_column = lo_cols->get_column( 'BASE_DIFAL_SAP' ).
*            lo_column->set_visible( if_salv_c_bool_sap=>false ).

        lo_column ?= lo_cols->get_column( 'MATNR' ).
        lo_column->set_short_text( 'Nº Mater.' ).
        lo_column->set_medium_text( 'Nº Material' ).
        lo_column->set_long_text( 'Nº Material' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '18' ).

        lo_column ?= lo_cols->get_column( 'ZEPI_CA' ).
        lo_column->set_short_text( 'Nº CA EPI' ).
        lo_column->set_medium_text( 'Nº CA EPI' ).
        lo_column->set_long_text( 'Nº CA EPI' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '10' ).

        lo_column ?= lo_cols->get_column( 'ZEPI_PERI' ).
        lo_column->set_short_text( 'P.TrocaEpi' ).
        lo_column->set_medium_text( 'Per./Troca EPI' ).
        lo_column->set_long_text( 'Período Troca EPI' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '10' ).

        lo_column ?= lo_cols->get_column( 'ZEPI_VALCA' ).
        lo_column->set_short_text( 'Valid.CA' ).
        lo_column->set_medium_text( 'Validade CA' ).
        lo_column->set_long_text( 'Validade CA' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '10' ).

        lo_column ?= lo_cols->get_column( 'STATUS' ).
        lo_column->set_short_text( 'Status' ).
        lo_column->set_medium_text( 'Status' ).
        lo_column->set_long_text( 'Status' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '10' ).

        lo_column ?= lo_cols->get_column( 'MENSSAGEM' ).
        lo_column->set_short_text( 'Menssagem' ).
        lo_column->set_medium_text( 'Menssagem' ).
        lo_column->set_long_text( 'Menssagem' ).
        lo_column->set_optimized( abap_false ).
        "lo_column->set_alignment( if_salv_c_alignment=>left ).
        lo_column->set_output_length( '50' ).


      catch cx_salv_not_found.
    endtry.

  endmethod.

  method set_edit_alv.
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
  endmethod.

  method modify_data.

*    SORT it_saida BY matnr ASCENDING.

    data: _erro           type  char01,
          _msg            type  char255,
          wa_ZTD_OPNS_018 type ztd_opns_018,
          it_ZTD_OPNS_018 type table of ztd_opns_018.

    loop at it_saida assigning field-symbol(<_saida>).
      move-corresponding <_saida> to wa_ZTD_OPNS_018.
*      case sy-sysid.
*        when 'DEV'.
**          wa_ZTD_OPNS_018-sid        = 'DEV'.
**          wa_ZTD_OPNS_018-instance   = 'vhacxqasci_DEV_00'.
*        when 'QAS'.
**          wa_ZTD_OPNS_018-sid        = 'QAS'.
**          wa_ZTD_OPNS_018-instance   = 'vhacxqasci_QAS_00'.
*        when 'PRD'.
**          wa_ZTD_OPNS_018-sid        = 'PRD'.
**          wa_ZTD_OPNS_018-instance   = 'vhacxqasci_PRD_00'.
*        when others.
*      endcase.
      if wa_ZTD_OPNS_018 is not initial.
        append wa_ZTD_OPNS_018 to it_ztd_opns_018.
      endif.
      clear: wa_ZTD_OPNS_018.
    endloop.

    if it_ztd_opns_018 is not initial.
      modify ztd_opns_018 from table it_ztd_opns_018.
      commit work and wait.
      if sy-subrc eq 0.
        message s024(sd) with 'Dados salvo com sucesso'.
      else.
        message s024(sd) with 'Erro ao salvar dados'.
      endif.
    endif.

    call method set_refresh changing co_alv = o_alv.

  endmethod.


  method set_pf_status.

    data: lo_functions type ref to cl_salv_functions_list.
    lo_functions = co_alv->get_functions( ).
    lo_functions->set_all( abap_true ).
    "lo_functions->set_default( abap_true ).

    try.
        lo_functions->add_function( name     = 'PROCESSAR'
                                    icon     = '@2L@'
                                    text     = 'Processar'
                                    tooltip  = 'Gravar em Tabela'
                                    position = if_salv_c_function_position=>right_of_salv_functions ).


      catch cx_root.

    endtry.


  endmethod.

  method set_columns_position.

    data: lo_cols type ref to cl_salv_columns.


*        lo_cols->set_column_position( columnname = 'CHAVE_NFE_SAP'      position  =  1   ).
*        lo_cols->set_column_position( columnname = 'EMPRESA'            position  = 2   ).
*        lo_cols->set_column_position( columnname = 'FILIAL'             position  = 3   ).

  endmethod.


  method set_f4.
*
    data: lo_cols type ref to cl_salv_columns.
    lo_cols = co_alv->get_columns( ).

    data: lo_column type ref to cl_salv_column.
    data: lo_column_tab type ref to cl_salv_column_table.

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


    case e_salv_function.
      when 'PROCESSAR'.

        lo_report->modify_data( ).

      when 'DELETE_ROW'.

        if qtd_rows > 0.
          loop at lt_rows assigning field-symbol(<_index>).
            read table it_saida assigning field-symbol(<_del>) index <_index>.
            "DELETE zmmt0185 FROM <_del>.
          endloop.

        else.
          message 'Selecione ao menos uma linha!' type 'I' display like 'I'.
          exit.
        endif.

        call method set_refresh changing co_alv = o_alv.

      when 'REFRESH_ROW'.
        call method set_refresh changing co_alv = o_alv.
      when others.
    endcase.

  endmethod.

  method on_toolbar.

    data : mt_toolbar type stb_button.

    clear mt_toolbar.
    mt_toolbar-butn_type = '3'.   "separator
    append mt_toolbar to e_object->mt_toolbar.

    loop at e_object->mt_toolbar assigning field-symbol(<fs_tollbar>).
      "3 DESABILITA E 0 HABILITA
      if  <fs_tollbar>-function eq '&LOCAL&COPY_ROW'.
        <fs_tollbar>-butn_type = '3'.
      elseif <fs_tollbar>-function eq '&LOCAL&CREATE_ROW'.
        "<fs_tollbar>-butn_type = '3'.
      elseif <fs_tollbar>-function eq '&LOCAL&APPEND'.
        <fs_tollbar>-butn_type = '3'.
      endif.
      if <fs_tollbar>-function eq '&REFRESH'.
        <fs_tollbar>-function = 'REFRESH_ROW'.
      elseif <fs_tollbar>-function eq '&LOCAL&DELETE_ROW'.
        <fs_tollbar>-function = 'DELETE_ROW'.
      endif.
    endloop.

*    CLEAR mt_toolbar.
*    mt_toolbar-butn_type = '0'.   "normal Button
*    mt_toolbar-function = 'INSERT_ROW'.   "fcode
*    mt_toolbar-icon = '@B_INSR@'.
*    mt_toolbar-quickinfo = 'Inserir linha'.
*    APPEND mt_toolbar TO e_object->mt_toolbar.

  endmethod.

  method generate_output.

    data: container_main type ref to cl_gui_custom_container,
          painel_control type ref to cl_gui_splitter_container,
          painel1        type ref to cl_gui_container,
          painel2        type ref to cl_gui_container.

    container_main = new cl_gui_custom_container(
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

    data: lx_msg type ref to cx_salv_msg.


    try.
        cl_salv_table=>factory(
          exporting
            r_container    = container_main
            container_name = 'CONTAINER'
          importing
            r_salv_table   = o_alv
          changing
            t_table        = it_saida ).
      catch cx_salv_msg into lx_msg.
    endtry.

    call method set_pf_status
      changing
        co_alv = o_alv.

    call method set_layout
      changing
        co_alv = o_alv.

    call method set_HANDLER
      changing
        co_alv    = o_alv
        co_report = lo_report.

*    CALL METHOD set_f4
*      CHANGING
*        co_alv = o_alv.

    call method set_columns_position
      changing
        co_alv = o_alv.

    call method me->set_columns_build
      changing
        co_alv = o_alv.

    data lr_display_settings  type ref to cl_salv_display_settings.
    data l_title              type lvc_title.
    l_title = |Nome Relatório|.
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

*    CALL METHOD set_edit_alv "Este metodo precisa ser após a saida do display para que ele possa dar um refresh
*      CHANGING
*        co_alv = o_alv.


  endmethod.

  method set_HANDLER.
*
*...HotSpot
    data: lo_cols_tab type ref to cl_salv_columns_table,
          lo_col_tab  type ref to cl_salv_column_table,
          lo_events   type ref to cl_salv_events_table.

    lo_cols_tab = co_alv->get_columns( ).
    lo_events = co_alv->get_event( ).

*   event handler
    set handler co_report->on_link_click for lo_events.
    set handler co_report->on_user_command for lo_events.
    set handler co_report->on_toolbar for all instances activation 'X'.
    set handler co_report->on_change_data for all instances activation 'X'.
    "SET HANDLER co_report->on_after_refresh FOR ALL INSTANCES ACTIVATION 'X'.
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
  endmethod.

endclass.


class lcl_import definition deferred.
data: lo_import type ref to lcl_import.
class lcl_import definition.
  public section .
    class-methods:
      model importing i_file type string exporting e_tzaa005 type standard table ,
      get_file.

endclass.

class lcl_import implementation.
  method get_file.
    data: lt_file   type filetable,
          lv_action type i,
          lv_rc     type i,
          nm_file   type string.

    call method cl_gui_frontend_services=>file_open_dialog(
      exporting
        file_filter             = |xlsx (*.xlsx)\|*.xlsx\|{ cl_gui_frontend_services=>filetype_excel }|
      changing
        file_table              = lt_file
        rc                      = lv_rc
        user_action             = lv_action
      exceptions
        file_open_dialog_failed = 1
        cntl_error              = 2
        error_no_gui            = 3
        not_supported_by_gui    = 4
        others                  = 5
    ).
    if lv_action = cl_gui_frontend_services=>action_ok.

      if lv_rc = 1.
        read table lt_file index 1 into nm_file.
        if sy-subrc = 0.
          model( i_file = nm_file ).
        endif.
      endif.
    endif.

  endmethod.


  method model.

    types: begin of ty_EXCEL,
             a type string, "Código do LOG
             b type string, "nome do sistema SAP
             c type string, "Instância do servidor de aplicação
             d type string, "Data do sistema
             e type string, "Hora do sistema
             f type string, "Mandante
             g type string, "Evento - SM20
             h type string, "SysLog: nome de um usuário SAP
             i type string, "AuditSeg: nome de um terminal
             j type string, "Código de transação
             k type string, "Nome do programa
             l type string, "Cód.transação
             m type string, "Mensagem
           end of ty_EXCEL.

    types: begin of ty_import,
             zcod_log   type ze_opns_cod_log, "A
             sid        type syst_sysid, "B
             instance   type msname2, "C
             data       type sydatum, "D
             hora       type syuzeit, "E
             slgmand    type mandt, "F
             zareasubid type ze_opns_areasubid, "G
             slguser    type rslguser, "H
             slgltrm2   type  rsauterm, "I
             slgtc      type  tcode, "J
             slgrepna   type  program_id, "L
             sal_data   type  char255, "M
             menssagem  type atwrt30, "N
           end of ty_import.

    data: lt_itab   type solix_tab,
          ls_EXCEL  type ty_EXCEL,
          lt_excel  type standard table of ty_EXCEL initial size 0,
          ls_import type ty_import,
          lt_import type standard table of ty_import initial size 0.
    "Get Data.
    cl_gui_frontend_services=>gui_upload( exporting  filename = i_file    " Name of file
                                                     filetype = 'BIN'    " File Type (ASCII, Binary)
                                          changing   data_tab = lt_itab    " Transfer table for file contents
                                          exceptions others   = 1
                                                    ).
    if sy-subrc <> 0.
      return.

    else.

      field-symbols:
 <lfs_tab>    type standard table.
* <lfs_row> type ANY.

      "Get excel reference.
      try .
          data(lo_excel_ref) = new cl_fdt_xl_spreadsheet( document_name = i_file
                                                          xdocument     = cl_bcs_convert=>solix_to_xstring( lt_itab ) ).

        catch cx_fdt_excel_core.
          return.
      endtry .
      "GEt work sheets.
      lo_excel_ref->if_fdt_doc_spreadsheet~get_worksheet_names( importing worksheet_names = data(worksheet_names) ).
      "Get first work sheet (You can iterate over multiple if there are more than one work sheets.
      data(lo_data_ref) = lo_excel_ref->if_fdt_doc_spreadsheet~get_itab_from_worksheet( worksheet_names[ 1 ] ).

      assign lo_data_ref->* to  <lfs_tab>.

      move-corresponding <lfs_tab> to lt_excel.

*      sort lt_excel by a.
*      delete lt_excel where a eq space.

      loop at lt_excel assigning field-symbol(<_move_tab>) from 2.
        if <_move_tab>-a is initial.
          continue.
        endif.

        condense <_move_tab>-a no-gaps.
        condense <_move_tab>-b no-gaps.
        condense <_move_tab>-c no-gaps.

        condense <_move_tab>-d no-gaps.
        <_move_tab>-d = |{ <_move_tab>-d+4(4) }{ <_move_tab>-d+2(2) }{ <_move_tab>-d+0(2) }|.


        condense <_move_tab>-e no-gaps.
        condense <_move_tab>-f no-gaps.
        condense <_move_tab>-g no-gaps.
        condense <_move_tab>-h no-gaps.
        condense <_move_tab>-i no-gaps.
        condense <_move_tab>-j no-gaps.
        condense <_move_tab>-k no-gaps.
        condense <_move_tab>-l no-gaps.

        ls_import-zcod_log        = <_move_tab>-a.
        ls_import-sid             = <_move_tab>-b.
        ls_import-instance        = <_move_tab>-c.
        ls_import-data            = <_move_tab>-d.
        ls_import-hora            = <_move_tab>-e.
        ls_import-slgmand         = <_move_tab>-f.
        ls_import-zareasubid      = <_move_tab>-g.
        ls_import-slguser         = <_move_tab>-h.
        ls_import-slgltrm2        = <_move_tab>-i.
        ls_import-slgtc           = <_move_tab>-j.
        ls_import-slgrepna        = <_move_tab>-k.
        ls_import-sal_data        = <_move_tab>-l.
        append ls_import to lt_import.
        clear:
          ls_import.
      endloop.

    endif.

    move-corresponding lt_import to it_SAIDA.

  endmethod.
endclass.

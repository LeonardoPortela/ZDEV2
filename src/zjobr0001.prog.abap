*&---------------------------------------------------------------------*
*& Report ZJOBR0001
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
report zjobr0001.

*&---------------------------------------------------------------------*
*&  TYPES
*&---------------------------------------------------------------------*
types:
  begin of ty_body_alv,
    instancia       type zjob0002-nr_incidente,
    recorrencia     type i,
    itatendn1n2     type i,
    n3sistdesc(255) type c,
    status_job      type icon_d,
  end of ty_body_alv.

type-pools: icon.


*&---------------------------------------------------------------------*
*&  TABELAS E WORKAREAS
*&---------------------------------------------------------------------*
data: it_saida         type table of ty_body_alv,
      new_line         type ty_body_alv,
      wa_set_coment_ir type zfie_ob_set_coment_ir,
      wa_finalizar_ir  type zfie_ob_finalizar_ir,
      it_del_row       type table of lvc_s_row-index,
      wl_del_row       type lvc_s_row-index,
      gt_selected_rows type lvc_t_row.


*&---------------------------------------------------------------------*
*&  ALV
*&---------------------------------------------------------------------*
types: begin of ty_estrutura.
         include type slis_fieldcat_main.
         include type slis_fieldcat_alv_spec.
types: end of ty_estrutura.

data: dg_splitter_1        type ref to cl_gui_splitter_container,
      g_grid               type ref to cl_gui_alv_grid,
      g_custom_container   type ref to cl_gui_custom_container,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      container_1          type ref to cl_gui_container,
      cl_container_95      type ref to cl_gui_docking_container,
      obj_dyndoc_id        type ref to cl_dd_document,
      tl_function          type ui_functions,
      wl_function          type ui_func,
*
      t_fieldcat           type lvc_t_fcat,
      w_fieldcat           type lvc_s_fcat,
      t_colorcell          type table of lvc_s_scol,
      w_colorcell          type lvc_s_scol,
      t_exctab             type slis_t_extab,
      w_exctab             type slis_extab,
      w_layout             type lvc_s_layo,
      w_stable             type lvc_s_stbl,
      t_style              type lvc_t_styl,
      w_style              type lvc_s_styl,
      t_rows               type lvc_t_row,
      w_rows               type lvc_s_row,
      ok_code              type sy-ucomm,
*
      zcl_util             type ref to zcl_util.

data: variante         like disvariant.
data: gs_variant_c type disvariant.

data: it_return type table of ddshretval,
      it_t028g  type table of t028g.


*&---------------------------------------------------------------------*
*&  CLASSES
*&---------------------------------------------------------------------*
class lcl_event_handler definition.
  public section.


    class-methods:
      on_double_click for event double_click of cl_gui_alv_grid
        importing e_row e_column.

*    CLASS-METHODS:
*      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
*        IMPORTING e_row_id e_column_id.

    class-methods:
      set_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object.

    class-methods:
      get_ucomm for event user_command of cl_gui_alv_grid
        importing e_ucomm.

    class-methods:
      handle_data_changed for event data_changed of cl_gui_alv_grid
        importing
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm.

endclass.                    "LCL_EVENT_HANDLER DEFINITION


class lcl_event_handler implementation.
  method on_double_click.

  endmethod.

  method set_toolbar.

    data wa_tool type stb_button.

    move 3 to wa_tool-butn_type.
    append wa_tool to e_object->mt_toolbar.
    clear wa_tool.

    wa_tool-function     = 'BTN_INSERT_ROW'.
    wa_tool-icon         =  icon_insert_row.
    wa_tool-quickinfo    = 'Inserir linha'.
    append wa_tool to e_object->mt_toolbar.
    clear wa_tool.

    wa_tool-function     = 'BTN_DELETE_ROW'.
    wa_tool-icon         =  icon_delete_row.
    wa_tool-quickinfo    = 'Deletar linha'.
    append wa_tool to e_object->mt_toolbar.
    clear wa_tool.

    wa_tool-function     = 'BTN_FINALIZAR_IR'.
    wa_tool-icon         =  icon_system_end.
    wa_tool-quickinfo    = 'Finalizar IR'.
    wa_tool-text         = 'Finalizar IR'.
    append wa_tool to e_object->mt_toolbar.
    clear wa_tool.

  endmethod.

  method get_ucomm.
    case e_ucomm.
      when 'BTN_INSERT_ROW'.

        perform f_insert_row.

      when 'BTN_DELETE_ROW'.

        perform f_delete_row.

      when 'BTN_FINALIZAR_IR'.

        perform f_finalizar_ir.

    endcase.
  endmethod.


  method handle_data_changed..

    data wa_paste type ty_body_alv.
    data it_paste type table of ty_body_alv.

    loop at er_data_changed->mt_good_cells into data(_item_changed).

      condense _item_changed-value.

      read table it_saida assigning field-symbol(<fs_get_index>) index _item_changed-row_id.

      if sy-subrc <> 0.
        append new_line to it_saida.
      endif.

      read table it_saida assigning field-symbol(<fs_set_info>) index _item_changed-row_id.

      case _item_changed-fieldname.
        when 'INSTANCIA'.
          <fs_set_info>-instancia = _item_changed-value.
        when 'RECORRENCIA'.
          if _item_changed-value is initial.
            <fs_set_info>-recorrencia = '0'.
          else.
            <fs_set_info>-recorrencia = _item_changed-value.
          endif.
        when 'ITATENDN1N2 '.
          if _item_changed-value eq '0'.
            <fs_set_info>-itatendn1n2 = '1'.
          else.
            <fs_set_info>-itatendn1n2 = _item_changed-value.
          endif.
        when 'N3SISTDESC'.
          <fs_set_info>-n3sistdesc = _item_changed-value.
        when 'STATUS_JOB'.
          if _item_changed-value is initial.
            <fs_set_info>-status_job = icon_yellow_light.
          else.
            <fs_set_info>-status_job = _item_changed-value.
          endif.
      endcase.

    endloop.


    if er_data_changed->mt_good_cells is initial.
      case e_ucomm.
        when 'BTN_INSERT_ROW'.
          perform f_insert_row.
      endcase.
    else.
      leave to screen 0100.
    endif.

  endmethod.

*  METHOD on_hotspot_click.
*    DATA : l_columnid TYPE lvc_s_col.
*
*    READ TABLE it_saida INTO wa_saida INDEX e_row_id-index.
*
*    CASE e_column_id-fieldname.
*      WHEN 'EBELN'.
*        SET PARAMETER ID 'BES' FIELD wa_saida-ebeln.
*
*        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
*
*      WHEN 'BELNR'.
*        SET PARAMETER ID 'BLN' FIELD wa_saida-belnr.
*        SET PARAMETER ID 'BUK' FIELD wa_saida-bukrs.
*        SET PARAMETER ID 'GJR' FIELD wa_saida-gjahr.
*
*        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
*
*    ENDCASE.
*  ENDMETHOD.
endclass.


*&---------------------------------------------------------------------*
*&  START
*&---------------------------------------------------------------------*
start-of-selection.
  perform f_exibir_dados.


*&---------------------------------------------------------------------*
*& Form f_exibir_dados
*&---------------------------------------------------------------------*
form f_exibir_dados .

  call screen 0100.

endform.


*&---------------------------------------------------------------------*
*& Form f_imprimir_dados
*&---------------------------------------------------------------------*
form f_init_alv .

  data: wl_layout type slis_layout_alv.
  data:
    p_text      type sdydo_text_element,
    filtros	    type zif_screen_linha_filtro,
    i_filtros	  type zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) type c,
    v_uzeit(10) type c.


  perform f_fieldcatalog.

  variante = value #( report = sy-repid ).



  if g_grid is initial.

    clear: i_filtros.
    concatenate sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) into v_datum.
    concatenate sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) into v_uzeit.
    describe table it_saida lines data(v_lines).
    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
    append value #( parametro = 'Hora:' valor = v_uzeit ) to i_filtros.
    append value #( parametro = 'Registros:' valor = v_lines ) to i_filtros.

    p_text = 'Encerrar IR SAP x SE'.
  endif.

  if zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      exporting
        i_titulo  = conv #( p_text )
        i_filtros = i_filtros
      changing
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.


    w_layout-sel_mode = 'A'.
    w_layout-col_opt  = abap_true.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function to tl_function.

    set handler: lcl_event_handler=>get_ucomm for g_grid,
                lcl_event_handler=>set_toolbar for g_grid,
                lcl_event_handler=>handle_data_changed for g_grid.

    call method g_grid->set_table_for_first_display
      exporting
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      changing
        it_outtab                     = it_saida[]
        it_fieldcatalog               = t_fieldcat
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.


    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    if lines( t_rows ) > 0.
      call method g_grid->set_selected_rows
        exporting
          it_index_rows = t_rows.
    endif.

  else.
    call method g_grid->refresh_table_display( is_stable = w_stable ).
  endif.

  wl_layout-colwidth_optimize = 'X'.

  if it_saida is initial.
    perform f_insert_row.
  endif.

endform.


*&---------------------------------------------------------------------*
*& Form montar_layout
*&---------------------------------------------------------------------*
form f_fieldcatalog .

  free t_fieldcat[].

  perform f_estrutura_alv using:
 01  ''   ''   'IT_SAIDA'   'INSTANCIA  '            'Nº do Incidente            '       '15 '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'   'RECORRENCIA'            'Cód. Recorrência           '       '15 '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'   'ITATENDN1N2'            'Cód. Atendimento           '       '15 '  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'   'N3SISTDESC '            'Desc. Análise              '       '100'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'   'STATUS_JOB '            'Status do JOB              '       '15 '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

endform.


*&---------------------------------------------------------------------*
*& Form  f_estrutura_alv
*&---------------------------------------------------------------------*
form f_estrutura_alv  using value(p_col_pos)       type i                    "1
                           value(p_ref_tabname)   like dd02d-tabname        "2
                           value(p_ref_fieldname) like dd03d-fieldname      "3
                           value(p_tabname)       like dd02d-tabname        "4
                           value(p_field)         like dd03d-fieldname      "5
                           value(p_scrtext_l)     like dd03p-scrtext_l      "6
                           value(p_outputlen)                               "7
                           value(p_edit)                                    "8
                           value(p_sum)                                     "9
                           value(p_just)                                    "10
                           value(p_hotspot)                                 "11
                           value(p_f4)                                      "12
                           value(p_checkbox)                                "13
                           value(p_style)                                   "14
                           value(p_no_out)                                  "15
                           value(p_icon)                                    "16
                           value(p_fix).                                    "17

  clear w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
*  W_FIELDCAT-COL_OPT     = 'X'.

  append w_fieldcat to t_fieldcat.

endform.


*&---------------------------------------------------------------------*
*& FORM  f_insert_row
*&---------------------------------------------------------------------*
form f_insert_row.

  clear: new_line.

  new_line-recorrencia = '0'.
  new_line-itatendn1n2 = '1'.
  new_line-status_job = icon_yellow_light.

  append new_line to it_saida.

  perform f_init_alv.

  g_grid->refresh_table_display( ).

endform.


*&---------------------------------------------------------------------*
*& FORM  f_delete_row
*&---------------------------------------------------------------------*
form f_delete_row.

  clear: gt_selected_rows, gt_selected_rows[], it_del_row[], wl_del_row.

  call method g_grid->get_selected_rows
    importing
      et_index_rows = gt_selected_rows.

  loop at gt_selected_rows into data(wl_selected_rows).

    read table it_saida assigning field-symbol(<wa_saida>) index w_rows.

    wl_del_row = wl_selected_rows-index.
    append wl_del_row to it_del_row.

  endloop.

  if it_del_row[] is not initial.
    sort it_del_row descending.
    loop at it_del_row into wl_del_row.

      delete it_saida index wl_del_row.
    endloop.
  endif.

  g_grid->refresh_table_display( ).

endform.


*&---------------------------------------------------------------------*
*& FORM  f_finalizar_ir
*&---------------------------------------------------------------------*
form f_finalizar_ir.

  if it_saida is not initial.
    loop at it_saida assigning field-symbol(<wa_saida>).

      wa_set_coment_ir = value #( instancia    = value #( ( instancia = <wa_saida>-instancia ) )
                                  form = value #(
                                  n3sistdesc   = <wa_saida>-n3sistdesc
                                  recorrencia  = <wa_saida>-recorrencia
                                  itatendn1n2   = <wa_saida>-itatendn1n2 )
                                  entity_id    = 'IR' ).
      try .
          "Chama API para setar o comentário.
          zcl_int_ob_set_coment_ir=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = wa_set_coment_ir importing e_integracao = data(r_response_coment) ).
          if r_response_coment-nm_code eq '0200'.

            wa_finalizar_ir = value #( instancia    = <wa_saida>-instancia
                                       id_atividade = 'RealizaAtendNivel3Sist'
                                       id_sequencia = '6' ).
            "Chama API para encerrar o IR.
            zcl_int_ob_finalizar_ir=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = wa_finalizar_ir importing e_integracao = data(r_response_finalizar) ).
            if r_response_finalizar-nm_code eq '0200'.
              message 'Incidentes processados.' type 'S'.

              <wa_saida>-status_job = icon_green_light.
            endif.

          endif.

        catch zcx_integracao into data(ex_integra).    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        catch zcx_error into data(ex_error).    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

          try.
*          ==//================================//
              wa_finalizar_ir = value #( instancia    = <wa_saida>-instancia
                                           id_atividade = 'RealizaAtendNivel3Sist'
                                           id_sequencia = '5' ).
              "Chama API para encerrar o IR.
              zcl_int_ob_finalizar_ir=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = wa_finalizar_ir importing e_integracao = r_response_finalizar ).
              if r_response_finalizar-nm_code eq '0200'.
                message 'Incidentes processados.' type 'S'.
                <wa_saida>-status_job = icon_green_light.
              endif.
*          ===//==============================//catch zcx_integracao into data(ex_integra).    "
            catch zcx_integracao into ex_integra.    "
              ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

            catch zcx_error into ex_error.    "  "
              ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

              <wa_saida>-status_job = icon_red_light.
          endtry.
      endtry.
    endloop.


    call method g_grid->refresh_table_display( is_stable = w_stable ).
  endif.
endform.


*&---------------------------------------------------------------------*
*& Module STATUS_100 OUTPUT
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'PF0100'.
  set titlebar 'TB0100'.

  perform f_init_alv.
endmodule.


*&---------------------------------------------------------------------*
*&  Module  USER_COMMAND_100  INPUT
*&---------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.

    when 'EXIT' or 'CANCEL'.
      leave program.
  endcase.
endmodule.

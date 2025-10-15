**/===========================================================================\*
**|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
**|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
**|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
**|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
**|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
**|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
**| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
**/===========================================================================\*

**/===========================================================================\*
**|  Desenvolvedor:                                                           |*
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br)                         |*
**|                                                                           |*
**|  Tester:                                                                  |*
**|    + Anderson Oenning ( anderson.oenning@amaggi.com.br)                    |*
**|  Changelog:                                                               |*
**|                                                                           |*
**/===========================================================================\*

**/===========================================================================\*
**| Descrição:                                                                |*
**| Relatório para consultar perfis de acesso                   |*
**/===========================================================================\*
report zper0001.

tables: ztd_opns_018, agr_tcodes, adcp, /virsa/zcrtran, usr21, agr_define, agr_agrs, usr02, agr_users, iloa, sscrfields, zpert0001.

"Declaração ALV.
data: dg_splitter_1             type ref to cl_gui_splitter_container,
      dg_splitter_ccusto        type ref to cl_gui_splitter_container,
      dg_splitter_log           type ref to cl_gui_splitter_container,
      g_grid                    type ref to cl_gui_alv_grid,
      g_grid_log                type ref to cl_gui_alv_grid,
      g_grid_ccusto             type ref to cl_gui_alv_grid,
      g_custom_container        type ref to cl_gui_custom_container,
      g_custom_container_ccusto type ref to cl_gui_custom_container,
      g_custom_container_log    type ref to cl_gui_custom_container,
      c_alv_toolbarmanager      type ref to cl_alv_grid_toolbar_manager,
      container_1               type ref to cl_gui_container,
      container_1_ccusto        type ref to cl_gui_container,
      container_1_log           type ref to cl_gui_container,
      cl_container_95           type ref to cl_gui_docking_container,
      obj_dyndoc_id             type ref to cl_dd_document,
      tl_function               type ui_functions,
      wl_function               type ui_func,
      t_fieldcat                type lvc_t_fcat,
      w_fieldcat                type lvc_s_fcat,
      t_fieldcat_ccusto         type lvc_t_fcat,
      w_fieldcat_ccusto         type lvc_s_fcat,
      t_fieldcat_log            type lvc_t_fcat,
      w_fieldcat_log            type lvc_s_fcat,
      t_colorcell               type table of lvc_s_scol,
      w_colorcell               type lvc_s_scol,
      t_exctab                  type slis_t_extab,
      w_exctab                  type slis_extab,
      w_layout                  type lvc_s_layo,
      w_stable                  type lvc_s_stbl,
      w_stable_ccusto           type lvc_s_stbl,
      w_stable_log              type lvc_s_stbl,
      t_style                   type lvc_t_styl,
      w_style                   type lvc_s_styl,
      t_rows                    type lvc_t_row,
      w_rows                    type lvc_s_row,
      ok_code                   type sy-ucomm,
      g_container_ccusto        type scrfname value 'CC_CCUSTO',
      g_custom_ccusto           type ref to cl_gui_custom_container,
      g_container_log           type scrfname value 'LOG_USER',
      g_custom_log              type ref to cl_gui_custom_container.


data: dg_splitter_2          type ref to cl_gui_splitter_container,
      g_grid_2               type ref to cl_gui_alv_grid,
      g_custom_container_2   type ref to cl_gui_custom_container,
      c_alv_toolbarmanager_2 type ref to cl_alv_grid_toolbar_manager,
      container_2            type ref to cl_gui_container,
      cl_container_95_2      type ref to cl_gui_docking_container,
      obj_dyndoc_id_2        type ref to cl_dd_document,
      tl_function_2          type ui_functions,
      wl_function_2          type ui_func,
      t_fieldcat_2           type lvc_t_fcat,
      w_fieldcat_2           type lvc_s_fcat,
      t_colorcell_2          type table of lvc_s_scol,
      w_colorcell_2          type lvc_s_scol,
      t_exctab_2             type slis_t_extab,
      w_exctab_2             type slis_extab,
      w_layout_2             type lvc_s_layo,
      w_stable_2             type lvc_s_stbl,
      t_style_2              type lvc_t_styl,
      w_style_2              type lvc_s_styl,
      t_rows_2               type lvc_t_row,
      w_rows_2               type lvc_s_row,
      ok_code_2              type sy-ucomm.


data: variante         like disvariant.
data: gs_variant_c type disvariant,
      wl_toolbar   type stb_button.


"Declaração filtros.
data: it_param            type rs_t_select,
      it_saida_alv        type zpere0001_t,
      it_saida_select     type zpere0001_t,
      it_saida_ccusto     type zpere0003_t,
      it_saida_alv_two    type zpere0002_t,
      it_saida_select_two type zpere0002_t,
      it_saida_alv_LOG    type zpere0004_t,
      wa_selected_rows    type lvc_s_row,
      it_selected_rows    type lvc_t_row,
      lines               type sy-tabix.

data: icon_proc type string.


data: object type ref to zcl_perfil_acesso.
create object object.


selection-screen: begin of block b1 with frame title text-001.
  select-options: P_uname   for  usr21-bname,
                  p_ccusto  for  iloa-kostl,
                  p_perf_c  for  agr_users-agr_name,
                  p_perf_s  for  agr_users-agr_name,
                  p_ras     for  zpert0001-numero_ras,
                  p_date    for sy-datum modif id i no-extension,
                  p_tcode   for ztd_opns_018-sal_data.
selection-screen: end of block b1.

selection-screen begin of block b2 with frame title text-002.
  parameters: p_acess  type c default 'X' radiobutton group g1 user-command acess,
              p_remace type c radiobutton group g1,
              p_trans  type c radiobutton group g1,
              p_log    type c radiobutton group g1.
selection-screen end of block b2.

selection-screen function key 1.

initialization.

  icon_proc = icon_select_detail && 'Consultar Centro de Custo - Usuarios Ativos'.
  sscrfields-functxt_01 = icon_proc .

*at selection-screen on value-request for p_uname. "PAI


at selection-screen.
  case sscrfields-ucomm. "pushbutton pressed
    when 'FC01'.
      create object object.
      free: it_saida_ccusto.
      it_saida_ccusto = object->get_ccusto_ativo( ).
      perform fm_exibir_dados_ccusto.
  endcase.


at selection-screen on value-request for P_uname-low.
  perform onf4.


at selection-screen output.
  if p_acess eq abap_true.
    loop at screen.
      if screen-name eq  'P_DATE-LOW' or
        screen-name  eq  'P_DATE-HIGH' or
        screen-name  eq  '%_P_DATE_%_APP_%-TEXT' or
        screen-name eq '%_P_DATE_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_DATE_%_APP_%-VALU_PUSH' or
        screen-name eq  'P_TCODE-LOW' or
        screen-name  eq  'P_TCODE-HIGH' or
        screen-name  eq  '%_P_TCODE_%_APP_%-TEXT' or
        screen-name eq '%_P_TCODE_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_TCODE_%_APP_%-VALU_PUSH' or
        screen-name eq  'P_RAS-LOW' or
        screen-name  eq  'P_RAS-HIGH' or
        screen-name  eq  '%_P_RAS_%_APP_%-TEXT' or
        screen-name eq '%_P_RAS_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_RAS_%_APP_%-VALU_PUSH'.
        screen-active = 0.
        modify screen.
      endif.
    endloop.
  elseif p_trans eq abap_true.
    loop at screen.
      if screen-name eq  'P_DATE-LOW' or
        screen-name  eq  'P_DATE-HIGH' or
        screen-name  eq  '%_P_DATE_%_APP_%-TEXT' or
        screen-name eq '%_P_DATE_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_DATE_%_APP_%-VALU_PUSH' or
        screen-name eq  'P_TCODE-LOW' or
        screen-name  eq  'P_TCODE-HIGH' or
        screen-name  eq  '%_P_TCODE_%_APP_%-TEXT' or
        screen-name eq '%_P_TCODE_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_TCODE_%_APP_%-VALU_PUSH'.
        screen-active = 0.
        modify screen.

      endif.
    endloop.
  elseif p_remace eq abap_true.
    loop at screen.
      if screen-name eq  'P_TCODE-LOW' or
        screen-name  eq  'P_TCODE-HIGH' or
        screen-name  eq  '%_P_TCODE_%_APP_%-TEXT' or
        screen-name eq '%_P_TCODE_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_TCODE_%_APP_%-VALU_PUSH'.
        screen-active = 0.
        modify screen.

      endif.
    endloop.

  else.
    loop at screen.
      if screen-name eq 'P_PERF_C-LOW' or
        screen-name eq 'P_PERF_C-HIGH' or
        screen-name eq '%_P_PERF_C_%_APP_%-TEXT' or
        screen-name eq '%_P_PERF_C_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_PERF_C_%_APP_%-VALU_PUSH' or

        screen-name eq  'P_RAS-LOW' or
        screen-name  eq  'P_RAS-HIGH' or
        screen-name  eq  '%_P_RAS_%_APP_%-TEXT' or
        screen-name eq '%_P_RAS_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_RAS_%_APP_%-VALU_PUSH' or

        screen-name eq  'P_CCUSTO-LOW' or
        screen-name  eq  'P_CCUSTO-HIGH' or
        screen-name  eq  '%_P_CCUSTO_%_APP_%-TEXT' or
        screen-name eq '%_P_CCUSTO_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_CCUSTO_%_APP_%-VALU_PUSH' or

        screen-name eq 'P_PERF_S-LOW' or
        screen-name eq 'P_PERF_S-HIGH' or
        screen-name eq '%_P_PERF_S_%_APP_%-TEXT' or
        screen-name eq '%_P_PERF_S_%_APP_%-OPTI_PUSH' or
        screen-name eq '%_P_PERF_S_%_APP_%-VALU_PUSH' .

        screen-active = 0.
        modify screen.
      endif.
    endloop.

  endif.



*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_event_toolbar definition.
  public section.
    class-methods:
      set_toolbar  for event toolbar of cl_gui_alv_grid
        importing e_object.

    class-methods:
      get_ucomm   for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.                    "LCL_EVENT_TOOLBAR DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_toolbar IMPLEMENTATION
*----------------------------------------------------------------------*
class lcl_event_toolbar implementation.

  method set_toolbar.
    clear: wl_toolbar.

    if p_acess is not initial or p_trans is not initial.
      wl_toolbar-function     = 'BTN_REGIS_RAS'.
      wl_toolbar-icon         = icon_intensify_critical.
*      wl_toolbar-butn_type    = 1.
      wl_toolbar-text         = 'Registrar RAS/SE'.
      append wl_toolbar to e_object->mt_toolbar.
      clear wl_toolbar.
    endif.

    if p_remace is not initial.
      wl_toolbar-function     = 'BTN_RETORNAR_ACESSO'.
      wl_toolbar-icon         = icon_refresh.
*      wl_toolbar-butn_type    = 2.
      wl_toolbar-text         = 'Retornar acesso/transação'.
      append wl_toolbar to e_object->mt_toolbar.
      clear wl_toolbar.
    endif.

  endmethod.                    "SET_TOOLBAR

  method get_ucomm.
    case e_ucomm.
      when 'BTN_RETORNAR_ACESSO'.

        call method g_grid_2->get_selected_rows
          importing
            et_index_rows = it_selected_rows.

        describe table it_selected_rows lines lines.

        if ( lines is initial ).
          message text-e01 type 'I' display like 'E'.
        else.
          loop at it_selected_rows into wa_selected_rows.
            read table it_saida_alv_two into data(wa_saida_two) index wa_selected_rows-index.
            if sy-subrc eq 0.
              append wa_saida_two to it_saida_select_two.
            endif.
            clear: wa_saida_two.
          endloop.

          object->add_perfil(
            exporting
              i_param = it_saida_select_two " Estrutura de dados perfil de acesso
            importing
              e_param = it_saida_select_two " Campo de texto do comprimento 1
          ).

          it_saida_alv_two = object->get_logs_proc( it_param ).

          call method g_grid_2->refresh_table_display( is_stable = w_stable_2 ).

        endif.
      when 'BTN_REGIS_RAS'.

        call method g_grid->get_selected_rows
          importing
            et_index_rows = it_selected_rows.

        describe table it_selected_rows lines lines.

        if ( lines is initial ).
          message text-e01 type 'I' display like 'E'.
        else.
          loop at it_selected_rows into wa_selected_rows.
            read table it_saida_alv into data(wa_saida) index wa_selected_rows-index.
            if sy-subrc eq 0 and wa_saida-icon_name eq icon_red_light.
              append wa_saida to it_saida_select.
            endif.
            clear: wa_saida.
          endloop.

          if p_trans is not initial.
            data(it_it_saida_select_aux) = it_saida_select.
            sort it_it_saida_select_aux by perfil_simples transacao.
            delete adjacent duplicates from it_it_saida_select_aux comparing perfil_simples transacao.

            object->create_ras( i_param = it_it_saida_select_aux i_tipo_remov = 'TRANS' ).
            it_saida_alv = object->get_perfis_acesso_trans( it_param ).
            call method g_grid->refresh_table_display( is_stable = w_stable_2 ).
          else.
            object->create_ras( i_param = it_saida_select i_tipo_remov = 'PERFIL' ).
            it_saida_alv = object->get_perfis_acesso( it_param ).
            call method g_grid->refresh_table_display( is_stable = w_stable_2 ).
          endif.


        endif.
    endcase.


  endmethod.                    "GET_UCOMM
endclass.                    "LCL_EVENT_TOOLBAR IMPLEMENTATION



start-of-selection.

  if p_uname is not initial.
    loop at p_uname.
      append value #( fieldnm = 'UNAME' sign = p_uname-sign option = p_uname-option low = p_uname-low ) to it_param.
    endloop.
  endif.

  if p_ccusto is not initial.
    loop at p_ccusto.
      append value #( fieldnm = 'CENTRO_CUSTO' sign = p_ccusto-sign option = p_ccusto-option low = p_ccusto-low ) to it_param.
    endloop.
  endif.

  if p_perf_c is not initial.
    loop at p_perf_c.
      append value #( fieldnm = 'PERFIL_COMPOSTO' sign = p_perf_c-sign option = p_perf_c-option low = p_perf_c-low ) to it_param.
    endloop.
  endif.

  if p_perf_s is not initial.
    loop at p_perf_s.
      append value #( fieldnm = 'PERFIL_SIMPLES' sign = p_perf_s-sign option = p_perf_s-option low = p_perf_s-low ) to it_param.
    endloop.
  endif.

  if p_date is not initial.
    loop at p_date.
      append value #( fieldnm = 'DATE' sign = p_date-sign option = p_date-option low = p_date-low high = p_date-high   ) to it_param.
    endloop.
  endif.

  if p_ras is not initial.
    loop at p_RAS.
      append value #( fieldnm = 'RAS' sign = p_RAS-sign option = p_RAS-option low = p_RAS-low high = p_RAS-high   ) to it_param.
    endloop.
  endif.

  if p_TCODE is not initial.
    loop at p_tcode.
      append value #( fieldnm = 'TCODE' sign = p_TCODE-sign option = p_TCODE-option low = p_TCODE-low high = p_TCODE-high   ) to it_param.
    endloop.
  endif.

  if p_acess is not initial.
    "Seleciona dados perfil.
    free: it_saida_alv.
    it_saida_alv = object->get_perfis_acesso( it_param ).
  elseif p_trans eq abap_true.
    "Seleciona dados perfil.
    free: it_saida_alv.
    it_saida_alv = object->get_perfis_acesso_trans( it_param ).
  elseif p_remace eq abap_true.
    it_saida_alv_two = object->get_logs_proc( it_param ).
  elseif p_log eq abap_true.
    it_saida_alv_log = object->get_logs_acesso( it_param ).
  endif.

  perform fm_alv.

*&---------------------------------------------------------------------*
*& Form fm_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_alv .
  call screen 100.
endform.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'PF0100'.
  set titlebar 'TB0100'.

  if p_acess eq abap_true or p_trans eq abap_true.
    perform f_init_alv.
  endif.

  if p_remace eq abap_true.
    perform f_init_alv_two.
  endif.

  if p_log eq abap_true.
    perform f_init_alv_log.
  endif.




endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  case sy-ucomm.
    when 'BACK'.
      leave to screen 0.

    when 'EXIT'.
      leave program.
  endcase.
endmodule.
*&---------------------------------------------------------------------*
*& Form f_init_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
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

  data: zvar_dias      type p value '90',
        zvar_date_base type sy-datum,
        zv_periodo     type char30.


  perform f_fieldcatalog.

  variante = value #( report = sy-repid ).

  if g_grid is initial.

    "Seleciona quantidade de dias / acesso transação.
    select single * from tvarvc into
    @data(ws_perf)
    where name eq 'ZPERFIL_EXPIRA_ACESSO_DIAS'.
    if sy-subrc eq 0.
      zvar_dias = ws_perf-low.
    endif.
    zvar_date_base = sy-datum - zvar_dias.

    zv_periodo = |{ zvar_date_base+6(2) }/{ zvar_date_base+4(2) }/{ zvar_date_base+0(4) } - { sy-datum+6(2) }/{ sy-datum+4(2) }/{ sy-datum+0(4) }|.

    clear: i_filtros.
    concatenate sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) into v_datum.
    concatenate sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) into v_uzeit.
    describe table it_saida_alv lines data(v_lines).
    append value #( parametro = 'Periodo:' valor = zv_periodo ) to i_filtros.
    append value #( parametro = 'Qtde.dias' valor = zvar_dias ) to i_filtros.
*    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
*    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
*    append value #( parametro = 'Hora:' valor = v_uzeit ) to i_filtros.
    append value #( parametro = 'Qtde.Registros:' valor = v_lines ) to i_filtros.

  endif.

  p_text = 'Exibir perfil de acesso por usuario'.

  if zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      exporting
        i_titulo  = conv #( p_text )
        i_filtros = i_filtros
      changing
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.


    w_layout-sel_mode    = 'A'.
    w_layout-col_opt     = abap_true.
    w_layout-cwidth_opt  = abap_true.

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


    set handler:
     lcl_event_toolbar=>set_toolbar     for g_grid,
     lcl_event_toolbar=>get_ucomm       for g_grid.

    call method g_grid->set_table_for_first_display
      exporting
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      changing
        it_outtab                     = it_saida_alv[]
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


endform.
*&---------------------------------------------------------------------*
*& Form f_fieldcatalog
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_fieldcatalog .

  free t_fieldcat[].

  perform f_estrutura_alv using:
  01  ''   ''   'IT_SAIDA_ALV'   'ICON_NAME         '            'Status                            '       '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  02  ''   ''   'IT_SAIDA_ALV'   'DESC_STATUS       '            'Desc.Status                       '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.
  if p_trans eq abap_true.
    perform f_estrutura_alv using:
    03  ''   ''   'IT_SAIDA_ALV'   'NUMERO_RAS        '            'Número das RAS                    '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.
  endif.
  perform f_estrutura_alv using:
  04  ''   ''   'IT_SAIDA_ALV'   'USUARIO           '            'Usuario                           '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  05  ''   ''   'IT_SAIDA_ALV'   'CNAME             '            'Nome do usuario                   '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  06  ''   ''   'IT_SAIDA_ALV'   'FUNCAO            '            'Cargo                             '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  07  ''   ''   'IT_SAIDA_ALV'   'PERFIL_COMPOSTO   '            'Perfil Composto                   '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  08  ''   ''   'IT_SAIDA_ALV'   'PERFIL_SIMPLES    '            'Perfil Simples                    '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.
  if p_trans eq abap_true.
    perform f_estrutura_alv using:
    09  ''   ''   'IT_SAIDA_ALV'   'TRANSACAO         '            'Transação                         '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.
  endif.

  if p_trans eq abap_true.
    perform f_estrutura_alv using:
    10  ''   ''   'IT_SAIDA_ALV'   'ZQUANT_ACESSO     '          'Qte Acessada Transação           '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.
  else.
    perform f_estrutura_alv using:
  10  ''   ''   'IT_SAIDA_ALV'   'ZQUANT_ACESSO     '            'Qte Trans.Acessadas               '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.
  endif.
  perform f_estrutura_alv using:
*  10  ''   ''   'IT_SAIDA_ALV'   'ZQUANT_ACESSO     '            'Qte Trans.Acessadas               '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  11  ''   ''   'IT_SAIDA_ALV'   'STEXT             '            'Unid.Organizacional               '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  12  ''   ''   'IT_SAIDA_ALV'   'WERKS             '            'Filial                            '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  13  ''   ''   'IT_SAIDA_ALV'   'WERKSN            '            'Desc.Filial                       '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  14  ''   ''   'IT_SAIDA_ALV'   'KOSTL             '            'Centro de custo                   '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  15  ''   ''   'IT_SAIDA_ALV'   'CCUSTO            '            'Desc.Centro de custo              '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.


endform.
*&---------------------------------------------------------------------*
*& Form f_estrutura_alv
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
* w_fieldcat-col_opt     = 'X'.

  case p_field.
    when 'USUARIO'.
      w_fieldcat-ref_table = 'AGR_USERS'.
      w_fieldcat-ref_field = 'UNAME'.
    when 'PERFIL_COMPOSTO'.
      w_fieldcat-ref_table = 'AGR_AGRS'.
      w_fieldcat-ref_field = 'AGR_NAME'.
    when 'PERFIL_SIMPLES'.
      w_fieldcat-ref_table = 'AGR_AGRS'.
      w_fieldcat-ref_field = 'CHILD_AGR'.
    when 'KOSTL'.
      w_fieldcat-ref_table = 'ZHCMT0007'.
      w_fieldcat-ref_field = 'KOSTL'.
    when others.
  endcase.


  append w_fieldcat to t_fieldcat.

endform.
*&---------------------------------------------------------------------*
*& Form f_init_alv_two
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_init_alv_two .

  data: wl_layout type slis_layout_alv.
  data:
    p_text      type sdydo_text_element,
    filtros	    type zif_screen_linha_filtro,
    i_filtros	  type zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) type c,
    v_uzeit(10) type c.


  perform f_fieldcatalog_two.

  variante = value #( report = sy-repid ).


  if g_grid_2 is initial.

    clear: i_filtros.
    concatenate sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) into v_datum.
    concatenate sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) into v_uzeit.
    describe table it_saida_alv lines data(v_lines).
    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
    append value #( parametro = 'Hora:' valor = v_uzeit ) to i_filtros.
    append value #( parametro = 'Registros:' valor = v_lines ) to i_filtros.

  endif.

  p_text = 'Exibir logs processamento perfil de acesso'.

  if zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      exporting
        i_titulo  = conv #( p_text )
        i_filtros = i_filtros
      changing
        split     = dg_splitter_2
        alv       = g_grid_2 ) = abap_true.


    w_layout_2-sel_mode = 'A'.
    w_layout_2-col_opt  = abap_true.
    w_layout-cwidth_opt  = abap_true.

    w_stable_2-row          = abap_true.
    w_stable_2-col          = abap_true.

    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_move_row.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_paste.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    append wl_function_2 to tl_function.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_undo.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_append_row.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_copy.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_loc_cut.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_check.
    append wl_function_2 to tl_function_2.
    wl_function_2 = cl_gui_alv_grid=>mc_fc_refresh.
    append wl_function_2 to tl_function_2.


    set handler:
     lcl_event_toolbar=>set_toolbar     for g_grid_2,
     lcl_event_toolbar=>get_ucomm       for g_grid_2.

    call method g_grid_2->set_table_for_first_display
      exporting
        is_layout                     = w_layout_2
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function_2
        is_variant                    = variante
      changing
        it_outtab                     = it_saida_alv_two[]
        it_fieldcatalog               = t_fieldcat_2
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.


    call method g_grid_2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid_2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    if lines( t_rows ) > 0.
      call method g_grid->set_selected_rows
        exporting
          it_index_rows = t_rows_2.
    endif.

  else.
    call method g_grid_2->refresh_table_display( is_stable = w_stable_2 ).
  endif.

  wl_layout-colwidth_optimize = 'X'.


endform.
*&---------------------------------------------------------------------*
*& Form f_fieldcatalog_two
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_fieldcatalog_two.


  free t_fieldcat_2[].

  perform f_estrutura_alv_2 using:
 01  ''   ''   'IT_SAIDA_ALV_TWO'   'ICON_NAME         '            'Status                                  '       '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA_ALV_TWO'   'DESC_STATUS       '            'Desc.Status                             '       '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA_ALV_TWO'   'NUMERO_RAS        '            'Número das RAS                          '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA_ALV_TWO'   'USUARIO           '            'Usuario                                 '       '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA_ALV_TWO'   'CNAME             '            'Nome do usuario                         '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'IT_SAIDA_ALV_TWO'   'FUNCAO            '            'Cargo                                   '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'IT_SAIDA_ALV_TWO'   'PERFIL_COMPOSTO   '            'Perfil composto                         '       '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'IT_SAIDA_ALV_TWO'   'PERFIL_SIMPLES    '            'Perfil simples                          '       '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'IT_SAIDA_ALV_TWO'   'STEXT             '            'Unid.Organizacional                     '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''   ''   'IT_SAIDA_ALV_TWO'   'WERKS             '            'Filial                                  '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  ''   ''   'IT_SAIDA_ALV_TWO'   'WERKSN            '            'Desc.Filial                             '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 12  ''   ''   'IT_SAIDA_ALV_TWO'   'KOSTL             '            'Centro de custo                         '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 13  ''   ''   'IT_SAIDA_ALV_TWO'   'CCUSTO            '            'Desc.Centro de custo                    '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 14  ''   ''   'IT_SAIDA_ALV_TWO'   'DATE_CRIACAO      '            'Data criação doc                        '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 15  ''   ''   'IT_SAIDA_ALV_TWO'   'HR_CRIACAO        '            'Hora criação doc                        '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 16  ''   ''   'IT_SAIDA_ALV_TWO'   'USER_CRIACAO      '            'Usuario criação                         '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 17  ''   ''   'IT_SAIDA_ALV_TWO'   'USER_MODIF        '            'Usuario modificação                     '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 18  ''   ''   'IT_SAIDA_ALV_TWO'   'DATE_MODIF        '            'Data da modificação                     '       '12'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 19  ''   ''   'IT_SAIDA_ALV_TWO'   'HR_MODIF          '            'Hora da modificação                     '       '12'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.


endform.
*&---------------------------------------------------------------------*
*& Form f_estrutura_alv_2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
form f_estrutura_alv_2  using value(p_col_pos)       type i                    "1
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

  case p_field.
    when 'USUARIO'.
      w_fieldcat-ref_table = 'AGR_USERS'.
      w_fieldcat-ref_field = 'UNAME'.
    when 'PERFIL_COMPOSTO'.
      w_fieldcat-ref_table = 'AGR_AGRS'.
      w_fieldcat-ref_field = 'AGR_NAME'.
    when 'PERFIL_SIMPLES'.
      w_fieldcat-ref_table = 'AGR_AGRS'.
      w_fieldcat-ref_field = 'CHILD_AGR'.
    when 'KOSTL'.
      w_fieldcat-ref_table = 'ZHCMT0007'.
      w_fieldcat-ref_field = 'KOSTL'.
    when others.
  endcase.


  append w_fieldcat to t_fieldcat_2.
endform.
*&---------------------------------------------------------------------*
*& Form fm_exibir_dados_ccusto
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_exibir_dados_ccusto .

  call screen 0200 starting at 5 5 ending at 150 40.

endform.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0200 output.
  set pf-status 'ST0200'.
  set titlebar 'TIT0200'.

  perform f_init_alv_ccusto.
endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.

  case sy-ucomm.
    when 'BACK' or 'CANCEL' or 'EXIT'.
      leave to screen 0.

    when 'OKAY'.
      perform fm_sel_ccusto_ativo.
    when others.
  endcase.

endmodule.
*&---------------------------------------------------------------------*
*& Form f_init_alv_ccusto
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_init_alv_ccusto .

  data: wl_layout type slis_layout_alv.
  data:
    p_text      type sdydo_text_element,
    filtros	    type zif_screen_linha_filtro,
    i_filtros	  type zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) type c,
    v_uzeit(10) type c.

  if g_custom_ccusto is initial.

    create object g_custom_ccusto
      exporting
        container_name = g_container_ccusto.

    create object g_grid_ccusto
      exporting
        i_parent = g_custom_ccusto.

    perform f_fieldcatalog_ccusto.

    clear w_layout-stylefname.
    w_layout-cwidth_opt = abap_true.
    w_layout-zebra      = abap_true.
    w_layout-sel_mode   = 'A'.
    w_layout-col_opt    = abap_true.

*    w_stable-row          = abap_true.
*    w_stable-col          = abap_true.
*    free: tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_check.
*    append wl_function to tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
*    append wl_function to tl_function.

    w_layout-cwidth_opt = abap_true.

*    set handler:
*     lcl_event_toolbar=>set_toolbar     for g_grid_ccusto,
*     lcl_event_toolbar=>get_ucomm       for g_grid_ccusto.

    call method g_grid_ccusto->set_table_for_first_display
      exporting
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      changing
        it_outtab                     = it_saida_ccusto[]
        it_fieldcatalog               = t_fieldcat_ccusto
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.


    call method g_grid_ccusto->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid_ccusto->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    if lines( t_rows ) > 0.
      call method g_grid_ccusto->set_selected_rows
        exporting
          it_index_rows = t_rows.
    endif.

  else.
    call method g_grid_ccusto->refresh_table_display( is_stable = w_stable_ccusto ).
  endif.

  wl_layout-colwidth_optimize = 'X'.

endform.
*&---------------------------------------------------------------------*
*& Form f_fieldcatalog_ccusto
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_fieldcatalog_ccusto .

  free t_fieldcat_ccusto[].

  perform f_estrutura_alv_ccusto using:
 01  ''   ''   'T_SAIDA_CCUSTO'   'KOSTL         '            'Centro de custo                   '       '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'T_SAIDA_CCUSTO'   'CCUSTO        '            'Desc.Cent.Custo                   '       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'T_SAIDA_CCUSTO'   'BUKRS         '            'Empresa                           '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'T_SAIDA_CCUSTO'   'BUTXT         '            'Desc.Empresa                      '       '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'T_SAIDA_CCUSTO'   'WERKS         '            'Filial                            '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'T_SAIDA_CCUSTO'   'WERKSN        '            'Desc.Filial                       '       '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'T_SAIDA_CCUSTO'   'NUMERO_RAS    '            'Numero RAS                        '       '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'T_SAIDA_CCUSTO'   'DATE_CRIACAO  '            'Data criação doc                  '       '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'T_SAIDA_CCUSTO'   'HR_CRIACAO    '            'Hora criação doc                  '       '30'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.


endform.

form f_estrutura_alv_ccusto  using value(p_col_pos)       type i                    "1
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
* w_fieldcat-col_opt     = 'X'.

  case p_field.
    when 'KOSTL'.
      w_fieldcat-ref_table = 'ZHCMT0007'.
      w_fieldcat-ref_field = 'KOSTL'.
    when others.
  endcase.


  append w_fieldcat to t_fieldcat_ccusto.

endform.
*&---------------------------------------------------------------------*
*& Form fm_sel_ccusto_ativo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form fm_sel_ccusto_ativo .

  call method g_grid_ccusto->get_selected_rows
    importing
      et_index_rows = it_selected_rows.

  describe table it_selected_rows lines lines.

  if ( lines is initial ).
    message text-e01 type 'I' display like 'E'.
  else.
    loop at it_selected_rows into wa_selected_rows.
      read table it_saida_ccusto into data(wa_saida_ccusto) index wa_selected_rows-index.
      if sy-subrc eq 0.
        append value #( sign = 'I' option = 'EQ' low = wa_saida_ccusto-kostl ) to p_ccusto.
      endif.
    endloop.
    leave to screen 0.
  endif.

endform.
*&---------------------------------------------------------------------*
*& Form onf4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form onf4 .

  data: it_return like ddshretval occurs 0 with header line.
*        begin of it_user occurs 0,
*          bname type zhcmt0007-bname,
*          cname type zhcmt0007-cname,
*        end of it_user.

  select distinct a~bname, b~cname
    from usr02 as a
    left join zhcmt0007 as b on b~bname eq a~bname
    into table @data(it_user).
  if it_user is not initial.
    sort it_user by bname.
  endif.

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
*     DDIC_STRUCTURE         = ' '
      retfield   = 'BNAME'   "field of internal table
      value_org  = 'S'
    tables
      value_tab  = it_user
*     FIELD_TAB  =
      return_tab = it_return.

  write it_return-fieldval to p_uname.

endform.
*&---------------------------------------------------------------------*
*& Form f_init_alv_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_init_alv_log .

  data: wl_layout type slis_layout_alv.
  data:
    p_text      type sdydo_text_element,
    filtros	    type zif_screen_linha_filtro,
    i_filtros	  type zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) type c,
    v_uzeit(10) type c.


  perform f_fieldcatalog_log.

  variante = value #( report = sy-repid ).

  if g_grid_log is initial.

    clear: i_filtros.
    concatenate sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) into v_datum.
    concatenate sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) into v_uzeit.
    describe table it_saida_alv_log lines data(v_lines).
*    append value #( parametro = 'Periodo:' valor = zv_periodo ) to i_filtros.
*    append value #( parametro = 'Qtde.dias' valor = zvar_dias ) to i_filtros.
*    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
*    append value #( parametro = 'Data:' valor = v_datum ) to i_filtros.
*    append value #( parametro = 'Hora:' valor = v_uzeit ) to i_filtros.
    append value #( parametro = 'Qtde.Registros:' valor = v_lines ) to i_filtros.

  endif.

  p_text = 'Exibir log de acesso por usuario'.

  if zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      exporting
        i_titulo  = conv #( p_text )
        i_filtros = i_filtros
      changing
        split     = dg_splitter_log
        alv       = g_grid_log ) = abap_true.


    w_layout-sel_mode    = 'A'.
    w_layout-col_opt     = abap_true.
    w_layout-cwidth_opt  = abap_true.

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


*    set handler:
*     lcl_event_toolbar=>set_toolbar     for g_grid,
*     lcl_event_toolbar=>get_ucomm       for g_grid.

    call method g_grid_log->set_table_for_first_display
      exporting
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      changing
        it_outtab                     = it_saida_alv_log[]
        it_fieldcatalog               = t_fieldcat
      exceptions
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        others                        = 4.


    call method g_grid_log->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid_log->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    if lines( t_rows ) > 0.
      call method g_grid_log->set_selected_rows
        exporting
          it_index_rows = t_rows.
    endif.

  else.
    call method g_grid->refresh_table_display( is_stable = w_stable ).
  endif.

  wl_layout-colwidth_optimize = 'X'.

endform.
*&---------------------------------------------------------------------*
*& Form f_fieldcatalog_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
form f_fieldcatalog_log .

  free t_fieldcat[].

  perform f_estrutura_alv using:
  01  ''   ''   'IT_SAIDA_ALV_LOG'   'DATA             '           'Data             '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  02  ''   ''   'IT_SAIDA_ALV_LOG'   'HORA             '           'Hora             '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  03  ''   ''   'IT_SAIDA_ALV_LOG'   'SLGUSER          '           'Usuario          '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  04  ''   ''   'IT_SAIDA_ALV_TWO'   'CNAME            '           'Nome do usuario  '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  05  ''   ''   'IT_SAIDA_ALV_TWO'   'FUNCAO           '           'Cargo            '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  06  ''   ''   'IT_SAIDA_ALV_LOG'   'BUKRS            '           'Empresa          '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  07  ''   ''   'IT_SAIDA_ALV_LOG'   'BUTXT            '           'Desc.Empresa     '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  08  ''   ''   'IT_SAIDA_ALV_LOG'   'WERKS            '           'Filial           '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  09  ''   ''   'IT_SAIDA_ALV_LOG'   'WERKSN           '           'Desc.Filial      '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
*  10  ''   ''   'IT_SAIDA_ALV_LOG'   'KOSTL            '           'Centro Custo     '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
*  11  ''   ''   'IT_SAIDA_ALV_LOG'   'CCUSTO           '           'Desc.C.Custo     '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  12  ''   ''   'IT_SAIDA_ALV_LOG'   'SAL_DATA         '           'Transação        '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  12  ''   ''   'IT_SAIDA_ALV_LOG'   'TTEXT            '           'Desc.Transação   '       '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.
endform.

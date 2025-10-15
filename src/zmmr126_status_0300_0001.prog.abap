*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_0300.
*----------------------------------------------------------------------*

class lcl_alv_toolbar_0306 definition deferred.
class lcl_application_0300 definition deferred.
class lcl_event_handler_0306 definition deferred.

types: begin of ty_node_info_logs.
types: node_key    type tv_nodekey,
       item_name   type tv_itmname,
       registro    type zde_log_registro,
       dt_registro type char10,
       hr_registro type char08,
       us_registro type char30.
types: end of ty_node_info_logs.

data: ck_alterado_carga type c length 1,
      ck_alterado_ov    type c length 1,
      ck_alterado_chave type c length 1,
      ck_alterado_nota  type c length 1,
      ck_alterado_gmo   type c length 1,
      vg_tl_0310        type sy-dynnr,
      vg_tl_0302        type sy-dynnr,
      vg_tl_0305        type sy-dynnr,
      vg_tl_0307        type sy-dynnr,
      vg_tl_0312        type sy-dynnr,
      lbl_des_nota      type string value '--------------------------------------------------------------------------------------------------------'.

data: ctl_cccontainer_0306    type ref to cl_gui_custom_container,
      ctl_alv_0306            type ref to cl_gui_alv_grid,
      it_fieldcatalog_0306    type lvc_t_fcat,
      gs_variant_0306         type disvariant,
      gs_layout_0306          type lvc_s_layo,
      event_handler_0306      type ref to lcl_event_handler_0306,
      obg_toolbar_0306        type ref to lcl_alv_toolbar_0306,
      obj_toolbarmanager_0306 type ref to cl_alv_grid_toolbar_manager,
      it_notas_sel            type table of zde_zsdt0001nt_alv with header line,
      gs_scroll_col_0306      type lvc_s_col,
      gs_scroll_row_0306      type lvc_s_roid,
      wa_stable_0306          type lvc_s_stbl,
      nm_field_set_carga      type c length 50,
      nm_field_set_nota       type c length 50,
      pos                     type i.

data: ctl_cccontainer_9012 type ref to cl_gui_custom_container,
      picture_9012         type ref to cl_gui_picture.

data: docking_0300       type ref to cl_gui_docking_container,
      docking_0300_se    type ref to cl_gui_docking_container,
      tree_0300          type ref to cl_gui_column_tree,
      g_application_0300 type ref to lcl_application_0300,
      events_0300        type cntl_simple_events,
      node_table_0300    type treev_ntab,
      item_table_0300    type standard table of mtreeitm,
      it_tree_info_log   type table of ty_node_info_logs with header line.

field-symbols: <fs_fld> type any.

"ZMM0127 Imputar planilha - Auto_Preenchimento - BG #131076 - INICIO
data: t_file  type table of zde_zsdt0001o_plan, " zppe0035,
      wa_file type  zde_zsdt0001o_plan,
      t_tab   type table of alsmex_tabline,
      w_tab   type alsmex_tabline.
data: r_value type rlgrap-filename.
"ZMM0127 Imputar planilha - Auto_Preenchimento - BG #131076 - FIM
constants:
  begin of c_tree_0300,
    column1 type tv_itmname value 'USUARIO',
    column2 type tv_itmname value 'DATA',
    column3 type tv_itmname value 'HORA',
  end of c_tree_0300.

class lcl_application_0300 definition.
  public section.
    methods:
      handle_link_click  for event link_click of cl_gui_column_tree importing node_key item_name.
endclass.                    "LCL_APPLICATION DEFINITION

class lcl_alv_toolbar_0306 definition.
  public section.
    methods: constructor   importing io_alv_grid type ref to cl_gui_alv_grid,
      on_toolbar           for event toolbar of cl_gui_alv_grid importing e_object,
      handle_user_command  for event user_command of cl_gui_alv_grid importing e_ucomm,
      double_click_command for event double_click of cl_gui_alv_grid importing e_row e_column es_row_no.
endclass.

class lcl_event_handler_0306 definition.
  public section.
    methods handle_double_click  for event double_click  of cl_gui_alv_grid importing e_row.
endclass.                    "lcl_event_handler DEFINITION

class lcl_event_handler_0306 implementation.
  method handle_double_click.
    perform handle_double_click_0306 using e_row.
  endmethod.                    "HANDLE_DOUBLE_CLICK
endclass.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_alv_toolbar_0306 implementation.

  method constructor.
*   Create ALV toolbar manager instance
    create object obj_toolbarmanager_0306
      exporting
        io_alv_grid = io_alv_grid.

  endmethod.                    "constructor

  method on_toolbar.

    data: ty_toolbar   type stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

*    "Marcar Todos os Documentos
*    ty_toolbar-icon      = icon_change_text.
*    ty_toolbar-function  = 'EDITAR'.
*    ty_toolbar-quickinfo = text-015.
*    "TY_TOOLBAR-TEXT      = TEXT-015.
*    ty_toolbar-butn_type = 0.
*    append ty_toolbar to e_object->mt_toolbar.
*    clear ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'ELIMINAR'.
    ty_toolbar-quickinfo = text-014.
    "TY_TOOLBAR-TEXT      = TEXT-014.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    call method obj_toolbarmanager_0306->reorganize
      exporting
        io_alv_toolbar = e_object.

  endmethod.                    "on_toolbar

  method handle_user_command.

    data: et_index_rows	type lvc_t_row.

    call method ctl_alv_0306->get_selected_rows
      importing
        et_index_rows = et_index_rows.

    clear: it_notas_sel[].
    loop at et_index_rows into data(wa_index_rows).
      read table it_notas into data(wa_notas_sel) index wa_index_rows-index.
      append wa_notas_sel to it_notas_sel.
    endloop.

    case e_ucomm.
      when 'ELIMINAR'.
        clear: wa_nota_selecionada.

        clear: event_handler_0312a.

        if ctl_alv_0312a is not initial.
          ctl_alv_0312a->free( ).
        endif.
        clear: ctl_alv_0312a.

        clear: obg_toolbar_0312a.

        if ctl_cccontainer_0312a is not initial.
          ctl_cccontainer_0312a->free( ).
        endif.
        clear: ctl_cccontainer_0312a.

        clear: it_takes_saldo[].

        loop at it_notas_sel into wa_notas_sel.
          try .
              objeto->excluir_nota_fiscal( exporting i_nota = wa_notas_sel importing e_notas = it_notas[] ).
                                                            "US143677
              if it_notas[] is not initial.
                read table it_notas into data(w_notas) with key id_nota = 1.
                zde_zsdt0001cg_alv-in_gmo               = w_notas-in_gmo_nt.
                zde_zsdt0001cg_alv-nr_resultado_01      = w_notas-nr_resultado_01_nt.
                zde_zsdt0001cg_alv-nr_resultado_02      = w_notas-nr_resultado_02_nt.
                zde_zsdt0001cg_alv-nr_res_rr1_rr2       = w_notas-nr_res_rr1_rr2_nt.
                zde_zsdt0001cg_alv-in_gmo_03            = w_notas-in_gmo_03_nt.
                zde_zsdt0001cg_alv-in_srr_origem_partic = w_notas-in_srr_origem_partic_nt.
                zde_zsdt0001cg_alv-id_outro_partic      = w_notas-id_outro_partic_nt.
                zde_zsdt0001cg_alv-in_srr_declarado     = w_notas-in_srr_declarado_nt.
                zde_zsdt0001cg_alv-in_teste_srr         = w_notas-in_teste_srr_nt.
                zde_zsdt0001cg_alv-in_srr_declarado_2   = w_notas-in_srr_declarado_2_nt.
                zde_zsdt0001cg_alv-in_teste_srr_2       = w_notas-in_teste_srr_2_nt.
                zde_zsdt0001cg_alv-id_classificadora    = w_notas-id_classificadora_nt.
                zde_zsdt0001cg_alv-tp_transgenia        = w_notas-tp_transgenia_nt.
              endif.
                                                            "US143677
            catch zcx_carga into ex_carga.
              ex_carga->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
          endtry.
        endloop.
        leave to screen 0300.

      when 'EDITAR'.

        clear: wa_nota_selecionada.

        clear: event_handler_0312a.

        if ctl_alv_0312a is not initial.
          ctl_alv_0312a->free( ).
        endif.
        clear: ctl_alv_0312a.

        clear: obg_toolbar_0312a.

        if ctl_cccontainer_0312a is not initial.
          ctl_cccontainer_0312a->free( ).
        endif.
        clear: ctl_cccontainer_0312a.

        clear: it_takes_saldo[].

                                                            "US143677
        read table it_notas_sel index 1 into zde_zsdt0001nt_alv.
        vg_tl_0307 = '0307'.
        clear zde_zsdt0001cg_aux.
        ck_alterado_gmo = abap_false.
        if zde_zsdt0001nt_alv-id_classificacao is initial.
          zde_zsdt0001nt_alv-id_classificacao = zde_zsdt0001nt_alv-nr_nota.
        endif.

        objeto->get_classificacao( importing e_registro = data(r_classificacao_principal) ).
        objeto->get_classificao_notas( exporting i_id_carga = zde_zsdt0001nt_alv-id_carga i_id_classificacao = zde_zsdt0001nt_alv-id_classificacao importing e_classificacao = data(r_classificacao) ).
        if r_classificacao is not initial.
          move-corresponding zde_zsdt0001cg_alv to zde_zsdt0001cg_aux.
          move-corresponding r_classificacao to zde_zsdt0001cg_alv.
        endif.

        call screen 0307 starting at 40 05.
        clear vg_tl_0307.
        objeto->set_classificacao( exporting i_classificacao = r_classificacao_principal ).
        if zde_zsdt0001cg_aux is not initial.
          move-corresponding zde_zsdt0001cg_aux to zde_zsdt0001cg_alv.
        endif.
        leave to screen 0300.
                                                            "US143677

    endcase.
  endmethod. "zm_handle_user_command

  method double_click_command.

    clear: wa_nota_selecionada.


  endmethod.

endclass.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION_0300 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_application_0300 implementation.

  method  handle_link_click.
    perform mostrar_node_log using node_key item_name.
  endmethod.                    "HANDLE_LINK_CLICK

endclass.                    "LCL_APPLICATION IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0300 output.

  data: it_tucomm                type table of sy-ucomm,
        lc_hierarchy_header_0300 type treev_hhdr,
        lc_event_0300            type cntl_simple_event,
        html_pagina              type string,
        html_pagina2             type string.

  clear: it_tucomm.

  case ptipca.
    when zif_carga=>st_tp_carga_entrada_fob.

      try .
          objeto->get_ck_saida_automatica( ).
        catch zcx_carga.
          append 'FATURAMENT' to it_tucomm.
      endtry.

      if objeto->ck_alterou eq abap_false.
        append 'SAVE' to it_tucomm.
      endif.

      if objeto->at_manutencao eq abap_true.
        try .
            objeto->get_ck_sol_ajuste_nao_proc( ).
          catch zcx_carga.    "
            append 'PROCESSSOL' to it_tucomm.
        endtry.
      else.
        append 'PROCESSSOL' to it_tucomm.
      endif.
      if ( zde_zsdt0001cg_alv-id_carga is initial ) or ( objeto->at_manutencao eq abap_true ).
        append 'SHOWLOGS' to it_tucomm.
      endif.

      objeto->get_tp_status( importing e_tp_status = data(e_tp_status) ).

      append 'GMO01' to it_tucomm.
      append 'GMO02' to it_tucomm.

      case e_tp_status.
        when zif_carga=>st_status_aberto.
          append 'ABRIR'      to it_tucomm.
          append 'CONFERIR'   to it_tucomm.
          append 'FATURAMENT' to it_tucomm.
          append 'NOVA_SOLIC' to it_tucomm.
          if zde_zsdt0001cg_alv-id_carga is initial.
            append 'CANCELAR' to it_tucomm.
          endif.
        when zif_carga=>st_status_fechado.
          append 'ADD_NOTA'   to it_tucomm.
          append 'IMP_PLAN'   to it_tucomm.
          append 'FECHAR'     to it_tucomm.
          append 'NOVA_SOLIC' to it_tucomm.
        when zif_carga=>st_status_cancelada.
          append 'ADD_NOTA'   to it_tucomm.
          append 'IMP_PLAN'   to it_tucomm.
          append 'ABRIR'      to it_tucomm.
          append 'FECHAR'     to it_tucomm.
          append 'CONFERIR'   to it_tucomm.
          append 'FATURAMENT' to it_tucomm.
          append 'CANCELAR'   to it_tucomm.
          append 'NOVA_SOLIC' to it_tucomm.
        when zif_carga=>st_status_conferido.
          append 'ADD_NOTA' to it_tucomm.
          append 'IMP_PLAN'   to it_tucomm.
          append 'ABRIR'    to it_tucomm.
          append 'CONFERIR' to it_tucomm.
          if objeto->at_manutencao eq abap_true.
            append 'NOVA_SOLIC' to it_tucomm.
          else.
            append 'CANCELAR' to it_tucomm.
          endif.
      endcase.

      if ck_registro_log eq abap_true.
        append 'ADD_NOTA' to it_tucomm.
        append 'IMP_PLAN'   to it_tucomm.
        append 'ABRIR'    to it_tucomm.
        append 'FECHAR'   to it_tucomm.
        append 'CONFERIR' to it_tucomm.
        append 'CANCELAR' to it_tucomm.
        append 'FATURAMENT' to it_tucomm.
        "Registro Log: Data: &1 Hora: &2 Usuário: &3
        set titlebar 'TL0301' with it_tree_info_log-dt_registro it_tree_info_log-hr_registro it_tree_info_log-us_registro.
      elseif pmanut eq abap_true.
        set titlebar 'TL0302'.
      else.
        set titlebar 'TL0300'.
      endif.

    when zif_carga=>st_tp_carga_saida_opus.
      append 'IMP_PLAN'   to it_tucomm.
      append 'SAVE' to it_tucomm.
      append 'PROCESSSOL' to it_tucomm.
      append 'SHOWLOGS'   to it_tucomm.
      append 'GMO01'      to it_tucomm.
      append 'GMO02'      to it_tucomm.
      append 'ABRIR'      to it_tucomm.
      append 'CONFERIR'   to it_tucomm.
      append 'NOVA_SOLIC' to it_tucomm.
      append 'CANCELAR'   to it_tucomm.
      append 'ADD_NOTA'   to it_tucomm.
      append 'FECHAR'     to it_tucomm.
      append 'FATURAMENT' to it_tucomm.
      set titlebar 'TL0300'.
    when zif_carga=>st_tp_carga_saida_ent_fob.

      append 'IMP_PLAN'   to it_tucomm.
      append 'SAVE'       to it_tucomm.
      append 'PROCESSSOL' to it_tucomm.
      append 'SHOWLOGS'   to it_tucomm.
      append 'GMO01'      to it_tucomm.
      append 'GMO02'      to it_tucomm.
      append 'ABRIR'      to it_tucomm.
      append 'CONFERIR'   to it_tucomm.
      append 'NOVA_SOLIC' to it_tucomm.
      append 'CANCELAR'   to it_tucomm.
      append 'ADD_NOTA'   to it_tucomm.
      append 'FECHAR'     to it_tucomm.

      try .
          objeto->get_ck_saida_automatica( ).
        catch zcx_carga.
          append 'FATURAMENT' to it_tucomm.
      endtry.

      set titlebar 'TL0300'.
  endcase.

  if not ( zde_zsdt0001cg_alv-tp_status = zif_carga=>st_status_fechado or zde_zsdt0001cg_alv-tp_status = zif_carga=>st_status_conferido ).
    append 'FTENTRADA' to it_tucomm.
  endif.

  set pf-status 'PF0300' excluding it_tucomm.

  if zde_zsdt0001acb_alv-tp_solicitacao_status eq zif_carga=>st_status_manut_aberto or
     zde_zsdt0001acb_alv-tp_solicitacao_status eq zif_carga=>st_status_manut_cancelada or
     zde_zsdt0001acb_alv-tp_solicitacao_status eq zif_carga=>st_status_manut_aprovado or
     zde_zsdt0001acb_alv-tp_solicitacao_status eq zif_carga=>st_status_manut_enviado or
     zde_zsdt0001acb_alv-tp_solicitacao_status eq zif_carga=>st_status_manut_recusada.

    perform retornar_html_workflow using zde_zsdt0001acb_alv changing html_pagina.

    if docking_0300_se is initial.
      create object docking_0300_se
        exporting
          repid     = sy-repid
          dynnr     = sy-dynnr
          side      = cl_gui_docking_container=>dock_at_right
          extension = 400.

      cl_abap_browser=>show_html(
        exporting
          html_string = html_pagina
          modal       = abap_false
          format      = cl_abap_browser=>landscape
          size        = cl_abap_browser=>medium
          container   = docking_0300_se ).
    else.
      cl_abap_browser=>close_browser( ).
      cl_abap_browser=>show_html(
        exporting
          html_string = html_pagina
          modal       = abap_false
          format      = cl_abap_browser=>landscape
          size        = cl_abap_browser=>medium
          container   = docking_0300_se ).
    endif.

  elseif zde_zsdt0001cg_alv-id_carga is not initial and ck_mostrar_logs eq abap_false and docking_0300 is initial
     and zde_zsdt0001cg_alv-tp_status = zif_carga=>st_status_conferido.

    create object docking_0300
      exporting
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = docking_0300->dock_at_right
        extension = 400.

    perform cria_alv_documentos using 2 docking_0300->screen0 docking_0300.

    perform atualiza_tree using zde_zsdt0001cg_alv-id_carga.

  elseif zde_zsdt0001cg_alv-id_carga is not initial and ck_mostrar_logs eq abap_true and docking_0300 is initial.

    create object docking_0300
      exporting
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = docking_0300->dock_at_right
        extension = 400.

    lc_hierarchy_header_0300-heading = text-016.
    lc_hierarchy_header_0300-width   = 31.

    clear: events_0300.
    lc_event_0300-eventid    = cl_gui_column_tree=>eventid_link_click.
    lc_event_0300-appl_event = 'X'.
    append lc_event_0300 to events_0300.

    create object g_application_0300.

    create object tree_0300
      exporting
        parent                = docking_0300
        node_selection_mode   = tree_0300->node_sel_mode_single
        item_selection        = 'X'
        hierarchy_column_name = c_tree_0300-column1
        hierarchy_header      = lc_hierarchy_header_0300.

    tree_0300->set_registered_events( exporting  events                    = events_0300
                                      exceptions cntl_error                = 1
                                                 cntl_system_error         = 2
                                                 illegal_event_combination = 3
                                                 others                    = 4 ).

    if sy-subrc is not initial.
      message id sy-msgid type sy-msgty number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    endif.

    set handler g_application_0300->handle_link_click for tree_0300.

    call method tree_0300->add_column
      exporting
        name        = c_tree_0300-column2
        width       = 20
        header_text = text-017
        alignment   = cl_gui_column_tree=>align_center.

    call method tree_0300->add_column
      exporting
        name        = c_tree_0300-column3
        width       = 20
        header_text = text-018
        alignment   = cl_gui_column_tree=>align_center.

    perform atualiza_tree_logs.

  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0300_exit input.

  data: answer type c.

  if ok_code eq 'CHNFE'.
    perform informar_chave_nfe.
  else.
    answer = '1'.
    if objeto->ck_alterou eq abap_true.
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = text-001
          text_question         = text-002
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        importing
          answer                = answer
        exceptions
          text_not_found        = 1
          others                = 2.

      if sy-subrc is not initial.
        exit.
      endif.
    endif.
    check answer eq '1'.
    perform limpa_tela_0300.
    leave to screen 0.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0300 input.

  data: lc_titlebar      type string,
        lc_text_question type string.

  case ok_code.
    when 'FATURAMENT'.
      clear: ok_code.
      perform faturamento_9011.

    when 'PROCESSSOL'.

      clear: ok_code.
      try .
          objeto->set_aceite_soli_manutencao( exporting i_tp_aprovacao = 'XX' i_tp_resposta = 'X' ).
        catch zcx_carga into ex_carga.
          ex_carga->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        catch zcx_parceiros into ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        catch zcx_ordem_venda into ex_ordem_venda.    "
          ex_ordem_venda->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        catch zcx_ordem_carregamento into ex_ordem.    "
          ex_ordem->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        catch zcx_cadastro into ex_cadastro.    "
          ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        catch zcx_pedido_compra_exception into ex_pedido.    "
          ex_pedido->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        catch zcx_job into ex_job.    "
          ex_job->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
        catch zcx_miro_exception into ex_miro.    "
          ex_miro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
      endtry.

    when 'SAVE'.
      clear: ok_code.

      case objeto->at_manutencao.
        when abap_false.
          lc_titlebar      = text-020.
          lc_text_question = text-021.
        when abap_true.
          lc_titlebar      = text-026.
          lc_text_question = text-027.
      endcase.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        importing
          answer                = answer
        exceptions
          text_not_found        = 1
          others                = 2.

      check answer eq '1'.

      try .
          objeto->gravar_registro(
               )->get_info_alv_apresentacao( importing e_apresentacao = data(e_apresentacao)
               ).

          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].

        catch zcx_carga into ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_carga->msgid ex_carga->msgno.
        catch zcx_parceiros into ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_parceiros->msgid ex_parceiros->msgno.
        catch zcx_ordem_venda into ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_ordem_venda->msgid ex_ordem_venda->msgno.
      endtry.

    when 'ABRIR'.
      clear: ok_code.

      case objeto->at_manutencao.
        when abap_false.
          lc_titlebar      = text-022.
          lc_text_question = text-023.
        when abap_true.
          lc_titlebar      = text-028.
          lc_text_question = text-029.
      endcase.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        importing
          answer                = answer
        exceptions
          text_not_found        = 1
          others                = 2.

      check answer eq '1'.

      try .
          objeto->set_abrir( importing e_carga_recebimento = zde_zsdt0001cg_alv
                                                             )->get_info_alv_apresentacao( importing e_apresentacao = e_apresentacao ).

          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].

          perform limpa_tela_0300.
        catch zcx_carga into ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_ordem_carregamento into ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_parceiros into ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_ordem_venda into ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_soft_expert_workflow into ex_soft_expert_workflow.
          ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_job into ex_job.
          ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      endtry.

    when 'CANCELAR'.

      clear: ok_code.

      case objeto->at_manutencao.
        when abap_false.
          lc_titlebar      = text-024.
          lc_text_question = text-025.
        when abap_true.
          lc_titlebar      = text-030.
          lc_text_question = text-031.
      endcase.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        importing
          answer                = answer
        exceptions
          text_not_found        = 1
          others                = 2.

      check answer eq '1'.

      try .
          objeto->set_cancelar(
               )->get_info_alv_apresentacao( importing e_apresentacao = e_apresentacao
               ).

          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].

          perform limpa_tela_0300.
        catch zcx_carga into ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_parceiros into ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_ordem_carregamento into ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_ordem_venda into ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      endtry.

    when 'FECHAR'.

      clear: ok_code.

      case objeto->at_manutencao.
        when abap_false.
          lc_titlebar      = text-101.
          lc_text_question = text-102.
        when abap_true.
          lc_titlebar      = text-032.
          lc_text_question = text-033.
      endcase.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        importing
          answer                = answer
        exceptions
          text_not_found        = 1
          others                = 2.

      check answer eq '1'.

      try .

          objeto->verif_saldo_ordem_venda( ).

          objeto->verif_ticket_pesagem( ).

          objeto->verif_peso_notas( ).

          objeto->set_fechar( importing e_fechou = data(e_fechou)
               )->get_info_alv_apresentacao( importing e_apresentacao = e_apresentacao
               ).

          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].

          if not e_fechou eq abap_true.
            if sy-msgid eq 'ZCARGA'.
              case sy-msgno.
                when 039.
                  set cursor field 'ZDE_ZSDT0001CG_ALV-ID_ENTRADA'.
              endcase.
            endif.
            exit.
          else.

            zde_zsdt0001cg_alv-id_carga = objeto->carga-id_carga.

            if pmanut eq abap_true.

              zde_zsdt0001acb_alv-id_solicitacao = objeto->solicitacao_manutencao-id_solicitacao.

              objeto->free(
                )->limpar_registro(
                )->set_registro_manutencao( i_id_solicitacao = zde_zsdt0001acb_alv-id_solicitacao
                )->get_info_alv_apresentacao( importing e_apresentacao = e_apresentacao
                ).

            else.

              objeto->free(
                )->limpar_registro(
                )->set_registro( i_id_carga = zde_zsdt0001cg_alv-id_carga
                )->get_info_alv_apresentacao( importing e_apresentacao = e_apresentacao
 ).
              "
              select *
                "BUG133332 Ajuste ZMM0127 -  parametrizar user pula conf - BG -- INICIO
                  from tvarvc
                  into table @data(t_tvarvc)
                 where name = 'MAGGI_USER_RPA'.

              if sy-subrc is initial and t_tvarvc is not initial.
                read table t_tvarvc into data(v_starv) with key low = sy-uname.

                if sy-subrc is initial. "zde_zsdt0001cg_alv-id_local_entrega  = '418'. "CS2023000641 = LC 418-RPA ROBÔ
                  "BUG133332 Ajuste ZMM0127 -  parametrizar user pula conf - BG -- FIM
                  try .
                      objeto->set_conferido(
                        exporting
                          i_proximo_passo_automatico = ck_saida_automatica
                        importing
                          e_conferiu                 = data(e_conferiu_) ).

                      if not e_conferiu_ eq abap_true.
                        exit.
                      endif.

                      objeto->get_registro( importing e_registro = data(e_registro_) ).

                      set parameter id 'ZIDCARGA' field e_registro_-id_carga.

                    catch zcx_carga into ex_carga.
                      ex_carga->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
                      exit.

                    catch zcx_parceiros into ex_parceiros.
                      ex_parceiros->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
                      exit.

                    catch zcx_ordem_venda into ex_ordem_venda.
                      ex_ordem_venda->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
                      exit.

                    catch zcx_job into ex_job.
                      ex_job->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
                      exit.

                    catch zcx_cadastro into ex_cadastro.
                      ex_cadastro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
                      exit.

                    catch zcx_pedido_compra_exception into ex_pedido.
                      ex_pedido->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
                      exit.

                    catch zcx_miro_exception into ex_miro.
                      ex_miro->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
                      exit.

                    catch zcx_ordem_carregamento into ex_ordem.
                      ex_ordem->published_erro( i_msgty = 'I' i_msgty_display = 'E' ).
                      exit.

                  endtry.

                  ck_conferiu = abap_true.
                endif.
              endif.

            endif.

          endif.
          perform limpa_tela_0300.

        catch zcx_carga into ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_carga->msgid ex_carga->msgno.
          exit.
        catch zcx_ordem_carregamento into ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_ordem->msgid ex_ordem->msgno.
          exit.
        catch zcx_cadastro into ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_cadastro->msgid ex_cadastro->msgno.
          exit.
        catch zcx_parceiros into ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_parceiros->msgid ex_parceiros->msgno.
          exit.
        catch zcx_ordem_venda into ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_ordem_venda->msgid ex_ordem_venda->msgno.
          exit.
        catch zcx_soft_expert_workflow into ex_soft_expert_workflow.
          ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          perform seta_campo using ex_soft_expert_workflow->msgid ex_soft_expert_workflow->msgno.
          exit.
        catch zcx_job into ex_job.
          ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          exit.
      endtry.

    when 'CONFERIR'.

      clear: ok_code.

      case objeto->at_manutencao.
        when abap_false.
          lc_titlebar      = text-103.
          lc_text_question = text-104.
        when abap_true.
          lc_titlebar      = text-034.
          lc_text_question = text-035.
      endcase.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        importing
          answer                = answer
        exceptions
          text_not_found        = 1
          others                = 2.

      check answer eq '1'.

      perform conferir_carga.

      if ck_conferiu eq abap_true.
        ck_confer_carga = abap_true.
        leave to screen 0.
      endif.
      "Imputar planilha - Auto_Preenchimento - BG #131076
    when 'IMP_PLAN'.

      perform z_popup.
      perform get_excel.
      perform f_carrega_arquivo.
      perform f_processa_arquivo.
      clear ok_code.
    when 'ADD_NOTA'.

      clear: ok_code.
      clear: zde_zsdt0001nt_alv.

      clear vg_tl_0307.  "BUG174959                                                   "US143677
*      vg_tl_0307 = '0307'.
      objeto->get_classificacao( importing e_registro = data(r_classificacao_principal) ).
      move-corresponding zde_zsdt0001cg_alv to zde_zsdt0001cg_aux.
      call screen 0307 starting at 20 09.
      clear vg_tl_0307.
      objeto->set_classificacao( exporting i_classificacao = r_classificacao_principal ).
      move-corresponding zde_zsdt0001cg_aux to zde_zsdt0001cg_alv.
                                                            "US143677
    when 'SHOWLOGS'.
      clear: ok_code.
      if ck_mostrar_logs eq abap_true.
        ck_mostrar_logs = abap_false.
      else.
        ck_mostrar_logs = abap_true.
      endif.
      perform limpa_tela_0300.

    when 'NOVA_SOLIC'.
      "Solicitar Manutenação
      clear: ok_code.

      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = text-105
          text_question         = text-106
          text_button_1         = text-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = text-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        importing
          answer                = answer
        exceptions
          text_not_found        = 1
          others                = 2.

      check answer eq '1'.

      submit zmmr126_0001
                     with pck_cad  eq abap_true
                     with pmanut   eq abap_true
                     with psafra   eq psafra
                     with pempre   eq pempre
                     with pfilia   eq pfilia
                     with pidcarga eq objeto->carga-id_carga and return.

    when 'FTENTRADA'.
      perform emitir_frete_entrada_carga in program zmmr155 using objeto->carga-id_carga if found.

  endcase.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0302  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0302 output.

*-CS2021000253-26.04.2024-#59941-JT comentado a pedido de Leo
* RJF - Ini - 2023.03 -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
  vg_text = 'Ord.Carregamento'(044).
* CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT
*  READ TABLE it_ordens_venda INTO DATA(wa_ordem_vendax) INDEX 1.
*  IF zde_zsdt0001ov_alv-nr_ordem_venda IS NOT INITIAL.
** Documento de vendas: dados comerciais - VBKD-INCO1
*    SELECT vbeln, inco1
*      FROM vbkd
*      INTO TABLE @DATA(it_vbkd)
*      WHERE vbeln EQ @zde_zsdt0001ov_alv-nr_ordem_venda.
*
*    IF sy-subrc IS INITIAL.
*      LOOP AT it_vbkd ASSIGNING FIELD-SYMBOL(<fs_vbkd>).
*        IF <fs_vbkd>-inco1 EQ 'CPT'. " Modalidade CPT
*          vg_ord_ext = abap_on.
*          vg_text = 'Ord.Carreg.Exter'(045).
*          EXIT.
*        ELSE.
*          FREE vg_ord_ext.
*          vg_text = 'Ord.Carregamento'(044).
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
*  ELSE.
*    vg_text = 'Ord.Carregamento'(044).
*  ENDIF.
** RJF - Fim - 2023.03 -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
*-CS2021000253-26.04.2024-#59941-JT comentado a pedido de Leo

  loop at screen.
    split screen-name at '-' into: data(str1a) data(str2a).
    if str1a eq 'ZDE_ZSDT0001CG_ALV' or str1a eq 'ZDE_ZSDT0001OD_ALV' or str1a eq 'ZDE_ZSDT0001OV_ALV'.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( exporting i_campo = conv #( str2a ) importing e_permitido = data(e_permitido_0302) ).
      if e_permitido_0302 eq abap_false.
        screen-input = 0.
        modify screen.
      endif.
    endif.
  endloop.

*  IF ZDE_ZSDT0001CG_ALV-ID_PRODUTO IS INITIAL.
*    VG_TL_0310 = '9012'.
*  ELSE.
*    SELECT SINGLE * INTO @WA_MARA
*      FROM MARA
*     WHERE MATNR EQ @ZDE_ZSDT0001CG_ALV-ID_PRODUTO.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT  = WA_MARA-MATKL
*      IMPORTING
*        OUTPUT = WA_MARA-MATKL.
*
*    IF WA_MARA-MATKL EQ ZIF_CARGA=>ST_GRUPO_ALGODAO_PLUMA. "Algodão
*      VG_TL_0310 = '0311'.
*    ELSE.
  vg_tl_0310 = '0310'.
*    ENDIF.
*  ENDIF.

  if vg_tl_0302 is initial.
    vg_tl_0302 = '0309'.
  endif.

  describe table it_notas lines data(qtd_notas).
  if qtd_notas eq 1.
    if zde_zsdt0001nt_alv is initial.
      read table it_notas index 1.
      zde_zsdt0001nt_alv = it_notas.
    endif.
    vg_tl_0305 = '0305'.
  elseif qtd_notas eq 0.
    vg_tl_0305 = '0305'.
  else.
    vg_tl_0305 = '0306'.
  endif.

  if nm_field_set_carga is not initial.
    set cursor field nm_field_set_carga offset pos.
    clear: nm_field_set_carga.
  endif.

  describe table it_ordens_venda lines qtd_notas.
  if qtd_notas eq 1.
    read table it_ordens_venda index 1.
    zde_zsdt0001ov_alv = it_ordens_venda.
  endif.

  describe table it_pedido_compra lines qtd_notas.
  if qtd_notas eq 1.
    read table it_pedido_compra index 1.
    move-corresponding it_pedido_compra to zde_zsdt0001ov_alv.
    zde_zsdt0001ov_alv-nr_ordem_venda = it_pedido_compra-nr_pedido_compra.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_NR_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module validar_nr_ordem input.

  condense zde_zsdt0001cg_alv-nr_ordem no-gaps.

  try .
*-CS2021000253-26.04.2024-#59941-JT comentado a pedido de Leo
*     IF vg_ord_ext IS INITIAL. "RJF - CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT - -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
*-CS2021000253-26.04.2024-#59941-JT comentado a pedido de Leo
      objeto->set_ordem_carregamento(
        exporting
          i_nr_safra          = zde_zsdt0001cg_alv-nr_safra
          i_id_bukrs          = zde_zsdt0001cg_alv-id_bukrs
          i_id_branch         = zde_zsdt0001cg_alv-id_branch
          i_nr_ordem          = zde_zsdt0001cg_alv-nr_ordem
        importing
          e_ordem_carrgamento = zde_zsdt0001od_alv
        changing
          i_carga_alv         = zde_zsdt0001cg_alv ).

*-CS2021000253-26.04.2024-#59941-JT comentado a pedido de Leo
** RJF - Ini - CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT - -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
*      ELSE.
*        objeto->set_ordem_carregamento_ext(
*        EXPORTING
*          i_nr_safra  = zde_zsdt0001cg_alv-nr_safra
*          i_id_bukrs  = zde_zsdt0001cg_alv-id_bukrs
*          i_id_branch = zde_zsdt0001cg_alv-id_branch
*          i_nr_ordem  = zde_zsdt0001cg_alv-nr_ordem
*      CHANGING
*        i_carga_alv = zde_zsdt0001cg_alv ).
*      ENDIF.
* RJF - Fim - CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT - -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
*-CS2021000253-26.04.2024-#59941-JT comentado a pedido de Leo

    catch zcx_ordem_carregamento into data(ex_ordem_carregamento).
      ex_ordem_carregamento->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    catch zcx_parceiros into ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  endtry.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_CARGA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module atribui_info_carga input.

  try .
      objeto->set_transgenia(
        changing
          i_in_gmo               = zde_zsdt0001cg_alv-in_gmo
          i_nr_resultado_01      = zde_zsdt0001cg_alv-nr_resultado_01
          i_nr_resultado_02      = zde_zsdt0001cg_alv-nr_resultado_02
*         i_nr_resultado_03      = zde_zsdt0001cg_alv-nr_resultado_03 "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
          "i_nr_resultado_04       = zde_zsdt0001cg_alv-nr_resultado_04 "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
          "i_nr_fita               = zde_zsdt0001cg_alv-nr_fita         "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
          i_nr_res_rr1_rr2       = zde_zsdt0001cg_alv-nr_res_rr1_rr2
          i_in_gmo_03            = zde_zsdt0001cg_alv-in_gmo_03
          i_in_srr_origem_partic = zde_zsdt0001cg_alv-in_srr_origem_partic
          i_id_outro_partic      = zde_zsdt0001cg_alv-id_outro_partic
          i_in_srr_declarado     = zde_zsdt0001cg_alv-in_srr_declarado
          i_in_teste_srr         = zde_zsdt0001cg_alv-in_teste_srr
          i_in_srr_declarado_2   = zde_zsdt0001cg_alv-in_srr_declarado_2
          i_in_teste_srr_2       = zde_zsdt0001cg_alv-in_teste_srr_2
          i_id_classificadora    = zde_zsdt0001cg_alv-id_classificadora
          i_id_ck_class_dest     = zde_zsdt0001cg_alv-ck_class_dest
          i_tp_transgenia        = zde_zsdt0001cg_alv-tp_transgenia ).
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  endtry.

  ck_alterado_carga = abap_true.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0302  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0302 input.

  if ck_alterado_carga eq abap_true.
    try.
        objeto->set_carga( exporting i_carga = zde_zsdt0001cg_alv importing e_carga_recebimento = zde_zsdt0001cg_alv ).
        clear: ck_alterado_carga.
      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        perform seta_campo using ex_carga->msgid ex_carga->msgno.
    endtry.
  endif.

  case ok_code.
    when 'GMO01'.
      clear: ok_code.
      call screen 0303 starting at 20 15.
    when 'GMO02'.
      clear: ok_code.
      call screen 0304 starting at 20 15.
    when 'GMO'.
      clear: ok_code.
  endcase.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0303 input.

  if ck_alterado_carga eq abap_true.
    try.
        objeto->set_carga( exporting i_carga = zde_zsdt0001cg_alv importing e_carga_recebimento = zde_zsdt0001cg_alv ).
      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    endtry.
  endif.

  case ok_code.
    when 'CONFIRMAR'.
      clear: ok_code.
      check ck_alterado_carga eq abap_false.
      leave to screen 0.
    when 'GMO'.
      clear: ok_code.
  endcase.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0303  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0303 output.
  set pf-status 'TL0303'.
  set titlebar 'TL0303'.

  loop at screen.
    split screen-name at '-' into: str1a str2a.
    if str1a eq 'ZDE_ZSDT0001CG_ALV' or str1a eq 'ZDE_ZSDT0001OD_ALV'.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( exporting i_campo = conv #( str2a ) importing e_permitido = data(e_permitido_0303) ).
      if e_permitido_0303 eq abap_false.
        screen-input = 0.
        modify screen.
      endif.
    endif.
  endloop.

  clear: ck_alterado_carga.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0303_exit input.
  leave to screen 0.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0304  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0304 output.
  set pf-status 'TL0303'.
  set titlebar 'TL0304'.

  loop at screen.
    split screen-name at '-' into: str1a str2a.
    if str1a eq 'ZDE_ZSDT0001CG_ALV' or str1a eq 'ZDE_ZSDT0001OD_ALV'.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( exporting i_campo = conv #( str2a ) importing e_permitido = data(e_permitido_0304) ).
      if e_permitido_0304 eq abap_false.
        screen-input = 0.
        modify screen.
      endif.
    endif.
  endloop.

  clear: ck_alterado_carga.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0304 input.

  if ck_alterado_carga eq abap_true.
    try.
        objeto->set_carga( exporting i_carga = zde_zsdt0001cg_alv importing e_carga_recebimento = zde_zsdt0001cg_alv ).
      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    endtry.
  endif.

  case ok_code.
    when 'CONFIRMAR'.
      clear: ok_code.
      check ck_alterado_carga eq abap_false.
      leave to screen 0.
  endcase.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  PERC_01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module perc_01 input.

  try.
      objeto->set_carga( exporting i_carga = zde_zsdt0001cg_alv importing e_carga_recebimento = zde_zsdt0001cg_alv ).
      clear: ck_alterado_carga.
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  endtry.

  perform perc_resultado using '1' zde_zsdt0001cg_alv-nr_perc_umi
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                      changing zde_zsdt0001cg_alv-nr_qtde_umi.
  objeto->ck_digitado_umidade = abap_true.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  PERC_02  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module perc_02 input.

  perform perc_resultado using '2' zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                      changing zde_zsdt0001cg_alv-nr_qtde_imp.

  objeto->ck_digitado_impureza = abap_true.

  perform perc_resultado using '1' zde_zsdt0001cg_alv-nr_perc_umi
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                      changing zde_zsdt0001cg_alv-nr_qtde_umi.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  PERC_03  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module perc_03 input.

  data: lc_ck_avariado type char01.

  if zde_zsdt0001cg_alv-nr_perc_ava is initial.
    clear: zde_zsdt0001cg_alv-nr_perc_ava_arq,
           zde_zsdt0001cg_alv-nr_perc_ava_que,
           zde_zsdt0001cg_alv-nr_perc_ava_mof,
           zde_zsdt0001cg_alv-nr_perc_ava_pic,
           zde_zsdt0001cg_alv-nr_perc_ava_fer,
           zde_zsdt0001cg_alv-nr_perc_ava_ger,
           zde_zsdt0001cg_alv-nr_perc_ava_ard,
           zde_zsdt0001cg_alv-nr_perc_ava_ges.
    lc_ck_avariado = abap_true.
  else.
    perform atribui_perc_sub_avariado using zde_zsdt0001cg_alv-nr_perc_ava changing lc_ck_avariado.
  endif.

  if lc_ck_avariado eq abap_true.
    perform perc_resultado using '3' zde_zsdt0001cg_alv-nr_perc_ava
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            changing zde_zsdt0001cg_alv-nr_qtde_ava.
  else.
    message e236.
  endif.
  objeto->ck_digitado_avariado = abap_true.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  PERC_04  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module perc_04 input.
  perform perc_resultado using '4' zde_zsdt0001cg_alv-nr_perc_ard
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                          changing zde_zsdt0001cg_alv-nr_qtde_ard.
  objeto->ck_digitado_ardido = abap_true.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  PERC_05  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module perc_05 input.
  perform perc_resultado using '5' zde_zsdt0001cg_alv-nr_perc_que
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                          changing zde_zsdt0001cg_alv-nr_qtde_que.
  objeto->ck_digitado_quebrado = abap_true.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  PERC_06  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module perc_06 input.
  perform perc_resultado using '6' zde_zsdt0001cg_alv-nr_perc_esv
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                          changing zde_zsdt0001cg_alv-nr_qtde_esv.
  objeto->ck_digitado_esverdeado = abap_true.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  PERC_07  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module perc_07 input.
  perform perc_resultado using '7' zde_zsdt0001cg_alv-nr_perc_car
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                          changing zde_zsdt0001cg_alv-nr_qtde_car.
  objeto->ck_digitado_carunchado = abap_true.
endmodule.

*&---------------------------------------------------------------------*
*&      Form  PERC_RESULTADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0501   text
*----------------------------------------------------------------------*
form perc_resultado  using p_tipo       type char01
                           p_percentual type zde_nr_perc_class_com
                           p_perc_imp   type zde_nr_perc_impureza
                           p_subtotal   type zde_nm_peso_subtotal
                  changing p_retorno type zde_nr_qtde_umidade.

  "DATA: OBJ_RECEBIMENTO TYPE REF TO ZIF_CARGA.
  data: i_classificacao	type zde_pes_resultado_class.
  i_classificacao-centro = zde_zsdt0001cg_alv-id_branch.

  call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
    exporting
      input  = zde_zsdt0001cg_alv-id_produto
    importing
      output = i_classificacao-material.

  i_classificacao-safra              = zde_zsdt0001cg_alv-nr_safra.
  i_classificacao-caracteristica     = p_tipo.

  p_retorno = 0.
  check p_subtotal is not initial.
  check zde_zsdt0001cg_alv-id_produto is not initial.

  write p_percentual to i_classificacao-percentual.
  write p_perc_imp   to i_classificacao-percentualimpureza.
  write p_subtotal   to i_classificacao-subtotal.
  condense i_classificacao-percentual         no-gaps.
  condense i_classificacao-percentualimpureza no-gaps.
  condense i_classificacao-subtotal           no-gaps.

  try .

      zcl_factory_carga=>zif_factory_carga~get_instance(
        )->set_factory_objeto( exporting i_tp_carga = ptipca i_tp_produto = zif_carga=>st_tp_produto_carga_granel
        )->get_factory_objeto(
        )->get_result_desc_classificacao( exporting i_classificacao = i_classificacao importing e_resultado = data(r_resultado)
        )->free(
        ).

      p_retorno = r_resultado-desconto-valordesconto.

    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  endtry.

endform.

*&---------------------------------------------------------------------*
*&      Module  ALTERA_BRUTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module altera_bruto input.
  perform ajusta_descontos.
endmodule.

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_DESCONTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ajusta_descontos .

  if not ( zde_zsdt0001cg_alv-nr_perc_umi ne 0 or zde_zsdt0001cg_alv-nr_perc_imp ne 0 or zde_zsdt0001cg_alv-nr_perc_ava ne 0
  or zde_zsdt0001cg_alv-nr_perc_ard ne 0 or zde_zsdt0001cg_alv-nr_perc_que ne 0 or zde_zsdt0001cg_alv-nr_perc_esv ne 0 ).
    exit.
  endif.

  if zde_zsdt0001cg_alv-nr_perc_umi ne 0.
    perform perc_resultado using '1' zde_zsdt0001cg_alv-nr_perc_umi
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                        changing zde_zsdt0001cg_alv-nr_qtde_umi.
  endif.

  if zde_zsdt0001cg_alv-nr_perc_imp ne 0.
    perform perc_resultado using '2' zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            changing zde_zsdt0001cg_alv-nr_qtde_imp.
  endif.

  if zde_zsdt0001cg_alv-nr_perc_ava ne 0.
    perform perc_resultado using '3' zde_zsdt0001cg_alv-nr_perc_ava
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            changing zde_zsdt0001cg_alv-nr_qtde_ava.
  endif.

  if zde_zsdt0001cg_alv-nr_perc_ard ne 0.
    perform perc_resultado using '4' zde_zsdt0001cg_alv-nr_perc_ard
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            changing zde_zsdt0001cg_alv-nr_qtde_ard.
  endif.

  if zde_zsdt0001cg_alv-nr_perc_que ne 0.
    perform perc_resultado using '5' zde_zsdt0001cg_alv-nr_perc_que
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            changing zde_zsdt0001cg_alv-nr_qtde_que.
  endif.

  if zde_zsdt0001cg_alv-nr_perc_esv ne 0.
    perform perc_resultado using '6' zde_zsdt0001cg_alv-nr_perc_esv
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            changing zde_zsdt0001cg_alv-nr_qtde_esv.
  endif.

  if zde_zsdt0001cg_alv-nr_perc_car ne 0.
    perform perc_resultado using '7' zde_zsdt0001cg_alv-nr_perc_car
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            changing zde_zsdt0001cg_alv-nr_qtde_car.
  endif.

  try.
      objeto->set_carga( exporting i_carga = zde_zsdt0001cg_alv importing e_carga_recebimento = zde_zsdt0001cg_alv ).
      clear: ck_alterado_carga.
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  endtry.

endform.

*&---------------------------------------------------------------------*
*&      Module  ALTERA_TARA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module altera_tara input.
  perform ajusta_descontos.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  SET_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_ordem input.

  try .
      data(lc_id_produto) = zde_zsdt0001cg_alv-id_produto.

      objeto->set_ordem_venda( exporting i_ordem_venda        = zde_zsdt0001ov_alv-nr_ordem_venda
                               importing e_carga              = data(lc_saida_carga)
                               changing  c_zde_zsdt0001ov_alv = zde_zsdt0001ov_alv
                                       ).

      zde_zsdt0001cg_alv-id_produto        = lc_saida_carga-id_produto.
      zde_zsdt0001cg_alv-ds_produto        = lc_saida_carga-ds_produto.
      zde_zsdt0001cg_alv-id_local_coleta   = lc_saida_carga-id_local_coleta.
      zde_zsdt0001cg_alv-id_local_descarga = lc_saida_carga-id_local_descarga.
      zde_zsdt0001cg_alv-id_local_destino  = lc_saida_carga-id_local_destino.
      zde_zsdt0001cg_alv-id_agent_frete    = lc_saida_carga-id_agent_frete.
      zde_zsdt0001cg_alv-ds_local_coleta   = lc_saida_carga-ds_local_coleta.
      zde_zsdt0001cg_alv-ds_local_descarga = lc_saida_carga-ds_local_descarga.
      zde_zsdt0001cg_alv-ds_local_destino  = lc_saida_carga-ds_local_destino.
      zde_zsdt0001cg_alv-ds_agent_frete    = lc_saida_carga-ds_agent_frete.
      zde_zsdt0001cg_alv-in_transferencia  = lc_saida_carga-in_transferencia.
      zde_zsdt0001cg_alv-ck_gera_aviso     = lc_saida_carga-ck_gera_aviso.
      zde_zsdt0001cg_alv-ck_frete_entrada  = lc_saida_carga-ck_frete_entrada.

      if lc_id_produto ne zde_zsdt0001cg_alv-id_produto.
        perform ajusta_descontos.
      endif.

    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    catch zcx_parceiros into ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  endtry.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0305  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0305 output.
  "BREAK-POINT.

  if zde_zsdt0001nt_alv-id_mod_fiscal is initial.
    zde_zsdt0001nt_alv-id_mod_fiscal = zif_carga=>st_model_fiscal_papel.
  endif.

  loop at screen.
    split screen-name at '-' into: str1a str2a.
    if str1a eq 'ZDE_ZSDT0001NT_ALV'.

      objeto->valida_atributo_alteravel(
        exporting
          i_campo         = conv #( str2a )
          i_modelo_fiscal = zde_zsdt0001nt_alv-id_mod_fiscal
          i_id_entrada    = zde_zsdt0001nt_alv-id_entrada
          i_id_empresa    = zde_zsdt0001cg_alv-id_bukrs
        importing
          e_permitido     = data(e_permitido_0305) ).

      if e_permitido_0305 eq abap_false.
        screen-input = 0.
        modify screen.
      endif.
    endif.
  endloop.

  if nm_field_set_nota is not initial.
    set cursor field nm_field_set_nota offset pos.
    clear nm_field_set_nota.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_NOTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module atribui_info_nota input.
  clear: it_takes_saldo[].
  ck_alterado_nota = abap_true.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0305  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0305 input.

  data: wa_zsdt0001pd type zsdt0001pd.

  check vg_tl_0305 eq '0305'.

  if ck_alterado_nota eq abap_true.
    try .
        objeto->add_nota_fiscal( exporting i_nota = zde_zsdt0001nt_alv importing e_nota = zde_zsdt0001nt_alv ).
        read table it_notas with key id_nota = zde_zsdt0001nt_alv-id_nota assigning field-symbol(<fs_nota>).
        if sy-subrc is initial.
          <fs_nota> = zde_zsdt0001nt_alv.
        else.
          append zde_zsdt0001nt_alv to it_notas.
        endif.
        ck_alterado_nota = abap_false.

      catch zcx_parceiros into ex_parceiros.  "
        ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        perform seta_campo using ex_parceiros->msgid ex_parceiros->msgno.
        exit.
      catch zcx_carga into ex_carga.  "

        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        perform seta_campo using ex_carga->msgid ex_carga->msgno.

        if ex_carga->msgid eq zcx_carga=>zcx_forn_sem_parametro-msgid and
           ex_carga->msgno eq zcx_carga=>zcx_forn_sem_parametro-msgno and
           zde_zsdt0001nt_alv-nr_fornecedor_ie is not initial.
          ex_carga->published_erro( exporting i_msgty = 'W' i_msgty_display = 'W' ).
          wa_zsdt0001pd-id_branch    = objeto->carga-id_branch.
          wa_zsdt0001pd-id_bukrs     = objeto->carga-id_bukrs.
          wa_zsdt0001pd-nr_safra     = objeto->carga-nr_safra.

          try .
              zcl_fornecedores=>zif_parceiros~get_instance(
                )->set_parceiro_ie( i_insc_estatual = conv #( zde_zsdt0001nt_alv-nr_fornecedor_ie )
                )->get_id_parceiro( importing e_parceiro = wa_zsdt0001pd-id_produtor
                )->ck_ativo(
                )->ck_ativo_empresa( i_empresa = objeto->carga-id_bukrs
                ).
              insert into zsdt0001pd values wa_zsdt0001pd.
              commit work.

            catch zcx_parceiros into ex_parceiros.
              ex_parceiros->published_erro( exporting i_msgty = 'S' i_msgty_display = 'E' ).
              exit.
          endtry.

        else.
          ex_carga->published_erro( exporting i_msgty = 'S' i_msgty_display = 'W' ).
          exit.
        endif.
    endtry.
  endif.


  case ok_code.
    when 'PEDIDO'.

      if zde_zsdt0001nt_alv-cfop is not initial.

        "CFOP de Retorno de Armazenagem
        if zcl_cfop=>get_ck_cfop_retorno_amazem( i_cfop = zde_zsdt0001nt_alv-cfop ) = abap_true.

          zcl_pedido_compra=>get_pedido_compra_chave_e(
            exporting
              i_lifnr               = zde_zsdt0001nt_alv-id_fornecedor    " Nº conta do fornecedor
              i_bukrs               = objeto->carga-id_bukrs     " Empresa
              i_werks               = objeto->carga-id_branch    " Centro
              i_matnr               = objeto->carga-id_produto    " Grupo de mercadorias
              i_lgort               = 'ARMZ'    " Depósito
              i_charg               = conv #( objeto->carga-nr_safra )    " Número do lote
              i_bstyp               = 'F'    " Categoria do documento de compras
              i_bsart               = 'ZARM'    " Tipo de documento de compras
            receiving
              r_ekpo                = data(r_ekpo)
            exceptions
              nao_encontrado_pedido = 1
              others                = 2
          ).

          if sy-subrc is not initial.
            message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like sy-msgty.
            exit.
          endif.

          check r_ekpo is not initial.

          try .
              zde_zsdt0001nt_alv-po_number = r_ekpo-ebeln.
              zde_zsdt0001nt_alv-po_item   = r_ekpo-ebelp.
              objeto->add_nota_fiscal( exporting i_nota = zde_zsdt0001nt_alv importing e_nota = zde_zsdt0001nt_alv ).
              read table it_notas with key id_nota = zde_zsdt0001nt_alv-id_nota assigning <fs_nota>.
              if sy-subrc is initial.
                <fs_nota> = zde_zsdt0001nt_alv.
              else.
                append zde_zsdt0001nt_alv to it_notas.
              endif.
              ck_alterado_nota = abap_false.
            catch zcx_parceiros into ex_parceiros.  "
              ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              perform seta_campo using ex_parceiros->msgid ex_parceiros->msgno.
              exit.
            catch zcx_carga into ex_carga.
              ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              perform seta_campo using ex_carga->msgid ex_carga->msgno.
              exit.
          endtry.

        endif.

      endif.

  endcase.


endmodule.

*&---------------------------------------------------------------------*
*&      Form  INFORMAR_CHAVE_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form informar_chave_nfe .

  check zde_zsdt0001cg_alv-tp_status eq zif_carga=>st_status_aberto.

  if ( zde_zsdt0001nt_alv-id_mod_fiscal ne '55' and zde_zsdt0001nt_alv-id_mod_fiscal is not initial ).
    message s033 display like 'E'.
    exit.
  endif.

  clear: wa_add_nfe_9002.

  call screen 9002 starting at 40 10.

  if wa_add_nfe_9002-ck_incluir eq abap_true.
    "Entrar com a chave NF-e
    zde_zsdt0001nt_alv-id_mod_fiscal      = '55'.
    zde_zsdt0001nt_alv-dt_emissao         = wa_add_nfe_9002-dt_emissao.
    zde_zsdt0001nt_alv-ds_fornecedor      = wa_add_nfe_9002-name1.
    zde_zsdt0001nt_alv-id_fornecedor      = wa_add_nfe_9002-parid.
    zde_zsdt0001nt_alv-nr_fornecedor_ie   = wa_add_nfe_9002-parid_ie.
    zde_zsdt0001nt_alv-dt_vencimento_form = wa_add_nfe_9002-dt_emissao.
    zde_zsdt0001nt_alv-nr_nota            = wa_add_nfe_9002-numero.
    zde_zsdt0001nt_alv-nm_serie           = wa_add_nfe_9002-serie.
    zde_zsdt0001nt_alv-nr_valor           = wa_add_nfe_9002-nftot.
    zde_zsdt0001nt_alv-nr_quantidade      = wa_add_nfe_9002-ntgew.
    zde_zsdt0001nt_alv-nr_chave_nfe       = wa_add_nfe_9002-n55_chave_acesso.
    zde_zsdt0001nt_alv-cfop               = wa_add_nfe_9002-cfop.

    try .
        objeto->add_nota_fiscal( exporting i_nota = zde_zsdt0001nt_alv importing e_nota = zde_zsdt0001nt_alv ).
        read table it_notas with key id_nota = zde_zsdt0001nt_alv-id_nota assigning field-symbol(<fs_nota>).
        if sy-subrc is initial.
          <fs_nota> = zde_zsdt0001nt_alv.
        elseif vg_tl_0307 = '0307'.
          objeto->set_transgenia(
            changing
              i_in_gmo               = zde_zsdt0001cg_alv-in_gmo
              i_nr_resultado_01      = zde_zsdt0001cg_alv-nr_resultado_01
              i_nr_resultado_02      = zde_zsdt0001cg_alv-nr_resultado_02
              i_nr_res_rr1_rr2       = zde_zsdt0001cg_alv-nr_res_rr1_rr2
              i_in_gmo_03            = zde_zsdt0001cg_alv-in_gmo_03
              i_in_srr_origem_partic = zde_zsdt0001cg_alv-in_srr_origem_partic
              i_id_outro_partic      = zde_zsdt0001cg_alv-id_outro_partic
              i_in_srr_declarado     = zde_zsdt0001cg_alv-in_srr_declarado
              i_in_teste_srr         = zde_zsdt0001cg_alv-in_teste_srr
              i_in_srr_declarado_2   = zde_zsdt0001cg_alv-in_srr_declarado_2
              i_in_teste_srr_2       = zde_zsdt0001cg_alv-in_teste_srr_2
              i_id_classificadora    = zde_zsdt0001cg_alv-id_classificadora
              i_id_ck_class_dest     = zde_zsdt0001cg_alv-ck_class_dest
              i_tp_transgenia        = zde_zsdt0001cg_alv-tp_transgenia
              i_nr_nota              = zde_zsdt0001nt_alv-nr_nota
              i_classificacao        = zde_zsdt0001nt_alv-id_classificacao "US143677  zde_zsdt0001nt_alv-id_classificacao
          ).
          append zde_zsdt0001nt_alv to it_notas.
          objeto->set_transgenia(
            changing
              i_in_gmo               = zde_zsdt0001cg_aux-in_gmo
              i_nr_resultado_01      = zde_zsdt0001cg_aux-nr_resultado_01
              i_nr_resultado_02      = zde_zsdt0001cg_aux-nr_resultado_02
              i_nr_res_rr1_rr2       = zde_zsdt0001cg_aux-nr_res_rr1_rr2
              i_in_gmo_03            = zde_zsdt0001cg_aux-in_gmo_03
              i_in_srr_origem_partic = zde_zsdt0001cg_aux-in_srr_origem_partic
              i_id_outro_partic      = zde_zsdt0001cg_aux-id_outro_partic
              i_in_srr_declarado     = zde_zsdt0001cg_aux-in_srr_declarado
              i_in_teste_srr         = zde_zsdt0001cg_aux-in_teste_srr
              i_in_srr_declarado_2   = zde_zsdt0001cg_aux-in_srr_declarado_2
              i_in_teste_srr_2       = zde_zsdt0001cg_aux-in_teste_srr_2
              i_id_classificadora    = zde_zsdt0001cg_aux-id_classificadora
              i_id_ck_class_dest     = zde_zsdt0001cg_aux-ck_class_dest
              i_tp_transgenia        = zde_zsdt0001cg_aux-tp_transgenia
          ).
        else.
          append zde_zsdt0001nt_alv to it_notas.
        endif.
        ck_alterado_nota = abap_false.
      catch zcx_parceiros into ex_parceiros.    "
        ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      catch zcx_carga into ex_carga.    "
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    endtry.

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Module  VALIDA_SUBTOTAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module valida_subtotal input.
  "
  objeto->get_calcular_subtotal( exporting i_peso_bruto    = zde_zsdt0001cg_alv-nm_peso_bruto
                                           i_peso_tara     = zde_zsdt0001cg_alv-nm_peso_tara
                                 importing e_peso_subtotal = data(e_peso_subtotal) ).

  if zde_zsdt0001cg_alv-nm_peso_subtotal ne e_peso_subtotal.
    message e063.
  endif.

  perform ajusta_descontos.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  SET_MODEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_model input.

  if zde_zsdt0001nt_alv-id_mod_fiscal ne '55' and zde_zsdt0001nt_alv-id_mod_fiscal ne '1'.
    message e055.
  endif.

  if zde_zsdt0001nt_alv-id_mod_fiscal eq '1'.
    clear: zde_zsdt0001nt_alv-nr_chave_nfe.
  endif.

endmodule.

**&---------------------------------------------------------------------*
**&      Module  PSQ_PLACA  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PSQ_PLACA INPUT.
*
*  CHECK ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR IS NOT INITIAL.
*
*  TRY.
*      OBJETO->AT_CARGA_RECEBIMENTO->ZIF_CARGA~GET_INFO_PLACA( EXPORTING I_PLACA = ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR I_VALIDAR = ABAP_TRUE I_TRACAO = ABAP_TRUE IMPORTING E_ZLEST0002 = DATA(R_ZLEST0002) ).
*      ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO = R_ZLEST0002-PROPRIETARIO.
*    CATCH ZCX_CARGA INTO EX_CARGA.
*      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**&      Module  PSQ_REPO1  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PSQ_REPO1 INPUT.
*
*  CHECK ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1 IS NOT INITIAL.
*
*  TRY.
*      OBJETO->AT_CARGA_RECEBIMENTO->ZIF_CARGA~GET_INFO_PLACA( EXPORTING I_PLACA = ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1 I_VALIDAR = ABAP_TRUE IMPORTING E_ZLEST0002 = R_ZLEST0002 ).
*      IF R_ZLEST0002-PROPRIETARIO NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO.
*        MESSAGE W071 WITH ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1.
*      ENDIF.
*    CATCH ZCX_CARGA INTO EX_CARGA.
*      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**&      Module  PSQ_REPO2  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PSQ_REPO2 INPUT.
*
*  CHECK ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_2 IS NOT INITIAL.
*
*  TRY.
*      OBJETO->AT_CARGA_RECEBIMENTO->ZIF_CARGA~GET_INFO_PLACA( EXPORTING I_PLACA = ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_2 I_VALIDAR = ABAP_TRUE IMPORTING E_ZLEST0002 = R_ZLEST0002 ).
*      IF R_ZLEST0002-PROPRIETARIO NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO.
*        MESSAGE W071 WITH ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1.
*      ENDIF.
*    CATCH ZCX_CARGA INTO EX_CARGA.
*      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**&      Module  PSQ_REPO3  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PSQ_REPO3 INPUT.
*
*  CHECK ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_3 IS NOT INITIAL.
*
*  TRY.
*      OBJETO->AT_CARGA_RECEBIMENTO->ZIF_CARGA~GET_INFO_PLACA( EXPORTING I_PLACA = ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_3 I_VALIDAR = ABAP_TRUE IMPORTING E_ZLEST0002 = R_ZLEST0002 ).
*      IF R_ZLEST0002-PROPRIETARIO NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO.
*        MESSAGE W071 WITH ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1.
*      ENDIF.
*    CATCH ZCX_CARGA INTO EX_CARGA.
*      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0306  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0306 output.

  if ctl_alv_0306 is initial.

    create object ctl_cccontainer_0306
      exporting
        container_name = 'ALV_0306'.

    create object ctl_alv_0306
      exporting
        i_parent = ctl_cccontainer_0306.

    perform fill_it_fieldcatalog_0306.

    perform fill_gs_variant_0306.

    gs_layout_0306-sel_mode   = 'A'.
    gs_layout_0306-zebra      = abap_false.
    gs_layout_0306-cwidth_opt = abap_true.

    if zde_zsdt0001cg_alv-tp_status ne zif_carga=>st_status_aberto.
      gs_layout_0306-no_toolbar = abap_true.
    else.
      gs_layout_0306-no_toolbar = abap_false.
    endif.

    create object obg_toolbar_0306
      exporting
        io_alv_grid = ctl_alv_0306.

    set handler obg_toolbar_0306->on_toolbar for ctl_alv_0306.
    set handler obg_toolbar_0306->handle_user_command for ctl_alv_0306.

    call method ctl_alv_0306->set_table_for_first_display
      exporting
        is_layout       = gs_layout_0306
        is_variant      = gs_variant_0306
        i_save          = 'A'
      changing
        it_fieldcatalog = it_fieldcatalog_0306
        it_outtab       = it_notas[].

    create object event_handler_0306.
    set handler event_handler_0306->handle_double_click  for ctl_alv_0306.

  endif.

  wa_stable_0306-row = abap_true.
  wa_stable_0306-col = abap_true.

  call method ctl_alv_0306->refresh_table_display
    exporting
      is_stable      = wa_stable_0306
      i_soft_refresh = abap_true.

endmodule.


*&---------------------------------------------------------------------*
*&      Form  LIMPA_TELA_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form limpa_tela_0300 .

  if tree is not initial.
    tree->free( ).
  endif.
  clear: tree.

  clear: obg_toolbar_0306,
         obj_toolbarmanager_0306,
         obg_toolbar_0313a,
         obj_toolbarmanager_0313a,
         obg_toolbar_0312a,
         obj_toolbarmanager_0312a,
         obg_toolbar_0312b,
         obj_toolbarmanager_0312b,
         event_handler_0312a.

  "Alv de Take UP's Vinculados
  if ctl_alv_0313a is not initial.
    ctl_alv_0313a->free( ).
  endif.
  clear: ctl_alv_0313a.

  if ctl_cccontainer_0313a is not initial.
    ctl_cccontainer_0313a->free( ).
  endif.
  clear: ctl_cccontainer_0313a.

  "Alv de Take UP's Livres
  if ctl_alv_0312a is not initial.
    ctl_alv_0312a->free( ).
  endif.
  clear: ctl_alv_0312a.

  if ctl_cccontainer_0312a is not initial.
    ctl_cccontainer_0312a->free( ).
  endif.
  clear: ctl_cccontainer_0312a.

  "Alv de Take UP's Vinculados
  if ctl_alv_0312b is not initial.
    ctl_alv_0312b->free( ).
  endif.
  clear: ctl_alv_0312b.

  if ctl_cccontainer_0312b is not initial.
    ctl_cccontainer_0312b->free( ).
  endif.
  clear: ctl_cccontainer_0312b.

  if ctl_alv_0306 is not initial.
    ctl_alv_0306->free( ).
  endif.
  clear: ctl_alv_0306.

  if ctl_cccontainer_0306 is not initial.
    ctl_cccontainer_0306->free( ).
  endif.
  clear: ctl_cccontainer_0306.

  if picture_9012 is not initial.
    picture_9012->free( ).
  endif.
  clear: picture_9012.

  if ctl_cccontainer_9012 is not initial.
    ctl_cccontainer_9012->free( ).
  endif.
  clear: ctl_cccontainer_9012.

  if tree_0300 is not initial.
    tree_0300->free( ).
  endif.
  clear: tree_0300.

  clear: g_application_0300.

  if docking_0300_se is not initial.
    cl_abap_browser=>close_browser( ).
    docking_0300_se->free( ).
  endif.
  clear: docking_0300_se.

  if docking_0300 is not initial.
    docking_0300->free( ).
  endif.
  clear: docking_0300.

endform.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_it_fieldcatalog_0306 .

  data: lc_col_pos  type lvc_colpos.

  field-symbols: <fs_cat> type lvc_s_fcat.

  clear: it_fieldcatalog_0306[].

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name = 'ZDE_ZSDT0001NT_ALV'
    changing
      ct_fieldcat      = it_fieldcatalog_0306.

  loop at it_fieldcatalog_0306 assigning <fs_cat>.

    <fs_cat>-tabname = 'ZDE_ZSDT0001NT_ALV'.

    case <fs_cat>-fieldname.
      when 'ID_CARGA'.
        <fs_cat>-no_out = abap_true.
      when 'DT_EMISSAO' or 'DT_VENCIMENTO_FORM'.
        <fs_cat>-just    = 'C'.
      when 'NR_QUANTIDADE' or 'NR_VALOR'    or
           'NR_QTDE_UMI'   or 'NR_QTDE_IMP' or
           'NR_QTDE_AVA'   or 'NR_QTDE_ARD' or
           'NR_QTDE_QUE'   or 'NR_QTDE_ESV'.
        <fs_cat>-do_sum = abap_true.
    endcase.

    if <fs_cat>-fieldname <> 'ST_MENSAGEM'.
      <fs_cat>-col_pos = lc_col_pos.
      add 1 to lc_col_pos.
    endif.

  endloop.

endform.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form fill_gs_variant_0306.

  gs_variant_0306-report      = sy-repid.
  gs_variant_0306-handle      = '0306'.
  gs_variant_0306-log_group   = abap_false.
  gs_variant_0306-username    = abap_false.
  gs_variant_0306-variant     = abap_false.
  gs_variant_0306-text        = abap_false.
  gs_variant_0306-dependvars  = abap_false.

endform.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0307  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0307 output.
  set pf-status 'PF0307'.
  set titlebar 'TL0307'.
endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0307_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0307_exit input.

  if ok_code eq 'CHNFE'.
    perform informar_chave_nfe.
  else.
    clear zde_zsdt0001nt_alv.
    leave to screen 0.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0307  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0307 input.

  case ok_code.
    when 'ENTER'.

      if ck_alterado_nota eq abap_true.

        try .
            objeto->add_nota_fiscal( exporting i_nota = zde_zsdt0001nt_alv importing e_nota = zde_zsdt0001nt_alv ).
            read table it_notas with key id_nota = zde_zsdt0001nt_alv-id_nota assigning <fs_nota>.
            if sy-subrc is initial.
              <fs_nota> = zde_zsdt0001nt_alv.
            else.
              move-corresponding zde_zsdt0001cg_alv to zde_zsdt0001nt_alv.
              append zde_zsdt0001nt_alv to it_notas.
            endif.
            ck_alterado_nota = abap_false.
          catch zcx_parceiros into ex_parceiros.  "
            ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            perform seta_campo using ex_parceiros->msgid ex_parceiros->msgno.
            exit.
          catch zcx_carga into ex_carga.
            ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            perform seta_campo using ex_carga->msgid ex_carga->msgno.
            exit.
        endtry.

      endif.

      "US 143677 - transgenia por nota
      if zde_zsdt0001nt_alv-id_classificacao is  initial.
*        zde_zsdt0001nt_alv-id_classificacao = zde_zsdt0001nt_alv-nr_nota.
      endif.
      if ck_alterado_gmo = abap_true.
        objeto->set_transgenia(
          changing
            i_in_gmo               = zde_zsdt0001cg_alv-in_gmo
            i_nr_resultado_01      = zde_zsdt0001cg_alv-nr_resultado_01
            i_nr_resultado_02      = zde_zsdt0001cg_alv-nr_resultado_02
            i_nr_res_rr1_rr2       = zde_zsdt0001cg_alv-nr_res_rr1_rr2
            i_in_gmo_03            = zde_zsdt0001cg_alv-in_gmo_03
            i_in_srr_origem_partic = zde_zsdt0001cg_alv-in_srr_origem_partic
            i_id_outro_partic      = zde_zsdt0001cg_alv-id_outro_partic
            i_in_srr_declarado     = zde_zsdt0001cg_alv-in_srr_declarado
            i_in_teste_srr         = zde_zsdt0001cg_alv-in_teste_srr
            i_in_srr_declarado_2   = zde_zsdt0001cg_alv-in_srr_declarado_2
            i_in_teste_srr_2       = zde_zsdt0001cg_alv-in_teste_srr_2
            i_id_classificadora    = zde_zsdt0001cg_alv-id_classificadora
            i_id_ck_class_dest     = zde_zsdt0001cg_alv-ck_class_dest
            i_tp_transgenia        = zde_zsdt0001cg_alv-tp_transgenia
            i_nr_nota              = zde_zsdt0001nt_alv-nr_nota "US143677  zde_zsdt0001nt_alv-id_classificacao
            i_classificacao        = zde_zsdt0001nt_alv-id_classificacao "US143677  zde_zsdt0001nt_alv-id_classificacao
        ).
      endif.


      "Adicionar classicação.
      if ck_alterado_nota ne abap_true.

      endif.


      leave to screen 0.

    when 'PEDIDO'.

      if zde_zsdt0001nt_alv-cfop is not initial.

        "CFOP de Retorno de Armazenagem
        if zcl_cfop=>get_ck_cfop_retorno_amazem( i_cfop = zde_zsdt0001nt_alv-cfop ) = abap_true.

          zcl_pedido_compra=>get_pedido_compra_chave_e(
            exporting
              i_lifnr               = zde_zsdt0001nt_alv-id_fornecedor    " Nº conta do fornecedor
              i_bukrs               = objeto->carga-id_bukrs     " Empresa
              i_werks               = objeto->carga-id_branch    " Centro
              i_matnr               = objeto->carga-id_produto    " Grupo de mercadorias
              i_lgort               = 'ARMZ'    " Depósito
              i_charg               = conv #( objeto->carga-nr_safra )    " Número do lote
              i_bstyp               = 'F'    " Categoria do documento de compras
              i_bsart               = 'ZARM'    " Tipo de documento de compras
            receiving
              r_ekpo                = r_ekpo
            exceptions
              nao_encontrado_pedido = 1
              others                = 2
          ).

          if sy-subrc is not initial.
            message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 display like sy-msgty.
            exit.
          endif.

          check r_ekpo is not initial.

          try .
              zde_zsdt0001nt_alv-po_number = r_ekpo-ebeln.
              zde_zsdt0001nt_alv-po_item   = r_ekpo-ebelp.
              objeto->add_nota_fiscal( exporting i_nota = zde_zsdt0001nt_alv importing e_nota = zde_zsdt0001nt_alv ).
              read table it_notas with key id_nota = zde_zsdt0001nt_alv-id_nota assigning <fs_nota>.
              if sy-subrc is initial.
                <fs_nota> = zde_zsdt0001nt_alv.
              else.
                append zde_zsdt0001nt_alv to it_notas.
              endif.
              ck_alterado_nota = abap_false.
            catch zcx_parceiros into ex_parceiros.  "
              ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              perform seta_campo using ex_parceiros->msgid ex_parceiros->msgno.
              exit.
            catch zcx_carga into ex_carga.
              ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
              perform seta_campo using ex_carga->msgid ex_carga->msgno.
              exit.
          endtry.

        endif.

      endif.

  endcase.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0306  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_scroll_info_0306 input.

  if ctl_alv_0306 is not initial.
    call method ctl_alv_0306->get_scroll_info_via_id
      importing
        es_col_info = gs_scroll_col_0306
        es_row_no   = gs_scroll_row_0306.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0306  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_selected_rows_0306 input.

  if ctl_alv_0306 is not initial.
    clear it_selected_rows.
    call method ctl_alv_0306->get_selected_rows
      importing
        et_index_rows = it_selected_rows.

    clear: it_notas_sel[], it_notas_sel.

    loop at it_selected_rows into wa_selected_rows.
      read table it_notas into data(wa_nota) index wa_selected_rows-index.
      append wa_nota to it_notas_sel.
    endloop.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  GET_FORNE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_forne input.

  check zde_zsdt0001nt_alv-nr_fornecedor_ie is not initial.

  try .
      objeto->get_nota_fornecedor_ie( exporting i_stcd3 = conv #( zde_zsdt0001nt_alv-nr_fornecedor_ie ) importing e_nota = data(lc_nota_ie) ).

      "Verificar Restrição de Embargos """""""""""""""""""""""""""""""""""""
      zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = lc_nota_ie-id_fornecedor
        )->ck_restricao_embargo( exporting i_gera_erro = abap_false importing e_resultado   = data(e_resultado_restri)
        ).

      if e_resultado_restri-bloqueado eq abap_true.
        perform apresenta_restricao using e_resultado_restri.
        message e_resultado_restri-motivo type 'E'.
      endif.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      zde_zsdt0001nt_alv-id_fornecedor = lc_nota_ie-id_fornecedor.
      zde_zsdt0001nt_alv-ds_fornecedor = lc_nota_ie-ds_fornecedor.


    catch zcx_carga into ex_carga.

      if ex_carga->msgid eq zcx_carga=>zcx_forn_sem_parametro-msgid.

        ex_carga->published_erro( exporting i_msgty = 'W' i_msgty_display = 'W' ).
        wa_zsdt0001pd-id_branch    = objeto->carga-id_branch.
        wa_zsdt0001pd-id_bukrs     = objeto->carga-id_bukrs.
        wa_zsdt0001pd-nr_safra     = objeto->carga-nr_safra.

        try .

            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro_ie( i_insc_estatual = conv #( zde_zsdt0001nt_alv-nr_fornecedor_ie )
              )->get_id_parceiro( importing e_parceiro = wa_zsdt0001pd-id_produtor
              )->ck_ativo(
              )->ck_ativo_empresa( i_empresa = objeto->carga-id_bukrs
              ).

            insert into zsdt0001pd values wa_zsdt0001pd.
            commit work.

          catch zcx_parceiros into ex_parceiros.
            ex_parceiros->published_erro( exporting i_msgty = 'E' i_msgty_display = 'E' ).
        endtry.

      else.
        ex_carga->published_erro( exporting i_msgty = 'E' i_msgty_display = 'E' ).
      endif.
  endtry.

endmodule.

*&---------------------------------------------------------------------*
*&      Form  CONFERIR_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form conferir_carga .

  data: lc_peso_liquido type zde_nm_peso_liquido.

  clear: ck_conferiu.

*  LC_PESO_LIQUIDO = ZDE_ZSDT0001CG_ALV-NM_PESO_LIQUIDO.

*  DESCRIBE TABLE IT_NOTAS LINES DATA(QT_NOTAS).

*  IF QT_NOTAS GT 1.
*    "Sujerir o Peso Fiscal no Peso SubTotal
*    LOOP AT IT_NOTAS ASSIGNING FIELD-SYMBOL(<FS_NOTA>) WHERE NM_PESO_SUBTOTAL IS INITIAL.
*      <FS_NOTA>-NM_PESO_SUBTOTAL = <FS_NOTA>-NR_QUANTIDADE.
*
*      IF LC_PESO_LIQUIDO LE <FS_NOTA>-NM_PESO_SUBTOTAL.
*        <FS_NOTA>-NM_PESO_LIQUIDO = LC_PESO_LIQUIDO.
*        LC_PESO_LIQUIDO = 0.
*      ELSE.
*        <FS_NOTA>-NM_PESO_LIQUIDO = <FS_NOTA>-NM_PESO_SUBTOTAL.
*        IF ( LC_PESO_LIQUIDO - <FS_NOTA>-NM_PESO_SUBTOTAL ) LE 0.
*          LC_PESO_LIQUIDO = 0.
*        ELSE.
*          LC_PESO_LIQUIDO = LC_PESO_LIQUIDO - <FS_NOTA>-NM_PESO_SUBTOTAL.
*        ENDIF.
*      ENDIF.
*
*      TRY .
*          OBJETO->SET_PESOS_NOTAS(
*            EXPORTING
*              I_ID_CARGA      = <FS_NOTA>-ID_CARGA
*              I_ID_NOTA       = <FS_NOTA>-ID_NOTA
*              I_PESO_SUBTOTAL = <FS_NOTA>-NM_PESO_SUBTOTAL
*              I_PESO_LIQUIDO  = <FS_NOTA>-NM_PESO_LIQUIDO
*            IMPORTING
*              E_NOTA          = DATA(INFO_NOTA) ).
*          MOVE-CORRESPONDING INFO_NOTA TO <FS_NOTA>.
*        CATCH ZCX_CARGA.    "
*      ENDTRY.
*
*    ENDLOOP.
*  ELSE.
*    LOOP AT IT_NOTAS ASSIGNING <FS_NOTA>.
*
*      <FS_NOTA>-NM_PESO_SUBTOTAL = ZDE_ZSDT0001CG_ALV-NM_PESO_SUBTOTAL.
*      <FS_NOTA>-NM_PESO_LIQUIDO  = ZDE_ZSDT0001CG_ALV-NM_PESO_LIQUIDO.
*
*      TRY .
*          OBJETO->SET_PESOS_NOTAS(
*            EXPORTING
*              I_ID_CARGA      = <FS_NOTA>-ID_CARGA
*              I_ID_NOTA       = <FS_NOTA>-ID_NOTA
*              I_PESO_SUBTOTAL = <FS_NOTA>-NM_PESO_SUBTOTAL
*              I_PESO_LIQUIDO  = <FS_NOTA>-NM_PESO_LIQUIDO
*            IMPORTING
*              E_NOTA          = INFO_NOTA ).
*          MOVE-CORRESPONDING INFO_NOTA TO <FS_NOTA>.
*        CATCH ZCX_CARGA.    "
*      ENDTRY.
*
*    ENDLOOP.
*
*  ENDIF.

  call screen 9004 starting at 40 05.

endform.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_NODE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*      -->P_ITEM_NAME  text
*----------------------------------------------------------------------*
form mostrar_node_log
  using  p_node_key  type  tv_nodekey
         p_item_name type  tv_itmname.

  read table it_tree_info_log with key node_key = p_node_key item_name = p_item_name binary search.
  if sy-subrc is initial.
    try .
        objeto->get_info_alv_apresentacao_log(
          exporting
            i_dt_registro  = it_tree_info_log-registro-dt_registro
            i_hr_registro  = it_tree_info_log-registro-hr_registro
            i_us_registro  = it_tree_info_log-registro-us_registro
          importing
            e_apresentacao = data(r_apresentacao) ).

        ck_registro_log    = abap_true.
        zde_zsdt0001cg_alv = r_apresentacao-carga.
        zde_zsdt0001od_alv = r_apresentacao-ordem_carrega.
        it_notas[] = r_apresentacao-notas[].
        it_takes_vincu[] = e_apresentacao-takeup[].

        leave to screen 0300.
      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      catch zcx_ordem_carregamento into ex_ordem_carregamento.
        ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    endtry.
  endif.

endform.


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TREE_LOGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form atualiza_tree_logs .

  data: qtd_itens type i,
        node      type treev_node,
        item      type mtreeitm.

  check tree_0300 is not initial.

  tree_0300->delete_all_nodes( ).

  clear: node_table_0300[], item_table_0300[], it_tree_info_log[], it_tree_info_log.

  try .
      objeto->get_logs_historico( importing e_logs = data(it_logs) ).
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  endtry.

  check it_logs[] is not initial.

  select * into table @data(it_usuarios)
    from user_addr
     for all entries in @it_logs
   where bname eq @it_logs-us_registro.

  sort it_usuarios by bname.

  describe table it_logs lines data(qtd_linhas).

  loop at it_logs into data(wa_logs).

    clear: node.
    if sy-tabix eq qtd_linhas.
      node-n_image    = icon_customer.
      node-exp_image  = icon_customer.
    else.
      node-n_image    = icon_hr_position.
      node-exp_image  = icon_hr_position.
    endif.

    add 1 to qtd_itens.
    node-node_key   = qtd_itens.
    condense node-node_key no-gaps.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = node-node_key
      importing
        output = node-node_key.

    node-hidden     = abap_false. " The node is visible,
    node-disabled   = abap_false. " selectable,
    node-isfolder   = abap_true. " a folder.
    node-expander   = abap_false.
    append node to node_table_0300.

    it_tree_info_log-node_key    = node-node_key.
    it_tree_info_log-item_name   = c_tree_0300-column1.
    it_tree_info_log-registro    = wa_logs.
    concatenate wa_logs-dt_registro+6(2) '.' wa_logs-dt_registro+4(2) '.' wa_logs-dt_registro(4) into it_tree_info_log-dt_registro.
    concatenate wa_logs-hr_registro(2) ':' wa_logs-hr_registro+2(2) ':' wa_logs-hr_registro+4(2) into it_tree_info_log-hr_registro.

    clear item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0300-column1.
    item-class     = cl_gui_list_tree=>item_class_link. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_intensifd_critical.
    item-font      = cl_gui_list_tree=>item_font_prop.
    read table it_usuarios into data(wa_usuarios) with key bname = wa_logs-us_registro.
    if sy-subrc is initial.
      item-text = wa_usuarios-name_textc.
      it_tree_info_log-us_registro = wa_usuarios-name_textc.
    else.
      item-text = wa_logs-us_registro.
      it_tree_info_log-us_registro = wa_logs-us_registro.
    endif.
    append item to item_table_0300.

    clear item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0300-column2.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_center.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_default.
    item-text      = it_tree_info_log-dt_registro.
    append item to item_table_0300.

    clear item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0300-column3.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_center.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_default.
    item-text      = it_tree_info_log-hr_registro.
    append item to item_table_0300.

    append it_tree_info_log.
  endloop.

  call method tree_0300->add_nodes_and_items
    exporting
      node_table                     = node_table_0300
      item_table                     = item_table_0300
      item_table_structure_name      = 'MTREEITM'
    exceptions
      failed                         = 1
      cntl_system_error              = 3
      error_in_tables                = 4
      dp_error                       = 5
      table_structure_name_not_found = 6.

*    TREE_0300->EXPAND_NODES( EXPORTING NODE_KEY_TABLE = IT_NODE
*      EXCEPTIONS
*        FAILED                  = 1
*        CNTL_SYSTEM_ERROR       = 2
*        ERROR_IN_NODE_KEY_TABLE = 3
*        DP_ERROR                = 4
*        OTHERS                  = 5 ).

  sort it_tree_info_log by node_key item_name.

endform.

*&---------------------------------------------------------------------*
*&      Module  GET_ENTRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module get_entra input.

  if zde_zsdt0001nt_alv-id_entrada is not initial.
    select single ds_entrada into zde_zsdt0001nt_alv-ds_entrada
      from zsdt0001tetx
     where id_entrada eq zde_zsdt0001nt_alv-id_entrada.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  PRC_IN_GMO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module prc_in_gmo input.

  try .

      objeto->set_transgenia(
        changing
          i_in_gmo               = zde_zsdt0001cg_alv-in_gmo
          i_nr_resultado_01      = zde_zsdt0001cg_alv-nr_resultado_01
          i_nr_resultado_02      = zde_zsdt0001cg_alv-nr_resultado_02
          "i_nr_resultado_03       = zde_zsdt0001cg_alv-nr_resultado_03 "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
          "i_nr_resultado_04       = zde_zsdt0001cg_alv-nr_resultado_04 "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
          "i_nr_fita               = zde_zsdt0001cg_alv-nr_fita         "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
          i_nr_res_rr1_rr2       = zde_zsdt0001cg_alv-nr_res_rr1_rr2
          i_in_gmo_03            = zde_zsdt0001cg_alv-in_gmo_03
          i_in_srr_origem_partic = zde_zsdt0001cg_alv-in_srr_origem_partic
          i_id_outro_partic      = zde_zsdt0001cg_alv-id_outro_partic
          i_in_srr_declarado     = zde_zsdt0001cg_alv-in_srr_declarado
          i_in_teste_srr         = zde_zsdt0001cg_alv-in_teste_srr
          i_in_srr_declarado_2   = zde_zsdt0001cg_alv-in_srr_declarado_2
          i_in_teste_srr_2       = zde_zsdt0001cg_alv-in_teste_srr_2
          i_id_classificadora    = zde_zsdt0001cg_alv-id_classificadora
          i_id_ck_class_dest     = zde_zsdt0001cg_alv-ck_class_dest
          i_tp_transgenia        = zde_zsdt0001cg_alv-tp_transgenia ).
      ck_alterado_gmo = abap_true.
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  endtry.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  VALU_REQUEST_ID_ENTRADA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module value_request_id_entrada input.

  data: lc_tp_entrada type  zsdt0001tetx.

  call function 'Z_PSQ_TIPO_ENTRADA'
    exporting
      id_bukrs   = zde_zsdt0001cg_alv-id_bukrs
      id_branch  = zde_zsdt0001cg_alv-id_branch
      nr_safra   = zde_zsdt0001cg_alv-nr_safra
    importing
      tp_entrada = lc_tp_entrada
    exceptions
      erro       = 1
      others     = 2.

  if sy-subrc is not initial.
    message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  else.
    zde_zsdt0001nt_alv-id_entrada = lc_tp_entrada-id_entrada.
  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Form  SETA_CAMPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EX_ORDEM_VENDA_>MSGID  text
*      -->P_EX_ORDEM_VENDA_>MSGNO  text
*----------------------------------------------------------------------*
form seta_campo  using    p_msgid type syst_msgid
                          p_msgno type syst_msgno.

  clear: nm_field_set_nota,
         nm_field_set_carga.

  case p_msgid.
    when 'ZODVENDA'. "Ordem de Venda

      nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM_VENDA'.

    when 'ZORDEMCA'. "Ordem de Carregamento

      "Local Entrega
      nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM'.

    when 'ZCARGA'.
      case p_msgno.

          """" Cabeçalho """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        when zcx_carga=>zcx_obg_inf_ordem_venda-msgno or
             zcx_carga=>zcx_obg_inf_produto-msgno.

          "Ordem de Venda
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM_VENDA'.

        when zcx_carga=>zcx_ordem_empresaag-msgno.

          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE'.

        when zcx_carga=>zcx_obg_inf_lc_entrega-msgno or
             zcx_carga=>zcx_le_sem_param_lc_negocio-msgno or
             zcx_carga=>zcx_le_sem_param_material-msgno.

          "Local Entrega
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_ENTREGA'.

        when zcx_carga=>zcx_obg_inf_lc_coleta-msgno.

          "Local de Coleta
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_COLETA'.

        when zcx_carga=>zcx_obg_inf_lc_destino-msgno.

          "Destino
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESTINO'.

        when zcx_carga=>zcx_obg_inf_lc_descarga-msgno.

          "Local de Descarga
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESCARGA'.

        when zcx_carga=>zcx_ordem_vencida-msgno or
             zcx_carga=>zcx_ordem_produto-msgno or
             zcx_carga=>zcx_placa_trator-msgno or
             zcx_carga=>zcx_placa_reboque1-msgno or
             zcx_carga=>zcx_placa_reboque2-msgno or
             zcx_carga=>zcx_placa_reboque3-msgno or
             zcx_carga=>zcx_ordem_motorista-msgno or
             zcx_carga=>zcx_ordem_proprietario-msgno or
             zcx_carga=>zcx_ordem_destino-msgno or
             zcx_carga=>zcx_ordem_descarga-msgno.

          "Ordem de Carregamento
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM'.

        when zcx_carga=>zcx_obg_inf_motorista-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_MOTORISTA'.

        when zcx_carga=>zcx_obg_inf_proprietario-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO'.

        when zcx_carga=>zcx_obg_inf_trator-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR'.

        when zcx_carga=>zcx_obg_inf_ticket-msgno or
             zcx_carga=>zcx_erro_ticket_utilizado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_TICKET'.

        when zcx_carga=>zcx_obg_inf_ps_bruto-msgno.

        when zcx_carga=>zcx_obg_inf_ps_bruto-msgno or
             zcx_carga=>zcx_tara_maior_bruto-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_BRUTO'.

        when zcx_carga=>zcx_obg_inf_ps_tara-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_TARA'.

        when zcx_carga=>zcx_obg_inf_ps_subtotal-msgno or
             zcx_carga=>zcx_errp_ps_subtotal-msgno or
             zcx_carga=>zcx_peso_liq_subtotal-msgno or
             zcx_carga=>zcx_obg_inf_ps_liquido-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_SUBTOTAL'.

        when zcx_carga=>zcx_obg_emp_classificadora-msgno or
             zcx_carga=>zcx_fornecedor_nao_classifica-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_CLASSIFICADORA'.

        when zcx_carga=>zcx_obg_class_umidade-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_UMI'.
        when zcx_carga=>zcx_obg_class_impureza-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_IMP'.
        when zcx_carga=>zcx_obg_class_avariado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA'.
        when zcx_carga=>zcx_obg_class_ardido-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_ARD'.
        when zcx_carga=>zcx_obg_class_quebrado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_QUE'.
        when zcx_carga=>zcx_obg_class_esverdeado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_ESV'.

        when zcx_carga=>zcx_nao_teste_amaggi_positivo-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-IN_GMO'.

        when zcx_carga=>zcx_obg_inf_outro_part-msgno or
             zcx_carga=>zcx_nao_inf_outro_part-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_OUTRO_PARTIC'.

          """" Nota Fiscal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "Tipo de Entrada
        when zcx_carga=>zcx_obg_inf_tp_entrada-msgno or
             zcx_carga=>zcx_tp_entrada_nao_permitido-msgno or
             zcx_carga=>zcx_te_md_fiscal_sem_param-msgno or
             zcx_carga=>zcx_te_sem_ct_fiscal-msgno or
             zcx_carga=>zcx_te_sem_tp_mov_merc-msgno or
             zcx_carga=>zcx_te_sem_iva-msgno or
             zcx_carga=>zcx_te_sem_form_pagamento-msgno or
             zcx_carga=>zcx_te_sem_ch_bloqueio-msgno or
             zcx_carga=>zcx_te_sem_bnc_empresa-msgno or
             zcx_carga=>zcx_te_somente_fisica-msgno or
             zcx_carga=>zcx_te_somente_juridica-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-ID_ENTRADA'.

          "Modelo de Documento Fiscal
        when zcx_carga=>zcx_nao_permitido_md_fiscal-msgno or
             zcx_carga=>zcx_pessoa_fis_nfe-msgno or
             zcx_carga=>zcx_pessoa_jus_papel-msgno or
             zcx_carga=>zcx_nf_propria_nfe-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL'.

          "Chave NF-e
        when zcx_carga=>zcx_obg_inf_chave_nfe-msgno or
             zcx_carga=>zcx_nfe_nao_distribuida-msgno or
             zcx_carga=>zcx_nfe_item_nao_distribuido-msgno or
             zcx_carga=>zcx_nfe_item_unidade-msgno or
             zcx_carga=>zcx_xml_nfe_nao_recebido-msgno.

          nm_field_set_nota = 'BTN_CHAVE'.

          "IE do Fornecedor
        when zcx_carga=>zcx_obg_inf_ie_prod-msgno or
             zcx_carga=>zcx_obg_inf_fornecedor-msgno or
             zcx_carga=>zcx_forn_sem_parametro-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_FORNECEDOR_IE'.

          "Numero Documento
        when zcx_carga=>zcx_obg_inf_nf_numero-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_NOTA'.

          "Série do Documento
        when zcx_carga=>zcx_obg_inf_nf_serie-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NM_SERIE'.

          "Data de Emissão
        when zcx_carga=>zcx_obg_inf_nf_dt_emissao-msgno or
             zcx_carga=>zcx_data_emissao_nf-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-DT_EMISSAO'.

          "Quantidade
        when zcx_carga=>zcx_obg_inf_nf_quantidade-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_QUANTIDADE'.

          "Valor
        when zcx_carga=>zcx_obg_inf_nf_valor_total-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_VALOR'.

          "Data de Vencimento
        when zcx_carga=>zcx_obg_inf_dt_venc_form-msgno or
             zcx_carga=>zcx_data_formulario_venc-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-DT_VENCIMENTO_FORM'.

          "CFOP
        when zcx_carga=>zcx_obg_inf_cfop-msgno or
             zcx_carga=>zcx_cfop_nao_permitido_te-msgno or
             zcx_carga=>zcx_cfop_nao_permitido_forn-msgno or
             zcx_carga=>zcx_xml_nfe_cfop_invalido-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-CFOP'.

      endcase.
  endcase.

endform.

*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_OV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module value_request_ov input.

  data: e_vbeln type  zde_ordem_venda_psq.

  check objeto->carga-tp_status eq zif_carga=>st_status_aberto.

  call function 'POPUP_TO_CONFIRM'
    exporting
      titlebar              = 'Tipo de Pesquisaq'
      text_question         = 'Pesquisar Ordem de Venda?'
      text_button_1         = text-003
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = text-004
      icon_button_2         = 'ICON_INCOMPLETE'
      default_button        = '1'
      display_cancel_button = ' '
    importing
      answer                = answer
    exceptions
      text_not_found        = 1
      others                = 2.

  case answer.
    when '1'.

      call function 'Z_PSQ_ORDEM_VENDA'
        exporting
          i_id_bukrs  = zde_zsdt0001cg_alv-id_bukrs
          i_id_branch = zde_zsdt0001cg_alv-id_branch
          i_nr_safra  = zde_zsdt0001cg_alv-nr_safra
        importing
          e_vbeln     = e_vbeln
        exceptions
          erro        = 1
          others      = 2.

      if sy-subrc is not initial.
        message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      else.
        zde_zsdt0001ov_alv-nr_ordem_venda = e_vbeln-vbeln.
      endif.

    when '2'.

      "MILHO EM GRAOS ADQ TERCEIROS
      "SOJA EM GRAOS ADQ TERCEIROS

      zcl_pedido_compra=>get_pedido_compra_chave_e(
        exporting
          i_lifnr               = conv #( zcl_string=>lpad( i_str = conv #( zde_zsdt0001cg_alv-id_branch ) i_qtd = 10 i_char = '0' ) )
          i_bukrs               = zde_zsdt0001cg_alv-id_bukrs    " Empresa
          i_charg               = conv #( zde_zsdt0001cg_alv-nr_safra )
          i_bstyp               = 'F'    " Categoria do documento de compras
          i_bsart               = 'ZUB'    " Tipo de documento de compras
          i_matnr_t             = value #( ( matnr = '000000000000119892' ) ( matnr = '000000000000119895' ) ( ) )
        receiving
          r_ekpo                = r_ekpo    " Item do documento de compras
        exceptions
          nao_encontrado_pedido = 1
          others                = 2
      ).

      if sy-subrc is not initial.
        message id sy-msgid type 'S' number sy-msgno with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      else.
        zde_zsdt0001ov_alv-nr_ordem_venda = r_ekpo-ebeln.
      endif.

  endcase.


endmodule.

*&---------------------------------------------------------------------*
*&      Form  RETORNAR_HTML_WORKFLOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZDE_ZSDT0001ACB_ALV  text
*      <--P_HTML_PAGINA  text
*----------------------------------------------------------------------*
form retornar_html_workflow  using    p_zsdt0001acb_alv type zde_zsdt0001acb_alv
                             changing p_html_pagina type string.

  data: tx_status_filial    type string,
        tx_status_fiscal    type string,
        tx_status_comercial type string.

  data: mt_status_filial    type string,
        mt_status_fiscal    type string,
        mt_status_comercial type string.

  data: cl_status_filial    type string,
        cl_status_fiscal    type string,
        cl_status_comercial type string.

  p_html_pagina = '<!DOCTYPE html>' && '<html>' && '<head>' && '<style>'.

*A  Solicitação Aprovada
*R  Solicitação Recusada
*W  Solicitação Em Espera de Aprovação
*S  Solicitação não gera Aprovação

  case p_zsdt0001acb_alv-tp_solicitacao_status.
    when zif_carga=>st_status_manut_aberto.
      cl_status_filial    = 'inicial'.
      cl_status_fiscal    = 'inicial'.
      cl_status_comercial = 'inicial'.
      tx_status_filial    = 'Solicitação de Manutenção não enviada'.
      tx_status_fiscal    = 'Solicitação de Manutenção não enviada'.
      tx_status_comercial = 'Solicitação de Manutenção não enviada'.
    when others.
      case p_zsdt0001acb_alv-rs_aceite_filial.
        when zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_filial = 'sem_aprovacao'.
          tx_status_filial = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> da Filial'.
        when zif_carga=>st_rs_aceite_manut_espera.
          cl_status_filial = 'enviado'.
          tx_status_filial = 'Solicitação de Manutenção <b>enviada para aprovação</b> pela Filial'.
        when zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_filial = 'aprovado'.
          tx_status_filial = 'Solicitação de Manutenção <b>aprovada</b> pela Filial'.
        when zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_filial = 'recusado'.
          tx_status_filial = 'Solicitação de Manutenção <b>recusada</b> pela Filial'.
          mt_status_filial = '<b>' && p_zsdt0001acb_alv-ds_aceite_filial && '</b>'.
      endcase.

      case p_zsdt0001acb_alv-rs_aceite_fiscal.
        when zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_fiscal = 'sem_aprovacao'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> do CSC Fiscal'.
        when zif_carga=>st_rs_aceite_manut_espera.
          cl_status_fiscal = 'enviado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>enviada para aprovação</b> do CSC Fiscal'.
        when zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_fiscal = 'aprovado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>aprovada</b> pelo CSC Fiscal'.
        when zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_fiscal = 'recusado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>recusada</b> pelo CSC Fiscal'.
          mt_status_fiscal = '<b>' && p_zsdt0001acb_alv-ds_aceite_fiscal && '</b>'.
      endcase.

      case p_zsdt0001acb_alv-rs_aceite_comercial.
        when zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_comercial = 'sem_aprovacao'.
          tx_status_comercial = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> do CSC Financeiro'.
        when zif_carga=>st_rs_aceite_manut_espera.
          cl_status_comercial = 'enviado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>enviada para aprovação</b> do CSC Financeiro'.
        when zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_comercial = 'aprovado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>aprovada</b> pelo do CSC Financeiro'.
        when zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_comercial = 'recusado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>recusada</b> pelo do CSC Financeiro'.
          mt_status_comercial = '<b>' && p_zsdt0001acb_alv-ds_aceite_comercial && '</b>'.
      endcase.
  endcase.

  p_html_pagina = p_html_pagina &&
 ' .inicial {' && ' background-color:LightGray;' && ' color:black;' &&' margin:10px;' && ' padding:10px;' && '}' &&
 ' .enviado {' && ' background-color:Orange;' &&' color:black;' &&' margin:10px;' && ' padding:10px;' && '}' &&
 ' .sem_aprovacao {' &&' background-color:DodgerBlue;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}' &&
 ' .aprovado {' &&' background-color:MediumSeaGreen;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}' &&
 ' .recusado {' &&' background-color:Tomato;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}'.

  p_html_pagina = p_html_pagina && '</style>' && '</head>' && '<body>'. " && '<table style="width:100%">' && '<tr>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_filial && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação Filial</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_filial && '</p>'.
  if mt_status_filial is not initial.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_filial && '</b></p>'.
  endif.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_fiscal && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação CSC Fiscal</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_fiscal && '</p>'.
  if mt_status_fiscal is not initial.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_fiscal && '</b></p>'.
  endif.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_comercial && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação CSC Financeira</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_comercial && '</p>'.
  if mt_status_comercial is not initial.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_comercial && '</b></p>'.
  endif.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '</body>' && '</html>'.

endform.

*&---------------------------------------------------------------------*
*&      Module  VALUE_HELP_CLASSIFICA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module value_help_classifica input.

*-CS2022001166-27.12.2022-#98820-JT-inicio - wpp COMENTADO 05/07/2023 Subir ajuste PRD
  html_pagina =
'<!DOCTYPE html>' &&
'<html>' &&
'<style>' &&
' .teste_amaggi { background-color:White; color:black; margin:5px; padding:5px; }' &&
' .teste_declarado { background-color:Lavender; color:black; margin:5px; padding:5px; }' &&
' .teste_monsanto { background-color:Beige; color:black; margin:5px; padding:5px; }' &&
' .teste_participante { background-color:SeaShell; color:black; margin:5px; padding:5px; }' &&
' .outros { background-color:WhiteSmoke; color:black; margin:5px; padding:5px; }' &&
'</style>' &&
'<body>' &&
'<div align="center"><p><h3>Ajuda preenchimento de transgenia</h3></p></div>' &&
'<div class="teste_amaggi">' &&
'<p>O campo <b>“Teste Amaggi”</b> deve ser utilizado/preenchido <b>apenas</b> para cargas de <b>Soja Convencional</b>. Nesses casos, o ticket/laudo de ' &&
'classificação que acompanha a carga deve estar assinalado pelo classificador com a informação de que o resultado do teste foi negativo, e portanto, o ' &&
'produto é convencional. No lançamento deve ser selecionada a opção <b>“Negativo”</b>. Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
'</div>' &&

'<div class="outros">' &&
'<div class="outros">' &&
'<p>Para os casos de recebimento de soja transgênico, é necessário identificar se é um produto que possui a tecnologia Intacta. ' &&
'Essa definição pode ocorrer da seguinte forma:</p>' &&
'</div>' &&
'<div class="teste_declarado">' &&
'<p><b>Caso tenha sido descrito (Declarado) pelo produtor/fornecedor na Nota Fiscal da carga que o produto é Intacta (RR2)</b>, ' &&
'não deve ser realizado nenhum teste do produto, e o lançamento, a única marcação que deve ser feita é assinalar “Sim” no campo <b>“Intac. ' &&
'Declarado (RR2)”</b>. Nenhum dos demais campos deve ser marcado pelo usuário. <b>Essa opção deve ser selecionada apenas se estiver ' &&
'explicitamente escrito na Nota Fiscal que o produto é Intacta/RR2</b>.</p>' &&
'</div>' &&
'<div class="teste_monsanto">' &&
'<p>Em caso de <b>embarque de soja depositado em um armazém de empresa que é Participante do Programa Monsanto, devidamente comprovado ' &&
'através de consulta no site da Monsanto</b>, também não é realizado teste do produto. No lançamento da carga o usuário deve selecionar ' &&
'a opção <b>“Sim” no campo “Outro Participante?”, vinculando no campo “Outro Participante” o nome/endereço do Participante</b>. Nenhum dos ' &&
'demais campos deve ser marcado pelo usuário. <b>Deve estar descrito na Nota Fiscal da carga que o produto foi embarcado em Armazém Participante</b>.</p>' &&
'</div>' &&
'<div class="teste_participante">' &&
'<p>Caso o produto <b>não seja embarcado em um armazém Participante do Programa Monsanto</b> e a na <b>Nota Fiscal da carga não esteja Declarado ' &&
'que o produto é Intacta (RR2), deve ser realizado o teste</b> para identificar se o produto possui ou não a tecnologia Intacta, e o resultado ' &&
'desse teste deve ser informado pelo classificador no ticket/laudo de classificação.</p>' &&
'<p><b>Se o resultado do teste for negativo</b>, o usuário deve selecionar a opção <b>“Negativo”</b> no campo <b>“Monsanto Intac.(RR2)”</b>. ' &&
'Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
'<p><b>Se o resultado do teste for positivo</b>, o usuário deve selecionar a opção <b>“Positivo”</b> no campo <b>“Monsanto Intac.(RR2)”</b>. ' &&
'Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
'</div>' &&
'</div>' &&
'</body>' &&
'</html>'.

*  html_pagina =
*'<!DOCTYPE html>' &&
*'<html>' &&
*'<style>' &&
*' .teste_amaggi { background-color:White; color:black; margin:5px; padding:5px; }' &&
*' .teste_declarado { background-color:Lavender; color:black; margin:5px; padding:5px; }' &&
*' .teste_monsanto { background-color:Beige; color:black; margin:5px; padding:5px; }' &&
*' .teste_participante { background-color:SeaShell; color:black; margin:5px; padding:5px; }' &&
*' .outros { background-color:WhiteSmoke; color:black; margin:5px; padding:5px; }' &&
*'</style>' &&
*'<body>' &&
*'<div align="center"><p><h3>Ajuda preenchimento de transgenia</h3></p></div>' &&
*'<div class="teste_amaggi">' &&
*'<p>O campo <b>“Teste de transgênia - convencional”</b> deve ser utilizado/preenchido <b>apenas</b> para cargas de <b>Soja Convencional</b>. Nesses casos, o ticket/laudo de ' &&
*'classificação que acompanha a carga deve estar assinalado pelo classificador com a informação de que o resultado do teste foi negativo, e portanto, o ' &&
*'produto é convencional. No lançamento deve ser selecionada a opção <b>“Negativo”</b>. Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
*'</div>' &&
*
*'<div class="outros">' &&
*'<div class="outros">' &&
*'<p>Para os casos de recebimento de soja transgênico, é necessário identificar se é um produto que possui a tecnologia de transgenia com patente valida. ' &&
*'Essa definição pode ocorrer da seguinte forma:</p>' &&
*'</div>' &&
*'<div class="teste_declarado">' &&
*'<p><b>Caso tenha sido descrito (Declarado) pelo produtor/fornecedor na Nota Fiscal da carga que o produto possui uma das tecnologias de transgenia com patente valida, </b>, ' &&
*'não deve ser realizado nenhum teste do produto, e o lançamento, a única marcação que deve ser feita é assinalar “Sim” no campo <b>“Uso de tecnologia com ' &&
*'patente válida Declarado”</b>. Nenhum dos demais campos deve ser marcado pelo usuário. <b>Essa opção deve ser selecionada apenas se estiver ' &&
*'explicitamente escrito na Nota Fiscal que o produto possui tecnologia de transgenia com patente válida.</p>' &&
*'</div>' &&
*'<div class="teste_monsanto">' &&
*'<p>Em caso de <b>embarque de soja depositado em um armazém de empresa que é Participante do Programa Bayer/Monsanto, devidamente comprovado ' &&
*'através de consulta no ITS</b>, também não é realizado teste do produto. No lançamento da carga o usuário deve selecionar ' &&
*'a opção <b>“Sim” no campo “Participante?”, vinculando o nome/endereço do Participante</b>. Nenhum dos ' &&
*'demais campos deve ser marcado pelo usuário. <b>Deve estar descrito na Nota Fiscal da carga que o produto foi embarcado em Armazém Participante</b>.</p>' &&
*'</div>' &&
*'<div class="teste_participante">' &&
*'<p>Caso o produto <b>não seja embarcado em um armazém Participante do Programa Bayer/Monsanto</b> e a na <b>Nota Fiscal da carga não esteja Declarado ' &&
*'que o produto possui tecnologia de transgenia com patente válida, deve ser realizado o teste</b> para identificar se o produto possui ou não a tecnologia Intacta, e o resultado ' &&
*'desse teste deve ser informado pelo classificador no ticket/laudo de classificação.</p>' &&
*'<p><b>Se o resultado do teste for negativo</b>, o usuário deve selecionar a opção <b>“Negativo”</b> no campo <b>“Teste de Tecnologia”</b>. ' &&
*'Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
*'<p><b>Se o resultado do teste for positivo</b>, o usuário deve selecionar a opção <b>“Positivo”</b> no campo <b>“Teste de Tecnologia”</b>. ' &&
*'Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
*'</div>' &&
*'</div>' &&
*'</body>' &&
*'</html>'.
*-CS2022001166-27.12.2022-#98820-JT-fim

  call screen 9006 starting at 05 05.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  SET_ORDEM_CARREG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module set_ordem_carreg input.

  check zde_zsdt0001cg_alv-nr_ordem       is not initial.
  check zde_zsdt0001ov_alv-nr_ordem_venda is not initial.



  try .

      objeto->verif_ordem_carregamento( ).

*      CATCH ZCX_CARGA.    "
*      CATCH ZCX_ORDEM_CARREGAMENTO.    "

*      DATA(R_ORDEM) =
*        ZCL_ORDEM_CARREGAMENTO=>BUSCA_ORDEM_CARREGAMENTO_NR(
*          EXPORTING
*            I_NR_SAFRA             = ZDE_ZSDT0001CG_ALV-NR_SAFRA
*            I_ID_BUKRS             = ZDE_ZSDT0001CG_ALV-ID_BUKRS
*            I_ID_BRANCH            = ZDE_ZSDT0001CG_ALV-ID_BRANCH
*            I_NR_ORDEM             = ZDE_ZSDT0001CG_ALV-NR_ORDEM ).
*
*
*      IF R_ORDEM-NR_SAFRA NE ZDE_ZSDT0001CG_ALV-NR_SAFRA.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_CARGA_SAFRA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_CARGA_SAFRA-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_CARGA_SAFRA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_CARGA_SAFRA-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_BUKRS NE ZDE_ZSDT0001CG_ALV-ID_BUKRS.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_CARGA_EMPRESA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_CARGA_EMPRESA-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_CARGA_EMPRESA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_CARGA_EMPRESA-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_BRANCH NE ZDE_ZSDT0001CG_ALV-ID_BRANCH.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_CARGA_FILIAL-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_CARGA_FILIAL-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_CARGA_FILIAL-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_CARGA_FILIAL-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DT_VALIDADE LT SY-DATLO AND R_ORDEM-DT_VALIDADE LT SY-DATUM
*          AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE
*          AND OBJETO->CK_EXECUTAR_MANUTENCAO_ENTRADA EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_VENCIDA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_VENCIDA-MSGNO ATTR1 = CONV #( R_ORDEM-DT_VALIDADE ) )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_VENCIDA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_VENCIDA-MSGNO
*            MSGV1  = CONV #( R_ORDEM-DT_VALIDADE ).
*      ENDIF.
*
*      DATA: LC_ID_AGENT_FRETE TYPE ZDE_ID_AGENT_FRETE.
*
*      LC_ID_AGENT_FRETE = R_ORDEM-ID_BRANCH_AG.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = LC_ID_AGENT_FRETE
*        IMPORTING
*          OUTPUT = LC_ID_AGENT_FRETE.
*
*      IF LC_ID_AGENT_FRETE                 NE ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE  AND
*         ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO AND
*         OBJETO->AT_MANUTENCAO  EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_EMPRESAAG-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_EMPRESAAG-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_EMPRESAAG-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_EMPRESAAG-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_PRODUTO NE ZDE_ZSDT0001CG_ALV-ID_PRODUTO.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_PRODUTO-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_PRODUTO-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_PRODUTO-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_PRODUTO-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DS_PLACA_TRATOR NE ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PLACA_TRATOR-MSGID MSGNO = ZCX_CARGA=>ZCX_PLACA_TRATOR-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_PLACA_TRATOR-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_PLACA_TRATOR-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DS_PLACA_REBOQ_1 NE ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1 AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PLACA_REBOQUE1-MSGID MSGNO = ZCX_CARGA=>ZCX_PLACA_REBOQUE1-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_PLACA_REBOQUE1-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_PLACA_REBOQUE1-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DS_PLACA_REBOQ_2 NE ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_2 AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PLACA_REBOQUE2-MSGID MSGNO = ZCX_CARGA=>ZCX_PLACA_REBOQUE2-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_PLACA_REBOQUE2-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_PLACA_REBOQUE2-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DS_PLACA_REBOQ_3 NE ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_3 AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PLACA_REBOQUE3-MSGID MSGNO = ZCX_CARGA=>ZCX_PLACA_REBOQUE3-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_PLACA_REBOQUE3-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_PLACA_REBOQUE3-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_MOTORISTA NE ZDE_ZSDT0001CG_ALV-ID_MOTORISTA AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_MOTORISTA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_MOTORISTA-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_MOTORISTA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_MOTORISTA-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_PROPRIETARIO NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_PROPRIETARIO-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_PROPRIETARIO-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_PROPRIETARIO-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_PROPRIETARIO-MSGNO.
*      ENDIF.
*
*      "Fornecedor
*      IF R_ORDEM-ID_LOCAL_DESTINO NE ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESTINO.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_DESTINO-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_DESTINO-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_DESTINO-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_DESTINO-MSGNO.
*      ENDIF.
*
*      "Destino
*      IF R_ORDEM-ID_LOCAL_DESCARGA NE ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESCARGA.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_DESCARGA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_DESCARGA-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_DESCARGA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_DESCARGA-MSGNO.
*      ENDIF.
    catch zcx_carga into ex_carga.
      ex_carga->published_erro( exporting i_msgty = 'E' i_msgty_display = 'E' ).
*    CATCH ZCX_ORDEM_CARREGAMENTO.
  endtry.

endmodule.

*&---------------------------------------------------------------------*
*&      Form  APRESENTA_RESTRICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_RESULTADO_RESTRI  text
*----------------------------------------------------------------------*
form apresenta_restricao  using e_restri type zde_pes_resultado_restricao.

  html_pagina =
'<!DOCTYPE html>' &&
'<html lang="en">' &&
'<head>' &&
'<title>CSS Template</title>' &&
'<meta charset="utf-8">' &&
'<meta name="viewport" content="width=device-width, initial-scale=1">' &&
'<style>' &&
'* {' &&
'    box-sizing: border-box;' &&
'}' &&
'' &&
'body {' &&
'    font-family: Arial, Helvetica, sans-serif;' &&
'}' &&
'' &&
'/* Style the header */' &&
'header {' &&
'    background-color: #666;' &&
'    padding: 30px;' &&
'    text-align: center;' &&
'    color: white;' &&
'}' &&
'' &&
'/* Create two columns/boxes that floats next to each other */' &&
'nav {' &&
'    float: left;' &&
'    width: 30%;' &&
'    height: 300px; /* only for demonstration, should be removed */' &&
'    background: #ccc;' &&
'    padding: 20px;' &&
'}' &&

'/* Style the list inside the menu */' &&
'nav ul {' &&
'    list-style-type: none;' &&
'    padding: 0;' &&
'}' &&

'article {' &&
'    float: left;' &&
'    padding: 20px;' &&
'    width: 100%;' &&
'    background-color: #f1f1f1;' &&
'    height: 300px; /* only for demonstration, should be removed */' &&
'}' &&

'/* Clear floats after the columns */' &&
'section:after {' &&
'    content: "";' &&
'    display: table;' &&
'    clear: both;' &&
'}' &&

'/* Style the footer */' &&
'footer {' &&
'    background-color: #777;' &&
'    padding: 10px;' &&
'    text-align: center;' &&
'    color: white;' &&
'}' &&

'/* Responsive layout - makes the two columns/boxes stack on top of each other instead of next to each other, on small screens */' &&
'@media (max-width: 600px) {' &&
'    nav, article {' &&
'        width: 100%;' &&
'        height: auto;' &&
'    }' &&
'}' &&
'</style>' &&
'</head>' &&
'<body>' &&

'<h2 style="text-align:center;color:Tomato;">Restrição de Embargo</h2>' &&

'<header>' &&
'<h4>' && e_restri-nome && '</h4>' &&
'</header>' &&

'<section>' &&
'  <article>' &&
    '<h1>' && e_restri-tipo && '</h1>' &&
    '<p>' && e_restri-motivo && '</p>' &&
'  </article>' &&
'</section>' &&

'<footer>' &&
'  <p>OPUS Crédito</p>' &&
'</footer>' &&

'</body>' &&
'</html>'.

  call screen 9006 starting at 05 05 ending at 85 28.


endform.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0309  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0309 output.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  loop at screen.
    split screen-name at '-' into: data(str10309) data(str20309).
    if str10309 eq 'ZDE_ZSDT0001CG_ALV' or str10309 eq 'ZDE_ZSDT0001OD_ALV' or str10309 eq 'ZDE_ZSDT0001OV_ALV'.
      if str20309 eq 'NM_PESO_SUBTOTAL' and  vg_tl_0307 ne '0307'.
        objeto->valida_atributo_alteravel( exporting i_campo = 'NM_PESO_BRUTO' importing e_permitido = data(e_permitido_0309) ).
        if e_permitido_0309 eq abap_false.
          screen-input = 0.
          modify screen.
        endif.
        continue.
      endif.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( exporting i_campo = conv #( str20309 ) importing e_permitido = e_permitido_0309 ).
      if e_permitido_0309 eq abap_false.
        screen-input = 0.
        modify screen.
      endif.
      if vg_tl_0307 = '0307'.
        check str20309 eq 'CK_CLASS_DEST'       or
              str20309 eq 'NR_TICKET'           or
              str20309 eq 'ID_CLASSIFICADORA'   or
              str20309 eq 'IN_GMO'              or
              str20309 eq 'TP_TRANSGENIA'       or
              str20309 eq 'IN_SRR_DECLARADO_2'  or          "BUG174959
              str20309 eq 'NM_PESO_BRUTO'       or
              str20309 eq 'NM_PESO_TARA'        or
              str20309 eq 'NM_PESO_SUBTOTAL'    or
              str20309 eq 'NR_PERC_UMI'         or
              str20309 eq 'NR_QTDE_UMI'         or
              str20309 eq 'NR_PERC_AVA'         or
              str20309 eq 'NR_QTDE_AVA'         or
              str20309 eq 'NR_PERC_QUE'         or
              str20309 eq 'NR_QTDE_QUE'         or
              str20309 eq 'NR_PERC_CAR'         or
              str20309 eq 'NR_QTDE_CAR'         or
              str20309 eq 'NR_PERC_IMP'         or
              str20309 eq 'NR_QTDE_IMP'         or
              str20309 eq 'NR_PERC_ARD'         or
              str20309 eq 'NR_QTDE_ARD'         or
              str20309 eq 'NR_PERC_ESV'         or
              str20309 eq 'NR_QTDE_ESV'.
        screen-input = 0.
        modify screen.
      endif.

    endif.

    if screen-name eq 'ZDE_ZSDT0001CG_ALV-ID_CLASSIFICADORA'.
      if zde_zsdt0001cg_alv-ck_class_dest is not initial.
        screen-input = 0.
        modify screen.
      endif.
    endif.

  endloop.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0311  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0311 output.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  if objeto->carga-tp_status eq zif_carga=>st_status_aberto.
    vg_tl_0312 = '0312'.
  else.
    vg_tl_0312 = '0313'.
  endif.

  loop at screen.
    split screen-name at '-' into: data(str10311) data(str20311).
    if str10311 eq 'ZDE_ZSDT0001CG_ALV' or str10311 eq 'ZDE_ZSDT0001OD_ALV' or str10311 eq 'ZDE_ZSDT0001OV_ALV'.
      if str20311 eq 'NM_PESO_SUBTOTAL'.
        objeto->valida_atributo_alteravel( exporting i_campo = 'NM_PESO_BRUTO' importing e_permitido = data(e_permitido_0311) ).
        if e_permitido_0311 eq abap_false.
          screen-input = 0.
          modify screen.
        endif.
        continue.
      endif.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( exporting i_campo = conv #( str20311 ) importing e_permitido = e_permitido_0311 ).
      if e_permitido_0311 eq abap_false.
        screen-input = 0.
        modify screen.
      endif.

    endif.
  endloop.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9012  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_9012 output.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  if ctl_cccontainer_9012 is initial.

    create object ctl_cccontainer_9012
      exporting
        container_name = 'IMAGEM'.

    create object picture_9012
      exporting
        parent = ctl_cccontainer_9012
      exceptions
        error  = 1.

    call method picture_9012->set_display_mode
      exporting
        display_mode = picture_9012->display_mode_stretch
      exceptions
        error        = 1.

    perform load_pic_from_db using picture_9012.

  endif.

endmodule.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
form handle_double_click_0306  using p_row type lvc_s_row.

  data: lc_row type lvc_t_row.

  check p_row-index is not initial.

  if p_row-rowtype is initial.

    append p_row to lc_row.

    call method ctl_alv_0306->set_selected_rows
      exporting
        it_index_rows = lc_row.

    read table it_notas index p_row-index into wa_nota_selecionada.
    "US 143677 - transgenia por nota
    if wa_nota_selecionada-id_nota = '000001'. "a principal nao edita
      exit.
    endif.

    clear: event_handler_0312a.

    if ctl_alv_0312a is not initial.
      ctl_alv_0312a->free( ).
    endif.
    clear: ctl_alv_0312a.

    clear: obg_toolbar_0312a.

    if ctl_cccontainer_0312a is not initial.
      ctl_cccontainer_0312a->free( ).
    endif.
    clear: ctl_cccontainer_0312a.

    clear: it_takes_saldo[].

    clear zde_zsdt0001cg_aux.
    ck_alterado_gmo = abap_false.
    move-corresponding wa_nota_selecionada to zde_zsdt0001nt_alv.
    if wa_nota_selecionada-id_classificacao is not initial and wa_nota_selecionada-id_carga is not initial.
      objeto->get_classificacao( importing e_registro = data(r_classificacao_principal) ).
      objeto->get_classificao_notas( exporting i_id_carga = wa_nota_selecionada-id_carga i_id_classificacao = wa_nota_selecionada-id_classificacao importing e_classificacao = data(r_classificacao) ).

      if r_classificacao is not initial.
        move-corresponding zde_zsdt0001cg_alv to zde_zsdt0001cg_aux.
        move-corresponding r_classificacao to zde_zsdt0001cg_alv.
      endif.

      call screen 0307 starting at 40 05.
      clear vg_tl_0307.

      objeto->set_classificacao( exporting i_classificacao = r_classificacao_principal ).
      move-corresponding r_classificacao_principal to zde_zsdt0001cg_alv.

      if zde_zsdt0001cg_aux is not initial.
*        move-corresponding zde_zsdt0001cg_aux to zde_zsdt0001cg_alv.
      endif.
    else.
      message s024(sd) with text-110  display like 'E'.
    endif.

    leave to screen 0300.

  endif.

endform.

"ZMM0127 Imputar planilha - Auto_Preenchimento - BG #131076 - INICIO
**********************************************************************
* carregar arquivo
**********************************************************************
form f_carrega_arquivo.


  data: l_erro type char1,
        l_cols type i.



  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 50
      text       = text-130.

*----------------------------------------
* upload excel
*----------------------------------------
  call function 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    exporting
      filename                = r_value
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 27
      i_end_row               = 30000
*     i_end_col               = 256
*     i_end_row               = 65536
    tables
      intern                  = t_tab
    exceptions
      inconsistent_parameters = 1
      upload_ole              = 2
      others                  = 3.

  if sy-subrc <> 0.
    message s024(sd) with text-109  display like 'E'.
    return.
  endif.

*----------------------------------------
* carrega tabela interna
*----------------------------------------
  free: l_erro, l_cols.

  loop at t_tab into w_tab.

    l_cols = l_cols + 1.
    assign component w_tab-col of structure wa_file to <fs_fld>.
    <fs_fld> = w_tab-value.
    at end of row.

      if l_cols <> 27 and  w_tab-row = 0001.
        l_erro = abap_true.
      endif.
      append wa_file to t_file.
      clear wa_file.
      free l_cols.
    endat.
  endloop.


  if l_erro = abap_true.
    message s024(sd) with text-115 text-116 display like 'E'.
    stop.
  endif.

endform.

form f_processa_arquivo.


  data: l_tabix           type sy-tabix,
        wa_nota_adicional type zde_zsdt0001nt_alv.
  clear: wa_file.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = 90
      text       = text-130.

  delete t_file index 1.


  "LOOP AT t_file INTO wa_file.

  read table  t_file into wa_file index 1.
  if sy-subrc is initial.
    l_tabix = sy-tabix.


*------------------------------
*---qtde
*------------------------------
    "PERFORM f_change_text CHANGING wa_file-qtde.


    replace all occurrences of ',' in wa_file-nm_peso_bruto with '.'.
    replace all occurrences of ',' in wa_file-nm_peso_tara with '.'.
    replace all occurrences of ',' in wa_file-nm_peso_subtotal with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_umi with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_imp with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_ava with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_ard with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_que with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_esv with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_car with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_pic with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_fer with '.'.
    replace all occurrences of ',' in wa_file-nr_perc_ges with '.'.

    zde_zsdt0001ov_alv-nr_ordem_venda  = wa_file-nr_ordem_venda.

    "Ordem de venda-------------------------------------------
    if zde_zsdt0001ov_alv-nr_ordem_venda is not initial.
      try .
          data(lc_id_produtos) = zde_zsdt0001cg_alv-id_produto.

          call function 'CONVERSION_EXIT_ALPHA_INPUT'
            exporting
              input  = zde_zsdt0001ov_alv-nr_ordem_venda
            importing
              output = zde_zsdt0001ov_alv-nr_ordem_venda.


          objeto->set_ordem_venda( exporting i_ordem_venda        = zde_zsdt0001ov_alv-nr_ordem_venda
                                   importing e_carga              = data(lc_saida_cargas)
                                   changing  c_zde_zsdt0001ov_alv = zde_zsdt0001ov_alv
                                           ).

          zde_zsdt0001cg_alv-id_produto        = lc_saida_cargas-id_produto.
          zde_zsdt0001cg_alv-ds_produto        = lc_saida_cargas-ds_produto.
          zde_zsdt0001cg_alv-id_local_coleta   = lc_saida_cargas-id_local_coleta.
          zde_zsdt0001cg_alv-id_local_descarga = lc_saida_cargas-id_local_descarga.
          zde_zsdt0001cg_alv-id_local_destino  = lc_saida_cargas-id_local_destino.
          zde_zsdt0001cg_alv-id_agent_frete    = lc_saida_cargas-id_agent_frete.
          zde_zsdt0001cg_alv-ds_local_coleta   = lc_saida_cargas-ds_local_coleta.
          zde_zsdt0001cg_alv-ds_local_descarga = lc_saida_cargas-ds_local_descarga.
          zde_zsdt0001cg_alv-ds_local_destino  = lc_saida_cargas-ds_local_destino.
          zde_zsdt0001cg_alv-ds_agent_frete    = lc_saida_cargas-ds_agent_frete.
          zde_zsdt0001cg_alv-in_transferencia  = lc_saida_cargas-in_transferencia.
          zde_zsdt0001cg_alv-ck_gera_aviso     = lc_saida_cargas-ck_gera_aviso.
          zde_zsdt0001cg_alv-ck_frete_entrada  = lc_saida_cargas-ck_frete_entrada.

          if lc_id_produto ne zde_zsdt0001cg_alv-id_produto.
            perform ajusta_descontos.
          endif.

        catch zcx_carga into ex_carga.
          ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
        catch zcx_parceiros into ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
      endtry.
    endif.
    "Ordem de venda-------------------------------------------

    zde_zsdt0001cg_alv-id_local_entrega = wa_file-id_local_entrega.
    if zde_zsdt0001cg_alv-id_local_entrega is not initial.
      select single ds_local_entrega
        into zde_zsdt0001cg_alv-ds_local_entrega
        from zsdt0001le where id_local_entrega eq zde_zsdt0001cg_alv-id_local_entrega.
    endif.

    "ordem carregamento----------------------------------
    zde_zsdt0001cg_alv-nr_ordem = wa_file-nr_ordem.
    try .
        objeto->set_ordem_carregamento(
          exporting
            i_nr_safra          = zde_zsdt0001cg_alv-nr_safra
            i_id_bukrs          = zde_zsdt0001cg_alv-id_bukrs
            i_id_branch         = zde_zsdt0001cg_alv-id_branch
            i_nr_ordem          = zde_zsdt0001cg_alv-nr_ordem
          importing
            e_ordem_carrgamento = zde_zsdt0001od_alv
          changing
            i_carga_alv         = zde_zsdt0001cg_alv ).

      catch zcx_ordem_carregamento into data(ex_ordem_carregamento).
        ex_ordem_carregamento->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
      catch zcx_parceiros into ex_parceiros.
        ex_parceiros->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    endtry.
    "ordem carregamento----------------------------------

    zde_zsdt0001cg_alv-nr_ticket = wa_file-nr_ticket.

    zde_zsdt0001cg_alv-nm_peso_bruto = wa_file-nm_peso_bruto.
    zde_zsdt0001cg_alv-nm_peso_tara = wa_file-nm_peso_tara.
    zde_zsdt0001cg_alv-nm_peso_subtotal = wa_file-nm_peso_subtotal.
    zde_zsdt0001cg_alv-id_classificadora = wa_file-id_classificadora.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_file-id_classificadora
      importing
        output = wa_file-id_classificadora.

    if wa_file-id_classificadora is not initial.
      select single name1
        into zde_zsdt0001cg_alv-ds_classificadora
        from lfa1 where lifnr eq wa_file-id_classificadora.
    endif.
    try .

        objeto->set_transgenia(
          changing
            i_in_gmo               = zde_zsdt0001cg_alv-in_gmo
            i_nr_resultado_01      = zde_zsdt0001cg_alv-nr_resultado_01
            i_nr_resultado_02      = zde_zsdt0001cg_alv-nr_resultado_02
            "i_nr_resultado_03       = zde_zsdt0001cg_alv-nr_resultado_03 "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
            "i_nr_resultado_04       = zde_zsdt0001cg_alv-nr_resultado_04 "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
            "i_nr_fita               = zde_zsdt0001cg_alv-nr_fita         "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
            i_nr_res_rr1_rr2       = zde_zsdt0001cg_alv-nr_res_rr1_rr2
            i_in_gmo_03            = zde_zsdt0001cg_alv-in_gmo_03
            i_in_srr_origem_partic = zde_zsdt0001cg_alv-in_srr_origem_partic
            i_id_outro_partic      = zde_zsdt0001cg_alv-id_outro_partic
            i_in_srr_declarado     = zde_zsdt0001cg_alv-in_srr_declarado
            i_in_teste_srr         = zde_zsdt0001cg_alv-in_teste_srr
            i_in_srr_declarado_2   = zde_zsdt0001cg_alv-in_srr_declarado_2
            i_in_teste_srr_2       = zde_zsdt0001cg_alv-in_teste_srr_2
            i_id_classificadora    = zde_zsdt0001cg_alv-id_classificadora
            i_id_ck_class_dest     = zde_zsdt0001cg_alv-ck_class_dest
            i_tp_transgenia        = zde_zsdt0001cg_alv-tp_transgenia ).
      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    endtry.




    zde_zsdt0001cg_alv-nr_perc_umi = wa_file-nr_perc_umi.
    zde_zsdt0001cg_alv-nr_perc_imp = wa_file-nr_perc_imp.
    zde_zsdt0001cg_alv-nr_perc_ava = wa_file-nr_perc_ava.
    zde_zsdt0001cg_alv-nr_perc_ard = wa_file-nr_perc_ard.
    zde_zsdt0001cg_alv-nr_perc_que = wa_file-nr_perc_que.
    zde_zsdt0001cg_alv-nr_perc_esv = wa_file-nr_perc_esv.
    zde_zsdt0001cg_alv-nr_perc_car = wa_file-nr_perc_car.
    zde_zsdt0001cg_alv-nr_perc_ava_pic = wa_file-nr_perc_pic.
    zde_zsdt0001cg_alv-nr_perc_ava_fer = wa_file-nr_perc_fer.
    zde_zsdt0001cg_alv-nr_perc_ava_ges = wa_file-nr_perc_ges.
    zde_zsdt0001nt_alv-id_entrada = wa_file-id_entrada.

    if zde_zsdt0001nt_alv-id_entrada is not initial.
      select single ds_entrada
          into zde_zsdt0001nt_alv-ds_entrada
          from zsdt0001tetx where id_entrada eq zde_zsdt0001nt_alv-id_entrada.
    endif.
    zde_zsdt0001nt_alv-id_mod_fiscal = wa_file-id_mod_fiscal.
    zde_zsdt0001nt_alv-nr_chave_nfe = wa_file-nr_chave_nfe.
    wa_add_nfe_9002-n55_chave_acesso = wa_file-nr_chave_nfe.


    "Nota Fiscal--------------------------------------------
    perform buscar_indo_nota_digitada.
    zde_zsdt0001nt_alv-dt_emissao         = wa_add_nfe_9002-dt_emissao  .
    zde_zsdt0001nt_alv-nr_nota            = wa_add_nfe_9002-numero.
    zde_zsdt0001nt_alv-nm_serie           = wa_add_nfe_9002-serie.
    zde_zsdt0001nt_alv-id_fornecedor = wa_add_nfe_9002-parid.
    zde_zsdt0001nt_alv-nr_fornecedor_ie   = wa_add_nfe_9002-parid_ie.
    zde_zsdt0001nt_alv-ds_fornecedor      = wa_add_nfe_9002-butxt.
    zde_zsdt0001nt_alv-nr_valor           = wa_add_nfe_9002-nftot.
    zde_zsdt0001nt_alv-nr_quantidade      = wa_add_nfe_9002-ntgew.
    zde_zsdt0001nt_alv-cfop               = wa_add_nfe_9002-cfop.
    try .
        objeto->add_nota_fiscal( exporting i_nota = zde_zsdt0001nt_alv importing e_nota = zde_zsdt0001nt_alv ).
        read table it_notas with key id_nota = zde_zsdt0001nt_alv-id_nota assigning field-symbol(<fs_nota>).
        if sy-subrc is initial.
          <fs_nota> = zde_zsdt0001nt_alv.
        else.
          append zde_zsdt0001nt_alv to it_notas.
        endif.
        ck_alterado_nota = abap_false.
      catch zcx_parceiros into ex_parceiros.    "
        ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      catch zcx_carga into ex_carga.    "
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    endtry.

    if wa_file-id_entrada1 is not initial.
      wa_nota_adicional-id_entrada = wa_file-id_entrada1.
      wa_nota_adicional-id_mod_fiscal = wa_file-id_mod_fiscal1.
      wa_nota_adicional-nr_chave_nfe = wa_file-nr_chave_nfe1.
      try .
          objeto->add_nota_fiscal( exporting i_nota = wa_nota_adicional importing e_nota = wa_nota_adicional ).
          read table it_notas with key id_nota = wa_nota_adicional-id_nota assigning field-symbol(<fs_nota1>).
          if sy-subrc is initial.
            <fs_nota1> = wa_nota_adicional.
          else.
            append wa_nota_adicional to it_notas.
          endif.
          ck_alterado_nota = abap_false.
        catch zcx_parceiros into ex_parceiros.    "
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_carga into ex_carga.    "
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      endtry.

    endif.

    if wa_file-id_entrada2 is not initial.
      wa_nota_adicional-id_entrada = wa_file-id_entrada2.
      wa_nota_adicional-id_mod_fiscal = wa_file-id_mod_fiscal2.
      wa_nota_adicional-nr_chave_nfe = wa_file-nr_chave_nfe2.

      try .
          objeto->add_nota_fiscal( exporting i_nota = wa_nota_adicional importing e_nota = wa_nota_adicional ).
          read table it_notas with key id_nota = wa_nota_adicional-id_nota assigning field-symbol(<fs_nota2>).
          if sy-subrc is initial.
            <fs_nota2> = wa_nota_adicional.
          else.
            append wa_nota_adicional to it_notas.
          endif.
          ck_alterado_nota = abap_false.
        catch zcx_parceiros into ex_parceiros.    "
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        catch zcx_carga into ex_carga.    "
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      endtry.

    endif.
    "Nota Fiscal--------------------------------------------
    try.
        objeto->set_carga( exporting i_carga = zde_zsdt0001cg_alv importing e_carga_recebimento = zde_zsdt0001cg_alv ).
        clear: ck_alterado_carga.
      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        perform seta_campo using ex_carga->msgid ex_carga->msgno.
    endtry.

    try .
        objeto->set_transgenia(
          changing
            i_in_gmo               = zde_zsdt0001cg_alv-in_gmo
            i_nr_resultado_01      = zde_zsdt0001cg_alv-nr_resultado_01
            i_nr_resultado_02      = zde_zsdt0001cg_alv-nr_resultado_02
*           i_nr_resultado_03      = zde_zsdt0001cg_alv-nr_resultado_03 "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
            "i_nr_resultado_04       = zde_zsdt0001cg_alv-nr_resultado_04 "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
            "i_nr_fita               = zde_zsdt0001cg_alv-nr_fita         "-CS2022001166-27.12.2022-#98820-JT -- wpp COMENTADO 05/07/2023 Subir ajuste PRD
            i_nr_res_rr1_rr2       = zde_zsdt0001cg_alv-nr_res_rr1_rr2
            i_in_gmo_03            = zde_zsdt0001cg_alv-in_gmo_03
            i_in_srr_origem_partic = zde_zsdt0001cg_alv-in_srr_origem_partic
            i_id_outro_partic      = zde_zsdt0001cg_alv-id_outro_partic
            i_in_srr_declarado     = zde_zsdt0001cg_alv-in_srr_declarado
            i_in_teste_srr         = zde_zsdt0001cg_alv-in_teste_srr
            i_in_srr_declarado_2   = zde_zsdt0001cg_alv-in_srr_declarado_2
            i_in_teste_srr_2       = zde_zsdt0001cg_alv-in_teste_srr_2
            i_id_classificadora    = zde_zsdt0001cg_alv-id_classificadora
            i_id_ck_class_dest     = zde_zsdt0001cg_alv-ck_class_dest
            i_tp_transgenia        = zde_zsdt0001cg_alv-tp_transgenia ).
      catch zcx_carga into ex_carga.
        ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    endtry.
  endif.

endform.

form get_excel.

  data: it_file type filetable,
        l_subrc type i.

  cl_gui_frontend_services=>file_open_dialog( exporting default_filename = ' '
                                                        file_filter      = '*.xls'
                                              changing  file_table       = it_file
                                                        rc               = l_subrc
                                                      ).
  try .
      r_value = it_file[ 1 ].
    catch cx_sy_itab_line_not_found.
      clear r_value.
  endtry.

endform.

form z_popup.

  types text   type c length 85.
  data  itab   type table of text.

  data: l_text_table like tline occurs 3,
        l_tdline     like line of l_text_table.
*
*  append 'Utilizar o arquivo excel disponibilizado como modelo, seguindo a sequência abaixo:' to itab.
*  append 'Campo 1: Ordem de Venda'        to itab.
*  append 'Campo 2: Id.Lc. Entrega'        to itab.
*  append 'Campo 3: Ord. Carregamento'     to itab.
*  append 'Campo 4: Ticket'                to itab.
*  append 'Campo 5: Pedo Bruto'            to itab.
*  append 'Campo 6: Peso Tara'             to itab.
*  append 'Campo 7: Peso SubTo'            to itab.
*  append 'Campo 8: Classificadora'        to itab.
*  append 'Campo 9: %Umi'                  to itab.
*  append 'Campo 10: %Imp'                 to itab.
*  append 'Campo 11: %Ava'                 to itab.
*  append 'Campo 12: %Ard'                 to itab.
*  append 'Campo 13: %Que'                 to itab.
*  append 'Campo 14: %Esv'                 to itab.
*  append 'Campo 15: %Car'                 to itab.
*  append 'Campo 16: %Picados'             to itab.
*  append 'Campo 17: %Ferentados'          to itab.
*  append 'Campo 18: %Gessados'            to itab.
*  append 'Campo 19: Tp.Ent.'              to itab.
*  append 'Campo 20: Modelo NF    Campo 21: Chave NF-e'            to itab.




  concatenate space space 'Utilizar o arquivo excel disponibilizado como modelo, seguindo a sequência abaixo:'  into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 1: Ordem de Venda'        into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 2: Id.Lc. Entrega'        into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 3: Ord. Carregamento'     into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 4: Ticket'                into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 5: Pedo Bruto'            into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 6: Peso Tara'             into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 7: Peso SubTo'            into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 8: Classificadora'        into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 9: %Umi'                  into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 10: %Imp'                 into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 11: %Ava'                 into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 12: %Ard'                 into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 13: %Que'                 into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 14: %Esv'                 into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 15: %Car'                 into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 16: %Picados'             into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 17: %Ferentados'          into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 18: %Gessados'            into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 19: Tp.Ent.'              into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  concatenate space space 'Campo 20: Modelo NF'            into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.

  concatenate space space 'Campo 21: Chave NF-e'           into l_tdline separated by space.
  append l_tdline to l_text_table.
  clear l_tdline.
  .

  call function 'COPO_POPUP_TO_DISPLAY_TEXTLIST'
    exporting
*     TASK       = 'DISPLAY'
      titel      = 'Importar Planilha'
*       IMPORTING
*     FUNCTION   =
    tables
      text_table = l_text_table.

endform.


"ZMM0127 Imputar planilha - Auto_Preenchimento - BG #131076 - FIM

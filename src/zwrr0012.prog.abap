*&---------------------------------------------------------------------*
*& Report  ZWRR0012
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zwrr0012.
types:
  begin of ty_fields,
    campo(30) type c,
    group1(5) type c,
    value     type sy-tabix,
    invisible type sy-tabix,
  end   of ty_fields,

  begin of ty_editor,
    line(72),
  end   of ty_editor,

  begin of ty_estra ,
    operacao  type zfi_estrategia_znfw-operacao,
    dep_resp  type zfi_estrategia_znfw-dep_resp,
    aprovador type zfi_estrategia_znfw-aprovador,
    nivel     type zfi_estrategia_znfw-nivel,
    estado    type zfi_estrategia_znfw-estado,
    opcoes    type zfi_estrategia_znfw-opcoes,
  end of ty_estra,

  begin of ty_docs ,
    operacao   type zfi_docs_znfw-operacao,
    txt_compl  type zfi_docs_znfw-txt_compl,
    dep_resp   type zfi_docs_znfw-dep_resp,
    dt_ini_val type zfi_docs_znfw-dt_ini_val,
    dt_fim_val type zfi_docs_znfw-dt_fim_val,
  end of ty_docs.



*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
data: ok-code         type sy-ucomm,
      tg_selectedcell type lvc_t_cell,
      wg_selectedcell type lvc_s_cell.
types: begin of ty_operacoes,
         status(4),
         operacao  type zfi_operacoes_znfw-operacao,
         dep_resp  type zfi_operacoes_znfw-dep_resp,
         descricao type zfi_operacoes_znfw-descricao,
*        data(10),
         color(4),
       end of ty_operacoes.

data  dyfields like dynpread occurs 1 with header line.
data: tl_bdc type table of bdcdata,
      wl_bdc type bdcdata.

** Criação de tabela dinamica
data: t_fieldcatalog type lvc_t_fcat,
      w_fieldcatalog type lvc_s_fcat,
      wa_layout      type lvc_s_layo,
      wa_stable      type lvc_s_stbl,
      wg_editor      type ty_editor,
      wg_cadoperacao type zfi_operacoes_znfw,
      wa_estra       type ty_estra,
      wa_docs        type ty_docs,
      w_docs         type ty_docs,
      tg_operacoes   type standard table of ty_operacoes initial size 0,

      tg_fields      type table of ty_fields   with header line,

      tg_editor      type table of ty_editor,
      tg_estra       type table of ty_estra,
      tg_docs        type table of ty_docs,
      it_docs        type table of ty_docs,
      it_estra       type table of ty_estra,

      tg_msg_ret     type table of zfiwrs0001 with header line.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
constants: begin of c_tab_strip_imp,
             tab1 like sy-ucomm value 'TAB_STRIP_IMP_FC1',
             tab2 like sy-ucomm value 'TAB_STRIP_IMP_FC2',
             tab3 like sy-ucomm value 'TAB_STRIP_IMP_FC3',
           end of c_tab_strip_imp.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
controls:  tab_strip_imp type tabstrip.
data: begin of g_tab_strip_imp,
        subscreen   like sy-dynnr,
        prog        like sy-repid value 'ZWRR0012',
        pressed_tab like sy-ucomm value c_tab_strip_imp-tab1,
      end of g_tab_strip_imp.

data: ok_code          like sy-ucomm,
      wg_mensagem(30),
      wg_acao(30),
      vdt_apuracao(1),
      vmes_apuracao(1),
      vkokrs           type tka02-kokrs,
      xclasse(1),
      xmodif(1),
      vdep_resp(2),
      vpgt_forn(1),
*-CS2021000723 - 18.10.2021 - JT - inicio
      btn_rej(30)      type c.
*-CS2021000723 - 18.10.2021 - JT - fim

*Class definition for ALV toolbar
class:      lcl_alv_toolbar   definition deferred.
*            lcl_alv_toolbar2  definition deferred.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
data: g_container          type scrfname value 'CC_OPERACOES',
      g_custom_container   type ref to cl_gui_custom_container,
      container_1          type ref to cl_gui_container,       "splitter conteiner 1
      container_2          type ref to cl_gui_container,       "splitter conteiner 2
      splitter             type ref to cl_gui_splitter_container,
      grid1                type ref to cl_gui_alv_grid,
      grid2                type ref to cl_gui_alv_grid,
      grid3                type ref to cl_gui_alv_grid,
      obg_toolbar          type ref to lcl_alv_toolbar,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      g_custom_cont_desc   type ref to cl_gui_custom_container,
      obg_descbox          type ref to cl_gui_textedit,
      obg_docking          type ref to cl_gui_docking_container,
      obg_conteiner_estra  type ref to cl_gui_custom_container,
      obg_conteiner_docs   type ref to cl_gui_custom_container,
      g_cc_estra           type scrfname value 'CC_ESTRA',
      g_cc_docs            type scrfname value 'CC_DOCS',
      wa_style             type lvc_s_styl,
      style                type lvc_t_styl  with header line,
      style2               type lvc_t_styl with header line.

* alrs
*Declaration for toolbar buttons
data : ty_toolbar type stb_button.
*** TREE DE MENSAGENS.
data node_itab like node_str occurs 0.
data node like node_str.

data container type ref to cl_gui_custom_container.
data splitter_msg type ref to cl_gui_easy_splitter_container.
data right type ref to cl_gui_container.
data left  type ref to cl_gui_container.

data editor type ref to cl_gui_textedit.
data tree type ref to cl_gui_simple_tree.

data behaviour_left type ref to cl_dragdrop.
data behaviour_right type ref to cl_dragdrop.

data handle_tree type i.
data num_row type i value 0.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
constants: c_0               type c value '0',
           c_1               type c value '1',
           c_2               type c value '2',
           c_b               type c value 'B',
           c_s               type c value 'S',
           c_l               type c value 'L',
           c_x               type c value 'X',
           c_d               type c value 'D',
           c_k               type c value 'K',
           c_w               type c value 'W',
           c_f               type c value 'F',
           c_t               type c value 'T',
           c_i               type c value 'I',
           c_n               type c value 'N',
           c_h               type c value 'H',
           c_ag(2)           type c value 'AG',
           c_ne(2)           type c value 'NE',
           c_01(2)           type c value '01',
           c_30(2)           type c value '30',
           c_40(2)           type c value '40',
           c_50(4)           type c value '0050',
           c_76(2)           type c value '76',
           c_71(2)           type c value '71',
           c_72(2)           type c value '72',
           c_br(2)           type c value 'BR',
           c_lf(2)           type c value 'LF',
           c_lr(2)           type c value 'LR',
           c_z1(2)           type c value 'Z1',
           c_add(3)          type c value 'ADD',
           c_del(3)          type c value 'DEL',
           c_dg1(3)          type c value 'DG1',
           c_dg2(3)          type c value 'DG2',
           c_dummy_header(3) type c value '099',
           c_dummy_itens(3)  type c value '098',
           c_exit(4)         type c value 'EXIT',
           c_root(4)         type c value 'ROOT',
           c_minimizar(4)    type c value '@K2@',
           c_maximizar(4)    type c value '@K1@',
           c_back(4)         type c value 'BACK',
           c_save(4)         type c value 'SAVE',
           c_desat(5)        type c value 'DESAT',
           c_dmbtr(5)        type c value 'DMBTR',
           c_refresh(7)      type c value 'REFRESH',
           c_modif(5)        type c value 'MODIF',
           c_cancel(6)       type c value 'CANCEL',
           c_deldoc(6)       type c value 'DELDOC',
           c_displa(6)       type c value 'DISPLA',
           c_dclick(6)       type c value 'DCLICK',
           c_search(6)       type c value 'SEARCH',
           c_atuali(6)       type c value 'ATUALI',
           c_add_msg(7)      type c value 'ADD_MSG',
           c_del_msg(7)      type c value 'DEL_MSG',
           c_clos_msg(8)     type c value 'CLOS_MSG',
           c_save_msg(8)     type c value 'SAVE_MSG',
           c_show_msgre(10)  type c value 'SHOW_MSGRE'.

*ALRS
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
class lcl_event_handler definition.

  public section.
    class-methods:
      on_double_click for event double_click of cl_gui_alv_grid
        importing e_row e_column.
    class-methods:
      on_double_click2 for event double_click of cl_gui_alv_grid
        importing e_row e_column.

    class-methods:
      on_click for event hotspot_click  of cl_gui_alv_grid
        importing e_row_id e_column_id es_row_no.

    class-methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    class-methods:
      on_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.


endclass.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_treeobject DEFINITION
*---------------------------------------------------------------------*
*       Definition of Data Container                                  *
*---------------------------------------------------------------------*
class lcl_drag_object definition.
  public section.
    data text type mtreesnode-text.
endclass.                    "lcl_drag_object DEFINITION
*---------------------------------------------------------------------*
*       CLASS dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
class lcl_dragdrop_receiver definition.
  public section.
    methods:
      node_double_click for event node_double_click of cl_gui_simple_tree
        importing node_key.

endclass.                    "lcl_dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class lcl_alv_toolbar definition.
  public section.
*Constructor
    methods: constructor
      importing io_alv_grid type ref to cl_gui_alv_grid,
*Event for toolbar
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.
endclass.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
class lcl_alv_toolbar implementation.
  method constructor.
*   Create ALV toolbar manager instance
    create object c_alv_toolbarmanager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.                    "constructor

  method on_toolbar.
    data: wl_desactive.

    if wg_acao ne c_modif.
      wl_desactive = 1.
    endif.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.


    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-butn_type = 3.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    call method c_alv_toolbarmanager->reorganize
      exporting
        io_alv_toolbar = e_object.
  endmethod.                    "on_toolbar
  method handle_user_command.


  endmethod.                    "zm_handle_user_command

endclass.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_event_handler implementation.
* Método de  execução para Duplo-click
  method on_double_click.
    data: wl_operacoes like line of tg_operacoes,
          vflg_ico(1),
          tl_function  type ui_functions,
          wl_function  like tl_function.


    clear vdep_resp.
    if e_row gt 0.
      clear: wg_cadoperacao.
      refresh tg_estra.
      refresh tg_docs.
      read table tg_operacoes into wl_operacoes index e_row.
      if wl_operacoes-status = icon_alert.
        message 'O Lançamento selecionado esta com a data de vencimento no passado, não é permitido aprovar.' type 'I'.
        call function 'SAPGUI_SET_FUNCTIONCODE'
          exporting
            functioncode           = '=ENT'
          exceptions
            function_not_supported = 1
            others                 = 2.
        exit.
        call method grid2->refresh_table_display
          exporting
            is_stable = wa_stable.
        call method grid3->refresh_table_display
          exporting
            is_stable = wa_stable.
      endif.

      move-corresponding wl_operacoes to wg_cadoperacao.

      call function 'SAPGUI_SET_FUNCTIONCODE'
        exporting
          functioncode           = '=ENT'
        exceptions
          function_not_supported = 1
          others                 = 2.

      refresh tg_estra.
      loop at it_estra into wa_estra where operacao = wl_operacoes-operacao.
        append wa_estra to tg_estra.
      endloop.
      refresh tg_docs.
      loop at it_docs into wa_docs where operacao = wl_operacoes-operacao.
        append wa_docs to tg_docs.
      endloop.

      sort tg_estra by nivel.
      call method grid2->refresh_table_display
        exporting
          is_stable = wa_stable.

      call method grid3->refresh_table_display
        exporting
          is_stable = wa_stable.

    endif.
  endmethod.                    "ON_DOUBLE_CLICK

  method on_double_click2.
    data: wl_docs like line of tg_docs.
    data: opt        type ctu_params.
    if e_row-index gt 0.
      if e_column = 'OPERACAO'.
        read table tg_docs into wl_docs index e_row.
        data: bdcdata   type table of bdcdata,
              bdc_entry type bdcdata,
              options   type ctu_params,
              messtab   type table of bdcmsgcoll,
              tete      type num.

        clear bdcdata.

        bdc_entry-program = 'ZWRR0001'.
        bdc_entry-dynpro  = '0100'.
        bdc_entry-dynbegin = 'X'.
        append bdc_entry to bdcdata.

        clear bdc_entry.
        bdc_entry-fnam = 'ZFIWRT0001-OPERACAO'.
        bdc_entry-fval = wl_docs-operacao.
        append bdc_entry to bdcdata.

        clear bdc_entry.
        bdc_entry-fnam = 'BDC_OKCODE'.
        bdc_entry-fval = 'ATUALI'.
        append bdc_entry to bdcdata.

        call transaction 'ZNFW0001' using bdcdata mode 'E'.

*        SET PARAMETER ID 'OPERACAO' FIELD   wl_docs-operacao.
*        CALL TRANSACTION 'OPERACAO' AND SKIP FIRST SCREEN.
      endif.
    endif.
  endmethod.                    "ON_DOUBLE_CLICK2
  method on_click.

    data: v_msg        type char50,
          l_uname      type sy-uname,
          t_operacoes  type table of zfi_operacoes_znfw,
          w_operacoes  type          zfi_operacoes_znfw,
          wl_estra     like line of tg_estra,
          t_estra      type table of zfi_estrategia_znfw,
          w_estra      type          zfi_estrategia_znfw,
          t_docs       type table of zfi_docs_znfw,
          w_docs       type          zfi_docs_znfw,
          wl_operacoes like line of tg_operacoes.

    l_uname = sy-uname.

    if e_row_id gt 0.
      read table tg_estra into wl_estra index e_row_id.

      read table tg_operacoes into wl_operacoes with key operacao = wl_estra-operacao binary search.
      move-corresponding wl_operacoes to w_operacoes.
      append w_operacoes  to t_operacoes.

      loop at it_estra into wl_estra where operacao = wl_operacoes-operacao.
        move-corresponding wl_estra to w_estra.
        append w_estra to t_estra.
      endloop.

      call function 'Z_ZW_ESTRATEGIA_EXECUTAR'
        exporting
          v_usuario   = l_uname
        importing
          msg         = v_msg
        tables
          t_operacoes = t_operacoes
          t_estra     = t_estra.

      loop at t_estra into w_estra.
        if e_row_id = sy-tabix.
          move: w_estra-opcoes to wl_estra-opcoes,
                w_estra-estado to wl_estra-estado.
          modify tg_estra from wl_estra index sy-tabix transporting opcoes estado.
        endif.
      endloop.

      call method grid2->refresh_table_display
        exporting
          is_stable = wa_stable.

    endif.
  endmethod.                    "ON_CLICK

  method on_data_changed.

  endmethod.                    "ON_DATA_CHANGED

  method on_data_changed_finished.

  endmethod.                    "on_data_changed_finisheD

  "on_data_changed_finisheD
endclass.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
class lcl_dragdrop_receiver implementation.
  method node_double_click.

  endmethod.                    "drop_complete
endclass.                    "lcl_dragdrop_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module trata_fields output.

  loop at tg_fields.
    loop at screen.
      if screen-name eq tg_fields-campo
      or screen-group1 eq tg_fields-group1.
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        modify screen.
*        EXIT.
      endif.
    endloop.
  endloop.
endmodule.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  data: fcode type table of sy-ucomm.

  refresh: fcode.
  append c_save to fcode.
  set pf-status 'Z001' excluding fcode.
  call method cl_gui_cfw=>dispatch.
  set titlebar '0100'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos output.
  data: event       type cntl_simple_event,
        events      type cntl_simple_events,
        tl_filter   type lvc_t_filt,
        wl_filter   type lvc_s_filt,
        tl_function type ui_functions,
        wl_function like tl_function with header line.
  data: waref      type ref to data.

*-CS2021000723 - 18.10.2021 - JT - inicio
  btn_rej = text-b01.
*-CS2021000723 - 18.10.2021 - JT - fim

  if g_custom_container is initial.
*    WA_LAYOUT-CWIDTH_OPT = C_X.
    wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
    wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
    wa_stable-row        = c_x.
    wa_layout-info_fname = 'COLOR'.

    wa_layout-no_toolbar = c_x.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Operações disponíveis para Liberação'.

    create object g_custom_container
      exporting
        container_name = g_container.

    create object splitter
      exporting
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    call method splitter->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = container_1.

    create object grid1
      exporting
        i_parent = container_1.


    perform montar_layout.

    refresh tl_function.
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

    call method grid1->set_table_for_first_display
      exporting
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      changing
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_operacoes[].

    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
              lcl_event_handler=>on_double_click for grid1,
              lcl_event_handler=>on_click for grid1,
              lcl_event_handler=>on_data_changed_finished for grid1,
              lcl_event_handler=>on_data_changed for grid1.

*    posiciona spliter na altura x
    call method splitter->set_row_height
      exporting
        id     = 1
        height = 100.
  else.
    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  "GRID2
  if obg_conteiner_estra is initial.
    create object obg_conteiner_estra
      exporting
        container_name = g_cc_estra.


    create object grid2
      exporting
        i_parent = obg_conteiner_estra.


    perform montar_layout_estra.

    refresh: tl_function.
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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Estratégia de Liberação'.
    wa_layout-no_toolbar = c_x.
    perform montar_layout_estra.

    call method grid2->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_estra[].

    call method grid2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
     lcl_event_handler=>on_click for grid2.
    "LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK2 FOR GRID2.
  else.
    call method grid2->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  "GRID3
  if obg_conteiner_docs is initial.
    create object obg_conteiner_docs
      exporting
        container_name = g_cc_docs.


    create object grid3
      exporting
        i_parent = obg_conteiner_docs.


    perform montar_layout_docs.

    refresh: tl_function.
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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = 'Itens da Operação'.
    wa_layout-no_toolbar = c_x.
    perform montar_layout_docs.

    call method grid3->set_table_for_first_display
      exporting
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      changing
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_docs[].

    call method grid3->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
                  lcl_event_handler=>on_double_click2 for grid3.

  else.
    call method grid3->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.
endmodule.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 ' '                    ' '             'TG_OPERACOES' 'OPERACAO'          'Operacao'        '10' ' ' ' ' ' ',
        2 ' '                    ' '             'TG_OPERACOES' 'DEP_RESP'          'Departamento'    '10' ' ' ' ' ' ',
        3 ' '                    ' '             'TG_OPERACOES' 'DESCRICAO'         'Descrição'       '50' ' ' ' ' ' '.


endform.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form montar_estrutura using value(p_col_pos)       type i
                            value(p_ref_tabname)   like dd02d-tabname
                            value(p_ref_fieldname) like dd03d-fieldname
                            value(p_tabname)       like dd02d-tabname
                            value(p_field)         like dd03d-fieldname
                            value(p_scrtext_l)     like dd03p-scrtext_l
                            value(p_outputlen)
                            value(p_edit)
                            value(p_sum)
                            value(p_emphasize).

  clear w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  if p_outputlen is not initial.
    w_fieldcatalog-outputlen      = p_outputlen.
  endif.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  if p_field eq 'OPCOES'.
    w_fieldcatalog-hotspot = c_x.
  endif.

  append w_fieldcatalog to t_fieldcatalog.

endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.
  data: vflg_ico(1),
        l_uname     type sy-uname,
        wt_estra    type ty_estra.

  l_uname = sy-uname.

  case ok-code.
    when c_refresh.
      perform f_refresh.

*-CS2021000723 - 18.10.2021 - JT - inicio
    when 'REJ'.
      read table tg_estra into wa_estra with key aprovador = l_uname. "sy-uname.
      if sy-subrc = 0.
        btn_rej = text-b01.
        if  wa_estra-opcoes = icon_reject.
          wa_estra-opcoes = icon_set_state.
        elseif  wa_estra-opcoes = icon_set_state.
          wa_estra-opcoes = icon_reject.
        endif.
        modify tg_estra from wa_estra index sy-tabix transporting opcoes.
        read table it_estra into wt_estra with key operacao = wa_estra-operacao.
        if sy-subrc = 0.
          modify it_estra from wa_estra index sy-tabix.
        endif.
      endif.
*-CS2021000723 - 18.10.2021 - JT - fim

  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_exit input.
  case ok-code.
    when c_back.
      set screen 0.

    when c_exit.
      leave program.
  endcase.
endmodule.                 " USER_COMMAND_EXIT  INPUT

*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_estra .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 ' '                        ' '        'TG_ESTRA' 'NIVEL'            'Nível'             '10' ' ' ' ' ' ',
        2 ' '                        ' '        'TG_ESTRA' 'APROVADOR'        'Aprovador'         '12' ' ' ' ' ' ',
        3 ' '                        ' '        'TG_ESTRA' 'ESTADO'           'Estado'            '12' ' ' ' ' ' ',
        4 ' '                        ' '        'TG_ESTRA' 'OPCOES'           'Opções'            '12' ' ' ' ' ' '.

endform.                    " MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_docs .
  refresh t_fieldcatalog.

  perform montar_estrutura using:
        1 ' '           ' '         'TG_DOCS'  'OPERACAO'         'Operação'              '10' ' ' ' ' ' ',
        1 ' '           ' '         'TG_DOCS'  'TXT_COMPL'        'Texto Compl.'          '50' ' ' ' ' ' ',
        2 ' '           ' '         'TG_DOCS'  'DEP_RESP'         'Departamento'          '10' ' ' ' ' ' ',
        3 ' '           ' '         'TG_DOCS'  'DT_INI_VAL'       'Data Início'           '15' ' ' 'X' ' ',
        4 ' '           ' '         'TG_DOCS'  'DT_FIM_VAL'       'Data Fim'              '15' ' ' 'X' ' '.



endform.                    " MONTAR_LAYOUT_DOCS

*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
" MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*&      Module  CARREGA_operacoes  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module carrega_operacoes output.
  data xtotal type zglt036-vlr_moeda_int.

  if g_custom_container is initial.
    perform atualiza_operacoes.
  endif.


endmodule.                 " CARREGA_operacoes  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  dynp_values_update
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->US_REPID   text
*      -->US_DYNNR   text
*      -->US_FIELD   text
*      -->US_VALUE   text
*      -->CH_SUBRC   text
*----------------------------------------------------------------------*
form dynp_values_update using us_repid
                              us_dynnr
                              us_field
                              us_value
                     changing ch_subrc.

  data: da_dynpfield_tab like dynpread occurs 0 with header line,
        da_stepl         like sy-stepl,
        da_repid         like d020s-prog,
        da_dynnr         like d020s-dnum.

  ch_subrc = 4.
  refresh da_dynpfield_tab.

  move us_repid to da_repid.
  move us_dynnr to da_dynnr.

  get cursor line da_stepl.

  move da_stepl to da_dynpfield_tab-stepl.
  move us_field to da_dynpfield_tab-fieldname.
  move us_value to da_dynpfield_tab-fieldvalue.
  append da_dynpfield_tab.

  call function 'DYNP_VALUES_UPDATE'
    exporting
      dyname               = da_repid
      dynumb               = da_dynnr
    tables
      dynpfields           = da_dynpfield_tab
    exceptions
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      others               = 8.

  if sy-subrc eq 0.
    ch_subrc = 0.
  endif.

endform.                    " DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*&      Form  Atualiza_operacoes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form atualiza_operacoes .
  data: v_msg       type char50,
        l_uname     type sy-uname,
        t_operacoes type table of zfi_operacoes_znfw,
        w_operacoes type          zfi_operacoes_znfw,
        t_estra     type table of zfi_estrategia_znfw,
        w_estra     type          zfi_estrategia_znfw,
        t_docs      type table of zfi_docs_znfw,
        w_docs      type          zfi_docs_znfw,
        vdata(10).

  l_uname = sy-uname.

  call function 'Z_ZW_ESTRATEGIA_LISTA'
    exporting
      v_usuario   = l_uname
    importing
      msg         = v_msg
    tables
      t_operacoes = t_operacoes
      t_estra     = t_estra
      t_docs      = t_docs.

  refresh: tg_operacoes, it_estra, it_docs.

  move-corresponding t_operacoes to tg_operacoes.

  sort tg_operacoes by operacao.

  loop at t_estra into w_estra.
    move-corresponding w_estra to wa_estra.
    append wa_estra to it_estra.
  endloop.

  sort it_estra by operacao .

  loop at t_docs into w_docs.
    move-corresponding w_docs to wa_docs.
    append wa_docs to it_docs.
  endloop.

  sort it_docs by operacao .

  if g_custom_container is not  initial.
    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  if obg_conteiner_estra is not initial.
    call method grid2->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.

  if obg_conteiner_docs is not initial.
    call method grid3->refresh_table_display
      exporting
        is_stable = wa_stable.
  endif.


endform.                    " Atualiza_operacoes

*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form envia_email tables tg_estra using value(wg_cadoperacao) type zfi_operacoes_znfw plinha  .

  field-symbols: <fs_solix> type solix.

* Objetos para enviar email
  data: objpack     like sopcklsti1 occurs  2 with header line.
  data: objhead     like solisti1   occurs  1 with header line.
  data: objbin_ord  like solisti1   occurs 10 with header line.
  data: objbin_log  like solisti1   occurs 10 with header line.
  data: objbin_ann  type solisti1.
  data: objbin    like solisti1   occurs 10 with header line,
        objbin1   type soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin like line of objbin.
  data: content_hex type standard table of solix with header line.
  data: objtxt      like solisti1   occurs 10 with header line.
  data: reclist     like somlreci1  occurs  5 with header line.
  data: doc_chng    like sodocchgi1.
  data: tab_lines   like sy-tabix.
  data: l_anex      type string.
  data: l_leng      type i.
  data: l_arq       type string.
  data: l_tam       type i.
  data: l_tam_ord   type i.
  data: l_tam_log   type i.
  data: l_email(300) type c.
  data: vlinha      type i.
  data: vuser       type sy-uname.
  data: it_shortcut_param like zst_shortcut_par occurs 0 with header line.
  data: content type string.

*  ** Pass the required parameters and create the shortcut
  clear it_shortcut_param.
  refresh it_shortcut_param.

  vlinha = plinha.
  add 1 to vlinha.

  read table tg_estra into wa_estra index vlinha .

  data: bsmtp_addr type adr6-smtp_addr.

  select single adr6~smtp_addr into bsmtp_addr
    from usr21
      inner join adr6
         on  usr21~addrnumber = adr6~addrnumber
        and usr21~persnumber = adr6~persnumber
            where usr21~bname = wa_estra-aprovador.

* Criação do documento de Email
  doc_chng-obj_name = 'LOG_ESTRA'.

* Assunto do Email
  doc_chng-obj_descr = 'Aprovação Operações Writer'.

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP, as Operações Writer abaixo.'.
  append objtxt.
  clear objtxt.
  append objtxt.

  objtxt-line = 'Para aprovar clique no link "Estratégia" em anexo.' .
  append objtxt.
  clear objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  append objtxt.
  clear objtxt.

  data: vdata(10).

  concatenate 'Operação:'  wg_cadoperacao-operacao ' Departamento:' wg_cadoperacao-dep_resp into objtxt separated by space.
  append objtxt.
  clear objtxt.

* Setar tamanho da mensagem
  describe table objtxt lines tab_lines.
  read table objtxt index tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  clear objpack-transf_bin.
  "OBJPACK-TRANSF_BIN = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  append objpack.

  call function 'ZFM_CREATE_SHORTCUT'
    exporting
      recipient_user_id = wa_estra-aprovador
      transaction       = 'ZNFW0007'
    importing
      content           = content
    tables
      shortcut_param    = it_shortcut_param.

  clear : tab_lines, objbin.
  concatenate content wa_objbin-line into wa_objbin-line.
  append  wa_objbin to objbin.

  describe table objbin lines tab_lines.
  objhead = 'ESTRATEGIA.SAP'.
  append objhead.

** Creation of the entry for the compressed attachment
  objpack-transf_bin = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 1.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'EXT'." SAP
  objpack-obj_name   = 'SAPSHORTCUTMAIL'.
  objpack-obj_descr  = 'ESTRATEGIA.SAP'.
  objpack-doc_size   = tab_lines * 255.
  append objpack.

* Alimentar destinatários do email
  if bsmtp_addr is initial.
    message 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.' type 'I'.
    exit.
  endif.

  reclist-receiver = bsmtp_addr.
  reclist-rec_type = 'U'.                    "Define email externo
  append reclist.

* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.
  call function 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    exporting
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    tables
      packing_list               = objpack
      object_header              = objhead
      contents_bin               = objbin
      contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
      receivers                  = reclist
    exceptions
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      others                     = 99.

  sy-uname = vuser.
*  CASE sy-subrc.
*    WHEN 0.
*      WRITE: / 'Result of the send process:'.
*
*      LOOP AT reclist.
*        WRITE: / reclist-receiver(48), ':'.
*
*        IF reclist-retrn_code = 0.
*          WRITE 'The document was sent'.
*        ELSE.
*          WRITE 'The document could not be sent'.
*        ENDIF.
*
*      ENDLOOP.
*
*    WHEN 1.
*      WRITE: / 'No authorization for sending to the specified number',
*               'of recipients'.
*
*    WHEN 2.
*      WRITE: / 'Document could not be sent to any recipient'.
*
*    WHEN 4.
*      WRITE: / 'No send authorization'.
*
*    WHEN OTHERS.
*      WRITE: / 'Error occurred while sending'.
*
*  ENDCASE.


endform.                    " ENVIA_EMAIL
*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_refresh .
  perform atualiza_operacoes.
  refresh tg_estra.
  loop at it_estra into wa_estra where operacao =  wg_cadoperacao-operacao.
    append wa_estra to tg_estra.
  endloop.
  refresh tg_docs.
  loop at it_docs into wa_docs where operacao = wg_cadoperacao-operacao.
    append wa_docs to tg_docs.
  endloop.

  sort tg_estra by nivel.

  call method grid2->refresh_table_display
    exporting
      is_stable = wa_stable.

  call method grid3->refresh_table_display
    exporting
      is_stable = wa_stable.
endform.                    " F_REFRESH

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_START    text
*      -->L_NAME     text
*      -->L_VALUE    text
*----------------------------------------------------------------------*
form f_preencher_dynpro using l_start type c l_name type c l_value.

  move l_start to wl_bdc-dynbegin.
  if l_start = 'X'.
    move:
  l_name  to wl_bdc-program,
  l_value to wl_bdc-dynpro.
  else.
    move:
      l_name  to wl_bdc-fnam,
      l_value to wl_bdc-fval.
  endif.
  append wl_bdc to tl_bdc.
  clear: wl_bdc.

endform.                    " f_preencher_dynpro

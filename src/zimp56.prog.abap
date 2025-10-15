*&---------------------------------------------------------------------*
*& Report  ZIMP56
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

report  zimp56.

types: begin of ty_cadlote,
         empresa(30) type c,
         lote(50)    type c,
         usuario(20) type c,
         total       type zimp_lanc_imp_ct-valor_imp,
         dep_resp(2),
         data(10),
       end of ty_cadlote,


       begin of ty_fields,
         campo(30) type c,
         group1(5) type c,
         value     type sy-tabix,
         invisible type sy-tabix,
       end   of ty_fields,


       begin of ty_estra ,
         bukrs     type zimp_lotes_aprov-bukrs,
         lote      type zimp_lotes_aprov-lote,
         valor_de  type zimp_aprovador-valor_de,
         valor_ate type zimp_aprovador-valor_ate,
         aprovador type zimp_aprovador-aprovador,
         nivel     type zimp_aprovador-nivel,
         estado(4),
         opcoes(4),
       end of ty_estra,

       begin of ty_docs ,
         lote              type zimp_cad_lote-lote,
         doc_imposto       type zimp_lanc_impost-doc_imposto,
         descr_imposto(55) type c,
         conv_banco        type zimp_lanc_impost-conv_banco,
         hbkid(20)         type c,
         valor             type zimp_lanc_imp_ct-valor_imp,
         observacao        type zimp_lanc_impost-observacao,
       end of ty_docs.



*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
data: ok-code         type sy-ucomm,
      tg_selectedcell type lvc_t_cell,
      wg_selectedcell type lvc_s_cell,



      begin of tg_lotes occurs 0,
        status(4),
        empresa(30)  type c,
        lote         type zimp_cad_lote-lote,
        dep_resp(25) type c,
        dt_venc      type zimp_lanc_impost-dt_venc,
        total        type zimp_lanc_imp_ct-valor_imp,
        color(4),
      end of tg_lotes.

data   dyfields like dynpread occurs 1 with header line.

** Criação de tabela dinamica
data: t_fieldcatalog type lvc_t_fcat,
      w_fieldcatalog type lvc_s_fcat,
      wa_layout      type lvc_s_layo,
      wa_stable      type lvc_s_stbl,

      wg_cadlote     type ty_cadlote,

      wa_estra       type ty_estra,
      wa_docs        type ty_docs,

      tg_fields      type table of ty_fields   with header line,

      tg_estra       type table of ty_estra,
      tg_docs        type table of ty_docs,
      it_docs        type table of ty_docs,
      it_estra       type table of ty_estra,

      tg_msg_ret     type table of zfiwrs0002 with header line.

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
        prog        like sy-repid value 'ZIMP56',
        pressed_tab like sy-ucomm value c_tab_strip_imp-tab1,
      end of g_tab_strip_imp.

data: ok_code         like sy-ucomm,
      wg_mensagem(30),
      wg_acao(30),
      btn_rej(30)     value '@8Y@ Rejeitar'.





*Class definition for ALV toolbar
class:      lcl_alv_toolbar   definition deferred.
*            lcl_alv_toolbar2  definition deferred.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
data: g_container          type scrfname value 'CC_LOTES',
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
    data: wl_lotes         like line of tg_lotes,
          vflg_ico(1),
          wa_zimp_cad_lote type zimp_cad_lote.

    if e_row gt 0.
      clear: wg_cadlote.
      refresh tg_estra.
      refresh tg_docs.
      read table tg_lotes into wl_lotes index e_row.
      if wl_lotes-status = icon_alert.
        message 'O Lote selecionado esta com a data de vencimento no passado, não é permitido aprovar.' type 'I'.
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

      select single *
        from zimp_cad_lote
        into wa_zimp_cad_lote
        where lote = wl_lotes-lote.

      wg_cadlote-empresa  = wl_lotes-empresa.
      concatenate  wl_lotes-lote '-' wa_zimp_cad_lote-descr_lote into wg_cadlote-lote.
      wg_cadlote-usuario  = wa_zimp_cad_lote-usuario.
      wg_cadlote-total    = wl_lotes-total.
      wg_cadlote-dep_resp = wl_lotes-dep_resp+0(2).
      concatenate wl_lotes-dt_venc+6(2) wl_lotes-dt_venc+4(2) wl_lotes-dt_venc+0(4) into wg_cadlote-data separated by '.'.


      call function 'SAPGUI_SET_FUNCTIONCODE'
        exporting
          functioncode           = '=ENT'
        exceptions
          function_not_supported = 1
          others                 = 2.

      refresh tg_estra.
      loop at it_estra into wa_estra where lote = wl_lotes-lote.
        append wa_estra to tg_estra.
      endloop.

      refresh tg_docs.
      loop at it_docs into wa_docs where lote = wl_lotes-lote..
        append wa_docs to tg_docs.
      endloop.

    endif.
    sort tg_estra by nivel.
    call method grid2->refresh_table_display
      exporting
        is_stable = wa_stable.
    call method grid3->refresh_table_display
      exporting
        is_stable = wa_stable.
  endmethod.                    "ON_DOUBLE_CLICK

  method on_double_click2.
    data: wl_docs like line of tg_docs.
    if e_row gt 0.
      if e_column = 'DOC_IMPOSTO'.
        read table tg_docs into wl_docs index e_row.
        set parameter id 'BUK' field wg_cadlote-empresa+0(4).
        set parameter id 'BLN' field wl_docs-doc_imposto.
        call transaction 'ZIMP53' and skip first screen.
      endif.
    endif.
  endmethod.                    "ON_DOUBLE_CLICK2
  method on_click.
    data: v_msg    type char50,
          t_lotes  type table of zfi_lotes_imp,
          w_lotes  type          zfi_lotes_imp,
          wl_estra like line of tg_estra,
          t_estra  type table of zfi_estrategia_imp,
          w_estra  type          zfi_estrategia_imp,
          t_docs   type table of zfi_docs_imp,
          w_docs   type          zfi_docs_imp,
          wl_lotes like line of tg_lotes.

    if e_row_id gt 0.
      read table tg_estra into wl_estra index e_row_id.

      read table tg_lotes into wl_lotes with key lote = wl_estra-lote binary search.
      move-corresponding wl_lotes to w_lotes.
      append w_lotes  to t_lotes.

      loop at it_estra into wl_estra where lote = wl_lotes-lote.
        move-corresponding wl_estra to w_estra.
        append w_estra to t_estra.
      endloop.

      call function 'Z_FI_ESTRATEGIA_EXECUTAR'
        exporting
          v_usuario = sy-uname
        importing
          msg       = v_msg
        tables
          t_lotes   = t_lotes
          t_estra   = t_estra.

      loop at t_estra into w_estra
        where aprovador eq sy-uname.                    "Modificação CS 2016000820
        "IF E_ROW_ID = SY-TABIX.                        "Modificação CS 2016000820
        move: w_estra-opcoes to wl_estra-opcoes,
              w_estra-estado to wl_estra-estado.
        modify tg_estra from wl_estra index sy-tabix transporting opcoes estado.
        "ENDIF.
      endloop.
      perform atualiza_lotes.


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

*ALRS fim
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
    wa_layout-grid_title = 'Lotes disponíveis para Liberação'.

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

*    CREATE OBJECT OBG_TOOLBAR
*      EXPORTING
*        IO_ALV_GRID = GRID1.
*
**      * Register event handler
*    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
*    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.
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
        it_outtab            = tg_lotes[].

    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method grid1->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    set handler:
              lcl_event_handler=>on_double_click for grid1,
              lcl_event_handler=>on_data_changed_finished for grid1,
              lcl_event_handler=>on_data_changed for grid1.

*    posiciona spliter na altura x
    call method splitter->set_row_height
      exporting
        id     = 1
        height = 100.
  else.
*    CALL METHOD GRID1->GET_FRONTEND_FIELDCATALOG
*      IMPORTING
*        ET_FIELDCATALOG = T_FIELDCATALOG[].
*
*    LOOP AT T_FIELDCATALOG INTO W_FIELDCATALOG.
*
*    ENDLOOP.
*
*    CALL METHOD GRID1->SET_FRONTEND_FIELDCATALOG
*      EXPORTING
*        IT_FIELDCATALOG = T_FIELDCATALOG[].
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
    wa_layout-grid_title = 'Documentos do lote'.
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
        1 ' '                        ' '               'TG_LOTES' 'STATUS'           ' '             '02' ' ' ' ' ' ',
        2 ' '                        ' '               'TG_LOTES' 'EMPRESA'          'Empresa'       '30' ' ' ' ' ' ',
        3 'ZIMP_CAD_LOTE'            'LOTE'            'TG_LOTES' 'LOTE'             'Lote'          '10' ' ' ' ' ' ',
        4 ' '                        ' '               'TG_LOTES' 'DEP_RESP'         'Departamento'  '25' ' ' ' ' ' ',
        5 'ZIMP_CAD_LOTE'            'DT_VENC'         'TG_LOTES' 'DT_VENC'          'Vencimento'    '15' ' ' ' ' ' ',
        6 'ZIMP_LANC_IMP_CT'         'VALOR_IMP'       'TG_LOTES' 'TOTAL'            'Total'         '15' ' ' ' ' ' '.


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

  if p_field eq 'OPCOES' or p_field eq 'DOC_IMPOSTO'.
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
  data:   vflg_ico(1).

  case ok-code.
    when c_refresh.
      refresh: tg_estra, tg_docs.
      perform atualiza_lotes.
    when 'REJ'.
      read table tg_estra into wa_estra with key  aprovador = sy-uname.
      if sy-subrc = 0.
        if  wa_estra-opcoes = icon_reject.
          wa_estra-opcoes = icon_set_state.
        elseif  wa_estra-opcoes = icon_set_state.
          wa_estra-opcoes = icon_reject.
        endif.
        modify tg_estra from wa_estra index sy-tabix transporting opcoes.
      endif.
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
        1 'ZIMP_APROVADOR'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         'Valor de'      '15' ' ' ' ' ' ',
        1 'ZIMP_APROVADOR'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        'Valor ate'     '15' ' ' ' ' ' ',
        1 'ZIMP_APROVADOR'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        'Aprovador'     '20' ' ' ' ' ' ',
        1 ' '                        ' '               'TG_ESTRA' 'ESTADO'           'Estado'        '10' ' ' ' ' ' ',
        1 ' '                        ' '               'TG_ESTRA' 'OPCOES'           'Opções Liber.' '12' ' ' ' ' ' '.

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
        1 'ZIMP_LANC_IMPOST'           'DOC_IMPOSTO'        'TG_DOCS'  'DOC_IMPOSTO'         'Doc.Imposto'      '15' ' ' ' ' ' ',
        1 ' '                          ' '                  'TG_DOCS'  'DESCR_IMPOSTO'       'Imposto'          '42' ' ' ' ' ' ',
        3 'ZIMP_LANC_IMPOST'           'CONV_BANCO'         'TG_DOCS'  'CONV_BANCO'          'Convênio'         '10' ' ' ' ' ' ',
        3 ' '                          ' '                  'TG_DOCS'  'HBKID'               'Banco Pagador'    '15' ' ' ' ' ' ',
        3 'ZIMP_LANC_IMP_CT'           'VALOR_IMP'          'TG_DOCS'  'VALOR'               'Valor'            '15' ' ' 'X' ' ',
        3 'ZIMP_LANC_IMPOST'           'OBSERVACAO'         'TG_DOCS'  'OBSERVACAO'          'Observação'       '50' ' ' ' ' ' '.

endform.                    " MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*&      Module  CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module carrega_lotes output.
  data xtotal type zimp_lanc_imp_ct-valor_imp.

  if g_custom_container is initial.
    perform atualiza_lotes.
  endif.


endmodule.                 " CARREGA_LOTES  OUTPUT
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
*&      Form  Atualiza_lotes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form atualiza_lotes .

  data: v_msg     type char50,
        t_lotes   type table of zfi_lotes_imp,
        w_lotes   type          zfi_lotes_imp,
        t_estra   type table of zfi_estrategia_imp,
        w_estra   type          zfi_estrategia_imp,
        t_docs    type table of zfi_docs_imp,
        w_docs    type          zfi_docs_imp,
        vdata(10).

  call function 'Z_FI_ESTRATEGIA_LISTA'
    exporting
      v_usuario = sy-uname
    importing
      msg       = v_msg
    tables
      t_lotes   = t_lotes
      t_estra   = t_estra
      t_docs    = t_docs.
  refresh: tg_lotes, it_estra, it_docs.

  loop at t_lotes into w_lotes.
    move w_lotes-dt_venc to vdata.
    move-corresponding w_lotes to tg_lotes.
    concatenate vdata+6(4) vdata+3(2) vdata+0(2) into tg_lotes-dt_venc.
    append tg_lotes.
  endloop.
  sort tg_lotes by lote .

  loop at t_estra into w_estra.
    move-corresponding w_estra to wa_estra.
    append wa_estra to it_estra.
  endloop.
  sort it_estra by lote .

  loop at t_docs into w_docs.
    move-corresponding w_docs to wa_docs.
    append wa_docs to it_docs.
  endloop.
  sort it_docs by lote .


endform.                    " Atualiza_lotes
*&---------------------------------------------------------------------*
*&      Form  GRAVA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form grava_zib using p_lote.
  data: wl_setleaf type setleaf,
        i_head     type tbtcjob.

  data:   wl_job_id   like tbtcjob-jobcount.
  data:   wl_jobn(32).

  data: begin of i_steplist occurs 10.
          include structure tbtcstep.
  data: end of i_steplist.
  data : c_no(1) type c . "value 'N', " Criação do job

  data: wl_tbtcjob  type  tbtcjob,
        wl_tbtcstrt type  tbtcstrt.

  data: lv_repname like  rsvar-report.           " for variant handling
  data: iv_varname like  raldb-variant value 'SAP_UPGRADE'.
  data: iv_varianttext  like  varit-vtext value 'Upgrade variant'.
  data: wl_subrc type sy-subrc.
  data: tt_reportparam type table of  rsparams with header line.

  select single *
   from setleaf
   into wl_setleaf
    where setname eq 'MAGGI_JOB_USER'.

  if sy-subrc ne 0.
    message 'ERRO AO EXECUTAR JOB, CONTACTE A T.I.' type 'E'.
    exit.
  endif.
  concatenate 'Z_GRAVA_ZIB_IMP' p_lote  into wl_jobn separated by '|'.

  i_head-jobname = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
  i_head-sdlstrttm = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
  i_head-stepcount = 1.

  tt_reportparam-selname = 'P_LOTE'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = p_lote.
  append tt_reportparam.
  clear tt_reportparam.

  lv_repname = 'Z_GRAVA_ZIB_IMP'.
*    Write the variant first (Insert or Update)
  call function 'SUBST_WRITE_UPGRADE_VARIANT'
    exporting
      iv_reportname         = lv_repname
      iv_variantname        = iv_varname
      iv_varianttext        = iv_varianttext
    importing
      ev_funcrc             = wl_subrc
    tables
      tt_reportparam        = tt_reportparam
    exceptions
      exist_check_failed    = 1
      update_failed         = 2
      update_not_authorized = 3
      update_no_report      = 4
      update_no_variant     = 5
      update_variant_locked = 6
      insert_failed         = 7
      insert_not_authorized = 8
      insert_no_report      = 9
      insert_variant_exists = 10
      insert_variant_locked = 11
      others                = 12.

  i_steplist-parameter = iv_varname. " Nome da variante
  i_steplist-program = 'Z_GRAVA_ZIB_IMP'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
  i_steplist-typ = 'A'. " Tipo de Job
  i_steplist-authcknam = wl_setleaf-valfrom.
  i_steplist-language = sy-langu.
  i_steplist-arcuser = wl_setleaf-valfrom.

  append i_steplist.


  c_no = 'N'.
  call function 'BP_JOB_CREATE'
    exporting
      job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
      job_cr_head_inp     = i_head " os valores atribuidos
    importing
      job_cr_head_out     = wl_tbtcjob
      job_cr_stdt_out     = wl_tbtcstrt
    tables
      job_cr_steplist     = i_steplist
    exceptions
      cant_create_job     = 1
      invalid_dialog_type = 2
      invalid_job_data    = 3
      job_create_canceled = 4
      others              = 5.

  call function 'JOB_CLOSE'
    exporting
      jobname   = wl_jobn
      jobcount  = wl_tbtcjob-jobcount
      strtimmed = 'X'.

endform.                    " GRAVA_ZIB
*&---------------------------------------------------------------------*
*&      Form  ENvia_email_Fim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form envia_email_fim using value(wg_cadlote) type ty_cadlote.

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
  data: vuser         type sy-uname,
        vflag_mail(1).

  data:      ctotal(20),
             vdata(10),
             vbukrs type zimp_cad_lote-bukrs.

  data: it_shortcut_param like zst_shortcut_par occurs 0 with header line.
  data: content type string.

*  ** Pass the required parameters and create the shortcut
  clear it_shortcut_param.
  refresh it_shortcut_param.

  vbukrs = wg_cadlote-empresa+0(4).
  data: it_mail type table of zmail with header line.
  select *
    from zmail
    into table it_mail
    where tcode = 'ZIMP57'
    and   bukrs     le vbukrs.
*    AND   BUKRS_ATE GE VBUKRS.
  if sy-subrc ne 0.
    select *
    from zmail
    into table it_mail
    where tcode = 'ZIMP57'.
  endif.
  vflag_mail = ''.

*&----CS2024000431 Parte  2 - Corrigir programas que usam tabela ZMAIL  campo BURKS_ATE / AOENNING ---Código comentado--&
  loop at it_mail.
*    IF  IT_MAIL-BUKRS_ATE IS INITIAL.
    if  it_mail-bukrs ne vbukrs.
      continue.
    endif.
*    IF  IT_MAIL-BUKRS GT VBUKRS OR IT_MAIL-BUKRS LT VBUKRS.
*      CONTINUE.
*    ENDIF.

    if not it_mail-usuario is initial.
      vflag_mail = 'X'.
      exit.
    endif.
  endloop.
*&----CS2024000431 Parte  2 - Corrigir programas que usam tabela ZMAIL  campo BURKS_ATE / AOENNING ---Código comentado--&

  if it_mail-usuario is initial or vflag_mail  is initial.
*    MESSAGE 'Usuário padrão não cadastrado.' TYPE 'E'.
    message i024(sd) with 'Usuario' it_mail-usuario 'não cadastrado na transação' 'ZMAIL'.
    exit.
  elseif it_mail[] is initial.
    message 'E-Mail não cadastrado para esta transação.' type 'E'.
    exit.
  endif.


  "READ TABLE IT_ZIMP_CAD_LOTE INTO WA_ZIMP_CAD_LOTE WITH KEY LOTE = WA_ESTRA-LOTE BINARY SEARCH.
  "CONCATENATE WA_ZIMP_CAD_LOTE-DT_VENC+6(2) WA_ZIMP_CAD_LOTE-DT_VENC+4(2) WA_ZIMP_CAD_LOTE-DT_VENC+0(4) INTO VDATA SEPARATED BY '.'.
  vdata = wg_cadlote-data.
* Criação do documento de Email
  doc_chng-obj_name = 'LOG_REL'.

* Assunto do Email
  concatenate 'Pagamento de Imposto Venc .' vdata wg_cadlote-empresa into doc_chng-obj_descr separated by space .

* Texto
  objtxt-line = 'O lote abaixo foi aprovado e encontra-se disponível para pagamento.'.
  append objtxt.
  clear objtxt.
  append objtxt.

  objtxt-line = 'Clique no link anexo "GUIAS", para a relação individual das guias para pagamento'.
  append objtxt.
  clear objtxt.

  objtxt-line = '--------------------------------------------------------------------------------------------------------------' .
  append objtxt.
  clear objtxt.

  write wg_cadlote-total to ctotal currency 'USD'.

  condense ctotal no-gaps.
  concatenate 'Empresa:'  wg_cadlote-empresa ' Lote:' wg_cadlote-lote ' R$' ctotal ' Venc.'  vdata into objtxt separated by space.
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

  it_shortcut_param-fieldname = 'P_BUKRS'.
  it_shortcut_param-fieldvalue = wg_cadlote-empresa+0(4).
  append it_shortcut_param.

  it_shortcut_param-fieldname = 'P_LOTE-LOW'.
  it_shortcut_param-fieldvalue = wg_cadlote-lote+0(10).
  append it_shortcut_param.

  call function 'ZFM_CREATE_SHORTCUT'
    exporting
      recipient_user_id = it_mail-usuario
      transaction       = 'ZIMP57'
    importing
      content           = content
    tables
      shortcut_param    = it_shortcut_param.

  clear : tab_lines, objbin.
  concatenate content wa_objbin-line into wa_objbin-line.
  append  wa_objbin to objbin.

  describe table objbin lines tab_lines.
  objhead = 'GUIAS.SAP'.
  append objhead.

** Creation of the entry for the compressed attachment
  objpack-transf_bin = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 1.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'EXT'." SAP
  objpack-obj_name   = 'SAPSHORTCUTMAIL'.
  objpack-obj_descr  = 'GUIAS.SAP'.
  objpack-doc_size   = tab_lines * 255.
  append objpack.

* Alimentar destinatários do email
  loop at it_mail.
    reclist-receiver = it_mail-email.
    reclist-rec_type = 'U'.                    "Define email externo
    append reclist.
  endloop.


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

endform.                    "ENvia_email_Fim
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
"FORM envia_email USING plinha.
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TG_ESTRA           text
*      -->VALUE(WG_CADLOTE)  text
*      -->PLINHA             text
*----------------------------------------------------------------------*
form envia_email tables tg_estra  using value(wg_cadlote) type ty_cadlote plinha  .

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
  doc_chng-obj_descr = 'Aprovação Pagamento Imposto - Vencimento'.

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP, o lote de impostos abaixo.'.
  append objtxt.
  clear objtxt.
  append objtxt.

  objtxt-line = 'Para aprovar clique no link "Estratégia" em anexo.' .
  append objtxt.
  clear objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  append objtxt.
  clear objtxt.

  data: ctotal(20),
        vdata(10).
*  READ TABLE IT_ZIMP_CAD_LOTE INTO WA_ZIMP_CAD_LOTE WITH KEY LOTE = WA_ESTRA-LOTE BINARY SEARCH.
*  CONCATENATE WA_ZIMP_CAD_LOTE-DT_VENC+6(2) WA_ZIMP_CAD_LOTE-DT_VENC+4(2) WA_ZIMP_CAD_LOTE-DT_VENC+0(4) INTO VDATA SEPARATED BY '.'.

  vdata = wg_cadlote-data.
  write wg_cadlote-total to ctotal currency 'USD'.

  condense ctotal no-gaps.
  concatenate 'Empresa:'  wg_cadlote-empresa ' Lote:' wg_cadlote-lote ' R$' ctotal ' Venc.'  vdata into objtxt separated by space.
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
      transaction       = 'ZIMP56'
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

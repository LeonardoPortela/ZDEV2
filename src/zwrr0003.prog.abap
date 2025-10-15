*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....: 18/08/2010                                              &*
*& Descrição: Cockipt - Gestão de Emissão de Nota Fiscal              &*
*& Transação: ZNFW0004                                                &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   03.08.2010                            &*
*&--------------------------------------------------------------------&*

report  zwrr0003.
include <icon>.
types: begin of ty_monitor,
        mark,
        operacao  type zfiwrt0008-operacao,
        seq_lcto  type zfiwrt0008-seq_lcto,
        bukrs     type zfiwrt0008-bukrs,
        branch    type zfiwrt0008-branch,
        parvw     type zfiwrt0008-parvw,
        parid     type zfiwrt0008-parid,
        nftype    type zfiwrt0008-nftype,
        netwr     type zfiwrt0009-netwr,
        descricao type zfiwrt0001-descricao,
        usnam     type zfiwrt0008-usnam,
        dt_criacao type zfiwrt0008-dt_criacao,
        hr_criacao type zfiwrt0008-hr_criacao,
        aprov      type zfiwrt0007-usnam,
       end of ty_monitor,

       begin of ty_itens,
        itmnum type zfiwrt0009-itmnum,
        matnr  type zfiwrt0009-matnr,
        maktx  type makt-maktx,
        charg  type zfiwrt0009-charg,
        cfop   type zfiwrt0009-cfop,
        menge  type zfiwrt0009-menge,
        netpr  type zfiwrt0009-netpr,
        netwr  type zfiwrt0009-netwr,
        itmtyp type zfiwrt0009-itmtyp,
        bwkey  type zfiwrt0009-bwkey,
        anln1  type zfiwrt0009-anln1,
        anln2  type zfiwrt0009-anln2,
       end of ty_itens,

       begin of ty_makt,
         matnr type makt-matnr,
         maktx type makt-maktx,
       end of ty_makt.

*Class definition for ALV toolbar
class:      lcl_alv_toolbar   definition deferred.
data: g_container          type scrfname value 'CC_GRID1',
      g_custom_container   type ref to cl_gui_custom_container,
      grid1                type ref to cl_gui_alv_grid,
      grid2                type ref to cl_gui_alv_grid,
      obg_toolbar          type ref to lcl_alv_toolbar,
      c_alv_toolbarmanager type ref to cl_alv_grid_toolbar_manager,
      container_1          type ref to cl_gui_container,       "splitter conteiner 1
      container_2          type ref to cl_gui_container,       "splitter conteiner 2
      splitter             type ref to cl_gui_splitter_container.
*Declaration for toolbar buttons
data : ty_toolbar type stb_button.
** Criação de tabela dinamica
data: t_fieldcatalog        type lvc_t_fcat,
      w_fieldcatalog        type lvc_s_fcat,
      wa_layout             type lvc_s_layo,
      wa_stable             type lvc_s_stbl.

constants: c_x           type c value 'X',
           c_a           type c value 'A',
           c_r           type c value 'R',
           c_br(2)       type c value 'BR',
           c_lf(2)       type c value 'LF',
           c_ag(2)       type c value 'AG',
           c_exit(4)     type c value 'EXIT',
           c_back(4)     type c value 'BACK',
           c_aprov(5)    type c value 'APROV',
           c_cancel(6)   type c value 'CANCEL',
           c_atuali(6)   type c value 'ATUALI',
           c_search(6)   type c value 'SEARCH',
           c_reprov(6)   type c value 'REPROV',
           c_clos_itens(10) type c value 'CLOS_ITENS'.


data: tg_monitor type table of ty_monitor with header line,
      tg_0007    type table of zfiwrt0007 with header line,
      tg_0009    type table of zfiwrt0009 with header line,
      tg_itens   type table of ty_itens,
      wg_itens   type ty_itens,
      tg_makt    type table of ty_makt,
      wg_makt    type ty_makt.
data: tl_bdc    type table of bdcdata,
      wl_bdc    type bdcdata.

data: ok_code     type sy-ucomm,
      p_seq_lcto  type zfiwrt0008-seq_lcto,
      p_usuario   type zfiwrt0007-usnam,
      p_bukrs     type zfiwrt0008-bukrs,
      p_branch    type zfiwrt0008-branch,
      p_parvw     type zfiwrt0008-parvw,
      p_parid     type zfiwrt0008-parid,
      wg_desc_branch(30),
      wg_desc_parvw(30),
      wg_desc_parid(30),
      wg_desc_bukrs(30),
      wg_desc_usuario(30),
      wg_msg(100).


call screen 100.
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
class lcl_event_handler definition.

  public section.
    class-methods:
      on_double_click for event double_click of cl_gui_alv_grid
                      importing e_row e_column,

     on_hotsopt_click for event hotspot_click of cl_gui_alv_grid
                      importing e_row_id e_column_id es_row_no.


endclass.                    "LCL_EVENT_HANDLER DEFINITION
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
               importing  e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
               importing e_ucomm.
endclass.                    "lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
class lcl_event_handler implementation.
* Método de  execução para Duplo-click
  method on_double_click.
    data:   tl_function         type ui_functions,
            wl_function         like line of tl_function.

    data: wl_result type i.
    if e_row gt 0.
      read table tg_monitor into tg_monitor index e_row.

      refresh: tg_itens.

      loop at tg_0009 into tg_0009
         where seq_lcto eq tg_monitor-seq_lcto.

        read table tg_makt into wg_makt
          with key matnr = tg_0009-matnr.

        move: tg_0009-itmnum to wg_itens-itmnum,
              tg_0009-matnr  to wg_itens-matnr,
              wg_makt-maktx  to wg_itens-maktx,
              tg_0009-charg  to wg_itens-charg,
              tg_0009-cfop   to wg_itens-cfop,
              tg_0009-menge  to wg_itens-menge,
              tg_0009-netpr  to wg_itens-netpr,
              tg_0009-netwr  to wg_itens-netwr,
              tg_0009-itmtyp to wg_itens-itmtyp ,
              tg_0009-bwkey  to wg_itens-bwkey,
              tg_0009-anln1  to wg_itens-anln1,
              tg_0009-anln2  to wg_itens-anln2.

        append wg_itens to tg_itens.
        clear: wg_itens, wg_makt.
      endloop.

      call method splitter->set_row_height
        exporting
          id     = 1
          height = 50.

      call method splitter->set_row_sash
        exporting
          id    = 1
          type  = 0
          value = 1.

      if container_2 is  initial.
        call method splitter->get_container
          exporting
            row       = 2
            column    = 1
          receiving
            container = container_2.

        create object grid2
          exporting
            i_parent = container_2.

        perform montar_layout_grid2.
        wa_layout-sel_mode = space.
        wa_layout-box_fname = space.
        call method grid2->set_table_for_first_display
          exporting
            it_toolbar_excluding = tl_function
            is_layout            = wa_layout
          changing
            it_fieldcatalog      = t_fieldcatalog[]
            it_outtab            = tg_itens[].
      else.
        wa_stable-col = c_x.
        call method grid2->refresh_table_display
          exporting
            is_stable = wa_stable.
      endif.
    endif.
  endmethod.              "ON_DOUBLE_CLICK
  method on_hotsopt_click.
    data opt type ctu_params.

    refresh: tl_bdc.

    if e_column_id eq 'SEQ_LCTO'.
      read table tg_monitor into tg_monitor index e_row_id-index.

      perform f_preencher_dynpro using:
      'X' 'ZWRR0002'             '0100',
      ' ' 'P_SEQ_LCTO'           tg_monitor-seq_lcto,
      ' ' 'BDC_OKCODE'           'SEARCH'.

      opt-dismode = 'E'.
      opt-defsize = ' '.

      call transaction 'ZNFW0002' using tl_bdc options from opt.

    elseif e_column_id eq 'OPERACAO'.
      read table tg_monitor into tg_monitor index e_row_id-index.

      perform f_preencher_dynpro using:
      'X' 'ZWRR0001'             '0100',
      ' ' 'ZFIWRT0001-OPERACAO'  tg_monitor-operacao,
      ' ' 'BDC_OKCODE'           'ATUALI'.

      opt-dismode = 'E'.
      opt-defsize = ' '.

      call transaction 'ZNFW0001' using tl_bdc options from opt.
    endif.
  endmethod.                    "on_hotsopt_click
endclass.           "LCL_EVENT_HANDLER IMPLEMENTATION
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
*   Add customized toolbar buttons.
*    READ TABLE TG_FIELDS TRANSPORTING NO FIELDS
*      WITH KEY GROUP1 = 'GR1'.
*    IF SY-SUBRC IS INITIAL.
*      WL_DESACTIVE = SPACE.
*    ELSE.
*      WL_DESACTIVE = 1.
*    ENDIF.
    ty_toolbar-icon      =  icon_booking_ok.
    ty_toolbar-function  =  c_aprov.
    ty_toolbar-quickinfo = 'Aprovar documentos'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-butn_type = 3.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-icon      =  icon_booking_stop.
    ty_toolbar-function  =  c_reprov.
    ty_toolbar-quickinfo = 'Reprovar documentos'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.
*
    ty_toolbar-butn_type = 3.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.
**   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_itens.
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
    data: tl_index_rows type lvc_t_row,
          wl_index_rows type lvc_s_row.
*
*    CALL METHOD grid1->refresh_table_display
*      EXPORTING
*        is_stable = wa_stable.
    case e_ucomm.
      when c_aprov.
        call method grid1->get_selected_rows
          importing
            et_index_rows = tl_index_rows.

        loop at tl_index_rows into wl_index_rows.
          read table tg_monitor into tg_monitor index wl_index_rows-index.

          perform modifica_status using c_a.

        endloop.
        message s836(sd) with 'O(s) documentos foi(rao) aprovado(s)'.
      when c_reprov.
        call method grid1->get_selected_rows
          importing
            et_index_rows = tl_index_rows.

        loop at tl_index_rows into wl_index_rows.
          read table tg_monitor into tg_monitor index wl_index_rows-index.

          perform modifica_status using c_r.

        endloop.
        message s836(sd) with 'O(s) documentos foi(rao) reprovado(s)'.
      when c_clos_itens.
        call method splitter->set_row_height
          exporting
            id     = 1
            height = 100.

        call method splitter->set_row_sash
          exporting
            id    = 1
            type  = 0
            value = 0.
    endcase.

  endmethod.                    "zm_handle_user_command

endclass.                    "lcl_alv_toolbar IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos output.
  data: event type cntl_simple_event,
        events type cntl_simple_events,
        tl_filter           type lvc_t_filt,
        wl_filter           type lvc_s_filt,
        tl_function         type ui_functions,
        wl_function         like tl_function with header line.

  if g_custom_container is initial.
    wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
    wa_layout-box_fname  = 'MARK'.
    wa_layout-sel_mode   = c_a.
*    wa_layout-edit_mode  = c_x.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
*    wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
    wa_stable-row        = c_x.

    create object g_custom_container
      exporting
        container_name = g_container.

    create object splitter
      exporting
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    call method splitter->set_row_height
      exporting
        id     = 1
        height = 100.

    call method splitter->set_row_sash
      exporting
        id    = 1
        type  = 0
        value = 0.

    call method splitter->get_container
      exporting
        row       = 1
        column    = 1
      receiving
        container = container_1.

    create object grid1
      exporting
        i_parent = container_1.


    perform montar_layout_grid1.
    create object obg_toolbar
      exporting
        io_alv_grid = grid1.

*      * Register event handler
    set handler obg_toolbar->on_toolbar for grid1.
    set handler obg_toolbar->handle_user_command for grid1.

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
        it_outtab            = tg_monitor[].

    set handler:
                  lcl_event_handler=>on_double_click for grid1.
    set handler:
                  lcl_event_handler=>on_hotsopt_click for grid1.
  else.
    call method grid1->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.
endmodule.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'Z001'.
  set titlebar 'Z001'.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  case ok_code.
    when c_search.
      perform limpa_campos.
      perform busca_descricoes.
      perform busca_dados.
    when c_atuali.
      perform busca_dados.
    when c_back.
      leave to screen 0.
    when c_cancel.
      leave list-processing and return to screen 100.
    when c_exit.
      leave program.
  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_GRID1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_grid1 .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZFIWRT0008'   'OPERACAO'      'TG_MONITOR' 'OPERACAO'    ' '          '4'   ' ' ' ' 'X',
        1 'ZFIWRT0008'   'SEQ_LCTO'      'TG_MONITOR' 'SEQ_LCTO'    ' '          '15'  ' ' ' ' 'X',
        2 'ZFIWRT0008'   'BUKRS'         'TG_MONITOR' 'BUKRS'       ' '          '7'  ' ' ' ' ' ',
        3 'ZFIWRT0008'   'BRANCH'        'TG_MONITOR' 'BRANCH'      ' '          '5'   ' ' ' ' ' ',
        4 'ZFIWRT0008'   'PARVW'         'TG_MONITOR' 'PARVW'       ' '          '8'   ' ' ' ' ' ',
        5 'ZFIWRT0008'   'PARID'         'TG_MONITOR' 'PARID'       ' '          '5'   ' ' ' ' ' ',
        6 'ZFIWRT0008'   'NFTYPE'        'TG_MONITOR' 'NFTYPE'      ' '          '10'  ' ' ' ' ' ',
        7 'KONP'         'KBETR'         'TG_MONITOR' 'NETWR'       'Total'      '10'  ' ' ' ' ' ',
        8 'ZFIWRT0008'   'USNAM'         'TG_MONITOR' 'USNAM'       ' '          '10'  ' ' ' ' ' ',
        9 'ZFIWRT0008'   'DT_CRIACAO'    'TG_MONITOR' 'DT_CRIACAO'  'Data'       '10'  ' ' ' ' ' ',
       10 'ZFIWRT0008'   'HR_CRIACAO'    'TG_MONITOR' 'HR_CRIACAO'  'Hora'       '10'  ' ' ' ' ' ',
       11 ' '            ' '             'TG_MONITOR' 'APROV'       'Aprovador'  '10'  ' ' ' ' ' '.

endform.                    " MONTAR_LAYOUT_GRID1
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
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
                            value(p_hotspot).

  clear w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
*  w_fieldcatalog-key_sel       = 'X'.
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
  w_fieldcatalog-hotspot     = p_hotspot.

  if p_field eq 'ESTORNO'.
*    W_FIELDCATALOG-CHECKBOX = C_X.
  endif.

*  IF P_FIELD EQ 'DMBTR'.
*
*  ENDIF.

  append w_fieldcatalog to t_fieldcatalog.

endform.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_descricoes .
  data: wl_0001 type zfiwrt0001,
        wl_t001 type t001,
        wl_1bbranch type j_1bbrancht,
        wl_1bad     type j_1bad,
        wl_kna1     type kna1,
        wl_lfa1     type lfa1,
        wl_1badt    type j_1badt,
        wl_user_addr  type user_addr.

  select single *
        from t001
        into wl_t001
         where bukrs eq p_bukrs.

  if sy-subrc is initial.
    move: wl_t001-butxt to wg_desc_bukrs.
  else.
    clear : wg_desc_bukrs.
  endif.

  select single *
      from j_1bbrancht
      into wl_1bbranch
       where bukrs  eq p_bukrs
         and branch eq p_branch
         and language eq sy-langu.

  if sy-subrc is initial.
    move: wl_1bbranch-name to wg_desc_branch.
  else.
    clear : wg_desc_branch.
  endif.

  select single *
  from j_1bad
  into wl_1bad
   where parvw eq p_parvw.

  if sy-subrc is initial.
    select single *
      from j_1badt
      into wl_1badt
       where spras eq sy-langu
         and parvw eq p_parvw.

    move: wl_1badt-partxt to wg_desc_parvw.
  else.
    clear : wg_desc_parvw.
  endif.
  if p_parvw eq c_ag.
    select single *
      from kna1
      into wl_kna1
       where kunnr eq p_parid.

    if sy-subrc is initial.
      move: wl_lfa1-name1 to wg_desc_parid.
    else.
      clear : wg_desc_parid.
    endif.

  elseif p_parvw eq c_lf
      or   p_parvw eq c_br.
    select single *
            from lfa1
            into wl_lfa1
             where lifnr eq p_parid.
    if sy-subrc is initial.
      move: wl_lfa1-name1 to wg_desc_parid.
    else.
      clear : wg_desc_parid.
    endif.
  endif.

  select single *
    from user_addr
    into wl_user_addr
     where bname eq p_usuario.

  if sy-subrc is initial.
    concatenate wl_user_addr-name_first wl_user_addr-name_last into wg_desc_usuario separated by
      space.
  else.
    clear: wg_desc_usuario.
  endif.
endform.                    " BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form busca_dados .
  data: tl_0001 type table of zfiwrt0001 with header line,
        tl_0007 type table of zfiwrt0007 with header line,
        tl_0007_aux type table of zfiwrt0007,
        tl_0008 type table of zfiwrt0008 with header line,
        tl_0009 type table of zfiwrt0009 with header line,
        tl_0014 type table of zfiwrt0014 with header line,
        wl_cont type sy-tabix.

  ranges : rl_seq_lcto for zfiwrt0008-seq_lcto,
           rl_bukrs    for zfiwrt0008-bukrs,
           rl_branch   for zfiwrt0008-branch,
           rl_parvw    for zfiwrt0008-parvw,
           rl_parid    for zfiwrt0008-parid,
           rl_usuario  for zfiwrt0008-usnam.

  refresh: rl_seq_lcto, rl_bukrs, rl_branch, rl_parvw, rl_parid, rl_usuario.
  clear: rl_seq_lcto, rl_bukrs, rl_branch, rl_parvw, rl_parid, rl_usuario.

  if p_seq_lcto is not initial.
    rl_seq_lcto-sign = 'I'.
    rl_seq_lcto-option = 'EQ'.
    rl_seq_lcto-low = p_seq_lcto.
    append rl_seq_lcto.
    add 1 to wl_cont.
  endif.

  if p_bukrs is not initial.
    rl_bukrs-sign = 'I'.
    rl_bukrs-option = 'EQ'.
    rl_bukrs-low = p_bukrs.
    append rl_bukrs.
    add 1 to wl_cont.
  endif.

  if p_branch is not initial.
    rl_branch-sign = 'I'.
    rl_branch-option = 'EQ'.
    rl_branch-low = p_branch.
    append rl_branch.
    add 1 to wl_cont.
  endif.

  if p_parvw is not initial.
    rl_parvw-sign = 'I'.
    rl_parvw-option = 'EQ'.
    rl_parvw-low = p_parvw.
    append rl_parvw.
    add 1 to wl_cont.
  endif.

  if p_parid is not initial.
    rl_parid-sign = 'I'.
    rl_parid-option = 'EQ'.
    rl_parid-low = p_parid.
    append rl_parid.
    add 1 to wl_cont.
  endif.

  if p_usuario is not initial.
    rl_usuario-sign = 'I'.
    rl_usuario-option = 'EQ'.
    rl_usuario-low = p_usuario.
    append rl_usuario.
    add 1 to wl_cont.
  endif.


  if wl_cont lt 3.
    wg_msg = '@0S@ Você consegue melhorar a sua busca, preenchendo mais alguns campos!'.

  else.
    clear: wg_msg.

  endif.

  select *
    from zfiwrt0007
    into table tl_0007_aux
     where usnam eq sy-uname.

  if sy-subrc is initial.
** CABECARIO DA NOTA FISCAL
    select *
      from zfiwrt0008
      into table tl_0008
      for all entries in tl_0007_aux
       where seq_lcto in rl_seq_lcto
         and operacao eq tl_0007_aux-operacao
         and bukrs    in rl_bukrs
         and branch   in rl_branch
         and parvw    in rl_parvw
         and parid    in rl_parid
         and usnam    in rl_usuario
         and loekz    eq space
         and status   eq space.

    if sy-subrc is initial.
**  iTENS DA NOTA FISCAL
      select *
        from zfiwrt0009
        into table tl_0009
         for all entries in tl_0008
         where seq_lcto eq tl_0008-seq_lcto.

      tg_0009[] = tl_0009[].
      if tg_0009[] is not initial.
        select matnr maktx
          from makt
          into table tg_makt
           for all entries in tg_0009
           where matnr eq tg_0009-matnr.

      endif.
**  Parametrizacoes de operações
      select *
        from zfiwrt0001
        into table tl_0001
         for all entries in tl_0008
          where operacao eq tl_0008-operacao.

** NIVEIS NESCESSARIOS PARA SEREM APROVADOS
      select *
        from zfiwrt0007
        into table tl_0007
        for all entries in tl_0008
         where operacao eq tl_0008-operacao.
      tg_0007[] = tl_0007[].

** LOG DE ACAO DE APROV/REPROV
      select *
        from zfiwrt0014
        into table tl_0014
        for all entries in tl_0008
         where seq_lcto eq tl_0008-seq_lcto.

      perform organiza_dados tables tl_0001
                                    tl_0008
                                    tl_0009
                                    tl_0007
                                    tl_0014.

    endif.
  else.
    message s836(sd) display like 'E' with 'Não foram encontradas solicitações de aprovação'
                                           'para seu usuário.'.
  endif.

endform.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Module  SEARCH_PARID  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module search_parid input.
  data: begin of dynpfields occurs 1.
          include structure dynpread.
  data: end of dynpfields.

* Parameter für F4IF_FIELD_VALUE_REQUEST
  data: mc_obj like help_info-mcobj.
  data: return_values like ddshretval occurs 0 with header line.
  data: da_display type c.

  refresh:dynpfields.

  move 'P_PARVW'  to dynpfields-fieldname.
  append dynpfields.
  move 'P_BUKRS'  to dynpfields-fieldname.
  append dynpfields.
  move 'P_PARID'  to dynpfields-fieldname.
  append dynpfields.

  call function 'DYNP_VALUES_READ'
    exporting
      dyname                   = sy-repid
      dynumb                   = sy-dynnr
      perform_input_conversion = 'X'
    tables
      dynpfields               = dynpfields
    exceptions
      invalid_abapworkarea     = 1
      invalid_dynprofield      = 2
      invalid_dynproname       = 3
      invalid_dynpronummer     = 4
      invalid_request          = 5
      no_fielddescription      = 6
      invalid_parameter        = 7
      undefind_error           = 8
      others                   = 9.

  read table dynpfields
      with key fieldname = 'P_PARVW'.

  if p_parvw eq c_ag
  or dynpfields-fieldvalue eq c_ag.

    read table dynpfields
       with key fieldname = 'P_PARID'.
*   Matchcodeobjekt aufrufen
    mc_obj = 'C_KUNNR'.
    if dynpfields-fieldinp eq space.
      da_display = c_x. "CHARX.
    endif.
    call function 'F4IF_FIELD_VALUE_REQUEST'
      exporting
        tabname           = space
        fieldname         = space
        searchhelp        = mc_obj
        dynprofield       = 'X'
        value             = dynpfields-fieldvalue
        display           = da_display
      tables
        return_tab        = return_values
      exceptions
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        others            = 5.
    if sy-subrc = 0 and
       dynpfields-fieldinp ne space.
      p_parid = return_values-fieldval.
    endif.

  elseif p_parvw eq c_br
    or   p_parvw eq c_lf
    or dynpfields-fieldvalue eq c_br
    or dynpfields-fieldvalue eq c_lf.

    read table dynpfields
       with key fieldname = 'P_PARID'.
*   Matchcodeobjekt aufrufen
    mc_obj = 'KRED_C'.
    if dynpfields-fieldinp eq space.
      da_display = c_x. "CHARX.
    endif.
    call function 'F4IF_FIELD_VALUE_REQUEST'
      exporting
        tabname           = space
        fieldname         = space
        searchhelp        = mc_obj
        dynprofield       = 'X'
        value             = dynpfields-fieldvalue
        display           = da_display
      tables
        return_tab        = return_values
      exceptions
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        no_values_found   = 4
        others            = 5.
    if sy-subrc = 0 and
       dynpfields-fieldinp ne space.
      p_parid = return_values-fieldval.
    endif.
  endif.
endmodule.                 " SEARCH_PARID  INPUT
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_0008  text
*      -->P_TL_0009  text
*      -->P_TL_O007  text
*      -->P_TL_0014  text
*----------------------------------------------------------------------*
form organiza_dados  tables   tl_0001 structure zfiwrt0001
                              tl_0008 structure zfiwrt0008
                              tl_0009 structure zfiwrt0009
                              tl_0007 structure zfiwrt0007
                              tl_0014 structure zfiwrt0014.

  data: begin of tl_0009_aux occurs 0,
         seq_lcto type zfiwrt0009-seq_lcto,
         netwr    type zfiwrt0009-netwr,
        end of tl_0009_aux.

  data: x_aguardando,
        x_jarespod.

  loop at tl_0009.
    move-corresponding: tl_0009 to tl_0009_aux.
    collect tl_0009_aux.
  endloop.

**> Deixa apenas a ultima ação do usuario aprovador
  sort: tl_0014 by seq_lcto nivel_aprov dzeile descending.
  delete adjacent duplicates from tl_0014 comparing seq_lcto nivel_aprov.
**<
  sort: tl_0007 by nivel_aprov.

  loop at tl_0008.    "header do documento

**> Verificador se deve o registro deve ser exibido para aprovacao,
** pois o documento so pode ser exibido para aprovação caso o mesmo, nao tenha
** sido aprovado por um nivel a cima ou se ja processado (falta segunda parte)

    "Caso houver reprovação,ignorar o registro.
    read table tl_0014
      with key seq_lcto    = tl_0008-seq_lcto
               tipo        = c_r.
    if sy-subrc is initial.
      clear: x_aguardando, x_jarespod, tg_monitor.
      continue.
    endif.

    loop at tl_0007   " Aprovadores por operação
       where operacao eq tl_0008-operacao.

      if tl_0007-branch is initial.
        tl_0007-branch = tl_0008-branch.
      endif.

       if tl_0007-branch eq tl_0008-branch.

        if tl_0007-usnam ne sy-uname.
          read table tl_0014
            with key seq_lcto    = tl_0008-seq_lcto
                     nivel_aprov = tl_0007-nivel_aprov
                     tipo        = c_a.

          if sy-subrc is initial.
            if x_jarespod is initial.
              continue.

            else.
              x_aguardando = c_x.
              exit.

            endif.
          else.
            if x_jarespod is initial.
              x_aguardando = c_x.

            endif.
            exit.
          endif.
        else.

          read table tl_0014
            with key seq_lcto    = tl_0008-seq_lcto
                     nivel_aprov = tl_0007-nivel_aprov
                     tipo        = c_a.

          if sy-subrc is initial.
            x_jarespod = c_x.
            continue.
          else.
            clear: x_aguardando.
            exit.
          endif.
        endif.
      else.
        x_aguardando = c_x.
        continue.
      endif.
*    ENDDO.
    endloop.
**< verificador

    if x_aguardando is not initial.
      clear: x_aguardando.
      continue.
    else.
      read table tl_0001
        with key operacao = tl_0008-operacao.

      read table tl_0009_aux
        with key seq_lcto = tl_0008-seq_lcto.

      move: tl_0008-operacao  to tg_monitor-operacao,
            tl_0008-seq_lcto  to tg_monitor-seq_lcto,
            tl_0008-bukrs     to tg_monitor-bukrs,
            tl_0008-branch    to tg_monitor-branch,
            tl_0008-parvw     to tg_monitor-parvw,
            tl_0008-parid     to tg_monitor-parid,
            tl_0008-nftype    to tg_monitor-nftype,
            tl_0009_aux-netwr to tg_monitor-netwr,
            tl_0001-descricao to tg_monitor-descricao,
            tl_0008-usnam     to tg_monitor-usnam,
            tl_0008-dt_criacao to tg_monitor-dt_criacao,
            tl_0008-hr_criacao to tg_monitor-hr_criacao,
            sy-uname           to tg_monitor-aprov.

      append tg_monitor.
      clear: x_aguardando, x_jarespod, tg_monitor.
    endif.
  endloop.
endform.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form limpa_campos .

  refresh: tg_monitor, tg_0007, tg_0009, tg_itens, tg_makt.
  clear: wg_desc_branch, wg_desc_parvw, wg_desc_parid, wg_desc_bukrs, wg_desc_usuario,
         wg_msg, wg_itens, wg_makt.
endform.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_A  text
*----------------------------------------------------------------------*
form modifica_status  using    p_acao.
  data: tl_0014 type table of zfiwrt0014 with header line,
        wl_input_0014 type zfiwrt0014,
        wl_linhas type sy-tabix.

  select *
    from zfiwrt0014
    into table tl_0014
     where seq_lcto eq tg_monitor-seq_lcto.

  describe table tl_0014 lines wl_linhas.

  read table tl_0014 index wl_linhas.
  read table tg_0007
    with key usnam = sy-uname.

  move: tg_monitor-seq_lcto to wl_input_0014-seq_lcto,
        tg_0007-nivel_aprov to wl_input_0014-nivel_aprov,
        p_acao              to wl_input_0014-tipo,
        sy-datum            to wl_input_0014-erdat,
        sy-uzeit            to wl_input_0014-erzet.

  wl_input_0014-dzeile = tl_0014-dzeile + 1.

  modify zfiwrt0014 from wl_input_0014.

endform.                    " MODIFICA_STATUS
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_GRID2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form montar_layout_grid2 .
  refresh t_fieldcatalog.
  perform montar_estrutura using:
        1 'ZFIWRT0009'   'ITMNUM'        'TG_ITENS' 'ITMNUM'      ' '           '6'   ' ' ' ' ' ',
        1 'ZFIWRT0009'   'MATNR'         'TG_ITENS' 'MATNR'       ' '           '15'  ' ' ' ' ' ',
        2 'MAKT'         'MAKTX'         'TG_ITENS' 'MAKTX'       ' '           '25'  ' ' ' ' ' ',
        2 'ZFIWRT0009'   'CHARG'         'TG_ITENS' 'CHARG'       ' '           '10'  ' ' ' ' ' ',
        3 'ZFIWRT0009'   'CFOP'          'TG_ITENS' 'CFOP'        ' '           '8'   ' ' ' ' ' ',
        4 'ZFIWRT0009'   'BWKEY'         'TG_ITENS' 'BWKEY'       'Centro'      '8'   ' ' ' ' ' ',
        5 'ZFIWRT0009'   'ITMTYP'        'TG_ITENS' 'ITMTYP'      ' '           '4'   ' ' ' ' ' ',
        6 'ZFIWRT0009'   'ANLN1'         'TG_ITENS' 'ANLN1'       ' '           '8'  ' ' ' ' ' ',
        7 'ZFIWRT0009'   'ANLN2'         'TG_ITENS' 'ANLN2'       ' '           '5'  ' ' ' ' ' ',
        8 'ZFIWRT0009'   'MENGE'         'TG_ITENS' 'MENGE'       'Quantidade'  '16'  ' ' 'X' ' ',
        9 'ZFIWRT0009'   'NETPR'         'TG_ITENS' 'NETPR'       'Valor Unit.' '16'  ' ' 'X' ' ',
       10 'ZFIWRT0009'   'NETWR'         'TG_ITENS' 'NETWR'       'Valot Total' '16'  ' ' 'X' ' '.
endform.                    " MONTAR_LAYOUT_GRID2
*&---------------------------------------------------------------------*
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0380   text
*      -->P_0381   text
*      -->P_0382   text
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

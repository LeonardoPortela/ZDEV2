*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Autor......: Rogério Filipsick                                       *
* Data.......: 27/08/2019                                              *
* Descrição  : Exibir informações da fatura                            *
* Transação..: ZPM0066                                                 *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data | Nome | Request | Descrição                                    *
*----------------------------------------------------------------------*
report zpmr0051.

*----------------------------------------------------------------------*
* Classes -------------------------------------------------------------*
*----------------------------------------------------------------------*
class cl_gui_column_tree definition load.
class cl_gui_cfw         definition load.

*----------------------------------------------------------------------*
* Tipos ---------------------------------------------------------------*
*----------------------------------------------------------------------*
type-pools: abap.

types: begin of ty_s_zpmt0031.
         include type zpmt0031 as data.
types:   nkey   type lvc_nkey,
         fatura type zde_fatura.
types: parent_key type lvc_nkey.
types: end of ty_s_zpmt0031.
types: ty_t_zpmt0031 type standard table of ty_s_zpmt0031
                      with default key.

types: begin of ty_tela,
         bsart      type zpmt0025-bsart,
         desc_bsart type t161t-batxt,
         pedido     type zpmt0024-pedido,
         lifnr      type zpmt0024-lifnr,
         name1      type lfa1-name1,
         fatura     type zpmt0024-fatura,
         data       type sy-datum,
         zterm      type t052-zterm,
         ztag1      type t052-ztag1,
         inco1      type tinc-inco1,
         ekorg      type t024e-ekorg,
         ekgrp      type t024-ekgrp,
         bukrs      type zpmt0025-empresa,
         liquido    type zpmt0025-vlr_total, "valor liquido pedido
         bruto      type zpmt0025-vlr_total, "valor bruto pedido
         imposto    type zpmt0025-vlr_total, "valor imposto pedido
       end of ty_tela.

types: begin of ty_aba1.
         include type zpme0043.
types: end of ty_aba1.

types: begin of ty_aba2.
         include type zpme0044.
types: end of ty_aba2.

*----------------------------------------------------------------------*
* Tabelas internas ----------------------------------------------------*
*----------------------------------------------------------------------*
data:
  t_aba1         type table of ty_aba1,
  t_aba2         type table of ty_aba2,
  t_fatura       type table of zpmt0025,
  t_zpmt0024     type table of zpmt0024,
  t_zpmt0025     type table of zpmt0025,
  t_zmmt0134     type table of zmmt0134,
  t_zpmt0034_aux type table of zpmt0034,
  t_zpmt0032     type table of zpmt0032,
  t_zpmt0034     type table of zpmt0034,
  t_obs          type table of zpme0045,
  t_zpmt0031     type ty_t_zpmt0031,
  t_zpmt0031_2   type ty_t_zpmt0031,
  t_fcat         type lvc_t_fcat,
  t_fcat2        type slis_t_fieldcat_alv,
  t_zpmt0024_2   type table of zpmt0024,
  t_zpmt0026_2   type table of zpmt0026,
  t_report       type table of zpme0039.

*----------------------------------------------------------------------*
* Estruturas ----------------------------------------------------------*
*----------------------------------------------------------------------*
data:
  gs_tela   type ty_tela,
  gs_aba1   type ty_aba1,
  gs_aba2   type ty_aba2,
  gs_layout type slis_layout_alv.

data: w_cursor_field type c length 50.

*----------------------------------------------------------------------*
* Variaveis -----------------------------------------------------------*
*----------------------------------------------------------------------*
data:
  v_no_unico type c,
  v_node_key type lvc_nkey,
  v_ucomm    type sy-ucomm,
  v_tabix    type sy-tabix,
  v_erro     type c,
  v_obrig    type c,
  go_docking type ref to cl_gui_docking_container,
  go_tree    type ref to cl_gui_alv_tree.

*----------------------------------------------------------------------*
* Constantes ----------------------------------------------------------*
*----------------------------------------------------------------------*
* Grupo de constantes que informa a aba acionada
constants:
  begin of c_abas,
    tab1 like sy-ucomm value 'ABAS_F1',
    tab2 like sy-ucomm value 'ABAS_F2',
    tab3 like sy-ucomm value 'ABAS_F3',
    tab5 like sy-ucomm value 'ABAS_F5',
  end of c_abas.

data:
  g_custom_cont_desc type ref to cl_gui_custom_container,
  obg_descbox        type ref to cl_gui_textedit,
  g_descbox          type scrfname value 'CC_DESC'.

* Declaração da Tabstrip .
controls : abas type tabstrip.

controls : zaba1 type tableview using screen 0201,
           zaba2 type tableview using screen 0202.

data:
  begin of g_abas,
    subscreen   like sy-dynnr,
    prog        like sy-repid value 'ZPMR0051',
    pressed_tab like sy-ucomm value c_abas-tab1,
  end of g_abas.

* Comando do User
data:  ok_code like sy-ucomm.

*----------------------------------------------------------------------*
*       CLASS lcl_eventhandler DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_eventhandler definition.
  public section.

    class-methods:

      handle_node_double_click
        for event node_double_click of cl_gui_alv_tree
        importing node_key,

      handle_item_double_click
        for event item_double_click of cl_gui_alv_tree
        importing node_key
                  fieldname.

endclass.                    "lcl_eventhandler DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_eventhandler IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class lcl_eventhandler implementation.

  method handle_node_double_click.

  endmethod.                    "handle_node_double_click

  method handle_item_double_click.

    clear: v_node_key.
    v_node_key = node_key.
    condense v_node_key.
    v_node_key = v_node_key - 1.
    condense v_node_key.

    delete t_zpmt0031 where fatura = space.

    if t_zpmt0031[] is not initial.
      v_ucomm = '1'.
      clear: t_aba1, t_aba2, t_fatura, t_obs.
      call screen '0200'.
    endif.

    if ok_code = 'APR' or
       ok_code = 'REP'.
      call method go_tree->delete_subtree
        exporting
          i_node_key                = node_key
          i_update_parents_expander = abap_true.

      call method cl_gui_cfw=>flush.
    endif.

  endmethod.                    "handle_item_double_click

endclass.                    "lcl_eventhandler IMPLEMENTATION

*----------------------------------------------------------------------*
* START-OF-SELECTION --------------------------------------------------*
*----------------------------------------------------------------------*
start-of-selection.

  call screen '0100'.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0100 output.

  set pf-status 'PF_0100'.
  set titlebar 'TITULO_0100'.

  perform f_init_controls.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0100 input.

  case sy-ucomm.
    when 'BACK'.
      set screen 0.
    when 'CANC' or 'EXIT'.
      leave to screen 0.
  endcase.

endmodule.

*&---------------------------------------------------------------------*
*&      Form  INIT_CONTROLS
*&---------------------------------------------------------------------*
form f_init_controls .

* create Hierarchy-header
  data ls_hierarchy_header type treev_hhdr.

* Create docking container
  create object go_docking
    exporting
      parent = cl_gui_container=>screen0
      ratio  = 90
    exceptions
      others = 6.

* create tree control
  create object go_tree
    exporting
      parent                      = go_docking
      node_selection_mode         = cl_gui_column_tree=>node_sel_mode_multiple
      item_selection              = 'X'  " required for double-click event on item
      no_html_header              = ''
      no_toolbar                  = ''
    exceptions
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      illegal_node_selection_mode = 5
      failed                      = 6
      illegal_column_name         = 7.

  if sy-subrc <> 0.
    message x208(00) with 'ERROR'.                          "#EC NOTEXT
  endif.

  perform f_build_hierarchy_header changing ls_hierarchy_header.

  perform f_build_fieldcatalog.

* create emty tree-control
  call method go_tree->set_table_for_first_display
    exporting
      i_structure_name    = 'ZPMT0031'
      i_default           = 'X'
      is_hierarchy_header = ls_hierarchy_header
    changing
      it_outtab           = t_zpmt0031
      it_fieldcatalog     = t_fcat.

* create hierarchy
  perform f_create_hierarchy.

  if t_zpmt0031[] is initial.
    message s000(z_les) with text-001 display like 'S'.
    leave to screen 0.
  endif.

* register events
  perform f_register_events.

* adjust column_width
  call method go_tree->column_optimize.


endform.

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_HIERARCHY_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LS_HIERARCHY_HEADER  text
*----------------------------------------------------------------------*
form f_build_hierarchy_header  changing p_hierarchy_header type treev_hhdr.

  p_hierarchy_header-heading = 'Tipo Documento'.            "#EC NOTEXT
  p_hierarchy_header-tooltip =
                         'Tipo Documento'.                  "#EC NOTEXT
  p_hierarchy_header-width = 30.
  p_hierarchy_header-width_pix = ''.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_build_fieldcatalog .

  refresh: t_fcat.
  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name       = 'ZPMT0031'
    changing
      ct_fieldcat            = t_fcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_CREATE_HIERARCHY
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_create_hierarchy .

  data: ld_fatura_key type lvc_nkey,
        ld_item_key   type lvc_nkey,
        ld_last_key   type lvc_nkey.
  data: it_node_key      type lvc_t_nkey,
        t_t_zpmt0034_aux type table of zpmt0034,
        t_aux_25         type table of ty_s_zpmt0031.

  break rfilipsick.

  select *
    from zpmt0034
    into table t_zpmt0034
   where usuario = sy-uname.

  if sy-subrc ne 0.
* Usuário não cadastrado - transação ZPM0068 !
    message s000(z_les) with text-011 display like 'S'.
    leave to screen 0.
  endif.

  "====================================================Ajuste bug 122060 / aoenning
  t_zpmt0034_aux = value #( for l in t_zpmt0034 (
     usuario = l-usuario
       matnr = |{ l-matnr alpha = in }|
       werks = l-werks
       bsart = l-bsart
cod_material = |{ l-cod_material  alpha = in }|
  ) ).

  if t_zpmt0034_aux is not initial.
    append lines of t_zpmt0034_aux to  t_zpmt0034.
  endif.
  "====================================================Ajuste bug 122060 / aoenning


  select *
    from zpmt0025
    into corresponding fields of table t_zpmt0031
     for all entries in  t_zpmt0034
   where cod_material eq t_zpmt0034-cod_material
     and centro eq  t_zpmt0034-werks
     and cod_status = '1'.

  sort t_zpmt0031 by fatura.

***
  loop at t_zpmt0031 assigning field-symbol(<fs_fatura>).
    collect <fs_fatura> into t_aux_25.
  endloop.

  clear: t_zpmt0031.
  move-corresponding t_aux_25[] to t_zpmt0031[].
***

  delete adjacent duplicates from t_zpmt0031 comparing fatura.
  move-corresponding t_zpmt0031[] to t_zpmt0031_2[].

  loop at t_zpmt0031_2 assigning field-symbol(<fs_zpmt0031>).

    perform f_add_customer_line using  <fs_zpmt0031>-data
                                         ''
                              changing ld_fatura_key.

    if ld_fatura_key  is not initial.
      append ld_fatura_key to it_node_key.
    endif.

    on change of <fs_zpmt0031>-fatura.
      perform f_add_item_line using <fs_zpmt0031>-data
                                    <fs_zpmt0031>-fatura
                                    ld_fatura_key
                           changing ld_item_key.
    endon.
    delete t_zpmt0031_2 where fatura = <fs_zpmt0031>-fatura.
  endloop.

*" calculate totals
  call method go_tree->update_calculations.

* this method must be called to send the data to the frontend
  call method go_tree->frontend_update.

  delete adjacent duplicates from it_node_key comparing all fields.
  call method go_tree->expand_nodes( it_node_key = it_node_key ).

endform.

*&---------------------------------------------------------------------*
*&      Form  F_register_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form f_register_events.
* define the events which will be passed to the backend
  data: lt_events type cntl_simple_events,
        l_event   type cntl_simple_event.

* define the events which will be passed to the backend
  l_event-eventid = cl_gui_column_tree=>eventid_expand_no_children.
  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_node_double_click.
  append l_event to lt_events.
  l_event-eventid = cl_gui_column_tree=>eventid_item_double_click.
  append l_event to lt_events.

  call method go_tree->set_registered_events
    exporting
      events                    = lt_events
    exceptions
      cntl_error                = 1
      cntl_system_error         = 2
      illegal_event_combination = 3.
  if sy-subrc <> 0.
    message x208(00) with 'ERROR'.                          "#EC NOTEXT
  endif.

* set Handler
  set handler:
    lcl_eventhandler=>handle_node_double_click for go_tree,
    lcl_eventhandler=>handle_item_double_click for go_tree.

endform.                               " register_events

*&---------------------------------------------------------------------*
*&      Form  F_ADD_CUSTOMER_LINE
*&---------------------------------------------------------------------*
form f_add_customer_line  using us_data      type ty_s_zpmt0031-data
                                ud_relat_key type lvc_nkey
                       changing cd_node_key  type lvc_nkey.

  data: l_node_text type lvc_value.

* set item-layout
  data: lt_item_layout type lvc_t_layi,
        ls_item_layout type lvc_s_layi.

  ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  append ls_item_layout to lt_item_layout.

* add node
  if v_no_unico = space.
    l_node_text =  'Fatura'.

    data: ls_node type lvc_s_layn.

    call method go_tree->add_node
      exporting
        i_relat_node_key = ud_relat_key
        i_relationship   = cl_gui_column_tree=>relat_last_child
        i_node_text      = l_node_text
*       IS_OUTTAB_LINE   = US_DATA
        is_node_layout   = ls_node
        it_item_layout   = lt_item_layout
      importing
        e_new_node_key   = cd_node_key.
  endif.

  v_no_unico = abap_true.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_ADD_ITEM_LINE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_LS_OUTTAB_DATA  text
*      -->P_LD_FATURA_KEY  text
*      <--P_LD_ITEM_KEY  text
*----------------------------------------------------------------------*
form f_add_item_line  using  us_data      type ty_s_zpmt0031-data
                             fatura       type zde_fatura
                             ud_relat_key type lvc_nkey
                    changing cd_node_key  type lvc_nkey.

  data: l_node_text type lvc_value.
  data: ls_node type lvc_s_layn.

* set item-layout
  data: lt_item_layout type lvc_t_layi,
        ls_item_layout type lvc_s_layi.

  ls_item_layout-fieldname = go_tree->c_hierarchy_column_name.
  ls_item_layout-style   =
                        cl_gui_column_tree=>style_intensifd_critical.
  append ls_item_layout to lt_item_layout.

* add node
  l_node_text       =  fatura.
  ls_node-n_image   = space.
  ls_node-exp_image = space.

  call method go_tree->add_node
    exporting
      i_relat_node_key = ud_relat_key
      i_relationship   = cl_gui_column_tree=>relat_last_child
      i_node_text      = l_node_text
      is_outtab_line   = us_data
      is_node_layout   = ls_node
      it_item_layout   = lt_item_layout
    importing
      e_new_node_key   = cd_node_key.

endform.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0200 output.

  set pf-status 'PF_0200'.
  set titlebar 'TITULO_0200'.

  condense v_ucomm no-gaps.
  if v_ucomm = '1'.
    v_ucomm = v_ucomm + 1.
    perform f_seleciona_dados.
  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0200 input.

  data: l_bruto type zpmt0025-vlr_total.
  data: vlr_tot_bruto_item type zpmt0025-vlr_total.

  clear: v_erro, v_tabix.

  get cursor field w_cursor_field.
  case sy-ucomm.
    when 'ABAS_F2'.
      read table t_aba2 assigning field-symbol(<ws_aba2>) index 1.
      if sy-subrc eq 0.
        if <ws_aba2>-costcenter ne gs_aba2-costcenter and gs_aba2-costcenter is not initial.
          <ws_aba2>-costcenter = gs_aba2-costcenter.
        endif.

        if <ws_aba2>-ordem ne gs_aba2-ordem and gs_aba2-ordem is not initial.
          <ws_aba2>-ordem = gs_aba2-ordem.
        endif.

      endif.
    when 'BACK'.
      clear: t_aba1, t_aba2, gs_aba1.

      data: p_respo type c.

      call function 'POPUP_TO_CONFIRM'
        exporting        "TITLEBAR = 'Confirmar'
          text_question         = 'Sair da transação sem salvar as alterações?'
          text_button_1         = 'Sim'
          text_button_2         = 'Não'
          display_cancel_button = ' '
        importing
          answer                = p_respo.

      if p_respo = 1.

        set screen 0.
*        SET SCREEN 0.
      else.
        v_ucomm = '1'.
      endif.
      clear: p_respo.

    when 'CANC' or 'EXIT'.
*      CLEAR: T_ABA1, T_ABA2, GS_ABA1.

*      CLEAR: P_RESPO.

*      CALL FUNCTION 'POPUP_TO_CONFIRM'
*        EXPORTING        "TITLEBAR = 'Confirmar'
*          TEXT_QUESTION         = 'Sair da transação sem salvar as alterações?'
*          TEXT_BUTTON_1         = 'Sim'
*          TEXT_BUTTON_2         = 'Não'
*          DISPLAY_CANCEL_BUTTON = ' '
*        IMPORTING
*          ANSWER                = P_RESPO.
*
*      IF P_RESPO = 1.
*
*        LEAVE TO SCREEN 0.
      set screen 0.
*      ELSE.
*        V_UCOMM = '1'.
*      ENDIF.
*      CLEAR: P_RESPO.

    when 'APR'.
      perform f_aprovar.
      if v_obrig is initial.
        leave to screen 0.
      endif.
    when 'REP'.
      perform f_reprovar.
      if v_obrig is initial.
        leave to screen 0.
      endif.
    when 'DOUBLE'.
      if w_cursor_field = 'GS_TELA-FATURA'.
        perform f_alv.
      endif.
    when 'ABAS_F5'.

      call method obg_descbox->set_text_as_r3table
        exporting
          table = t_obs.

      call method obg_descbox->set_readonly_mode
        exporting
          readonly_mode = 0.

    when 'ABAS_F3'.
      clear: gs_tela-liquido, gs_tela-bruto, gs_tela-imposto, vlr_tot_bruto_item.
      loop at t_aba1 assigning field-symbol(<fs_aba1>).
        clear: l_bruto.
        gs_tela-liquido = gs_tela-liquido + <fs_aba1>-netpr.
        l_bruto = <fs_aba1>-bruto * <fs_aba1>-menge.
        add  l_bruto to vlr_tot_bruto_item.
      endloop.
      gs_tela-bruto = vlr_tot_bruto_item.
      gs_tela-imposto = gs_tela-bruto - gs_tela-liquido.
  endcase.

endmodule.

*&---------------------------------------------------------------------*
*&  Include           MZ_ABASO01                                       *
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  ABAS_SET  OUTPUT
*&---------------------------------------------------------------------*
module abas_set output.


  abas-activetab = g_abas-pressed_tab.
  case g_abas-pressed_tab.
    when c_abas-tab1.
      g_abas-subscreen = '0201'.
    when c_abas-tab2.
      g_abas-subscreen = '0202'.
    when c_abas-tab3.
      g_abas-subscreen = '0204'.
    when c_abas-tab5.
      g_abas-subscreen = '0205'.
    when others.
  endcase.
endmodule.                 " ABAS_SET  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  ABAS_GET  INPUT
*&---------------------------------------------------------------------*
module abas_get input.

  ok_code = sy-ucomm.
  case ok_code.
    when c_abas-tab1.
      g_abas-pressed_tab = c_abas-tab1.
    when c_abas-tab2.
      g_abas-pressed_tab = c_abas-tab2.
    when c_abas-tab3.
      g_abas-pressed_tab = c_abas-tab3.
    when c_abas-tab5.
      g_abas-pressed_tab = c_abas-tab5.
    when others.
  endcase.

endmodule.                 " ABAS_GET  INPUT
*&---------------------------------------------------------------------*
*&      Form  F_APROVAR
*&---------------------------------------------------------------------*
form f_aprovar .

  clear: t_fatura, v_obrig.
  if gs_tela-zterm is initial.
    message s000(z_les) with text-004 display like 'E'.
    v_obrig = abap_true.
    return.
  endif.

  "====================================================Ajuste bug 122060 / aoenning
  "Selecionar classificação contabil material.
  select a~matnr, a~matkl, b~saknr
     from mara as a
     inner join zmmt0039 as b on b~matkl eq a~matkl
     into table @data(it_grupmat)
      for all entries in @t_aba1
      where matnr eq @t_aba1-matnr.
  "====================================================Ajuste bug 122060 / aoenning

  loop at t_aba1 assigning field-symbol(<fs_aba1>).

    if gs_aba1-mwskz is initial or
       gs_aba1-matnr is initial or
       gs_aba1-por   eq 0       or
       gs_aba1-data_remessa is initial.
      message s000(z_les) with text-007 text-008 display like 'E'.
      v_obrig = abap_true.
      exit.
    endif.

    read table it_grupmat into data(ws_grupmat) with key matnr = <fs_aba1>-matnr.
    if sy-subrc eq 0.
      <fs_aba1>-gl_account = ws_grupmat-saknr.
    endif.

    append initial line to t_fatura assigning field-symbol(<fs_fatura>).
    <fs_fatura>-item          = <fs_aba1>-ebelp.
    <fs_fatura>-acctasscat    = <fs_aba1>-acctasscat.
    <fs_fatura>-gl_account    = <fs_aba1>-gl_account.
    <fs_fatura>-tax_code      = <fs_aba1>-mwskz.
    <fs_fatura>-matnr         = <fs_aba1>-matnr.
    <fs_fatura>-desc_material = <fs_aba1>-desc_material.
    <fs_fatura>-qtde          = <fs_aba1>-menge.
    <fs_fatura>-und           = <fs_aba1>-und.
    <fs_fatura>-dt_fatura     = sy-datum.
    <fs_fatura>-data_remessa  = <fs_aba1>-data_remessa.
    <fs_fatura>-vlr_unt       = <fs_aba1>-vlr_unt.
    <fs_fatura>-centro        = <fs_aba1>-centro.
    <fs_fatura>-werks         = <fs_aba1>-centro.
    <fs_fatura>-price_unit    = <fs_aba1>-por.
    <fs_fatura>-fatura        = gs_tela-fatura.
    <fs_fatura>-lifnr         = gs_tela-lifnr.
    <fs_fatura>-zterm         = gs_tela-zterm.
    <fs_fatura>-ekgrp         = gs_tela-ekgrp.
    <fs_fatura>-co_area       = gs_tela-ekorg.
    <fs_fatura>-bsart         = gs_tela-bsart.

    read table t_aba2 assigning field-symbol(<fs_aba2>)
                                    with key fatura = <fs_aba1>-fatura.
    if sy-subrc is initial.
      <fs_fatura>-activity      = <fs_aba2>-activity.
      <fs_fatura>-costcenter    = <fs_aba2>-costcenter.
      <fs_fatura>-ordem         = <fs_aba2>-ordem.
    endif.

  endloop.

  check v_obrig is initial.

***Criar pedido.
  zcl_webservic_protheus=>set_pedido( exporting t_fatura  = t_fatura
                                                t_obs     = t_obs
                                      importing e_pedido  = data(p_pedido)
                                                e_message = data(p_message)
                                      receiving e_returng = data(t_return) ).

  if p_pedido is not initial.
    loop at t_zpmt0024 assigning field-symbol(<fs_zpmt0024>).
      <fs_zpmt0024>-cod_status  = '2'.
      <fs_zpmt0024>-pedido      = p_pedido.
      <fs_zpmt0024>-status_proc = 'Pedido aguardando aprovação'.
    endloop.

    loop at t_zpmt0025 assigning field-symbol(<fs_zpmt0025>).
      <fs_zpmt0025>-cod_status  = '2'.
      <fs_zpmt0025>-pedido      = p_pedido.
      <fs_zpmt0025>-ordem       = |{ gs_aba2-ordem alpha = in }|.
      <fs_zpmt0025>-costcenter  = |{ gs_aba2-costcenter  alpha = in }|.
      <fs_zpmt0025>-status_proc = 'Pedido aguardando aprovação'.
    endloop.

    loop at t_zpmt0032 assigning field-symbol(<fs_zpmt0032>).
      <fs_zpmt0032>-cod_status   = '2'.
      <fs_zpmt0032>-pedido      = p_pedido.
      <fs_zpmt0032>-status_proc = 'Pedido aguardando aprovação'.
    endloop.

    modify zpmt0025 from table t_zpmt0025.
    modify zpmt0024 from table t_zpmt0024.
    modify zpmt0032 from table t_zpmt0032.
    commit work.

    message s000(z_les) with text-009 p_pedido text-010 display like 'S'.
  else.
    message s000(z_les) with p_message display like 'E'.
    v_obrig = abap_true.
    return.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_REPROVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_reprovar .

  data: t_ch_text type table of txline,
        l_flag    type c,
        l_texto   type string.

  clear: v_obrig.
  call function 'CATSXT_SIMPLE_TEXT_EDITOR'
    exporting
      im_title        = 'Inserir a observação'
      im_start_column = 50
      im_start_row    = 5
    changing
      ch_text         = t_ch_text.

  if t_ch_text[] is initial.

    message s000(z_les) with text-002 display like 'E'.
    v_obrig = abap_true.

  elseif t_ch_text[] is not initial.

    loop at t_ch_text assigning field-symbol(<fs_ch_text>).
      concatenate l_texto <fs_ch_text>
      into l_texto separated by space.
    endloop.

    loop at t_zpmt0024 assigning field-symbol(<fs_zpmt0024>).
      <fs_zpmt0024>-cod_status  = '999'.
      <fs_zpmt0024>-status_proc = 'Erro'.
      <fs_zpmt0024>-observ      = l_texto.
    endloop.

    loop at t_zpmt0025 assigning field-symbol(<fs_zpmt0025>).
      <fs_zpmt0025>-cod_status  = '999'.
      <fs_zpmt0025>-status_proc = 'Erro'.
      <fs_zpmt0025>-observ      = l_texto.
    endloop.

    loop at t_zpmt0032 assigning field-symbol(<fs_zpmt0032>).
      <fs_zpmt0032>-cod_status  = '999'.
      <fs_zpmt0032>-status_proc = 'Erro'.
      <fs_zpmt0032>-observ      = l_texto.
    endloop.

    modify zpmt0025 from table t_zpmt0025.
    modify zpmt0024 from table t_zpmt0024.
    modify zpmt0032 from table t_zpmt0032.
    commit work.

  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
form f_seleciona_dados.

  data:
    l_valor   type zpmt0025-vlr_total.

  clear: v_erro, v_tabix.

  if t_aba1[] is initial.
    clear: gs_tela, t_zpmt0025, t_zpmt0024, t_zpmt0032, t_obs.
    read table t_zpmt0031 assigning field-symbol(<fs_zpmt0031>) index v_node_key.

    if sy-subrc is initial.

      select *
        from zpmt0025
        into table t_zpmt0025
       where fatura = <fs_zpmt0031>-fatura
         and cnpj   = <fs_zpmt0031>-cnpj.

      if sy-subrc is initial.

*CS2019001803 - Inicio
        select *
          from zpmt0034
          into table t_zpmt0034_aux
           for all entries in t_zpmt0025
         where cod_material = t_zpmt0025-cod_material
            and werks = t_zpmt0025-werks.

        sort t_zpmt0034_aux by matnr werks.

        delete adjacent duplicates from t_zpmt0034_aux comparing matnr werks.

        if  t_zpmt0034_aux is not initial.
          select *
            from zmmt0134
            into table t_zmmt0134
             for all entries in  t_zpmt0034_aux
           where matnr =  t_zpmt0034_aux-matnr
               and werks =  t_zpmt0034_aux-werks
               and ativo = 'X'.
        endif.
*CS2019001803 - fim

        select *
          from zpmt0024
          into table t_zpmt0024
         where fatura = <fs_zpmt0031>-fatura
          and  cnpj   = <fs_zpmt0031>-cnpj.

        select *
          from zpmt0032
          into table t_zpmt0032
         where fatura = <fs_zpmt0031>-fatura
          and  cnpj   = <fs_zpmt0031>-cnpj.

        loop at t_zpmt0025 assigning field-symbol(<fs_zpmt0025>).

          read table t_zpmt0034 assigning field-symbol(<fs_zpmt0034>)
                                              with key cod_material = <fs_zpmt0025>-cod_material
                                                       werks        = <fs_zpmt0025>-centro.

          if gs_tela-fatura is initial. "PEDIDO
            if <fs_zpmt0025>-zterm is assigned.
              select single ztag1
                from t052
                into gs_tela-ztag1
               where zterm = <fs_zpmt0025>-zterm.
            endif.

            select single name1
              from lfa1
              into gs_tela-name1
             where lifnr = <fs_zpmt0025>-lifnr.

            if <fs_zpmt0034> is assigned.
              gs_tela-bsart  = <fs_zpmt0034>-bsart.

              select single batxt
                from t161t
                into gs_tela-desc_bsart
               where bsart =  gs_tela-bsart
                 and spras = sy-langu.
            endif.

            gs_tela-pedido = <fs_zpmt0025>-pedido.
            gs_tela-lifnr  = <fs_zpmt0025>-lifnr.
            gs_tela-fatura = <fs_zpmt0025>-fatura.
            gs_tela-data   = sy-datum.
            gs_tela-inco1  = 'CIF'.
            gs_tela-ekorg  = 'OC01'.
            gs_tela-ekgrp  = <fs_zpmt0025>-ekgrp.
            gs_tela-bukrs  = '0001'.
          endif.

          gs_aba1-fatura        =  <fs_zpmt0025>-fatura.
          gs_aba1-ebelp         = <fs_zpmt0025>-item.
          gs_aba1-acctasscat    = <fs_zpmt0025>-acctasscat.
          gs_aba1-mwskz         = <fs_zpmt0025>-tax_code.
          gs_aba1-menge         = <fs_zpmt0025>-qtde.
          gs_aba1-data_remessa  = sy-datum.
          gs_aba1-desc_material = <fs_zpmt0025>-desc_material.
          gs_aba1-netpr         = <fs_zpmt0025>-vlr_unt.
          gs_aba1-bruto         = <fs_zpmt0025>-vlr_unt.
          gs_aba1-vlr_unt       = <fs_zpmt0025>-vlr_unt.
          gs_aba1-centro        = <fs_zpmt0025>-centro.

*CS2019001803 - Inicio
          read table t_zpmt0034_aux assigning field-symbol(<fs_zpmt0034_aux>)
                                              with key cod_material = <fs_zpmt0025>-cod_material
                                                       werks        = <fs_zpmt0025>-centro.

          if <fs_zpmt0034_aux> is assigned.
            read table  t_zmmt0134 assigning field-symbol(<fs_zmmt0134>)
                                               with key matnr = <fs_zpmt0034_aux>-matnr
                                                        werks = <fs_zpmt0034_aux>-werks.

            if <fs_zmmt0134> is assigned.
              gs_aba1-zterm   = <fs_zmmt0134>-cod_pgto. "código pagamento
              gs_aba1-mwskz   = <fs_zmmt0134>-cod_imp.  "condigo imposto
              gs_aba1-por     = <fs_zmmt0134>-qnt_por.  "valor calculado por
              gs_aba1-vlr_unt = <fs_zmmt0134>-prec_liq. "preço liquido
              gs_aba1-matnr   = <fs_zmmt0134>-matnr.    "código do material.

              unassign <fs_zpmt0034_aux>.
              unassign <fs_zmmt0134>.


            endif.
          endif.
*CS2019001803 - Fim

          append gs_aba1 to t_aba1.
          clear: gs_aba1.

          gs_aba2-fatura     =  <fs_zpmt0025>-fatura.
          gs_aba2-costcenter = <fs_zpmt0025>-costcenter.
          gs_aba2-ordem      = <fs_zpmt0025>-ordem.
          gs_aba2-activity   = <fs_zpmt0025>-activity.
          append gs_aba2 to t_aba2.
          clear: gs_aba2.
        endloop.

      endif.
    endif.
  endif.

  delete adjacent duplicates from t_aba2.

  if t_aba1[] is initial.
    message s000(z_les) with text-003 display like 'S'.
    leave to screen 0.
  endif.

  if sy-ucomm ne 'ABAS_F1' and
     sy-ucomm ne 'ABAS_F2' and
     sy-ucomm ne 'ABAS_F3' and
     sy-ucomm ne 'ABAS_F5'.
    loop at t_aba1 assigning field-symbol(<fs_aba1>).
      gs_tela-liquido = gs_tela-liquido + <fs_aba1>-netpr.
    endloop.
  endif.

endform.
*&---------------------------------------------------------------------*
*&      Module  ATUALIZA_ABA1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module atualiza_aba1 input.

  data: l_calculo type zpme0043-vlr_unt.

  get cursor field w_cursor_field.
  if w_cursor_field = 'GS_TELA-ZTERM'.
    return.
  endif.

  if sy-ucomm = 'BACK' or
     sy-ucomm = 'CANC' or
     sy-ucomm = 'EXIT' or
     sy-ucomm = 'DOUBLE'.
    return.
  endif.

  if v_erro = abap_true.
    return.
  endif.

  v_tabix = v_tabix + sy-tabix.

  if gs_aba1-mwskz is initial or
     gs_aba1-matnr is initial or
     gs_aba1-por   eq 0       or
     gs_aba1-data_remessa is initial.
    modify t_aba1 from gs_aba1 index v_tabix.
    v_erro = abap_true.
    message s000(z_les) with text-007 text-008 display like 'E'.
    return.
  endif.

  select single meins
    from mara
    into gs_aba1-und
   where matnr = gs_aba1-matnr.

  select single maktx
    from makt
    into gs_aba1-desc_material
   where matnr = gs_aba1-matnr
     and spras = sy-langu.

  zcl_webservic_protheus=>get_conta_razao( exporting matnr = gs_aba1-matnr
                                           importing saknr = data(w_saknr) ).
  gs_aba1-gl_account = w_saknr.

  clear: l_calculo.
  l_calculo = ( gs_aba1-vlr_unt / gs_aba1-por ) * gs_aba1-menge.
  gs_aba1-netpr = l_calculo.

  modify t_aba1 from gs_aba1 index v_tabix.

* Legenda
*  GS_ABA1-VLR_UNT = Preço Líquido
*  GS_ABA1-NETPR   = Valor Liquido

endmodule.
*&---------------------------------------------------------------------*
*&      Module  MARK  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module mark input.

  modify t_aba1 from gs_aba1 index zaba1-current_line transporting mark.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0201  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0201 output.

  data: l_lines like sy-index.

  if zaba1-lines is not initial.
    describe table t_aba1 lines l_lines.
    zaba1-lines = l_lines.
  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module cria_objetos output.

  if g_custom_cont_desc is initial.
    create object g_custom_cont_desc
      exporting
        container_name = g_descbox.

    if g_custom_cont_desc is not initial.
      create object obg_descbox
        exporting
          parent            = g_custom_cont_desc
          wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = 200 "72
          max_number_chars  = 350.

      call method obg_descbox->set_toolbar_mode
        exporting
          toolbar_mode = '0'.

      call method obg_descbox->set_readonly_mode
        exporting
          readonly_mode = 1.
    endif.
  endif.

endmodule.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0205  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0205 input.

  case sy-ucomm.

    when 'ABAS_F1' or 'ABAS_F2' or 'ABAS_F3'.

      if obg_descbox is not initial.
        clear t_obs.
        call method obg_descbox->get_text_as_r3table
          importing
            table = t_obs.
      endif.

  endcase.

endmodule.
*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       ALV
*----------------------------------------------------------------------*
form f_alv .

  if gs_tela-fatura is not initial.

    clear: t_zpmt0026_2.
    select *
      from zpmt0026
      into table t_zpmt0026_2
      for all entries in t_zpmt0025
     where fatura = t_zpmt0025-fatura "gs_tela-fatura
       and cnpj   = t_zpmt0025-cnpj. "gs_tela-CNPJ.

    if sy-subrc is initial.

      clear: t_zpmt0024_2.
      select *
        from zpmt0024
        into table t_zpmt0024_2
         for all entries in t_zpmt0026_2
       where fatura = t_zpmt0026_2-fatura.

    endif.
  endif.

  clear: t_report.
  loop at t_zpmt0026_2 assigning field-symbol(<zpmt0026>).

    read table t_zpmt0024_2 assigning field-symbol(<zpmt0024>)
                                        with key fatura       = <zpmt0026>-fatura
                                                 cupom_fisc   = <zpmt0026>-cupom_fisc.
    if sy-subrc is initial.

      append initial line to t_report assigning field-symbol(<fs_report>).

      <fs_report>-fatura        = <zpmt0026>-fatura.
      <fs_report>-desc_material = <zpmt0026>-desc_material.
      <fs_report>-qtde          = <zpmt0026>-qtde.
      <fs_report>-vlr_unt       = <zpmt0026>-vlr_unt.
      <fs_report>-vlr_total     = <zpmt0026>-vlr_total.
*            <FS_REPORT>-UND           = <ZPMT0026>-UND.

      <fs_report>-dt_fatura     = <zpmt0024>-dt_fatura.
      <fs_report>-hr_fatura     = <zpmt0024>-hr_fatura.
      <fs_report>-cupom_fisc    = <zpmt0024>-cupom_fisc.
      <fs_report>-dt_cupom_fisc = <zpmt0024>-dt_cupom_fisc.
      <fs_report>-hr_cupom_fisc = <zpmt0024>-hr_cupom_fisc.
      <fs_report>-dt_exportacao = <zpmt0024>-dt_exportacao.
      <fs_report>-hr_exportacao = <zpmt0024>-hr_exportacao.
      <fs_report>-placa         = <zpmt0024>-placa.
      <fs_report>-odometro      = <zpmt0024>-odometro.
      <fs_report>-ordem         = <zpmt0024>-ordem.
      <fs_report>-status_proc   = <zpmt0024>-status_proc.
    endif.
  endloop.

  perform f_monta_alv.

endform.
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_ALV
*&---------------------------------------------------------------------*
form f_monta_alv .

  data: lv_program type sy-repid.

  lv_program  = sy-repid.
  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = lv_program
      i_structure_name       = 'ZPME0039'
    changing
      ct_fieldcat            = t_fcat2
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

  delete t_fcat2 where fieldname = 'FLAG'.

  gs_layout-expand_all = abap_true.
  gs_layout-colwidth_optimize = abap_true.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program = lv_program
      is_layout          = gs_layout
      it_fieldcat        = t_fcat2
    tables
      t_outtab           = t_report
    exceptions
      program_error      = 1
      others             = 2.

endform.

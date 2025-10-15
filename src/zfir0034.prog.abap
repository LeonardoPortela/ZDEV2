*&---------------------------------------------------------------------*
*& Report ZFIR0034
*& Solicitação Adiantamento - Estratégia de Liberação
*&---------------------------------------------------------------------*
*& Autor            Request       Data
*& Não informado    --            --
*& Marcos Faneli    DEVK937219    06.05.2014
*&---------------------------------------------------------------------*

report  zfir0034.

types: begin of ty_cadlote,
         empresa(30) type c,
         nro_sol     type zfit0045-nro_sol,
         usuario(20) type c,
         total(30), "       TYPE ZFIT0046-VLR_ADIANTAMENTO,
         dep_resp(2),
         data(10),
       end of ty_cadlote,


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
         bukrs     type zadt_sol_aprov-bukrs,
         nro_sol   type zadt_sol_aprov-nro_sol,
         valor_de  type zadto_aprovador-valor_de,
         valor_ate type zadto_aprovador-valor_ate,
         aprovador type zadto_aprovador-aprovador,
         nivel     type zadto_aprovador-nivel,
         estado(4),
         opcoes(4),
       end of ty_estra,

       begin of ty_user,
         bname     type v_usr_name-bname,
         name_text type v_usr_name-name_text,
       end of ty_user,

       begin of ty_makt,
         matnr type makt-matnr,
         maktx type makt-maktx,
       end of ty_makt,

       begin of ty_lfa1,
         lifnr type lfa1-lifnr,
         name1 type lfa1-name1,
       end of ty_lfa1,

       begin of ty_docs ,
         nro_sol          type zadt_sol_aprov-nro_sol,
         ebeln            type zfit0046-ebeln,
         lifnr            type zfit0045-lifnr,
         name1            type lfa1-name1,
         lifnr_po         type zfit0045-lifnr,
         name1_po         type lfa1-name1,
         ebelp            type zfit0046-ebelp,
         matnr            type zfit0046-matnr,
         maktx            type makt-maktx,
         saldo_item       type zfit0046-saldo_item,
         pgtos_real       type zfit0046-pgtos_real,
         sdo_disponivel   type zfit0046-sdo_disponivel,
         vlr_adiantamento type zfit0046-vlr_adiantamento,
         dt_pgto          type zfit0045-dt_pgto,
         motivo           type zfit0045-motivo,
         dt_prev_liq      type zfit0045-dt_prev_liq,
         moeda_pgto       type zfit0045-moeda_pgto,
         solicitante      type v_usr_name-name_text,
         negociador       type v_usr_name-name_text,
         cellcolor        type lvc_t_scol,
       end of ty_docs,

       begin of ty_zimp_cad_depto,
         dep_resp      type zimp_cad_depto-dep_resp,
         dep_resp_desc type zimp_cad_depto-dep_resp_desc,
       end of ty_zimp_cad_depto,

       begin of ty_t001,
         bukrs type t001-bukrs,
         butxt type t001-butxt,
       end of ty_t001,


       begin of ty_zadto_aprovador,
         bukrs     type zadto_aprovador-bukrs,
         bukrs_ate type zadto_aprovador-bukrs_ate,
         dep_resp  type zadto_aprovador-dep_resp,
         waers     type zadto_aprovador-waers,
         nivel     type zadto_aprovador-nivel,
         aprovador type zadto_aprovador-aprovador,
         valor_de  type zadto_aprovador-valor_de,
         valor_ate type zadto_aprovador-valor_ate,
       end of ty_zadto_aprovador,

       begin of ty_zadt_sol_aprov,
         bukrs      type zadt_sol_aprov-bukrs,
         nro_sol    type zadt_sol_aprov-nro_sol,
         nivel      type zadt_sol_aprov-nivel,
         aprovador  type zadt_sol_aprov-aprovador,
         valor_de   type zadt_sol_aprov-valor_de,
         valor_ate  type zadt_sol_aprov-valor_ate,
         data_atual type zadt_sol_aprov-data_atual,
         hora_atual type zadt_sol_aprov-hora_atual,
         usuario    type zadt_sol_aprov-usuario,
       end of ty_zadt_sol_aprov.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
data: ok-code         type sy-ucomm,
      tg_selectedcell type lvc_t_cell,
      wg_selectedcell type lvc_s_cell,



      begin of tg_lotes occurs 0,
        status(4),
        empresa(30)  type c,
        nro_sol      type zfit0045-nro_sol,
        dep_resp(25) type c,
        dt_venc      type zfit0045-dt_prev_liq,
        total(30), "        TYPE ZFIT0046-VLR_ADIANTAMENTO,
        limite       type zfit0045-limite,
        saldo        type zfit0045-saldo,
        moeda_pgto   type zfit0045-moeda_pgto,
        color(4),
      end of tg_lotes.

data   dyfields like dynpread occurs 1 with header line.

** Criação de tabela dinamica
data: t_fieldcatalog     type lvc_t_fcat,
      w_fieldcatalog     type lvc_s_fcat,
      wa_layout          type lvc_s_layo,
      wa_stable          type lvc_s_stbl,
      wg_editor          type ty_editor,
      wg_cadlote         type ty_cadlote,

      wa_zfit0045        type zfit0045,
      wa_zfit0046        type zfit0046,

      wa_zimp_cad_depto  type ty_zimp_cad_depto,
      wa_zadto_aprovador type ty_zadto_aprovador,
      wa_zadt_sol_aprov  type ty_zadt_sol_aprov,
      wa_user            type ty_user,
      wa_makt            type ty_makt,
      wa_lfa1            type ty_lfa1,
      wa_t001            type ty_t001,
      wa_estra           type ty_estra,
      wa_docs            type ty_docs,
      wa_cellcolor       type lvc_s_scol,

      tg_fields          type table of ty_fields   with header line,

      tg_editor          type table of ty_editor,
      tg_estra           type table of ty_estra,
      tg_docs            type table of ty_docs,
      it_docs            type table of ty_docs,

      it_zfit0045        type table of zfit0045,
      it_zfit0046        type table of zfit0046,

      it_zimp_cad_depto  type table of ty_zimp_cad_depto,
      it_zadto_aprovador type table of ty_zadto_aprovador,
      it_zadt_sol_aprov  type table of ty_zadt_sol_aprov,
      it_user            type table of ty_user,
      it_makt            type table of ty_makt,
      it_lfa1            type table of ty_lfa1,
      it_t001            type table of ty_t001,
      it_estra           type table of ty_estra,
      tg_msg_ret         type table of zfiwrs0002 with header line.

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
        prog        like sy-repid value 'ZFIR0034',
        pressed_tab like sy-ucomm value c_tab_strip_imp-tab1,
      end of g_tab_strip_imp.

data: ok_code          like sy-ucomm,
      wg_mensagem(30),
      wg_acao(30),
      btn_rej(30)      value '@8Y@ Rejeitar',
      vdt_apuracao(1),
      vmes_apuracao(1),
      vkokrs           type tka02-kokrs,
      xclasse(1),
      xmodif(1),
      vdep_resp(2),
      vdata(10),
      vvalor_ate       type zadt_sol_aprov-valor_ate.




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
      g_cc_docs            type scrfname value 'CC_DOC',
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

    class-methods:
      on_hotspot for event hotspot_click of cl_gui_alv_grid
        importing e_row_id
                  e_column_id
                  es_row_no.


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
    data: wl_lotes like line of tg_lotes.

    data: v_msg     type char50,
          t_lotes   type table of zad_lotes_imp,
          w_lotes   type          zad_lotes_imp,
          t_estra   type table of zfi_estrategia_imp,
          w_estra   type          zfi_estrategia_imp,
          t_docs    type table of zad_docs_imp,
          w_docs    type          zad_docs_imp,
          vdata(10).

    if e_row gt 0.
      clear: wg_cadlote.
      refresh tg_estra.
      refresh tg_docs.
      read table tg_lotes into wl_lotes index e_row.
      if wl_lotes-status = icon_alert.
        message 'Pedido fora do prazo para aprovação' type 'I'.
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

      "READ TABLE IT_ZFIT0045 INTO WA_ZFIT0045 WITH KEY NRO_SOL = WL_LOTES-NRO_SOL BINARY SEARCH.
      wg_cadlote-empresa  = wl_lotes-empresa.
      wg_cadlote-nro_sol  = wl_lotes-nro_sol.
*      WG_CADLOTE-TOTAL    = WL_LOTES-TOTAL.
      write wl_lotes-total  to wg_cadlote-total.
      wg_cadlote-dep_resp = wl_lotes-dep_resp+0(2).
      "CONCATENATE WA_ZFIT0045-DT_PREV_LIQ+6(2) WA_ZFIT0045-DT_PREV_LIQ+4(2) WA_ZFIT0045-DT_PREV_LIQ+0(4) INTO VDATA SEPARATED BY '.'.


      call function 'SAPGUI_SET_FUNCTIONCODE'
        exporting
          functioncode           = '=ENT'
        exceptions
          function_not_supported = 1
          others                 = 2.

      refresh tg_estra.
      loop at it_estra into wa_estra where nro_sol = wl_lotes-nro_sol.
        append wa_estra to tg_estra.
      endloop.

      refresh tg_docs.
      loop at it_docs into wa_docs where nro_sol = wl_lotes-nro_sol.
        if wa_docs-name1 ne wa_docs-name1_po.
          wa_cellcolor-fname = 'NAME1'.
          wa_cellcolor-color-col = '6'.
          wa_cellcolor-color-int = '1'.
          append wa_cellcolor to wa_docs-cellcolor.
          wa_cellcolor-fname = 'NAME1_PO'.
          append wa_cellcolor to wa_docs-cellcolor.
        endif.
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
      if e_column = 'NRO_SOL'.

      endif.
    endif.
  endmethod.                    "ON_DOUBLE_CLICK2
  method on_click.
    data: wl_estra  like line of tg_estra,
          v_msg     type char50,
          t_lotes   type table of zad_lotes_imp,
          w_lotes   type          zad_lotes_imp,
          wl_lotes  like line of tg_lotes,
          t_estra   type table of zfi_estrategia_imp,
          w_estra   type          zfi_estrategia_imp,
          t_docs    type table of zad_docs_imp,
          w_docs    type          zad_docs_imp,
          vdata(10).

    if e_row_id gt 0.
      read table tg_estra into wl_estra index e_row_id.

      read table tg_lotes into wl_lotes with key nro_sol = wl_estra-nro_sol binary search.
      move-corresponding wl_lotes to w_lotes.
      append w_lotes  to t_lotes.

      loop at it_estra into wl_estra where nro_sol = w_lotes-nro_sol.
        move-corresponding wl_estra to w_estra.
        move wl_estra-nro_sol to w_estra-lote.
        append w_estra to t_estra.
      endloop.
      data: i_ok.
      call function 'Z_AD_ESTRATEGIA_EXECUTAR'
        exporting
          v_usuario = sy-uname
        importing
          msg       = v_msg
          ok        = i_ok
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
      perform atualiza_sol.


**===============================================================================================Incluido criação do workflow.
*      IF I_OK IS NOT INITIAL.
*        "Check solicitação.
*        SELECT SINGLE * FROM ZFIT0045 INTO @DATA(L_ZFIT0045) WHERE NRO_SOL EQ @WL_ESTRA-NRO_SOL.
*        IF L_ZFIT0045-ORIG_PGT EQ 'E'.
*
*          "Se origigem do pgamento for 'E', criar o Workflow no SE tipo SFC.
*          DATA: VMSG(50).
*          VMSG = 'Criando Workflow SFC para solicitação ->' && WL_ESTRA-NRO_SOL.
*          CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*            EXPORTING
*              TEXT = VMSG.
*
*          "Criando Workflow tipo SFC no SE.
*          ZCL_INT_SE=>CREATE_WORKFLOW_SOFTEXPERT_SFC(
*                EXPORTING
*              SOLICITACAO = L_ZFIT0045
*                IMPORTING
*                  E_RECORDID = DATA(E_RECORDID)
*                  E_MSG      = DATA(E_MSG)
*          ).
*
*          IF E_RECORDID IS NOT INITIAL.
*            MESSAGE 'Workflow ->' && E_RECORDID && ' criado com sucesso' TYPE 'S' DISPLAY LIKE 'I'.
*          ELSE.
*            MESSAGE 'Não foi possivél criar Workflow SFC para esta solicitação ' && WL_ESTRA-NRO_SOL TYPE 'E' DISPLAY LIKE 'I'.
*          ENDIF.
*        ENDIF.
*      ENDIF.
**===============================================================================================Incluido criação do workflow.


      call method grid2->refresh_table_display
        exporting
          is_stable = wa_stable.
    endif.
  endmethod.                    "ON_CLICK

  method on_data_changed.

  endmethod.                    "ON_DATA_CHANGED

  method on_hotspot.
    read table tg_lotes into data(wl_lotes2)  index e_row_id-index.
    if e_column_id =  'NRO_SOL'.
      call function 'Z_FUC_EXIBIR_ZFI0025'
        exporting
          i_nro_sol = wl_lotes2-nro_sol.
    endif.

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
    wa_layout-grid_title = 'Solicitações disponíveis para Liberação'.

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
              lcl_event_handler=>on_data_changed for grid1,
              lcl_event_handler=>on_hotspot for grid1.

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
    wa_layout-grid_title = 'Pedido da Solicitação'.
    wa_layout-no_toolbar = c_x.
    wa_layout-ctab_fname = 'CELLCOLOR'.

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
                  lcl_event_handler=>on_double_click2 for grid3,
                  lcl_event_handler=>on_hotspot for grid3.

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
        1 ' '                        ' '                'TG_LOTES' 'STATUS'           ' '              '02' ' ' ' ' ' ',
        2 ' '                        ' '                'TG_LOTES' 'EMPRESA'          'Empresa'        '25' ' ' ' ' ' ',
        3 'ZFIT0045'                 'NRO_SOL'          'TG_LOTES' 'NRO_SOL'          'Solicitação'    '10' ' ' ' ' ' ',
        4 ' '                        ' '                'TG_LOTES' 'DEP_RESP'         'Departamento'   '20' ' ' ' ' ' ',
        5 'ZFIT0045'                 'DT_PREV_LIQ'      'TG_LOTES' 'DT_VENC'          'Vencimento'     '12' ' ' ' ' ' ',
        6 'Zfit0046'                 'VLR_ADIANTAMENTO' 'TG_LOTES' 'TOTAL'            'Total'          '15' ' ' ' ' ' ',
        6 'Zfit0045'                 'LIMITE'           'TG_LOTES' 'LIMITE'           'Limite Cred.'   '15' ' ' ' ' ' ',
        6 'Zfit0045'                 'SALDO'            'TG_LOTES' 'SALDO'            'Sdo.Limi.Cred.' '15' ' ' ' ' ' '.

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

  if p_field eq 'OPCOES' .
    w_fieldcatalog-hotspot = c_x.
  endif.

  if p_field eq 'NRO_SOL'  and p_tabname = 'TG_DOCS'.
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


  case ok-code.
    when c_refresh.
      refresh: tg_estra, tg_docs.
      perform atualiza_sol.
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
        1 'ZADTO_APROVADOR'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         'Valor de'      '15' ' ' ' ' ' ',
        1 'ZADTO_APROVADOR'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        'Valor ate'     '15' ' ' ' ' ' ',
        1 'ZADTO_APROVADOR'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        'Aprovador'     '20' ' ' ' ' ' ',
        1 ' '                         ' '               'TG_ESTRA' 'ESTADO'           'Estado'        '10' ' ' ' ' ' ',
        1 ' '                         ' '               'TG_ESTRA' 'OPCOES'           'Opções Liber.' '12' ' ' ' ' ' '.

endform.                     " MONTAR_LAYOUT_ESTRA
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
        1 'ZFIT0045'           'NRO_SOL'          'TG_DOCS'  'NRO_SOL'          'Solicitação'       '10' ' ' ' ' ' ',
        1 'ZFIT0046'           'EBELN'            'TG_DOCS'  'EBELN'            'Pedido'            '12' ' ' ' ' ' ',
        2 'LFA1'               'NAME1'            'TG_DOCS'  'NAME1'            'Fornecedor'        '25' ' ' ' ' ' ',
        3 'LFA1'               'NAME1_PO'         'TG_DOCS'  'NAME1_PO'         'Fornecedor pedido' '25' ' ' ' ' ' ',
        4 'ZFIT0046'           'EBELP'            'TG_DOCS'  'EBELP'            'Item'              '05' ' ' ' ' ' ',
        5 'MAKT'               'MAKTX'            'TG_DOCS'  'MAKTX'            'Descr.Material'    '25' ' ' ' ' ' ',
        6 'ZFIT0046'           'SALDO_ITEM'       'TG_DOCS'  'SALDO_ITEM'       'Vlr. Total Item'   '15' ' ' ' ' ' ',
        7 'ZFIT0046'           'PGTOS_REAL'       'TG_DOCS'  'PGTOS_REAL'       'Pg. Realizados'    '15' ' ' ' ' ' ',
        8 'ZFIT0046'           'SDO_DISPONIVEL'   'TG_DOCS'  'SDO_DISPONIVEL'   'Saldo disponível'  '15' ' ' ' ' ' ',
        9 'ZFIT0046'           'VLR_ADIANTAMENTO' 'TG_DOCS'  'VLR_ADIANTAMENTO' 'Vlr. Adiantamento' '15' ' ' ' ' ' ',
        10 'ZFIT0045'           'MOEDA_PGTO'       'TG_DOCS'  'MOEDA_PGTO'       'Moeda Pgto.'       '10' ' ' ' ' ' ',
       11 'ZFIT0045'           'DT_PGTO'          'TG_DOCS'  'DT_PGTO'          'Dt.Pgto'           '10' ' ' ' ' ' ',
       12 'ZFIT0045'           'MOTIVO'           'TG_DOCS'  'MOTIVO'           'Motivo'            '20' ' ' ' ' ' ',
       13 'ZFIT0045'           'DT_PREV_LIQ'      'TG_DOCS'  'DT_PREV_LIQ'      'Dt.Prev.Liq.'      '10' ' ' ' ' ' ',
       14 ' '                  ' '                'TG_DOCS'  'SOLICITANTE'      'Solicitante'       '20' ' ' ' ' ' ',
       15 ' '                  ' '                'TG_DOCS'  'NEGOCIADOR'       'Negociador'        '20' ' ' ' ' ' '.

endform.                    " MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*&      Module  CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module carrega_lotes output.
  data xtotal type zfit0046-vlr_adiantamento.

  if g_custom_container is initial.
    perform atualiza_sol.
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
form atualiza_sol .


  data: v_msg     type char50,
        t_lotes   type table of zad_lotes_imp,
        w_lotes   type          zad_lotes_imp,
        t_estra   type table of zfi_estrategia_imp,
        w_estra   type          zfi_estrategia_imp,
        t_docs    type table of zad_docs_imp,
        w_docs    type          zad_docs_imp,
        vdata(10).

  data: vg_date_ref           type sy-datum,
        vg_date_ref_pgto      type sy-datum,
        vg_date_ref_pgto_util type sy-datum,
        vg_date_ref_util      type sy-datum.


  data: v_valor_numerico(11) type p decimals 2.
  data: v_valor_formatado type char30.


  call function 'Z_AD_ESTRATEGIA_LISTA'
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
    select single *
      from zfit0045
      into @data(w45)
      where nro_sol = @w_lotes-nro_sol.
    tg_lotes-moeda_pgto = w45-moeda_pgto.
    tg_lotes-limite     = w45-limite.
    tg_lotes-saldo      = w45-saldo.
    v_valor_numerico = w_lotes-total.
    write v_valor_numerico to v_valor_formatado currency 'BRL'.
    shift v_valor_formatado left deleting leading space.
    tg_lotes-total = v_valor_formatado.

** User Story 144133 / AOENNING.
*    clear: vg_date_ref, vg_date_ref_util, vg_date_ref_pgto.
*    vg_date_ref = w45-dt_atual + 5.
*    zcl_miro=>get_proximo_dia_util( exporting i_data_base = vg_date_ref
*                                              i_signum    = '+'
*                                    receiving r_data      = vg_date_ref_util ).
*
*    vg_date_ref_pgto = sy-datum + 3.
*    zcl_miro=>get_proximo_dia_util( exporting i_data_base = vg_date_ref_pgto
*                                              i_signum    = '+'
*                                    receiving r_data      = vg_date_ref_pgto_util ).
*
*
*    if vg_date_ref_pgto_util > vg_date_ref_util.
*      continue.
*    endif.
** User Story 144133 / AOENNING.


    append tg_lotes.
  endloop.
  sort tg_lotes by nro_sol .

  loop at t_estra into w_estra.
    move-corresponding w_estra to wa_estra.
    wa_estra-nro_sol = w_estra-lote.
    append wa_estra to it_estra.
  endloop.
  sort it_estra by nro_sol  .

  loop at t_docs into w_docs.
    move-corresponding w_docs to wa_docs.
    select single moeda_pgto
      from zfit0045
      into wa_docs-moeda_pgto
      where nro_sol = w_docs-nro_sol.

    append wa_docs to it_docs.
  endloop.
  sort it_docs by nro_sol  .

endform.                    " Atualiza_lotes

*&---------------------------------------------------------------------*
*&      Form  ENvia_email_Fim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form envia_email_fim.

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

*&--------- Comentado /  LES-ZMAIL - Remodelar usando padrão ZREGISTER_DATA #141955 / AOENNING&*
  vbukrs = wg_cadlote-empresa+0(4).
  data: it_mail type table of zmail with header line.
*  SELECT *
*    FROM zmail
*    INTO TABLE it_mail
*    WHERE tcode = 'ZFI0031'
*    AND   bukrs     LE vbukrs
*    AND   bukrs_ate GE vbukrs.
*  IF sy-subrc NE 0.
*    SELECT *
*    FROM zmail
*    INTO TABLE it_mail
*    WHERE tcode = 'ZFI0031'.
*  ENDIF.
*  vflag_mail = ''.
*  LOOP AT it_mail.
*    IF  it_mail-bukrs_ate IS INITIAL.
*      IF  it_mail-bukrs NE vbukrs.
*        CONTINUE.
*      ENDIF.
*    ELSEIF it_mail-bukrs     GT vbukrs OR
*           it_mail-bukrs_ate LT vbukrs.
*      CONTINUE.
*    ENDIF.
*    IF NOT it_mail-usuario IS INITIAL.
*      vflag_mail = 'X'.
*      EXIT.
*    ENDIF.
*  ENDLOOP.
*&--------- Comentado /  LES-ZMAIL - Remodelar usando padrão ZREGISTER_DATA #141955 / AOENNING&*
  if it_mail-usuario is initial or vflag_mail  is initial.
    message 'Usuário padrão não cadastrado.' type 'E'.
    exit.
  elseif it_mail[] is initial.
    message 'E-Mail não cadastrado para esta transação.' type 'E'.
    exit.
  endif.


  read table it_zfit0045 into wa_zfit0045 with key nro_sol = wa_estra-nro_sol binary search.
  concatenate wa_zfit0045-dt_prev_liq+6(2) wa_zfit0045-dt_prev_liq+4(2) wa_zfit0045-dt_prev_liq+0(4) into vdata separated by '.'.
* Criação do documento de Email
  doc_chng-obj_name = 'LOG_REL'.

* Assunto do Email
  concatenate 'Solicitação de adiantamento Venc .' vdata wg_cadlote-empresa into doc_chng-obj_descr separated by space .

* Texto
  objtxt-line = 'A Solicitação abaixo foi aprovada e encontra-se disponível para pagamento.'.
  append objtxt.
  clear objtxt.
  append objtxt.

  objtxt-line = 'Clique no link anexo "GUIAS", para a relação individual das guias para pagamento'.
  append objtxt.
  clear objtxt.

  objtxt-line = '--------------------------------------------------------------------------------------------------------------' .
  append objtxt.
  clear objtxt.

  write wg_cadlote-total to ctotal." CURRENCY 'USD'.

  condense ctotal no-gaps.
  concatenate 'Empresa:'  wg_cadlote-empresa ' Solicitação:' wg_cadlote-nro_sol ' R$' ctotal ' Venc.'  vdata into objtxt separated by space.
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

  it_shortcut_param-fieldname = 'P_BUKRS-LOW'.
  it_shortcut_param-fieldvalue = wg_cadlote-empresa+0(4).
  append it_shortcut_param.

  it_shortcut_param-fieldname = 'P_NRSOL-LOW'.
  it_shortcut_param-fieldvalue = wg_cadlote-nro_sol.
  append it_shortcut_param.

  call function 'ZFM_CREATE_SHORTCUT'
    exporting
      recipient_user_id = it_mail-usuario
      transaction       = 'ZFI0026'
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
form envia_email  tables tg_estra  using value(wg_cadlote) type ty_cadlote plinha  .

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
  doc_chng-obj_descr = 'Aprovação Solicitação Adiantamento - Vencimento'.

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP, a solicitação de adiantamento abaixo.'.
  append objtxt.
  clear objtxt.
  append objtxt.

  objtxt-line = 'Para aprovar clique no link "Estratégia" em anexo.' .
  append objtxt.
  clear objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  append objtxt.
  clear objtxt.

  data: ctotal(30).

*  READ TABLE IT_ZFIT0045 INTO WA_ZFIT0045 WITH KEY NRO_SOL = WA_ESTRA-NRO_SOL BINARY SEARCH.
*  CONCATENATE WA_ZFIT0045-DT_PREV_LIQ+6(2) WA_ZFIT0045-DT_PREV_LIQ+4(2) WA_ZFIT0045-DT_PREV_LIQ+0(4) INTO VDATA SEPARATED BY '.'.

  vdata = wg_cadlote-data.
  write wg_cadlote-total to ctotal. " CURRENCY 'USD'.

  condense ctotal no-gaps.
  concatenate 'Empresa:'  wg_cadlote-empresa ' Solicitação:' wg_cadlote-nro_sol ' R$' ctotal ' Venc.'  vdata into objtxt separated by space.
  append objtxt.
  clear objtxt.
  select *
       from zfit0045
       into table it_zfit0045
       where nro_sol = wg_cadlote-nro_sol.

  select *
  from zfit0046
  into table it_zfit0046
  for all entries in it_zfit0045
  where nro_sol eq it_zfit0045-nro_sol.

  select lifnr name1
    from lfa1
    into table it_lfa1
    for all entries in it_zfit0045
    where lifnr = it_zfit0045-lifnr.

  select   bname name_text
    from v_usr_name
    into table it_user
    for all entries in it_zfit0045
    where bname = it_zfit0045-usnam.

  select   bname name_text
    from v_usr_name
    appending table it_user
    for all entries in it_zfit0045
    where bname = it_zfit0045-resp_neg.

  loop at it_zfit0046 into wa_zfit0046.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = wa_zfit0046-matnr
      importing
        output = wa_zfit0046-matnr.
    modify it_zfit0046 from wa_zfit0046 index sy-tabix transporting matnr.
  endloop.

  select matnr maktx
  from makt
  into table it_makt
  for all entries in it_zfit0046
  where matnr = it_zfit0046-matnr
  and   spras = 'P'.

  sort:
        it_zfit0045         by nro_sol,
        it_zfit0046         by nro_sol,
        it_user             by bname,
        it_makt             by matnr,
        it_lfa1             by lifnr.

  loop at it_zfit0046 into wa_zfit0046 where nro_sol = wg_cadlote-nro_sol.
    move-corresponding wa_zfit0046 to wa_docs.
    read table it_zfit0045 into wa_zfit0045 with key nro_sol = wa_zfit0046-nro_sol binary search.
    wa_docs-dt_pgto     = wa_zfit0045-dt_pgto.
    wa_docs-motivo      = wa_zfit0045-motivo.
    wa_docs-dt_prev_liq = wa_zfit0045-dt_prev_liq.

    read table it_lfa1 into wa_lfa1 with key lifnr = wa_zfit0045-lifnr binary search.
    concatenate wa_lfa1-lifnr '-' wa_lfa1-name1 into wa_docs-name1.

    read table it_user into wa_user with key bname = wa_zfit0045-usnam binary search.
    wa_docs-solicitante = wa_user-name_text.
    read table it_user into wa_user with key bname = wa_zfit0045-resp_neg binary search.
    wa_docs-negociador = wa_user-name_text.
    read table it_makt into wa_makt with key matnr = wa_zfit0046-matnr binary search.
    wa_docs-maktx = wa_makt-maktx.
  endloop.
*  FORNECEDOR :
  concatenate 'FORNECEDOR :'  wa_docs-name1  into objtxt-line separated by space.
  append objtxt.
  clear objtxt.
*  MOTIVO :
  concatenate 'MOTIVO :'   wa_docs-motivo  into objtxt-line separated by space.
  append objtxt.
  clear objtxt.
*  Solicitante :
  concatenate 'Solicitante :' wa_docs-solicitante    into objtxt-line separated by space.
  append objtxt.
  clear objtxt.
*  Negociador :
  concatenate 'Negociador :' wa_docs-negociador    into objtxt-line separated by space.
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
      transaction       = 'ZFI0031'
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

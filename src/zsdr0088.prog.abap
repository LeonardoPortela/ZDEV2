*&---------------------------------------------------------------------*
*& Report  ZSDR0088
*&
*&---------------------------------------------------------------------*
*&Autor: Jean Antunes
*&Aprovação de Solicitação de criação de OV criada na ZSDT0062
*&Transação: ZSDT0138
*&---------------------------------------------------------------------*
REPORT zsdr0088.

TYPES:

  BEGIN OF ty_fields,
    campo(30) TYPE c,
    group1(5) TYPE c,
    value     TYPE sy-tabix,
    invisible TYPE sy-tabix,
  END   OF ty_fields,

  BEGIN OF ty_editor,
    line(72),
  END   OF ty_editor,

  BEGIN OF ty_sol_ov,
    empresa(30)    TYPE c,
    nro_sol_ov(20) TYPE c,
    usuario(20)    TYPE c,
    moeda(3)       TYPE c,
    total          TYPE zsdt0053-dmbtr,
    data(10),
  END OF ty_sol_ov,

  BEGIN OF ty_estra ,
    bukrs      TYPE zsdt0162-bukrs,
    nro_sol_ov TYPE zsdt0162-vbeln,
    valor_de   TYPE zsdt0161-valor_de,
    valor_ate  TYPE zsdt0161-valor_ate,
    aprovador  TYPE zsdt0161-aprovador,
    nivel      TYPE zsdt0161-nivel,
    waers(3),
    estado(4),
    opcoes(4),
  END OF ty_estra,

  BEGIN OF ty_docs ,
    nro_sol_ov       TYPE zsdt0053-nro_sol_ov,
    item             TYPE zsdt0053-posnr,
    material         TYPE zsdt0053-matnr,
    txt_material(30) TYPE c,
    centro           TYPE zsdt0053-werks,
    ponto_coleta     TYPE zsdt0053-ponto_c,
    terminal         TYPE zsdt0053-terminal,
    deposito         TYPE zsdt0053-lgort,
    lote             TYPE zsdt0053-charg,
    qtd_prevista     TYPE zsdt0053-zmeng,
    um               TYPE zsdt0053-zieme,
    preco            TYPE zsdt0053-dmbtr,
    um_preco         TYPE zsdt0053-pmein,
    valor_total      TYPE zsdt0053-vlrtot,
    data_venc        TYPE zsdt0053-valdt,
  END OF ty_docs,

  BEGIN OF ty_ordens,
    nro_sol_ov       TYPE zsdt0051-nro_sol_ov,
    tp_venda         TYPE zsdt0051-tp_venda,
    org_vendas       TYPE zsdt0051-vkorg,
    esc_vendas       TYPE zsdt0051-vkbur,
    moeda            TYPE zsdt0051-waerk,
    cliente          TYPE zsdt0051-kunnr,
    cliente_name     TYPE kna1-name1,
    cond_pgto        TYPE zsdt0052-zterm,
    valor            TYPE zsdt0053-vlrtot,
    icon_obs(4)      TYPE c,
    observacoes(255) TYPE c,
    usuario          TYPE zsdt0051-usnam,
    cellstyles       TYPE lvc_t_styl.
TYPES:END OF ty_ordens,

BEGIN OF ty_makt,
  matnr TYPE makt-matnr,
  maktx TYPE makt-maktx,
END OF ty_makt.


DATA: ok-code         TYPE sy-ucomm,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      btn_rej(30).

DATA  dyfields LIKE dynpread OCCURS 1 WITH HEADER LINE.

** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl,
      wg_editor      TYPE ty_editor,
      tg_fields      TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_msg_ret     TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,

** Tabelas e WorkAreas OV
      tg_zsdt0161    TYPE TABLE OF zsdt0161,
      tg_zsdt0162    TYPE TABLE OF zsdt0162,
      tg_editor      TYPE TABLE OF ty_editor,
      tg_ordens      TYPE TABLE OF ty_ordens WITH HEADER LINE,
      tg_estra       TYPE TABLE OF ty_estra,
      wg_estra       LIKE LINE OF  tg_estra, "RJF
      tg_docs        TYPE TABLE OF ty_docs,
      it_estra       TYPE TABLE OF ty_estra,
      it_docs        TYPE TABLE OF ty_docs,

      wa_cadordem    TYPE ty_sol_ov,
      wg_cadordem    TYPE ty_sol_ov,
      wa_estra       TYPE ty_estra,
      wa_docs        TYPE ty_docs,
      w_docs         TYPE ty_docs,
      wg_zsdt0161    LIKE LINE OF tg_zsdt0161,
      wg_zsdt0162    LIKE LINE OF tg_zsdt0162.

FIELD-SYMBOLS: <wl_ordens> TYPE ty_ordens.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC3',
           END OF c_tab_strip_imp.

*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  tab_strip_imp TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip_imp,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZSDR0088',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

** Variáveis
DATA: ok_code          LIKE sy-ucomm,
      vkokrs           TYPE tka02-kokrs,
      vvalor_ate       TYPE zlest0157-valor_ate,
      wg_mensagem(30),
      wg_acao(30),
      vdt_apuracao(1),
      vmes_apuracao(1),
      xclasse(1),
      xmodif(1),
      txtemp(10),
      txtord(15),
      txtusu(15),
      txtval(15).

CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.


*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_ORDENS',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      grid3                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      g_custom_cont_desc   TYPE REF TO cl_gui_custom_container,
      obg_descbox          TYPE REF TO cl_gui_textedit,
      obg_docking          TYPE REF TO cl_gui_docking_container,

      obg_conteiner_estra  TYPE REF TO cl_gui_custom_container,
      obg_conteiner_docs   TYPE REF TO cl_gui_custom_container,
      g_cc_estra           TYPE scrfname VALUE 'CC_ESTRA',
      g_cc_docs            TYPE scrfname VALUE 'CC_DOCS',
      wa_style             TYPE lvc_s_styl,
      style                TYPE lvc_t_styl  WITH HEADER LINE,
      style2               TYPE lvc_t_styl WITH HEADER LINE.

* alrs
*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.
*** TREE DE MENSAGENS.
DATA node_itab LIKE node_str OCCURS 0.
DATA node LIKE node_str.

DATA container TYPE REF TO cl_gui_custom_container.
DATA splitter_msg TYPE REF TO cl_gui_easy_splitter_container.
DATA right TYPE REF TO cl_gui_container.
DATA left  TYPE REF TO cl_gui_container.

DATA editor TYPE REF TO cl_gui_textedit.
DATA tree TYPE REF TO cl_gui_simple_tree.

DATA behaviour_left TYPE REF TO cl_dragdrop.
DATA behaviour_right TYPE REF TO cl_dragdrop.

DATA handle_tree TYPE i.
DATA num_row TYPE i VALUE 0.

DATA: gt_bdc TYPE TABLE OF bdcdata,
      gw_bdc TYPE bdcdata.

*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_0               TYPE c VALUE '0',
           c_1               TYPE c VALUE '1',
           c_2               TYPE c VALUE '2',
           c_b               TYPE c VALUE 'B',
           c_s               TYPE c VALUE 'S',
           c_l               TYPE c VALUE 'L',
           c_x               TYPE c VALUE 'X',
           c_d               TYPE c VALUE 'D',
           c_k               TYPE c VALUE 'K',
           c_w               TYPE c VALUE 'W',
           c_f               TYPE c VALUE 'F',
           c_t               TYPE c VALUE 'T',
           c_i               TYPE c VALUE 'I',
           c_n               TYPE c VALUE 'N',
           c_h               TYPE c VALUE 'H',
           c_ag(2)           TYPE c VALUE 'AG',
           c_ne(2)           TYPE c VALUE 'NE',
           c_01(2)           TYPE c VALUE '01',
           c_30(2)           TYPE c VALUE '30',
           c_40(2)           TYPE c VALUE '40',
           c_50(4)           TYPE c VALUE '0050',
           c_76(2)           TYPE c VALUE '76',
           c_71(2)           TYPE c VALUE '71',
           c_72(2)           TYPE c VALUE '72',
           c_br(2)           TYPE c VALUE 'BR',
           c_lf(2)           TYPE c VALUE 'LF',
           c_lr(2)           TYPE c VALUE 'LR',
           c_z1(2)           TYPE c VALUE 'Z1',
           c_add(3)          TYPE c VALUE 'ADD',
           c_del(3)          TYPE c VALUE 'DEL',
           c_dg1(3)          TYPE c VALUE 'DG1',
           c_dg2(3)          TYPE c VALUE 'DG2',
           c_dummy_header(3) TYPE c VALUE '099',
           c_dummy_itens(3)  TYPE c VALUE '098',
           c_exit(4)         TYPE c VALUE 'EXIT',
           c_root(4)         TYPE c VALUE 'ROOT',
           c_minimizar(4)    TYPE c VALUE '@K2@',
           c_maximizar(4)    TYPE c VALUE '@K1@',
           c_back(4)         TYPE c VALUE 'BACK',
           c_save(4)         TYPE c VALUE 'SAVE',
           c_desat(5)        TYPE c VALUE 'DESAT',
           c_dmbtr(5)        TYPE c VALUE 'DMBTR',
           c_refresh(7)      TYPE c VALUE 'REFRESH',
           c_modif(5)        TYPE c VALUE 'MODIF',
           c_cancel(6)       TYPE c VALUE 'CANCEL',
           c_deldoc(6)       TYPE c VALUE 'DELDOC',
           c_displa(6)       TYPE c VALUE 'DISPLA',
           c_dclick(6)       TYPE c VALUE 'DCLICK',
           c_search(6)       TYPE c VALUE 'SEARCH',
           c_atuali(6)       TYPE c VALUE 'ATUALI',
           c_add_msg(7)      TYPE c VALUE 'ADD_MSG',
           c_del_msg(7)      TYPE c VALUE 'DEL_MSG',
           c_clos_msg(8)     TYPE c VALUE 'CLOS_MSG',
           c_save_msg(8)     TYPE c VALUE 'SAVE_MSG',
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE'.

*ALRS
*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_click2 FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

    CLASS-METHODS:
      on_click3 FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_treeobject DEFINITION
*---------------------------------------------------------------------*
*       Definition of Data Container                                  *
*---------------------------------------------------------------------*
CLASS lcl_drag_object DEFINITION.
  PUBLIC SECTION.
    DATA text TYPE mtreesnode-text.
ENDCLASS.                    "lcl_drag_object DEFINITION
*---------------------------------------------------------------------*
*       CLASS dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      node_double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.                    "lcl_dragdrop_receiver DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*Event for toolbar
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.
ENDCLASS.                    "lcl_alv_toolbar DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "constructor

  METHOD on_toolbar.
    DATA: wl_desactive.

    IF wg_acao NE c_modif.
      wl_desactive = 1.
    ENDIF.

    ty_toolbar-icon      =  icon_insert_row.
    ty_toolbar-function  =  c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.


    ty_toolbar-icon      =  icon_delete_row.
    ty_toolbar-function  =  c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*   variable for Toolbar Button
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_event_handler IMPLEMENTATION.

* Método de  execução para Duplo-click
  METHOD on_double_click.
    DATA: wl_ordens   LIKE LINE OF tg_ordens,
          vflg_ico(1).

    vvalor_ate = 0.

    IF e_row GT 0.

      CLEAR: wg_cadordem.
      REFRESH: tg_estra, tg_docs.

      READ TABLE tg_ordens INTO wl_ordens INDEX e_row.

*        MESSAGE TEXT-I01 TYPE 'I'.
*
*        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*          EXPORTING
*            FUNCTIONCODE           = '=ENT'
*          EXCEPTIONS
*            FUNCTION_NOT_SUPPORTED = 1
*            OTHERS                 = 2.
*
*        EXIT.
*
*        CALL METHOD GRID2->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = WA_STABLE.
*        CALL METHOD GRID3->REFRESH_TABLE_DISPLAY
*          EXPORTING
*            IS_STABLE = WA_STABLE.

      wg_cadordem-empresa     = wl_ordens-org_vendas.
      wg_cadordem-nro_sol_ov  = wl_ordens-nro_sol_ov.
      wg_cadordem-usuario     = wl_ordens-usuario.
      wg_cadordem-total       = wl_ordens-valor.




      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '=ENT'
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.

      REFRESH tg_estra.
      LOOP AT it_estra INTO wa_estra WHERE nro_sol_ov = wl_ordens-nro_sol_ov.



        APPEND wa_estra TO tg_estra.
      ENDLOOP.
      REFRESH tg_docs.

      LOOP AT it_docs INTO wa_docs WHERE nro_sol_ov = wl_ordens-nro_sol_ov.
        APPEND wa_docs TO tg_docs.
      ENDLOOP.

    ENDIF.

    SORT tg_estra BY nivel.

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_click2.
    DATA: wl_docs LIKE LINE OF tg_docs,
          vl_sol  TYPE zlest0155-vbeln.

    IF e_row_id GT 0.
      IF e_column_id = 'NRO_SOL_OV'.
        CLEAR vl_sol.
        READ TABLE tg_docs INTO wl_docs INDEX e_row_id.
        vl_sol = wl_docs-nro_sol_ov.

        SUBMIT zsdr0022 WITH psolici EQ vl_sol.
*        SET PARAMETER ID 'AUN' FIELD  VG_ORDEM.
*        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK2

  METHOD on_click.

    DATA: v_msg     TYPE           char50,

          w_ordens  TYPE           zsds018,
          w_estra   TYPE           zsds019,
          w_docs    TYPE           zsds020,

          t_ordens  TYPE TABLE OF  zsds018,
          t_estra   TYPE TABLE OF  zsds019,
          t_docs    TYPE TABLE OF  zsds020,

          wl_ordens LIKE LINE OF   tg_ordens,
          wl_estra  LIKE LINE OF   tg_estra,
          wl_docs   LIKE LINE OF   tg_docs.


    IF e_row_id GT 0.

      READ TABLE tg_estra INTO wl_estra INDEX e_row_id.
* RJF - INI - CS2022000728 - Envio das confirmaões de vendas junto no e-mail de Liberaçaõ da Venda #83043
      IF sy-subrc IS INITIAL.
        MOVE wl_estra TO wg_estra.
      ELSE.
        FREE wg_estra.
      ENDIF.
* RJF - FIM - CS2022000728 - Envio das confirmaões de vendas junto no e-mail de Liberaçaõ da Venda #83043

      READ TABLE tg_ordens INTO wl_ordens WITH KEY nro_sol_ov = wl_estra-nro_sol_ov BINARY SEARCH.

      MOVE-CORRESPONDING wl_ordens TO w_ordens.

      APPEND w_ordens  TO t_ordens.

      DELETE it_estra WHERE nro_sol_ov = wl_estra-nro_sol_ov.

      APPEND wl_estra TO it_estra.

      LOOP AT it_estra INTO wl_estra WHERE nro_sol_ov = wl_ordens-nro_sol_ov.
        MOVE-CORRESPONDING wl_estra TO w_estra.
        APPEND w_estra TO t_estra.
      ENDLOOP.

      CALL FUNCTION 'Z_SOL_OV_ESTRAT_EXECUTA'
        EXPORTING
          v_usuario      = sy-uname  " Campo do sistema ABAP: nome do usuário atual
        IMPORTING
          msg            = v_msg     " Comentário
        TABLES
          t_solicitacoes = t_ordens  " Solicitações de OV disponiveis para Liberação
          t_estra        = t_estra.  " Estratégia de liberação - Solicitações OV

* RJF - INI - CS2022000728 - Envio das confirmaões de vendas junto no e-mail de Liberaçaõ da Venda #83043
      IF sy-subrc IS INITIAL.
        PERFORM f_gera_envia.
      ENDIF.
* RJF - FIM - CS2022000728 - Envio das confirmaões de vendas junto no e-mail de Liberaçaõ da Venda #83043

      LOOP AT t_estra INTO w_estra
        WHERE aprovador EQ sy-uname.
        MOVE: w_estra-opcoes TO wl_estra-opcoes,
              w_estra-estado TO wl_estra-estado.
        MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
      ENDLOOP.

      CALL METHOD grid2->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      MESSAGE s836(sd) DISPLAY LIKE 'S' WITH v_msg.
      PERFORM f_refresh.

    ENDIF.
  ENDMETHOD.

  METHOD on_click3.

    DATA opt TYPE ctu_params.

    FREE gt_bdc.

    CASE e_column_id.
      WHEN: 'NRO_SOL_OV'.
        READ TABLE tg_ordens INTO tg_ordens INDEX e_row_id.

        PERFORM f_preencher_dynpro USING:
                      'X' 'ZSDR0022'                      '0050',
                      ' ' 'WG_HEADER-NRO_SOL_OV'          tg_ordens-nro_sol_ov,
                      ' ' 'BDC_OKCODE'                    'ATUAL'.

        opt-dismode = 'E'.
        opt-defsize = ' '.

        CALL TRANSACTION 'ZSDT0062' USING gt_bdc OPTIONS FROM opt.

    ENDCASE.

  ENDMETHOD.


  METHOD handle_button_click.
    DATA: anexo_obj     TYPE REF TO cl_gos_manager,
          vl_ip_service TYPE sgs_srvnam,
          wa_bor        TYPE borident,
          vl_obj_key    TYPE sibflporb-instid,
          tl_anexos     TYPE TABLE OF bdn_con,
          ip_mode       TYPE sgs_rwmod,
          tl_texto      TYPE catsxt_longtext_itab,
          wl_texto      LIKE LINE OF tl_texto.

    DATA(vl_ano) = sy-datum+0(4).

    READ TABLE tg_ordens[] INTO DATA(wl_ordens) INDEX es_row_no-row_id.

    CASE es_col_id.
      WHEN 'ICON_OBS'.
        CLEAR: wl_texto, tl_texto[].

        wl_texto = wl_ordens-observacoes+0(72).
        APPEND wl_texto TO tl_texto[].
        wl_texto = wl_ordens-observacoes+72(72).
        APPEND wl_texto TO tl_texto[].
        wl_texto = wl_ordens-observacoes+144(72).
        APPEND wl_texto TO tl_texto[].

        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
          EXPORTING
            im_title        = 'Observação:'
            im_display_mode = 'X'
          CHANGING
            ch_text         = tl_texto.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.

  ENDMETHOD.
ENDCLASS.



MODULE trata_fields OUTPUT.

  LOOP AT tg_fields.
    LOOP AT SCREEN.
      IF screen-name EQ tg_fields-campo
      OR screen-group1 EQ tg_fields-group1.
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        MODIFY SCREEN.
*        EXIT.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDMODULE.


MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.
  APPEND c_save TO fcode.
  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '0100'.

ENDMODULE.


MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.
  DATA: waref      TYPE REF TO data.

  DATA: wa_event_0001    TYPE REF TO lcl_event_handler.

  IF g_custom_container IS INITIAL.
    txtemp = TEXT-l01.
    txtord = TEXT-l02.
    txtusu = TEXT-l03.
    txtval = TEXT-l04.
    btn_rej = TEXT-b01.


    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_stable-row        = c_x.
    wa_layout-info_fname = 'COLOR'.
    wa_layout-no_toolbar = c_x.
    wa_layout-stylefname = 'CELLSTYLES'.
    wa_layout-grid_title = TEXT-t01 .

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.


    PERFORM montar_layout.

    REFRESH tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    IF wa_event_0001 IS INITIAL.
      CREATE OBJECT wa_event_0001.
      SET HANDLER: wa_event_0001->handle_button_click FOR grid1,
                   wa_event_0001->on_click3 FOR grid1.

    ENDIF.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_ordens[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_double_click FOR grid1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID2
  IF obg_conteiner_estra IS INITIAL.
    CREATE OBJECT obg_conteiner_estra
      EXPORTING
        container_name = g_cc_estra.


    CREATE OBJECT grid2
      EXPORTING
        i_parent = obg_conteiner_estra.


    PERFORM montar_layout_estra.

    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.


    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = TEXT-t02 .
    wa_layout-no_toolbar = c_x.
    PERFORM montar_layout_estra.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_estra[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
     lcl_event_handler=>on_click FOR grid2.
  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  "GRID3
  IF obg_conteiner_docs IS INITIAL.
    CREATE OBJECT obg_conteiner_docs
      EXPORTING
        container_name = g_cc_docs.

    CREATE OBJECT grid3
      EXPORTING
        i_parent = obg_conteiner_docs.

    PERFORM montar_layout_docs.

    REFRESH: tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function.

    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = TEXT-t03 .
    wa_layout-no_toolbar = c_x.
    PERFORM montar_layout_docs.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_docs[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
                  lcl_event_handler=>on_click2 FOR grid3.

  ELSE.
    PERFORM montar_layout_docs.

    CALL METHOD grid3->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT


FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZSDT0051'  'NRO_SOL_OV'  'TG_ORDENS' 'NRO_SOL_OV'    TEXT-a00        '10' 'C' ' ' ' ' ' ' 'X',
        1 'ZSDT0051'  'TP_VENDA'    'TG_ORDENS' 'TP_VENDA'      TEXT-a34        '08' 'C' ' ' ' ' ' ' ' ',
        2 'ZSDT0051'  'VKORG'       'TG_ORDENS' 'ORG_VENDAS'    TEXT-a01        '05' 'C' ' ' ' ' ' ' ' ',
        3 'ZSDT0051'  'VKBUR'       'TG_ORDENS' 'ESC_VENDAS'    TEXT-a02        '05' 'C' ' ' ' ' ' ' ' ',
        4 'ZSDT0051'  'WAERK'       'TG_ORDENS' 'MOEDA'         TEXT-a03        '05' 'C' ' ' ' ' ' ' ' ',
        5 ''          ''            'TG_ORDENS' 'CLIENTE_NAME'  TEXT-a04        '20' 'C' ' ' ' ' ' ' ' ',
        6 'ZSDT0052'  'ZTERM'       'TG_ORDENS' 'COND_PGTO'     TEXT-a05        '08' 'C' ' ' ' ' ' ' ' ',
        7 'ZSDT0053'  'VLRTOT'      'TG_ORDENS' 'VALOR'         TEXT-a06        '10' 'C' ' ' ' ' ' ' ' ',
        8 ''          ''            'TG_ORDENS' 'ICON_OBS'      TEXT-a07        '15' 'C' ' ' ' ' ' ' ' ',
        9 'ZSDT0051'  'USNAM'       'TG_ORDENS' 'USUARIO'       TEXT-a08        '15' 'C' ' ' ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT


FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            p_scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_just)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize)
                            VALUE(p_hot).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-just          = p_just.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.
  w_fieldcatalog-hotspot       = p_hot.

  IF p_field EQ 'OPCOES' OR p_field EQ 'OV'.
    w_fieldcatalog-hotspot = c_x.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura

MODULE user_command_0100 INPUT.
  DATA:   vflg_ico(1).

  CASE ok-code.
    WHEN c_refresh.
      PERFORM f_refresh.
    WHEN 'REJ'.
      READ TABLE tg_estra INTO wa_estra WITH KEY  aprovador = sy-uname.
      IF sy-subrc = 0.
        btn_rej = TEXT-b01.
        IF  wa_estra-opcoes = icon_reject.
          wa_estra-opcoes = icon_set_state.
        ELSEIF  wa_estra-opcoes = icon_set_state.
          wa_estra-opcoes = icon_reject.
        ENDIF.
        MODIFY tg_estra FROM wa_estra INDEX sy-tabix TRANSPORTING opcoes.
      ENDIF.
  ENDCASE.

  CASE sy-ucomm.
    WHEN c_refresh.
      PERFORM f_refresh.
    WHEN 'REJ'.
      READ TABLE tg_estra INTO wa_estra WITH KEY  aprovador = sy-uname.
      IF sy-subrc = 0.
        btn_rej = TEXT-b01.
        IF  wa_estra-opcoes = icon_reject.
          wa_estra-opcoes = icon_set_state.
        ELSEIF  wa_estra-opcoes = icon_set_state.
          wa_estra-opcoes = icon_reject.
        ENDIF.
        MODIFY tg_estra FROM wa_estra INDEX sy-tabix TRANSPORTING opcoes.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT


MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN c_cancel.
      SET SCREEN 0.

    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.

  CASE sy-ucomm.
    WHEN 'CANCEL'.
      SET SCREEN 0.
    WHEN 'BACK' OR 'EXIT'.
      LEAVE PROGRAM.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_EXIT  INPUT


FORM f_estra USING p_bukrs p_ordem p_dep_resp p_total.

  REFRESH: tg_estra, tg_zsdt0162.

  SELECT  bukrs
          vbeln
          nivel
          aprovador
          valor_de
          valor_ate
          data_atual
          hora_atual
          usuario
    FROM zsdt0162
    INTO CORRESPONDING FIELDS OF TABLE tg_zsdt0162
    WHERE  vbeln  =  p_ordem.

  SORT tg_zsdt0162 BY nivel aprovador.

  SELECT  bukrs bukrs_ate nivel aprovador valor_de valor_ate
    FROM zlest0156
    INTO CORRESPONDING FIELDS OF TABLE tg_zsdt0161
    WHERE bukrs     LE p_bukrs
    AND   bukrs_ate GE p_bukrs.

  SORT tg_zsdt0161 BY bukrs bukrs_ate nivel.
  vflg_ico = 'N'.

  vvalor_ate = 0.

  LOOP AT tg_zsdt0161 INTO wg_zsdt0161.
    IF  wg_zsdt0161-bukrs_ate IS INITIAL.
      IF  wg_zsdt0161-bukrs NE p_bukrs.
        CONTINUE.
      ENDIF.
    ELSEIF wg_zsdt0161-bukrs     GT p_bukrs OR
           wg_zsdt0161-bukrs_ate LT p_bukrs.
      CONTINUE.
    ENDIF.

  ENDLOOP.


  LOOP AT tg_zsdt0161 INTO wg_zsdt0161.
    IF  wg_zsdt0161-bukrs_ate IS INITIAL.
      IF  wg_zsdt0161-bukrs NE p_bukrs.
        CONTINUE.
      ENDIF.
    ELSEIF wg_zsdt0161-bukrs     GT p_bukrs OR
           wg_zsdt0161-bukrs_ate LT p_bukrs.
      CONTINUE.
    ENDIF.
    IF wg_zsdt0161-valor_ate <= vvalor_ate.

      wa_estra-bukrs        = p_bukrs.
      wa_estra-nro_sol_ov   = p_ordem.
      wa_estra-valor_de     = wg_zsdt0161-valor_de.
      wa_estra-valor_ate    = wg_zsdt0161-valor_ate.
      wa_estra-aprovador    = wg_zsdt0161-aprovador.
      wa_estra-nivel        = wg_zsdt0161-nivel.

      READ TABLE tg_zsdt0162 INTO wg_zsdt0162 WITH KEY nivel   = wg_zsdt0161-nivel
                                                     aprovador = wg_zsdt0161-aprovador BINARY SEARCH.
      IF sy-subrc = 0.
        wa_estra-estado       = icon_checked .
        wa_estra-opcoes       = icon_system_undo.
        vflg_ico = 'N'.
      ELSEIF vflg_ico = 'S'.
        wa_estra-estado       = icon_led_yellow .
        wa_estra-opcoes       = '' .
      ELSE.
        IF sy-uname NE wg_zsdt0161-aprovador.
          wa_estra-estado       =  ' '.
          wa_estra-opcoes       = icon_led_yellow  .
        ELSE.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = icon_set_state  .
        ENDIF.
        vflg_ico = 'X'.
      ENDIF.

      APPEND wa_estra TO tg_estra.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "F_ESTRA


FORM montar_layout_estra .

  txtemp = 'Empresa'.
  txtord = 'Sol.OV'.
  txtusu = 'Usuário'.
  txtval = 'Valor'.

  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZSDT0162'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         TEXT-a28      '15' 'C'  ' ' ' ' ' ' ' ',
        2 'ZSDT0162'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        TEXT-a29      '15' 'C'  ' ' ' ' ' ' ' ',
        3 'ZSDT0162'           'WAERS'           'TG_ESTRA' 'WAERS'            TEXT-a30      '05' 'C'  ' ' ' ' ' ' ' ',
        4 'ZSDT0162'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        TEXT-a31      '13' 'C'  ' ' ' ' ' ' ' ',
        5 ' '                       ' '          'TG_ESTRA' 'ESTADO'           TEXT-a32      '05' 'C'  ' ' ' ' ' ' ' ',
        6 ' '                       ' '          'TG_ESTRA' 'OPCOES'           TEXT-a33      '08' 'C'  ' ' ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_ESTRA


FORM montar_layout_docs .
  REFRESH t_fieldcatalog.
  CLEAR w_docs.
  IF tg_docs[] IS NOT INITIAL.
    READ TABLE tg_docs INTO w_docs INDEX 1.
  ENDIF.
  PERFORM montar_estrutura USING:
        1  'ZSDT0053'  'NRO_SOL_OV' 'TG_DOCS' 'NRO_SOL_OV'      TEXT-a13   '12' 'C' ' ' ' ' ' ' ' ',
        2  'ZSDT0053'  'POSNR'      'TG_DOCS' 'ITEM'            TEXT-a14   '06' 'C' ' ' ' ' ' ' ' ',
        3  'ZSDT0053'  'MATNR'      'TG_DOCS' 'MATERIAL'        TEXT-a15   '08' 'C' ' ' ' ' ' ' ' ',
        4  ''          ''           'TG_DOCS' 'TXT_MATERIAL'    TEXT-a16   '25' 'C' ' ' ' ' ' ' ' ',
        5  'ZSDT0053'  'WERKS'      'TG_DOCS' 'CENTRO'          TEXT-a17   '06' 'C' ' ' ' ' ' ' ' ',
        6  'ZSDT0053'  'PONTO_C'    'TG_DOCS' 'PONTO_COLETA'    TEXT-a18   '10' 'C' ' ' ' ' ' ' ' ',
        7  'ZSDT0053'  'TERMINAL'   'TG_DOCS' 'TERMINAL'        TEXT-a19   '10' 'C' ' ' ' ' ' ' ' ',
        8  'ZSDT0053'  'LGORT'      'TG_DOCS' 'DEPOSITO'        TEXT-a20   '10' 'C' ' ' ' ' ' ' ' ',
        9  'ZSDT0053'  'CHARG'      'TG_DOCS' 'LOTE'            TEXT-a21   '06' 'C' ' ' ' ' ' ' ' ',
        10 'ZSDT0053'  'ZMENG'      'TG_DOCS' 'QTD_PREVISTA'    TEXT-a22   '15' 'C' ' ' ' ' ' ' ' ',
        11 'ZSDT0053'  'ZIEME'      'TG_DOCS' 'UM'              TEXT-a23   '08' 'C' ' ' ' ' ' ' ' ',
        12 'ZSDT0053'  'DMBTR'      'TG_DOCS' 'PRECO'           TEXT-a24   '10' 'C' ' ' ' ' ' ' ' ',
        13 'ZSDT0053'  'PMEIN'      'TG_DOCS' 'UM_PRECO'        TEXT-a25   '08' 'C' ' ' ' ' ' ' ' ',
        14 'ZSDT0053'  'VLRTOT'     'TG_DOCS' 'VALOR_TOTAL'     TEXT-a26   '10' 'C' ' ' ' ' ' ' ' ',
        15 'ZSDT0053'  'VALDT'      'TG_DOCS' 'DATA_VENC'       TEXT-a27   '11' 'C' ' ' ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_DOCS

MODULE carrega_ordens OUTPUT.
  DATA: xtotal  TYPE zglt036-vlr_moeda_int,
        xtotald TYPE zglt036-vlr_moeda_doc.

  IF g_custom_container IS INITIAL.
    PERFORM atualiza_ordens.
  ENDIF.


ENDMODULE.                 " CARREGA_ORDENS  OUTPUT

FORM dynp_values_update USING us_repid
                              us_dynnr
                              us_field
                              us_value
                     CHANGING ch_subrc.

  DATA: da_dynpfield_tab LIKE dynpread OCCURS 0 WITH HEADER LINE,
        da_stepl         LIKE sy-stepl,
        da_repid         LIKE d020s-prog,
        da_dynnr         LIKE d020s-dnum.

  ch_subrc = 4.
  REFRESH da_dynpfield_tab.

  MOVE us_repid TO da_repid.
  MOVE us_dynnr TO da_dynnr.

  GET CURSOR LINE da_stepl.

  MOVE da_stepl TO da_dynpfield_tab-stepl.
  MOVE us_field TO da_dynpfield_tab-fieldname.
  MOVE us_value TO da_dynpfield_tab-fieldvalue.
  APPEND da_dynpfield_tab.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname               = da_repid
      dynumb               = da_dynnr
    TABLES
      dynpfields           = da_dynpfield_tab
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      OTHERS               = 8.
  IF sy-subrc EQ 0.
    ch_subrc = 0.
  ENDIF.

ENDFORM.                    " DYNP_VALUES_UPDATE

FORM atualiza_ordens .
  DATA: v_msg     TYPE char50,
        t_ordens  TYPE TABLE OF zsds018,
        w_ordens  TYPE          zsds018,
        t_estra   TYPE TABLE OF zsds019,
        w_estra   TYPE          zsds019,
        t_docs    TYPE TABLE OF zsds020,
        w_docs    TYPE          zsds020,
        vdata(10),
        tabix     TYPE sy-tabix.

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  CALL FUNCTION 'Z_SOL_OV_ESTRAT_LISTA'
    EXPORTING
      v_usuario      = sy-uname  " Campo do sistema ABAP: nome do usuário atual
    IMPORTING
      msg            = v_msg     " Comentário
    TABLES
      t_solicitacoes = t_ordens  " Solicitações de OV disponiveis para Liberação
      t_estra        = t_estra   " Estratégia de liberação - Solicitações OV
      t_docs         = t_docs.   " Solicitações de OV - detalhes

  REFRESH: tg_ordens, it_estra, it_docs.

  LOOP AT t_ordens INTO w_ordens.
    MOVE-CORRESPONDING w_ordens TO tg_ordens.

    SELECT SINGLE name1
      FROM kna1
      INTO tg_ordens-cliente_name
      WHERE kunnr EQ tg_ordens-cliente.

    APPEND tg_ordens.
  ENDLOOP.

  LOOP AT t_estra INTO w_estra.
    MOVE-CORRESPONDING w_estra TO wa_estra.
    APPEND wa_estra TO it_estra.
  ENDLOOP.
  SORT it_estra BY nro_sol_ov nivel.

  LOOP AT t_docs INTO w_docs.
    MOVE-CORRESPONDING w_docs TO wa_docs.
    APPEND wa_docs TO it_docs.
  ENDLOOP.
  SORT it_docs BY nro_sol_ov.

  LOOP AT tg_ordens ASSIGNING <wl_ordens>.

    FREE: <wl_ordens>-cellstyles.
    IF ( <wl_ordens>-observacoes IS NOT INITIAL ).
      l_mode               = cl_gui_alv_grid=>mc_style_enabled.
      ls_celltab-style     = cl_gui_alv_grid=>mc_style_button.
      ls_celltab-fieldname = 'ICON_OBS'.
      INSERT ls_celltab INTO TABLE lt_celltab.
    ENDIF.

    INSERT LINES OF lt_celltab INTO TABLE <wl_ordens>-cellstyles.

    <wl_ordens>-icon_obs = '@0P@'.

  ENDLOOP.

  IF g_custom_container IS NOT  INITIAL.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF obg_conteiner_estra IS NOT INITIAL.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF obg_conteiner_docs IS NOT INITIAL.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDFORM.                    " Atualiza_ORDENS


FORM f_refresh .
  CLEAR: wg_cadordem.
  PERFORM atualiza_ordens.
  REFRESH tg_estra.
  LOOP AT it_estra INTO wa_estra WHERE nro_sol_ov =  wg_cadordem-nro_sol_ov+0(10).
    APPEND wa_estra TO tg_estra.
  ENDLOOP.
  REFRESH tg_docs.
  LOOP AT it_docs INTO wa_docs WHERE nro_sol_ov = wg_cadordem-nro_sol_ov+0(10).
    APPEND wa_docs TO tg_docs.
  ENDLOOP.

  SORT tg_estra BY nivel.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.                    " F_REFRESH


FORM gera_html_liberacao USING i_nro_sol_ov.

  DATA: lt_obj_cont TYPE TABLE OF solisti1,
        ls_obj_cont TYPE solisti1,
        tl_zmail    TYPE TABLE OF zmail WITH HEADER LINE.

  DATA: wg_header     TYPE zsdt0051,
        tg_itens      TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tg_itens_mail TYPE TABLE OF zsdt0053 WITH HEADER LINE,
        tg_makt       TYPE TABLE OF ty_makt WITH HEADER LINE.

  DATA: wl_field(300),
        wl_qtd(20),
        wl_data(10),
        wl_t001w      TYPE t001w,
        wl_matnr(40), "*---> 15/06/2023 - Migração S4 - JS
*        wl_kna1       TYPE kna1,
        wl_assunto    TYPE string.

  DATA: wg_desc_vkgrp(27),
        wg_desc_kunnr(47),
        wg_desc_vkorg(20).

  DEFINE add_html.

    ls_obj_cont-line = &1.
    APPEND ls_obj_cont TO lt_obj_cont.
  END-OF-DEFINITION.

*Refresh Tables
  REFRESH : lt_obj_cont, tl_zmail, tg_itens, tg_itens_mail, tg_makt.
  CLEAR: wg_header, wl_t001w, tg_itens, tg_itens_mail, tg_makt.

  SELECT SINGLE *
      FROM zsdt0051
      INTO wg_header
       WHERE nro_sol_ov EQ i_nro_sol_ov.

  SELECT *
    FROM zsdt0053
    INTO TABLE tg_itens
     WHERE nro_sol_ov EQ wg_header-nro_sol_ov.

  IF ( 0 < lines( tg_itens[] ) ).

    SELECT matnr , maktx
      FROM makt
      INTO TABLE @tg_makt
       FOR ALL ENTRIES IN @tg_itens
       WHERE matnr EQ @tg_itens-matnr
      AND spras EQ @sy-langu.

  ENDIF.

  SELECT SINGLE kunnr, ort01 , regio
  FROM kna1
  INTO @DATA(wl_kna1)
  WHERE kunnr EQ @wg_header-kunnr.

  SELECT SINGLE vtext
    FROM tvkot
    INTO wg_desc_vkorg
     WHERE spras EQ sy-langu
       AND vkorg EQ wg_header-vkorg.

  SELECT SINGLE bezei
    FROM tvgrt
    INTO wg_desc_vkgrp
     WHERE spras EQ sy-langu
       AND vkgrp EQ wg_header-vkgrp.

  SELECT SINGLE name1
  FROM kna1
  INTO wg_desc_kunnr
   WHERE kunnr EQ wg_header-kunnr.

  add_html:
  '<!DOCTYPE html>',
  '<html>',
  '<head>',
  '<title>',
  'Liberação de Documento','</title>',
  '<style type="text/css">',
  'body,td,th {',
  '  font-family: "Arial", Times, serif; }',
  '</style>',
  '</head>',
'<p><img src= "cid:img1.gif"></p>',
******Header do HTML
  '<table width="700" height="361" border="1">',
  '  <tr>',
  '    <td width="801" valign="top"><table width="800" border="0">',
  '      <tr>',
  '       <td width="173">','Solicitação','</td>',
  '        <td width="166">','Tipo de Ordem','</td>',
  '        <td width="135">','Data Venda','</td>',
  '        <td width="160">','Tipo de Frete','</td>',
  '      </tr>',
  '      <tr>'.
  CONCATENATE '          <td>' wg_header-nro_sol_ov '</td>' INTO wl_field.
  CONDENSE wl_field NO-GAPS.
  add_html: wl_field.
*  '       <td>#NRO_SOL_OV#</td>',
  CONCATENATE '          <td>' wg_header-auart '</td>' INTO wl_field.
  CONDENSE wl_field NO-GAPS.
  add_html: wl_field.
*  '        <td>#AUART#</td>',
  WRITE wg_header-data_atual TO wl_data.
  CONCATENATE '          <td>' wl_data '</td>' INTO wl_field.
  CONDENSE wl_field NO-GAPS.
  add_html: wl_field.
*  '        <td>#DATA_ATUAL#</td>',
  CONCATENATE '          <td>' wg_header-inco1 '</td>' INTO wl_field.
  CONDENSE wl_field NO-GAPS.
  add_html: wl_field,
*  '        <td>#INCO1#</td>',
  '     </tr>',
  '    </table>',
  '      <br />',
  '      <table width="800" border="0">',
  '        <tr>',
  '          <td width="260">','Organização Vendas','</td>',
*  '          <td width="262">',TEXT-W07,'</td>',
  '          <td width="262">',' ','</td>',
  '          <td width="264">','Vendedor','</td>',
  '        </tr>',
  '        <tr>'.
  CONCATENATE '          <td width="260">' wg_header-vkorg '-' wg_desc_vkorg '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field.
*  '          <td>#VKORG#</td>',
  READ TABLE tg_itens INDEX 1.
  SELECT SINGLE *
    FROM t001w
    INTO wl_t001w
      WHERE werks EQ tg_itens-werks.
*  CONCATENATE '          <td width="262">' TG_ITENS-WERKS '-' WL_T001W-NAME1 '</td>' INTO WL_FIELD.
  CONCATENATE '          <td width="262">'  '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field.
*  '          <td>#WERKS#</td>'.
  CONCATENATE '          <td width="264">' wg_header-vkgrp '-' wg_desc_vkgrp '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
*  '          <td>#VKGRP#</td>',
  '        </tr>',
  '      </table>',
  '      <br />',
  '      <table width="800" border="0">',
  '        <tr>',
  '         <td width="426">','Cliente','</td>',
  '          <td width="286">','Local','</td>',
  '          <td width="88">','UF','</td>',
  '        </tr>',
  '        <tr>'.
  CONCATENATE '          <td width="426">' wg_header-kunnr '-' wg_desc_kunnr '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field.
*'          <td>#KUNNR#</td>',
  CONCATENATE '          <td width="286">' wl_kna1-ort01 '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field.
*'          <td width="286">#LOCAL#</td>',
  CONCATENATE '          <td width="286">' wl_kna1-regio '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
*'          <td width="88">#UF#</td>',
'        </tr>',
'      </table>',
'      <br />',
'      <table width="800" border="0">',
'        <tr>',
'          <td width="486">', 'Produto','</td>',
'          <td width="486">', 'Centro' ,'</td>',
'          <td width="214" align="center">','Quantidade','</td>',
'          <td width="100"> </td>',
'        </tr>',
'        <tr>'.

  LOOP AT tg_itens.
    wl_matnr = tg_itens-matnr.
    READ TABLE tg_makt INTO tg_makt WITH KEY matnr = tg_itens-matnr.
    SHIFT wl_matnr LEFT DELETING LEADING '0'.
    CONCATENATE '          <td width="486">' wl_matnr ' - ' tg_makt-maktx '</td>' INTO wl_field.
*    CONDENSE WL_FIELD NO-GAPS.
    add_html: wl_field.

    CONCATENATE '          <td width="486">' tg_itens-werks '</td>' INTO wl_field.
*    CONDENSE WL_FIELD NO-GAPS.
    add_html: wl_field.

    WRITE tg_itens-zmeng TO wl_qtd.
    CONDENSE wl_qtd NO-GAPS.
    CONCATENATE '          <td width="214" align="right">' wl_qtd '</td>' INTO wl_field.
    add_html: wl_field.
*    '          <td align="right">&nbsp;</td>',
    CONCATENATE '          <td width="100">' tg_itens-zieme '</td>' INTO wl_field.
*    CONDENSE WL_FIELD NO-GAPS.
    add_html: wl_field,
*    '          <td>&nbsp;</td>',
    '        </tr>  '.
  ENDLOOP.
  add_html:
  '      </table>',
  '      <br />',
  '      <table width="800" border="0">',
  '        <tr>',
  '          <td width="787">','Observações: ','</td>',
  '        </tr>',
  '        <tr>'.
  CONCATENATE '          <td>' wg_header-observacao '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
*    '          <td>&nbsp;</td>',
  '        </tr>',
  '      </table>',
  '      <br />',
  '      <table width="800" border="0">',
  '        <tr>',
  '          <td colspan="2">','Periodo Embarque','</td>',
  '          <td width="164">&nbsp;</td>',
  '          <td width="371">&nbsp;</td>',
  '        </tr>',
  '        <tr>',
  '          <td width="70">','Inicial: ','</td>'.
  WRITE wg_header-dtde_logist TO wl_data..
  CONCATENATE '          <td width="192">' wl_data '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
*    '          <td width="127">&nbsp;</td>',
  '          <td width="60">','Final: ','</td>'.
  WRITE wg_header-dtate_logist TO wl_data.
  CONCATENATE '          <td width="460">' wl_data '</td>' INTO wl_field.
*  CONDENSE WL_FIELD NO-GAPS.
  add_html: wl_field,
**    '          <td wid th="545">&nbsp;</td>',
  '        </tr>',
  '    </table>',
  '    <br /></td>',
  '  </tr>',
  '</table>',
  '<p>&nbsp;</p>',
  '</body>',
  '</html>'.

  SELECT *
    FROM zmail
     INTO  TABLE tl_zmail
     WHERE bukrs EQ wg_header-vkorg
       AND param_espec EQ wg_header-param_espec
       AND tcode EQ sy-tcode.

  CONCATENATE 'Solicitação de Venda' wg_header-nro_sol_ov 'Liberada'
   INTO wl_assunto SEPARATED BY space.

  tg_itens_mail[] = tg_itens[].
  SORT tg_itens_mail[] BY werks.
  DELETE ADJACENT DUPLICATES FROM tg_itens_mail[] COMPARING werks.

  LOOP AT tg_itens_mail.
    READ TABLE tl_zmail TRANSPORTING NO FIELDS WITH KEY werks = tg_itens_mail-werks.
    IF sy-subrc IS INITIAL.
      LOOP AT tl_zmail WHERE werks = tg_itens_mail-werks.
        PERFORM envia_email_html TABLES lt_obj_cont
                                 USING tl_zmail-email
                                       wl_assunto.
      ENDLOOP.
    ELSE.
      LOOP AT tl_zmail WHERE werks = ''.
        PERFORM envia_email_html TABLES lt_obj_cont
                                 USING tl_zmail-email
                                       wl_assunto.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

*  LOOP AT TL_ZMAIL.
*      PERFORM ENVIA_EMAIL_HTML TABLES LT_OBJ_CONT
*                               USING TL_ZMAIL-EMAIL
*                                     WL_ASSUNTO.
*    ELSE.
*    ENDIF.
*  ENDLOOP.
ENDFORM.                    " GERA_HTML

FORM envia_email_html TABLES lt_obj_cont STRUCTURE solisti1
                      USING p_mail p_assunto.
*--------------------------------------------------------------------*
*Mailing Related Data Decleration
*--------------------------------------------------------------------*
  DATA : ls_type     TYPE sood-objtp,

         lt_obj_head TYPE TABLE OF solisti1,
         ls_obj_head TYPE solisti1,

*         LT_OBJ_CONT TYPE TABLE OF SOLISTI1,
         ls_obj_cont TYPE solisti1,

         lt_recever  TYPE TABLE OF somlreci1,
         ls_recever  TYPE somlreci1,

         lv_date     TYPE char10,
*         LV_STR TYPE STRING,
         wl_email    TYPE adr6-smtp_addr.
*Refresh Tables
  REFRESH : lt_obj_head,  lt_recever. "lt_member.

* Type
  MOVE: 'HTML' TO ls_type,
         p_mail TO wl_email.

*--------------------------------------------------------------------*
*Send Email Via Class
*--------------------------------------------------------------------*
  DATA: lo_document    TYPE REF TO cl_document_bcs,
        lo_bcs         TYPE REF TO cl_bcs,
        lo_sapuser_bcs TYPE REF TO cl_sapuser_bcs,
        lo_recipient   TYPE REF TO if_recipient_bcs,
        lo_ex_bcs      TYPE REF TO cx_bcs,
        lv_message     TYPE string.

  CLEAR: lo_document.

  DATA : lv_sub TYPE so_obj_des.
  lv_sub = p_assunto.

  TRY.

      lo_document = cl_document_bcs=>create_document(
        i_type    = 'HTM'
        i_subject = lv_sub
        i_text    = lt_obj_cont[] ). "lt_txt_cont

      lo_bcs = cl_bcs=>create_persistent( ).
      lo_bcs->set_document( lo_document ).

      lo_recipient = cl_cam_address_bcs=>create_internet_address( wl_email ).

      lo_bcs->set_message_subject( ip_subject = p_assunto ).   "Subject

    CATCH cx_send_req_bcs.
    CATCH cx_address_bcs.
    CATCH cx_document_bcs.
  ENDTRY.

*--------------------------------------------------------------------*
*Image from MIME
*--------------------------------------------------------------------*

  DATA: o_mr_api         TYPE REF TO if_mr_api.

  DATA is_folder TYPE boole_d.
  DATA l_img1 TYPE xstring.
  DATA l_img2 TYPE xstring.
  DATA l_loio TYPE skwf_io.

  IF o_mr_api IS INITIAL.

    o_mr_api = cl_mime_repository_api=>if_mr_api~get_api( ).

  ENDIF.
  CALL METHOD o_mr_api->get
    EXPORTING
      i_url              = '/SAP/PUBLIC/zmime/amaggi.gif'
    IMPORTING
      e_is_folder        = is_folder
      e_content          = l_img1
      e_loio             = l_loio
    EXCEPTIONS
      parameter_missing  = 1
      error_occured      = 2
      not_found          = 3
      permission_failure = 4
      OTHERS             = 5.

  DATA :lt_hex1      TYPE solix_tab,
        lt_hex2      TYPE solix_tab,
        ls_hex       LIKE LINE OF lt_hex1,
        lv_img1_size TYPE sood-objlen,
        lv_img2_size TYPE sood-objlen.

  CLEAR : lt_hex1, lt_hex2, ls_hex, lv_img1_size, lv_img2_size.

  WHILE l_img1 IS NOT INITIAL.
    ls_hex-line = l_img1.
    APPEND ls_hex TO lt_hex1.
    SHIFT l_img1 LEFT BY 255 PLACES IN BYTE MODE.
  ENDWHILE.

*Findthe Size of the image
  DESCRIBE TABLE lt_hex1 LINES lv_img1_size.
  lv_img1_size = lv_img1_size * 255.

*--------------------------------------------------------------------*
*Attach Images
*--------------------------------------------------------------------*
  TRY.

      lo_document->add_attachment(
        EXPORTING
          i_attachment_type    = 'gif'                  " Document Class for Attachment
          i_attachment_subject = 'img1'                " Attachment Title
          i_attachment_size    = lv_img1_size           " Size of Document Content
          i_att_content_hex    = lt_hex1  " Content (Binary)
      ).

*--------------------------------------------------------------------*
*Add the recipient
*--------------------------------------------------------------------*

      CALL METHOD lo_bcs->add_recipient
        EXPORTING
          i_recipient = lo_recipient
          i_express   = 'X'.

*  LO_SAPUSER_BCS = CL_SAPUSER_BCS=>CREATE( SY-UNAME ).
      lo_sapuser_bcs = cl_sapuser_bcs=>create( 'JOBADM' ).
      lo_bcs->set_sender( i_sender = lo_sapuser_bcs ).
      lo_bcs->set_send_immediately( 'X' ).

    CATCH cx_send_req_bcs.
    CATCH cx_address_bcs.
    CATCH cx_document_bcs.
  ENDTRY.


*--------------------------------------------------------------------*
*Send Mail
*--------------------------------------------------------------------*
  TRY.
      CALL METHOD lo_bcs->send( ).

      COMMIT WORK.
      MESSAGE 'Send Successfully' TYPE 'S'.
    CATCH cx_bcs INTO lo_ex_bcs.
      lv_message = lo_ex_bcs->get_text( ).
  ENDTRY.
ENDFORM.                    " FORME


FORM f_preencher_dynpro   USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO gw_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO gw_bdc-program,
  l_value TO gw_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO gw_bdc-fnam,
      l_value TO gw_bdc-fval.
  ENDIF.
  APPEND gw_bdc TO gt_bdc.
  CLEAR: gw_bdc.

ENDFORM.
* RJF - INI - CS2022000728 - Envio das confirmaões de vendas junto no e-mail de Liberaçaõ da Venda #83043
*&---------------------------------------------------------------------*
*&      Form  F_GERA_ENVIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gera_envia .
  DATA: ls_control        TYPE ssfctrlop,
        ls_options        TYPE ssfcompop,
        job_output_info   TYPE ssfcrescl,
        ls_xsfparam_line  TYPE ssfxsfp,
        v_bin_filesize    TYPE i,
        it_docs           TYPE STANDARD TABLE OF docs,
        it_lines          TYPE STANDARD TABLE OF tline,
        lv_fname          TYPE rs38l_fnam,
        lv_mail_recipient TYPE swotobjid,
        lv_mail_sender    TYPE swotobjid,
        lv_control        TYPE ssfctrlop,
        lv_name           TYPE so_name,
        lv_output         TYPE ssfcompop,
        wl_zmeng(20),
        wl_dmbtr(20),
        wl_vlrtot(20).

  DATA: vl_form TYPE tdsfname,
        vl_name TYPE rs38l_fnam.

  DATA: i_otf       TYPE itcoo OCCURS 0 WITH HEADER LINE,
        i_tline     TYPE TABLE OF tline WITH HEADER LINE,
        i_receivers TYPE TABLE OF somlreci1 WITH HEADER LINE,
        i_record    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
* Objects to send mail.
        i_objpack   LIKE sopcklsti1 OCCURS 0 WITH HEADER LINE,
        i_objtxt    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_objbin    LIKE solisti1 OCCURS 0 WITH HEADER LINE,
        i_reclist   LIKE somlreci1 OCCURS 0 WITH HEADER LINE,
* Work Area declarations
        wa_objhead  TYPE soli_tab,
        w_ctrlop    TYPE ssfctrlop,
        w_compop    TYPE ssfcompop,
        w_return    TYPE ssfcrescl,
        wa_doc_chng TYPE sodocchgi1,
        w_data      TYPE sodocchgi1,
        wa_buffer   TYPE string, "To convert from 132 to 255
* Variables declarations
        v_form_name TYPE rs38l_fnam,
        v_len_in    LIKE sood-objlen,
        v_len_out   LIKE sood-objlen,
        v_len_outn  TYPE i,
        v_lines_txt TYPE i,
        v_lines_bin TYPE i,
        zva_txt     TYPE char255.

* RJF - ini - CS2023000883 Enviar confirmação de venda automaticamenbte para o Emial do cliente da ZSDT0062
  IF wg_estra-nro_sol_ov IS NOT INITIAL.
    SELECT SINGLE * FROM zsdt0051 " RJF
      INTO @DATA(wa_zsdt0051)
      WHERE nro_sol_ov EQ @wg_estra-nro_sol_ov.
  ENDIF.
* RJF - fim - CS2023000883 Enviar confirmação de venda automaticamenbte para o Emial do cliente da ZSDT0062

  vl_form = 'ZSDS0008'.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = vl_form
    IMPORTING
      fm_name            = vl_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

*  Impresora
  ls_control-no_dialog = 'X'. "Evita la pantalla de opciones de salida del formulario
  ls_options-tddest   = 'LOCL'.
  ls_options-tdimmed  = c_x.
  ls_options-tdnewid  = c_x.
  ls_options-tdnoarch = c_x.

  ls_control-preview = space.
  ls_control-device  = 'PRINTER'.
  ls_control-getotf  = 'X'.

  CLEAR:job_output_info.
  CALL FUNCTION vl_name
    EXPORTING
      user_settings      = ' '
      control_parameters = ls_control
      output_options     = ls_options
      i_nro_sol_ov       = wg_estra-nro_sol_ov "wg_header-nro_sol_ov
      i_digital          = 'X'
    IMPORTING
      job_output_info    = job_output_info
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
    " Erro
  ELSE.
    i_otf[] = job_output_info-otfdata[].
    CALL FUNCTION 'CONVERT_OTF'
      EXPORTING
        format                = 'PDF'
        max_linewidth         = 132
      IMPORTING
        bin_filesize          = v_bin_filesize
      TABLES
        otf                   = i_otf
        lines                 = i_tline
      EXCEPTIONS
        err_max_linewidth     = 1
        err_format            = 2
        err_conv_not_possible = 3
        OTHERS                = 4.
    IF sy-subrc EQ 0.
    ENDIF.
    LOOP AT i_tline.
      TRANSLATE i_tline USING '~'.
      CONCATENATE wa_buffer i_tline INTO wa_buffer.
    ENDLOOP.
    TRANSLATE wa_buffer USING '~'.
    DO.
      i_record = wa_buffer.
      APPEND i_record.
      SHIFT wa_buffer LEFT BY 255 PLACES.
      IF wa_buffer IS INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
* Attachment
    REFRESH: i_reclist,
    i_objtxt,
    i_objbin,
    i_objpack.
    CLEAR wa_objhead.
    i_objbin[] = i_record[].
* Create Message Body Title and Description
*    i_objtxt = TEXT-w01.
    zva_txt = |{ TEXT-w06 } { TEXT-w07 }|. "USER STORY 170715 / AOENNING
    i_objtxt = zva_txt. "USER STORY 170715 / AOENNING
    APPEND i_objtxt.

    DESCRIBE TABLE i_objtxt LINES v_lines_txt.
    READ TABLE i_objtxt INDEX v_lines_txt.
    wa_doc_chng-obj_name = 'smartform'.
    wa_doc_chng-expiry_dat = sy-datum + 10.
    CONCATENATE TEXT-w02 wg_estra-nro_sol_ov INTO wa_doc_chng-obj_descr
      SEPARATED BY space.
*     = 'smartform'.
    wa_doc_chng-sensitivty = 'F'.
    wa_doc_chng-doc_size = v_lines_txt * 255.
* Main Text
    CLEAR i_objpack-transf_bin.
    i_objpack-head_start = 1.
    i_objpack-head_num = 0.
    i_objpack-body_start = 1.
    i_objpack-body_num = v_lines_txt.
    i_objpack-doc_type = 'RAW'.
    APPEND i_objpack.
* Attachment (pdf-Attachment)
    i_objpack-transf_bin = 'X'.
    i_objpack-head_start = 1.
    i_objpack-head_num = 0.
    i_objpack-body_start = 1.
    DESCRIBE TABLE i_objbin LINES v_lines_bin.
    READ TABLE i_objbin INDEX v_lines_bin.
    i_objpack-doc_size = v_lines_bin * 255 .
    i_objpack-body_num = v_lines_bin.
    i_objpack-doc_type = 'PDF'.
    i_objpack-obj_name = 'smart'.
    i_objpack-obj_descr = wg_estra-nro_sol_ov.
    APPEND i_objpack.
    CLEAR i_reclist.
    i_reclist-receiver = 'mercadointerno@amaggi.com.br'(w05). "'ronaldo.freitas@amaggi.com.br'.
    i_reclist-rec_type = 'U'.
    APPEND i_reclist.
* RJF - Ini - CS2023000883 Enviar confirmação de venda automaticamenbte para o Emial do cliente da ZSDT0062
    IF wa_zsdt0051-v_email IS NOT INITIAL.
      i_reclist-receiver = wa_zsdt0051-v_email.
      i_reclist-rec_type = 'U'.
      APPEND i_reclist.
    ENDIF.
* RJF - Fim - CS2023000883 Enviar confirmação de venda automaticamenbte para o Emial do cliente da ZSDT0062
    CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
      EXPORTING
        document_data              = wa_doc_chng
        put_in_outbox              = 'X'
        commit_work                = 'X'
      TABLES
        packing_list               = i_objpack
        object_header              = wa_objhead
        contents_bin               = i_objbin
        contents_txt               = i_objtxt
        receivers                  = i_reclist
      EXCEPTIONS
        too_many_receivers         = 1
        document_not_sent          = 2
        document_type_not_exist    = 3
        operation_no_authorization = 4
        parameter_error            = 5
        x_error                    = 6
        enqueue_error              = 7
        OTHERS                     = 8.
    IF sy-subrc NE 0.
*WRITE:/ ‘Error When Sending the File’, SY-SUBRC.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-w03.
    ELSE.
      MESSAGE s836(sd) WITH TEXT-w04.
*WRITE:/ ‘Mail sent’.
    ENDIF.
  ENDIF.
ENDFORM.
* RJF - FIM - CS2022000728 - Envio das confirmaões de vendas junto no e-mail de Liberaçaõ da Venda #83043

*&---------------------------------------------------------------------*
*& Report  zgl017
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zhcmr_py0005.
TYPES: BEGIN OF ty_cadlote,
         empresa(30) TYPE c,
         lote(50)    TYPE c,
         usuario(20) TYPE c,
         total(15),
         dep_resp(2),
         data(10),
       END OF ty_cadlote,

       BEGIN OF ty_docs,
         dt_cad       TYPE sy-datum,
         status(10)   TYPE c, "Status Aprovação
         ocrsn        TYPE pc261-ocrsn,
         tp_pgto(10)  TYPE c, "Tipo pagamento
         doc_calc(10) TYPE c, "Doc. Calculo
         id_arq(10)   TYPE c,   "Ident. Arquivo
         doc_cont(10) TYPE c, "Doc Contabil
         bukrs        TYPE p0001-bukrs,   "Empresa
         butxt        TYPE t001-butxt,   "Nome Empresa
         abkrs        TYPE p0001-abkrs,
         areafpg(20)  TYPE c, "T549T-ATEXT,  "Area Folha - P0001-ABKRS
         werks        TYPE p0001-werks,
         arearh(20)   TYPE c, "T500P-NAME1, "Area RH - P0001-WERKS
         pernr        TYPE pa0001-pernr,
         cname        TYPE pa0002-cname,
         paydt        TYPE pc261-paydt, "Data Pagamento
         dt_cred      TYPE pc261-paydt, "Data Crédito
         betrg        TYPE p0009-betrg, "Valor
         bankl        TYPE bapip0009-bankl,
         banka        TYPE bapip0009-bankn,
         kostl        TYPE p0001-kostl, "Centro de Custo
         lote         TYPE zhcmt_py_0004-lote,
       END OF ty_docs,


       BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END   OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor,

       BEGIN OF ty_estra ,
         bukrs     TYPE zglt038-bukrs,
         lote      TYPE zglt038-lote,
         valor_de  TYPE zglt037-valor_de,
         valor_ate TYPE zglt037-valor_ate,
         aprovador TYPE zglt037-aprovador,
         nivel     TYPE zglt037-nivel,
         waers     TYPE zglt037-waers,
         estado(4),
         opcoes(4),
       END OF ty_estra,


       BEGIN OF ty_t001,
         bukrs TYPE t001-bukrs,
         butxt TYPE t001-butxt,
         land1 TYPE t001-land1,
       END OF ty_t001.



*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: ok-code         TYPE sy-ucomm,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      btn_rej(30).



DATA tg_lotes TYPE TABLE OF zfi_lotes_fol WITH HEADER LINE.

DATA   dyfields LIKE dynpread OCCURS 1 WITH HEADER LINE.

** Criação de tabela dinamica
DATA: t_fieldcatalog   TYPE lvc_t_fcat,
      w_fieldcatalog   TYPE lvc_s_fcat,
      wa_layout        TYPE lvc_s_layo,
      wa_stable        TYPE lvc_s_stbl,
      wg_editor        TYPE ty_editor,
      wg_cadlote       TYPE ty_cadlote,

      wa_zhcmt_py_0004 TYPE zhcmt_py_0004,

      wa_t001          TYPE ty_t001,
      wa_estra         TYPE ty_estra,
      wa_docs          TYPE ty_docs,

      tg_fields        TYPE TABLE OF ty_fields   WITH HEADER LINE,

      tg_editor        TYPE TABLE OF ty_editor,
      tg_estra         TYPE TABLE OF ty_estra,
      tg_docs          TYPE TABLE OF ty_docs,
      it_docs          TYPE TABLE OF ty_docs,

      it_estra         TYPE TABLE OF ty_estra,

      tg_msg_ret       TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.


DATA: it_estra_stat_ant   TYPE TABLE OF ty_estra,
      it_estra_stat_atual TYPE TABLE OF ty_estra.

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
        prog        LIKE sy-repid VALUE 'ZGL017',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

DATA: ok_code          LIKE sy-ucomm,
      wg_mensagem(30),
      wg_acao(30),
      vdt_apuracao(1),
      vmes_apuracao(1),
      vkokrs           TYPE tka02-kokrs,
      xclasse(1),
      xmodif(1),
      vdep_resp(2),
      vpgt_forn(1),
      vvalor_ate       TYPE zglt038-valor_ate.
DATA  txtemp(10).
DATA  txtlot(15).
DATA  txtusu(15).
DATA  txtval(15).




*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.
*            lcl_alv_toolbar2  definition deferred.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_LOTES',
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
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.


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


  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*

"lcl_alv_toolbar IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.
    DATA: wl_lotes    LIKE LINE OF tg_lotes,
          vflg_ico(1).

    CLEAR vdep_resp.
    vvalor_ate = 0.
    IF e_row GT 0.
      CLEAR: wg_cadlote.
      REFRESH tg_estra.

      READ TABLE tg_lotes INTO wl_lotes INDEX e_row.
      IF wl_lotes-status = icon_alert.
        MESSAGE text-i01 TYPE 'I'.
        CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
          EXPORTING
            functioncode           = '=ENT'
          EXCEPTIONS
            function_not_supported = 1
            OTHERS                 = 2.
        EXIT.
        CALL METHOD grid2->refresh_table_display
          EXPORTING
            is_stable = wa_stable.

        CALL METHOD grid3->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      ENDIF.

      SELECT SINGLE  *
        FROM zhcmt_py_0004 INTO wa_zhcmt_py_0004
        WHERE lote = wl_lotes-lote.

      wg_cadlote-empresa  = wl_lotes-empresa.
      CONCATENATE  wl_lotes-lote '-' wl_lotes-tp_pgto INTO wg_cadlote-lote.
      wg_cadlote-usuario  = wa_zhcmt_py_0004-usuario.
      wg_cadlote-total    = wl_lotes-total.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '=ENT'
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.

      REFRESH tg_estra.
      LOOP AT it_estra INTO wa_estra WHERE lote = wl_lotes-lote.
        APPEND wa_estra TO tg_estra.
      ENDLOOP.

      REFRESH tg_docs.
      LOOP AT it_docs INTO wa_docs WHERE lote = wl_lotes-lote..
        APPEND wa_docs TO tg_docs.
      ENDLOOP.
    ENDIF.
    SORT tg_estra BY nivel.

*    PERFORM ATUALIZA_LOTES.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_click2.

  ENDMETHOD.                    "ON_DOUBLE_CLICK2
  METHOD on_click.
    DATA: v_msg     TYPE char50,
          t_lotes   TYPE TABLE OF zfi_lotes_fol,
          w_lotes   TYPE          zfi_lotes_fol,
          wl_estra  LIKE LINE OF tg_estra,
          wl_estra2 LIKE LINE OF tg_estra,
          t_estra   TYPE TABLE OF zfi_estrategia_fol,
          w_estra   TYPE          zfi_estrategia_fol,
          t_docs    TYPE TABLE OF zgl_docs_imp,
          w_docs    TYPE          zgl_docs_imp,
          wl_lotes  LIKE LINE OF tg_lotes.

    IF e_row_id GT 0.
      READ TABLE tg_estra INTO wl_estra2 INDEX e_row_id.

      READ TABLE tg_lotes INTO wl_lotes WITH KEY lote = wl_estra2-lote BINARY SEARCH.
      MOVE-CORRESPONDING wl_lotes TO w_lotes.

      APPEND w_lotes  TO t_lotes.

* Verifica o status anterior
      PERFORM zf_capturar_status_anterior USING wl_lotes-lote.

      LOOP AT it_estra INTO wl_estra WHERE lote = wl_lotes-lote.
        MOVE-CORRESPONDING wl_estra TO w_estra.
        IF wl_estra-aprovador = wl_estra2-aprovador.
          w_estra-opcoes  = wl_estra2-opcoes.
        ENDIF.
        APPEND w_estra TO t_estra.
      ENDLOOP.

      CALL FUNCTION 'Z_FL_ESTRATEGIA_EXECUTAR'
        EXPORTING
          v_usuario = sy-uname
        IMPORTING
          msg       = v_msg
        TABLES
          t_lotes   = t_lotes
          t_estra   = t_estra.

      LOOP AT t_estra INTO w_estra
        WHERE aprovador EQ sy-uname.
        MOVE: w_estra-opcoes TO wl_estra-opcoes,
              w_estra-estado TO wl_estra-estado.
        MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
      ENDLOOP.

* Verifica o status anterior
      PERFORM zf_capturar_status_atual USING wl_lotes-lote.
      PERFORM zf_enviar_email_prox_aprovador USING wl_lotes-lote.

      PERFORM atualiza_lotes.

      REFRESH: tg_estra, tg_docs.
      CALL METHOD grid2->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      CALL METHOD grid3->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDIF.
  ENDMETHOD.                    "ON_CLICK

  METHOD on_data_changed.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

  ENDMETHOD.                    "on_data_changed_finisheD

  "on_data_changed_finisheD
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.

  ENDMETHOD.                    "drop_complete
ENDCLASS.                    "lcl_dragdrop_receiver IMPLEMENTATION

*ALRS fim
*&---------------------------------------------------------------------*
*&      Module  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.
  APPEND c_save TO fcode.
  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.
  DATA: waref      TYPE REF TO data.
  IF g_custom_container IS INITIAL.
    txtemp = text-l01.
    txtlot = text-l02.
    txtusu = text-l03.
    txtval = text-l04.
    btn_rej = text-b01.


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
    wa_layout-grid_title = text-t01 .

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

*    CREATE OBJECT OBG_TOOLBAR
*      EXPORTING
*        IO_ALV_GRID = GRID1.
*
**      * Register event handler
*    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
*    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.
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

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_lotes[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_double_click FOR grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed FOR grid1.

*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = text-t02 .
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
    "LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK2 FOR GRID2.
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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = text-t03 .
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
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 ' '                    ' '                'TG_LOTES' 'STATUS'           ' '             '05' ' ' ' ' ' ',
        2 ' '                    ' '                'TG_LOTES' 'EMPRESA'          text-a01        '31' ' ' ' ' ' ',
        3 'ZGLT034'              'LOTE'             'TG_LOTES' 'LOTE'             text-a02        '08' ' ' ' ' ' ',
        4 ' '                    ' '                'TG_LOTES' 'AREAFPG'          text-a03        '18' ' ' ' ' ' ',
        6 'ZGLT036'              'VLR_MOEDA_INT'    'TG_LOTES' 'TOTAL'            text-a14        '12' ' ' ' ' ' ',
        6 'ZGLT036'              'SGTXT'            'TG_LOTES' 'SGTXT'            text-a05        '20' ' ' ' ' ' '.


ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      Form  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            p_scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-edit          = p_edit.
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

  IF p_field EQ 'OPCOES' OR p_field EQ 'DOC_LCTO' OR p_field EQ 'BELNR'.
    w_fieldcatalog-hotspot = c_x.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA:   vflg_ico(1).

  CASE ok-code.
    WHEN c_refresh.
      PERFORM f_refresh.
    WHEN 'REJ'.
      READ TABLE tg_estra INTO wa_estra WITH KEY  aprovador = sy-uname.
      IF sy-subrc = 0.
        btn_rej = text-b01.
        IF  wa_estra-opcoes = icon_reject.
          wa_estra-opcoes = icon_set_state.
        ELSEIF  wa_estra-opcoes = icon_set_state.
          wa_estra-opcoes = icon_reject.
        ENDIF.
        MODIFY tg_estra FROM wa_estra INDEX sy-tabix TRANSPORTING opcoes.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN c_back.
      SET SCREEN 0.

    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_estra .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
*        1 'ZGLT037'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         TEXT-A06      '10' ' ' ' ' ' ',
*        1 'ZGLT037'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        TEXT-A07      '10' ' ' ' ' ' ',
*        1 'ZGLT037'           'WAERS'           'TG_ESTRA' 'WAERS'            TEXT-A16      '04' ' ' ' ' ' ',
        1 'ZGLT037'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        text-a08      '20' ' ' ' ' ' ',
        1 ' '                        ' '        'TG_ESTRA' 'ESTADO'           text-a09      '10' ' ' ' ' ' ',
        1 ' '                        ' '        'TG_ESTRA' 'OPCOES'           text-a10      '20' ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_docs .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
      0  ''  ''   'TG_DOCS' 'LOTE'       'Lote'                '10'  ' '    '' ' ',
      1  ''  ''   'TG_DOCS' 'STATUS'     'Status'              '08'  ' '    '' ' ',
      3  ''  ''   'TG_DOCS' 'TP_PGTO'    'Tipo Pgto'           '10'  ' '    '' ' ',
      3  ''  ''   'TG_DOCS' 'BUKRS'      'Empr.'               '05'  ' '    '' ' ',
      4  ''  ''   'TG_DOCS' 'BUTXT'      'Nome Empresa'        '30'  ' '    '' ' ',
      5  ''  ''   'TG_DOCS' 'AREAFPG'    'Area de Folha'       '20'  ' '    '' ' ',
      6  ''  ''   'TG_DOCS' 'AREARH'     'Area de RH'          '20'  ' '    '' ' ',
      7  ''  ''   'TG_DOCS' 'PERNR'      'Nro.Pessoal'         '12'  ' '    '' ' ',
      8  ''  ''   'TG_DOCS' 'CNAME'      'Nome colaborador'    '30'  ' '    '' ' ',
      9  ''  ''   'TG_DOCS' 'KOSTL'      'Centro de Custo'     '18'  ' '    '' ' ',
     10  ''  ''   'TG_DOCS' 'PAYDT'      'Dt.Pgto'             '10'  ' '    '' ' ',
     11  ''  ''   'TG_DOCS' 'DT_CRED'    'Dt.Crédito'          '12'  ' '    '' ' ',
     12  ''  ''   'TG_DOCS' 'BETRG'      'Valor R$'            '10'  ' '    '' ' ',
     13  ''  ''   'TG_DOCS' 'BANKA'      'Banco Pagador'       '20'  ' '    '' ' ',
     14  ''  ''   'TG_DOCS' 'DOC_CALC'   'Doc.Calculo'         '12'  ' '    '' ' ',
     15  ''  ''   'TG_DOCS' 'ID_ARQ'     'Ident.Arquivo'       '15'  ' '    '' ' ',
     16  ''  ''   'TG_DOCS' 'DOC_CONT'   'Doc.Contabil'        '15'  ' '    '' ' '.

ENDFORM.                    " MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*&      Module  CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE carrega_lotes OUTPUT.
  DATA xtotal TYPE zglt036-vlr_moeda_int.

  IF g_custom_container IS INITIAL.
    PERFORM atualiza_lotes.
  ENDIF.


ENDMODULE.                 " CARREGA_LOTES  OUTPUT
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
*&---------------------------------------------------------------------*
*&      Form  Atualiza_lotes
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_lotes .
  DATA: v_msg      TYPE char50,
        t_lotes    TYPE TABLE OF zfi_lotes_fol,
        w_lotes    TYPE          zfi_lotes_fol,
        t_estra    TYPE TABLE OF zfi_estrategia_fol,
        w_estra    TYPE          zfi_estrategia_fol,
        vdata(10),
        tabix      TYPE sy-tabix,
        it_py_0004 TYPE TABLE OF zhcmt_py_0004,
        wa_py_0004 LIKE LINE OF it_py_0004.

* Valor
  DATA: v_valor_numerico(11) TYPE p DECIMALS 2.
  DATA: v_valor_formatado TYPE char30.

  CALL FUNCTION 'Z_FL_ESTRATEGIA_LISTA'
    EXPORTING
      v_usuario = sy-uname
    IMPORTING
      msg       = v_msg
    TABLES
      t_lotes   = t_lotes
      t_estra   = t_estra.

  REFRESH: tg_lotes, it_estra, it_docs.
  LOOP AT t_lotes INTO w_lotes.
    MOVE-CORRESPONDING w_lotes TO tg_lotes.
    v_valor_numerico = tg_lotes-total.
    WRITE v_valor_numerico TO v_valor_formatado CURRENCY 'BRL'.
    SHIFT v_valor_formatado LEFT DELETING LEADING space.
    tg_lotes-total = v_valor_formatado.
    APPEND tg_lotes.
  ENDLOOP.

  IF tg_lotes[] IS NOT INITIAL.

    SORT tg_lotes BY lote .

    LOOP AT t_estra INTO w_estra.
      MOVE-CORRESPONDING w_estra TO wa_estra.
      APPEND wa_estra TO it_estra.
    ENDLOOP.
    SORT it_estra BY lote nivel.

    SELECT  *
    FROM zhcmt_py_0004
      INTO TABLE it_py_0004
  FOR ALL ENTRIES IN tg_lotes
  WHERE lote EQ tg_lotes-lote.

    LOOP AT it_py_0004 INTO wa_py_0004.
      MOVE-CORRESPONDING wa_py_0004 TO wa_docs.
      APPEND wa_docs TO it_docs.
    ENDLOOP.
    SORT it_docs BY lote.
  ENDIF.


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


ENDFORM.                    " Atualiza_lotes

*&---------------------------------------------------------------------*
*&      Form  F_REFRESH
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_refresh .
  PERFORM atualiza_lotes.
  REFRESH tg_estra.
  LOOP AT it_estra INTO wa_estra WHERE lote =  wg_cadlote-lote+0(10).
    APPEND wa_estra TO tg_estra.
  ENDLOOP.

  SORT tg_estra BY nivel.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.                    " F_REFRESH
*&---------------------------------------------------------------------*
*&      Form  ZF_CAPTURAR_STATUS_ANTERIOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_capturar_status_anterior USING p_lote.
  it_estra_stat_ant = it_estra.
  DELETE it_estra_stat_ant WHERE lote <> p_lote.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_CAPTURAR_STATUS_ATUAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_LOTES_LOTE  text
*----------------------------------------------------------------------*
FORM zf_capturar_status_atual  USING  p_lote.
  it_estra_stat_atual = tg_estra.
  DELETE it_estra_stat_atual WHERE lote <> p_lote.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ENVIAR_EMAIL_PROX_APROVADOR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_enviar_email_prox_aprovador USING p_lote .

  DATA: l_tabix TYPE sy-tabix.

  LOOP AT it_estra_stat_ant INTO DATA(w_stat_ant).

    READ TABLE it_estra_stat_atual INTO DATA(w_stat_atual)
                              WITH KEY aprovador = w_stat_ant-aprovador.

    CHECK  sy-subrc IS INITIAL
       AND ( w_stat_ant-opcoes = '@3J@' "Se o status Anterior for aguard. aprovação
        AND w_stat_atual-opcoes = '@2W@' ). "status atual for Aprovado.

* verifica se há um próximo aprovador e encaminha e-mail
    l_tabix = sy-tabix.
    ADD 1 TO l_tabix.

    READ TABLE it_estra_stat_atual INTO DATA(w_aprovador) INDEX l_tabix.
    CHECK sy-subrc IS INITIAL.

    PERFORM zf_envia_email_notificacao USING w_aprovador-aprovador p_lote.

  ENDLOOP.

  REFRESH: it_estra_stat_ant, it_estra_stat_atual.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_ENVIA_EMAIL_NOTIFICACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_PY_0004  text
*----------------------------------------------------------------------*
FORM zf_envia_email_notificacao USING p_aprovador p_lote.

  DATA: tl_itens TYPE TABLE OF zhcmt_py_0004,
        wl_itens LIKE LINE OF tl_itens.

  DATA: vl_aprovador TYPE zhcmt_py_0005-aprovador.

* Busca Cód. Empresa
  IF  p_lote IS NOT INITIAL.

* Busca itens do lote
    SELECT *
     FROM zhcmt_py_0004
     INTO TABLE @DATA(tl_0004)
    WHERE lote = @p_lote
      and status = 'LIB'.

    LOOP AT tl_0004 INTO DATA(wl_py_0004).

      wl_itens-bukrs   = wl_py_0004-bukrs.
      wl_itens-butxt   = wl_py_0004-butxt.
      wl_itens-lote    = wl_py_0004-lote.
      wl_itens-dt_cad  = wl_py_0004-dt_cad.
      wl_itens-betrg   = wl_py_0004-betrg.

      COLLECT wl_itens INTO tl_itens.

    ENDLOOP.

    IF tl_itens[] IS NOT INITIAL.

* Verifica validade do aprovador
      SELECT * FROM zhcmt_py_0005 INTO TABLE @DATA(tl_0005)
        FOR ALL ENTRIES IN @tl_itens
        WHERE bukrs     LE @tl_itens-bukrs
        AND bukrs_ate   GE @tl_itens-bukrs
        AND dt_val_ate  >= @sy-datum
        AND abkrs       EQ ' '.

    ENDIF.

* Verifica preenchimentos de aprovador temporário
    READ TABLE tl_0005 INTO DATA(wl_0005) WITH KEY aprovador = p_aprovador.

    CHECK sy-subrc IS INITIAL.

    IF wl_0005-aprovador_tmp IS NOT INITIAL AND wl_0005-dt_val_ate_tmp >= sy-datum.
      vl_aprovador = wl_0005-aprovador_tmp.
    ELSE.
      vl_aprovador = wl_0005-aprovador.
    ENDIF.

* Dispara e-mail de notificação
    CHECK vl_aprovador IS NOT INITIAL.

    CALL FUNCTION 'ZHCMF_EMAIL_NOTIFICACAO'
      EXPORTING
        i_aprovador = vl_aprovador
      TABLES
        tl_itens    = tl_itens.

  ENDIF.

ENDFORM.

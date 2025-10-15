*&---------------------------------------------------------------------*
*& Report  ZIMP56
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfir0027.

TYPES: BEGIN OF ty_cadlote,
         empresa(30) TYPE c,
         lote(50)    TYPE c,
         usuario(20) TYPE c,
         total       TYPE zfit0036-vlr_pgto,
         dep_resp(2),
         data(10),
       END OF ty_cadlote,


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
         bukrs     TYPE zinv_lotes_aprov-bukrs,
         lote      TYPE zinv_lotes_aprov-lote,
         valor_de  TYPE zinv_aprovador-valor_de,
         valor_ate TYPE zinv_aprovador-valor_ate,
         aprovador TYPE zinv_aprovador-aprovador,
         nivel     TYPE zinv_aprovador-nivel,
         estado(4),
         opcoes(4),
       END OF ty_estra,

       BEGIN OF ty_docs ,
         lote       TYPE zimp_cad_lote-lote,
         obj_key    TYPE zfit0036-obj_key,
         bukrs      TYPE zfit0036-bukrs,
         invoice    TYPE zfit0036-invoice,
         nro_sol_ov TYPE zsdt0051-nro_sol_ov,
         navio      TYPE zfit0036-navio,
         vlr_pgto   TYPE zfit0036-vlr_pgto,
         hbkid      TYPE zfit0036-hbkid,
         name1      TYPE lfa1-name1,
         observacao TYPE zfit0036-observacao,
       END OF ty_docs.



*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: ok-code         TYPE sy-ucomm,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,



      BEGIN OF tg_lotes OCCURS 0,
        status(4),
        empresa(30) TYPE c,
        lote        TYPE zfit0036-lote,
        dt_venc(10),
        moeda_pgto  TYPE zfit0036-moeda_pgto,
        total       TYPE zfit0036-vlr_pgto,
        usuario     TYPE zfit0036-usuario,
        tipo(12),
        tp_operacao TYPE zfit0043-tp_operacao,
        ds_operacao TYPE zfit0043-ds_operacao,
        matnr       TYPE zfit0036-matnr,
        maktx       TYPE makt-maktx,
        color(4),
      END OF tg_lotes.

DATA   dyfields LIKE dynpread OCCURS 1 WITH HEADER LINE.

** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl,
      wg_editor      TYPE ty_editor,
      wg_cadlote     TYPE ty_cadlote,


      wa_estra       TYPE ty_estra,
      wa_docs        TYPE ty_docs,

      tg_fields      TYPE TABLE OF ty_fields   WITH HEADER LINE,

      tg_editor      TYPE TABLE OF ty_editor,
      it_estra       TYPE TABLE OF ty_estra,
      tg_estra       TYPE TABLE OF ty_estra,
      tg_docs        TYPE TABLE OF ty_docs,
      it_docs        TYPE TABLE OF ty_docs,

      tg_msg_ret     TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.

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
        prog        LIKE sy-repid VALUE 'ZIMP56',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

DATA: ok_code         LIKE sy-ucomm,
      wg_mensagem(30),
      wg_acao(30),
      btn_rej(30)     VALUE '@8Y@ Rejeitar'.


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
      on_double_click2 FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

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
    DATA: wl_lotes LIKE LINE OF tg_lotes.

    IF e_row GT 0.
      CLEAR: wg_cadlote.
      REFRESH tg_estra.
      REFRESH tg_docs.
      READ TABLE tg_lotes INTO wl_lotes INDEX e_row.
      IF wl_lotes-status = icon_alert.
        MESSAGE 'O Lote selecionado esta com a data de vencimento no passado, não é permitido aprovar.'(001) TYPE 'I'.
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
      wg_cadlote-empresa  = wl_lotes-empresa.
      CONCATENATE wl_lotes-lote '-' wl_lotes-tipo INTO wg_cadlote-lote.
      wg_cadlote-usuario  = wl_lotes-usuario.
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
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_double_click2.
    DATA: wl_docs LIKE LINE OF tg_docs.
    IF e_row GT 0.
      IF e_column = 'NRO_SOL_OV' .
        READ TABLE tg_docs INTO wl_docs INDEX e_row.
        IF wl_docs-nro_sol_ov IS NOT INITIAL.
          SET PARAMETER ID 'SOL' FIELD wl_docs-nro_sol_ov.
          CALL TRANSACTION 'ZSDT0062' AND SKIP FIRST SCREEN.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK2
  METHOD on_click.

    DATA: v_msg    TYPE char50,
          t_lotes  TYPE TABLE OF ziv_lotes_imp,
          w_lotes  TYPE          ziv_lotes_imp,
          t_estra  TYPE TABLE OF zfi_estrategia_imp,
          w_estra  TYPE          zfi_estrategia_imp,
          wl_estra LIKE LINE OF tg_estra,
          t_docs   TYPE TABLE OF ziv_docs_imp,
          w_docs   TYPE          ziv_docs_imp,
          wl_lotes LIKE LINE OF tg_lotes.

    IF e_row_id GT 0.
      READ TABLE tg_estra INTO wl_estra INDEX e_row_id.

      READ TABLE tg_lotes INTO wl_lotes WITH KEY lote = wl_estra-lote BINARY SEARCH.
      MOVE-CORRESPONDING wl_lotes TO w_lotes.
      APPEND w_lotes  TO t_lotes.

      LOOP AT it_estra INTO wl_estra WHERE lote = wl_lotes-lote.
        MOVE-CORRESPONDING wl_estra TO w_estra.
        APPEND w_estra TO t_estra.
      ENDLOOP.

      CALL FUNCTION 'Z_IV_ESTRATEGIA_EXECUTAR'
        EXPORTING
          v_usuario = sy-uname
        IMPORTING
          msg       = v_msg
        TABLES
          t_lotes   = t_lotes
          t_estra   = t_estra.

      MESSAGE s836(sd) DISPLAY LIKE 'W' WITH v_msg.

      LOOP AT t_estra INTO w_estra
        WHERE aprovador EQ sy-uname.                    "Modificação CS 2016000820
        "IF E_ROW_ID = SY-TABIX.                        "Modificação CS 2016000820
        MOVE: w_estra-opcoes TO wl_estra-opcoes,
              w_estra-estado TO wl_estra-estado.
        MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
        "ENDIF.
      ENDLOOP.
      PERFORM atualiza_lotes.

      CALL METHOD grid2->refresh_table_display
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

  IF sy-langu = 'E'.
    CLEAR: btn_rej.
    MOVE '@8Y@ Reject' TO btn_rej.
  ENDIF.

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
    wa_layout-grid_title = 'Lotes disponíveis para Liberação'(002).

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
    wa_layout-grid_title = 'Estratégia de Liberação'(003).
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
    wa_layout-grid_title = 'Documentos do lote'(004).
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
                  lcl_event_handler=>on_double_click2 FOR grid3.

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
        1 ' '                   ' '               'TG_LOTES' 'STATUS'           ' '                  '02' ' ' ' ' ' ',
        "2 ' '                   ' '               'TG_LOTES' 'EMPRESA'          'Empresa'           '30' ' ' ' ' ' ',
        3 'ZFIT0036'            'LOTE'            'TG_LOTES' 'LOTE'             'Lote'(005)          '08' ' ' ' ' ' ',
        3 ' '                   ' '               'TG_LOTES' 'TIPO'             'Tipo'(006)          '15' ' ' ' ' ' ',
        3 ' '                   ' '               'TG_LOTES' 'TP_OPERACAO'      'Referente'(007)     '06' ' ' ' ' ' ',
        3 ' '                   ' '               'TG_LOTES' 'DS_OPERACAO'      'Descr.Ref.'(008)    '10' ' ' ' ' ' ',
        3 ' '                   ' '               'TG_LOTES' 'MAKTX'            'Material'(009)      '12' ' ' ' ' ' ',
        4 'ZFIT0036'            'DT_VENC'         'TG_LOTES' 'DT_VENC'          'Vencimento'(010)    '12' ' ' ' ' ' ',
        4 'ZFIT0036'            'MOEDA_PGTO'      'TG_LOTES' 'MOEDA_PGTO'       'Moeda Pgto'(011)    '10' ' ' ' ' ' ',
        5 'ZFIT0036'            'VLR_PGTO'        'TG_LOTES' 'TOTAL'            'Total'(012)         '15' ' ' ' ' ' '.


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
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
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

  IF p_field EQ 'OPCOES' OR p_field EQ 'NRO_SOL_OV'.
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
  CASE ok-code.
    WHEN c_refresh.
      REFRESH: tg_estra, tg_docs.
      PERFORM atualiza_lotes.
    WHEN 'REJ'.
      READ TABLE tg_estra INTO wa_estra WITH KEY  aprovador = sy-uname.
      IF sy-subrc = 0.
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
        1 'ZINV_APROVADOR'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         'Valor de'(013)      '15' ' ' ' ' ' ',
        1 'ZINV_APROVADOR'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        'Valor ate'(014)     '15' ' ' ' ' ' ',
        1 'ZINV_APROVADOR'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        'Aprovador'(015)     '20' ' ' ' ' ' ',
        1 ' '                        ' '               'TG_ESTRA' 'ESTADO'           'Estado'(016)        '10' ' ' ' ' ' ',
        1 ' '                        ' '               'TG_ESTRA' 'OPCOES'           'Opções Liber.'(017) '12' ' ' ' ' ' '.

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
        1 'ZFIT0036'           'OBJ_KEY'             'TG_DOCS'  'OBJ_KEY'     'OBJ_KEY'(018)      '20' ' ' ' ' ' ',
        1 'ZFIT0036'           'BUKRS'               'TG_DOCS'  'BUKRS'       'Empresa'(019)      '10' ' ' ' ' ' ',
        3 'ZFIT0036'           'INVOICE'             'TG_DOCS'  'INVOICE'     'Invoice'(020)      '15' ' ' ' ' ' ',
        3 'ZSDT0051'           'NRO_SOL_OV'          'TG_DOCS'  'NRO_SOL_OV'  'Nro.Sol.OV.'(021)  '15' ' ' ' ' ' ',
        3 'ZFIT0036'           'NAVIO'               'TG_DOCS'  'NAVIO'       'Navio'(022)        '30' ' ' ' ' ' ',
        3 'ZFIT0036'           'VLR_PGTO'            'TG_DOCS'  'VLR_PGTO'    'Valor'(023)        '15' ' ' ' ' ' ',
        3 'LFA1'               'NAME1'               'TG_DOCS'  'NAME1'       'Fornecedor'(024)   '30' ' ' ' ' ' ',
        3 'ZFIT0036'           'OBSERVACAO'          'TG_DOCS'  'OBSERVACAO'  'Observação'(025)   '50' ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*&      Module  CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE carrega_lotes OUTPUT.
  DATA xtotal TYPE zfit0036-vlr_pgto.

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

  DATA: v_msg   TYPE char50,
        t_lotes TYPE TABLE OF ziv_lotes_imp,
        w_lotes TYPE          ziv_lotes_imp,
        t_estra TYPE TABLE OF zfi_estrategia_imp,
        w_estra TYPE          zfi_estrategia_imp,
        t_docs  TYPE TABLE OF ziv_docs_imp,
        w_docs  TYPE          ziv_docs_imp.

  CALL FUNCTION 'Z_IV_ESTRATEGIA_LISTA'
    EXPORTING
      v_usuario = sy-uname
    IMPORTING
      msg       = v_msg
    TABLES
      t_lotes   = t_lotes
      t_estra   = t_estra
      t_docs    = t_docs.

  REFRESH: tg_lotes, it_estra, it_docs.

  LOOP AT t_lotes INTO w_lotes.
    MOVE-CORRESPONDING w_lotes TO tg_lotes.
    APPEND tg_lotes.
  ENDLOOP.
  SORT tg_lotes BY lote.

  LOOP AT t_estra INTO w_estra.
    MOVE-CORRESPONDING w_estra TO wa_estra.
    APPEND wa_estra TO it_estra.
  ENDLOOP.
  SORT t_estra BY lote.

  LOOP AT t_docs INTO w_docs.
    MOVE-CORRESPONDING w_docs TO wa_docs.
    APPEND wa_docs TO it_docs.
  ENDLOOP.
  SORT it_docs BY lote .

ENDFORM.                    " Atualiza_lotes

*&---------------------------------------------------------------------*
*&      Form  ENvia_email_Fim
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM envia_email_fim.

  FIELD-SYMBOLS: <fs_solix> TYPE solix.

* Objetos para enviar email
  DATA: objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.
  DATA: objhead     LIKE solisti1   OCCURS  1 WITH HEADER LINE.
  DATA: objbin_ord  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_log  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_ann  TYPE solisti1.
  DATA: objbin    LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin1   TYPE soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin LIKE LINE OF objbin.
  DATA: content_hex TYPE STANDARD TABLE OF solix WITH HEADER LINE.
  DATA: objtxt      LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE.
  DATA: doc_chng    LIKE sodocchgi1.
  DATA: tab_lines   LIKE sy-tabix.
  DATA: l_anex      TYPE string.
  DATA: l_leng      TYPE i.
  DATA: l_arq       TYPE string.
  DATA: l_tam       TYPE i.
  DATA: l_tam_ord   TYPE i.
  DATA: l_tam_log   TYPE i.
  DATA: l_email(300) TYPE c.
  DATA: vlinha      TYPE i.
  DATA: vuser       TYPE sy-uname.

  DATA: it_shortcut_param LIKE zst_shortcut_par OCCURS 0 WITH HEADER LINE.
  DATA: content TYPE string.

*  ** Pass the required parameters and create the shortcut
  CLEAR it_shortcut_param.
  REFRESH it_shortcut_param.

  DATA:      ctotal(20),
             vdata(10),
             vbukrs TYPE zimp_cad_lote-bukrs.
  vbukrs = wg_cadlote-empresa+0(4).
  DATA: it_mail TYPE TABLE OF zmail WITH HEADER LINE.
  SELECT *
    FROM zmail
    INTO TABLE it_mail
    WHERE tcode = 'ZFI0019'
    AND   bukrs = vbukrs.
  IF sy-subrc NE 0.
    SELECT *
    FROM zmail
    INTO TABLE it_mail
    WHERE tcode = 'ZFI0019'.
  ENDIF.

  LOOP AT it_mail.
    IF NOT it_mail-usuario IS INITIAL.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF it_mail-usuario IS INITIAL.
    MESSAGE 'Usuário padrão não cadastrado.'(026) TYPE 'E'.
    EXIT.
  ELSEIF it_mail[] IS INITIAL.
    MESSAGE 'E-Mail não cadastrado para esta transação.'(027) TYPE 'E'.
    EXIT.
  ENDIF.


*  READ TABLE IT_ZFIT0036 INTO WA_ZFIT0036 WITH KEY LOTE = WA_ESTRA-LOTE BINARY SEARCH.
*  CONCATENATE WA_ZFIT0036-DT_PGTO+6(2) WA_ZFIT0036-DT_PGTO+4(2) WA_ZFIT0036-DT_PGTO+0(4) INTO VDATA SEPARATED BY '.'.
* Criação do documento de Email
  doc_chng-obj_name = 'LOG_REL'.

* Assunto do Email
  CONCATENATE 'Pagamento de Imposto Venc .' vdata wg_cadlote-empresa INTO doc_chng-obj_descr SEPARATED BY space .

* Texto
  objtxt-line = 'O lote abaixo foi aprovado e encontra-se disponível para pagamento.'(028).
  APPEND objtxt.
  CLEAR objtxt.
  APPEND objtxt.

  objtxt-line = 'Clique no link anexo "GUIAS", para a relação individual das guias para pagamento'(029).
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = '--------------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  WRITE wg_cadlote-total TO ctotal CURRENCY 'USD'.

  CONDENSE ctotal NO-GAPS.
  CONCATENATE 'Empresa:'(019)   wg_cadlote-empresa ' Lote:'(030) wg_cadlote-lote ' R$' ctotal ' Venc.'  vdata INTO objtxt SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  "OBJPACK-TRANSF_BIN = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.

  DESCRIBE TABLE objbin LINES tab_lines.
  objhead = 'GUIAS.SAP'.
  APPEND objhead.

  it_shortcut_param-fieldname = 'P_BUKRS'.
  it_shortcut_param-fieldvalue = wg_cadlote-empresa+0(4).
  APPEND it_shortcut_param.

  it_shortcut_param-fieldname = 'P_LOTE'.
  it_shortcut_param-fieldvalue = wg_cadlote-lote+0(10).
  APPEND it_shortcut_param.

  CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
    EXPORTING
      recipient_user_id = it_mail-usuario
      transaction       = 'ZIMP57'
    IMPORTING
      content           = content
    TABLES
      shortcut_param    = it_shortcut_param.

  CLEAR : tab_lines, objbin.
  CONCATENATE content wa_objbin-line INTO wa_objbin-line.
  APPEND  wa_objbin TO objbin.


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
  APPEND objpack.

* Alimentar destinatários do email
  LOOP AT it_mail.
    reclist-receiver = it_mail-email.
    reclist-rec_type = 'U'.                    "Define email externo
    APPEND reclist.
  ENDLOOP.


* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      object_header              = objhead
      contents_bin               = objbin
      contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.
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

ENDFORM.                    "ENvia_email_Fim
*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM envia_email TABLES tg_estra  USING VALUE(wg_cadlote) TYPE ty_cadlote plinha  .

  FIELD-SYMBOLS: <fs_solix> TYPE solix.

* Objetos para enviar email
  DATA: objpack     LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE.
  DATA: objhead     LIKE solisti1   OCCURS  1 WITH HEADER LINE.
  DATA: objbin_ord  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_log  LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: objbin_ann  TYPE solisti1.
  DATA: objbin    LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin1   TYPE soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin LIKE LINE OF objbin.
  DATA: content_hex TYPE STANDARD TABLE OF solix WITH HEADER LINE.
  DATA: objtxt      LIKE solisti1   OCCURS 10 WITH HEADER LINE.
  DATA: reclist     LIKE somlreci1  OCCURS  5 WITH HEADER LINE.
  DATA: doc_chng    LIKE sodocchgi1.
  DATA: tab_lines   LIKE sy-tabix.
  DATA: l_anex      TYPE string.
  DATA: l_leng      TYPE i.
  DATA: l_arq       TYPE string.
  DATA: l_tam       TYPE i.
  DATA: l_tam_ord   TYPE i.
  DATA: l_tam_log   TYPE i.
  DATA: l_email(300) TYPE c.
  DATA: vlinha      TYPE i.
  DATA: vuser       TYPE sy-uname.
  DATA: it_shortcut_param LIKE zst_shortcut_par OCCURS 0 WITH HEADER LINE.
  DATA: content TYPE string.

*  ** Pass the required parameters and create the shortcut
  CLEAR it_shortcut_param.
  REFRESH it_shortcut_param.

  vlinha = plinha.
  ADD 1 TO vlinha.

  READ TABLE tg_estra INTO wa_estra INDEX vlinha .

  DATA: bsmtp_addr TYPE adr6-smtp_addr.

  SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
    FROM usr21
      INNER JOIN adr6
         ON  usr21~addrnumber = adr6~addrnumber
        AND usr21~persnumber = adr6~persnumber
            WHERE usr21~bname = wa_estra-aprovador.

* Criação do documento de Email
  doc_chng-obj_name = 'LOG_ESTRA'.

* Assunto do Email
  doc_chng-obj_descr = 'Aprovação INVOICE - Contas a Pagar'(031).

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP, o lote  abaixo.'(032).
  APPEND objtxt.
  CLEAR objtxt.
  APPEND objtxt.

  objtxt-line = 'Para aprovar clique no link "Estratégia" em anexo.'(033) .
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  DATA: ctotal(20),
        vdata(10).
*  READ TABLE IT_ZFIT0036 INTO WA_ZFIT0036 WITH KEY LOTE = WA_ESTRA-LOTE BINARY SEARCH.
*  CONCATENATE WA_ZFIT0036-DT_PGTO+6(2) WA_ZFIT0036-DT_PGTO+4(2) WA_ZFIT0036-DT_PGTO+0(4) INTO VDATA SEPARATED BY '.'.

  vdata = wg_cadlote-data.
  WRITE wg_cadlote-total TO ctotal CURRENCY 'USD'.

  CONDENSE ctotal NO-GAPS.
  CONCATENATE 'Empresa:'(019)   wg_cadlote-empresa ' Lote:'(030) wg_cadlote-lote ' R$' ctotal ' Venc.'  vdata INTO objtxt SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  "OBJPACK-TRANSF_BIN = 'X'.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.


  CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
    EXPORTING
      recipient_user_id = wa_estra-aprovador
      transaction       = 'ZFI0019'
    IMPORTING
      content           = content
    TABLES
      shortcut_param    = it_shortcut_param.

  CLEAR : tab_lines, objbin.
  CONCATENATE content wa_objbin-line INTO wa_objbin-line.
  APPEND  wa_objbin TO objbin.

  DESCRIBE TABLE objbin LINES tab_lines.
  objhead = 'ESTRATEGIA.SAP'.
  APPEND objhead.


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
  APPEND objpack.


* Alimentar destinatários do email
  IF bsmtp_addr IS INITIAL.
    MESSAGE 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.'(034) TYPE 'I'.
    EXIT.
  ENDIF.

  reclist-receiver = bsmtp_addr.
  reclist-rec_type = 'U'.                    "Define email externo
  APPEND reclist.

* Enviar email
  vuser = sy-uname.
  sy-uname = 'R3JOB'.
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      document_data              = doc_chng
      put_in_outbox              = 'X'
      commit_work                = 'X'
    TABLES
      packing_list               = objpack
      object_header              = objhead
      contents_bin               = objbin
      contents_txt               = objtxt      "CONTENTS_HEX = CONTENT_HEX
      receivers                  = reclist
    EXCEPTIONS
      too_many_receivers         = 1
      document_not_sent          = 2
      operation_no_authorization = 4
      OTHERS                     = 99.

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


ENDFORM.                    " ENVIA_EMAIL

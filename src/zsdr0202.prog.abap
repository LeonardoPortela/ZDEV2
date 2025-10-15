*&---------------------------------------------------------------------*
*& Report  zsdr0202
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zsdr0202.

*&--------------------------------------------------------------------&*
*& Types                                                              &*
*&--------------------------------------------------------------------&*

TYPES:  BEGIN OF ty_cad_ordem.
          INCLUDE TYPE zsd_checklist_est.
TYPES:  END OF ty_cad_ordem.

TYPES:  BEGIN OF ty_ordens.
          INCLUDE TYPE zsd_sim_checklist_est.
TYPES:    escvenda TYPE string.
TYPES:END OF ty_ordens.

TYPES:  BEGIN OF ty_estra.
          INCLUDE TYPE zsd_estrategia_checklist.
TYPES:  END OF ty_estra.

TYPES:  BEGIN OF ty_itens.
          INCLUDE TYPE zsd_itens_checklsit_est.
TYPES:    escvenda TYPE string.
TYPES   END OF ty_itens.

TYPES: BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END OF ty_editor.

*&--------------------------------------------------------------------&*
*& Declaração objetos ALV                                             &*
*&--------------------------------------------------------------------&*

CLASS: lcl_alv_toolbar   DEFINITION DEFERRED.

DATA: t_fieldcatalog       TYPE lvc_t_fcat,
      w_fieldcatalog       TYPE lvc_s_fcat,
      wa_layout            TYPE lvc_s_layo,
      wa_stable            TYPE lvc_s_stbl,
      wg_editor            TYPE ty_editor,
      dyfields             LIKE dynpread OCCURS 1 WITH HEADER LINE,
      ok-code              TYPE sy-ucomm,
      tg_selectedcell      TYPE lvc_t_cell,
      wg_selectedcell      TYPE lvc_s_cell,
      g_container          TYPE scrfname VALUE 'CC_ORDENS',
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
      obg_conteiner_itens  TYPE REF TO cl_gui_custom_container,
      g_cc_estra           TYPE scrfname VALUE 'CC_ESTRA',
      g_cc_itens           TYPE scrfname VALUE 'CC_ITENS',
      wa_style             TYPE lvc_s_styl,
      tl_function          TYPE ui_functions,
      wl_function          LIKE tl_function WITH HEADER LINE,
      style                TYPE lvc_t_styl  WITH HEADER LINE,
      style2               TYPE lvc_t_styl WITH HEADER LINE,
      ty_toolbar           TYPE stb_button,
      node_itab            LIKE node_str OCCURS 0,
      node                 LIKE node_str,
      container            TYPE REF TO cl_gui_custom_container,
      splitter_msg         TYPE REF TO cl_gui_easy_splitter_container,
      right                TYPE REF TO cl_gui_container,
      left                 TYPE REF TO cl_gui_container,
      editor               TYPE REF TO cl_gui_textedit,
      tree                 TYPE REF TO cl_gui_simple_tree,
      behaviour_left       TYPE REF TO cl_dragdrop,
      behaviour_right      TYPE REF TO cl_dragdrop,
      handle_tree          TYPE i,
      num_row              TYPE i VALUE 0,
      btn_rej(30).

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: wg_cad_ordem TYPE ty_cad_ordem,
      wa_estra     TYPE ty_estra,
      wa_itens     TYPE ty_itens,
      w_itens      TYPE ty_itens,
      tg_fields    TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_editor    TYPE TABLE OF ty_editor,
      tg_ordens    TYPE TABLE OF ty_ordens WITH HEADER LINE,
      tg_estra     TYPE TABLE OF ty_estra,
      tg_itens     TYPE TABLE OF ty_itens,
      it_itens     TYPE TABLE OF ty_itens,
      it_estra     TYPE TABLE OF ty_estra,
      tg_msg_ret   TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.

CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC3',
           END OF c_tab_strip_imp.

CONTROLS:  tab_strip_imp TYPE TABSTRIP.

DATA: BEGIN OF g_tab_strip_imp,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZSDR0069',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

*&--------------------------------------------------------------------&*
*& Declaração de variaveis                                            &*
*&--------------------------------------------------------------------&*

DATA: ok_code          LIKE sy-ucomm,
      wg_mensagem(30),
      wg_acao(30),
      vdt_apuracao(1),
      vmes_apuracao(1),
      vkokrs           TYPE tka02-kokrs,
      xclasse(1),
      xmodif(1),
      vvalor_ate       TYPE zsdt0142-valor_ate.

DATA: txtemp(10),
      txtlot(15),
      txtusu(15),
      txtmoe(15),
      txtval(15).

DATA: aux_empresa TYPE vbak-vkbur,
      aux_t001w   TYPE t001w,
      p_doc_sim   TYPE zsded003,
      p_emp       TYPE vbak-vkbur,
      p_bukrs     TYPE vbak-bukrs_vf.


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
           c_filtro(6)       TYPE c VALUE 'FILTRO',
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


CLASS lcl_drag_object DEFINITION.
  PUBLIC SECTION.
    DATA text TYPE mtreesnode-text.
ENDCLASS.                    "lcl_drag_object DEFINITION

CLASS lcl_dragdrop_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      node_double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.                    "lcl_dragdrop_receiver DEFINITION

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

    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.


  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.

    vvalor_ate = 0.

    IF e_row GT 0.
      CLEAR: wg_cad_ordem, tg_estra[], tg_itens[].

      READ TABLE tg_ordens INTO DATA(wl_ordens) INDEX e_row.

      CHECK sy-subrc = 0.

      wg_cad_ordem-doc_simulacao = wl_ordens-doc_simulacao.
      wg_cad_ordem-empresa   = wl_ordens-vkorg.
      wg_cad_ordem-usuario = wl_ordens-user_create.
      wg_cad_ordem-netwr   = wl_ordens-vlrtot.
      wg_cad_ordem-waerk   = wl_ordens-waerk.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '=ENT'
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.

      LOOP AT it_estra INTO DATA(wa_estra) WHERE doc_simulacao = wl_ordens-doc_simulacao.
        APPEND wa_estra TO tg_estra.
      ENDLOOP.

      LOOP AT it_itens INTO DATA(wa_itens) WHERE doc_simulacao = wl_ordens-doc_simulacao.

***********************************************************************
**120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
*        FREE: aux_t001w.
*        CLEAR: aux_t001w.
*
*        SELECT SINGLE werks,name1 FROM t001w
*          INTO CORRESPONDING FIELDS OF @aux_t001w
*          WHERE werks = @wa_itens-vkbur.
*        wa_itens-escvenda = |{ aux_t001w-werks } - { aux_t001w-name1 }|.
***********************************************************************


        APPEND wa_itens TO tg_itens.
      ENDLOOP.

    ENDIF.

    SORT tg_estra BY nivel.

    SORT tg_itens BY doc_simulacao checklistid checkid.

    " alv estrategia
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    " alv questionario(itens)
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_click2.

    CHECK e_row_id GT 0.

    IF e_column_id = 'VBELN'.
      READ TABLE tg_itens INTO DATA(wl_itens) INDEX e_row_id.
      CHECK sy-subrc = 0.
      SET PARAMETER ID 'AUN' FIELD wl_itens-doc_simulacao.
      CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.                    "ON_DOUBLE_CLICK2

  METHOD on_click.

    DATA: v_msg        TYPE char50,
          t_checklists TYPE TABLE OF ty_ordens,
          w_ordens     TYPE          ty_ordens,
          t_estra      TYPE TABLE OF ty_estra,
          w_estra      TYPE          ty_estra.

    CHECK e_row_id GT 0.

    CLEAR: w_ordens, t_checklists[], t_estra[].

    READ TABLE tg_estra INTO DATA(wl_estra_aux) INDEX e_row_id-index.

    CHECK sy-subrc = 0.

    CHECK wl_estra_aux-opcoes IS NOT INITIAL.

    READ TABLE tg_ordens INTO DATA(wl_ordens) WITH KEY doc_simulacao = wl_estra_aux-doc_simulacao.

    CHECK sy-subrc = 0.

    DATA(lt_estra) = tg_estra.
    SORT lt_estra BY   nivel DESCENDING.

    MOVE-CORRESPONDING wl_ordens TO w_ordens.

    APPEND w_ordens  TO t_checklists.

    LOOP AT tg_estra INTO DATA(wl_estra) WHERE doc_simulacao = wl_estra_aux-doc_simulacao..

      CLEAR: w_estra.

*      IF wl_estra-aprovador EQ sy-uname.
*
*        MOVE-CORRESPONDING wl_estra_aux TO w_estra.
*
*      ELSE.

      MOVE-CORRESPONDING wl_estra TO w_estra.

*      ENDIF.

      APPEND w_estra TO t_estra.

    ENDLOOP.

    DATA lt_text_aux TYPE TABLE OF txw_note.
    DATA lv_edit TYPE c.
    DATA tl_texto TYPE catsxt_longtext_itab.

    DATA: lt_trtexts TYPE trtexts,
          lw_trtexts TYPE trtext.
    DATA lv_texto(9000).

    LOOP AT tg_itens ASSIGNING FIELD-SYMBOL(<fs_textos>) WHERE tpcheck = 'T'.
      lv_texto = lv_texto && <fs_textos>-pergunta.
      CONDENSE lv_texto.
    ENDLOOP.

    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text  = lv_texto
        iv_len   = 60
      IMPORTING
        et_lines = lt_trtexts.

    LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

      APPEND INITIAL LINE TO lt_text_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).

      <fs_aux>-line = <fs_line>.

    ENDLOOP.

    lv_edit = abap_false.

    CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
      EXPORTING
        edit_mode = abap_false
      TABLES
        t_txwnote = lt_text_aux[].

    CHECK sy-ucomm = 'CONT'. "<- retorno da função

    CALL FUNCTION 'Z_CL_ESTRATEGIA_EXECUTAR'
      EXPORTING
        i_usuario    = sy-uname
      IMPORTING
        e_msg        = v_msg
      TABLES
        t_checklists = t_checklists
        t_estra      = t_estra.

    LOOP AT t_estra INTO w_estra WHERE aprovador EQ sy-uname.
      MOVE: w_estra-opcoes TO wl_estra-opcoes,
            w_estra-estado TO wl_estra-estado.
      MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
    ENDLOOP.

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_CLICK

  METHOD on_data_changed.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

  ENDMETHOD.                    "on_data_changed_finisheD

  METHOD on_double_click2.

    DATA lt_text_aux TYPE TABLE OF txw_note.
    DATA lv_edit TYPE c.
    DATA tl_texto TYPE catsxt_longtext_itab.

    DATA: lt_trtexts     TYPE trtexts,
          lw_trtexts     TYPE trtext,
          lv_texto(4000).

    CHECK e_row-index IS NOT INITIAL.

    CHECK e_column = 'RADIO_SIM'.

    READ TABLE tg_itens ASSIGNING FIELD-SYMBOL(<fs_item>) INDEX e_row-index.

    CHECK sy-subrc EQ 0.

    CHECK <fs_item>-tpcheck = 'T'.

    lv_texto = <fs_item>-pergunta.

    CALL FUNCTION 'TR_SPLIT_TEXT'
      EXPORTING
        iv_text  = lv_texto
        iv_len   = 60
      IMPORTING
        et_lines = lt_trtexts.

    LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

      APPEND INITIAL LINE TO lt_text_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).

      <fs_aux>-line = <fs_line>.

    ENDLOOP.

    lv_edit = abap_false.

    CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
      EXPORTING
        edit_mode = lv_edit
      TABLES
        t_txwnote = lt_text_aux[].

  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.

  ENDMETHOD.                    "drop_complete
ENDCLASS.                    "lcl_dragdrop_receiver IMPLEMENTATION


MODULE trata_fields OUTPUT.
  LOOP AT tg_fields.
    LOOP AT SCREEN.
      IF ( screen-name   EQ tg_fields-campo  ) OR
         ( screen-group1 EQ tg_fields-group1 ).
        screen-input     = tg_fields-value.
        screen-invisible = tg_fields-invisible.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT

MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.
  APPEND c_save TO fcode.
  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE cria_objetos OUTPUT.

  DATA: event     TYPE cntl_simple_event,
        events    TYPE cntl_simple_events,
        tl_filter TYPE lvc_t_filt,
        wl_filter TYPE lvc_s_filt.

  DATA: waref      TYPE REF TO data.
  IF g_custom_container IS INITIAL.
    txtemp = TEXT-l01.
    txtlot = TEXT-l02.
    txtusu = TEXT-l03.
    txtval = TEXT-l04.
    txtmoe = TEXT-l05.
    btn_rej = TEXT-b01.

    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_stable-row        = c_x.
    wa_layout-info_fname = 'COLOR'.

    wa_layout-cwidth_opt = abap_true.
    wa_layout-no_toolbar = c_x.
    wa_layout-stylefname = 'STYLE2'.
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

    PERFORM montar_layout_checklists.

    PERFORM f_config_function_alv USING 'GRID1'.

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

    PERFORM f_config_function_alv USING 'GRID2'.

    wa_layout-cwidth_opt = abap_false.
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
  IF obg_conteiner_itens IS INITIAL.
    CREATE OBJECT obg_conteiner_itens
      EXPORTING
        container_name = g_cc_itens.


    CREATE OBJECT grid3
      EXPORTING
        i_parent = obg_conteiner_itens.

    PERFORM montar_layout_itens.

    PERFORM f_config_function_alv USING 'GRID3'.

    wa_layout-cwidth_opt = abap_false.
    wa_layout-col_opt = abap_false.
    wa_layout-no_toolbar = space.
    wa_layout-stylefname = 'STYLE2'.
    wa_layout-grid_title = TEXT-t03 .
    wa_layout-no_toolbar = c_x.
    PERFORM montar_layout_itens.

    APPEND INITIAL LINE TO tl_filter ASSIGNING FIELD-SYMBOL(<fs_filter>).

    <fs_filter>-fieldname = 'HIDE'.
    <fs_filter>-sign = 'I'.
    <fs_filter>-option = 'EQ'.
    <fs_filter>-low = space.
    <fs_filter>-order = 1.

    SET HANDLER: lcl_event_handler=>on_double_click2 FOR grid3.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_itens[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.



  ELSE.
    PERFORM montar_layout_itens.

    CALL METHOD grid3->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    " CALL METHOD grid3->refresh_table_display( ).

  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT

FORM montar_layout_checklists.


  REFRESH t_fieldcatalog.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSD_SIM_CHECKLIST_EST'
      i_internal_tabname     = 'TG_ORDENS'
    CHANGING
      ct_fieldcat            = t_fieldcatalog
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

  "DELETE t_fieldcatalog WHERE fieldname = 'NIVEL'.

  READ TABLE t_fieldcatalog ASSIGNING FIELD-SYMBOL(<fs_field>)
    WITH KEY fieldname = 'TPSIM'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = 'TpVenda'.
    "<fs_field>-just = 'C'.
    "<fs_field>-dd_outlen = 000020.
  ENDIF.

  READ TABLE t_fieldcatalog ASSIGNING <fs_field>
    WITH KEY fieldname = 'TP_SIM_DESC'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = 'TpVenda'.
    "<fs_field>-just = 'C'.
    "<fs_field>-dd_outlen = 000020.
  ENDIF.

  READ TABLE t_fieldcatalog ASSIGNING <fs_field>
    WITH KEY fieldname = 'DESCR'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = <fs_field>-scrtext_l =
    <fs_field>-scrtext_m = <fs_field>-scrtext_s = 'Descrição'.
    "<fs_field>-just = 'C'.
    "<fs_field>-dd_outlen = 000020.
  ENDIF.



*
*  PERFORM montar_estrutura USING:
*        1 ' '                    ' '                'TG_ORDENS' 'STATUS'           ' '             '03' ' ' ' ' ' ',
*        2 'VBAK'                 'VKORG'            'TG_ORDENS' 'EMPRESA'          TEXT-a01        '10' ' ' ' ' ' ', "ajustado tamanho da coluna - SMC - #120237
*        3 'ESCVENDA'             'ESCVENDA'         'TG_ORDENS' 'ESCVENDA'         TEXT-a22        '20' ' ' ' ' ' ',
*        4 'VBAK'                 'VBELN'            'TG_ORDENS' 'VBELN'            TEXT-a02        '12' ' ' ' ' ' ',
*        5 'VBAK'                 'WAERK'            'TG_ORDENS' 'WAERK'            TEXT-a16        '05' ' ' ' ' ' ',
*        6 ''                     ''                 'TG_ORDENS' 'DS_CLIENTE'       TEXT-a24        '30' ' ' ' ' ' ',
*        7 'VBAP'                 'NETWR'            'TG_ORDENS' 'NETWR'            TEXT-a11        '13' ' ' ' ' ' ',
*        8 ''                     ''                 'TG_ORDENS' 'JUST_WORKFLOW'    TEXT-a25        '255' ' ' ' ' ' '.
*


ENDFORM.                    " MONTAR_LAYOUT

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

MODULE user_command_0100 INPUT.

  CASE ok-code.
    WHEN c_refresh.
      PERFORM f_refresh.
    WHEN c_filtro.
      PERFORM f_filtro. "PSA

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
    WHEN c_back.
      SET SCREEN 0.

    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT

FORM montar_layout_estra .
  REFRESH t_fieldcatalog.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSD_ESTRATEGIA_CHECKLIST'
      i_internal_tabname     = 'TG_ESTRA'
    CHANGING
      ct_fieldcat            = t_fieldcatalog
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE t_fieldcatalog WHERE fieldname = 'NIVEL'.

  READ TABLE t_fieldcatalog ASSIGNING FIELD-SYMBOL(<fs_field>)
    WITH KEY fieldname = 'ESTADO'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = TEXT-a09.
    <fs_field>-just = 'C'.
    <fs_field>-dd_outlen = 000020.
  ENDIF.

  READ TABLE t_fieldcatalog ASSIGNING <fs_field>
    WITH KEY fieldname = 'OPCOES'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = TEXT-a10.
    <fs_field>-just = 'C'.
    <fs_field>-dd_outlen = 000020.
    <fs_field>-hotspot = c_x.
  ENDIF.


*  PERFORM montar_estrutura USING:
*        1 'VBAP'           'BUKRS'      'TG_ESTRA' 'BUKRS'         TEXT-a06      '15' ' ' ' ' ' ',
*        1 'VBAP'           'NETWR'      'TG_ESTRA' 'VALOR_ATE'        TEXT-a07      '15' ' ' ' ' ' ',
*        1 'ZSDT0141'       'WAERS'      'TG_ESTRA' 'WAERK'            TEXT-a16      '05' ' ' ' ' ' ',
*        1 'ZSDT0141'       'APROVADOR'  'TG_ESTRA' 'APROVADOR'        TEXT-a08      '13' ' ' ' ' ' ',
*        1 ' '              ' '          'TG_ESTRA' 'ESTADO'           TEXT-a09      '05' ' ' ' ' ' ',
*        1 ' '              ' '          'TG_ESTRA' 'OPCOES'           TEXT-a10      '08' ' ' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_itens.

  REFRESH t_fieldcatalog.

  CLEAR w_itens.
  IF tg_itens[] IS NOT INITIAL.
    READ TABLE tg_itens INTO w_itens INDEX 1.
  ENDIF.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSD_ITENS_CHECKLSIT_EST'
      i_internal_tabname     = 'TG_ITENS'
    CHANGING
      ct_fieldcat            = t_fieldcatalog
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE t_fieldcatalog WHERE fieldname = 'BUKRS'.
  DELETE t_fieldcatalog WHERE fieldname = 'VKBUR'.
  DELETE t_fieldcatalog WHERE fieldname = 'CHECKID'.
  DELETE t_fieldcatalog WHERE fieldname = 'DOC_SIMULACAO'.
  DELETE t_fieldcatalog WHERE fieldname = 'CHECKLISTID'.
  DELETE t_fieldcatalog WHERE fieldname = 'FLAG_SIM'.
  DELETE t_fieldcatalog WHERE fieldname = 'FLAG_NAO'.
  DELETE t_fieldcatalog WHERE fieldname = 'INCONFORMIDADE'.
  DELETE t_fieldcatalog WHERE fieldname = 'TPCHECK'.
  DELETE t_fieldcatalog WHERE fieldname = 'TPCOND'.
  DELETE t_fieldcatalog WHERE fieldname = 'TPINCONF'.
  DELETE t_fieldcatalog WHERE fieldname = 'HIDE'.

  READ TABLE t_fieldcatalog ASSIGNING FIELD-SYMBOL(<fs_field>)
    WITH KEY fieldname = 'PERGUNTA'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = 'Item a ser Avaliado/Apontado'.
    <fs_field>-col_opt = abap_false.
    <fs_field>-dd_outlen = 0000150.
    <fs_field>-outputlen = 0000150.
  ENDIF.

  READ TABLE t_fieldcatalog ASSIGNING <fs_field>
    WITH KEY fieldname = 'TEXTO'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = 'Valores'.
    <fs_field>-col_opt = abap_false.
    <fs_field>-dd_outlen = 0000030.
    <fs_field>-outputlen = 30.
  ENDIF.

  READ TABLE t_fieldcatalog ASSIGNING <fs_field>
    WITH KEY fieldname = 'RADIO_SIM'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Sim'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE t_fieldcatalog ASSIGNING <fs_field>
    WITH KEY fieldname = 'RADIO_NAO'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Não'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE t_fieldcatalog ASSIGNING <fs_field>
  WITH KEY fieldname = 'ICON'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Inconformidade'.
    <fs_field>-dd_outlen = 00011.
    <fs_field>-outputlen = 11.
  ENDIF.


ENDFORM.                    " MONTAR_LAYOUT_DOCS

MODULE carrega_ordens OUTPUT.

  IF g_custom_container IS INITIAL.
    PERFORM atualiza_ordens.
  ENDIF.

ENDMODULE.                 " CARREGA_LOTES  OUTPUT

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

FORM atualiza_ordens.

  DATA: v_msg        TYPE char50,
        t_checklists TYPE TABLE OF zsd_sim_checklist_est,
        t_estra      TYPE TABLE OF zsd_estrategia_checklist,
        t_itens      TYPE TABLE OF zsd_itens_checklsit_est,
        t_itens_r    TYPE TABLE OF zsd_itens_checklsit_est_r.

  CLEAR: wg_cad_ordem.

  CALL FUNCTION 'Z_CL_ESTRATEGIA_LISTA'
    EXPORTING
      iv_usuario   = sy-uname
    IMPORTING
      e_msg        = v_msg
    TABLES
      t_checklists = t_checklists
      t_estra      = t_estra
      t_itens      = t_itens
      t_itens_r    = t_itens_r.

  REFRESH: tg_ordens, it_estra, it_itens.

  LOOP AT t_checklists INTO DATA(w_ordens).
    CLEAR: tg_ordens.
    MOVE-CORRESPONDING w_ordens TO tg_ordens.
**********************************************************************
*120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
    FREE: aux_t001w.
    CLEAR: aux_t001w.

    SELECT SINGLE vkbur FROM vbak
      INTO @aux_empresa
      WHERE vbeln = @w_ordens-doc_simulacao.
    SELECT SINGLE werks,name1 FROM t001w
      INTO CORRESPONDING FIELDS OF @aux_t001w
      WHERE werks = @aux_empresa.
    tg_ordens-escvenda = |{ aux_t001w-werks } - { aux_t001w-name1 }|. "120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
    tg_ordens-vkbur = aux_t001w-werks. "120237 CS2023000633 ZSDT0117 Exibir e adicionar filtro para filial
**********************************************************************
    APPEND tg_ordens.
  ENDLOOP.

  IF tg_ordens[] IS NOT INITIAL.

    LOOP AT t_estra INTO DATA(w_estra).
      CLEAR: wa_estra.
      MOVE-CORRESPONDING w_estra TO wa_estra.
      APPEND wa_estra TO it_estra.
    ENDLOOP.

    SORT it_estra BY doc_simulacao nivel.

    LOOP AT t_itens INTO DATA(w_itens).

      IF w_itens-tpcheck = 'P'." <- PERGUNTA
        CHECK w_itens-flag_sim IS NOT INITIAL OR w_itens-flag_nao IS NOT INITIAL.
      ELSE.

        IF w_itens-tpcheck = 'T'. "<-- TEXTO
          w_itens-checkid = 0. " para deixar os textos no cabeçalho
          w_itens-color = 'C500'.
        ELSE.

        ENDIF.

      ENDIF.

      CLEAR: wa_itens.

      MOVE-CORRESPONDING w_itens TO wa_itens.

      PERFORM f_check_hide TABLES t_itens t_itens_r USING wa_itens.

      IF w_itens-inconformidade = abap_true.
        wa_itens-icon = '@02@'.
      ELSE.
        wa_itens-icon = '@01@'.
      ENDIF.

      APPEND wa_itens TO it_itens.

    ENDLOOP.

    SORT it_itens BY checkid ASCENDING.

  ENDIF.

  "DELETE it_itens WHERE tpcheck = 'T'. " 26.05.2025

  IF g_custom_container IS NOT INITIAL.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF obg_conteiner_estra IS NOT INITIAL.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF obg_conteiner_itens IS NOT INITIAL.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDFORM.                    " Atualiza_lotes

FORM envia_email TABLES tg_estra USING VALUE(wg_cad_ordem) TYPE ty_cad_ordem plinha  .

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
  doc_chng-obj_descr = 'Aprovação Ordem Venda'.

* Texto
  objtxt-line = 'Está disponível para aprovação no sistema SAP, as Ordem de Venda abaixo.'.
  APPEND objtxt.
  CLEAR objtxt.
  APPEND objtxt.

  objtxt-line = 'Para aprovar clique no link "Estratégia" em anexo.' .
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  DATA: ctotal(20),
        vdata(10).

  WRITE wg_cad_ordem-netwr TO ctotal CURRENCY 'USD'.

  CONDENSE ctotal NO-GAPS.

*  SELECT SINGLE waerk
*     FROM vbak INTO @DATA(_waerk)
*    WHERE vbeln = @wg_cad_ordem-vbeln.
*
*  CONCATENATE 'Empresa:'     wg_cad_ordem-empresa
*              'Ordem Venda:' wg_cad_ordem-vbeln
*              'Moeda:'       _waerk
*              'Valor:'       ctotal
*         INTO objtxt SEPARATED BY space.

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
      transaction       = 'ZSDT0117'
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
    MESSAGE 'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.' TYPE 'I'.
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

ENDFORM.                    " ENVIA_EMAIL

FORM f_refresh.

  PERFORM atualiza_ordens.

  CLEAR: tg_estra[], tg_itens[].

  LOOP AT it_estra INTO wa_estra WHERE doc_simulacao =  wg_cad_ordem-doc_simulacao.
    APPEND wa_estra TO tg_estra.
  ENDLOOP.

  LOOP AT it_itens INTO wa_itens WHERE doc_simulacao  = wg_cad_ordem-doc_simulacao.

    APPEND wa_itens TO tg_itens.

  ENDLOOP.

  SORT tg_estra BY nivel.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.                    " F_REFRESH

FORM f_config_function_alv  USING p_grid.

  CASE p_grid.
    WHEN 'GRID1' OR 'GRID2' OR 'GRID3'.
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

      IF p_grid = 'GRID1'.
        wl_function = cl_gui_alv_grid=>mc_fc_check.
        APPEND wl_function TO tl_function.
        wl_function = cl_gui_alv_grid=>mc_fc_refresh.
        APPEND wl_function TO tl_function.
      ENDIF.

  ENDCASE.

ENDFORM.

FORM f_filtro.

  DATA: wa_fields LIKE sval,
        li_fields TYPE STANDARD TABLE OF sval.

  MOVE 'T001' TO wa_fields-tabname.
  MOVE 'BUKRS' TO wa_fields-fieldname.
  APPEND wa_fields TO li_fields.

  MOVE 'VBAK' TO wa_fields-tabname.
  MOVE 'VKBUR' TO wa_fields-fieldname.
  APPEND wa_fields TO li_fields.

  MOVE 'ZSDT0040' TO wa_fields-tabname.
  MOVE 'DOC_SIMULACAO' TO wa_fields-fieldname.
  APPEND wa_fields TO li_fields. " add more fields if required

  PERFORM f_refresh.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*     NO_VALUE_CHECK = ' '
      popup_title = 'Filtros'
*     START_COLUMN = '5'
*     START_ROW   = '5'
* IMPORTING
*     RETURNCODE  =
    TABLES
      fields      = li_fields
* EXCEPTIONS
*     ERROR_IN_FIELDS = 1
*     OTHERS      = 2
    .
  CLEAR: p_emp,p_doc_sim.

*  p_emp = VALUE #( FOR wa_doc IN li_fields WHERE ( tabname = 'VBAK' AND fieldname = 'VKBUR' ) ( option = 'EQ' sign = 'I' low = wa_doc-value ) ).
*    p_doc = VALUE #( FOR wa_doc IN li_fields WHERE ( tabname = 'BSEG' AND fieldname = 'BELNR' ) ( option = 'EQ' sign = 'I' low = wa_doc-value ) ).


  FIELD-SYMBOLS <li_fields> LIKE sval.

  CLEAR: p_emp,p_doc_sim,p_bukrs.

  LOOP AT li_fields ASSIGNING <li_fields> WHERE tabname = 'VBAK' AND fieldname = 'VKBUR'.
    p_emp = <li_fields>-value.
  ENDLOOP.

  LOOP AT li_fields ASSIGNING <li_fields> WHERE tabname = 'ZSDT0040' AND fieldname = 'DOC_SIMULACAO'.
    p_doc_sim = <li_fields>-value.
  ENDLOOP.
  LOOP AT li_fields ASSIGNING <li_fields> WHERE tabname = 'T001' AND fieldname = 'BUKRS'.
    p_bukrs = <li_fields>-value.
  ENDLOOP.

  IF p_doc_sim IS NOT INITIAL.
    DELETE tg_ordens WHERE doc_simulacao <> p_doc_sim.
  ENDIF.

  IF p_emp IS NOT INITIAL.
    DELETE tg_ordens WHERE vkbur <> p_emp.
  ENDIF.

  IF p_bukrs IS NOT INITIAL.
    DELETE tg_ordens WHERE vkorg <> p_bukrs.
  ENDIF.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_hide
*&---------------------------------------------------------------------*
FORM f_check_hide TABLES t_item STRUCTURE zsd_itens_checklsit_est
                         t_item_r STRUCTURE zsd_itens_checklsit_est_r
                   USING us_itens TYPE ty_itens.

  LOOP AT t_item_r ASSIGNING FIELD-SYMBOL(<fs_0380_r>)
    WHERE checklistid = us_itens-checklistid
      AND checkid = us_itens-checkid.

    READ TABLE t_item ASSIGNING FIELD-SYMBOL(<fs_alv_r>)
      WITH KEY checkid = <fs_0380_r>-checkid_r.

    CHECK sy-subrc EQ 0.

    IF us_itens-tpcond = 'S' AND us_itens-flag_sim = abap_true.

      <fs_alv_r>-hide = abap_false.

    ELSEIF us_itens-tpcond = 'N' AND us_itens-flag_nao = abap_true.
      <fs_alv_r>-hide = abap_false.
    ELSE.
      <fs_alv_r>-hide = abap_true.
    ENDIF.

  ENDLOOP.

ENDFORM.

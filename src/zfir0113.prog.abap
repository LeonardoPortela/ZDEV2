*&---------------------------------------------------------------------*
*& Report  zfir0113
*&
*&---------------------------------------------------------------------*

REPORT  zfir0113.

DATA: it_lotes    TYPE TABLE OF zelotes_gfixo,
      wa_lotes    TYPE zelotes_gfixo,
      it_zfit0217 TYPE TABLE OF zfit0217,
      wa_zfit0217 TYPE zfit0217,
      it_docs     TYPE TABLE OF zfit0217, "zedocs_gfixo,
      wa_docs     TYPE zfit0217, "zedocs_gfixo,
      it_cadlote  TYPE TABLE OF zecadlote_gfixo,
      wa_cadlote  TYPE zecadlote_gfixo,
      it_estra    TYPE TABLE OF zeestrategia_gfixo,
      wa_estra    TYPE zeestrategia_gfixo,
      v_bukrs     TYPE bukrs,
      v_werks     TYPE werks_d,
      v_lote      TYPE zlote_num,
      v_lote_ant  TYPE zlote_num, "BUG - 174198 - CBRAND
      v_aprovador TYPE usnam,
      v_monat     TYPE zelotes_gfixo-monat, "BUG - 177783 - CBRAND
      v_gjahr     TYPE zelotes_gfixo-gjahr, "BUG - 177783 - CBRAND
      v_sydat     TYPE sy-datum, "BUG - 177783 - CBRAND
      v_dmbtr     TYPE decfloat34. "dmbtr.


DATA: c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager.

DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl.


DATA: g_container         TYPE scrfname VALUE 'CC_LOTES',
      g_custom_container  TYPE REF TO cl_gui_custom_container,
      container_1         TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2         TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter            TYPE REF TO cl_gui_splitter_container,
      grid1               TYPE REF TO cl_gui_alv_grid,
      grid2               TYPE REF TO cl_gui_alv_grid,
      grid3               TYPE REF TO cl_gui_alv_grid,
      obg_conteiner_estra TYPE REF TO cl_gui_custom_container,
      obg_conteiner_docs  TYPE REF TO cl_gui_custom_container,
      g_cc_estra          TYPE scrfname VALUE 'CC_ESTRA',
      g_cc_docs           TYPE scrfname VALUE 'CC_DOCS',
      wa_style            TYPE lvc_s_styl,
      it_dynpfields       TYPE STANDARD TABLE OF dynpread,
      wa_dynpfields       TYPE dynpread,
      style               TYPE lvc_t_styl  WITH HEADER LINE,
      style2              TYPE lvc_t_styl WITH HEADER LINE.

DATA: ok-code         TYPE sy-ucomm,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      wg_acao(30),
      ty_toolbar      TYPE stb_button,
      wg_sub01        TYPE sy-dynnr VALUE '0140'.



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
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE',
           txtemp(8)         TYPE c VALUE 'Empresa:',
           txtfil(7)         TYPE c VALUE 'Filial:',
           txtlot(5)         TYPE c VALUE 'Lote:',
           txtapr(10)        TYPE c VALUE 'Aprovador:',
           txtval(12)        TYPE c VALUE 'Valor Total:',
           btn_rej(8)        TYPE c VALUE 'Rejeitar'.

CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC3',
           END OF c_tab_strip_imp.

DATA: BEGIN OF g_tab_strip_imp,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZFIR0113',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

CONTROLS:  tab_strip_imp TYPE TABSTRIP.

CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.



    CLASS-METHODS:
      on_click FOR EVENT hotspot_click  OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

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
  ENDMETHOD.
  METHOD handle_user_command.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.
  METHOD on_click.
    DATA: v_msg    TYPE char50,
          t_lotes  TYPE TABLE OF zfi_lotes_imp,
          w_lotes  TYPE          zfi_lotes_imp,
          wl_estra LIKE LINE OF it_estra,
          t_estra  TYPE TABLE OF zeestrategia_gfixo,
          w_estra  TYPE          zeestrategia_gfixo,
          westra   LIKE LINE OF it_estra,
          t_docs   TYPE TABLE OF zgl_docs_imp,
          w_docs   TYPE          zgl_docs_imp,
          "wl_lotes LIKE LINE OF it_lotes,
          lv_stop  TYPE c.

*** BUG -  177783 - Inicio - CBRAND
    DATA: lva_dtlote TYPE sy-datum.
    CONCATENATE v_gjahr v_monat '01' INTO lva_dtlote.
*** BUG -  177783 - Fim - CBRAND

    CLEAR lv_stop.

    IF e_row_id GT 0.

      CASE e_column_id.
        WHEN 'OPCOES'.
          CLEAR: wa_estra.
          READ TABLE it_estra INTO wa_estra WITH KEY  aprovador = sy-uname.
          IF sy-subrc = 0.
*** BUG -  177783 - Inicio - CBRAND
            IF lva_dtlote >= v_sydat.
              MESSAGE | 'Esse lançamento só pode ser aprovado no mês' | && v_monat  && sy-msgv2 TYPE 'I' DISPLAY LIKE 'E'.
              EXIT.
            ELSE.
*** BUG -  177783 - Fim - CBRAND
              UPDATE zfit0217 SET status = 'A' WHERE werks = v_werks AND lote = v_lote.
              COMMIT WORK.
              PERFORM grava_zib.
              PERFORM f_refresh .
            ENDIF.
          ENDIF.
        WHEN ''.
        WHEN OTHERS.
      ENDCASE.


    ENDIF.
  ENDMETHOD.

  METHOD on_double_click.
    IF e_row GT 0.
      wg_sub01 = '0140'.
      CLEAR: wa_cadlote.
      REFRESH it_estra.
      REFRESH it_docs.

      it_docs[] = CORRESPONDING #( it_zfit0217 ).

      READ TABLE it_lotes INTO wa_lotes INDEX e_row.

      DELETE it_docs[] WHERE lote <> wa_lotes-lote.

      SELECT SINGLE bukrs, cajo_number AS werks, val_ini, val_fim, uname AS aprovador FROM zfit0120
            WHERE cajo_number = @wa_lotes-werks AND uname = @sy-uname INTO CORRESPONDING FIELDS OF @wa_estra.

      IF sy-subrc <> 0.
        MESSAGE 'Não é possivel trazer os dados para aprovação pois não foi encontrado o Cadastro!' TYPE 'I'.
        EXIT.
      ENDIF.

      wa_estra-opcoes       = icon_set_state .

      APPEND wa_estra TO it_estra[].


      MOVE-CORRESPONDING wa_estra TO wa_cadlote.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '=ENT'
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.

    ENDIF.

    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    v_bukrs = wa_lotes-bukrs.
    v_werks = wa_lotes-werks.
    v_lote  = wa_lotes-lote.
    v_aprovador = wa_estra-aprovador.
    v_dmbtr = wa_lotes-dmbtr.
*** BUG - 177883 - CBRAND - Inicio
    v_monat = wa_lotes-monat.
    v_gjahr = wa_lotes-gjahr.
*** BUG - 177883 - CBRAND - Fim
  ENDMETHOD.

ENDCLASS.

CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.

  ENDMETHOD.
ENDCLASS.

MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.
  REFRESH: fcode.
  APPEND c_save TO fcode.
  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '0100'.
ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE cria_objetos OUTPUT.
  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        waref       TYPE REF TO data.

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

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_lotes[].

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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
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
        it_outtab            = it_estra[].

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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
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
        it_outtab            = it_docs[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

*    SET HANDLER:
*                  lcl_event_handler=>on_click2 FOR grid3.

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

  IF p_field EQ 'OPCOES'.
    w_fieldcatalog-hotspot = c_x.
    w_fieldcatalog-just = 'C'.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.

MODULE user_command_0100 INPUT.
  CASE ok-code.
    WHEN c_refresh.
      PERFORM f_refresh.
    WHEN 'REJ'.
      CLEAR: wa_estra.
      READ TABLE it_estra INTO wa_estra WITH KEY  aprovador = sy-uname.
      IF sy-subrc = 0.
        UPDATE zfit0217 SET status = 'R' WHERE werks = v_werks AND lote = v_lote.
        COMMIT WORK.
        PERFORM f_refresh .
      ENDIF.
  ENDCASE.
ENDMODULE.

MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.

FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        01 ' '              ' '                'IT_LOTES' 'BUKRS'    'Empresa'   '06' ' ' ' ' ' ',
        02 ' '              ' '                'IT_LOTES' 'WERKS'    'Filial'    '06' ' ' ' ' ' ',
        03 ' '              ' '                'IT_LOTES' 'NOME'     'Nome'      '30' ' ' ' ' ' ',
        04 ' '              ' '                'IT_LOTES' 'MONAT'    'Mês'       '04' ' ' ' ' ' ',
        05 ' '              ' '                'IT_LOTES' 'GJAHR'    'Ano'       '04' ' ' ' ' ' ',
        06 ' '              ' '                'IT_LOTES' 'LOTE'     'Lote'      '12' ' ' ' ' ' ',
        07 ' '              ' '                'IT_LOTES' 'DMBTR'    'Valor'     '12' ' ' ' ' ' ',
        08 ' '              ' '                'IT_LOTES' 'WAERS'    'Moeda'     '05' ' ' ' ' ' ',
        09 ' '              ' '                'IT_LOTES' 'D_C'      'D\C'       '03' ' ' ' ' ' '.

ENDFORM.

FORM montar_layout_estra .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        01 ' '           ' '         'IT_ESTRA' 'BUKRS'         'Empresa'        '10' ' ' ' ' ' ',
        02 ' '           ' '         'IT_ESTRA' 'WERKS'         'Filial'         '10' ' ' ' ' ' ',
        03 ' '           ' '         'IT_ESTRA' 'APROVADOR'     'Aprovador'      '10' ' ' ' ' ' ',
        04 ' '           ' '         'IT_ESTRA' 'VAL_INI'       'Data Inicio'    '10' ' ' ' ' ' ',
        05 ' '           ' '         'IT_ESTRA' 'VAL_FIM'       'Data Fim'       '10' ' ' ' ' ' ',
        06 ' '           ' '         'IT_ESTRA' 'OPCOES'        'Opções Liber.'  '08' ' ' ' ' ' '.

ENDFORM.             " MONTAR_LAYOUT_ESTRA

FORM montar_layout_docs .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        01 ' '           ' '       'IT_DOCS'  'LOTE'                    'Lote'                          '10' ' ' ' ' ' ',
        02 ' '           ' '       'IT_DOCS'  'DT_DOC'                  'Dt.Documento'                  '10' ' ' ' ' ' ',
        03 ' '           ' '       'IT_DOCS'  'SAKNR'                   'Conta Razão'                   '10' ' ' ' ' ' ',
        04 ' '           ' '       'IT_DOCS'  'TXT50'                   'Descrição Conta'               '10' ' ' ' ' ' ',
        05 ' '           ' '       'IT_DOCS'  'KOSTL'                   'Centro Custo'                  '10' ' ' ' ' ' ',
        06 ' '           ' '       'IT_DOCS'  'AUFNR'                   'Ordem'                         '10' ' ' ' ' ' ',
        07 ' '           ' '       'IT_DOCS'  'DESC_FORNEC'             'Fornecedor (Descrição)'        '10' ' ' ' ' ' ',
        08 ' '           ' '       'IT_DOCS'  'DESC_DESP_REC'           'Descrição Despesa\Receita'    '10' ' ' ' ' ' ',
        09 ' '           ' '       'IT_DOCS'  'XBLNR'                   'Nro.Documento'                 '10' ' ' ' ' ' ',
        10 ' '           ' '       'IT_DOCS'  'DMBTR'                   'Valor BRL'                     '10' ' ' ' ' ' ',
        11 ' '           ' '       'IT_DOCS'  'ESTORNO'                 'Estorno'                       '08' ' ' ' ' ' ',
        12 ' '           ' '       'IT_DOCS'  'D_C'                     'D\C'                           '03' ' ' ' ' ' ',
        13 ' '           ' '       'IT_DOCS'  'USNAM'                   'usuario'                       '10' ' ' ' ' ' ',
        14 ' '           ' '       'IT_DOCS'  'DT_ENTRADA'              'Dt.Entrada'                    '10' ' ' ' ' ' ',
        15 ' '           ' '       'IT_DOCS'  'HR_ENTRADA'              'Hora'                          '06' ' ' ' ' ' '.

ENDFORM.

MODULE carrega_lotes OUTPUT.
  IF g_custom_container IS INITIAL.
    REFRESH: it_lotes,it_estra,it_docs.
    PERFORM get_lotes.
  ENDIF.
ENDMODULE.

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

ENDFORM.

FORM grava_zib.

  DATA: it_zib_contabil TYPE STANDARD TABLE OF zib_contabil INITIAL SIZE 0,
        wa_zib_contabil TYPE zib_contabil,
        l_hkont         TYPE hkont.

  CLEAR: wa_docs,it_zib_contabil.
  DATA: _seq11(11)   TYPE n,
        _seq10(10)   TYPE n,
        _seq_item    TYPE num6,
        _ano(4)      TYPE n,
        _obj_key     TYPE zib_contabil-obj_key,
        _obj_key_ant TYPE zib_contabil-obj_key, "BUG - 174198
        _hj          TYPE char10,
        _mes         TYPE char10,
        _dt_lanc     TYPE char10.

  CLEAR: _ano,_obj_key,_seq10,_seq11,_seq_item,_hj.
  _ano = sy-datum+0(4).
  _hj = |{ sy-datum+6(2) }.{ sy-datum+4(2) }.{ sy-datum+0(4) }|.
  _mes =  |{ sy-datum+4(2) }|.

  SELECT SINGLE MAX( obj_key ) AS  obj_key
    FROM zib_contabil
    WHERE substring( obj_key,1,5 ) = 'GFIXO'
    INTO @DATA(l_obj_key).

  IF l_obj_key IS NOT INITIAL.
    _seq11 = l_obj_key+5(11).
    _seq11 = _seq11 + 1.
    SELECT SINGLE MAX( seqitem ) AS seqitem FROM zib_contabil WHERE obj_key = @l_obj_key INTO @_seq_item.
  ELSE.
    _seq11 = 1.
    _seq_item = 0.
  ENDIF.

  SELECT *
    FROM t001
    INTO TABLE @DATA(lt_t001)
    FOR ALL ENTRIES IN @it_docs
    WHERE bukrs EQ @it_docs-bukrs.

  LOOP AT it_docs[] INTO wa_docs WHERE lote = v_lote.
*** BUG - 174198 - CBRAND - Inicio
    IF wa_docs-obj_key <> _obj_key_ant.
      _seq_item = 0.
    ENDIF.

    _obj_key_ant  = wa_docs-obj_key.
** _seq_item = 0.
*** BUG - 174198 - CBRAND - Fim

    READ TABLE lt_t001 INTO DATA(ls_t001) WITH KEY bukrs = wa_docs-bukrs.

    "FF #180295 - inicio // Pegando o utlimo sequencial que foi inserido na it_zib_contabil.
    DATA(lv_max_seq) = REDUCE i( INIT max = 0
                                 FOR ls_row IN it_zib_contabil
                                 NEXT max = COND i( WHEN ls_row-seqitem > max THEN ls_row-seqitem ELSE max ) ).

    _seq_item = lv_max_seq.
    "FF #180295 - fim


    _seq_item = _seq_item + 1.
    wa_zib_contabil-seqitem       = _seq_item.
    IF wa_docs-xblnr IS NOT INITIAL.
*** Tratar estorno - CBRAND
      _seq11 = wa_docs-xblnr+5(11).
      _seq11 = _seq11 + 1.
*** Tratar estorno - CBRAND
      CONDENSE _seq11 NO-GAPS.
      _seq10 = _seq11.
      _obj_key = |GFIXO{ _seq10 }{ _ano }R|. "Reversão
      wa_zib_contabil-xblnr         = wa_docs-xblnr.
    ELSE.
      _obj_key = |GFIXO{ _seq11 }{ _ano }|. "Processamento
    ENDIF.
    wa_zib_contabil-mandt         = sy-mandt.
    wa_zib_contabil-obj_key       = _obj_key.

    IF wa_docs-d_c = 'D'.
      wa_zib_contabil-bschl       = '40'.
    ELSEIF wa_docs-d_c = 'C'.
      wa_zib_contabil-bschl       = '50'.
    ENDIF.
    wa_zib_contabil-bukrs         = wa_docs-bukrs.
    wa_zib_contabil-gsber         = wa_docs-werks.
    wa_zib_contabil-interface     = 0.
    CLEAR: _dt_lanc.
    CONDENSE wa_docs-dt_doc NO-GAPS.
    _dt_lanc = |{ wa_docs-dt_doc+6(2) }.{ wa_docs-dt_doc+4(2) }.{ wa_docs-dt_doc+0(4) }|.
    wa_zib_contabil-budat         = _hj."
    wa_zib_contabil-bldat         = _dt_lanc. "_HJ. "// Valor recebe o valor da WA_DOCS-DT_DOC linha 817 BUG-167461 wbarbosa 27-02-2025
    wa_zib_contabil-gjahr         = wa_docs-gjahr.
    wa_zib_contabil-monat         = wa_docs-monat.
    wa_zib_contabil-blart         = 'LM'.
    wa_zib_contabil-rg_atualizado = 'N'.
    wa_zib_contabil-sgtxt         = wa_docs-desc_desp_rec.
    wa_zib_contabil-zuonr         = |{ wa_docs-bukrs } { wa_docs-werks }|.
    wa_zib_contabil-bupla         = wa_docs-werks.
    wa_zib_contabil-waers         = wa_docs-waers.
    wa_zib_contabil-wrbtr         = wa_docs-dmbtr.
    wa_zib_contabil-hkont         = wa_docs-saknr.
    wa_zib_contabil-bktxt         = 'GFFIXO'.
    wa_zib_contabil-kostl         = wa_docs-kostl.

    "Linha do lançamento
    APPEND wa_zib_contabil TO it_zib_contabil.
    UPDATE zfit0217 SET obj_key = _obj_key dt_lanc = sy-datum WHERE werks = v_werks AND lote = v_lote AND seqitem = wa_docs-seqitem.
    COMMIT WORK.
**********************************************************************    "Linha da perna 111002
    "Linha da perna 111002
    _seq_item = _seq_item + 1.
    wa_zib_contabil-seqitem       = _seq_item.

    IF wa_zib_contabil-bschl = 40.
      wa_zib_contabil-bschl = 50.
    ELSEIF wa_zib_contabil-bschl = 50.
      wa_zib_contabil-bschl = 40.
    ENDIF.
    CLEAR:l_hkont.
    l_hkont = '111002'.
    wa_zib_contabil-hkont = l_hkont.
    APPEND wa_zib_contabil TO it_zib_contabil.
**********************************************************************
    IF wa_docs-saknr NE '0000111002'.
      CONTINUE.
    ENDIF.

    FREE it_zib_contabil.
    _seq_item = 0.

    ADD 1 TO _seq_item.

    APPEND
    VALUE #(  mandt         = sy-mandt                      "FF #180295
              obj_key       = _obj_key
              seqitem       = _seq_item
              bschl         = SWITCH #( wa_docs-d_c WHEN 'D' THEN '50' WHEN 'C' THEN '40')
              gsber         = wa_docs-werks
              bukrs         = wa_docs-bukrs
              interface     = 0
              bktxt         = 'GFFIXO'
              budat         = _hj "// Valor recebe o valor da WA_DOCS-DT_DOC linha 817 BUG-167461 wbarbosa 19-02-2025
              bldat         = _dt_lanc "// Valor recebe o valor da WA_DOCS-DT_DOC linha 817 BUG-167461 wbarbosa 19-02-2025
              gjahr         = _ano
              monat         = _mes
              blart         = 'LM'
              xblnr         = wa_docs-xblnr
              hkont         = wa_docs-saknr
              wrbtr         = wa_docs-dmbtr
              waers         = ls_t001-waers
              bupla         = wa_docs-werks
              zuonr         = |GFFIXO FILIAL { wa_docs-werks }|
              sgtxt         = wa_docs-desc_desp_rec
              rg_atualizado = 'N'
           ) TO it_zib_contabil.

    ADD 1 TO _seq_item.

    APPEND
    VALUE #(  mandt         = sy-mandt                      "FF #180295
              obj_key       = _obj_key
              seqitem       = _seq_item
              bschl         = SWITCH #( wa_docs-d_c WHEN 'D' THEN '25' WHEN 'C' THEN '35')
              gsber         = wa_docs-werks
              bukrs         = wa_docs-bukrs
              interface     = 0
              bktxt         = 'GFFIXO'
              budat         = _hj "// Valor recebe o valor da WA_DOCS-DT_DOC linha 817 BUG-167461 wbarbosa 19-02-2025
              bldat         = _dt_lanc "// Valor recebe o valor da WA_DOCS-DT_DOC linha 817 BUG-167461 wbarbosa 19-02-2025
              gjahr         = _ano
              monat         = _mes
              blart         = 'LM'
              xblnr         = wa_docs-xblnr
              hkont         = wa_docs-werks
              wrbtr         = wa_docs-dmbtr
              waers         = ls_t001-waers
              bupla         = wa_docs-werks
              zuonr         = |GFFIXO FILIAL { wa_docs-werks }|
              sgtxt         = wa_docs-desc_desp_rec
              rg_atualizado = 'N'
           ) TO it_zib_contabil.

    CLEAR: _obj_key, _seq_item, wa_docs, ls_t001.

  ENDLOOP.

  CLEAR: wa_zib_contabil,wa_docs.
  MODIFY zib_contabil FROM TABLE it_zib_contabil.
  COMMIT WORK.

  FREE it_zib_contabil.

  CLEAR: v_aprovador, v_bukrs, v_dmbtr, v_lote, v_werks.

ENDFORM.

FORM envia_email_fim.

ENDFORM.

FORM envia_email TABLES it_estra USING VALUE(wa_cadlote) TYPE zecadlote_gfixo plinha  .

ENDFORM.

*----------------------------------------------------------------------*
FORM f_refresh .
  REFRESH: it_lotes,it_estra,it_docs.
  PERFORM get_lotes.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = wa_stable.
ENDFORM.

FORM checa_doc CHANGING p_stop.

ENDFORM.

FORM get_lotes .
  DATA: vlr_c     TYPE zelotes_gfixo-dmbtr,
        vlr_d     TYPE zelotes_gfixo-dmbtr,
        lva_sydat TYPE sy-datum.

*** BUG -  177783 - Inicio - CBRAND
  CLEAR: v_sydat.
  CONCATENATE  sy-datum+0(4) sy-datum+4(2) '01' INTO  lva_sydat.
  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = lva_sydat
      days      = '00'
      months    = '01'
      signum    = '+'
      years     = '00'
    IMPORTING
      calc_date = v_sydat.
*** BUG -  177783 - Fim - CBRAND

  SELECT DISTINCT
    'I' AS sign,
   'EQ' AS option,
   cajo_number AS low,
   cajo_number AS high
     FROM zfit0120
    WHERE uname = @sy-uname
    AND val_ini <= @sy-datum
    AND val_fim >= @sy-datum
    ORDER BY cajo_number
    INTO TABLE @DATA(lr_zfit0120).
  REFRESH: it_lotes, it_estra, it_docs.
  SELECT * FROM zfit0217 WHERE werks IN @lr_zfit0120 AND status = 'L' INTO TABLE @it_zfit0217.
  SORT it_zfit0217 BY lote ASCENDING d_c ASCENDING.
  LOOP AT it_zfit0217 INTO DATA(_gp_zfit0217) GROUP BY _gp_zfit0217-lote.
    wa_lotes-bukrs = _gp_zfit0217-bukrs.
    wa_lotes-werks = _gp_zfit0217-werks.
    wa_lotes-gjahr = _gp_zfit0217-gjahr.
    wa_lotes-monat = _gp_zfit0217-monat.
    wa_lotes-lote = _gp_zfit0217-lote.
    CLEAR: vlr_c, vlr_d.
    LOOP AT it_zfit0217 INTO DATA(_calc_c_zfit0217) WHERE d_c = 'C' AND lote = wa_lotes-lote.
      vlr_c = vlr_c + _calc_c_zfit0217-dmbtr.
    ENDLOOP.
    LOOP AT it_zfit0217 INTO DATA(_calc_d_zfit0217) WHERE d_c = 'D' AND lote = wa_lotes-lote.
      vlr_d = vlr_d + _calc_d_zfit0217-dmbtr.
    ENDLOOP.
    wa_lotes-dmbtr = vlr_c + ( vlr_d * -1 ).
    IF wa_lotes-dmbtr > 0.
      wa_lotes-d_c = 'C'.
    ELSEIF wa_lotes-dmbtr < 0.
      wa_lotes-d_c = 'D'.
    ENDIF.
    SELECT SINGLE name1 FROM t001w WHERE werks = @wa_lotes-werks INTO @wa_lotes-nome.
    wa_lotes-waers = _gp_zfit0217-waers.
    APPEND wa_lotes TO it_lotes[].
  ENDLOOP.
ENDFORM.

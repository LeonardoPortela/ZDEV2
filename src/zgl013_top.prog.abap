*&---------------------------------------------------------------------*
*& Include ZGL013_TOP                                        PoolMóds.        ZGL013
*&
*&---------------------------------------------------------------------*
PROGRAM  zgl013.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TS_100'
CONSTANTS: BEGIN OF c_ts_100,
             tab1 LIKE sy-ucomm VALUE 'TS_100_FC1',
             tab2 LIKE sy-ucomm VALUE 'TS_100_FC2',
           END OF c_ts_100.
*&SPWIZARD: DATA FOR TABSTRIP 'TS_100'
CONTROLS:  ts_100 TYPE TABSTRIP.

DATA: BEGIN OF g_ts_100,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZGL013',
        pressed_tab LIKE sy-ucomm VALUE c_ts_100-tab1,
      END OF g_ts_100.


*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_glt031,
         tp_lcto         TYPE zglt031-tp_lcto,
         descricao       TYPE zglt031-descricao,
         dpto_resp       TYPE zglt031-dpto_resp,
         bukrs           TYPE zglt031-bukrs,
         moeda_doc       TYPE zglt031-moeda_doc,
         st_lc_moeda     TYPE zglt031-st_lc_moeda,
         moeda_interna   TYPE zglt031-moeda_interna,
         moeda_int_hist  TYPE zglt031-moeda_int_hist,
         moeda_forte     TYPE zglt031-moeda_forte,
         moeda_ft_hist   TYPE zglt031-moeda_ft_hist,
         moeda_grupo     TYPE zglt031-moeda_grupo,
         moeda_gp_hist   TYPE zglt031-moeda_gp_hist,
         blart           TYPE zglt031-blart,
         ltext           TYPE t003t-ltext,
         xblnr           TYPE zglt031-xblnr,
         bktxt           TYPE zglt031-bktxt,
         dt_doc          TYPE zglt031-dt_doc,
         dt_doc_ult_mes  TYPE zglt031-dt_doc_ult_mes,
         dt_lcto         TYPE zglt031-dt_lcto,
         dt_lcto_ult_mes TYPE zglt031-dt_lcto_ult_mes,
         prov_est        TYPE zglt031-prov_est,
         ref_lcto        TYPE zglt031-ref_lcto,
         usnam           TYPE zglt031-usnam,
         dt_entrada      TYPE zglt031-dt_entrada,
         hr_entrada      TYPE zglt031-hr_entrada,
         loekz           TYPE zglt031-loekz,
         st_ap_fiscal    TYPE zglt031-st_ap_fiscal,
         st_conc_banc    TYPE zglt031-st_conc_banc,
         st_trans_banc   TYPE zglt031-st_trans_banc,
         st_agrupa       TYPE zglt031-st_agrupa,
         st_aprova       TYPE zglt031-st_aprova,
       END OF ty_glt031,

       BEGIN OF ty_glt032,
         mark(1),
         tp_lcto  TYPE zglt032-tp_lcto,
         bschl    TYPE zglt032-bschl,
         hkont    TYPE zglt032-hkont,
         buzei    TYPE zglt032-buzei,
         descr    TYPE char50,
         umskz    TYPE zglt032-umskz,
         anbwa    TYPE zglt032-anbwa,
         bewar    TYPE zglt032-bewar,
         vbund    TYPE zglt032-vbund,
         kostl    TYPE zglt032-kostl,
         prctr    TYPE zglt032-prctr,
         aufnr    TYPE zglt032-aufnr,
         matnr    TYPE zglt032-matnr,
         matnr_fi TYPE zglt032-matnr_fi,
         zuonr    TYPE zglt032-zuonr,
         sgtxt    TYPE zglt032-sgtxt,
         tax_code TYPE zglt032-tax_code,
         akont    TYPE lfb1-akont,
         descr_a  TYPE char30,
       END OF ty_glt032,

       BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END OF ty_editor.
*
*     BEGIN OF TY_ZGLT076,
*         TP_LCTO     TYPE  ZGLT076-TP_LCTO  ,
*         FNAME       TYPE  ZGLT076-FNAME    ,
*         CHANGENR    TYPE  ZGLT076-CHANGENR ,
*         BUZEI       TYPE  ZGLT076-BUZEI    ,
*         CHANGEID    TYPE  ZGLT076-CHANGEID ,
*         FTEXT       TYPE  ZGLT076-FTEXT    ,
*         VALUE_OLD   TYPE  ZGLT076-VALUE_OLD,
*         VALUE_NEW   TYPE  ZGLT076-VALUE_NEW,
*         USERNAME    TYPE  ZGLT076-USERNAME ,
*         DATA        TYPE  ZGLT076-DATA     ,
*         HORA        TYPE  ZGLT076-HORA     ,
*     END OF TY_ZGLT076.

*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: ok-code         TYPE sy-ucomm,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      x_field(30),
*      BTN_VISAO(25)         VALUE '@KU@ Visão de Razão',
      btn_visao(25),
      x_visao(1).

** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl,

      it_zglt031     TYPE TABLE OF zglt031,
      wa_zglt031     TYPE zglt031,
      tg_fields      TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_editor      TYPE TABLE OF ty_editor,
      wg_editor      TYPE ty_editor,
      tg_msg_ret     TYPE TABLE OF zfiwrs0002  WITH HEADER LINE,
      vg_bname       TYPE user_addr-bname,

      tg_zglt031     TYPE TABLE OF ty_glt031,
      wg_zglt031     TYPE ty_glt031,
      tg_zglt032     TYPE TABLE OF ty_glt032,
      wg_zglt032     TYPE ty_glt032.

DATA: it_zglt031_log TYPE TABLE OF ty_glt031,
      wa_zglt031_log TYPE ty_glt031,
      it_zglt032_log TYPE TABLE OF ty_glt032,
      wa_zglt032_log TYPE ty_glt032.
*      WA_ZGLT076            TYPE TY_ZGLT076,
*      IT_ZGLT076            TYPE TABLE OF TY_ZGLT076.

DATA: ok_code         LIKE sy-ucomm,
      wg_mensagem(30),
      wg_acao(30).

DATA: tl_parametros TYPE ustyp_t_parameters,
      wl_parametros TYPE ustyp_parameters.

*&--------------------------------------------------------------------&*
*& Labels tela                                                        &*
*&--------------------------------------------------------------------&*
DATA  txt_tipo_lanc(20).

DATA  ts_100_tab1(15).
DATA  txt_empresa(10).
DATA  txt_dpto(35).
DATA  txt_tp_docu(20).
DATA  txt_ref(16).
DATA  txt_cab_docu(20).
DATA  txt_ap_fiscal(20).
DATA  txt_conc_banc(70).
DATA  txt_data_docu(15).
DATA  txt_data_lanc(15).
DATA  txt_prov(70).
DATA  txt_obs(22).
DATA  wgzglt031-dt_doc(22).
DATA  wgzglt031-dt_lcto(15).
DATA  wgzglt031-dt_doc_ult_mes(25).
DATA  wgzglt031-dt_lcto_ult_mes(25).
DATA  wgzglt031-prov_est(10).
DATA  wgzglt031-st_conc_banc(10).
DATA  wgzglt031-st_agrupa(50).
DATA  wgzglt031-st_agrupas(10).

DATA  wgzglt031-st_trans_banc(50).
DATA  wgzglt031-st_aprova(50).
DATA  wgzglt031-st_aprovas(10).
DATA  wgzglt031-st_trans_bancs(10).

DATA  wgzglt031-moeda_gp_hist(50).
DATA  wgzglt031-moeda_gp_hists(10).
DATA  ts_100_tab2(15).
DATA  bt_log(20).



*Class definition for ALV toolbar
CLASS:  lcl_alv_toolbar   DEFINITION DEFERRED.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_CONTABIL',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      g_descbox            TYPE scrfname VALUE 'CC_OBS',
      g_custom_cont_desc   TYPE REF TO cl_gui_custom_container,
      obg_descbox          TYPE REF TO cl_gui_textedit,
      obg_docking          TYPE REF TO cl_gui_docking_container,

      wa_style             TYPE lvc_s_styl,
      style                TYPE lvc_t_styl   WITH HEADER LINE,
      style2               TYPE lvc_t_styl   WITH HEADER LINE,
      gs_variant_c         TYPE disvariant.

* alrs
*Declaration for toolbar buttons
DATA: ty_toolbar TYPE stb_button.

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
CONSTANTS:
  c_0               TYPE c VALUE '0',
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
  c_modif(5)        TYPE c VALUE 'MODIF',
  c_cancel(6)       TYPE c VALUE 'CANCEL',
  c_deldoc(6)       TYPE c VALUE 'DELDOC',
  c_dclick(6)       TYPE c VALUE 'DCLICK',
  c_search(6)       TYPE c VALUE 'SEARCH',
  c_atuali(6)       TYPE c VALUE 'ATUALI',
  c_add_msg(7)      TYPE c VALUE 'ADD_MSG',
  c_del_msg(7)      TYPE c VALUE 'DEL_MSG',
  c_clos_msg(8)     TYPE c VALUE 'CLOS_MSG',
  c_save_msg(8)     TYPE c VALUE 'SAVE_MSG',
  c_displa(6)       TYPE c VALUE 'DISPLA',
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
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_onf4 FOR EVENT onf4 OF cl_gui_alv_grid
        IMPORTING e_fieldname e_fieldvalue es_row_no er_event_data et_bad_cells e_display.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS lcl_alv_toolbar DEFINITION
*---------------------------------------------------------------------*
*       ALV event handler
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*Constructor
    METHODS:
      constructor
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

*    IF wg_zglt031-tp_lcto IS INITIAL.
*      wl_desactive = 1.
*    ENDIF.

    IF wg_acao NE c_modif AND wg_acao NE c_add.
      wl_desactive = 1.
    ENDIF.

    ty_toolbar-icon      = icon_insert_row.
    ty_toolbar-function  = c_add.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_delete_row.
    ty_toolbar-function  = c_del.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      = icon_copy_object.
    ty_toolbar-function  = 'COPY'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*   variable for Toolbar Button
    ty_toolbar-icon      = icon_view_close.
    ty_toolbar-function  = c_clos_msg.
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
    DATA: tl_zglt032_aux TYPE TABLE OF ty_glt032,
          tl_zglt032_cop TYPE TABLE OF ty_glt032,
          wl_zglt032     TYPE ty_glt032,         "tg_zglt032,
          wl_lines       TYPE sy-tabix.
    REFRESH: tl_zglt032_aux.

    CASE e_ucomm.
      WHEN 'COPY'.
        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.
        LOOP AT tg_selectedcell INTO wg_selectedcell.
          wl_lines = wg_selectedcell-row_id-index.
        ENDLOOP.
        tl_zglt032_aux[] = tg_zglt032[].
        REFRESH: tl_zglt032_cop, tg_zglt032.
        LOOP AT tl_zglt032_aux INTO wl_zglt032.
          READ TABLE tg_selectedcell INTO wg_selectedcell
                      WITH KEY row_id-index = sy-tabix.
          IF sy-subrc = 0.
            APPEND wl_zglt032 TO tl_zglt032_cop.
          ENDIF.
        ENDLOOP.

        LOOP AT tl_zglt032_aux INTO wl_zglt032.
          APPEND wl_zglt032 TO tg_zglt032.
          IF sy-tabix = wl_lines.
            LOOP AT tl_zglt032_cop INTO wl_zglt032.
              APPEND wl_zglt032 TO tg_zglt032.
            ENDLOOP.
          ENDIF.
        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN c_add.
        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.
        CLEAR wg_selectedcell.
        IF tg_selectedcell[] IS NOT INITIAL.
          READ TABLE tg_selectedcell INTO wg_selectedcell INDEX 1.
        ENDIF.
        tl_zglt032_aux[] = tg_zglt032[].
        REFRESH: tg_zglt032.
        LOOP AT tl_zglt032_aux INTO wl_zglt032.
          APPEND wl_zglt032 TO tg_zglt032.
          IF sy-tabix = wg_selectedcell-row_id-index.
            CLEAR: wl_zglt032.
            APPEND wl_zglt032 TO tg_zglt032.
          ENDIF.
        ENDLOOP.
        IF wg_selectedcell-row_id-index = 0.
          CLEAR: wl_zglt032.
          APPEND wl_zglt032 TO tg_zglt032.
        ENDIF.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
      WHEN c_del.
        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.

        LOOP AT tg_selectedcell INTO wg_selectedcell.
          DELETE tg_zglt032 INDEX wg_selectedcell-row_id-index.
        ENDLOOP.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = wa_stable.
    ENDCASE.

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

  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_data_changed.
    DATA: ls_good    TYPE lvc_s_modi,
          lv_value   TYPE lvc_value,
          wl_zglt032 LIKE LINE OF tg_zglt032,
          wl_tbsl    TYPE tbsl,
          wl_lfa1    TYPE lfa1,
          wl_kna1    TYPE kna1,
          wl_skat    TYPE skat,
          wl_skb1    TYPE skb1,
          wl_anla    TYPE anla,
          wl_anln1   TYPE anla-anln1,
          wl_akont   TYPE lfb1-akont,
          wl_hkont   TYPE t074-hkont,
          wl_hkont2  TYPE t074-hkont,
          wl_umskz   TYPE zglt036-umskz,
* ---> S4 Migration - 17/07/2023 - CA
*          WL_CSKB    TYPE CSKB,
* <--- S4 Migration - 17/07/2023 - CA
          wl_csks    TYPE csks,
          wl_tka02   TYPE tka02.

    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE fieldname = 'BSCHL'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE tg_zglt032 INTO wl_zglt032 INDEX ls_good-row_id.

      CLEAR lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'HKONT'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'AKONT'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR_A'
          i_value     = lv_value.


      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'KOSTL'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'PRCTR'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'AUFNR'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MATNR'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MATNR_FI'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TAX_CODE'
          i_value     = lv_value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'ZUONR'
          i_value     = lv_value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'SGTXT'
          i_value     = lv_value.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                            INTO ls_good
                            WHERE fieldname = 'HKONT' OR fieldname = 'UMSKZ'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      READ TABLE tg_zglt032 INTO wl_zglt032 INDEX ls_good-row_id.
      CHECK wl_zglt032-bschl IS NOT INITIAL.
      IF  ls_good-fieldname = 'HKONT'.
        wl_hkont2 = lv_value.
        wl_umskz  = wl_zglt032-umskz.
      ELSE.
        wl_umskz  = lv_value.
        wl_hkont2 =  wl_zglt032-hkont.
      ENDIF.

      CLEAR lv_value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'AKONT'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR_A'
          i_value     = lv_value.

      MOVE-CORRESPONDING wl_zglt032 TO wg_zglt032.
      wg_zglt032-hkont = wl_hkont2.
      wg_zglt032-akont = wl_hkont2.
      wg_zglt032-umskz = wl_umskz.

      SELECT SINGLE * FROM tbsl
      INTO wl_tbsl
      WHERE bschl EQ wl_zglt032-bschl.
      CHECK sy-subrc = 0.
      CASE wl_tbsl-koart.
        WHEN 'K'.
          SELECT SINGLE * FROM lfa1 INTO wl_lfa1 WHERE lifnr EQ wl_hkont2.
          CHECK sy-subrc = 0.
          MOVE: wl_lfa1-name1 TO wg_zglt032-descr.
          IF wl_lfa1-vbund IS NOT INITIAL.
            lv_value = wl_lfa1-vbund.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'VBUND'
                i_value     = lv_value.
          ENDIF.
          "
          CLEAR wl_akont.
          SELECT SINGLE akont FROM lfb1 INTO wl_akont         WHERE lifnr = wl_hkont2  AND bukrs = wg_zglt031-bukrs.

          wg_zglt032-akont = wl_akont.
          IF wl_umskz NE ''.
            SELECT SINGLE skont FROM t074 INTO wl_hkont
              WHERE ktopl = '0050'
              AND   koart = 'K'
              AND   umskz = wl_umskz
              AND   hkont = wl_akont.
            wg_zglt032-akont = wl_hkont.
          ENDIF.
          SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr_a WHERE saknr EQ wg_zglt032-akont
                                                               AND spras EQ sy-langu
                                                               AND ktopl EQ '0050'.

        WHEN 'D'.
          SELECT SINGLE * FROM kna1 INTO wl_kna1 WHERE kunnr EQ wl_hkont2.
          CHECK sy-subrc = 0.
          IF wl_kna1-vbund IS NOT INITIAL.
            lv_value = wl_kna1-vbund.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'VBUND'
                i_value     = lv_value.
          ENDIF.

          SELECT SINGLE akont FROM knb1 INTO wl_akont         WHERE kunnr = wl_hkont2  AND bukrs = wg_zglt031-bukrs.
          wg_zglt032-akont = wl_akont.
          IF wl_umskz NE ''.
            SELECT SINGLE skont FROM t074 INTO wl_hkont
              WHERE ktopl = '0050'
              AND   koart = 'D'
              AND   umskz = wl_umskz
              AND   hkont = wl_akont.
            wg_zglt032-akont = wl_hkont.
          ENDIF.
          SELECT SINGLE txt50 FROM skat INTO wg_zglt032-descr_a WHERE saknr EQ wg_zglt032-akont
                                                               AND spras EQ sy-langu
                                                               AND ktopl EQ '0050'.

          MOVE: wl_kna1-name1 TO wg_zglt032-descr.
        WHEN 'S'.
          SELECT SINGLE * FROM skat INTO wl_skat WHERE saknr EQ wl_hkont2
                                                   AND spras EQ sy-langu
                                                   AND ktopl EQ '0050'.
          MOVE: wl_skat-txt50 TO wg_zglt032-descr.
          wg_zglt032-descr_a = wg_zglt032-descr.
        WHEN 'I'.
          SELECT SINGLE * FROM anla INTO wl_anla WHERE anln1 EQ wl_hkont2.
          MOVE: wl_anla-mcoa1 TO wg_zglt032-descr.
          wg_zglt032-descr_a = wg_zglt032-descr.
        WHEN 'A'.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = wl_hkont2
            IMPORTING
              output = wl_anln1.

          SELECT SINGLE * FROM anla INTO wl_anla WHERE anln1 EQ wl_anln1.
          MOVE: wl_anla-mcoa1 TO wg_zglt032-descr.
      ENDCASE.


      lv_value = wg_zglt032-akont.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'AKONT'
          i_value     = lv_value.

      lv_value = wg_zglt032-descr_a.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR_A'
          i_value     = lv_value.

      lv_value = wg_zglt032-descr.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'DESCR'
          i_value     = lv_value.

      SELECT SINGLE *
        FROM tka02
        INTO wl_tka02
        WHERE bukrs  = wg_zglt031-bukrs.

* ---> S4 Migration - 17/07/2023 - CA
*      CLEAR WL_CSKB. "Seleção não é utilizada
*      SELECT SINGLE *
*          FROM CSKB
*          INTO WL_CSKB
*          WHERE  KOKRS  = WL_TKA02-KOKRS
*          AND    KSTAR  = WL_HKONT2
*          AND    DATAB  LE SY-DATUM
*          AND    DATBI  GE SY-DATUM.
* <--- S4 Migration - 06/07/2023 - CA

      "Limpar objetos de custo
      lv_value     = ''.


      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'KOSTL'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'PRCTR'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'AUFNR'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MATNR'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'MATNR_FI'
          i_value     = lv_value.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TAX_CODE'
          i_value     = lv_value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'ZUONR'
          i_value     = lv_value.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'SGTXT'
          i_value     = lv_value.

    ENDLOOP.
  ENDMETHOD.                    "ON_DATA_CHANGED

*  METHOD on_data_changed4.
*
*  ENDMETHOD.                    "ON_DATA_CHANGED4

  METHOD on_data_changed_finished.

    "PERFORM: F_ATUALIZA_ALV2.
*** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    REFRESH tg_msg_ret.
*    PERFORM F_VERIFICA_ERROS.
    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen      = '100'
        i_show        = space
        i_repid       = sy-repid
        i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
        i_set_field   = 'X_FIELD'
      IMPORTING
        e_messagem    = wg_mensagem
      TABLES
        it_msgs       = tg_msg_ret.

  ENDMETHOD.                    "on_data_changed_finished

  METHOD on_onf4.
    TYPES: BEGIN OF ty_field,
             tabname   TYPE dd03l-tabname,     "Nome da tabela
             fieldname TYPE dd03l-fieldname,   "Nome de campo
             s(1)      TYPE c,
           END OF ty_field,

           BEGIN OF ty_value,
             tabname    TYPE dd03l-tabname,     "Nome da tabela
             fieldname  TYPE dd03l-fieldname,   "Nome de campo
             char79(79) TYPE c,
           END OF ty_value.

    DATA: BEGIN OF wl_valuetab,
            field(50),
          END OF wl_valuetab.

    DATA: tl_valuetab      LIKE TABLE OF wl_valuetab,
          tl_field         TYPE TABLE OF ty_field,
          wl_field         TYPE ty_field,
          tl_value         TYPE TABLE OF ty_value,
          wl_value         TYPE ty_value,

          wl_tbsl          TYPE tbsl,
          tl_lfa1          TYPE TABLE OF lfa1,
          wl_lfa1          TYPE lfa1,
          tl_kna1          TYPE TABLE OF kna1,
          wl_kna1          TYPE kna1,
          tl_skat          TYPE TABLE OF skat,
          wl_skat          TYPE skat,
          tl_anla          TYPE TABLE OF anla,
          wl_anla          TYPE anla,
          tl_t074u         TYPE TABLE OF t074u,
          wl_t074u         TYPE t074u,
          tl_t074t         TYPE TABLE OF t074t,
          wl_t074t         TYPE t074t,
          tl_t007s         TYPE TABLE OF t007s,
          wl_t007s         TYPE t007s,
          tl_t030k         TYPE TABLE OF t030k,
          wl_t030k         TYPE t030k,
          wl_t001          TYPE t001,
          wl_t005          TYPE t005,

          wl_index         TYPE sy-tabix,
          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30).

    READ TABLE tg_zglt032 INTO wg_zglt032 INDEX es_row_no-row_id.
    CASE e_fieldname.
      WHEN 'TAX_CODE'.
        IF wg_zglt031-bukrs IS NOT INITIAL.
          SELECT SINGLE *
            FROM t001
            INTO wl_t001
            WHERE bukrs = wg_zglt031-bukrs.

          SELECT SINGLE *
            FROM t005
            INTO wl_t005
            WHERE land1 = wl_t001-land1.
        ELSE.
          wl_t005-kalsm = 'TAXBRA'.
        ENDIF.

        SELECT *
          FROM t007s
          INTO CORRESPONDING FIELDS OF TABLE tl_t007s
             WHERE t007s~spras EQ sy-langu
               AND t007s~kalsm EQ wl_t005-kalsm.


        CHECK tl_t007s IS NOT INITIAL.

        SELECT *
        FROM t030k
          INTO TABLE tl_t030k
        FOR ALL ENTRIES IN tl_t007s
        WHERE ktopl = '0050'
        AND   mwskz = tl_t007s-mwskz.

        wl_fieldname  = 'MWSKZ'.
        wl_tabname    = 'T007S'.

        LOOP AT tl_t007s INTO wl_t007s.
          wl_index = sy-tabix.
          READ TABLE tl_t030k INTO wl_t030k WITH KEY mwskz = wl_t007s-mwskz.
          IF sy-subrc NE 0.
            DELETE tl_t007s INDEX wl_index.
            CONTINUE.
          ENDIF.
          MOVE: wl_t007s-mwskz TO wl_valuetab-field.
          APPEND wl_valuetab   TO tl_valuetab.

          MOVE: wl_t007s-text1  TO wl_valuetab-field.
          APPEND wl_valuetab    TO tl_valuetab.

          CLEAR:  wl_valuetab.
        ENDLOOP.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'MWSKZ'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'TEXT1'.
        wl_field-s = ' '.
        APPEND wl_field TO tl_field.
      WHEN 'HKONT'.
        CHECK wg_zglt032-bschl IS NOT INITIAL.

        SELECT SINGLE * FROM tbsl
          INTO wl_tbsl
        WHERE bschl EQ wg_zglt032-bschl.

        CHECK wl_tbsl IS NOT INITIAL.
        CASE wl_tbsl-koart.
          WHEN 'K'.
            wl_fieldname  = 'LIFNR'.
            wl_tabname    = 'LFA1'.

            IF wg_zglt031-bukrs IS INITIAL.
              SELECT * FROM lfa1 INTO TABLE tl_lfa1.
            ELSE.
              SELECT *
                FROM lfa1
                INNER JOIN lfb1
                ON  lfb1~lifnr = lfa1~lifnr
                AND lfb1~bukrs = wg_zglt031-bukrs
                INTO CORRESPONDING FIELDS OF TABLE tl_lfa1.
            ENDIF.

            CHECK tl_lfa1 IS NOT INITIAL.
            SORT tl_lfa1 BY lifnr.

            LOOP AT tl_lfa1 INTO wl_lfa1.
              MOVE: wl_lfa1-lifnr TO wl_valuetab-field.
              APPEND wl_valuetab TO tl_valuetab.

              MOVE: wl_lfa1-name1 TO wl_valuetab-field.
              APPEND wl_valuetab TO tl_valuetab.

              CLEAR: wl_lfa1, wl_valuetab.
            ENDLOOP.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'LIFNR'.
            wl_field-s = 'X'.
            APPEND wl_field TO tl_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'NAME1'.
            wl_field-s = ' '.
            APPEND wl_field TO tl_field.
          WHEN 'D'.
            wl_fieldname  = 'KUNNR'.
            wl_tabname    = 'KNA1'.

            IF wg_zglt031-bukrs IS INITIAL.
              SELECT * FROM kna1 INTO TABLE tl_kna1.
            ELSE.
              SELECT *
                FROM kna1
                INNER JOIN knb1
                ON  knb1~kunnr = kna1~kunnr
                AND knb1~bukrs = wg_zglt031-bukrs
                INTO CORRESPONDING FIELDS OF TABLE tl_kna1.
            ENDIF.


            CHECK tl_kna1 IS NOT INITIAL.
            SORT tl_kna1 BY kunnr.

            LOOP AT tl_kna1 INTO wl_kna1.
              MOVE: wl_kna1-kunnr TO wl_valuetab-field.
              APPEND wl_valuetab TO tl_valuetab.

              MOVE: wl_kna1-name1 TO wl_valuetab-field.
              APPEND wl_valuetab TO tl_valuetab.

              CLEAR: wl_kna1, wl_valuetab.
            ENDLOOP.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'KUNNR'.
            wl_field-s = 'X'.
            APPEND wl_field TO tl_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'NAME1'.
            wl_field-s = ' '.
            APPEND wl_field TO tl_field.
          WHEN 'S'.
            wl_fieldname  = 'SAKNR'.
            wl_tabname    = 'SKAT'.

            SELECT * FROM skat INTO TABLE tl_skat WHERE spras EQ sy-langu AND ktopl EQ '0050'.

            CHECK tl_skat IS NOT INITIAL.
            SORT tl_skat BY saknr.

            LOOP AT tl_skat INTO wl_skat.
              MOVE: wl_skat-saknr TO wl_valuetab-field.
              APPEND wl_valuetab TO tl_valuetab.

              MOVE: wl_skat-txt50 TO wl_valuetab-field.
              APPEND wl_valuetab TO tl_valuetab.

              CLEAR: wl_skat, wl_valuetab.
            ENDLOOP.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'SAKNR'.
            wl_field-s = 'X'.
            APPEND wl_field TO tl_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'TXT50'.
            wl_field-s = ' '.
            APPEND wl_field TO tl_field.
          WHEN 'I'.
            wl_fieldname  = 'ANLN1'.
            wl_tabname    = 'ANLA'.

            SELECT * FROM anla INTO TABLE tl_anla.

            CHECK tl_anla IS NOT INITIAL.
            SORT tl_anla BY anln1.

            LOOP AT tl_anla INTO wl_anla.
              MOVE: wl_anla-anln1 TO wl_valuetab-field.
              APPEND wl_valuetab TO tl_valuetab.

              MOVE: wl_anla-mcoa1 TO wl_valuetab-field.
              APPEND wl_valuetab TO tl_valuetab.

              CLEAR: wl_anla, wl_valuetab.
            ENDLOOP.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'ANLN1'.
            wl_field-s = 'X'.
            APPEND wl_field TO tl_field.

            wl_field-tabname = wl_tabname.
            wl_field-fieldname = 'MCOA1'.
            wl_field-s = ' '.
            APPEND wl_field TO tl_field.
        ENDCASE.
      WHEN 'UMSKZ'.
        SELECT SINGLE * FROM tbsl
          INTO wl_tbsl
        WHERE bschl EQ wg_zglt032-bschl.

        CHECK sy-subrc = 0.

        SELECT * FROM t074u INTO TABLE tl_t074u
          WHERE koart = wl_tbsl-koart.

        CHECK tl_t074u IS NOT INITIAL.

        SELECT *
          FROM t074t
          INTO CORRESPONDING FIELDS OF TABLE tl_t074t
          FOR ALL ENTRIES IN tl_t074u
             WHERE t074t~spras EQ sy-langu
               AND t074t~koart EQ tl_t074u-koart
               AND t074t~shbkz EQ tl_t074u-umskz.

        CHECK tl_t074t IS NOT INITIAL.

        wl_fieldname  = 'SHBKZ'.
        wl_tabname    = 'T074T'.

        LOOP AT tl_t074t INTO wl_t074t.
          MOVE: wl_t074t-shbkz TO wl_valuetab-field.
          APPEND wl_valuetab TO tl_valuetab.

          MOVE: wl_t074t-ltext TO wl_valuetab-field.
          APPEND wl_valuetab TO tl_valuetab.

          CLEAR: wl_kna1, wl_valuetab.
        ENDLOOP.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'SHBKZ'.
        wl_field-s = 'X'.
        APPEND wl_field TO tl_field.

        wl_field-tabname = wl_tabname.
        wl_field-fieldname = 'LTEXT'.
        wl_field-s = ' '.
        APPEND wl_field TO tl_field.
    ENDCASE.

    IF    wl_fieldname  IS NOT INITIAL
      AND wl_tabname    IS NOT INITIAL
      AND tl_field[]    IS NOT INITIAL
      AND tl_valuetab[] IS NOT INITIAL.

      CALL FUNCTION 'HELP_VALUES_GET_WITH_TABLE_EXT'
        EXPORTING
*         cucol                     = '3'
          fieldname                 = wl_fieldname
          tabname                   = wl_tabname
        IMPORTING
          index                     = wl_index
          select_value              = wl_char
        TABLES
          fields                    = tl_field
          select_values             = tl_value
          valuetab                  = tl_valuetab
        EXCEPTIONS
          field_not_in_ddic         = 001
          more_then_one_selectfield = 002
          no_selectfield            = 003.

      IF sy-subrc IS INITIAL.
        CASE e_fieldname.
          WHEN 'HKONT'.
            CASE wl_tbsl-koart.
              WHEN 'K'.
                READ TABLE tl_lfa1 INTO wl_lfa1 INDEX wl_index.
              WHEN 'D'.
                READ TABLE tl_kna1 INTO wl_kna1 INDEX wl_index.
              WHEN 'S'.
                READ TABLE tl_skat INTO wl_skat INDEX wl_index.
              WHEN 'I'.
                READ TABLE tl_anla INTO wl_anla INDEX wl_index.
            ENDCASE.
          WHEN 'UMSKZ'.
            READ TABLE tl_t074t INTO wl_t074t INDEX wl_index.
          WHEN 'TAX_CODE'.
            READ TABLE tl_t007s INTO wl_t007s INDEX wl_index.
        ENDCASE.

        IF es_row_no-row_id GT 0.
          READ TABLE tg_zglt032 INTO wg_zglt032 INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            CASE e_fieldname.
              WHEN 'HKONT'.
                CASE wl_tbsl-koart.
                  WHEN 'K'.
                    MOVE: wl_lfa1-lifnr TO wg_zglt032-hkont,
                          wl_lfa1-name1 TO wg_zglt032-descr.
                  WHEN 'D'.
                    MOVE: wl_kna1-kunnr TO wg_zglt032-hkont,
                          wl_kna1-name1 TO wg_zglt032-descr.
                  WHEN 'S'.
                    MOVE: wl_skat-saknr TO wg_zglt032-hkont,
                          wl_skat-txt50 TO wg_zglt032-descr.
                  WHEN 'I'.
                    MOVE: wl_anla-anln1 TO wg_zglt032-hkont,
                          wl_anla-mcoa1 TO wg_zglt032-descr.
                ENDCASE.
              WHEN 'UMSKZ'.
                MOVE: wl_t074t-shbkz TO wg_zglt032-umskz.
              WHEN 'TAX_CODE'.
                MOVE: wl_t007s-mwskz TO wg_zglt032-tax_code.
            ENDCASE.

            MODIFY tg_zglt032 FROM wg_zglt032 INDEX es_row_no-row_id.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

**** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.                    "ON_ONF4

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Antonio Luiz R. da Silva                                &*
*& Data.....: 11/02/2013                                              &*
*& Descrição: Cadastro de Impostos                                    &*
*& Transação: ZIMP51                                                  &*
*---------------------------------------------------------------------&*


INCLUDE zimp51top                               .    " global Data

* INCLUDE ZIMP51O01                               .  " PBO-Modules
* INCLUDE ZIMP51I01                               .  " PAI-Modules
* INCLUDE ZIMP51F01                               .  " FORM-Routines

*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*

TYPES: BEGIN OF ty_cadimp,
         cod_imposto   TYPE zimp_cad_imposto-cod_imposto,
         descr_imposto TYPE zimp_cad_imposto-descr_imposto,
         ref_imposto   TYPE zimp_cad_imposto-ref_imposto,
         tp_imposto    TYPE zimp_cad_imposto-tp_imposto,
         de_imposto    TYPE zimp_tipos_impos-arrecadacao,
         cod_pgto      TYPE zimp_cad_imposto-cod_pgto,
         conv_banco    TYPE zimp_cad_imposto-conv_banco,
         dep_resp      TYPE zimp_cad_imposto-dep_resp,
         dec_resp(20),
         hbkid         TYPE zimp_cad_imposto-hbkid,
         banka         TYPE bnka-banka,
         bukrs         TYPE zimp_cad_lote-bukrs,
         butxt         TYPE t001-butxt,
         gsber         TYPE zimp_cad_imposto-gsber,
         namefil       TYPE j_1bbranch-name,
         waers         TYPE zimp_cad_imposto-waers,
         waers_f       TYPE zimp_cad_imposto-waers_f,
       END OF ty_cadimp,

       BEGIN OF ty_cad_imp_it,
         cod_imposto  TYPE zimp_cad_imp_con-cod_imposto,
         cod_abertura TYPE zimp_campos_guia-cod_camp_guia,
         bschl        TYPE zimp_cad_imp_con-bschl,
         hkont        TYPE zimp_cad_imp_con-hkont,
         estorno      TYPE zimp_cad_imp_con-estorno,
       END OF ty_cad_imp_it,

       BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END   OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor.



*&--------------------------------------------------------------------&*
*& Declaração de tabelas e Work Areas                                 &*
*&--------------------------------------------------------------------&*
DATA: ok-code             TYPE sy-ucomm,
      wg_cadimp           TYPE ty_cadimp,
      wg_cad_imp_it       TYPE ty_cad_imp_it,
      tg_selectedcell     TYPE lvc_t_cell,
      wg_selectedcell     TYPE lvc_s_cell,
      wa_zimp_cad_imp_con TYPE zimp_cad_imp_con,
      x_field(30),

      BEGIN OF tg_itens OCCURS 0,
        mark(1),
        cod_imposto     TYPE zimp_cad_imp_con-cod_imposto,
        cod_abertura    TYPE zimp_cad_imp_con-cod_abertura,
        descr_camp_guia TYPE zimp_campos_guia-descr_camp_guia,
        bschl           TYPE zimp_cad_imp_con-bschl,
        umskz           TYPE zimp_cad_imp_con-umskz,
        lifnr           TYPE zimp_cad_imp_con-lifnr,
        kunnr           TYPE zimp_cad_imp_con-kunnr,
        estorno         TYPE zimp_cad_imp_con-estorno,
        hkont           TYPE zimp_cad_imp_con-hkont,
        agrupamento     TYPE zimp_cad_imp_con-agrupamento,
        tipoc           TYPE c,
      END OF tg_itens.


** Criação de tabela dinamica
DATA: t_fieldcatalog      TYPE lvc_t_fcat,
      w_fieldcatalog      TYPE lvc_s_fcat,
      wa_layout           TYPE lvc_s_layo,
      wa_stable           TYPE lvc_s_stbl,
      wg_editor           TYPE ty_editor,

      tg_fields           TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_editor           TYPE TABLE OF ty_editor,
      it_zimp_cad_imp_con TYPE TABLE OF zimp_cad_imp_con,
      tg_msg_ret          TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
           END OF c_tab_strip_imp.
*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  tab_strip_imp TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip_imp,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZIMP51',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

DATA: ok_code         LIKE sy-ucomm,
      wg_mensagem(30),
      wg_acao(30).



*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.
*            lcl_alv_toolbar2  definition deferred.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_ITENS_IMP',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      g_descbox            TYPE scrfname VALUE 'CC_DESC',
      g_custom_cont_desc   TYPE REF TO cl_gui_custom_container,
      obg_descbox          TYPE REF TO cl_gui_textedit,
      obg_docking          TYPE REF TO cl_gui_docking_container,

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
    IF ( wg_cadimp-cod_imposto IS INITIAL OR wg_cadimp-descr_imposto IS INITIAL ) .
      wl_desactive = 1.
    ENDIF.

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
    DATA: tl_itens_aux LIKE TABLE OF tg_itens,
          wl_itens     LIKE LINE OF tg_itens,
          wl_lines     TYPE sy-tabix.
    REFRESH: tl_itens_aux.

    IF wg_cadimp-cod_imposto IS  NOT INITIAL AND wg_cadimp-descr_imposto IS NOT INITIAL.
      CASE e_ucomm.
        WHEN c_add.
          tl_itens_aux[] = tg_itens[].
          REFRESH: tg_itens.
          LOOP AT tl_itens_aux INTO wl_itens.
            APPEND wl_itens TO tg_itens.
          ENDLOOP.
          CLEAR: wl_itens.
          APPEND wl_itens TO tg_itens.

          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid1->get_selected_cells
            IMPORTING
              et_cell = tg_selectedcell.

          LOOP AT tg_selectedcell INTO wg_selectedcell.
            DELETE tg_itens INDEX wg_selectedcell-row_id-index.
          ENDLOOP.

          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.
    ENDIF.
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

    DATA: ls_good             TYPE lvc_s_modi,
          lv_value            TYPE lvc_value,
          vl_value            TYPE lvc_value,
          wl_itens            LIKE LINE OF tg_itens,
          wl_zimp_campos_guia TYPE zimp_campos_guia,
          wl_tbsl             TYPE tbsl.


    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'AGRUPAMENTO'.

      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
      IF wl_itens-bschl IS NOT INITIAL AND ls_good-value IS NOT INITIAL.
        SELECT SINGLE *
        FROM tbsl
        INTO @wl_tbsl
        WHERE bschl = @wl_itens-bschl.
        IF sy-subrc IS INITIAL.
          IF wl_tbsl-shkzg = 'H'. "C
            wl_itens-tipoc = 'C'.
            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id.
          ELSE. "D
            wl_itens-tipoc = 'D'.
            MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id.
          ENDIF.
        ENDIF.
      ELSEIF wl_itens-bschl IS NOT INITIAL AND ls_good-value IS INITIAL.
        SELECT SINGLE *
        FROM tbsl
        INTO @wl_tbsl
        WHERE bschl = @wl_itens-bschl.
        IF sy-subrc IS INITIAL.
          wl_itens-tipoc = abap_false.
          MODIFY tg_itens FROM wl_itens INDEX ls_good-row_id.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'COD_ABERTURA'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      SELECT SINGLE *
        FROM zimp_campos_guia
        INTO wl_zimp_campos_guia
          WHERE cod_camp_guia EQ lv_value.

      IF sy-subrc IS INITIAL.
        MOVE wl_zimp_campos_guia-descr_camp_guia TO lv_value.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'DESCR_CAMP_GUIA'
            i_value     = lv_value.
      ELSE.
*        CLEAR: lv_value.
*        CALL METHOD er_data_changed->modify_cell
*          EXPORTING
*            i_row_id    = ls_good-row_id
*            i_fieldname = 'COD_ABERTURA'
*            i_value     = lv_value.
*        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Código de abertura não foi encontrado!'.
      ENDIF.

    ENDLOOP.


  ENDMETHOD.                    "ON_DATA_CHANGED
*  METHOD on_data_changed4.
*
*  ENDMETHOD.                    "ON_DATA_CHANGED4
  METHOD on_data_changed_finished.

*** Método de atualização de dados na Tela
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    PERFORM verifica_erros.

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

  ENDMETHOD.                    "on_data_changed_finisheD

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

          tl_t074u         TYPE TABLE OF t074u,
          wl_t074u         TYPE t074u,
          tl_t074t         TYPE TABLE OF t074t,
          wl_t074t         TYPE t074t,
          wg_itens         LIKE LINE OF tg_itens,

          wl_index         TYPE sy-tabix,
          wl_char(20),
          wl_fieldname(30),
          wl_tabname(30).

    READ TABLE tg_itens INTO wg_itens INDEX es_row_no-row_id.
    CASE e_fieldname.

      WHEN 'UMSKZ'.
        SELECT * FROM t074u INTO TABLE tl_t074u.

        CHECK tl_t074u IS NOT INITIAL.

        SELECT * FROM t074t
          INTO TABLE tl_t074t
          FOR ALL ENTRIES IN tl_t074u
        WHERE spras EQ sy-langu
          AND koart EQ tl_t074u-koart
          AND shbkz EQ tl_t074u-umskz.

        CHECK tl_t074t IS NOT INITIAL.

        wl_fieldname  = 'SHBKZ'.
        wl_tabname    = 'T074T'.

        LOOP AT tl_t074t INTO wl_t074t.
          MOVE: wl_t074t-shbkz TO wl_valuetab-field.
          APPEND wl_valuetab TO tl_valuetab.

          MOVE: wl_t074t-ltext TO wl_valuetab-field.
          APPEND wl_valuetab TO tl_valuetab.

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

          WHEN 'UMSKZ'.
            READ TABLE tl_t074t INTO wl_t074t INDEX wl_index.
        ENDCASE.

        IF es_row_no-row_id GT 0.
          READ TABLE tg_itens INTO wg_itens INDEX es_row_no-row_id.
          IF sy-subrc IS INITIAL.
            CASE e_fieldname.
              WHEN 'UMSKZ'.
                MOVE: wl_t074t-shbkz TO wg_itens-umskz.
            ENDCASE.

            MODIFY tg_itens FROM wg_itens INDEX es_row_no-row_id.
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
  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field."'WG_DESC_OPERACAO'.
  ENDIF.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.


  IF wg_acao IS INITIAL.
    APPEND c_save TO fcode.
    APPEND c_deldoc TO fcode.
  ENDIF.
  SET PF-STATUS 'Z002' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_IMP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_strip_imp_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_strip_imp-tab1.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab1.
    WHEN c_tab_strip_imp-tab2.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab2.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TAB_STRIP_IMP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*&      Module  TAB_STRIP_IMP_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tab_strip_imp_active_tab_set OUTPUT.
  PERFORM verifica_erros.
  tab_strip_imp-activetab = g_tab_strip_imp-pressed_tab.
  CASE g_tab_strip_imp-pressed_tab.
    WHEN c_tab_strip_imp-tab1.
      g_tab_strip_imp-subscreen = '0200'.
    WHEN c_tab_strip_imp-tab2.
      g_tab_strip_imp-subscreen = '0300'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TAB_STRIP_IMP_ACTIVE_TAB_SET  OUTPUT
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
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA w_answer.
  CASE ok-code.
    WHEN c_deldoc.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          text_question         = 'Confirma a exclusão do imposto?'
          text_button_1         = 'Sim'(001)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Não'(002)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
*         USERDEFINED_F1_HELP   = ' '
          start_column          = 25
          start_row             = 6
*         POPUP_TYPE            =
*         IV_QUICKINFO_BUTTON_1 = ' '
*         IV_QUICKINFO_BUTTON_2 = ' '
        IMPORTING
          answer                = w_answer
*       TABLES
*         PARAMETER             =
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.
      .
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF w_answer = '1'.
        PERFORM eliminar_imposto.
      ENDIF.
    WHEN c_search.
      PERFORM busca_dados.
    WHEN c_save.
      DELETE tg_itens WHERE  cod_abertura IS INITIAL.

      CALL METHOD grid1->check_changed_data.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS INITIAL.
        CLEAR wg_acao.
        PERFORM grava_dados.
        REFRESH: tg_fields.
        PERFORM trata_campos USING space
                                 'GR2'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM trata_campos USING space
                                  'GR1'
                                   c_1       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 1.

      ELSE.
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Há erro no documento.'.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.

    WHEN c_back.
      CLEAR wg_acao.
    WHEN c_add.
      wg_acao = c_modif.
      PERFORM limpa_campos.
      PERFORM obtem_proximo.
      REFRESH: tg_fields.
      PERFORM trata_campos USING space
                                 'GR2'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                                'GR1'
                                 c_0       "INPUT 1     NO INPUT 0
                                 c_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 0.
    WHEN c_cancel.
      CLEAR wg_acao.
    WHEN c_atuali.

    WHEN c_modif.
      IF wg_acao = c_modif.
        CLEAR wg_acao.
        REFRESH: tg_fields.
        PERFORM trata_campos USING space
                                 'GR2'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM trata_campos USING space
                                  'GR1'
                                   c_1       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 1.
      ELSE.
        wg_acao = c_modif.
        PERFORM trata_campos USING space
                                   'GR2'
                                      c_1       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
        PERFORM trata_campos USING space
                                  'GR1'
                                   c_0       "INPUT 1     NO INPUT 0
                                   c_0.      "INVISIBLE 1 VISIBLE 0
        CALL METHOD obg_descbox->set_readonly_mode
          EXPORTING
            readonly_mode = 0.
      ENDIF.
    WHEN c_show_msgre.
      "CLEAR wg_acao.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '100'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.

    WHEN c_exit.

      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
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
        wl_function LIKE tl_function WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4     WITH HEADER LINE.

  DATA: waref      TYPE REF TO data.
  IF g_custom_container IS INITIAL.
*    WA_LAYOUT-CWIDTH_OPT = C_X.
    wa_layout-zebra      = c_x.
*    WA_LAYOUT-NO_TOOLBAR = C_X.
*    WA_LAYOUT-SGL_CLK_HD = C_X.
    wa_layout-no_rowmark = c_x.
*    WA_LAYOUT-COL_OPT    = C_X.
    wa_stable-row        = c_x.
    wa_layout-sel_mode   = 'A'.
    wa_layout-cwidth_opt   = 'X'.
    wa_layout-box_fname    = 'MARK'.

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
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.

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
        it_outtab            = tg_itens[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    lt_f4-fieldname = 'UMSKZ'.
    lt_f4-register  = 'X'.
    lt_f4-getbefore = 'X'.
    APPEND lt_f4.


    CALL METHOD grid1->register_f4_for_fields
      EXPORTING
        it_f4 = lt_f4[].

    SET HANDLER:
              lcl_event_handler=>on_double_click FOR grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed FOR grid1,
              lcl_event_handler=>on_onf4         FOR grid1.


*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF g_custom_cont_desc IS INITIAL.
    CREATE OBJECT g_custom_cont_desc
      EXPORTING
        container_name = g_descbox.

    IF g_custom_cont_desc IS NOT INITIAL.
      CREATE OBJECT obg_descbox
        EXPORTING
          parent            = g_custom_cont_desc
          wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
          wordwrap_position = 72
          max_number_chars  = 350.

      CALL METHOD obg_descbox->set_toolbar_mode
        EXPORTING
          toolbar_mode = '0'.

      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    ENDIF.
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
        1 'ZIMP_CAMPOS_GUIA'       'COD_CAMP_GUIA'   'TG_ITENS' 'COD_ABERTURA'     'Cod.Aber'     '10' 'X' ' ' 'X',
        2 'ZIMP_CAMPOS_GUIA'       'DESCR_CAMP_GUIA' 'TG_ITENS' 'DESCR_CAMP_GUIA'  'Descrição'    '25' ' ' ' ' ' ',
        3 'ZIMP_CAD_IMP_CON'       'BSCHL'           'TG_ITENS' 'BSCHL'            ' '            '10' 'X' ' ' 'X',
        4 ' '                       ''               'TG_ITENS' 'UMSKZ'            'Rz. Especial' '10' 'X' ' ' 'X',
        5 'ZIMP_CAD_IMP_CON'       'ESTORNO'         'TG_ITENS' 'ESTORNO'          'Estorno '     '10' 'X' ' ' ' ',
        6 'ZIMP_CAD_IMP_CON'       'HKONT'           'TG_ITENS' 'HKONT'            'Conta '       '12' 'X' ' ' 'X',
        7 'ZIMP_CAD_IMP_CON'       'LIFNR'           'TG_ITENS' 'LIFNR'            'Fornec.'      '12' 'X' ' ' 'X',
        8 'ZIMP_CAD_IMP_CON'       'KUNNR'           'TG_ITENS' 'KUNNR'            'Cliente'      '12' 'X' ' ' 'X',
        9 'ZIMP_CAD_IMP_CON'       'AGRUPAMENTO'     'TG_ITENS' 'AGRUPAMENTO'      'Agrupamento ' '14' 'X' ' ' ' '.

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

  IF p_field EQ 'ESTORNO'.
    w_fieldcatalog-checkbox = c_x.
  ENDIF.

  IF p_field EQ 'AGRUPAMENTO'.
    w_fieldcatalog-checkbox = c_x.
  ENDIF.

  IF p_field EQ 'HKONT' OR p_field EQ 'UMSKZ'.
    w_fieldcatalog-f4availabl = c_x.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_campos .
  CLEAR: wg_cadimp , tg_editor,wg_mensagem,x_field.
  wg_cadimp-waers = 'BRL'.
  REFRESH: tg_itens, tg_editor.


ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM obtem_proximo .

  SELECT  *
    FROM zimp_cad_imp_con
    INTO TABLE it_zimp_cad_imp_con
    ORDER BY cod_imposto DESCENDING.

  READ TABLE it_zimp_cad_imp_con INTO wa_zimp_cad_imp_con INDEX 1.
  IF sy-subrc NE 0.
    wg_cadimp-cod_imposto = 1.
  ELSE.
    wg_cadimp-cod_imposto = wa_zimp_cad_imp_con-cod_imposto.
    ADD 1 TO wg_cadimp-cod_imposto.
  ENDIF.

ENDFORM.                    " OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*&      Form  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*      -->P_1229   text
*      -->P_C_1  text
*      -->P_C_0  text
*----------------------------------------------------------------------*
FORM trata_campos  USING    p_field
                            p_group1
                            p_value
                            p_invisible.

  tg_fields-campo     = p_field.
  tg_fields-group1    = p_group1.
  tg_fields-value     = p_value.
  tg_fields-invisible = p_invisible.
  APPEND tg_fields.

ENDFORM.                    " TRATA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .
  DATA: wl_linha(6)  ,
        wl_itens            LIKE LINE OF tg_itens,
        wl_itens_aux        LIKE LINE OF tg_itens,
        tl_tbsl             TYPE TABLE OF tbsl       WITH HEADER LINE,
        tl_ska1             TYPE TABLE OF ska1       WITH HEADER LINE,
        tl_lfa1             TYPE TABLE OF lfa1       WITH HEADER LINE,
        tl_kna1             TYPE TABLE OF kna1       WITH HEADER LINE,
        wl_tcurc            TYPE tcurc,
        wl_t001             TYPE t001,
        wl_j_1bbranch       TYPE j_1bbranch,
        it_zimp_campos_guia TYPE TABLE OF zimp_campos_guia WITH HEADER LINE,
        tl_itens_aux        LIKE TABLE OF tg_itens,
        w_codigo            TYPE zimp_cad_imp_con-cod_abertura,
        v_qtde              TYPE i VALUE 0.

  REFRESH: tg_msg_ret,tl_tbsl,it_zimp_campos_guia,tl_ska1.
  CLEAR: tg_msg_ret, tl_tbsl,it_zimp_campos_guia,tl_ska1.

  IF wg_cadimp-bukrs IS INITIAL .
*    MOVE: TEXT-E01                  TO TG_MSG_RET-MSG,
*          'WG_CADLAN-BUKRS'         TO TG_MSG_RET-FIELD.
*    CONCATENATE  TG_MSG_RET-MSG 'Empresa' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
       FROM t001
       INTO wl_t001
        WHERE  bukrs EQ wg_cadimp-bukrs.
    IF sy-subrc NE 0.
      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba,
             'WG_CADLAN-BUKRS'         TO tg_msg_ret-field.
      CONCATENATE TEXT-e04 ' Empresa' INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
  IF wg_cadimp-waers IS INITIAL .
    MOVE: c_tab_strip_imp-tab1 TO tg_msg_ret-aba,
          TEXT-e01                  TO tg_msg_ret-msg,
          'WG_CADIMP-WAERS'         TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Moeda' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSE.
    SELECT SINGLE *
       FROM tcurc
       INTO wl_tcurc
        WHERE  waers EQ wg_cadimp-waers.
    IF sy-subrc NE 0.
      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba,
             'WG_CADIMP-WAERS'         TO tg_msg_ret-field.
      CONCATENATE TEXT-e04 ' Moeda do Documento' INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF NOT wg_cadimp-waers_f IS INITIAL AND wg_cadimp-waers_f NE 'USD'.
    MOVE: c_tab_strip_imp-tab1        TO tg_msg_ret-aba,
          TEXT-e11                    TO tg_msg_ret-msg,
          'WG_CADIMP-WAERS_F'         TO tg_msg_ret-field.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF wg_cadimp-gsber IS INITIAL.
*    MOVE:  C_TAB_STRIP_IMP-TAB1 TO TG_MSG_RET-ABA.
*    CONCATENATE TEXT-E01 ' Filial  '  INTO  TG_MSG_RET-MSG.
*    APPEND TG_MSG_RET.
*    CLEAR: TG_MSG_RET.
  ELSE.
    SELECT SINGLE *
    FROM j_1bbranch
    INTO  wl_j_1bbranch
    WHERE bukrs = wg_cadimp-bukrs
    AND  branch = wg_cadimp-gsber.
    IF sy-subrc NE 0.
      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba,
             'WG_CADIMP-GSBER'         TO tg_msg_ret-field.
      CONCATENATE TEXT-e04 ' Filial '  INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.

  IF wg_cadimp-descr_imposto IS INITIAL.
    MOVE: TEXT-e01                  TO tg_msg_ret-msg,
          c_tab_strip_imp-tab1      TO tg_msg_ret-aba,
          'WG_CADIMP-DESCR_IMPOSTO' TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Descrição do Imposto' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.
  "obg_descbox
  IF obg_descbox IS NOT INITIAL.
    CALL METHOD obg_descbox->get_text_as_r3table
      IMPORTING
        table = tg_editor.

    IF tg_editor[] IS INITIAL.
      MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            c_tab_strip_imp-tab1      TO tg_msg_ret-aba,
            'WG_CADIMP-REF_IMPOSTO' TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'Comentário do Imposto' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDIF.
  "conv_banco
  IF wg_cadimp-conv_banco IS NOT INITIAL AND wg_cadimp-hbkid IS INITIAL.
    MOVE: TEXT-e01                  TO tg_msg_ret-msg,
          c_tab_strip_imp-tab1      TO tg_msg_ret-aba,
          'WG_CADIMP-HBKID' TO tg_msg_ret-field.
    CONCATENATE  tg_msg_ret-msg 'Banco' INTO tg_msg_ret-msg SEPARATED BY space.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ELSEIF NOT 'BBD_BBRA' CS wg_cadimp-hbkid.
    MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba,
           'WG_CADIMP-HBKID' TO tg_msg_ret-field.
    CONCATENATE TEXT-e04 ' Banco' INTO  tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.

  ENDIF.

*  IF wg_cadimp-dep_resp IS INITIAL.
*    MOVE: text-e01                  TO tg_msg_ret-msg,
*          c_tab_strip_imp-tab1      TO tg_msg_ret-aba,
*          'WG_CADIMP-DEP_RESP' TO tg_msg_ret-field.
*    CONCATENATE  tg_msg_ret-msg 'Departamento Responsável' INTO tg_msg_ret-msg SEPARATED BY space.
*    APPEND tg_msg_ret.
*    CLEAR: tg_msg_ret.
*  ELSEIF NOT '01_02_03' CS wg_cadimp-dep_resp.
*    MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
*    CONCATENATE text-e04 'Departamento Responsável ' INTO  tg_msg_ret-msg.
*    APPEND tg_msg_ret.
*    CLEAR: tg_msg_ret.
*  ENDIF.

  IF tg_itens[] IS NOT INITIAL.
    SELECT *
      FROM zimp_campos_guia
      INTO TABLE it_zimp_campos_guia
      FOR ALL ENTRIES IN tg_itens
     WHERE cod_camp_guia EQ tg_itens-cod_abertura.

    SELECT *
     FROM tbsl
     INTO TABLE tl_tbsl
     FOR ALL ENTRIES IN tg_itens
      WHERE bschl EQ tg_itens-bschl.

    SELECT *                           "#EC CI_DB_OPERATION_OK[2431747]
        FROM ska1                      "#EC CI_DB_OPERATION_OK[2389136]
        INTO TABLE tl_ska1
         FOR ALL ENTRIES IN tg_itens
          WHERE ktopl EQ c_50
            AND saknr EQ tg_itens-hkont.

    SELECT *
        FROM lfa1
        INTO TABLE tl_lfa1
         FOR ALL ENTRIES IN tg_itens
          WHERE lifnr EQ tg_itens-lifnr.

    SELECT *
        FROM kna1
        INTO TABLE tl_kna1
         FOR ALL ENTRIES IN tg_itens
          WHERE kunnr EQ tg_itens-kunnr.
  ELSE.
    MOVE: TEXT-e09                  TO tg_msg_ret-msg,
          c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  tl_itens_aux[] = tg_itens[].
  SORT: tl_itens_aux BY cod_abertura.

  LOOP AT tg_itens INTO wl_itens.
    wl_linha = sy-tabix.
    v_qtde = 0.
    LOOP AT tl_itens_aux INTO wl_itens_aux WHERE cod_abertura = wl_itens-cod_abertura .
      IF wl_itens-cod_abertura = wl_itens-cod_abertura.
        ADD 1 TO v_qtde.
      ENDIF.
    ENDLOOP.

    IF v_qtde > 1.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE TEXT-e03 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
  ENDLOOP.

* RJF - Início
  DATA: lv_contd(2) TYPE n,
        lv_contc(2) TYPE n.
  DATA(tl_itens_aux_a) = tg_itens[].
  DELETE tl_itens_aux_a[] WHERE agrupamento IS INITIAL.
  IF lines( tl_itens_aux_a ) GT 3.
    MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba,
           TEXT-e20             TO tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  LOOP AT tg_itens INTO wl_itens.

    IF wl_itens-tipoc EQ 'D'.
      lv_contd = lv_contd + 1.
    ELSEIF wl_itens-tipoc EQ 'C'.
      lv_contc = lv_contc + 1.
    ENDIF.

  ENDLOOP.

  IF ok-code EQ 'SAVE'.
    IF lv_contd EQ 1 AND lv_contc IS INITIAL..
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba,
             TEXT-e21             TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


    IF lv_contc EQ 1 AND lv_contd IS INITIAL..
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba,
             TEXT-e21             TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


    IF lv_contd EQ 2 AND lv_contc IS INITIAL..
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba,
             TEXT-e21             TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


    IF lv_contc EQ 2 AND lv_contd IS INITIAL..
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba,
             TEXT-e21             TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDIF.

  IF lv_contc GT 2 AND lv_contd IS INITIAL.
    MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba,
           TEXT-e21             TO tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF lv_contd GT 2 AND lv_contc IS INITIAL.
    MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba,
           TEXT-e21             TO tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.

  IF lv_contc EQ 2 AND lv_contd EQ 2.
    MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba,
           TEXT-e21             TO tg_msg_ret-msg.
    APPEND tg_msg_ret.
    CLEAR: tg_msg_ret.
  ENDIF.
* RJF - Fim

  SORT: it_zimp_campos_guia BY cod_camp_guia,
       tl_tbsl              BY bschl,
       tl_lfa1              BY lifnr,
       tl_kna1              BY kunnr,
       tl_ska1              BY saknr.


  LOOP AT tg_itens.
    wl_linha = sy-tabix.
    IF tg_itens-cod_abertura IS INITIAL
         OR tg_itens-bschl IS INITIAL.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE TEXT-e04 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF tg_itens-kunnr IS NOT INITIAL AND tg_itens-lifnr IS NOT INITIAL.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE TEXT-e12 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    READ TABLE it_zimp_campos_guia
      WITH KEY cod_camp_guia = tg_itens-cod_abertura
               BINARY SEARCH.
    IF sy-subrc NE 0.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE TEXT-e04 ' Cod.Abertura' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    READ TABLE tl_tbsl
      WITH KEY bschl = tg_itens-bschl
               BINARY SEARCH.
    IF sy-subrc NE 0.
      MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
      CONCATENATE TEXT-e04 ' Chv.Lçto' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF tl_tbsl-koart = 'K'.
      IF tg_itens-lifnr IS INITIAL.
*        MOVE:  C_TAB_STRIP_IMP-TAB2 TO TG_MSG_RET-ABA.
*        CONCATENATE TEXT-E05 ' LINHA: ' WL_LINHA INTO  TG_MSG_RET-MSG.
*        APPEND TG_MSG_RET.
*        CLEAR: TG_MSG_RET.
      ELSE.
        READ TABLE tl_lfa1
        WITH KEY lifnr = tg_itens-lifnr
                   BINARY SEARCH.
        IF sy-subrc NE 0.
          MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e04 ' Fornecedor ' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.
      IF tg_itens-kunnr IS NOT INITIAL.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e07 'Cliente LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF tg_itens-umskz IS INITIAL.
        CLEAR: tl_tbsl.
        READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = tg_itens-bschl
                                                 xsonu = 'X'
                                        BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
          CONCATENATE 'Razão especial - Obrigatório' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF tg_itens-hkont IS NOT INITIAL.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e06 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ELSEIF tl_tbsl-koart NE 'K' AND  tl_tbsl-koart NE 'D'.
      IF tg_itens-lifnr IS NOT INITIAL.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e07 'Fornecedor LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
      IF tg_itens-kunnr IS NOT INITIAL.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e07 'Cliente LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
      IF tg_itens-hkont IS INITIAL.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e08 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        READ TABLE tl_ska1
          WITH KEY saknr = tg_itens-hkont
                   BINARY SEARCH.
        IF sy-subrc NE 0.
          MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e10 wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.
    ELSEIF tl_tbsl-koart = 'D'.
      IF tg_itens-kunnr IS INITIAL.

      ELSE.
        READ TABLE tl_kna1
        WITH KEY kunnr = tg_itens-kunnr
                   BINARY SEARCH.
        IF sy-subrc NE 0.
          MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e04 ' Cliente ' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF tg_itens-umskz IS INITIAL.
        CLEAR: tl_tbsl.
        READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = tg_itens-bschl
                                                 xsonu = 'X'
                                        BINARY SEARCH.
        IF sy-subrc EQ 0.
          MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
          CONCATENATE 'Razão especial - Obrigatório' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF tg_itens-lifnr IS NOT INITIAL.
        MOVE:  c_tab_strip_imp-tab2 TO tg_msg_ret-aba.
        CONCATENATE TEXT-e07 'Fornecedor LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.



  ENDLOOP.
ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: wl_input_cadimp TYPE zimp_cad_imposto,
        tl_input_cadimp TYPE TABLE OF zimp_cad_imp_con WITH HEADER LINE.

  MOVE: sy-mandt                TO wl_input_cadimp-mandt,
        wg_cadimp-cod_imposto   TO  wl_input_cadimp-cod_imposto,
        wg_cadimp-descr_imposto TO  wl_input_cadimp-descr_imposto,
        "wg_cadimp-ref_imposto   TO  wl_input_cadimp-ref_imposto,
        wg_cadimp-tp_imposto    TO  wl_input_cadimp-tp_imposto,
        wg_cadimp-cod_pgto      TO  wl_input_cadimp-cod_pgto,
        wg_cadimp-conv_banco    TO  wl_input_cadimp-conv_banco,
        wg_cadimp-hbkid         TO  wl_input_cadimp-hbkid,
        wg_cadimp-dep_resp      TO  wl_input_cadimp-dep_resp,
        wg_cadimp-bukrs         TO  wl_input_cadimp-bukrs,
        wg_cadimp-gsber         TO  wl_input_cadimp-gsber,
        wg_cadimp-waers         TO  wl_input_cadimp-waers,
        wg_cadimp-waers_f       TO  wl_input_cadimp-waers_f,

        sy-uname TO wl_input_cadimp-usuario,
        sy-datum TO wl_input_cadimp-data_atual,
        sy-uzeit TO wl_input_cadimp-hora_atual.

  REFRESH: tg_editor.
  IF obg_descbox IS NOT INITIAL.
    CALL METHOD obg_descbox->get_text_as_r3table
      IMPORTING
        table = tg_editor.

    LOOP AT tg_editor INTO wg_editor.
      IF sy-tabix EQ 1.
        wl_input_cadimp-ref_imposto = wg_editor-line.

      ELSEIF sy-tabix GE 2.
        CONCATENATE wl_input_cadimp-ref_imposto  wg_editor-line INTO wl_input_cadimp-ref_imposto. " SEPARATED BY space.

      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT tg_itens.
    MOVE: sy-mandt               TO tl_input_cadimp-mandt,
          wg_cadimp-cod_imposto  TO tl_input_cadimp-cod_imposto,
          tg_itens-cod_abertura  TO tl_input_cadimp-cod_abertura,
          tg_itens-bschl         TO tl_input_cadimp-bschl,
          tg_itens-umskz         TO tl_input_cadimp-umskz,
          tg_itens-hkont         TO tl_input_cadimp-hkont,
          tg_itens-lifnr         TO tl_input_cadimp-lifnr,
          tg_itens-kunnr         TO tl_input_cadimp-kunnr,
          tg_itens-estorno       TO tl_input_cadimp-estorno,
          sy-uname               TO tl_input_cadimp-usuario,
          sy-datum               TO tl_input_cadimp-data_atual,
          sy-uzeit               TO tl_input_cadimp-hora_atual,
          tg_itens-agrupamento   TO tl_input_cadimp-agrupamento.
    APPEND tl_input_cadimp.

  ENDLOOP.
  "
  DELETE FROM zimp_cad_imp_con WHERE cod_imposto = wg_cadimp-cod_imposto.
  MODIFY zimp_cad_imposto FROM       wl_input_cadimp.
  MODIFY zimp_cad_imp_con FROM TABLE tl_input_cadimp.


  MESSAGE s836(sd) WITH 'Imposto'
                         wg_cadimp-cod_imposto
                         ', criado/modificado com sucesso!'.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM busca_dados .
  DATA: wl_zimp_cad_imposto TYPE zimp_cad_imposto,
        tl_zimp_cad_imp_con TYPE TABLE OF zimp_cad_imp_con WITH HEADER LINE,
        tl_zimp_campos_guia TYPE TABLE OF zimp_campos_guia WITH HEADER LINE,
        wl_j_1bbranch       TYPE j_1bbranch,
        wl_t012             TYPE t012,
        wl_t001             TYPE t001,
        wl_bnka             TYPE bnka,
        wl_zimp_tipos_impos TYPE zimp_tipos_impos,
        wl_cont             TYPE sy-tabix,
        wl_cont_aux         TYPE sy-tabix,
        wl_cont_aux2        TYPE sy-tabix.

  IF wg_cadimp-cod_imposto IS NOT INITIAL AND wg_acao = c_modif .

    SELECT SINGLE *
         FROM t001
         INTO wl_t001
         WHERE bukrs = wg_cadimp-bukrs.
    IF sy-subrc = 0.
      MOVE: wl_t001-butxt   TO wg_cadimp-butxt.
    ENDIF.

    SELECT SINGLE *
   FROM j_1bbranch
   INTO wl_j_1bbranch
   WHERE bukrs = wg_cadimp-bukrs
   AND   branch = wg_cadimp-gsber.

    IF sy-subrc = 0.
      MOVE: wl_j_1bbranch-name   TO wg_cadimp-namefil.
    ENDIF.

    SELECT SINGLE *
      FROM zimp_tipos_impos
      INTO wl_zimp_tipos_impos
      WHERE tp_arrec = wg_cadimp-tp_imposto.

    IF sy-subrc = 0.
      MOVE: wl_zimp_tipos_impos-arrecadacao   TO wg_cadimp-de_imposto.
    ENDIF.

    SELECT SINGLE *
    FROM t012
    INTO wl_t012
    WHERE bukrs = wg_cadimp-bukrs
    AND   hbkid = wg_cadimp-hbkid.

    IF sy-subrc = 0.
      SELECT SINGLE *
        FROM bnka
        INTO wl_bnka
        WHERE bankl = wl_t012-bankl .
      IF sy-subrc = 0.
        MOVE wl_bnka-banka TO wg_cadimp-banka.
      ENDIF.
    ENDIF.
  ENDIF.
  IF wg_cadimp-cod_imposto IS NOT INITIAL AND wg_acao IS INITIAL .
    SELECT SINGLE *
      FROM zimp_cad_imposto
      INTO wl_zimp_cad_imposto
       WHERE  cod_imposto EQ wg_cadimp-cod_imposto.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº de Imposto não encontrado!'.
      LEAVE TO SCREEN 100.
    ELSEIF wl_zimp_cad_imposto-loekz IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº de Imposto foi eliminado!'.
      LEAVE TO SCREEN 100.
    ELSE.
      MOVE-CORRESPONDING wl_zimp_cad_imposto TO wg_cadimp.
      SELECT SINGLE *
        FROM t001
        INTO wl_t001
        WHERE bukrs = wl_zimp_cad_imposto-bukrs.

      IF sy-subrc = 0.
        MOVE: wl_t001-butxt   TO wg_cadimp-butxt.
      ENDIF.
      REFRESH: tg_editor.
      CLEAR: wl_cont_aux2, wl_cont_aux, wl_cont.
      wl_cont = strlen( wl_zimp_cad_imposto-ref_imposto ).
      wl_cont_aux = wl_cont / 72.

      DO.
        MOVE: wl_zimp_cad_imposto-ref_imposto+wl_cont_aux2 TO wg_editor-line.
        ADD 72 TO wl_cont_aux2.
        APPEND wg_editor TO tg_editor.

        IF wl_cont_aux2 GT wl_cont.
          EXIT.

        ENDIF.
      ENDDO.
      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
      "
      SELECT *
        FROM zimp_cad_imp_con
        INTO TABLE tl_zimp_cad_imp_con
         WHERE  cod_imposto EQ wg_cadimp-cod_imposto.
      "
      SELECT *
      FROM zimp_campos_guia
      INTO TABLE tl_zimp_campos_guia
      FOR ALL ENTRIES IN tl_zimp_cad_imp_con
      WHERE cod_camp_guia = tl_zimp_cad_imp_con-cod_abertura.

      SELECT SINGLE *
       FROM j_1bbranch
       INTO wl_j_1bbranch
       WHERE bukrs = wl_zimp_cad_imposto-bukrs
       AND   branch = wl_zimp_cad_imposto-gsber.

      IF sy-subrc = 0.
        MOVE: wl_j_1bbranch-name   TO wg_cadimp-namefil.
      ENDIF.

      SELECT SINGLE *
        FROM zimp_tipos_impos
        INTO wl_zimp_tipos_impos
        WHERE tp_arrec = wl_zimp_cad_imposto-tp_imposto.

      IF sy-subrc = 0.
        MOVE: wl_zimp_tipos_impos-arrecadacao   TO wg_cadimp-de_imposto.
      ENDIF.

      SELECT SINGLE *
      FROM t012
      INTO wl_t012
      WHERE bukrs = wg_cadimp-bukrs
      AND   hbkid = wl_zimp_cad_imposto-hbkid.

      IF sy-subrc = 0.
        SELECT SINGLE *
          FROM bnka
          INTO wl_bnka
          WHERE bankl = wl_t012-bankl .
        IF sy-subrc = 0.
          MOVE wl_bnka-banka TO wg_cadimp-banka.
        ENDIF.
      ENDIF.

      SORT tl_zimp_campos_guia BY cod_camp_guia.

      REFRESH: tg_itens.
      LOOP AT tl_zimp_cad_imp_con.
        MOVE-CORRESPONDING tl_zimp_cad_imp_con TO tg_itens.

        READ TABLE tl_zimp_campos_guia
          WITH KEY cod_camp_guia = tl_zimp_cad_imp_con-cod_abertura.

        MOVE tl_zimp_campos_guia-descr_camp_guia TO tg_itens-descr_camp_guia.
        APPEND tg_itens.
        CLEAR: tg_itens.
      ENDLOOP.
    ENDIF.
    CLEAR wg_acao.
    REFRESH: tg_fields.
    PERFORM trata_campos USING space
                               'GR2'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                              'GR1'
                               c_1       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.
ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ELIMINAR_IMPOSTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM eliminar_imposto .
  DATA: wl_zimp_cad_imposto TYPE zimp_cad_imposto.

  SELECT  SINGLE *
    FROM zimp_cad_imposto
    INTO wl_zimp_cad_imposto
     WHERE cod_imposto EQ wg_cadimp-cod_imposto.

  IF sy-subrc IS INITIAL.
    IF wl_zimp_cad_imposto-loekz IS INITIAL.
      MOVE: c_x TO wl_zimp_cad_imposto-loekz.
      MODIFY zimp_cad_imposto FROM wl_zimp_cad_imposto.
      MESSAGE s836(sd) WITH 'O documento foi eliminado!'.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Impossivel eliminar, o documento'
                            'já foi marcado para eliminação!'.
    ENDIF.
  ENDIF.
ENDFORM.                    " ELIMINAR_IMPOSTO
*&---------------------------------------------------------------------*
*&      Module  SEARCH_BANCO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_banco INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_banco OCCURS 0,
          hbkid TYPE zimp_cad_imposto-hbkid,
          text1 TYPE t012t-text1,
        END OF tl_banco.

  SELECT hbkid text1
    FROM t012t
    INTO TABLE tl_banco
    WHERE hbkid IN ('BBD','BBRA').

  SORT tl_banco BY hbkid.
  DELETE ADJACENT DUPLICATES FROM tl_banco COMPARING ALL FIELDS.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'HBKID'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_CAD_IMPOSTO-HBKID'
      value_org       = 'S'
    TABLES
      value_tab       = tl_banco
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_BANCO  INPUT
*&---------------------------------------------------------------------*
*&      Module  INICIALIZA_TELA200  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE inicializa_tela200 OUTPUT.
  IF wg_acao IS INITIAL.
    REFRESH: tg_fields.
    PERFORM trata_campos USING space
                               'GR2'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                              'GR1'
                               c_1       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.
ENDMODULE.                 " INICIALIZA_TELA200  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  BUS_BAN  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE bus_ban INPUT.
  IF wg_cadimp-conv_banco = 'X'.
    wg_cadimp-hbkid = 'BBD'.
  ELSE.
    wg_cadimp-hbkid = 'BBRA'.
  ENDIF.
ENDMODULE.                 " BUS_BAN  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_depto INPUT.
*DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
*           tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_dep OCCURS 0,
          dep_resp TYPE zimp_cad_imposto-dep_resp,
          text1    TYPE t012t-text1,
        END OF tl_dep.
  REFRESH tl_dep.
  CLEAR tl_dep.
  tl_dep-dep_resp = '01'.
  tl_dep-text1    = 'Tributos Indiretos'.
  APPEND tl_dep.

  CLEAR tl_dep.
  tl_dep-dep_resp = '02'.
  tl_dep-text1    = 'Tributos Diretos'.
  APPEND tl_dep.

  CLEAR tl_dep.
  tl_dep-dep_resp = '03'.
  tl_dep-text1    = 'Recursos Humanos'.
  APPEND tl_dep.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DEP_RESP'
      dynpprog        = sy-repid                            "'ZFINR018'
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_CAD_IMPOSTO-DEP_RESP'
      value_org       = 'S'
    TABLES
      value_tab       = tl_dep
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_DEPTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  SEARCH_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE search_filial INPUT.
  DATA: BEGIN OF tl_branch OCCURS 0,
          branch TYPE j_1bbranch-branch,
          name   TYPE j_1bbranch-name,
        END OF tl_branch.

  DATA: l_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE.
  REFRESH l_dynpfields.
  CLEAR   l_dynpfields.
  IF wg_cadimp-bukrs IS  INITIAL.
    l_dynpfields-fieldname  = 'WG_CADIMP-BUKRS'.
    APPEND l_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = l_dynpfields.
    READ TABLE l_dynpfields INDEX 1.
    MOVE l_dynpfields-fieldvalue TO wg_cadimp-bukrs.
  ENDIF.

  SELECT  branch name
    FROM j_1bbrancht
    INTO TABLE tl_branch
  WHERE bukrs = wg_cadimp-bukrs
    and language = sy-langu.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BRANCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_CAD_IMPOSTO-GSBER'
      value_org       = 'S'
    TABLES
      value_tab       = tl_branch
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_FILIAL  INPUT

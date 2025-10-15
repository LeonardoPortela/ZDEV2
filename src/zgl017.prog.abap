*&---------------------------------------------------------------------*
*& Report  zgl017
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zgl017.
TYPES: BEGIN OF ty_cadlote,
         empresa(30)        TYPE c,
         lote(50)           TYPE c,
         usuario(20)        TYPE c,
         total              TYPE zglt036-vlr_moeda_int,
         dep_resp(2),
         data(10),
         " Seguro
         tp_opr(30),
         vig_de             TYPE zglt050-vig_de,
         vig_ate            TYPE zglt050-vig_ate,
         cod_seguradora(50),
         seq_parc           TYPE zglt050-seq_parc,
         seq_tipo(45),
         observacao         TYPE zglt050-observacao,
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

       BEGIN OF ty_docs ,
         doc_lcto        TYPE zglt035-doc_lcto,
         bukrs           TYPE zglt035-bukrs,
         tp_lcto         TYPE zglt035-tp_lcto,
         descricao       TYPE zglt031-descricao,
         lote            TYPE zglt035-lote,
         moeda_doc       TYPE zglt035-moeda_doc,
         vlr_moeda_doc   TYPE zglt036-vlr_moeda_doc,
         vlr_moeda_int   TYPE zglt036-vlr_moeda_int,
         vlr_moeda_forte TYPE zglt036-vlr_moeda_forte,
         belnr           TYPE zib_contabil_chv-belnr,
         gjahr           TYPE zib_contabil_chv-gjahr,
       END OF ty_docs,

       BEGIN OF ty_zimp_cad_depto,
         dep_resp      TYPE zimp_cad_depto-dep_resp,
         dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
       END OF ty_zimp_cad_depto,

       BEGIN OF ty_zglt031,
         tp_lcto   TYPE zglt031-tp_lcto,
         descricao TYPE zglt031-descricao,
       END OF ty_zglt031,


       BEGIN OF ty_zglt034,
         lote        TYPE zglt034-lote,
         bukrs       TYPE zglt034-bukrs,
         descr_lote  TYPE zglt034-descr_lote,
         status_lote TYPE zglt034-status_lote,
         usnam       TYPE zglt034-usnam,
         dep_resp    TYPE zglt034-dep_resp,
         data_atual  TYPE zglt034-data_atual,
         tcode       TYPE zglt034-tcode,
         pgt_forn    TYPE zglt037-pgt_forn,
       END OF ty_zglt034,


       BEGIN OF ty_zglt035,
         doc_lcto   TYPE zglt035-doc_lcto,
         bukrs      TYPE zglt035-bukrs,
         tp_lcto    TYPE zglt035-tp_lcto,
         lote       TYPE zglt035-lote,
         dpto_resp  TYPE zglt035-dpto_resp,
         moeda_doc  TYPE zglt035-moeda_doc,
         blart      TYPE zglt035-blart,
         xblnr      TYPE zglt035-xblnr,
         bktxt      TYPE zglt035-bktxt,
         bldat      TYPE zglt035-bldat,
         budat      TYPE zglt035-budat,
         dt_lcto    TYPE zglt035-dt_lcto,
         ref_lcto   TYPE zglt035-ref_lcto,
         usnam      TYPE zglt035-usnam,
         dt_entrada TYPE zglt035-dt_entrada,
         hr_entrada TYPE zglt035-hr_entrada,
       END OF ty_zglt035,

       BEGIN OF ty_zglt036,
         lote            TYPE zglt035-lote,
         doc_lcto        TYPE zglt036-doc_lcto,
         seqitem         TYPE zglt036-seqitem,
         seqsub          TYPE zglt036-seqsub,
         tp_lcto         TYPE zglt036-tp_lcto,
         bschl           TYPE zglt036-bschl,
         hkont           TYPE zglt036-hkont,
         umskz           TYPE zglt036-umskz,
         anbwa           TYPE zglt036-anbwa,
         bewar           TYPE zglt036-bewar,
         vbund           TYPE zglt036-vbund,
         kostl           TYPE zglt036-kostl,
         prctr           TYPE zglt036-prctr,
         aufnr           TYPE zglt036-aufnr,
         matnr           TYPE zglt036-matnr,
         zuorn           TYPE zglt036-zuonr,
         sgtxt           TYPE zglt036-sgtxt,
         gsber           TYPE zglt036-gsber,
         vlr_moeda_int   TYPE zglt036-vlr_moeda_int,
         vlr_moeda_forte TYPE zglt036-vlr_moeda_forte,
         vlr_moeda_doc   TYPE zglt036-vlr_moeda_doc,
       END OF ty_zglt036,

       BEGIN OF ty_zglt037,
         bukrs     TYPE zglt037-bukrs,
         bukrs_ate TYPE zglt037-bukrs_ate,
         dep_resp  TYPE zglt037-dep_resp,
         pgt_forn  TYPE zglt037-pgt_forn,
         waers     TYPE zglt037-waers,
         nivel     TYPE zglt037-nivel,
         aprovador TYPE zglt037-aprovador,
         valor_de  TYPE zglt037-valor_de,
         valor_ate TYPE zglt037-valor_ate,
       END OF ty_zglt037,

       BEGIN OF ty_zglt038,
         bukrs     TYPE zglt038-bukrs,
         lote      TYPE zglt038-lote,
         nivel     TYPE zglt038-nivel,
         aprovador TYPE zglt038-aprovador,
         valor_de  TYPE zglt038-valor_de,
         valor_ate TYPE zglt038-valor_ate,
       END OF ty_zglt038,

       BEGIN OF ty_tbsl,
         bschl TYPE tbsl-bschl,
         shkzg TYPE tbsl-shkzg,
       END OF ty_tbsl,

       BEGIN OF ty_tcurr,
         kurst TYPE tcurr-kurst,
         fcurr TYPE tcurr-fcurr,
         tcurr TYPE tcurr-tcurr,
         gdatu TYPE tcurr-gdatu,
         ukurs TYPE tcurr-ukurs,
       END OF ty_tcurr,

       BEGIN OF ty_t005,
         land1 TYPE t005-land1,
         waers TYPE t005-waers,
       END OF   ty_t005,


       BEGIN OF ty_zib_contabil_chv,
         obj_key TYPE zib_contabil_chv-obj_key,
         belnr   TYPE zib_contabil_chv-belnr,
         bukrs   TYPE zib_contabil_chv-bukrs,
         gjahr   TYPE zib_contabil_chv-gjahr,
       END OF ty_zib_contabil_chv,

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
      wg_sub01        TYPE sy-dynnr VALUE '0140',
      btn_rej(30),



      BEGIN OF tg_lotes OCCURS 0,
        status(4),
        empresa(30)  TYPE c,
        lote         TYPE zglt034-lote,
        dep_resp(25) TYPE c,
        total        TYPE zglt036-vlr_moeda_int,
        total_d      TYPE zglt036-vlr_moeda_doc,
        total_f      TYPE zglt036-vlr_moeda_forte,
        sgtxt        TYPE zglt036-sgtxt,
        data(10),
        color(4),
      END OF tg_lotes.

DATA   dyfields LIKE dynpread OCCURS 1 WITH HEADER LINE.

** Criação de tabela dinamica
DATA: t_fieldcatalog      TYPE lvc_t_fcat,
      w_fieldcatalog      TYPE lvc_s_fcat,
      wa_layout           TYPE lvc_s_layo,
      wa_stable           TYPE lvc_s_stbl,
      wg_editor           TYPE ty_editor,
      wg_cadlote          TYPE ty_cadlote,
      wa_t005             TYPE ty_t005,
      wa_tcurr            TYPE ty_tcurr,

      wa_zglt031          TYPE ty_zglt031,
      wa_zglt034          TYPE ty_zglt034,
      wa_zglt035          TYPE ty_zglt035,
      wa_zglt036          TYPE ty_zglt036,
      wa_zglt037          TYPE ty_zglt037,
      wa_zglt038          TYPE ty_zglt038,
      wa_zimp_cad_depto   TYPE ty_zimp_cad_depto,

      wa_tbsl             TYPE ty_tbsl,
      wa_t001             TYPE ty_t001,
      wa_estra            TYPE zfi_estrategia_imp, "TY_ESTRA,
      wa_docs             TYPE ty_docs,
      w_docs              TYPE ty_docs,
      wa_zib_contabil_chv TYPE ty_zib_contabil_chv,

      tg_fields           TYPE TABLE OF ty_fields   WITH HEADER LINE,

      tg_editor           TYPE TABLE OF ty_editor,
      tg_estra            TYPE TABLE OF zfi_estrategia_imp, "TY_ESTRA,
      tg_docs             TYPE TABLE OF ty_docs,
      it_docs             TYPE TABLE OF ty_docs,


      it_zglt031          TYPE TABLE OF ty_zglt031,
      it_zglt034          TYPE TABLE OF ty_zglt034,
      it_zglt035          TYPE TABLE OF ty_zglt035,
      it_zglt036          TYPE TABLE OF ty_zglt036,
      it_zglt037          TYPE TABLE OF ty_zglt037,
      it_zglt038          TYPE TABLE OF ty_zglt038,
      it_zimp_cad_depto   TYPE TABLE OF ty_zimp_cad_depto,
      it_tbsl             TYPE TABLE OF ty_tbsl,
      it_t001             TYPE TABLE OF ty_t001,
      it_t005             TYPE TABLE OF ty_t005,
      t_tcurr             TYPE TABLE OF ty_tcurr,
      it_estra            TYPE TABLE OF zfi_estrategia_imp, "TY_ESTRA,

      tg_msg_ret          TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC3',
           END OF c_tab_strip_imp.

CONSTANTS: c_tcode TYPE zglt034-tcode VALUE 'ZGL059'.
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
          vlifnr      TYPE lfa1-lifnr,
          vflg_ico(1).

    DATA: idd07v TYPE TABLE OF  dd07v.
    DATA: widd07v TYPE  dd07v.
    DATA  vdomvalue_l TYPE  dd07v-domvalue_l.

    CLEAR vdep_resp.
    vvalor_ate = 0.
    IF e_row GT 0.
      wg_sub01 = '0140'.
      CLEAR: wg_cadlote.
      REFRESH tg_estra.
      REFRESH tg_docs.
      READ TABLE tg_lotes INTO wl_lotes INDEX e_row.
      IF wl_lotes-status = icon_alert.
        MESSAGE TEXT-i01 TYPE 'I'.
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
*      READ TABLE IT_ZGLT034 INTO WA_ZGLT034 WITH KEY LOTE = WL_LOTES-LOTE BINARY SEARCH.
      SELECT SINGLE  lote bukrs descr_lote status_lote usnam dep_resp data_atual tcode
        FROM zglt034 INTO wa_zglt034
        WHERE lote = wl_lotes-lote.

      SELECT SINGLE waers
        FROM zglt037
        INTO wa_zglt037-waers
        WHERE dep_resp   = wa_zglt034-dep_resp.

      wg_cadlote-empresa  = wl_lotes-empresa.
      CONCATENATE  wl_lotes-lote '-' wa_zglt034-descr_lote INTO wg_cadlote-lote.
      wg_cadlote-usuario  = wa_zglt034-usnam.
      IF wa_zglt037-waers NE 'USD'.
        wg_cadlote-total    = wl_lotes-total.
      ELSE.
        wg_cadlote-total    = wl_lotes-total_f.
      ENDIF.
      wg_cadlote-dep_resp = wl_lotes-dep_resp+0(2).
      " WG_SUB01 = '0150'.
      SELECT SINGLE *
        FROM zglt050
        INTO @DATA(wzgl050)
        WHERE lote = @wl_lotes-lote.
      IF sy-subrc = 0.
        wg_sub01 = '0150'.
        CALL FUNCTION 'DD_DOMVALUES_GET'
          EXPORTING
            domname        = 'ZOPR_SEG'
            text           = 'X'
            langu          = sy-langu
          TABLES
            dd07v_tab      = idd07v
          EXCEPTIONS
            wrong_textflag = 1
            OTHERS         = 2.
        vdomvalue_l = wzgl050-tp_opr.
        READ TABLE idd07v INTO widd07v WITH KEY domvalue_l = vdomvalue_l.
        CONCATENATE wzgl050-tp_opr '-' widd07v-ddtext INTO wg_cadlote-tp_opr.
        "
        wg_cadlote-vig_de          = wzgl050-vig_de.
        wg_cadlote-vig_ate         = wzgl050-vig_ate.
        vlifnr = wzgl050-cod_seguradora.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vlifnr
          IMPORTING
            output = vlifnr.

        SELECT SINGLE *
          FROM lfa1
          INTO @DATA(wlfa1)
          WHERE lifnr = @vlifnr.
        CONCATENATE wzgl050-cod_seguradora '-' wlfa1-name1 INTO wg_cadlote-cod_seguradora.
        "
        wg_cadlote-seq_parc        = wzgl050-seq_parc.

        CLEAR wg_cadlote-seq_tipo.
        SELECT SINGLE descr
          INTO wg_cadlote-seq_tipo
          FROM zglt064
          WHERE seq_tipo = wzgl050-seq_tipo.

        CONCATENATE wzgl050-seq_tipo '-' wg_cadlote-seq_tipo INTO wg_cadlote-seq_tipo.

        wg_cadlote-observacao      = wzgl050-observacao.
      ENDIF.

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

  METHOD on_click2.
    DATA: wl_docs   LIKE LINE OF tg_docs,
          vg_lote   TYPE zglt034-lote,
          lv_zgl019 TYPE c.
    IF e_row_id GT 0.
      IF e_column_id = 'DOC_LCTO'.
        CLEAR vg_lote.

        EXPORT lv_zgl019 FROM abap_true TO MEMORY ID 'ZGL019'.

        READ TABLE tg_docs INTO wl_docs INDEX e_row_id.
        SET PARAMETER ID 'BLN' FIELD wl_docs-doc_lcto.
        SET PARAMETER ID 'LOT' FIELD  vg_lote.
        CALL TRANSACTION 'ZGL016A' AND SKIP FIRST SCREEN.
        FREE MEMORY ID 'ZGL019'.
      ELSEIF e_column_id = 'BELNR'.
        SET PARAMETER ID 'BLN' FIELD wl_docs-belnr.
        SET PARAMETER ID 'BUK' FIELD wg_cadlote-empresa+0(4).
        SET PARAMETER ID 'GJR' FIELD wl_docs-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "ON_DOUBLE_CLICK2
  METHOD on_click.
    DATA: v_msg    TYPE char50,
          t_lotes  TYPE TABLE OF zfi_lotes_imp,
          w_lotes  TYPE          zfi_lotes_imp,
          wl_estra LIKE LINE OF tg_estra,
          t_estra  TYPE TABLE OF zfi_estrategia_imp,
          w_estra  TYPE          zfi_estrategia_imp,
          westra   LIKE LINE OF tg_estra,
          t_docs   TYPE TABLE OF zgl_docs_imp,
          w_docs   TYPE          zgl_docs_imp,
          wl_lotes LIKE LINE OF tg_lotes,
          lv_stop  TYPE c.

    CLEAR lv_stop.

    IF e_row_id GT 0.
      READ TABLE tg_estra INTO wl_estra INDEX e_row_id.

      READ TABLE tg_lotes INTO wl_lotes WITH KEY lote = wl_estra-lote BINARY SEARCH.
      MOVE-CORRESPONDING wl_lotes TO w_lotes.
      APPEND w_lotes  TO t_lotes.

      LOOP AT it_estra INTO westra WHERE lote = wl_lotes-lote.
        MOVE-CORRESPONDING westra TO w_estra.
        w_estra-nivelc = wl_estra-nivel.
        APPEND w_estra TO t_estra.
      ENDLOOP.

      PERFORM checa_doc CHANGING lv_stop.

      CHECK lv_stop NE abap_true.
      CALL FUNCTION 'Z_GL_ESTRATEGIA_EXECUTAR'
        EXPORTING
          v_usuario = sy-uname
        IMPORTING
          msg       = v_msg
        TABLES
          t_lotes   = t_lotes
          t_estra   = t_estra.

      LOOP AT t_estra INTO w_estra
        WHERE aprovador EQ sy-uname.                    "Modificação CS 2016000820
        "        IF E_ROW_ID = SY-TABIX.                "Modificação CS 2016000820
        MOVE: w_estra-opcoes TO wl_estra-opcoes,
              w_estra-estado TO wl_estra-estado.
        MODIFY tg_estra FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes estado.
        "        ENDIF.
      ENDLOOP.

      MESSAGE s836(sd) DISPLAY LIKE 'W' WITH v_msg .

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
  txtemp = TEXT-l01.
  txtlot = TEXT-l02.
  txtusu = TEXT-l03.
  txtval = TEXT-l04.
  btn_rej = TEXT-b01.
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
        1 ' '                    ' '                'TG_LOTES' 'STATUS'           ' '             '03' ' ' ' ' ' ',
        2 ' '                    ' '                'TG_LOTES' 'EMPRESA'          TEXT-a01        '15' ' ' ' ' ' ',
        3 'ZGLT034'              'LOTE'             'TG_LOTES' 'LOTE'             TEXT-a02        '08' ' ' ' ' ' ',
        4 ' '                    ' '                'TG_LOTES' 'DEP_RESP'         TEXT-a03        '14' ' ' ' ' ' ',
        6 'ZGLT036'              'VLR_MOEDA_DOC'    'TG_LOTES' 'TOTAL_D'          TEXT-a18        '12' ' ' ' ' ' ',
        6 'ZGLT036'              'VLR_MOEDA_FORTE'  'TG_LOTES' 'TOTAL_F'          TEXT-a15        '12' ' ' ' ' ' ',
        6 'ZGLT036'              'VLR_MOEDA_INT'    'TG_LOTES' 'TOTAL'            TEXT-a14        '12' ' ' ' ' ' ',
        6 'ZGLT036'              'SGTXT'            'TG_LOTES' 'SGTXT'            TEXT-a05        '20' ' ' ' ' ' '.


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
*&      Form  F_ESTRA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_estra USING p_bukrs p_lote p_dep_resp p_total.
  DATA vhbkid TYPE zglt036-hbkid.
  DATA vzlsch TYPE zglt036-zlsch.
  REFRESH tg_estra.
  "Estratégia salva
  REFRESH it_zglt038.
  SELECT bukrs lote nivel aprovador valor_de valor_ate
    FROM zglt038
    INTO TABLE it_zglt038
    WHERE  lote  =  p_lote.

  CLEAR vhbkid.
  SELECT SINGLE zglt036~hbkid
    FROM zglt035
    INNER JOIN zglt036
    ON  zglt036~doc_lcto = zglt035~doc_lcto
    AND zglt036~hbkid NE space
    INTO vhbkid
    WHERE zglt035~lote EQ p_lote.

  CLEAR vzlsch.
  SELECT SINGLE zglt036~zlsch
    FROM zglt035
    INNER JOIN zglt036
    ON  zglt036~doc_lcto = zglt035~doc_lcto
    AND zglt036~zlsch NE space
    INTO vzlsch
    WHERE zglt035~lote EQ p_lote.

  CLEAR wa_zglt034-pgt_forn.
  IF vhbkid IS NOT INITIAL OR vzlsch IS NOT INITIAL.
    wa_zglt034-pgt_forn = 'X'.
  ENDIF.

  SORT it_zglt038 BY nivel aprovador.

  SELECT  bukrs bukrs_ate dep_resp pgt_forn waers nivel aprovador valor_de valor_ate
    FROM zglt037
    INTO TABLE it_zglt037
    WHERE bukrs     LE p_bukrs
    AND   bukrs_ate GE p_bukrs.

  SORT it_zglt037 BY dep_resp pgt_forn bukrs bukrs_ate dep_resp nivel.
  vflg_ico = 'N'.
  CLEAR: vdep_resp, vpgt_forn.
  vvalor_ate = 0.
  LOOP AT it_zglt037 INTO wa_zglt037 WHERE dep_resp = p_dep_resp
                                     AND   pgt_forn = wa_zglt034-pgt_forn.
    IF  wa_zglt037-bukrs_ate IS INITIAL.
      IF  wa_zglt037-bukrs NE p_bukrs.
        CONTINUE.
      ENDIF.
    ELSEIF wa_zglt037-bukrs     GT p_bukrs OR
           wa_zglt037-bukrs_ate LT p_bukrs.
      CONTINUE.
    ENDIF.
    IF p_dep_resp+0(2) = wa_zglt037-dep_resp.
      IF p_total > vvalor_ate.
        vvalor_ate = wa_zglt037-valor_ate.
        vdep_resp = wa_zglt037-dep_resp.
        vpgt_forn = wa_zglt037-pgt_forn.
      ENDIF.
    ENDIF.
  ENDLOOP.
  IF vdep_resp IS INITIAL.
    LOOP AT it_zglt037 INTO wa_zglt037 WHERE pgt_forn = wa_zglt034-pgt_forn.
      IF  wa_zglt037-bukrs_ate IS INITIAL.
        IF  wa_zglt037-bukrs NE p_bukrs.
          CONTINUE.
        ENDIF.
      ELSEIF wa_zglt037-bukrs     GT p_bukrs OR
             wa_zglt037-bukrs_ate LT p_bukrs.
        CONTINUE.
      ENDIF.
      IF wa_zglt037-dep_resp IS INITIAL.
        IF p_total > vvalor_ate.
          vvalor_ate = wa_zglt037-valor_ate.
          vdep_resp = wa_zglt037-dep_resp.
          vpgt_forn = wa_zglt037-pgt_forn.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

  LOOP AT it_zglt037 INTO wa_zglt037 WHERE dep_resp = vdep_resp
                                     AND   pgt_forn = vpgt_forn.
    IF  wa_zglt037-bukrs_ate IS INITIAL.
      IF  wa_zglt037-bukrs NE p_bukrs.
        CONTINUE.
      ENDIF.
    ELSEIF wa_zglt037-bukrs     GT p_bukrs OR
           wa_zglt037-bukrs_ate LT p_bukrs.
      CONTINUE.
    ENDIF.
    IF wa_zglt037-valor_ate <= vvalor_ate.
      wa_estra-bukrs        = p_bukrs.
      wa_estra-lote         = p_lote.
      wa_estra-valor_de     = wa_zglt037-valor_de.
      wa_estra-valor_ate    = wa_zglt037-valor_ate.
      wa_estra-aprovador    = wa_zglt037-aprovador.
      wa_estra-nivel        = wa_zglt037-nivel.
      wa_estra-waers        = wa_zglt037-waers.

      READ TABLE it_zglt038 INTO wa_zglt038 WITH KEY nivel     = wa_zglt037-nivel
                                                     aprovador = wa_zglt037-aprovador BINARY SEARCH.
      IF sy-subrc = 0.
        wa_estra-estado       = icon_checked .
        wa_estra-opcoes       = icon_system_undo .
        vflg_ico = 'N'.
      ELSEIF vflg_ico = 'S'.
        wa_estra-estado       = icon_led_yellow .
        wa_estra-opcoes       = '' .
      ELSE.
        IF sy-uname NE wa_zglt037-aprovador.
          wa_estra-estado       =  ' '.
          wa_estra-opcoes       = icon_led_yellow  .
        ELSE.
          wa_estra-estado       = icon_led_yellow .
          wa_estra-opcoes       = icon_set_state  .
        ENDIF.
        vflg_ico = 'X'.
      ENDIF.

      IF vdep_resp IS INITIAL AND wa_zglt037-dep_resp IS INITIAL.
        IF vflg_ico = 'X'.
          vflg_ico = 'S'.
        ENDIF.
        APPEND wa_estra TO tg_estra.
      ELSEIF vdep_resp = wa_zglt037-dep_resp.
        IF vflg_ico = 'X'.
          vflg_ico = 'S'.
        ENDIF.
        APPEND wa_estra TO tg_estra.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "F_ESTRA
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
        1 'ZGLT037'           'VALOR_DE'        'TG_ESTRA' 'VALOR_DE'         TEXT-a06      '15' ' ' ' ' ' ',
        1 'ZGLT037'           'VALOR_ATE'       'TG_ESTRA' 'VALOR_ATE'        TEXT-a07      '15' ' ' ' ' ' ',
        1 'ZGLT037'           'WAERS'           'TG_ESTRA' 'WAERS'            TEXT-a16      '05' ' ' ' ' ' ',
        1 'ZGLT037'           'APROVADOR'       'TG_ESTRA' 'APROVADOR'        TEXT-a08      '13' ' ' ' ' ' ',
        1 ' '                        ' '        'TG_ESTRA' 'ESTADO'           TEXT-a09      '05' ' ' ' ' ' ',
        1 ' '                        ' '        'TG_ESTRA' 'OPCOES'           TEXT-a10      '08' ' ' ' ' ' '.

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
  CLEAR w_docs.
  IF tg_docs[] IS NOT INITIAL.
    READ TABLE tg_docs INTO w_docs INDEX 1.
  ENDIF.
  PERFORM montar_estrutura USING:
        1 'ZGLT035'           'DOC_LCTO'        'TG_DOCS'  'DOC_LCTO'            TEXT-a11         '20' ' ' ' ' ' ',
        1 'ZGLT035'           'TP_LCTO'         'TG_DOCS'  'TP_LCTO'             TEXT-a12         '20' ' ' ' ' ' ',
        1 'ZGLT031'           'DESCRICAO'       'TG_DOCS'  'DESCRICAO'           TEXT-a13         '30' ' ' ' ' ' ',
        3 'ZGLT035'           'BUKRS'           'TG_DOCS'  'BUKRS'               TEXT-a01         '15' ' ' ' ' ' ',
        3 'ZGLT035'           'MOEDA_DOC'       'TG_DOCS'  'MOEDA_DOC'           TEXT-a16         '06' ' ' ' ' ' '.

  IF w_docs-bukrs = '0201'.
    PERFORM montar_estrutura USING:
      3 'ZGLT036'           'VLR_MOEDA_DOC'   'TG_DOCS'  'VLR_MOEDA_DOC'       TEXT-a18         '15' ' ' 'X' ' ',
      3 'ZGLT036'           'VLR_MOEDA_FORTE' 'TG_DOCS'  'VLR_MOEDA_FORTE'     TEXT-a15         '15' ' ' 'X' ' ',
      3 'ZGLT036'           'VLR_MOEDA_INT'   'TG_DOCS'  'VLR_MOEDA_INT'       TEXT-a14         '15' ' ' 'X' ' ',
      3 ' '                 ' '               'TG_DOCS'  'BELNR'               TEXT-a17         '15' ' ' ' ' ' '.
  ELSE.
    PERFORM montar_estrutura USING:
        3 'ZGLT036'           'VLR_MOEDA_DOC'   'TG_DOCS'  'VLR_MOEDA_DOC'       TEXT-a18         '15' ' ' 'X' ' ',
        3 'ZGLT036'           'VLR_MOEDA_INT'   'TG_DOCS'  'VLR_MOEDA_INT'       TEXT-a14         '15' ' ' 'X' ' ',
        3 'ZGLT036'           'VLR_MOEDA_FORTE' 'TG_DOCS'  'VLR_MOEDA_FORTE'     TEXT-a15         '15' ' ' 'X' ' ',
        3 ' '                 ' '               'TG_DOCS'  'BELNR'               TEXT-a17         '15' ' ' ' ' ' '.
  ENDIF.



ENDFORM.                    " MONTAR_LAYOUT_DOCS
*&---------------------------------------------------------------------*
*&      Module  CARREGA_LOTES  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE carrega_lotes OUTPUT.
  DATA: xtotal  TYPE zglt036-vlr_moeda_int,
        xtotali TYPE zglt036-vlr_moeda_doc,
        xtotald TYPE zglt036-vlr_moeda_doc.

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
  DATA: v_msg     TYPE char50,
        t_lotes   TYPE TABLE OF zfi_lotes_imp,
        w_lotes   TYPE          zfi_lotes_imp,
        t_estra   TYPE TABLE OF zfi_estrategia_zgl,
        w_estra   TYPE          zfi_estrategia_zgl,
        t_docs    TYPE TABLE OF zgl_docs_imp,
        w_docs    TYPE          zgl_docs_imp,
        vdata(10),
        vryear    TYPE faglflexa-ryear,
        tabix     TYPE sy-tabix.

  CALL FUNCTION 'Z_GL_ESTRATEGIA_LISTA'
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
    MOVE w_lotes-dt_venc TO vdata.
    MOVE-CORRESPONDING w_lotes TO tg_lotes.
    "Busca moeda forte
    CONCATENATE vdata+6(4) vdata+3(2) vdata+0(2) INTO tg_lotes-data.
    APPEND tg_lotes.
  ENDLOOP.

  IF tg_lotes[] IS NOT INITIAL.
    REFRESH it_zglt036.
    SELECT  lote zglt036~doc_lcto seqitem seqsub zglt036~tp_lcto bschl hkont umskz anbwa bewar vbund kostl prctr aufnr matnr zuonr sgtxt gsber
          vlr_moeda_int vlr_moeda_forte vlr_moeda_doc
    FROM zglt035
    INNER JOIN zglt036
    ON zglt036~doc_lcto = zglt035~doc_lcto
    INTO TABLE it_zglt036
    FOR ALL ENTRIES IN tg_lotes
    WHERE lote EQ tg_lotes-lote
    AND   loekz EQ ''.

    IF  it_zglt036[] IS NOT INITIAL.
      SELECT bschl shkzg
         FROM tbsl
         INTO TABLE it_tbsl
         FOR ALL ENTRIES IN it_zglt036
         WHERE bschl EQ it_zglt036-bschl..
    ENDIF.

    SORT: it_tbsl    BY bschl,
          it_zglt036 BY lote.

    LOOP AT tg_lotes.
      CLEAR: xtotald, xtotal, xtotali.
      tabix = sy-tabix.

      SELECT SINGLE *
      FROM zglt034 INTO @DATA(wl_zglt034)
        WHERE lote EQ @tg_lotes-lote.

      IF wl_zglt034-tcode = 'FBB1'.
        vryear = wl_zglt034-data_atual+0(4).
        SELECT SUM( osl ) SUM( wsl ) SUM( hsl )
          FROM faglflexa INTO ( xtotal, xtotald, xtotali )
            WHERE ryear   EQ vryear
              AND docnr   EQ wl_zglt034-lote
              AND rbukrs  EQ wl_zglt034-bukrs
              AND rldnr   EQ '50'
              AND drcrk   EQ 'S'.


      ELSE.

        LOOP AT it_zglt036 INTO wa_zglt036 WHERE lote = tg_lotes-lote.
          READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_zglt036-bschl BINARY SEARCH.
          IF wa_tbsl-shkzg NE 'H'.
            IF wa_zglt036-vlr_moeda_int GT 0.
              ADD wa_zglt036-vlr_moeda_int TO xtotali .
            ENDIF.

            IF wa_zglt036-vlr_moeda_forte GT 0.
              ADD wa_zglt036-vlr_moeda_forte TO xtotal .
            ENDIF.

            IF wa_zglt036-vlr_moeda_doc GT 0.
              ADD wa_zglt036-vlr_moeda_doc TO xtotald.
            ENDIF.
          ENDIF.
        ENDLOOP.

      ENDIF.
      tg_lotes-total   = xtotali.
      tg_lotes-total_f = xtotal.
      tg_lotes-total_d = xtotald.
      MODIFY tg_lotes INDEX tabix TRANSPORTING total_f total_d  total.
    ENDLOOP.

    SORT tg_lotes BY lote .

    LOOP AT t_estra INTO w_estra.
      MOVE-CORRESPONDING w_estra TO wa_estra.
      APPEND wa_estra TO it_estra.
    ENDLOOP.
    SORT it_estra BY lote nivel.

    SORT   it_zglt036 BY doc_lcto.
    LOOP AT t_docs INTO w_docs.
      MOVE-CORRESPONDING w_docs TO wa_docs.
      CONCATENATE 'ZGL17' w_docs-doc_lcto '*'  INTO wa_zib_contabil_chv-obj_key.
      SELECT SINGLE obj_key belnr bukrs gjahr
      FROM zib_contabil_chv
      INTO wa_zib_contabil_chv
      WHERE obj_key LIKE wa_zib_contabil_chv-obj_key.

      IF sy-subrc = 0.
        wa_docs-belnr = wa_zib_contabil_chv-belnr.
        wa_docs-gjahr = wa_zib_contabil_chv-gjahr.
      ENDIF.
      "novo 27.06.2017
      CLEAR  xtotald.
      LOOP AT it_zglt036 INTO wa_zglt036 WHERE doc_lcto = w_docs-doc_lcto.
        READ TABLE it_tbsl INTO wa_tbsl WITH KEY bschl = wa_zglt036-bschl BINARY SEARCH.
        IF wa_tbsl-shkzg NE 'H'.
          ADD wa_zglt036-vlr_moeda_doc TO xtotald.
        ENDIF.
      ENDLOOP.
      wa_docs-vlr_moeda_doc = xtotald.

      APPEND wa_docs TO it_docs.
    ENDLOOP.
    SORT it_docs BY lote .
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
*&      Form  GRAVA_ZIB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_zib USING p_lote.
  DATA: wl_setleaf TYPE setleaf,
        i_head     TYPE tbtcjob.

  DATA:   wl_job_id   LIKE tbtcjob-jobcount.
  DATA:   wl_jobn(32).

  DATA: BEGIN OF i_steplist OCCURS 10.
          INCLUDE STRUCTURE tbtcstep.
  DATA: END OF i_steplist.
  DATA : c_no(1) TYPE c . "value 'N', " Criação do job

  DATA: wl_tbtcjob  TYPE  tbtcjob,
        wl_tbtcstrt TYPE  tbtcstrt.

  DATA: lv_repname LIKE  rsvar-report.           " for variant handling
  DATA: iv_varname LIKE  raldb-variant VALUE 'SAP_UPGRADE'.
  DATA: iv_varianttext  LIKE  varit-vtext VALUE 'Upgrade variant'.
  DATA: wl_subrc TYPE sy-subrc.
  DATA: tt_reportparam TYPE TABLE OF  rsparams WITH HEADER LINE.

  SELECT SINGLE *
   FROM setleaf
   INTO wl_setleaf
    WHERE setname EQ 'MAGGI_JOB_USER'.

  IF sy-subrc NE 0.
    MESSAGE TEXT-e01 TYPE 'E'.
    EXIT.
  ENDIF.
  CONCATENATE 'Z_GRAVA_ZIB_ZGL' p_lote  INTO wl_jobn SEPARATED BY '|'.

  i_head-jobname = wl_jobn. " Nome do JOBi_head-sdlstrtdt = sy-datum. " Dia
  i_head-sdlstrttm = sy-uzeit + 20. " Hora de inícioPassa para o Job o nome da Classe de Jobs da Tabela
  i_head-stepcount = 1.

  tt_reportparam-selname = 'P_LOTE'.
  tt_reportparam-kind =  'P'.
  tt_reportparam-sign = 'I'.
  tt_reportparam-option = 'EQ'.
  tt_reportparam-low = p_lote.
  APPEND tt_reportparam.
  CLEAR tt_reportparam.

  lv_repname = 'Z_GRAVA_ZIB_ZGL'.
*    Write the variant first (Insert or Update)
  CALL FUNCTION 'SUBST_WRITE_UPGRADE_VARIANT'
    EXPORTING
      iv_reportname         = lv_repname
      iv_variantname        = iv_varname
      iv_varianttext        = iv_varianttext
    IMPORTING
      ev_funcrc             = wl_subrc
    TABLES
      tt_reportparam        = tt_reportparam
    EXCEPTIONS
      exist_check_failed    = 1
      update_failed         = 2
      update_not_authorized = 3
      update_no_report      = 4
      update_no_variant     = 5
      update_variant_locked = 6
      insert_failed         = 7
      insert_not_authorized = 8
      insert_no_report      = 9
      insert_variant_exists = 10
      insert_variant_locked = 11
      OTHERS                = 12.

  i_steplist-parameter = iv_varname. " Nome da variante
  i_steplist-program = 'Z_GRAVA_ZIB_ZGL'. " Nome do programa de INBOUNDPassa para o Job o nome da Classe de Jobs da Tabela ZTUP_SERVIDOR
  i_steplist-typ = 'A'. " Tipo de Job
  i_steplist-authcknam = wl_setleaf-valfrom.
  i_steplist-language = sy-langu.
  i_steplist-arcuser = wl_setleaf-valfrom.

  APPEND i_steplist.


  c_no = 'N'.
  CALL FUNCTION 'BP_JOB_CREATE'
    EXPORTING
      job_cr_dialog       = c_no " Coloque 'Y' se quiser ver
      job_cr_head_inp     = i_head " os valores atribuidos
    IMPORTING
      job_cr_head_out     = wl_tbtcjob
      job_cr_stdt_out     = wl_tbtcstrt
    TABLES
      job_cr_steplist     = i_steplist
    EXCEPTIONS
      cant_create_job     = 1
      invalid_dialog_type = 2
      invalid_job_data    = 3
      job_create_canceled = 4
      OTHERS              = 5.

  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobname   = wl_jobn
      jobcount  = wl_tbtcjob-jobcount
      strtimmed = 'X'.

ENDFORM.                    " GRAVA_ZIB
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
  DATA: vuser         TYPE sy-uname,
        vflag_mail(1).

  DATA:      ctotal(20),
             vdata(10),
             vbukrs TYPE zglt034-bukrs.

  DATA: it_shortcut_param LIKE zst_shortcut_par OCCURS 0 WITH HEADER LINE.
  DATA: content TYPE string.

*  ** Pass the required parameters and create the shortcut
  CLEAR it_shortcut_param.
  REFRESH it_shortcut_param.

  vbukrs = wg_cadlote-empresa+0(4).
  DATA: it_mail TYPE TABLE OF zmail WITH HEADER LINE.
  SELECT *
    FROM zmail
    INTO TABLE it_mail
    WHERE tcode = 'ZGL017'
    AND   bukrs     LE vbukrs.
*    AND   bukrs_ate GE vbukrs.(comentado por causa da mudança implementada pelo CS da Leila - 141955 - CS2024000431 Parte 1 - Transação ZMAIL - Remodelar usando padrão ZREGISTER_DATA) - SMC
  IF sy-subrc NE 0.
    SELECT *
    FROM zmail
    INTO TABLE it_mail
    WHERE tcode = 'ZGL017'.
  ENDIF.
  vflag_mail = ''.
  LOOP AT it_mail.
*    IF  it_mail-bukrs_ate IS INITIAL.(comentado por causa da mudança implementada pelo CS da Leila - 141955 - CS2024000431 Parte 1 - Transação ZMAIL - Remodelar usando padrão ZREGISTER_DATA) - SMC
      IF  it_mail-bukrs NE vbukrs.
        CONTINUE.

    ELSEIF it_mail-bukrs     GT vbukrs." OR - (comentado por causa da mudança implementada pelo CS da Leila - 141955 - CS2024000431 Parte 1 - Transação ZMAIL - Remodelar usando padrão ZREGISTER_DATA) - SMC
*           it_mail-bukrs_ate LT vbukrs.(comentado por causa da mudança implementada pelo CS da Leila - 141955 - CS2024000431 Parte 1 - Transação ZMAIL - Remodelar usando padrão ZREGISTER_DATA) - SMC
      CONTINUE.
    ENDIF.
    IF NOT it_mail-usuario IS INITIAL.
      vflag_mail = 'X'.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF it_mail-usuario IS INITIAL OR vflag_mail  IS INITIAL.
    MESSAGE TEXT-e02 TYPE 'E'.
    EXIT.
  ELSEIF it_mail[] IS INITIAL.
    MESSAGE TEXT-e03 TYPE 'E'.
    EXIT.
  ENDIF.


  READ TABLE it_zglt034 INTO wa_zglt034 WITH KEY lote = wa_estra-lote BINARY SEARCH.

* Criação do documento de Email
  doc_chng-obj_name = 'LOG_REL'.

* Assunto do Email
  CONCATENATE TEXT-m01  wg_cadlote-empresa INTO doc_chng-obj_descr SEPARATED BY space .

* Texto
  objtxt-line = TEXT-m02.
  APPEND objtxt.
  CLEAR objtxt.
  APPEND objtxt.

  objtxt-line = TEXT-m03.
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = '--------------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  WRITE wg_cadlote-total TO ctotal CURRENCY 'USD'.

  CONDENSE ctotal NO-GAPS.
  CONCATENATE TEXT-a01  wg_cadlote-empresa '' TEXT-a02 wg_cadlote-lote ' R$' ctotal  INTO objtxt SEPARATED BY space.
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

  it_shortcut_param-fieldname = 'P_BUKRS'.
  it_shortcut_param-fieldvalue = wg_cadlote-empresa+0(4).
  APPEND it_shortcut_param.

  it_shortcut_param-fieldname = 'P_LOTE-LOW'.
  it_shortcut_param-fieldvalue = wg_cadlote-lote+0(10).
  APPEND it_shortcut_param.

  CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
    EXPORTING
      recipient_user_id = it_mail-usuario
      transaction       = 'ZGL017'
    IMPORTING
      content           = content
    TABLES
      shortcut_param    = it_shortcut_param.

  CLEAR : tab_lines, objbin.
  CONCATENATE content wa_objbin-line INTO wa_objbin-line.
  APPEND  wa_objbin TO objbin.

  DESCRIBE TABLE objbin LINES tab_lines.
  objhead = 'GUIAS.SAP'.
  APPEND objhead.

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
FORM envia_email TABLES tg_estra USING VALUE(wg_cadlote) TYPE ty_cadlote plinha  .

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
  doc_chng-obj_descr = TEXT-m08 .

* Texto
  objtxt-line = TEXT-m05 .
  APPEND objtxt.
  CLEAR objtxt.
  APPEND objtxt.

  objtxt-line = TEXT-m06 .
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = '-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  DATA: ctotal(20),
        ctotalf(20),
        vdata(10),
        vwaers      TYPE t005-waers,
        vl_forte    TYPE zglt036-vlr_moeda_forte.

  READ TABLE it_zglt034 INTO wa_zglt034 WITH KEY lote = wa_estra-lote BINARY SEARCH.

*  "Valor em Interno
*  SELECT SUM( ZGLT036~VLR_MOEDA_INT )
*    INTO WG_CADLOTE-TOTAL
*    FROM ZGLT035
*    INNER JOIN ZGLT036
*    ON ZGLT035~DOC_LCTO = ZGLT036~DOC_LCTO
*    WHERE ZGLT035~LOTE = WA_ESTRA-LOTE.

*  WG_CADLOTE-TOTAL = WG_CADLOTE-TOTAL / 2.

  WRITE wg_cadlote-total TO ctotal CURRENCY 'USD'.

  SELECT SINGLE t005~waers
    INTO vwaers
    FROM t001
    INNER JOIN t005
    ON t005~land1 = t001~land1
    WHERE t001~bukrs = wg_cadlote-empresa+0(4).

  CONDENSE ctotal NO-GAPS.
  CONCATENATE TEXT-a01  wg_cadlote-empresa TEXT-a02 wg_cadlote-lote vwaers ctotal TEXT-m07  vdata INTO objtxt SEPARATED BY space.
  APPEND objtxt.
  CLEAR objtxt.

*  IF VWAERS NE 'USD'.
*    "Valor em dolar
*    SELECT SUM( ZGLT036~VLR_MOEDA_FORTE )
*      INTO VL_FORTE
*      FROM ZGLT035
*      INNER JOIN ZGLT036
*      ON ZGLT035~DOC_LCTO = ZGLT036~DOC_LCTO
*      WHERE ZGLT035~LOTE = WA_ESTRA-LOTE.
*
*    VL_FORTE = VL_FORTE / 2.
*    WRITE VL_FORTE TO CTOTALF CURRENCY 'USD'.
*    CONDENSE CTOTALF NO-GAPS.
*    CONCATENATE TEXT-A01  WG_CADLOTE-EMPRESA TEXT-A02 WG_CADLOTE-LOTE 'USD' CTOTALF TEXT-M07  VDATA INTO OBJTXT SEPARATED BY SPACE.
*    APPEND OBJTXT.
*    CLEAR OBJTXT.
*  ENDIF.

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
      transaction       = 'ZGL019'
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
    MESSAGE TEXT-i02 TYPE 'I'.
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
  REFRESH tg_docs.
  LOOP AT it_docs INTO wa_docs WHERE lote = wg_cadlote-lote+0(10).
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

*&---------------------------------------------------------------------*
*& Form checa_doc
*&---------------------------------------------------------------------*
FORM checa_doc CHANGING p_stop.

  CHECK wa_zglt034-data_atual IS NOT INITIAL.
  CHECK wa_zglt034-tcode EQ c_tcode.

  DATA(mes_atual) = sy-datum+4(2).
  DATA(ano_atual) = sy-datum(4).

  IF wa_zglt034-data_atual+4(2) NE mes_atual OR
     wa_zglt034-data_atual(4)   NE ano_atual.
    MESSAGE text-e04 TYPE 'S' DISPLAY LIKE 'E'.
    p_stop = abap_true.
  ENDIF.
ENDFORM.

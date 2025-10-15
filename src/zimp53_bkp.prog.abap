*&                        DESENVOLVIMENTO INTERNO                     &*
*&--------------------------------------------------------------------&*
*& PROJETO..: AMAGGI                                                  &*
*& AUTOR....: ANTONIO LUIZ R. DA SILVA                                &*
*& DATA.....: 15/02/2013                                              &*
*& DESCRIÇÃO: CADASTRO DE LANÇAMENTOS DE IMPOSTOS                     &*
*& TRANSAÇÃO: ZIMP53                                                  &*
*---------------------------------------------------------------------&*

REPORT  zimp53_bkp.

*&--------------------------------------------------------------------&*
*& ESTRUTURAS                                                         &*
*&--------------------------------------------------------------------&*

TABLES: zimp_cad_lote.

TYPE-POOLS: icon.

TYPES: BEGIN OF ty_cadlan,
         doc_imposto      TYPE zimp_lanc_impost-doc_imposto,
         lote             TYPE zimp_cad_lote-lote,
         descr_lote       TYPE zimp_cad_lote-descr_lote,
         cod_imposto      TYPE zimp_cad_imposto-cod_imposto,
         descr_imposto    TYPE zimp_cad_imposto-descr_imposto,
         tp_imposto       TYPE zimp_cad_imposto-tp_imposto,
         arrecadacao      TYPE zimp_tipos_impos-arrecadacao,
         ref_imposto      TYPE zimp_cad_imposto-ref_imposto,
         cod_pgto         TYPE zimp_cad_imposto-cod_pgto,
         bukrs            TYPE zimp_cad_lote-bukrs,
         butxt            TYPE t001-butxt,
         conv_banco       TYPE zimp_cad_imposto-conv_banco,
         hbkid            TYPE zimp_cad_imposto-hbkid,
         gsber            TYPE zimp_lanc_impost-gsber,
         namefil          TYPE j_1bbranch-name,
         banka            TYPE bnka-banka,
         dt_apuracao      TYPE zimp_lanc_impost-dt_apuracao,
         mes_apuracao     TYPE zimp_lanc_impost-mes_apuracao,
         ano_apuracao     TYPE zimp_lanc_impost-ano_apuracao,
         dt_venc          TYPE zimp_cad_lote-dt_venc,
         observacao       TYPE zimp_lanc_impost-observacao,
         cod_barras       TYPE zimp_lanc_impost-cod_barras,
         qrcode           TYPE zimp_lanc_impost-qrcode, "US - 128395 - CSB
         waers            TYPE zimp_lanc_impost-waers,
         waers_f          TYPE zimp_lanc_impost-waers_f,
         identificador    TYPE zimp_lanc_impost-identificador,
         doc_contabil     TYPE zib_contabil_chv-belnr,
         budat            TYPE bkpf-budat,
         usuario          TYPE zimp_lanc_impost-usuario,
         data_atual       TYPE zimp_lanc_impost-data_atual,
         hora_atual       TYPE zimp_lanc_impost-hora_atual,
         moeda_gp_hist    TYPE zimp_lanc_impost-moeda_gp_hist,
         st_fecha         TYPE zimp_lanc_impost-st_fecha,
*Inicio Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779
         zimp_lanc_impost TYPE zimp_lanc_impost-zimp_lanc_impost,
*Fim Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779
         icon(4),
         iconc(4),
       END OF ty_cadlan,

       BEGIN OF ty_zib_contabil_err,
         obj_key        TYPE zib_contabil_err-obj_key,
         nr_item        TYPE zib_contabil_err-nr_item,
         interface      TYPE zib_contabil_err-interface,
         dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
         hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
         type           TYPE zib_contabil_err-type,
         id             TYPE zib_contabil_err-id,
         num            TYPE zib_contabil_err-num,
         message        TYPE zib_contabil_err-message,
         message_v1     TYPE zib_contabil_err-message_v1,
         message_v2     TYPE zib_contabil_err-message_v2,
         message_v3     TYPE zib_contabil_err-message_v3,
         message_v4     TYPE zib_contabil_err-message_v4,
       END OF ty_zib_contabil_err,

       BEGIN OF ty_zib_contabil_chv,
         obj_key TYPE zib_contabil_chv-obj_key,
         belnr   TYPE zib_contabil_chv-belnr,
         bukrs   TYPE zib_contabil_chv-bukrs,
         gjahr   TYPE zib_contabil_chv-gjahr,
       END OF ty_zib_contabil_chv,

       BEGIN OF ty_bkpf,
         bukrs TYPE bkpf-bukrs,
         belnr TYPE bkpf-belnr,
         gjahr TYPE bkpf-gjahr,
         budat TYPE bkpf-budat,
       END OF ty_bkpf,

       BEGIN OF ty_fields,
         campo(30) TYPE c,
         group1(5) TYPE c,
         value     TYPE sy-tabix,
         invisible TYPE sy-tabix,
       END   OF ty_fields,

       BEGIN OF ty_editor,
         line(72),
       END   OF ty_editor,

       BEGIN OF ty_obj,
         mark(1),
         seqitem         TYPE zimp_cad_imp_con-seqitem,
         iva(40)         TYPE c,
         vlr_moeda_doc   TYPE zimp_cad_imp_con-vlr_moeda_doc,
         vlr_moeda_forte TYPE zimp_cad_imp_con-vlr_moeda_forte,
         quantity        TYPE zimp_cad_imp_con-quantity,
         base_uom        TYPE zimp_cad_imp_con-base_uom,
       END OF ty_obj.

*&--------------------------------------------------------------------&*
*& DECLARAÇÃO DE TABELAS E WORK AREAS                                 &*
*&--------------------------------------------------------------------&*
DATA: ok-code          TYPE sy-ucomm,
      wg_cadlan        TYPE ty_cadlan,
      wg_cadlan_cl     TYPE ty_cadlan,
      wg_cadlan_aux    TYPE ty_cadlan,
      tg_selectedcell  TYPE lvc_t_cell,
      wg_selectedcell  TYPE lvc_s_cell,
      x_field(30),
      vg_inf_identific TYPE c,
      vtexto(50),

      BEGIN OF tg_itens OCCURS 0,
        mark(1),
        check(1),
        checkbox(1),
        cod_imposto     TYPE zimp_cad_imp_con-cod_imposto,
        cod_abertura    TYPE zimp_cad_imp_con-cod_abertura,
        descr_camp_guia TYPE zimp_campos_guia-descr_camp_guia,
        bschl           TYPE zimp_cad_imp_con-bschl,
        umskz           TYPE zimp_cad_imp_con-umskz,
        hkont           TYPE zimp_cad_imp_con-hkont,
        lifnr           TYPE zimp_lanc_imp_ct-lifnr,
        kunnr           TYPE zimp_lanc_imp_ct-kunnr,
        kostl_c(1)      TYPE c,
        debcre(1)       TYPE c,
        vlr_moeda_doc   TYPE zimp_lanc_imp_ct-vlr_moeda_doc,
        valor_imp       TYPE zimp_lanc_imp_ct-valor_imp,
        valor_for       TYPE zimp_lanc_imp_ct-valor_imp,
        xclasse(1)      TYPE c,
        style           TYPE lvc_t_styl,
        seqitem         TYPE zimp_lanc_imp_ct-seqitem,
      END OF tg_itens,

      BEGIN OF tg_itens_cl OCCURS 0,
        mark(1),
        check(1),
        checkbox(1),
        icon(4)         TYPE c,
        bukrs           TYPE zimp_cad_lote-bukrs,
        gsber           TYPE zimp_lanc_impost-gsber,
        waers           TYPE zimp_lanc_impost-waers,
        bschl           TYPE zimp_cad_imp_con-bschl,
        bschlc          TYPE zimp_cad_imp_con-bschl,
        bschldc         TYPE zimp_cad_imp_con-bschl,
        cod_imposto     TYPE zimp_cad_imposto-cod_imposto,
        cod_barras      TYPE zimp_lanc_impost-cod_barras,
        qrcode          TYPE zimp_lanc_impost-qrcode, "US - 128395 - CSB
        observacao      TYPE zimp_lanc_impost-observacao,
        mes_apuracao    TYPE zimp_lanc_impost-mes_apuracao,
        ano_apuracao    TYPE zimp_lanc_impost-ano_apuracao,
        cod_aberturad   TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docd  TYPE zimp_cad_imp_con-vlr_moeda_doc,
        cod_aberturac   TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docc  TYPE zimp_cad_imp_con-vlr_moeda_doc,
        cod_aberturadc  TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docdc TYPE zimp_cad_imp_con-vlr_moeda_doc,
        doc_imposto     TYPE zimp_lanc_impost-doc_imposto,
        kostl           TYPE zimp_lanc_imp_ct-kostl,
        lifnr           TYPE zimp_lanc_imp_ct-lifnr,
        kunnr           TYPE zimp_lanc_imp_ct-kunnr,
        hkont           TYPE zimp_lanc_imp_ct-hkont,
        dt_apuracao     TYPE zimp_lanc_imp_ct-data_atual,
        zcheck_ccusto   TYPE char01,
      END OF tg_itens_cl,

      BEGIN OF tg_itens_clx OCCURS 0,
        mark(1),
        check(1),
        checkbox(1),
        icon(4)         TYPE c,
        bukrs           TYPE zimp_cad_lote-bukrs,
        gsber           TYPE zimp_lanc_impost-gsber,
        waers           TYPE zimp_lanc_impost-waers,
        bschl           TYPE zimp_cad_imp_con-bschl,
        bschlc          TYPE zimp_cad_imp_con-bschl,
        bschldc         TYPE zimp_cad_imp_con-bschl,
        cod_imposto     TYPE zimp_cad_imposto-cod_imposto,
        cod_barras      TYPE zimp_lanc_impost-cod_barras,
        qrcode          TYPE zimp_lanc_impost-qrcode, "US - 128395 - CSB
        observacao      TYPE zimp_lanc_impost-observacao,
        mes_apuracao    TYPE zimp_lanc_impost-mes_apuracao,
        ano_apuracao    TYPE zimp_lanc_impost-ano_apuracao,
        cod_aberturad   TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docd  TYPE zimp_cad_imp_con-vlr_moeda_doc,
        cod_aberturac   TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docc  TYPE zimp_cad_imp_con-vlr_moeda_doc,
        cod_aberturadc  TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docdc TYPE zimp_cad_imp_con-vlr_moeda_doc,
        doc_imposto     TYPE zimp_lanc_impost-doc_imposto,
        kostl           TYPE csks-kostl,
        lifnr           TYPE zimp_lanc_imp_ct-lifnr,
        kunnr           TYPE zimp_lanc_imp_ct-kunnr,
        hkont           TYPE zimp_lanc_imp_ct-hkont,
        dt_apuracao     TYPE zimp_lanc_imp_ct-data_atual,
        zcheck_ccusto   TYPE char01,
      END OF tg_itens_clx,

      BEGIN OF tg_itens_cld OCCURS 0,
        mark(1),
        check(1),
        checkbox(1),
        icon(4)         TYPE c,
        bukrs           TYPE zimp_cad_lote-bukrs,
        gsber           TYPE zimp_lanc_impost-gsber,
        waers           TYPE zimp_lanc_impost-waers,
        bschl           TYPE zimp_cad_imp_con-bschl,
        bschlc          TYPE zimp_cad_imp_con-bschl,
        bschldc         TYPE zimp_cad_imp_con-bschl,
        cod_imposto     TYPE zimp_cad_imposto-cod_imposto,
        cod_barras      TYPE zimp_lanc_impost-cod_barras,
        qrcode          TYPE zimp_lanc_impost-qrcode, "US - 128395 - CSB
        observacao      TYPE zimp_lanc_impost-observacao,
        mes_apuracao    TYPE zimp_lanc_impost-mes_apuracao,
        ano_apuracao    TYPE zimp_lanc_impost-ano_apuracao,
        cod_aberturad   TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docd  TYPE zimp_cad_imp_con-vlr_moeda_doc,
        cod_aberturac   TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docc  TYPE zimp_cad_imp_con-vlr_moeda_doc,
        cod_aberturadc  TYPE zimp_cad_imp_con-cod_abertura,
        vlr_moeda_docdc TYPE zimp_cad_imp_con-vlr_moeda_doc,
        doc_imposto     TYPE zimp_lanc_impost-doc_imposto,
      END OF tg_itens_cld,

      BEGIN OF tg_itens_c OCCURS 0,
        cod_abertura TYPE zimp_cad_imp_con-cod_abertura,
        kostl        TYPE zimp_lanc_imp_ct-kostl,
        ktext        TYPE cskt-ktext,
        prctr        TYPE zimp_lanc_imp_ct-prctr,
        aufnr        TYPE zimp_lanc_imp_ct-aufnr,
        matnr        TYPE zimp_lanc_imp_ct-matnr,
        valor_cus    TYPE zimp_lanc_imp_ct-valor_imp,
        valor_for    TYPE zimp_lanc_imp_ct-valor_for,
        seqitem      TYPE zimp_lanc_imp_ct-seqitem,
      END OF tg_itens_c.

** CRIAÇÃO DE TABELA DINAMICA
DATA: t_fieldcatalog      TYPE lvc_t_fcat,
      t_fieldcatalog_cl   TYPE lvc_t_fcat,
      w_fieldcatalog      TYPE lvc_s_fcat,
      wa_layout           TYPE lvc_s_layo,
      wa_stable           TYPE lvc_s_stbl,
      wg_editor           TYPE ty_editor,
      wa_zib_contabil_chv TYPE ty_zib_contabil_chv,
      wa_zib_contabil_err TYPE ty_zib_contabil_err,
      wa_bkpf             TYPE ty_bkpf,
      wa_t012k            TYPE t012k,

      it_zib_contabil_err TYPE TABLE OF ty_zib_contabil_err  WITH HEADER LINE,
      wg_itens            LIKE LINE OF tg_itens_c,
      wg_itemmmm          LIKE LINE OF tg_itens,
      tg_fields           TYPE TABLE OF ty_fields   WITH HEADER LINE,
      tg_imp_lanc_impost  TYPE TABLE OF zimp_lanc_impost WITH HEADER LINE,
      tg_imp_lanc_impostx TYPE TABLE OF zimp_lanc_impost WITH HEADER LINE,
      tg_imp_lanc_imp_ct  TYPE TABLE OF zimp_lanc_imp_ct WITH HEADER LINE,
      tg_imp_lanc_imp_ctx TYPE TABLE OF zimp_lanc_imp_ct WITH HEADER LINE,
      tg_editor           TYPE TABLE OF ty_editor,
      it_zimp_cad_imp_con TYPE TABLE OF zimp_cad_imp_con,
      tg_msg_ret          TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,

      tg_itens_cl2        LIKE TABLE OF tg_itens_cl,
      wl_itenc_cl2        LIKE LINE OF tg_itens_cl,

      tg_itens_c2         LIKE TABLE OF tg_itens_c,
      wl_itenc_c2         LIKE LINE OF tg_itens_c,

      vg_bukrs            TYPE zimp_cad_lote-bukrs.

"WG_ITENS           TYPE LINE OF  TG_ITENS_C.

*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC3',
*             tab4 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC4',
           END OF c_tab_strip_imp.

CONSTANTS: BEGIN OF c_tab_strip_imp_cl,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC4',
           END OF c_tab_strip_imp_cl.

CONTROLS:  tab_strip_imp_cl TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip_imp_cl,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZIMP53',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp_cl-tab1,
      END OF g_tab_strip_imp_cl.

*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  tab_strip_imp TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip_imp,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZIMP53',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

DATA: ok_code          LIKE sy-ucomm,
      wg_mensagem(30),
      e_db_click       TYPE  zfiwrs0002,
      wg_acao(30),
      vdt_apuracao(1),
      vmes_apuracao(1),
      vkokrs           TYPE tka02-kokrs,
      xclasse(1),
      xmodif(1),
      xconv(1),
      xforte(1),
      xtotal           TYPE zimp_lanc_imp_ct-valor_imp VALUE 0,
      xtotald          TYPE zimp_lanc_imp_ct-valor_imp VALUE 0,
      xflagval(1)      VALUE 'N',
      vcod_abertura    TYPE  zimp_lanc_imp_ct-cod_abertura,
      vagrup(1)        TYPE  c,
      vseqitem         TYPE  zimp_lanc_imp_ct-seqitem,
      tg_obj           TYPE TABLE OF ty_obj,
      wg_obj           TYPE ty_obj.




*CLASS DEFINITION FOR ALV TOOLBAR
CLASS:      lcl_alv_toolbar   DEFINITION DEFERRED.
*            LCL_ALV_TOOLBAR2  DEFINITION DEFERRED.
*            LCL_ALV_TOOLBAR3  DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& DECLARAÇÃO DE OBJETOS/CLASSES                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_ITENS_IMP',
      g_copialote          TYPE scrfname VALUE 'CC_COPIALOTE',
      lt_f4                TYPE lvc_t_f4     WITH HEADER LINE,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      obg_conteiner_err    TYPE REF TO cl_gui_custom_container,
      g_copia_lote         TYPE REF TO cl_gui_custom_container,
      container_1          TYPE REF TO cl_gui_container,       "SPLITTER CONTEINER 1
      container_2          TYPE REF TO cl_gui_container,       "SPLITTER CONTEINER 2
      container_4          TYPE REF TO cl_gui_container,       "SPLITTER CONTEINER 4
      splitter             TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      grid3                TYPE REF TO cl_gui_alv_grid,
      grid4                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      obg_toolbar2         TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      g_descbox            TYPE scrfname VALUE 'CC_DESC',
      g_cc_err             TYPE scrfname VALUE 'CC_ERR',
      g_custom_cont_desc   TYPE REF TO cl_gui_custom_container,
      obg_descbox          TYPE REF TO cl_gui_textedit,
      obg_docking          TYPE REF TO cl_gui_docking_container,
      wa_style             TYPE lvc_s_styl,
      "WA_STYLE             TYPE LVC_S_STYL,
      style                TYPE lvc_t_styl  WITH HEADER LINE,
      style2               TYPE lvc_t_styl WITH HEADER LINE.

* ALRS
*DECLARATION FOR TOOLBAR BUTTONS
DATA : ty_toolbar TYPE stb_button.
*** TREE DE MENSAGENS.
DATA node_itab LIKE node_str OCCURS 0.
DATA node LIKE node_str.

DATA: vl_gdatu TYPE gdatu_inv,
      vl_ukurs TYPE ukurs_curr.

DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.

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
*& CONSTANTES                                                         &*
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
           c_reinicia(8)     TYPE c VALUE 'REINICIA',
           c_deldoc(6)       TYPE c VALUE 'DELDOC',
           c_agr(3)          TYPE c VALUE 'AGR',
           c_displa(6)       TYPE c VALUE 'DISPLA',
           c_dclick(6)       TYPE c VALUE 'DCLICK',
           c_search(6)       TYPE c VALUE 'SEARCH',
           c_atuali(6)       TYPE c VALUE 'ATUALI',
           c_add_msg(7)      TYPE c VALUE 'ADD_MSG',
           c_del_msg(7)      TYPE c VALUE 'DEL_MSG',
           c_clos_msg(8)     TYPE c VALUE 'CLOS_MSG',
           c_save_msg(8)     TYPE c VALUE 'SAVE_MSG',
           c_show_msgre(10)  TYPE c VALUE 'SHOW_MSGRE'.

DATA: wg_dg1(4) VALUE c_minimizar,
      wg_dg2(4) VALUE c_minimizar.


DATA:   vg_chamada.

*ALRS
*-----------------------------------------------------------------------
* CLASSE
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
      on_data_changed2 FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm .
    CLASS-METHODS:
      on_data_changed_finished2 FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_TREEOBJECT DEFINITION
*---------------------------------------------------------------------*
*       DEFINITION OF DATA CONTAINER                                  *
*---------------------------------------------------------------------*
CLASS lcl_drag_object DEFINITION.
  PUBLIC SECTION.
    DATA text TYPE mtreesnode-text.
ENDCLASS.                    "LCL_DRAG_OBJECT DEFINITION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      node_double_click FOR EVENT node_double_click OF cl_gui_simple_tree
        IMPORTING node_key.

ENDCLASS.                    "LCL_DRAGDROP_RECEIVER DEFINITION
*---------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR DEFINITION
*---------------------------------------------------------------------*
*       ALV EVENT HANDLER
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar DEFINITION.
  PUBLIC SECTION.
*CONSTRUCTOR
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
*EVENT FOR TOOLBAR
      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

ENDCLASS.                    "LCL_ALV_TOOLBAR DEFINITION

*---------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV EVENT HANDLER
*---------------------------------------------------------------------*
CLASS lcl_alv_toolbar IMPLEMENTATION.
  METHOD constructor.
*   CREATE ALV TOOLBAR MANAGER INSTANCE
    CREATE OBJECT c_alv_toolbarmanager
      EXPORTING
        io_alv_grid = io_alv_grid.
  ENDMETHOD.                    "CONSTRUCTOR

  METHOD on_toolbar.
    DATA: wl_desactive.
    CLEAR wl_desactive.
*    IF WG_ACAO NE C_MODIF.
*      WL_DESACTIVE = 1.
*    ENDIF.

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
*   VARIABLE FOR TOOLBAR BUTTON
    ty_toolbar-icon      =  icon_view_close.
    ty_toolbar-function  =  c_clos_msg.
    ty_toolbar-disabled  = space.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
**   CALL REORGANIZE METHOD OF TOOLBAR MANAGER TO
**   DISPLAY THE TOOLBAR
    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "ON_TOOLBAR
  METHOD handle_user_command.
    DATA: tl_itens_aux  LIKE TABLE OF tg_itens_c,
          wl_itens      LIKE LINE OF tg_itens_c,
          wl_lines      TYPE sy-tabix,
          tl_itensc_aux LIKE TABLE OF tg_itens_cl,
          wl_itensc     LIKE LINE OF tg_itens_cl,
          wl_linesc     TYPE sy-tabix,
          wl_itens_cod  LIKE LINE OF tg_itens,
          wl_itensc_cod LIKE LINE OF tg_itens_cl,
          xtotal        TYPE zimp_lanc_imp_ct-valor_imp,
          xtotalf       TYPE zimp_lanc_imp_ct-valor_for,
          xlinha        TYPE i.

    REFRESH: tl_itens_aux.
    IF vcod_abertura IS NOT INITIAL.

      CASE e_ucomm.
        WHEN c_clos_msg.
          CALL METHOD splitter->set_row_height
            EXPORTING
              id     = 1
              height = 100.
        WHEN c_add.
          tl_itens_aux[] = tg_itens_c2[].
          REFRESH: tg_itens_c2.
          LOOP AT tl_itens_aux INTO wl_itens.
            APPEND wl_itens TO tg_itens_c2.
          ENDLOOP.
          CLEAR: wl_itens.
          wl_itens-cod_abertura = vcod_abertura.
          APPEND wl_itens TO tg_itens_c2.

          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        WHEN c_del.
          CALL METHOD grid2->get_selected_cells
            IMPORTING
              et_cell = tg_selectedcell.

          LOOP AT tg_selectedcell INTO wg_selectedcell.
            DELETE tg_itens_c2 INDEX wg_selectedcell-row_id-index.
          ENDLOOP.

          DELETE tg_itens_c WHERE cod_abertura = vcod_abertura.
          LOOP AT tg_itens_c2 INTO wl_itens.
            APPEND wl_itens TO tg_itens_c.
          ENDLOOP.

          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

          xtotal = 0.
          xtotalf = 0.
          LOOP AT tg_itens_c INTO wl_itens.
            ADD wl_itens-valor_cus  TO xtotal.
            ADD wl_itens-valor_for  TO xtotalf.
          ENDLOOP.
          CLEAR xlinha.
          LOOP AT tg_itens INTO wl_itens_cod.
            IF wl_itens_cod-cod_abertura = vcod_abertura.
              xlinha = sy-tabix.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF xtotal > 0.
            wl_itens_cod-kostl_c = 'X'.
            wl_itens_cod-valor_imp = xtotal.
            wl_itens_cod-valor_for = xtotalf.
          ELSE.
            wl_itens_cod-kostl_c = ' '.
            wl_itens_cod-valor_imp = 0.
            wl_itens_cod-valor_for = 0.
          ENDIF.

*-PBI 64611 - 23.08.2021 - JT - inicio
          IF xlinha IS NOT INITIAL.
            MODIFY tg_itens FROM wl_itens_cod INDEX xlinha TRANSPORTING kostl_c valor_imp valor_for.
          ENDIF.
*-PBI 64611 - 23.08.2021 - JT - inicio
          CALL METHOD grid1->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
      ENDCASE.

    ENDIF.

    IF vagrup IS NOT INITIAL. "RJF

      CASE e_ucomm.
        WHEN c_clos_msg.
          CALL METHOD splitter->set_row_height
            EXPORTING
              id     = 1
              height = 100.
        WHEN c_add.
          REFRESH: tg_itens_cl2.
          CLEAR: wl_itensc.
          APPEND wl_itensc TO tg_itens_cl2.

          CALL METHOD grid4->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

          LOOP AT tg_itens_cl2 INTO wl_itensc.
            MOVE icon_led_yellow TO wl_itensc-icon.
            APPEND wl_itensc TO tg_itens_cl.
          ENDLOOP.

          CALL METHOD grid4->check_changed_data.

          CALL METHOD grid4->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        WHEN c_del.
          READ TABLE tg_itens_cl INTO DATA(wl_i) WITH KEY checkbox = abap_true.
          IF sy-subrc IS INITIAL.
            DELETE tg_itens_cl WHERE checkbox EQ abap_true.
          ELSE.
            MESSAGE 'Selecionar registro(s) através do checkbox.' TYPE 'I'.
          ENDIF.

          CALL METHOD grid4->check_changed_data.

          CALL METHOD grid4->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

      ENDCASE.

      CALL METHOD grid4->check_changed_data.

      CALL METHOD grid4->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDIF.
  ENDMETHOD.                    "ZM_HANDLE_USER_COMMAND



ENDCLASS.                    "LCL_ALV_TOOLBAR IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR IMPLEMENTATION
*---------------------------------------------------------------------*
*       ALV EVENT HANDLER
*---------------------------------------------------------------------*

"LCL_ALV_TOOLBAR IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.


* MÉTODO DE  EXECUÇÃO PARA DUPLO-CLICK
  METHOD on_double_click.


    DATA: wl_itens LIKE LINE OF tg_itens.

    DATA: event       TYPE cntl_simple_event,
          events      TYPE cntl_simple_events,
          tl_filter   TYPE lvc_t_filt,
          wl_filter   TYPE lvc_s_filt,
          tl_function TYPE ui_functions,
          wl_function LIKE LINE OF tl_function.

    CLEAR vcod_abertura.
*-PBI 64611 - 23.08.2021 - JT - inicio
    IF e_row-index GT 0 .
*-PBI 64611 - 23.08.2021 - JT - fim
      READ TABLE tg_itens INTO wl_itens INDEX e_row.
      IF  wl_itens-cod_imposto IS NOT INITIAL
        AND wl_itens-cod_abertura IS NOT INITIAL
        AND wl_itens-checkbox = 'X'
        AND wl_itens-xclasse = 'X'.
*    POSICIONA SPLITER NA ALTURA X
        vcod_abertura = wl_itens-cod_abertura.
        CALL METHOD splitter->set_row_height
          EXPORTING
            id     = 1
            height = 0.

        " VG_SUBSCREEN1 = C_DUMMY_HEADER.

        IF grid2 IS NOT INITIAL.
          CALL METHOD grid2->free.

        ENDIF.

        FREE: container_2, grid2.

        CALL METHOD splitter->get_container
          EXPORTING
            row       = 2
            column    = 1
          RECEIVING
            container = container_2.
        IF grid2 IS INITIAL.
          "WA_LAYOUT-NO_TOOLBAR = C_X.
          CREATE OBJECT grid2
            EXPORTING
              i_parent = container_2.

          "WA_LAYOUT-CWIDTH_OPT = C_X.
          wa_layout-edit = space.
          CONDENSE e_row NO-GAPS.
          CONCATENATE 'CENTROS CUSTO PARA CODIGO ABERTURA' '-' wl_itens-cod_abertura INTO wa_layout-grid_title SEPARATED BY space.

          PERFORM montar_layout_centro.
          IF wg_cadlan-waers_f = 'USD'.
            PERFORM montar_estrutura USING:
                   7 'ZIMP_LANC_IMP_CT'         'VALOR_FOR'       'TG_ITENS_C' 'VALOR_FOR'        'VALOR U$'       '15' 'X' ' ' ' '.
          ENDIF.

          CREATE OBJECT obg_toolbar
            EXPORTING
              io_alv_grid = grid2.

*            * REGISTER EVENT HANDLER
          SET HANDLER obg_toolbar->on_toolbar FOR grid2.
          SET HANDLER obg_toolbar->handle_user_command FOR grid2.

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

          REFRESH tg_itens_c2.

          LOOP AT tg_itens_c INTO wl_itenc_c2.
            IF wl_itenc_c2-cod_abertura = wl_itens-cod_abertura.
              APPEND wl_itenc_c2 TO tg_itens_c2.
            ENDIF.
          ENDLOOP.

          CALL METHOD grid2->set_table_for_first_display
            EXPORTING
              it_toolbar_excluding = tl_function
              is_layout            = wa_layout
            CHANGING
              it_fieldcatalog      = t_fieldcatalog[]
              it_outtab            = tg_itens_c2[].

          CALL METHOD grid2->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_modified.

          CALL METHOD grid2->register_edit_event
            EXPORTING
              i_event_id = cl_gui_alv_grid=>mc_evt_enter.

          SET HANDLER:
              lcl_event_handler=>on_data_changed_finished2 FOR grid2,
              lcl_event_handler=>on_data_changed2 FOR grid2.

*      *** MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.
        ELSE.

*      *** MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
          CALL METHOD grid2->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ENDIF.
        wg_dg1 = c_maximizar.
        LEAVE TO SCREEN 100.
      ELSE.
*    POSICIONA SPLITER NA ALTURA X
        CALL METHOD splitter->set_row_height
          EXPORTING
            id     = 1
            height = 100.
      ENDIF.
    ELSE.
*    POSICIONA SPLITER NA ALTURA X
      CALL METHOD splitter->set_row_height
        EXPORTING
          id     = 1
          height = 100.
    ENDIF.


  ENDMETHOD.                    "ON_DOUBLE_CLICK

  METHOD on_data_changed2.


    DATA: ls_good          TYPE lvc_s_modi,
          lv_value         TYPE lvc_value,
          lv_valuef        TYPE lvc_value,
          vl_value         TYPE lvc_value,
          wl_cskt          TYPE cskt,
          vdatax           TYPE sy-datum,
          taxa_moeda_forte TYPE tcurr-ukurs.

*


    DATA: w_tka02 TYPE tka02,
          wl_csks TYPE csks.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'KOSTL'.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

* Determinação área de contabilidade de custos
      SELECT SINGLE * FROM tka02
        INTO w_tka02
        WHERE bukrs	= wg_cadlan-bukrs.

* Valida se o centro de custo existe para filial
      SELECT SINGLE *
        FROM csks
        INTO wl_csks
          WHERE kokrs EQ  w_tka02-kokrs
          AND kostl   EQ  lv_value
          AND gsber   EQ  wg_cadlan-gsber.

      IF sy-subrc IS NOT INITIAL.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'KOSTL'
            i_value     = lv_value.

        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Centro de Custo informado não é desta filial.'.

      ELSE.

* Valida vencimento e bloqueio
        SELECT SINGLE *
          FROM csks
          INTO wl_csks
            WHERE kokrs EQ  w_tka02-kokrs
            AND kostl   EQ  lv_value
            AND bkzkp   EQ 'X'
            AND bkzks   EQ 'X'
            AND gsber   EQ  wg_cadlan-gsber
            AND datbi >= sy-datum.

        IF sy-subrc IS NOT INITIAL.

          SELECT SINGLE *
            FROM cskt
            INTO wl_cskt
              WHERE spras EQ 'P'
              AND kokrs   EQ  vkokrs
              AND kostl   EQ lv_value.

          IF sy-subrc IS INITIAL.
            MOVE wl_cskt-ktext TO lv_value.

          ELSE.
            CLEAR lv_value.
          ENDIF.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'KTEXT'
              i_value     = lv_value.

        ELSE.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'KOSTL'
              i_value     = lv_value.

          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'O centro de custo' 'informado está bloqueado' 'para lançamento'.

        ENDIF.

      ENDIF.

    ENDLOOP.



    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'VALOR_CUS'.

      CLEAR: lv_value, vdatax, vl_ukurs, vl_gdatu, lv_valuef.

      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.

      IF wg_cadlan-data_atual IS NOT INITIAL.
        vdatax = wg_cadlan-data_atual.

        IF wg_cadlan-st_fecha IS NOT INITIAL.
          vdatax = |{ vdatax(4) }{ vdatax+4(2) }01|.
        ENDIF.

      ENDIF.

      CREATE OBJECT obj_zcl_util_sd.

      obj_zcl_util_sd->set_kurst('B').
      obj_zcl_util_sd->set_waerk('USD').
      obj_zcl_util_sd->set_tcurr('BRL').

      lv_valuef = lv_value.


      MOVE  vdatax TO vl_gdatu.
      obj_zcl_util_sd->set_data( vl_gdatu ).
      vl_ukurs = obj_zcl_util_sd->taxa_cambio( ).
      MOVE  vl_ukurs TO taxa_moeda_forte .

      IF taxa_moeda_forte LT 0.
        MULTIPLY taxa_moeda_forte BY -1.
        lv_valuef = lv_valuef * taxa_moeda_forte.
      ELSE.
        lv_valuef = lv_valuef / taxa_moeda_forte .
      ENDIF.

      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'VALOR_FOR'
          i_value     = lv_valuef.

    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED2

  METHOD on_data_changed.
    DATA: ls_good             TYPE lvc_s_modi,
          v_round(15)         TYPE p DECIMALS 0,
          flag_r(1),
          lv_value            TYPE lvc_value,
          vl_value            TYPE lvc_value,
          lv_valueg           TYPE lvc_value,
          lv_valuef           TYPE lvc_value,
          lv_valueteste       TYPE lvc_value,
          wl_itens            LIKE LINE OF tg_itens,
          wl_zimp_campos_guia TYPE zimp_campos_guia,
          wl_cadimp           TYPE zimp_cad_imp_con,
          tl_imp_cad_imp_con  TYPE TABLE OF zimp_cad_imp_con,
          wl_itens_cl         LIKE LINE OF tg_itens_cl,
          tl_tbsl             TYPE TABLE OF tbsl,
          wl_tcurr_g          TYPE tcurr,
          wl_tcurr_f          TYPE tcurr,
          wl_tcurr            TYPE tcurr,
          tl_tcurr            TYPE TABLE OF tcurr,
          vdata               TYPE tcurr-gdatu,
          wl_t005             TYPE t005,
          wl_t001             TYPE t001,
          vdata_f             TYPE tcurr-gdatu,
          wl_tbsl             TYPE          tbsl,
          vg_sinal(1),
          vdatax              TYPE sy-datum,
          chdat(8)            TYPE c,
          houtput(8)          TYPE n,
          data_lanc(8)        TYPE c.

    IF sy-dynnr EQ '0600'.

      LOOP AT er_data_changed->mt_good_cells
                                 INTO ls_good
                                 WHERE fieldname = 'GSBER'. " GSBER
        READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.

        SELECT SINGLE *
        FROM j_1bbranch
        INTO  @DATA(wl_j_1bbranch)
        WHERE bukrs = @vg_bukrs
        AND   branch = @ls_good-value.

        IF sy-subrc IS NOT INITIAL AND wl_j_1bbranch IS INITIAL.
          MESSAGE 'Filial não pertencente a empresa!' TYPE 'I'.

          lv_value = wl_itens_cl-gsber.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'GSBER'
              i_value     = lv_value.
          FREE lv_value.

*        ELSE.
*
*          lv_value = wl_itens_cl-gsber.
*          CALL METHOD er_data_changed->modify_cell
*            EXPORTING
*              i_row_id    = ls_good-row_id
*              i_fieldname = 'GSBER'
*              i_value     = lv_value.
*          FREE lv_value.

        ENDIF.

      ENDLOOP.

      LOOP AT er_data_changed->mt_good_cells
                                INTO ls_good
                                WHERE fieldname = 'VLR_MOEDA_DOCC'. " VLR_MOEDA_DOCD
        READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.

        IF sy-subrc IS INITIAL
          AND wl_itens_cl-cod_imposto IS NOT INITIAL
          AND wl_itens_cl-cod_aberturad IS NOT INITIAL
          AND wl_itens_cl-cod_aberturac IS NOT INITIAL.

          IF wl_itens_cl-vlr_moeda_docd IS NOT INITIAL
          AND wl_itens_cl-vlr_moeda_docc IS NOT INITIAL
          AND wl_itens_cl-cod_aberturadc IS NOT INITIAL.
*          AND wl_itens_cl-vlr_moeda_docdc EQ 0.
            IF ls_good-value GT 0.

              DATA(lv_vx) = ls_good-value * ( -1 ).

              DATA(lv_prov) = lv_vx + wl_itens_cl-vlr_moeda_docd + wl_itens_cl-vlr_moeda_docdc.
              IF lv_prov EQ 0.
                lv_value = icon_led_yellow. "icon_checked.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'ICON'
                    i_value     = lv_value.
                FREE lv_value.

                lv_value = lv_vx.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'VLR_MOEDA_DOCC'
                    i_value     = lv_value.
                FREE lv_value.
              ELSE.
                lv_value = icon_message_error. "icon_checked.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'ICON'
                    i_value     = lv_value.
                FREE lv_value.

                lv_value = lv_vx.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'VLR_MOEDA_DOCC'
                    i_value     = lv_value.
                FREE lv_value.
              ENDIF.

*            DATA(lv_vx) = wl_itens_cl-vlr_moeda_docc * ( -1 ).
*            IF wl_itens_cl-vlr_moeda_docd EQ lv_vx.
*              lv_value = icon_led_yellow. "icon_checked.
*              CALL METHOD er_data_changed->modify_cell
*                EXPORTING
*                  i_row_id    = ls_good-row_id
*                  i_fieldname = 'ICON'
*                  i_value     = lv_value.
*              FREE lv_value.
*
*              lv_value = wl_itens_cl-vlr_moeda_docc.
*              CALL METHOD er_data_changed->modify_cell
*                EXPORTING
*                  i_row_id    = ls_good-row_id
*                  i_fieldname = 'VLR_MOEDA_DOCC'
*                  i_value     = lv_value.
*              FREE lv_value.
*
*            ENDIF.
            ENDIF.
          ELSEIF wl_itens_cl-vlr_moeda_docd IS NOT INITIAL
          AND wl_itens_cl-vlr_moeda_docc IS NOT INITIAL
          AND wl_itens_cl-cod_aberturadc IS INITIAL.

            lv_value = wl_itens_cl-vlr_moeda_docc.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'VLR_MOEDA_DOCC'
                i_value     = lv_value.
            FREE lv_value.

          ENDIF.
        ENDIF.
      ENDLOOP.

*----
      LOOP AT er_data_changed->mt_good_cells
                               INTO ls_good
                               WHERE fieldname = 'COD_ABERTURADC'. " VLR_MOEDA_DOCD
        READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.

        IF wl_itens_cl-vlr_moeda_docdc EQ 0 AND wl_itens_cl-cod_aberturadc IS INITIAL.

          IF sy-subrc IS INITIAL
            AND wl_itens_cl-cod_imposto IS NOT INITIAL
            AND wl_itens_cl-cod_aberturad IS NOT INITIAL
            AND wl_itens_cl-cod_aberturac IS NOT INITIAL.

            IF wl_itens_cl-vlr_moeda_docd IS NOT INITIAL
            AND wl_itens_cl-vlr_moeda_docc IS NOT INITIAL.
              DATA(lv_v) = wl_itens_cl-vlr_moeda_docc * ( -1 ).
              IF wl_itens_cl-vlr_moeda_docd EQ lv_v.
                lv_value = icon_led_yellow."icon_checked.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'ICON'
                    i_value     = lv_value.
                FREE lv_value.

                CLEAR lv_value.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'VLR_MOEDA_DOCDC'
                    i_value     = lv_value.
                FREE lv_value.

                CLEAR lv_value.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'COD_ABERTURADC'
                    i_value     = lv_value.
                FREE lv_value.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDLOOP.

*----
      LOOP AT er_data_changed->mt_good_cells
                               INTO ls_good
                               WHERE fieldname = 'VLR_MOEDA_DOCDC'. " VLR_MOEDA_DOCD
        READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.

        IF wl_itens_cl-cod_aberturadc IS NOT INITIAL AND
          ls_good-value IS NOT INITIAL AND wl_itens_cl-bschldc IS NOT INITIAL.

* Verificar sinal

          SELECT SINGLE shkzg
           FROM tbsl
           INTO @DATA(lv_shkzg)
           WHERE bschl = @wl_itens_cl-bschldc.

          IF lv_shkzg = 'H'. "C

            IF ls_good-value GT 0.
              lv_value = ls_good-value * ( -1 ).

              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'VLR_MOEDA_DOCDC'
                  i_value     = lv_value.

            ENDIF.

          ENDIF.

* Somar as 3

*If diferente de 0 marcar linha
          CLEAR lv_value.
          lv_value = wl_itens_cl-vlr_moeda_docd + wl_itens_cl-vlr_moeda_docc + ls_good-value.
          IF lv_value NE 0.
*            CALL METHOD er_data_changed->modify_cell
*              EXPORTING
*                i_row_id    = ls_good-row_id
*                i_fieldname = 'VLR_MOEDA_DOCDC'
*                i_value     = lv_value.

            lv_value = icon_message_error.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'ICON'
                i_value     = lv_value.
            FREE lv_value.
          ELSE.
            lv_value = icon_led_yellow.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'ICON'
                i_value     = lv_value.
            FREE lv_value.

          ENDIF.

        ELSEIF wl_itens_cl-cod_aberturadc IS INITIAL.

          CLEAR lv_value.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_DOCDC'
              i_value     = lv_value.
          FREE lv_value.

        ENDIF.


*        IF wl_itens_cl-vlr_moeda_docdc EQ 0 and wl_itens_cl-cod_aberturadc is not initial.
*
*          IF sy-subrc IS INITIAL
*            AND wl_itens_cl-cod_imposto IS NOT INITIAL
*            AND wl_itens_cl-cod_aberturad IS NOT INITIAL
*            AND wl_itens_cl-cod_aberturac IS NOT INITIAL.
*
*            IF wl_itens_cl-vlr_moeda_docd IS NOT INITIAL
*            AND wl_itens_cl-vlr_moeda_docc IS NOT INITIAL.
*              DATA(lv_v1) = wl_itens_cl-vlr_moeda_docc * ( -1 ).
*              IF wl_itens_cl-vlr_moeda_docd EQ lv_v1.
*                lv_value = icon_led_yellow. "icon_checked.
*                CALL METHOD er_data_changed->modify_cell
*                  EXPORTING
*                    i_row_id    = ls_good-row_id
*                    i_fieldname = 'ICON'
*                    i_value     = lv_value.
*                FREE lv_value.
*
*                CLEAR lv_value.
*                CALL METHOD er_data_changed->modify_cell
*                  EXPORTING
*                    i_row_id    = ls_good-row_id
*                    i_fieldname = 'VLR_MOEDA_DOCDC'
*                    i_value     = lv_value.
*                FREE lv_value.
*
*                CLEAR lv_value.
*                CALL METHOD er_data_changed->modify_cell
*                  EXPORTING
*                    i_row_id    = ls_good-row_id
*                    i_fieldname = 'COD_ABERTURADC'
*                    i_value     = lv_value.
*                FREE lv_value.
*
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.

      ENDLOOP.
*----
      LOOP AT er_data_changed->mt_good_cells
                               INTO ls_good
                               WHERE fieldname = 'VLR_MOEDA_DOCD'. " VLR_MOEDA_DOCD
        IF ls_good-value IS NOT INITIAL.
          lv_value = ls_good-value * ( -1 ).
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'VLR_MOEDA_DOCC'
              i_value     = lv_value.

          READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.
          lv_value = lv_value + ls_good-value + wl_itens_cl-vlr_moeda_docdc.
          IF lv_value NE 0.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'VLR_MOEDA_DOCDC'
                i_value     = lv_value.

            lv_value = icon_message_error.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'ICON'
                i_value     = lv_value.
            FREE lv_value.
* colocar do code ini

            READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.
            IF sy-subrc IS INITIAL
              AND wl_itens_cl-cod_imposto IS NOT INITIAL
              AND wl_itens_cl-cod_aberturad IS NOT INITIAL
              AND wl_itens_cl-cod_aberturac IS NOT INITIAL.

              SELECT *
              FROM zimp_cad_imp_con
              INTO TABLE tl_imp_cad_imp_con
              WHERE cod_imposto EQ wl_itens_cl-cod_imposto
                AND agrupamento EQ abap_true.

              IF sy-subrc EQ 0 AND tl_imp_cad_imp_con[] IS NOT INITIAL.

                SELECT *
                 FROM tbsl
                 INTO TABLE tl_tbsl
                 FOR ALL ENTRIES IN tl_imp_cad_imp_con
                 WHERE bschl = tl_imp_cad_imp_con-bschl.
                IF sy-subrc EQ 0 AND tl_tbsl[] IS NOT INITIAL.

                  LOOP AT tl_imp_cad_imp_con INTO wl_cadimp.

                    READ TABLE tl_tbsl INTO wl_tbsl
                      WITH KEY bschl = wl_cadimp-bschl.
                    IF wl_tbsl-shkzg = 'H'. "C

                      READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.
                      IF wl_cadimp-cod_abertura IS NOT INITIAL AND wl_itens_cl-cod_aberturac IS INITIAL. "
                        lv_value = wl_cadimp-cod_abertura.
                        CALL METHOD er_data_changed->modify_cell
                          EXPORTING
                            i_row_id    = ls_good-row_id
                            i_fieldname = 'COD_ABERTURAC'
                            i_value     = lv_value.

                        wl_itens_cl-bschlc = wl_cadimp-bschl.
                        MODIFY tg_itens_cl FROM wl_itens_cl INDEX ls_good-row_id.

                        FREE lv_value.
                      ELSEIF wl_itens_cl-cod_aberturac IS NOT INITIAL AND wl_itens_cl-cod_aberturac NE wl_cadimp-cod_abertura.
                        lv_value = wl_cadimp-cod_abertura.
                        CALL METHOD er_data_changed->modify_cell
                          EXPORTING
                            i_row_id    = ls_good-row_id
                            i_fieldname = 'COD_ABERTURADC'
                            i_value     = lv_value.

                        wl_itens_cl-bschldc = wl_cadimp-bschl.
                        MODIFY tg_itens_cl FROM wl_itens_cl INDEX ls_good-row_id.

                        FREE lv_value.
                      ENDIF.
                    ELSE.
                      READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.

                      IF wl_cadimp-cod_abertura IS NOT INITIAL AND wl_itens_cl-cod_aberturad IS INITIAL. "
                        lv_value = wl_cadimp-cod_abertura.
                        CALL METHOD er_data_changed->modify_cell
                          EXPORTING
                            i_row_id    = ls_good-row_id
                            i_fieldname = 'COD_ABERTURAD'
                            i_value     = lv_value.

                        wl_itens_cl-bschl = wl_cadimp-bschl.
                        MODIFY tg_itens_cl FROM wl_itens_cl INDEX ls_good-row_id.

                        FREE lv_value.
                      ENDIF.

                      READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.
                      IF wl_cadimp-cod_abertura IS NOT INITIAL AND wl_itens_cl-cod_aberturadc IS INITIAL
                        AND wl_itens_cl-vlr_moeda_docdc IS NOT INITIAL. "
                        lv_value = wl_cadimp-cod_abertura.
                        CALL METHOD er_data_changed->modify_cell
                          EXPORTING
                            i_row_id    = ls_good-row_id
                            i_fieldname = 'COD_ABERTURADC'
                            i_value     = lv_value.

                        wl_itens_cl-bschldc = wl_cadimp-bschl.
                        MODIFY tg_itens_cl FROM wl_itens_cl INDEX ls_good-row_id.

                        FREE lv_value.
                      ENDIF.

                    ENDIF.
                  ENDLOOP.

                ENDIF.
              ELSE.

                FREE: lv_value.
                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'COD_ABERTURAC'
                    i_value     = lv_value.

                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'COD_ABERTURAD'
                    i_value     = lv_value.

                CALL METHOD er_data_changed->modify_cell
                  EXPORTING
                    i_row_id    = ls_good-row_id
                    i_fieldname = 'COD_ABERTURADC'
                    i_value     = lv_value.

              ENDIF.
            ELSE.
              MESSAGE 'Código Imposto vazio, necessário preeenchimento!' TYPE 'I'.
            ENDIF.

          ELSE.
            lv_value = icon_led_yellow. "icon_checked.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'ICON'
                i_value     = lv_value.
            FREE lv_value.
          ENDIF.

        ENDIF.

      ENDLOOP.
*----
      DATA: lv_jd TYPE c,
            lv_jc TYPE c.

      LOOP AT er_data_changed->mt_good_cells
                               INTO ls_good
                               WHERE fieldname = 'COD_IMPOSTO'. " COD_IMPOSTO


        READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.
        IF sy-subrc IS INITIAL.
          SELECT *
          FROM zimp_cad_imp_con
          INTO TABLE tl_imp_cad_imp_con
          WHERE cod_imposto EQ ls_good-value
            AND agrupamento EQ abap_true.

          IF sy-subrc EQ 0 AND tl_imp_cad_imp_con[] IS NOT INITIAL.

            SELECT *
             FROM tbsl
             INTO TABLE tl_tbsl
             FOR ALL ENTRIES IN tl_imp_cad_imp_con
             WHERE bschl = tl_imp_cad_imp_con-bschl.
            IF sy-subrc EQ 0 AND tl_tbsl[] IS NOT INITIAL.

              FREE lv_value.
              CALL METHOD er_data_changed->modify_cell
                EXPORTING
                  i_row_id    = ls_good-row_id
                  i_fieldname = 'COD_ABERTURADC'
                  i_value     = lv_value.

              LOOP AT tl_imp_cad_imp_con INTO wl_cadimp.

                READ TABLE tl_tbsl INTO wl_tbsl
                  WITH KEY bschl = wl_cadimp-bschl.
                IF wl_tbsl-shkzg = 'H'. "C

                  READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.
                  IF wl_cadimp-cod_abertura IS NOT INITIAL AND lv_jc EQ abap_false. "AND wl_itens_cl-cod_aberturac IS INITIAL
                    lv_value = wl_cadimp-cod_abertura.
                    CALL METHOD er_data_changed->modify_cell
                      EXPORTING
                        i_row_id    = ls_good-row_id
                        i_fieldname = 'COD_ABERTURAC'
                        i_value     = lv_value.

                    lv_jc = abap_true.

                    wl_itens_cl-bschlc = wl_cadimp-bschl.
                    MODIFY tg_itens_cl FROM wl_itens_cl INDEX ls_good-row_id.

                  ELSEIF lv_jc IS NOT INITIAL.
                    READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.
                    IF wl_cadimp-cod_abertura IS NOT INITIAL. "AND wl_itens_cl-cod_aberturadc IS INITIAL.
*                      AND wl_itens_cl-vlr_moeda_docdc IS NOT INITIAL. "
                      lv_value = wl_cadimp-cod_abertura.
                      CALL METHOD er_data_changed->modify_cell
                        EXPORTING
                          i_row_id    = ls_good-row_id
                          i_fieldname = 'COD_ABERTURADC'
                          i_value     = lv_value.

                      wl_itens_cl-bschldc = wl_cadimp-bschl.
                      MODIFY tg_itens_cl FROM wl_itens_cl INDEX ls_good-row_id.

                    ENDIF.

                  ENDIF.
                ELSE.
                  READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.

                  IF wl_cadimp-cod_abertura IS NOT INITIAL AND lv_jd EQ abap_false. "AND wl_itens_cl-cod_aberturad IS INITIAL
                    lv_value = wl_cadimp-cod_abertura.
                    CALL METHOD er_data_changed->modify_cell
                      EXPORTING
                        i_row_id    = ls_good-row_id
                        i_fieldname = 'COD_ABERTURAD'
                        i_value     = lv_value.

                    lv_jd = abap_true.
                    wl_itens_cl-bschl = wl_cadimp-bschl.
                    MODIFY tg_itens_cl FROM wl_itens_cl INDEX ls_good-row_id.

                  ELSEIF lv_jd IS NOT INITIAL.
                    READ TABLE tg_itens_cl INTO wl_itens_cl INDEX ls_good-row_id.
                    IF wl_cadimp-cod_abertura IS NOT INITIAL." AND wl_itens_cl-cod_aberturadc IS INITIAL.
*                      AND wl_itens_cl-vlr_moeda_docdc IS NOT INITIAL. "
                      lv_value = wl_cadimp-cod_abertura.
                      CALL METHOD er_data_changed->modify_cell
                        EXPORTING
                          i_row_id    = ls_good-row_id
                          i_fieldname = 'COD_ABERTURADC'
                          i_value     = lv_value.

                      wl_itens_cl-bschldc = wl_cadimp-bschl.
                      MODIFY tg_itens_cl FROM wl_itens_cl INDEX ls_good-row_id.

                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.

            ENDIF.
          ELSE.

            MESSAGE 'Agrupamento para esse imposto não foi definido.' TYPE 'I'.

            lv_value = wl_itens_cl-cod_imposto.
            CALL METHOD er_data_changed->modify_cell
              EXPORTING
                i_row_id    = ls_good-row_id
                i_fieldname = 'COD_IMPOSTO'
                i_value     = lv_value.
*
*            CALL METHOD er_data_changed->modify_cell
*              EXPORTING
*                i_row_id    = ls_good-row_id
*                i_fieldname = 'COD_ABERTURAD'
*                i_value     = lv_value.

          ENDIF.
        ENDIF.
      ENDLOOP.

      PERFORM montar_layout_cl.
      CALL METHOD grid4->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog[].
      CALL METHOD grid4->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDIF.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'KUNNR'.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
      SELECT SINGLE *
      FROM tbsl
      INTO wl_tbsl
      WHERE bschl EQ wl_itens-bschl.
      IF wl_tbsl-koart = 'D'.
        SELECT SINGLE *
          FROM zimp_cad_imp_con
          INTO wl_cadimp
          WHERE cod_imposto  = wl_itens-cod_imposto
          AND   cod_abertura = wl_itens-cod_abertura.
        IF wl_cadimp-kunnr IS NOT INITIAL. " MANTEM O CÓDIGO
          lv_value = wl_cadimp-kunnr.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'KUNNR'
              i_value     = lv_value.
        ENDIF.
      ELSE.
        CLEAR lv_value .
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'KUNNR'
            i_value     = lv_value.
      ENDIF.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'LIFNR'.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
      SELECT SINGLE *
      FROM tbsl
      INTO wl_tbsl
      WHERE bschl EQ wl_itens-bschl.
      IF wl_tbsl-koart = 'K'.
        SELECT SINGLE *
          FROM zimp_cad_imp_con
          INTO wl_cadimp
          WHERE cod_imposto  = wl_itens-cod_imposto
          AND   cod_abertura = wl_itens-cod_abertura.
        IF wl_cadimp-lifnr IS NOT INITIAL. " MANTEM O CÓDIGO
          lv_value = wl_cadimp-lifnr.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'LIFNR'
              i_value     = lv_value.
        ENDIF.
      ELSE.
        CLEAR lv_value .
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'LIFNR'
            i_value     = lv_value.
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

      ENDIF.

    ENDLOOP.

    IF sy-dynnr NE '0600'.
      LOOP AT er_data_changed->mt_good_cells
                              INTO ls_good
                              WHERE fieldname = 'CHECKBOX'.
        READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
        IF wl_itens-checkbox EQ 'X'.
          lv_value = 0.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'VALOR_IMP'
              i_value     = lv_value.
        ENDIF.

      ENDLOOP.
    ENDIF.

    LOOP AT er_data_changed->mt_good_cells
                                 INTO ls_good
                                 WHERE fieldname = 'VALOR_IMP'.
      CLEAR: wl_tcurr_g, wl_tcurr.
      lv_value  = ls_good-value.
      lv_valueg = ls_good-value.
      lv_valuef = ls_good-value.
      CONDENSE lv_value  NO-GAPS.
      CONDENSE lv_valueg NO-GAPS.
      CONDENSE lv_valuef NO-GAPS.

      IF wg_cadlan-data_atual IS NOT INITIAL.
        vdatax = wg_cadlan-data_atual.

        IF wg_cadlan-st_fecha IS NOT INITIAL.
          vdatax = |{ vdatax(4) }{ vdatax+4(2) }01|.
        ENDIF.

      ENDIF.

      DATA: taxa_moeda_forte TYPE tcurr-ukurs.

      CREATE OBJECT obj_zcl_util_sd.


      obj_zcl_util_sd->set_kurst('B').
      obj_zcl_util_sd->set_waerk('USD').
      obj_zcl_util_sd->set_tcurr('BRL').

      MOVE  vdatax TO vl_gdatu.
      obj_zcl_util_sd->set_data( vl_gdatu ).
      vl_ukurs = obj_zcl_util_sd->taxa_cambio( ).
      MOVE  vl_ukurs TO taxa_moeda_forte .


      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
      IF wl_itens-checkbox NE 'X'.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_IMP'
            i_value     = lv_value.


        IF taxa_moeda_forte LT 0.
          MULTIPLY taxa_moeda_forte BY -1.
          lv_valuef = lv_valuef * taxa_moeda_forte.
        ELSE.
          lv_valuef = lv_valuef / taxa_moeda_forte .
        ENDIF.


        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_FOR'
            i_value     = lv_valuef.
      ENDIF.

      IF wl_itens-kostl_c = 'X' OR wl_itens-xclasse = 'X' .

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_IMP'
            i_value     = lv_value.


      ELSEIF taxa_moeda_forte NE 0 AND wg_cadlan-waers NE wl_t005-waers .


        IF taxa_moeda_forte LT 0.
          MULTIPLY taxa_moeda_forte BY -1.
          lv_valuef = lv_valuef * taxa_moeda_forte.
        ELSE.
          lv_valuef = lv_valuef / taxa_moeda_forte .
        ENDIF.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_FOR'
            i_value     = lv_valuef.

        flag_r = 'X'.


        vg_sinal = '+'.
        IF wl_tcurr_g-ukurs LT 0.
          vg_sinal = '-'.
          MULTIPLY wl_tcurr_g-ukurs BY -1.
        ENDIF.

        flag_r = 'X'.
      ELSEIF  wg_cadlan-waers = wl_t005-waers .

        "MOEDA FORTE
        IF wg_cadlan-waers NE wl_t005-curha.
*

          IF taxa_moeda_forte LT 0.
            MULTIPLY  taxa_moeda_forte BY -1.
            lv_valuef = lv_valuef * taxa_moeda_forte .
          ELSE.
            lv_valuef = lv_valuef / taxa_moeda_forte.
          ENDIF.
          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'VALOR_FOR'
              i_value     = lv_valuef.
          flag_r = 'X'.
        ELSE.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = ls_good-row_id
              i_fieldname = 'VALOR_FOR'
              i_value     = lv_valuef.
        ENDIF.

        flag_r = 'X'.
      ENDIF.

    ENDLOOP.

    LOOP AT er_data_changed->mt_good_cells
                         INTO ls_good
                         WHERE fieldname = 'VALOR_FOR'.
      READ TABLE tg_itens INTO wl_itens INDEX ls_good-row_id.
      IF wl_itens-checkbox NE 'X'.

        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_FOR'
            i_value     = lv_valuef.


      ELSEIF wl_itens-kostl_c = 'X' OR wl_itens-xclasse = 'X' .
        lv_value = wl_itens-valor_for.
        CALL METHOD er_data_changed->modify_cell
          EXPORTING
            i_row_id    = ls_good-row_id
            i_fieldname = 'VALOR_FOR'
            i_value     = lv_value.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "ON_DATA_CHANGED

  METHOD on_data_changed_finished.

    IF ok-code NE 'AGR' AND sy-dynnr NE '0600'.

*** MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      PERFORM verifica_erros.
      PERFORM f_atualiza_alv.
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

    ELSEIF sy-dynnr EQ '0600'.

**** MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
*      CALL METHOD grid4->refresh_table_display
*        EXPORTING
*          is_stable = wa_stable.

      PERFORM verifica_erros.
*    PERFORM f_atualiza_alv.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '600'
          i_show        = space
          i_repid       = sy-repid
          i_pressed_tab = 'G_TAB_STRIP_IMP_cl-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.

    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED

  METHOD on_data_changed_finished2.
    DATA: wl_itens     LIKE LINE OF tg_itens_c,
          wl_itens_cod LIKE LINE OF tg_itens,
          xtotal       TYPE zimp_lanc_imp_ct-valor_imp,
          xtotalf      TYPE zimp_lanc_imp_ct-valor_for,
          xlinha       TYPE i.

    IF vcod_abertura IS NOT INITIAL.
      DELETE tg_itens_c WHERE cod_abertura = vcod_abertura.
      LOOP AT tg_itens_c2 INTO wl_itens.
        APPEND wl_itens TO tg_itens_c.
      ENDLOOP.
      xtotal = 0.
      xtotalf = 0.
      LOOP AT tg_itens_c2 INTO wl_itens.
        ADD wl_itens-valor_cus  TO xtotal.
        ADD wl_itens-valor_for  TO xtotalf.
      ENDLOOP.
      CLEAR xlinha.
      LOOP AT tg_itens INTO wl_itens_cod.
        IF wl_itens_cod-cod_abertura = vcod_abertura.
          xlinha = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF xtotal > 0.
        wl_itens_cod-kostl_c = 'X'.
        wl_itens_cod-valor_imp = xtotal.
        wl_itens_cod-valor_for = xtotalf.
      ELSE.
        wl_itens_cod-kostl_c = ' '.
        wl_itens_cod-valor_imp = 0.
        wl_itens_cod-valor_for = 0.
      ENDIF.

*-PBI 64611 - 23.08.2021 - JT - inicio
      IF xlinha IS NOT INITIAL.
        MODIFY tg_itens FROM wl_itens_cod INDEX xlinha TRANSPORTING kostl_c valor_imp valor_for.
      ENDIF.
*-PBI 64611 - 23.08.2021 - JT - fim

      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED2
ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION
*---------------------------------------------------------------------*
*       CLASS DRAGDROP_RECEIVER IMPLEMENTATION
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
CLASS lcl_dragdrop_receiver IMPLEMENTATION.
  METHOD node_double_click.

  ENDMETHOD.                    "DROP_COMPLETE
ENDCLASS.                    "LCL_DRAGDROP_RECEIVER IMPLEMENTATION

*ALRS FIM
*&---------------------------------------------------------------------*
*&      MODULE  TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
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

      IF screen-name = 'WG_CADLAN-DT_APURACAO' AND ( tg_fields-value = 1 AND screen-group1 EQ tg_fields-group1 ).
        IF vdt_apuracao = 'X' .
          screen-input     = 1.
        ELSE.
          screen-input     = 0.
          CLEAR wg_cadlan-dt_apuracao.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
      IF ( screen-name = 'WG_CADLAN-MES_APURACAO' OR screen-name = 'WG_CADLAN-ANO_APURACAO' ) AND ( tg_fields-value = 1 AND screen-group1 EQ tg_fields-group1 ) .
        IF vmes_apuracao = 'X' .
          screen-input     = 1.
        ELSE.
          screen-input     = 0.

          CLEAR: wg_cadlan-mes_apuracao,wg_cadlan-ano_apuracao.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
*** US - 128395 - Inicio - CBRAND
      IF ( screen-name = 'WG_CADLAN-QRCODE' OR  screen-name = 'WG_CADLAN-COD_BARRAS' ) AND wg_acao <> 'DISPLA'.
        IF wg_cadlan-tp_imposto IS NOT INITIAL.
          SELECT SINGLE * INTO @DATA(wl_zimp_tp_imp)
            FROM zimp_tipos_impos
            WHERE tp_arrec = @wg_cadlan-tp_imposto.

          IF wl_zimp_tp_imp-qrcode = 'X' AND screen-name = 'WG_CADLAN-COD_BARRAS'  .
            screen-input     = 0.
            "screen-invisible = 1.
          ELSE.
            IF wl_zimp_tp_imp-qrcode = 'X' AND screen-name = 'WG_CADLAN-QRCODE' .
              screen-input     = 1.
              "screen-invisible = 0.
            ELSE.
              IF wl_zimp_tp_imp-qrcode IS INITIAL AND screen-name = 'WG_CADLAN-QRCODE' .
                screen-input     = 0.
              ELSE.
                IF wl_zimp_tp_imp-qrcode IS INITIAL AND screen-name = 'WG_CADLAN-COD_BARRAS' .
                  screen-input     = 1.
                ENDIF.
              ENDIF.
            ENDIF.
          ENDIF.
        ELSE.
          screen-input     = 0.
        ENDIF.
        MODIFY SCREEN.
      ENDIF.
*** US - 128395 - Inicio - CBRAND
    ENDLOOP.
  ENDLOOP.
  IF xconv = 'N'.
    LOOP AT SCREEN.
      IF screen-name = 'WG_CADLAN-CONV_BANCO'.
        screen-input     = 1.
        MODIFY SCREEN.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDIF.
  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field."'WG_DESC_OPERACAO'.
  ENDIF.
ENDMODULE.                 " TRATA_FIELDS  OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: fcode TYPE TABLE OF sy-ucomm.

  REFRESH: fcode.


  IF wg_acao IS INITIAL OR wg_acao = c_displa.
    APPEND c_save TO fcode.
    APPEND c_deldoc TO fcode.
    IF xmodif = 'X' OR wg_acao IS INITIAL OR wg_cadlan-doc_imposto IS INITIAL.
      APPEND c_modif TO fcode.
    ENDIF.
  ELSEIF xmodif = 'X' .
    APPEND c_modif TO fcode.
  ENDIF.
  SET PF-STATUS 'Z001' EXCLUDING fcode.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '100'.
ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  TAB_STRIP_IMP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE tab_strip_imp_active_tab_get INPUT.
  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_strip_imp-tab1.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab1.
    WHEN c_tab_strip_imp-tab2.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab2.
    WHEN c_tab_strip_imp-tab3.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab3.
*    WHEN c_tab_strip_imp-tab4.
*      g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab4.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TAB_STRIP_IMP_ACTIVE_TAB_GET  INPUT
*&---------------------------------------------------------------------*
*&      MODULE  TAB_STRIP_IMP_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE tab_strip_imp_active_tab_set OUTPUT.
  PERFORM verifica_erros.
*  IF ok-code EQ 'AGR' AND g_tab_strip_imp-pressed_tab NE c_tab_strip_imp-tab4.
*    g_tab_strip_imp-pressed_tab = c_tab_strip_imp-tab4.
*  ENDIF.
  tab_strip_imp-activetab = g_tab_strip_imp-pressed_tab.
  CASE g_tab_strip_imp-pressed_tab.
    WHEN c_tab_strip_imp-tab1.
      g_tab_strip_imp-subscreen = '0200'.
    WHEN c_tab_strip_imp-tab2.
      g_tab_strip_imp-subscreen = '0500'.
    WHEN c_tab_strip_imp-tab3.
      g_tab_strip_imp-subscreen = '0400'.
*    WHEN c_tab_strip_imp-tab4.
*      g_tab_strip_imp-subscreen = '0700'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.                 " TAB_STRIP_IMP_ACTIVE_TAB_SET  OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE user_command_exit INPUT.
  CASE ok-code.
    WHEN c_back OR 'CANCEL'.
      SET SCREEN 0.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
*&      FORM  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM verifica_erros .
  DATA: wl_zimp_cad_imposto TYPE zimp_cad_imposto,
        wl_t001             TYPE t001,
        wl_tcurc            TYPE tcurc,
        wl_j_1bbranch       TYPE j_1bbranch,
        wl_aufk             TYPE aufk,
        wl_mara             TYPE mara,
        tl_imp_cad_imp_con  TYPE TABLE OF zimp_cad_imp_con,
        wl_linha(6),
        wl_linha2(6),
        tabix               TYPE sy-tabix,
        tl_lfa1             TYPE TABLE OF lfa1       WITH HEADER LINE,
        tl_kna1             TYPE TABLE OF kna1       WITH HEADER LINE,
        tl_csks             TYPE TABLE OF csks       WITH HEADER LINE,
        tl_tka02            TYPE TABLE OF tka02      WITH HEADER LINE,
        tl_tbsl             TYPE TABLE OF tbsl       WITH HEADER LINE,
        tl_aufk             TYPE TABLE OF aufk       WITH HEADER LINE,
        tl_cepc             TYPE TABLE OF cepc       WITH HEADER LINE,
        tl_cskb             TYPE TABLE OF cskb       WITH HEADER LINE.

  REFRESH: tg_msg_ret.
  CLEAR: tg_msg_ret, wl_zimp_cad_imposto,wl_t001.

* RJF - Início
  IF sy-dynnr EQ '0600' AND tg_itens_cl[] IS NOT INITIAL.

    DATA(tg_duplic) = tg_itens_cl[].
    SORT tg_duplic BY gsber cod_imposto.
    DELETE ADJACENT DUPLICATES FROM tg_duplic COMPARING gsber cod_imposto.
    IF sy-subrc IS INITIAL.

      MOVE: 'Duplicidade'             TO tg_msg_ret-msg,
            'gsber'                   TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'Filial vs Imposto' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.

    ENDIF.

    FREE: tl_imp_cad_imp_con.
    SELECT * FROM zimp_cad_imp_con
    INTO TABLE tl_imp_cad_imp_con
    FOR ALL ENTRIES IN tg_itens_cl
    WHERE cod_imposto EQ tg_itens_cl-cod_imposto.
*    AND cod_abertura EQ tg_itens_cl-cod_aberturad.

    LOOP AT tg_itens_cl ASSIGNING FIELD-SYMBOL(<fs_itens_cl>).

      IF <fs_itens_cl>-gsber IS INITIAL.

        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
              'GSBER'                   TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'FILIAL' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ELSE.
        SELECT SINGLE *
        FROM j_1bbranch
        INTO  wl_j_1bbranch
        WHERE bukrs = vg_bukrs
        AND   branch = <fs_itens_cl>-gsber.
        IF sy-subrc NE 0.
          MOVE 'GSBER'           TO tg_msg_ret-field.
          CONCATENATE TEXT-e04 ' FILIAL '  INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.

      IF <fs_itens_cl>-waers IS INITIAL.
        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
              'waers'                   TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'MOEDA' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ELSE.
        SELECT SINGLE *
           FROM tcurc
           INTO wl_tcurc
            WHERE  waers EQ <fs_itens_cl>-waers.
        IF sy-subrc NE 0.
          CONCATENATE TEXT-e04 ' MOEDA' INTO  tg_msg_ret-msg.
          MOVE 'WAERS'                  TO tg_msg_ret-field.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
      ENDIF.

      IF <fs_itens_cl>-cod_imposto IS INITIAL.
        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
              'cod_imposto'             TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'Código Imposto' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSE.
        SELECT SINGLE *
           FROM zimp_cad_imposto
           INTO wl_zimp_cad_imposto
            WHERE  cod_imposto EQ <fs_itens_cl>-cod_imposto.
        IF sy-subrc NE 0.
          MOVE 'COD_IMPOSTO'          TO tg_msg_ret-field.
          CONCATENATE TEXT-e04 ' COD.IMPOSTO' INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ELSEIF wl_zimp_cad_imposto-loekz = 'X'.
          MOVE 'COD_IMPOSTO'          TO tg_msg_ret-field.
          CONCATENATE TEXT-e10 <fs_itens_cl>-cod_imposto INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

      ENDIF.

      IF <fs_itens_cl>-observacao IS INITIAL.
        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
              'observacao'             TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'Texto Contábil' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*mes_apuracao
      IF <fs_itens_cl>-mes_apuracao  IS INITIAL.

        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
              'mes_apuracao'             TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'Mês Apuração' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
*ano_apuracao
      IF <fs_itens_cl>-ano_apuracao  IS INITIAL.

        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
              'ano_apuracao'             TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'Ano Apuração' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.


      ENDIF.
*cod_aberturad

      IF <fs_itens_cl>-cod_aberturad  IS INITIAL.

        MOVE: TEXT-e01                   TO tg_msg_ret-msg,
              'cod_aberturad'             TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'Código Abertura Débito' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*vlr_moeda_docd
      IF <fs_itens_cl>-vlr_moeda_docd  IS INITIAL.
        MOVE: TEXT-e01                   TO tg_msg_ret-msg,
              'vlr_moeda_docd'             TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'Valor Débito' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.
*cod_aberturac
      IF <fs_itens_cl>-cod_aberturac  IS INITIAL.

        MOVE: TEXT-e01                   TO tg_msg_ret-msg,
              'cod_aberturac'             TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'Código Abertura Crédito' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.

      ENDIF.

*icon
      IF <fs_itens_cl>-icon EQ icon_message_error.
        MOVE: TEXT-ex1                   TO tg_msg_ret-msg,
              'vlr_moeda_dc'             TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'analisar!'(ex2) INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF <fs_itens_cl>-kostl IS INITIAL AND <fs_itens_cl>-zcheck_ccusto IS NOT INITIAL.
        "Valida centro de custo.
        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
        'kostl'                         TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'centro de custo' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      IF <fs_itens_cl>-dt_apuracao IS INITIAL.
        "Valida centro de custo.
        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
        'dt_apuracao'                         TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'data da operação' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

      "Verifica centro de custo.
      LOOP AT tl_imp_cad_imp_con ASSIGNING FIELD-SYMBOL(<ls_imp_cad_imp_con>) WHERE cod_imposto EQ <fs_itens_cl>-cod_imposto.
        IF <ls_imp_cad_imp_con>-lifnr IS NOT INITIAL.
          <fs_itens_cl>-lifnr = <ls_imp_cad_imp_con>-lifnr.
        ENDIF.

        IF <ls_imp_cad_imp_con>-kunnr IS NOT INITIAL.
          <fs_itens_cl>-kunnr = <ls_imp_cad_imp_con>-kunnr.
        ENDIF.

        IF <ls_imp_cad_imp_con>-hkont IS NOT INITIAL.
          <fs_itens_cl>-hkont = <ls_imp_cad_imp_con>-hkont.
        ENDIF.
      ENDLOOP.


      "Verifica centro de custo.
      IF <fs_itens_cl>-kostl IS INITIAL.
        SELECT SINGLE *
        FROM tka02
        INTO @DATA(wl_tka02)
        WHERE bukrs  = @wg_cadlan-bukrs.
        IF sy-subrc EQ 0.
          MOVE wl_tka02-kokrs TO vkokrs.

          SELECT SINGLE *
          FROM cskb
          INTO @DATA(wl_cskb)
          WHERE  kokrs  = @wl_tka02-kokrs
          AND    kstar  = @<fs_itens_cl>-hkont
          AND    datab  LE @sy-datum
          AND    datbi  GE @sy-datum.

          IF sy-subrc EQ 0.

            "Informar o centro de custo"
            MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            'kostl'                         TO tg_msg_ret-field.
            CONCATENATE  tg_msg_ret-msg 'centro de custo' INTO tg_msg_ret-msg SEPARATED BY space.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.

            <fs_itens_cl>-zcheck_ccusto = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
      CLEAR: wl_tka02, wl_tka02.

    ENDLOOP.

    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
* RJF - Fim

  IF sy-dynnr NE '0600'.

    IF wg_cadlan-lote IS INITIAL.
      MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            'WG_CADLAN-LOTE'         TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'LOTE PAGAMENTO' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wg_cadlan-doc_imposto IS INITIAL .
      MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            'WG_CADLAN-DOC_IMPOSTO'         TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'DOCUMENTO' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
    IF wg_cadlan-bukrs IS INITIAL .
      MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            'WG_CADLAN-BUKRS'         TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'EMPRESA' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
         FROM t001
         INTO wl_t001
          WHERE  bukrs EQ wg_cadlan-bukrs.
      IF sy-subrc NE 0.
        CONCATENATE TEXT-e04 ' EMPRESA' INTO  tg_msg_ret-msg.
        MOVE 'WG_CADLAN-BUKRS'         TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wg_cadlan-waers IS INITIAL .
      MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            'WG_CADLAN-WAERS'         TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'MOEDA' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
         FROM tcurc
         INTO wl_tcurc
          WHERE  waers EQ wg_cadlan-waers.
      IF sy-subrc NE 0.
        CONCATENATE TEXT-e04 ' MOEDA' INTO  tg_msg_ret-msg.
        MOVE 'WG_CADLAN-WAERS'         TO tg_msg_ret-field.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wg_cadlan-cod_imposto IS  INITIAL .
      MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            'WG_CADLAN-COD_IMPOSTO'          TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'COD.IMPOSTO' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
         FROM zimp_cad_imposto
         INTO wl_zimp_cad_imposto
          WHERE  cod_imposto EQ wg_cadlan-cod_imposto.
      IF sy-subrc NE 0.
        MOVE 'WG_CADLAN-COD_IMPOSTO'          TO tg_msg_ret-field.
        CONCATENATE TEXT-e04 ' COD.IMPOSTO' INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ELSEIF wl_zimp_cad_imposto-loekz = 'X'.
        MOVE 'WG_CADLAN-COD_IMPOSTO'          TO tg_msg_ret-field.
        CONCATENATE TEXT-e10 '' INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.

    ENDIF.

*Inicio Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779
    IF wg_cadlan-tp_imposto EQ '14' AND wg_cadlan-zimp_lanc_impost EQ '00000000000000000000'.
      MOVE: TEXT-e01                      TO tg_msg_ret-msg,
            'WG_CADLAN-ZIMP_LANC_IMPOST'  TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'Nº Referencia GRU' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.
*Fim Alteração - Leandro Valentim Ferreira - 18.09.23 - #122779

    IF wg_cadlan-tp_imposto IS NOT INITIAL.
      SELECT SINGLE * INTO @DATA(wl_zimp_tp_imp)
        FROM zimp_tipos_impos
        WHERE tp_arrec = @wg_cadlan-tp_imposto.

      IF wl_zimp_tp_imp-qrcode = 'X' AND wg_cadlan-qrcode IS INITIAL..
        MOVE 'WG_CADLAN-QRCODE'   TO tg_msg_ret-field.
        CONCATENATE TEXT-e13 '' INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
     FROM t012k
     INTO wa_t012k
     WHERE bukrs = wg_cadlan-bukrs
     AND   hbkid = wg_cadlan-hbkid .

    IF sy-subrc NE 0.
      IF wg_cadlan-conv_banco = 'X'.
        MOVE: TEXT-e06                  TO tg_msg_ret-msg,
         'WG_CADLAN-CONV_BANCO' TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'CONVÊNIO BANCÁRIO' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ELSE.
*    IF WG_CADLAN-CONV_BANCO NE 'X'.
*      MOVE: TEXT-E07                  TO TG_MSG_RET-MSG,
*       'WG_CADLAN-CONV_BANCO' TO TG_MSG_RET-FIELD.
*      CONCATENATE  TG_MSG_RET-MSG 'CONVÊNIO BANCÁRIO' INTO TG_MSG_RET-MSG SEPARATED BY SPACE.
*      APPEND TG_MSG_RET.
*      CLEAR: TG_MSG_RET.
*    ENDIF.
    ENDIF.

    IF wg_cadlan-conv_banco IS NOT INITIAL AND wg_cadlan-hbkid IS INITIAL.
      MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            'WG_CADLAN-HBKID' TO tg_msg_ret-field.
      CONCATENATE  tg_msg_ret-msg 'BANCO' INTO tg_msg_ret-msg SEPARATED BY space.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSEIF NOT 'BBD_BBRA' CS wg_cadlan-hbkid.
      MOVE 'WG_CADLAN-HBKID' TO tg_msg_ret-field.
      CONCATENATE TEXT-e04 ' BANCO' INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.


    IF vdt_apuracao = 'X'.
      IF wg_cadlan-dt_apuracao IS  INITIAL .
        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
              'WG_CADLAN-DT_APURACAO'   TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'DT. APURAÇÃO' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF vmes_apuracao = 'X'.
      IF wg_cadlan-mes_apuracao IS INITIAL OR wg_cadlan-mes_apuracao IS INITIAL.
        MOVE: TEXT-e01                   TO tg_msg_ret-msg,
              'WG_CADLAN-MES_APURACAO'   TO tg_msg_ret-field.
        CONCATENATE  tg_msg_ret-msg 'MÊS/ANO APURAÇÃO' INTO tg_msg_ret-msg SEPARATED BY space.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF wg_cadlan-gsber IS INITIAL.
      MOVE: TEXT-e01                  TO tg_msg_ret-msg,
            'WG_CADLAN-GSBER'         TO tg_msg_ret-field.
      CONCATENATE TEXT-e01 ' FILIAL  '  INTO  tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ELSE.
      SELECT SINGLE *
      FROM j_1bbranch
      INTO  wl_j_1bbranch
      WHERE bukrs = wg_cadlan-bukrs
      AND   branch = wg_cadlan-gsber.
      IF sy-subrc NE 0.
        MOVE 'WG_CADLAN-GSBER'           TO tg_msg_ret-field.
        CONCATENATE TEXT-e04 ' FILIAL '  INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.

    IF  wg_cadlan-identificador IS INITIAL.
      IF '03_07' CS wg_cadlan-tp_imposto.
        MOVE: TEXT-e01                  TO tg_msg_ret-msg,
             'WG_CADLAN-IDENTIFICADOR'         TO tg_msg_ret-field.
        CONCATENATE TEXT-e01 ' IDENTIFICADOR  '  INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ELSE.
      REFRESH tl_lfa1.
      SELECT *
        FROM lfa1
        INTO TABLE tl_lfa1
        WHERE stcd1 = wg_cadlan-identificador.
      IF sy-subrc NE 0.
        MOVE 'WG_CADLAN-IDENTIFICADOR'       TO tg_msg_ret-field.
        CONCATENATE TEXT-e04 ' IDENTIFICADOR '  INTO  tg_msg_ret-msg.
        APPEND tg_msg_ret.
        CLEAR: tg_msg_ret.
      ENDIF.
    ENDIF.
    IF tg_itens[] IS NOT INITIAL.
      REFRESH tl_lfa1.
      SELECT *
        FROM lfa1
        INTO TABLE tl_lfa1
        FOR ALL ENTRIES IN tg_itens
        WHERE lifnr = tg_itens-lifnr.

      REFRESH tl_kna1.
      SELECT *
        FROM kna1
        INTO TABLE tl_kna1
        FOR ALL ENTRIES IN tg_itens
        WHERE kunnr = tg_itens-kunnr.

      SELECT *
       FROM tbsl
       INTO TABLE tl_tbsl
       FOR ALL ENTRIES IN tg_itens
        WHERE bschl EQ tg_itens-bschl.
    ENDIF.
    IF tg_itens_c[] IS NOT INITIAL.
      SELECT  *
         FROM cskb
         INTO TABLE tl_cskb
         WHERE  kokrs  = vkokrs
         AND    datab  LE sy-datum
         AND    datbi  GE sy-datum.

      SELECT *
       FROM csks
       INTO TABLE tl_csks
       FOR ALL ENTRIES IN tg_itens_c
       WHERE kokrs = vkokrs
       AND   kostl = tg_itens_c-kostl.

      SELECT  *
          FROM cepc
          INTO TABLE tl_cepc
          WHERE  kokrs  = vkokrs
          AND    datab  LE sy-datum
          AND    datbi  GE sy-datum.

      SELECT *
        FROM aufk
        INTO TABLE tl_aufk
        FOR ALL ENTRIES IN tg_itens_c
        WHERE aufnr EQ tg_itens_c-aufnr.

    ENDIF.

    SORT: tl_lfa1     BY lifnr,
          tl_kna1     BY kunnr,
          tl_csks     BY kostl,
          tl_tbsl     BY bschl,
          tg_itens_c  BY cod_abertura kostl.

    SORT:  tl_cskb     BY kstar,
           tl_cepc     BY prctr.

    xflagval = 'N'.
    xtotal = 0.
    xtotald = 0.
    LOOP AT tg_itens.
      wl_linha = sy-tabix.
      IF tg_itens-checkbox = 'X'.
        ADD tg_itens-valor_imp TO xtotal.
        IF tg_itens-valor_imp NE 0.
          xflagval = 'S'.
        ENDIF.
        ADD tg_itens-valor_for TO xtotald.
        IF wg_cadlan-waers_f = 'USD' AND tg_itens-valor_for EQ 0.
          MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e08 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.
        IF tg_itens-xclasse = 'X' AND tg_itens-kostl_c IS INITIAL.
          MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
          CONCATENATE TEXT-e02 ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
          APPEND tg_msg_ret.
          CLEAR: tg_msg_ret.
        ENDIF.

        READ TABLE tl_tbsl
              WITH KEY bschl = tg_itens-bschl
             BINARY SEARCH.

        IF tl_tbsl-koart = 'K'.
          IF tg_itens-lifnr IS INITIAL.
            MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e01 'FORNECEDOR, LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ELSE.
            READ TABLE tl_lfa1
            WITH KEY lifnr = tg_itens-lifnr
                       BINARY SEARCH.
            IF sy-subrc NE 0.
              MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e04 ' FORNECEDOR ' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.
        ENDIF.

        IF tl_tbsl-koart = 'D'.
          IF tg_itens-kunnr IS INITIAL.
            MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
            CONCATENATE TEXT-e01 'CLIENTE, LINHA: ' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ELSE.
            READ TABLE tl_kna1
            WITH KEY kunnr = tg_itens-kunnr
                       BINARY SEARCH.
            IF sy-subrc NE 0.
              MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e04 ' CLIENTE' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.
        ENDIF.

        IF tg_itens-umskz IS INITIAL.
          CLEAR: tl_tbsl.
          READ TABLE tl_tbsl INTO tl_tbsl WITH KEY bschl = tg_itens-bschl
                                                    xsonu = 'X'
                                           BINARY SEARCH.
          IF sy-subrc EQ 0.
            MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
            CONCATENATE 'RAZÃO ESPECIAL - OBRIGATÓRIO' ' LINHA: ' wl_linha INTO  tg_msg_ret-msg.
            APPEND tg_msg_ret.
            CLEAR: tg_msg_ret.
          ENDIF.
        ENDIF.

        LOOP AT tg_itens_c WHERE cod_abertura = tg_itens-cod_abertura.
          tabix  = sy-tabix.
          IF '40_50' CS tg_itens-bschl.
            CLEAR: tl_cskb.
            READ TABLE tl_cskb INTO tl_cskb WITH KEY kstar = tg_itens-hkont+0(10)
                                            BINARY SEARCH.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                EXPORTING
                  input  = tg_itens_c-prctr
                IMPORTING
                  output = tg_itens_c-prctr.

              IF tl_cskb-katyp NE '01'.
                IF tg_itens_c-matnr IS NOT INITIAL.
                  SELECT SINGLE *
                    FROM mara
                    INTO wl_mara
                   WHERE matnr = tg_itens_c-matnr.

                  IF sy-subrc IS NOT INITIAL.
                    MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                    CONCATENATE TEXT-e80 '' wl_linha INTO  tg_msg_ret-msg SEPARATED BY space.
                    APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
                  ENDIF.

                ELSE.
                  MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                  CONCATENATE TEXT-e01 'ARTIGO, LINHA: ' wl_linha INTO tg_msg_ret-msg SEPARATED BY space.
                  APPEND tg_msg_ret.  CLEAR: tg_msg_ret.
                ENDIF.
              ENDIF.

              IF tl_cskb-katyp EQ '01' AND tg_itens_c-matnr IS NOT INITIAL .
                MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                CONCATENATE TEXT-ee2 '' wl_linha INTO  tg_msg_ret-msg.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ENDIF.
              IF tl_cskb-katyp EQ '01' AND tg_itens_c-kostl IS INITIAL .
                IF tg_itens_c-aufnr IS INITIAL.
                  MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                  CONCATENATE TEXT-e12 '' wl_linha INTO  tg_msg_ret-msg.
                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
                ENDIF.
              ELSEIF tl_cskb-katyp EQ '01' AND tg_itens_c-aufnr IS NOT INITIAL .
                "CHECAR CCUSTO DA ORDEM AQUI
                SELECT SINGLE *
                     FROM aufk
                     INTO wl_aufk
                     WHERE bukrs  = wg_cadlan-bukrs
                     AND   aufnr  = tg_itens_c-aufnr.
                IF sy-subrc EQ 0.
                  IF tg_itens_c-kostl NE wl_aufk-kostv.
                    MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                    CONCATENATE TEXT-e77 '' wl_linha INTO  tg_msg_ret-msg.
                    APPEND tg_msg_ret.
                    CLEAR: tg_msg_ret.
                  ENDIF.
                ENDIF.
              ELSEIF '11_12' CS tl_cskb-katyp  AND tg_itens_c-prctr IS INITIAL AND tg_itens_c-matnr IS INITIAL.
                MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                CONCATENATE TEXT-e98 '' wl_linha INTO  tg_msg_ret-msg.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ELSEIF '11_12' CS tl_cskb-katyp AND tg_itens_c-prctr EQ '0000009900' AND tg_itens_c-matnr IS INITIAL.
                MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                CONCATENATE TEXT-e99 '' wl_linha INTO  tg_msg_ret-msg.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ELSEIF '11_12' CS tl_cskb-katyp AND tg_itens_c-prctr NE '0000009900' AND tg_itens_c-matnr IS NOT INITIAL.
                tg_itens_c-prctr = '0000009900'.
                MODIFY tg_itens_c INDEX tabix TRANSPORTING prctr.
              ELSEIF '11_12' CS tl_cskb-katyp  AND tg_itens_c-prctr IS NOT INITIAL.
                READ TABLE tl_cepc INTO tl_cepc WITH KEY  prctr = tg_itens_c-prctr
                                          BINARY SEARCH.
                IF sy-subrc NE 0.
                  MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                  CONCATENATE TEXT-e77 '' wl_linha INTO  tg_msg_ret-msg.
                  APPEND tg_msg_ret.
                  CLEAR: tg_msg_ret.
                ENDIF.
              ENDIF.
              IF '11_12' CS tl_cskb-katyp AND tg_itens_c-kostl IS NOT INITIAL.
                MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
                CONCATENATE TEXT-ee3 '' wl_linha INTO  tg_msg_ret-msg.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.
              ENDIF.
            ENDIF.
          ENDIF.
          IF tg_itens_c-kostl IS NOT INITIAL.
            wl_linha2 = sy-tabix.
            READ TABLE tl_csks
            WITH KEY kostl = tg_itens_c-kostl
                     BINARY SEARCH.
            IF sy-subrc NE 0.
              MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e04 ' CENTRO DE CUSTO LINHA: ' wl_linha '/' wl_linha2 INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.

          IF tg_itens_c-aufnr IS NOT INITIAL.
            wl_linha2 = sy-tabix.
            READ TABLE tl_aufk
            WITH KEY aufnr = tg_itens_c-aufnr
                     BINARY SEARCH.
            IF sy-subrc NE 0.
              MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
              CONCATENATE TEXT-e04 ' ORDEM LINHA: ' wl_linha '/' wl_linha2 INTO  tg_msg_ret-msg.
              APPEND tg_msg_ret.
              CLEAR: tg_msg_ret.
            ENDIF.
          ENDIF.

        ENDLOOP.
      ENDIF.
    ENDLOOP.

    IF xflagval EQ 'N'.
      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
      MOVE: TEXT-e05                   TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF wg_cadlan-waers_f = 'USD' AND xtotald NE 0..
      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
      MOVE: TEXT-e09                   TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

    IF xtotal NE 0.
      MOVE:  c_tab_strip_imp-tab1 TO tg_msg_ret-aba.
      MOVE: TEXT-e03                   TO tg_msg_ret-msg.
      APPEND tg_msg_ret.
      CLEAR: tg_msg_ret.
    ENDIF.

  ENDIF.

ENDFORM.                    " VERIFICA_ERROS
*&---------------------------------------------------------------------*
*&      MODULE  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
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
    wa_layout-sel_mode   = 'A'.
    wa_layout-cwidth_opt   = 'X'.
    wa_layout-box_fname    = 'MARK'.
    wa_layout-no_toolbar = c_x.

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
**      * REGISTER EVENT HANDLER
*    SET HANDLER OBG_TOOLBAR->ON_TOOLBAR FOR GRID1.
*    SET HANDLER OBG_TOOLBAR->HANDLE_USER_COMMAND FOR GRID1.

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

    SET HANDLER:
              lcl_event_handler=>on_double_click FOR grid1,
              lcl_event_handler=>on_data_changed_finished FOR grid1,
              lcl_event_handler=>on_data_changed FOR grid1.

*    POSICIONA SPLITER NA ALTURA X
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.
  ELSE.
    IF grid2 IS NOT INITIAL.
      PERFORM montar_layout_centro.
      IF wg_cadlan-waers_f = 'USD'.
        PERFORM montar_estrutura USING:
               5 'ZIMP_LANC_IMP_CT'         'VALOR_FOR'       'TG_ITENS_C' 'VALOR_FOR'        'VALOR U$'       '15' 'X' ' ' ' '.
      ENDIF.
      CALL METHOD grid2->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog[].
      CALL METHOD grid2->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

    REFRESH t_fieldcatalog.
    IF wg_cadlan-waers_f = 'USD'.
      PERFORM montar_layout.
      PERFORM montar_estrutura USING:
              10 'ZIMP_LANC_IMP_CT'         'VALOR_FOR'       'TG_ITENS' 'VALOR_FOR'        'VALOR U$'       '18' 'X' 'X' 'X'.

    ELSE.
      PERFORM montar_layout.
    ENDIF.
    LOOP AT t_fieldcatalog INTO w_fieldcatalog
       WHERE fieldname EQ 'KOSTL_'
          OR fieldname EQ 'GSBER'
          OR fieldname EQ 'HKONT'
          OR fieldname EQ 'DESCR_CAMP_GUIA'
          OR fieldname EQ 'VALOR_IMP'.
      IF w_fieldcatalog-fieldname EQ 'HKONT'.
        w_fieldcatalog-outputlen = '15'.
      ELSEIF w_fieldcatalog-fieldname EQ 'DESCR_CAMP_GUIA'.
        w_fieldcatalog-outputlen = '20'.
      ELSEIF wg_cadlan-lote IS NOT INITIAL AND wg_cadlan-cod_imposto IS NOT INITIAL.
        w_fieldcatalog-edit      = 'X'.
        w_fieldcatalog-emphasize = 'X'.
        IF w_fieldcatalog-fieldname EQ 'GSBER'.
          w_fieldcatalog-f4availabl = 'X'.
        ENDIF.
        IF w_fieldcatalog-fieldname EQ 'VALOR_IMP'.
          w_fieldcatalog-outputlen = '15'.
        ENDIF.
      ELSE.
        w_fieldcatalog-edit      = ' '.
        w_fieldcatalog-emphasize = ' '.
      ENDIF.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.
    LOOP AT t_fieldcatalog INTO w_fieldcatalog
       WHERE fieldname EQ 'KOSTL_'
          OR fieldname EQ 'VALOR_IMP'
          OR fieldname EQ 'VALOR_FOR'
          OR fieldname EQ 'CHECKBOX'.
      IF wg_acao = c_displa.
        w_fieldcatalog-edit     = ' '.
        w_fieldcatalog-emphasize = ' '.
      ELSE.
        w_fieldcatalog-edit      = 'X'.
        w_fieldcatalog-emphasize = 'X'.
      ENDIF.
      MODIFY t_fieldcatalog FROM w_fieldcatalog.
    ENDLOOP.

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    IF grid4 IS NOT INITIAL.
      PERFORM montar_layout_cl.
      CALL METHOD grid4->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog[].
      CALL METHOD grid4->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

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

  "GRID3
  IF obg_conteiner_err IS INITIAL.
    CREATE OBJECT obg_conteiner_err
      EXPORTING
        container_name = g_cc_err.


    CREATE OBJECT grid3
      EXPORTING
        i_parent = obg_conteiner_err.


    PERFORM montar_layout_err.

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

*    WA_LAYOUT-CWIDTH_OPT = C_X.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.

    "PERFORM MONTAR_LAYOUT_DOCS.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_filter            = tl_filter
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = it_zib_contabil_err[].

    CALL METHOD grid3->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

*  "Grid4
*  IF g_copia_lote IS INITIAL.
*    "RJF
**    WA_LAYOUT-CWIDTH_OPT = C_X.
*    wa_layout-zebra      = c_x.
**    WA_LAYOUT-NO_TOOLBAR = C_X.
**    WA_LAYOUT-SGL_CLK_HD = C_X.
*    wa_layout-no_rowmark = c_x.
**    WA_LAYOUT-COL_OPT    = C_X.
*    wa_stable-row        = c_x.
*    wa_layout-sel_mode   = 'A'.
*    wa_layout-cwidth_opt   = 'X'.
*    wa_layout-box_fname    = 'MARK'.
*
**  CREATE OBJECT g_copia_lote
**      EXPORTING
**        container_name = g_copialote.
**
**     CREATE OBJECT grid3
**      EXPORTING
**        i_parent = g_copia_lote.
*
*    CREATE OBJECT g_copia_lote
*      EXPORTING
*        container_name = g_copialote. "G_CONTAINER.
*
*    CREATE OBJECT splitter
*      EXPORTING
*        parent  = g_copia_lote
*        rows    = 2
*        columns = 1.
*
*    CALL METHOD splitter->get_container
*      EXPORTING
*        row       = 1
*        column    = 1
*      RECEIVING
*        container = container_4.
*
*    CREATE OBJECT grid4
*      EXPORTING
*        i_parent = container_4.
*
*    "RJF
*
*    PERFORM montar_layout_cl.
*    CREATE OBJECT obg_toolbar
*      EXPORTING
*        io_alv_grid = grid4.
*
**      * Register event handler
*    SET HANDLER obg_toolbar->on_toolbar FOR grid4.
*    SET HANDLER obg_toolbar->handle_user_command FOR grid4.
*
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_check.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
*    APPEND wl_function TO tl_function.
*
*    CALL METHOD grid4->set_table_for_first_display
*      EXPORTING
*        it_toolbar_excluding = tl_function
*        is_layout            = wa_layout
*      CHANGING
*        it_fieldcatalog      = t_fieldcatalog[]
*        it_outtab            = tg_itens[].
*
*    CALL METHOD grid4->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*    CALL METHOD grid4->register_edit_event
*      EXPORTING
*        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
**    lt_f4-fieldname = 'UMSKZ'.
**    lt_f4-register  = 'X'.
**    lt_f4-getbefore = 'X'.
**    APPEND lt_f4.
**
**
**    CALL METHOD grid4->register_f4_for_fields
**      EXPORTING
**        it_f4 = lt_f4[].
*
**    SET HANDLER:
**              LCL_EVENT_HANDLER=>ON_DOUBLE_CLICK FOR GRID4,
**              LCL_EVENT_HANDLER=>ON_DATA_CHANGED_FINISHED FOR GRID4,
**              LCL_EVENT_HANDLER=>ON_DATA_CHANGED FOR GRID4,
**              LCL_EVENT_HANDLER=>ON_ONF4         FOR GRID4.
*
*    SET HANDLER:
*              lcl_event_handler=>on_double_click FOR grid4,
*              lcl_event_handler=>on_data_changed_finished FOR grid4,
*              lcl_event_handler=>on_data_changed FOR grid4.
*
*
**    posiciona spliter na altura x
*    CALL METHOD splitter->set_row_height
*      EXPORTING
*        id     = 1
*        height = 100.
*  ELSE.
*    CALL METHOD grid4->set_frontend_fieldcatalog
*      EXPORTING
*        it_fieldcatalog = t_fieldcatalog[].
*
*    CALL METHOD grid4->refresh_table_display
*      EXPORTING
*        is_stable = wa_stable.
*
*
*  ENDIF.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: w_answer.
  DATA: p_bukrs  TYPE t001-bukrs,
        p_lote   TYPE zimp_lanc_impost-lote,
        p_desc   TYPE zimp_cad_lote-descr_lote,
        xtotal_r TYPE zimp_lanc_imp_ct-valor_imp,
        p_cheq   TYPE zimp_lanc_impost-st_fecha.
  DATA: ln_cont(1) TYPE n.
  DATA: cursorfield(30) TYPE c,
        cursorline(30)  TYPE c,
        cursorvalue(30) TYPE c.

  CASE ok-code.
    WHEN 'ZIMP57'.
      CALL TRANSACTION 'ZIMP57' AND SKIP FIRST SCREEN.
    WHEN 'PICK'.
      GET CURSOR FIELD cursorfield LINE cursorline VALUE cursorvalue.
      IF NOT wa_zib_contabil_chv IS INITIAL AND  NOT cursorvalue IS INITIAL AND cursorfield = 'WG_CADLAN-DOC_CONTABIL'.
        SET PARAMETER ID 'BLN' FIELD cursorvalue.
        SET PARAMETER ID 'BUK' FIELD wa_zib_contabil_chv-bukrs.
        SET PARAMETER ID 'GJR' FIELD wa_zib_contabil_chv-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN c_deldoc.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
*         TITLEBAR              = ' '
*         DIAGNOSE_OBJECT       = ' '
          text_question         = 'CONFIRMA A EXCLUSÃO DO IMPOSTO?'
          text_button_1         = 'SIM'(001)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'NÃO'(002)
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
        PERFORM eliminar_lancto.
      ENDIF.
    WHEN c_reinicia.
      PERFORM limpa_campos.
      CLEAR: wg_cadlan-bukrs,
             wg_cadlan-lote ,
             wg_cadlan-descr_lote.

      REFRESH: tg_fields.
      PERFORM trata_campos USING space
                               'GR4'
                                c_0       "INPUT 1     NO INPUT 0
                                c_1.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                                 'GR3'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                                 'GR2'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                                'GR1'
                                 c_1       "INPUT 1     NO INPUT 0
                                 c_0.      "INVISIBLE 1 VISIBLE 0

      CALL METHOD obg_descbox->set_text_as_r3table
        EXPORTING
          table = tg_editor.
      CALL METHOD obg_descbox->set_readonly_mode
        EXPORTING
          readonly_mode = 1.
    WHEN c_search.
      CLEAR xmodif.
      PERFORM busca_dados.
      PERFORM verifica_erros.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen      = '100'
          i_show        = ''
          i_repid       = sy-repid
          i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
          i_set_field   = 'X_FIELD'
        IMPORTING
          e_messagem    = wg_mensagem
        TABLES
          it_msgs       = tg_msg_ret.
    WHEN c_save.
      AUTHORITY-CHECK OBJECT 'A_S_WERK'
      ID 'BUKRS' FIELD wg_cadlan-bukrs
      ID 'WERKS' FIELD wg_cadlan-gsber.
      IF sy-subrc <> 0.
        CONCATENATE  'SEM AUTORIZAÇÃO EMPRESA/FILIAL' wg_cadlan-bukrs '/' wg_cadlan-gsber INTO vtexto SEPARATED BY space .
        MESSAGE vtexto TYPE 'E'.
        EXIT.
      ENDIF.
      DELETE tg_itens WHERE  cod_abertura IS INITIAL.

      CALL METHOD grid1->check_changed_data.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS INITIAL.
        CLEAR wg_acao.
        PERFORM grava_dados.
        REFRESH: tg_fields.
        PERFORM trata_campos USING space
                                'GR4'
                                   c_0       "INPUT 1     NO INPUT 0
                                   c_1.      "INVISIBLE 1 VISIBLE 0
        PERFORM trata_campos USING space
                                 'GR3'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
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
        MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'HÁ ERRO NO DOCUMENTO.'.
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
    WHEN c_displa.
      wg_acao = c_displa.
      PERFORM limpa_campos.
      CLEAR: wg_cadlan-lote, wg_cadlan-bukrs, wg_cadlan-descr_lote, wg_cadlan-doc_imposto.
      REFRESH: tg_fields.
      PERFORM trata_campos USING space
                               'GR4'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_1.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                                 'GR3'
                                    c_1       "INPUT 1     NO INPUT 0
                                    c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                           'GR2'
                              c_0       "INPUT 1     NO INPUT 0
                              c_0.      "INVISIBLE 1 VISIBLE 0
      PERFORM trata_campos USING space
                                'GR1'
                                 c_0       "INPUT 1     NO INPUT 0
                                 c_0.      "INVISIBLE 1 VISIBLE 0
    WHEN c_add.
      PERFORM f_action_add.

    WHEN c_cancel.
      CLEAR wg_acao.
    WHEN c_atuali.

    WHEN c_modif.
      IF wg_cadlan-doc_imposto IS NOT INITIAL.
        IF wg_acao = c_modif.
          CLEAR wg_acao.
          REFRESH: tg_fields.
          PERFORM trata_campos USING space
                                 'GR4'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_1.      "INVISIBLE 1 VISIBLE 0
          PERFORM trata_campos USING space
                                   'GR3'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
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
          REFRESH: tg_fields.
          PERFORM trata_campos USING space
                                 'GR4'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_1.      "INVISIBLE 1 VISIBLE 0
          PERFORM trata_campos USING space
                                   'GR3'
                                      c_0       "INPUT 1     NO INPUT 0
                                      c_0.      "INVISIBLE 1 VISIBLE 0
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
      ENDIF.
    WHEN c_show_msgre.
      " CLEAR WG_ACAO.
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
*---------------------------------------------
* Início RJF
    WHEN c_agr.
* Agrupamento Processamento
      vagrup = abap_true.
      FREE: tg_itens_cl, tg_imp_lanc_impost, tg_imp_lanc_imp_ct.
      IF wg_cadlan-lote IS NOT INITIAL.
        SELECT SINGLE lote, descr_lote, bukrs, usuario
          FROM zimp_cad_lote
          INTO @DATA(wl_lote)
          WHERE lote = @wg_cadlan-lote
          AND loekz = ''
          AND status_lote = ''.
        IF sy-subrc IS INITIAL AND wl_lote-lote IS NOT INITIAL.

          vg_bukrs = wl_lote-bukrs.
          SELECT
                a~doc_imposto,
                a~bukrs,
                a~lote,
                a~dt_venc,
                a~dt_apuracao,
                a~mes_apuracao,
                a~ano_apuracao,
                a~observacao,
                a~cod_imposto,
                a~ref_imposto,
                a~tp_imposto,
                a~cod_pgto,
                a~conv_banco,
                a~hbkid,
                a~gsber,
                a~waers,
                a~waers_f,
                a~identificador,
                a~cod_barras,
                a~qrcode,
                a~data_atual,
                a~hora_atual,
                a~usuario,
                a~loekz,
                a~moeda_gp_hist,
                a~st_fecha
            FROM zimp_lanc_impost AS a
            INNER JOIN j_1bbranch AS b
                                       ON a~bukrs  EQ b~bukrs
                                       AND a~gsber EQ b~branch
            INTO TABLE @DATA(lt_imp_lanc_impost)
            WHERE a~lote        EQ @wg_cadlan-lote
              AND a~loekz       EQ @abap_false.

          IF lt_imp_lanc_impost IS NOT INITIAL.

            MOVE-CORRESPONDING lt_imp_lanc_impost[] TO tg_imp_lanc_impost[].

            SELECT * FROM zimp_lanc_imp_ct
              INTO TABLE @tg_imp_lanc_imp_ct"@DATA(lt_imp_lanc_imp_ct)
              FOR ALL ENTRIES IN @lt_imp_lanc_impost
              WHERE bukrs        EQ @lt_imp_lanc_impost-bukrs
                AND doc_imposto  EQ @lt_imp_lanc_impost-doc_imposto.
            IF sy-subrc IS INITIAL.
              SORT tg_imp_lanc_imp_ct BY doc_imposto seqitem.
            ENDIF.

            IF tg_imp_lanc_imp_ct[] IS NOT INITIAL.
              SELECT *
               FROM tbsl
               INTO TABLE @DATA(tl_tbsl)
               FOR ALL ENTRIES IN @tg_imp_lanc_imp_ct
               WHERE bschl EQ @tg_imp_lanc_imp_ct-bschl.
              IF sy-subrc IS INITIAL.
                SORT tl_tbsl BY bschl.
              ENDIF.

              SELECT * FROM zimp_cad_imp_con
                INTO TABLE @DATA(tl_imp_cad_imp_con)
                FOR ALL ENTRIES IN @tg_imp_lanc_imp_ct
                WHERE cod_imposto EQ @tg_imp_lanc_imp_ct-cod_imposto
                  AND cod_abertura EQ @tg_imp_lanc_imp_ct-cod_abertura.
*                AND agrupamento EQ @abap_true.
              IF sy-subrc IS INITIAL.
                SORT tl_imp_cad_imp_con BY cod_imposto cod_abertura.
*            ELSE.
*              " Não é possível realizar o agrupamento
*              MESSAGE 'Não é possível realizar o agrupamento'(EE8) TYPE 'I'.
*              EXIT.
              ENDIF.
            ENDIF.

*          FREE ln_cont.
*          LOOP AT tg_itens WHERE checkbox IS NOT INITIAL.
*            READ TABLE tl_imp_cad_imp_con INTO DATA(wl_valid)
*                                        WITH KEY cod_abertura = tg_itens-cod_abertura
*                                        BINARY SEARCH.
*            IF sy-subrc IS INITIAL.
*              ln_cont = ln_cont + 1.
*            ENDIF.
*          ENDLOOP.
*
*          IF ln_cont LT 2.
*            MESSAGE 'Não é possível realizar o agrupamento(Contabilização)'(EE9) TYPE 'I'.
*            EXIT.
*          ENDIF.

            LOOP AT lt_imp_lanc_impost INTO DATA(wa_lanc).
              MOVE-CORRESPONDING wa_lanc TO tg_itens_cl.
              IF tg_itens_cl-mes_apuracao EQ 0 OR tg_itens_cl-mes_apuracao IS INITIAL.
                tg_itens_cl-mes_apuracao = wa_lanc-dt_apuracao+4(2).
              ENDIF.
              IF tg_itens_cl-ano_apuracao EQ 0 OR tg_itens_cl-ano_apuracao IS INITIAL.
                tg_itens_cl-ano_apuracao = wa_lanc-dt_apuracao(4).
              ENDIF.
              MOVE icon_led_yellow TO tg_itens_cl-icon. " MOVE icon_checked TO tg_itens_cl-icon.
              IF tg_itens_cl-observacao IS INITIAL AND tg_itens_cl-doc_imposto IS INITIAL.
                MOVE: wg_cadlan-observacao TO tg_itens_cl-observacao.
              ENDIF.

              LOOP AT tg_imp_lanc_imp_ct INTO DATA(wa_lanc_imp_ct) WHERE doc_imposto EQ wa_lanc-doc_imposto.



                IF tl_tbsl[] IS NOT INITIAL.

                  READ TABLE tl_tbsl ASSIGNING FIELD-SYMBOL(<fs_tbsl>)
                    WITH KEY bschl = wa_lanc_imp_ct-bschl
                    BINARY SEARCH.

                  IF wa_lanc_imp_ct-seqitem LE 2.

                    IF <fs_tbsl> IS ASSIGNED AND <fs_tbsl>-shkzg NE 'H'.
                      tg_itens_cl-kostl = wa_lanc_imp_ct-kostl.
                      tg_itens_cl-bschl = wa_lanc_imp_ct-bschl.
                      tg_itens_cl-cod_aberturad  = wa_lanc_imp_ct-cod_abertura.
                      tg_itens_cl-vlr_moeda_docd = wa_lanc_imp_ct-valor_imp.
                      ADD wa_lanc_imp_ct-valor_imp TO xtotal_r.
                      DATA(lv_cdeb) = abap_true.
                    ELSE.
                      IF wa_lanc_imp_ct-valor_imp GT 0.
                        tg_itens_cl-vlr_moeda_docc = wa_lanc_imp_ct-valor_imp  * ( - 1 ).
                      ELSE.
                        tg_itens_cl-vlr_moeda_docc = wa_lanc_imp_ct-valor_imp.
                      ENDIF.
                      tg_itens_cl-cod_aberturac  = wa_lanc_imp_ct-cod_abertura.
                      tg_itens_cl-bschlc = wa_lanc_imp_ct-bschl.

                      ADD wa_lanc_imp_ct-valor_imp TO xtotal_r.
                      DATA(lv_ccred) = abap_true.
                    ENDIF.
                  ENDIF.
                  IF wa_lanc_imp_ct-seqitem GT 2.

                    IF <fs_tbsl> IS ASSIGNED AND <fs_tbsl>-shkzg NE 'H'.
                      tg_itens_cl-kostl = wa_lanc_imp_ct-kostl.
                      tg_itens_cl-bschldc = wa_lanc_imp_ct-bschl.
                      tg_itens_cl-cod_aberturadc  = wa_lanc_imp_ct-cod_abertura.
                      tg_itens_cl-vlr_moeda_docdc = wa_lanc_imp_ct-valor_imp.
                      ADD wa_lanc_imp_ct-valor_imp TO xtotal_r.
                    ELSE.
                      IF wa_lanc_imp_ct-valor_imp GT 0.
                        tg_itens_cl-vlr_moeda_docdc = wa_lanc_imp_ct-valor_imp  * ( - 1 ).
                      ELSE.
                        tg_itens_cl-vlr_moeda_docdc = wa_lanc_imp_ct-valor_imp.
                      ENDIF.
                      tg_itens_cl-cod_aberturadc  = wa_lanc_imp_ct-cod_abertura.
                      tg_itens_cl-bschldc = wa_lanc_imp_ct-bschl.

                      ADD wa_lanc_imp_ct-valor_imp TO xtotal_r.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              IF xtotal_r NE 0.
                MOVE icon_message_error TO tg_itens_cl-icon.
                READ TABLE tg_imp_lanc_imp_ct
                ASSIGNING FIELD-SYMBOL(<fs_imp_ctx>)
                WITH KEY doc_imposto = wa_lanc-doc_imposto
                         valor_imp = xtotal_r.
                IF <fs_imp_ctx> IS ASSIGNED AND <fs_imp_ctx> IS NOT INITIAL.
                  tg_itens_cl-cod_aberturadc  = <fs_imp_ctx>-cod_abertura.
                ENDIF.
              ENDIF.

              APPEND tg_itens_cl TO tg_itens_cl.
              CLEAR: tg_itens_cl, xtotal_r.
            ENDLOOP.
          ENDIF.


*BUG IMPEDITIVO 96635* / Anderson Oenning
          "Setar forncedor, cliente e conta.
          LOOP AT tg_itens_cl ASSIGNING FIELD-SYMBOL(<ls_itens_cl>).
            LOOP AT tl_imp_cad_imp_con ASSIGNING FIELD-SYMBOL(<ls_imp_cad_imp_con>) WHERE cod_imposto EQ <ls_itens_cl>-cod_imposto.
              IF <ls_imp_cad_imp_con>-lifnr IS NOT INITIAL.
                <ls_itens_cl>-lifnr = <ls_imp_cad_imp_con>-lifnr.
              ENDIF.

              IF <ls_imp_cad_imp_con>-kunnr IS NOT INITIAL.
                <ls_itens_cl>-kunnr = <ls_imp_cad_imp_con>-kunnr.
              ENDIF.

              IF <ls_imp_cad_imp_con>-hkont IS NOT INITIAL.
                <ls_itens_cl>-hkont = <ls_imp_cad_imp_con>-hkont.
              ENDIF.
            ENDLOOP.


            SELECT SINGLE *
            FROM tka02
            INTO @DATA(wl_tka02)
            WHERE bukrs  = @wg_cadlan-bukrs.
            IF sy-subrc EQ 0.
              MOVE wl_tka02-kokrs TO vkokrs.

              SELECT SINGLE *
              FROM cskb
              INTO @DATA(wl_cskb)
              WHERE  kokrs  = @wl_tka02-kokrs
              AND    kstar  = @<ls_itens_cl>-hkont
              AND    datab  LE @sy-datum
              AND    datbi  GE @sy-datum.

              IF sy-subrc EQ 0.
                "Informar o centro de custo"
                MOVE: TEXT-e01                  TO tg_msg_ret-msg,
                'kostl'                         TO tg_msg_ret-field.
                CONCATENATE  tg_msg_ret-msg 'Informe o centro de custo' INTO tg_msg_ret-msg SEPARATED BY space.
                APPEND tg_msg_ret.
                CLEAR: tg_msg_ret.

                <ls_itens_cl>-zcheck_ccusto = abap_true.
              ENDIF.
            ENDIF.
            CLEAR: wl_tka02, wl_tka02.
          ENDLOOP.

          " Screen Agrupamento
          CALL SCREEN 0600 STARTING AT 10 1.

          vagrup = abap_false.
          FREE: tg_msg_ret.

          CALL FUNCTION 'Z_DOC_CHECK_NEW'
            EXPORTING
              i_screen      = '100'
              i_show        = ''
              i_repid       = sy-repid
              i_pressed_tab = 'G_TAB_STRIP_IMP-PRESSED_TAB'
              i_set_field   = 'X_FIELD'
            IMPORTING
              e_messagem    = wg_mensagem
            TABLES
              it_msgs       = tg_msg_ret.
        ELSE. " lote
          MESSAGE 'Lote não existente!'(i02) TYPE 'I'.
        ENDIF.
      ELSE.
        MESSAGE 'Preencher o campo Lote!'(i03) TYPE 'I'.
      ENDIF.
    WHEN c_exit.
      LEAVE PROGRAM.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      FORM  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM montar_layout .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 ' '                        ' '               'TG_ITENS' 'CHECKBOX'         ' '           '03' 'X' ' ' ' ',
        1 'ZIMP_CAMPOS_GUIA'         'COD_CAMP_GUIA'   'TG_ITENS' 'COD_ABERTURA'     'COD.ABER'    '10' ' ' ' ' ' ',
        2 'ZIMP_CAMPOS_GUIA'         'DESCR_CAMP_GUIA' 'TG_ITENS' 'DESCR_CAMP_GUIA'  'DESCRIÇÃO'   '20' ' ' ' ' ' ',
        3 'ZIMP_CAD_IMP_CON'         'BSCHL'           'TG_ITENS' 'BSCHL'            ' '           '07' ' ' ' ' ' ',
        3 'ZIMP_CAD_IMP_CON'         'UMSKZ'           'TG_ITENS' 'UMSKZ'            'RZ.ESP '     '05' ' ' ' ' ' ',
        4 'ZFIWRT0003'               'HKONT'           'TG_ITENS' 'HKONT'            'CONTA '      '10' ' ' ' ' ' ',
        5 'ZIMP_LANC_IMP_CT'         'LIFNR'           'TG_ITENS' 'LIFNR'            'FORNECEDOR'  '10' 'X' ' ' ' ',
        5 'ZIMP_LANC_IMP_CT'         'KUNNR'           'TG_ITENS' 'KUNNR'            'CLIENTE'     '10' 'X' ' ' ' ',
        6 ' '                        ' '               'TG_ITENS' 'KOSTL_C'          'C.CUSTO '    '08' ' ' ' ' ' ',
        8 'ZIMP_CAD_IMP_CON'         'DEBCRE'          'TG_ITENS' 'DEBCRE'           'D\C'         '05' ' ' ' ' ' ',
        9 'ZIMP_LANC_IMP_CT'         'VALOR_IMP'       'TG_ITENS' 'VALOR_IMP'        'VALOR R$'    '15' ' ' 'X' ' '.

ENDFORM.                    " MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*&      FORM  MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       TEXT
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

  IF p_field EQ 'CHECKBOX'.
    w_fieldcatalog-checkbox = c_x.
  ENDIF.

  IF p_field EQ 'KOSTL_C'.
    w_fieldcatalog-checkbox = c_x.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      FORM  BUSCA_DADOS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM busca_dados .
  DATA: wl_zimp_lanc_impost     TYPE zimp_lanc_impost,
        wl_zimp_cad_imposto     TYPE zimp_cad_imposto,
        wl_zimp_cad_lote        TYPE zimp_cad_lote,
        wl_zimp_tipos_impos     TYPE zimp_tipos_impos,
        wl_t001                 TYPE t001,
        wl_j_1bbranch           TYPE j_1bbranch,
        wl_t012                 TYPE t012,
        wl_bnka                 TYPE bnka,
        wl_tka02                TYPE tka02,
        wl_cskb                 TYPE cskb,
        tl_cskb                 TYPE TABLE OF cskb,
        wl_tbsl                 TYPE tbsl,
        tl_tbsl                 TYPE TABLE OF tbsl             WITH HEADER LINE,
        tl_zimp_lanc_imp_ct     TYPE TABLE OF zimp_lanc_imp_ct WITH HEADER LINE,
        tl_zimp_lanc_imp_ct_aux TYPE TABLE OF zimp_lanc_imp_ct WITH HEADER LINE,
        tl_zimp_cad_imp_con     TYPE TABLE OF zimp_cad_imp_con WITH HEADER LINE,
        tl_zimp_campos_guia     TYPE TABLE OF zimp_campos_guia WITH HEADER LINE,
        tl_cskt                 TYPE TABLE OF cskt             WITH HEADER LINE,
        wl_cont                 TYPE sy-tabix,
        wl_cont_aux             TYPE sy-tabix,
        wl_cont_aux2            TYPE sy-tabix,
        xtotal                  TYPE zimp_lanc_imp_ct-valor_imp VALUE 0,
        xobj_key                TYPE zib_contabil-obj_key,
        xlimpa(1),
        vdescr_imposto          TYPE zimp_cad_imposto-descr_imposto,
        xtotal_r                TYPE zimp_lanc_imp_ct-valor_imp,
        xtotal_u                TYPE zimp_lanc_imp_ct-valor_imp,
        xcontador               TYPE i.


  IF NOT wg_cadlan-descr_imposto IS INITIAL.
    MOVE wg_cadlan-cod_imposto TO wl_zimp_lanc_impost-cod_imposto.
    SELECT SINGLE *
    FROM t012k
    INTO wa_t012k
    WHERE bukrs = wg_cadlan-bukrs
    AND   hbkid = wg_cadlan-hbkid .
    xconv = 'S'.
    IF sy-subrc NE 0.
      IF wg_cadlan-conv_banco = 'X'.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'PARA ESTA EMPRESA NÃO EXISTE CONVÊNIO BANCÁRIO'.
        xconv = 'N'.
      ENDIF.
    ENDIF.

    SELECT SINGLE descr_imposto
       FROM zimp_cad_imposto
       INTO vdescr_imposto
        WHERE  cod_imposto EQ wg_cadlan-cod_imposto.

  ENDIF.

  IF wg_cadlan-lote IS NOT INITIAL. "  AND WG_ACAO = C_MODIF.
    SELECT SINGLE *
      FROM zimp_cad_lote
      INTO wl_zimp_cad_lote
       WHERE  lote EQ wg_cadlan-lote
       AND loekz = ''.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº DE LOTE NÃO ENCONTRADO!'.
      CLEAR wg_cadlan-lote.
      LEAVE TO SCREEN 100.
    ELSEIF wl_zimp_cad_imposto-loekz IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº DE IMPOSTO FOI ELIMINADO!'.
      LEAVE TO SCREEN 100.
    ELSEIF wl_zimp_cad_lote-status_lote = 'L'.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'LOTE JÁ LIBERADO!'.
      LEAVE TO SCREEN 100.
    ELSEIF wl_zimp_cad_lote-status_lote = 'A'.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'LOTE JÁ FINALIZADO!'.
      LEAVE TO SCREEN 100.
    ENDIF.
    wg_cadlan-bukrs = wl_zimp_cad_lote-bukrs.
    wg_cadlan-descr_lote = wl_zimp_cad_lote-descr_lote.
    wg_cadlan-dt_venc    = wl_zimp_cad_lote-dt_venc.

    SELECT SINGLE *
         FROM t001
         INTO wl_t001
         WHERE bukrs = wl_zimp_cad_lote-bukrs.

    IF sy-subrc = 0.
      MOVE: wl_t001-butxt   TO wg_cadlan-butxt.
    ENDIF.
  ENDIF.
  IF wg_cadlan-bukrs IS NOT INITIAL AND wg_cadlan-gsber IS NOT  INITIAL AND wg_acao NE c_displa.
    SELECT SINGLE *
      FROM j_1bbranch
      INTO wl_j_1bbranch
      WHERE bukrs = wg_cadlan-bukrs
      AND   branch = wg_cadlan-gsber.
    IF sy-subrc = 0.
      MOVE: wl_j_1bbranch-name   TO wg_cadlan-namefil.
    ENDIF.

  ENDIF.

  IF wg_cadlan-cod_imposto IS NOT INITIAL AND wg_acao IS NOT INITIAL AND wg_acao NE c_displa.
    SELECT *
     FROM zimp_lanc_imp_ct
     INTO TABLE tl_zimp_lanc_imp_ct
      WHERE doc_imposto  EQ wg_cadlan-doc_imposto
      AND   bukrs        EQ wg_cadlan-bukrs.
    IF sy-subrc EQ 0 OR wl_zimp_lanc_impost IS INITIAL OR vdescr_imposto NE wg_cadlan-descr_imposto.
      IF tl_zimp_lanc_imp_ct-cod_imposto NE wg_cadlan-cod_imposto.
        SELECT SINGLE *
        FROM zimp_cad_imposto
        INTO wl_zimp_cad_imposto
         WHERE  cod_imposto EQ wg_cadlan-cod_imposto.
        MOVE-CORRESPONDING wg_cadlan TO wg_cadlan_aux.
        MOVE-CORRESPONDING wl_zimp_cad_imposto TO wg_cadlan.
        MOVE: sy-uname TO wg_cadlan-usuario,
              sy-datum TO wg_cadlan-data_atual,
              sy-uzeit TO wg_cadlan-hora_atual.
        MOVE wg_cadlan_aux-bukrs TO wg_cadlan-bukrs.

        SELECT SINGLE *
            FROM t012k
            INTO wa_t012k
            WHERE bukrs = wg_cadlan-bukrs
            AND   hbkid = wg_cadlan-hbkid .
        xconv = 'S'.
        IF sy-subrc NE 0.
          IF wg_cadlan-conv_banco = 'X'.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'PARA ESTA EMPRESA NÃO EXISTE CONVÊNIO BANCÁRIO'.
            xconv = 'N'.
          ENDIF.
        ENDIF.

        IF wg_cadlan-gsber IS INITIAL.
          MOVE   wg_cadlan_aux-gsber TO wg_cadlan-gsber.
        ENDIF.
        "ENDIF.
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

*        SELECT SINGLE *
*          FROM T001
*          INTO WL_T001
*          WHERE BUKRS = WL_ZIMP_CAD_LOTE-BUKRS.
*
*        IF SY-SUBRC = 0.
*          MOVE: WL_T001-BUTXT   TO WG_CADLAN-BUTXT.
*        ENDIF.

        SELECT SINGLE *
          FROM zimp_lanc_impost
          INTO wl_zimp_lanc_impost
          WHERE doc_imposto  EQ wg_cadlan-doc_imposto
          AND   bukrs        EQ wg_cadlan-bukrs.

        SELECT SINGLE *
          FROM j_1bbranch
          INTO wl_j_1bbranch
          WHERE bukrs = wl_zimp_cad_lote-bukrs
          AND   branch = wl_zimp_lanc_impost-gsber.

        IF sy-subrc = 0.
          MOVE: wl_j_1bbranch-name   TO wg_cadlan-namefil.
        ENDIF.

        SELECT SINGLE *
          FROM zimp_tipos_impos
          INTO wl_zimp_tipos_impos
          WHERE tp_arrec = wl_zimp_cad_imposto-tp_imposto.

        IF sy-subrc = 0.
          MOVE: wl_zimp_tipos_impos-arrecadacao   TO wg_cadlan-arrecadacao,
                wl_zimp_tipos_impos-dt_apuracao   TO vdt_apuracao,
                wl_zimp_tipos_impos-mes_apuracao  TO vmes_apuracao.
        ENDIF.

        SELECT SINGLE *
          FROM t012
          INTO wl_t012
          WHERE bukrs = wg_cadlan-bukrs
          AND   hbkid = wl_zimp_cad_imposto-hbkid.

        IF sy-subrc = 0.
          SELECT SINGLE *
            FROM bnka
            INTO wl_bnka
            WHERE bankl = wl_t012-bankl .
          IF sy-subrc = 0.
            MOVE wl_bnka-banka TO wg_cadlan-banka.
          ENDIF.
        ENDIF.

        SELECT *
        FROM zimp_cad_imp_con
        INTO TABLE tl_zimp_cad_imp_con
         WHERE  cod_imposto EQ wg_cadlan-cod_imposto.

        SELECT *
        FROM tbsl
        INTO TABLE tl_tbsl
        FOR ALL ENTRIES IN tl_zimp_cad_imp_con
        WHERE bschl = tl_zimp_cad_imp_con-bschl.

        "
        SELECT *
        FROM zimp_campos_guia
        INTO TABLE tl_zimp_campos_guia
        FOR ALL ENTRIES IN tl_zimp_cad_imp_con
        WHERE cod_camp_guia = tl_zimp_cad_imp_con-cod_abertura.

        SORT: tl_zimp_campos_guia BY cod_camp_guia,
              tl_tbsl             BY bschl  .

        xlimpa = 'N'.
        IF tg_itens[] IS NOT INITIAL.
          READ TABLE tg_itens INDEX 1.
          IF wg_cadlan-cod_imposto NE tg_itens-cod_imposto.
            xlimpa = 'S'.
          ENDIF.
        ELSE.
          xlimpa = 'S'.
        ENDIF.

        IF xlimpa = 'S'.
          REFRESH: tg_itens.
          CLEAR tg_itens.
          LOOP AT tl_zimp_cad_imp_con.
            MOVE-CORRESPONDING tl_zimp_cad_imp_con TO tg_itens.

            SELECT SINGLE *
            FROM tka02
            INTO wl_tka02
            WHERE bukrs  = wg_cadlan-bukrs.
            MOVE wl_tka02-kokrs TO vkokrs.

            SELECT SINGLE *
              FROM cskb
              INTO wl_cskb
              WHERE  kokrs  = wl_tka02-kokrs
              AND    kstar  = tl_zimp_cad_imp_con-hkont
              AND    datab  LE sy-datum
              AND    datbi  GE sy-datum.

            IF sy-subrc = 0.
              MOVE 'X' TO tg_itens-xclasse.
            ENDIF.

            READ TABLE tl_tbsl
              WITH KEY bschl = tl_zimp_cad_imp_con-bschl.
            IF tl_tbsl-shkzg = 'H'.
              MOVE  'C'  TO tg_itens-debcre.
            ELSE.
              MOVE  'D'  TO tg_itens-debcre.
            ENDIF.

            READ TABLE tl_zimp_campos_guia
              WITH KEY cod_camp_guia = tl_zimp_cad_imp_con-cod_abertura.

            MOVE tl_zimp_campos_guia-descr_camp_guia TO tg_itens-descr_camp_guia.
            APPEND tg_itens.
            CLEAR: tg_itens.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF wg_cadlan-doc_imposto IS NOT INITIAL  AND wg_cadlan-bukrs IS NOT INITIAL AND wg_acao EQ c_displa.
    SELECT SINGLE *
      FROM zimp_lanc_impost
      INTO wl_zimp_lanc_impost
       WHERE doc_imposto  EQ wg_cadlan-doc_imposto
       AND   bukrs  EQ wg_cadlan-bukrs.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº DE LANÇAMENTO NÃO ENCONTRADO!'.
      LEAVE TO SCREEN 100.
    ELSEIF wl_zimp_lanc_impost-loekz IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nº DE LANÇAMENTO FOI ELIMINADO!'.
      LEAVE TO SCREEN 100.
    ELSE.
      MOVE-CORRESPONDING wl_zimp_lanc_impost TO wg_cadlan.
      SELECT SINGLE *
     FROM zimp_cad_lote
     INTO wl_zimp_cad_lote
      WHERE  lote EQ wg_cadlan-lote
      AND loekz = ''.
      wg_cadlan-descr_lote = wl_zimp_cad_lote-descr_lote.
      wg_cadlan-dt_venc = wl_zimp_cad_lote-dt_venc.


      SELECT SINGLE *
         FROM t012
         INTO wl_t012
         WHERE bukrs = wg_cadlan-bukrs
         AND   hbkid = wl_zimp_lanc_impost-hbkid.

      IF sy-subrc = 0.
        SELECT SINGLE *
          FROM bnka
          INTO wl_bnka
          WHERE bankl = wl_t012-bankl .
        IF sy-subrc = 0.
          MOVE wl_bnka-banka TO wg_cadlan-banka.
        ENDIF.
      ENDIF.

      CONCATENATE 'ZP' wl_zimp_lanc_impost-bukrs wl_zimp_lanc_impost-doc_imposto '%' INTO xobj_key.
      SELECT SINGLE obj_key belnr bukrs gjahr
        FROM zib_contabil_chv
        INTO wa_zib_contabil_chv
        WHERE obj_key LIKE xobj_key
        AND   bukrs   EQ wg_cadlan-bukrs.
      IF sy-subrc NE 0.
        CONCATENATE 'ZIMP' wl_zimp_lanc_impost-doc_imposto '%' INTO xobj_key.
        SELECT SINGLE obj_key belnr bukrs gjahr
       FROM zib_contabil_chv
       INTO wa_zib_contabil_chv
       WHERE obj_key LIKE xobj_key
       AND   bukrs   EQ wg_cadlan-bukrs.
      ENDIF.

      CLEAR wg_cadlan-iconc.
      IF sy-subrc = 0.
        wg_cadlan-doc_contabil =  wa_zib_contabil_chv-belnr.
        wg_cadlan-iconc        = icon_checked .
        SELECT SINGLE bukrs belnr gjahr budat
        FROM bkpf
        INTO wa_bkpf
        WHERE bukrs = wa_zib_contabil_chv-bukrs
        AND   belnr = wa_zib_contabil_chv-belnr
        AND   gjahr = wa_zib_contabil_chv-gjahr.
        IF sy-subrc = 0.
          wg_cadlan-budat = wa_bkpf-budat.
        ENDIF.
      ELSE.
        CLEAR: wg_cadlan-doc_contabil,
               wa_zib_contabil_chv,
               wg_cadlan-budat.
      ENDIF.
      REFRESH it_zib_contabil_err.
      CONCATENATE 'ZP' wl_zimp_lanc_impost-bukrs wl_zimp_lanc_impost-doc_imposto '%' INTO xobj_key.
      SELECT  obj_key nr_item interface dt_atualizacao hr_atualizacao type id num message message_v1 message_v2 message_v3  message_v4
       FROM zib_contabil_err
       INTO TABLE it_zib_contabil_err
       WHERE obj_key LIKE xobj_key.

      IF sy-subrc = 0.
        wg_cadlan-iconc        = icon_message_error.
      ENDIF.
      SELECT SINGLE *
         FROM t001
         INTO wl_t001
         WHERE bukrs = wl_zimp_lanc_impost-bukrs.

      IF sy-subrc = 0.
        MOVE: wl_t001-butxt   TO wg_cadlan-butxt.
      ENDIF.


      SELECT SINGLE *
        FROM j_1bbranch
        INTO wl_j_1bbranch
        WHERE bukrs = wl_zimp_lanc_impost-bukrs
        AND   branch = wl_zimp_lanc_impost-gsber.

      IF sy-subrc = 0.
        MOVE: wl_j_1bbranch-name   TO wg_cadlan-namefil.
      ENDIF.

      DELETE tg_fields WHERE group1 = 'GR4'.
      IF wl_zimp_cad_lote-status_lote EQ ''.
        PERFORM trata_campos USING space
                               'GR4'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_1.      "INVISIBLE 1 VISIBLE 0
      ELSE.
        xmodif = 'X'.
        PERFORM trata_campos USING space
                               'GR4'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
      ENDIF.
      IF wl_zimp_cad_lote-status_lote = 'L'.
        wg_cadlan-icon = icon_yellow_light.
      ELSEIF wl_zimp_cad_lote-status_lote = 'A'.
        wg_cadlan-icon = icon_green_light.
      ENDIF.
    ENDIF.

    SELECT SINGLE *
    FROM zimp_cad_imposto
    INTO wl_zimp_cad_imposto
    WHERE  cod_imposto EQ wl_zimp_lanc_impost-cod_imposto.
    MOVE  wl_zimp_cad_imposto-descr_imposto TO wg_cadlan-descr_imposto.

*    IF WG_CADLAN-WAERS IS INITIAL.
*      WG_CADLAN-WAERS = WL_ZIMP_CAD_IMPOSTO-WAERS.
*    ENDIF.
*
*    IF WG_CADLAN-BUKRS IS INITIAL.
*      WG_CADLAN-BUKRS = WL_ZIMP_CAD_IMPOSTO-BUKRS.
*      WG_CADLAN-GSBER = WL_ZIMP_CAD_IMPOSTO-GSBER.
*    ENDIF.


    SELECT SINGLE *
      FROM zimp_tipos_impos
      INTO wl_zimp_tipos_impos
      WHERE tp_arrec = wl_zimp_lanc_impost-tp_imposto.

    IF sy-subrc = 0.
      MOVE: wl_zimp_tipos_impos-arrecadacao   TO wg_cadlan-arrecadacao,
            wl_zimp_tipos_impos-dt_apuracao   TO vdt_apuracao,
            wl_zimp_tipos_impos-mes_apuracao  TO vmes_apuracao.
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

    IF wg_acao NE c_displa.
      CLEAR wg_acao.
    ENDIF.

    DELETE tg_fields WHERE group1 = 'GR1'.
    DELETE tg_fields WHERE group1 = 'GR2'.
    DELETE tg_fields WHERE group1 = 'GR3'.
    PERFORM trata_campos USING space
                               'GR3'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                               'GR2'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                              'GR1'
                               c_0       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0
    SELECT *
      FROM zimp_lanc_imp_ct
      INTO TABLE tl_zimp_lanc_imp_ct
       WHERE doc_imposto  EQ wg_cadlan-doc_imposto
       AND   bukrs        EQ wg_cadlan-bukrs.
    IF sy-subrc = 0.
      SELECT *
      FROM zimp_campos_guia
      INTO TABLE tl_zimp_campos_guia
      FOR ALL ENTRIES IN tl_zimp_lanc_imp_ct
      WHERE cod_camp_guia = tl_zimp_lanc_imp_ct-cod_abertura.

      SELECT *
       FROM tbsl
       INTO TABLE tl_tbsl
       FOR ALL ENTRIES IN tl_zimp_lanc_imp_ct
       WHERE bschl = tl_zimp_lanc_imp_ct-bschl.

      SORT: tl_zimp_campos_guia BY cod_camp_guia,
            tl_tbsl             BY bschl  .

      REFRESH: tg_itens,tg_itens_c.
      CLEAR tg_itens.

      SELECT SINGLE *
      FROM tka02
      INTO wl_tka02
      WHERE bukrs  = wg_cadlan-bukrs.

      MOVE wl_tka02-kokrs TO vkokrs.

      SELECT *
      FROM cskt
      INTO TABLE tl_cskt
      FOR ALL ENTRIES IN tl_zimp_lanc_imp_ct
      WHERE kokrs = vkokrs
      AND   kostl = tl_zimp_lanc_imp_ct-kostl.

      tl_zimp_lanc_imp_ct_aux[] = tl_zimp_lanc_imp_ct[].

      SORT: tl_zimp_lanc_imp_ct     BY cod_abertura seqitem,
            tl_zimp_lanc_imp_ct_aux BY cod_abertura seqitem,
            tl_cskt                 BY kostl.

      LOOP AT tl_zimp_lanc_imp_ct.
        MOVE-CORRESPONDING tl_zimp_lanc_imp_ct TO tg_itens.
        xtotal_r = 0.
        xtotal_u = 0.
        xcontador = 0.
        IF tl_zimp_lanc_imp_ct-cod_abertura = tl_zimp_lanc_imp_ct_aux-cod_abertura.
          CONTINUE.
        ENDIF.
        LOOP AT tl_zimp_lanc_imp_ct_aux WHERE cod_abertura = tl_zimp_lanc_imp_ct-cod_abertura.
          ADD tl_zimp_lanc_imp_ct_aux-valor_imp TO xtotal_r.
          ADD tl_zimp_lanc_imp_ct_aux-valor_for TO xtotal_u.
          IF NOT tl_zimp_lanc_imp_ct_aux-kostl IS INITIAL.
            ADD 1 TO xcontador.
            tg_itens_c-cod_abertura       = tl_zimp_lanc_imp_ct_aux-cod_abertura.
            tg_itens_c-kostl              = tl_zimp_lanc_imp_ct_aux-kostl.
            tg_itens_c-prctr              = tl_zimp_lanc_imp_ct_aux-prctr.
            tg_itens_c-matnr              = tl_zimp_lanc_imp_ct_aux-matnr.
            READ TABLE tl_cskt WITH KEY kostl = tl_zimp_lanc_imp_ct_aux-kostl BINARY SEARCH.
            IF sy-subrc = 0.
              tg_itens_c-ktext = tl_cskt-ktext.
            ELSE.
              CLEAR tg_itens_c-ktext.
            ENDIF.
            tg_itens_c-aufnr              = tl_zimp_lanc_imp_ct-aufnr.
            tg_itens_c-valor_cus          = tl_zimp_lanc_imp_ct_aux-valor_imp.
            tg_itens_c-valor_for          = tl_zimp_lanc_imp_ct_aux-valor_for.
            APPEND tg_itens_c.
          ENDIF.
        ENDLOOP.
        MOVE: xtotal_r TO tg_itens-valor_imp,
              xtotal_u TO tg_itens-valor_for.

        IF xcontador > 0.
          tg_itens-kostl_c = 'X'.
        ELSE.
          CLEAR tg_itens-kostl_c .
        ENDIF.

        IF tg_itens-valor_imp NE 0.
          tg_itens-checkbox = 'X'.
        ENDIF.

        SELECT SINGLE *
         FROM tka02
         INTO wl_tka02
         WHERE bukrs  = wg_cadlan-bukrs.
        MOVE wl_tka02-kokrs TO vkokrs.

        SELECT SINGLE *
          FROM cskb
          INTO wl_cskb
          WHERE  kokrs  = wl_tka02-kokrs
          AND    kstar  = tl_zimp_lanc_imp_ct-hkont
          AND    datab  LE sy-datum
          AND    datbi  GE sy-datum.

        IF sy-subrc = 0.
          MOVE 'X' TO tg_itens-xclasse.
        ENDIF.


        READ TABLE tl_tbsl
          WITH KEY bschl = tl_zimp_lanc_imp_ct-bschl.
        IF tl_tbsl-shkzg = 'H'.
          MOVE  'C'  TO tg_itens-debcre.
          tl_zimp_lanc_imp_ct-valor_imp = tl_zimp_lanc_imp_ct-valor_imp * -1.
        ELSE.
          MOVE  'D'  TO tg_itens-debcre.
        ENDIF.
        READ TABLE tl_zimp_campos_guia
          WITH KEY cod_camp_guia = tl_zimp_lanc_imp_ct-cod_abertura.

        MOVE tl_zimp_campos_guia-descr_camp_guia TO tg_itens-descr_camp_guia.
        APPEND tg_itens.
        CLEAR: tg_itens.
      ENDLOOP.

    ENDIF.
  ELSEIF  wg_acao EQ c_displa.
    IF wg_cadlan-doc_imposto IS INITIAL.
      MESSAGE s836(sd) WITH 'INFORME O DOCUMENTO'.
    ELSEIF  wg_cadlan-bukrs IS INITIAL.
      MESSAGE s836(sd) WITH 'INFORME A EMPRESA!'.
    ENDIF.
  ENDIF.

ENDFORM.                    " BUSCA_DADOS
*&---------------------------------------------------------------------*
*&      FORM  ELIMINAR_LANCTO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM eliminar_lancto .
  DATA: wl_zimp_lanc_impost TYPE zimp_lanc_impost.

  SELECT  SINGLE *
    FROM zimp_lanc_impost
    INTO wl_zimp_lanc_impost
     WHERE doc_imposto EQ wg_cadlan-doc_imposto
     AND   bukrs       EQ wg_cadlan-bukrs.

  IF sy-subrc IS INITIAL.
    IF wl_zimp_lanc_impost-loekz IS INITIAL.
      MOVE: c_x TO wl_zimp_lanc_impost-loekz.
      MODIFY zimp_lanc_impost FROM wl_zimp_lanc_impost.
      MESSAGE s836(sd) WITH 'O DOCUMENTO FOI ELIMINADO!'.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'IMPOSSIVEL ELIMINAR, O DOCUMENTO'
                            'JÁ FOI MARCADO PARA ELIMINAÇÃO!'.
    ENDIF.
  ENDIF.
ENDFORM.                    " ELIMINAR_LANCTO
*&---------------------------------------------------------------------*
*&      FORM  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: wl_input_cadlan TYPE zimp_lanc_impost,
        tl_input_cadlan TYPE TABLE OF zimp_lanc_imp_ct WITH HEADER LINE,
        vflag_cus(1).

  MOVE-CORRESPONDING wg_cadlan TO wl_input_cadlan.
  MOVE: sy-mandt                TO wl_input_cadlan-mandt,
        sy-uname TO wl_input_cadlan-usuario,
        sy-datum TO wl_input_cadlan-data_atual,
        sy-uzeit TO wl_input_cadlan-hora_atual.


  REFRESH: tg_editor.
  IF obg_descbox IS NOT INITIAL.
    CALL METHOD obg_descbox->get_text_as_r3table
      IMPORTING
        table = tg_editor.

    LOOP AT tg_editor INTO wg_editor.
      IF sy-tabix EQ 1.
        wl_input_cadlan-ref_imposto = wg_editor-line.

      ELSEIF sy-tabix GE 2.
        CONCATENATE wl_input_cadlan-ref_imposto  wg_editor-line INTO wl_input_cadlan-ref_imposto.

      ENDIF.
    ENDLOOP.
  ENDIF.
  SORT tg_itens_c BY cod_abertura.
  vseqitem = 0.
  LOOP AT tg_itens.
    MOVE-CORRESPONDING tg_itens TO tl_input_cadlan.

    MOVE: sy-mandt               TO tl_input_cadlan-mandt,
          sy-uname               TO tl_input_cadlan-usuario,
          sy-datum               TO tl_input_cadlan-data_atual,
          sy-uzeit               TO tl_input_cadlan-hora_atual,
          wg_cadlan-doc_imposto  TO tl_input_cadlan-doc_imposto,
          wg_cadlan-bukrs        TO tl_input_cadlan-bukrs.

    vflag_cus = 'N'.
    LOOP AT tg_itens_c WHERE cod_abertura = tg_itens-cod_abertura.
      vflag_cus = 'S'.
      ADD 1 TO vseqitem .
      MOVE: tg_itens_c-kostl       TO tl_input_cadlan-kostl,
            tg_itens_c-prctr       TO tl_input_cadlan-prctr,
            vseqitem               TO tl_input_cadlan-seqitem,
            tg_itens_c-aufnr       TO tl_input_cadlan-aufnr,
            tg_itens_c-matnr       TO tl_input_cadlan-matnr,
            tg_itens_c-valor_cus   TO tl_input_cadlan-valor_imp,
            tg_itens_c-valor_for   TO tl_input_cadlan-valor_for.
      APPEND tl_input_cadlan.
    ENDLOOP.
    IF vflag_cus = 'N'.
      ADD 1 TO vseqitem .
      MOVE vseqitem   TO tl_input_cadlan-seqitem.
      APPEND tl_input_cadlan.
    ENDIF.
    CLEAR tl_input_cadlan.
  ENDLOOP.
  "
  DELETE FROM zimp_lanc_imp_ct WHERE doc_imposto = wg_cadlan-doc_imposto
                               AND   bukrs       = wg_cadlan-bukrs.
  MODIFY zimp_lanc_impost FROM       wl_input_cadlan.
  MODIFY zimp_lanc_imp_ct FROM TABLE tl_input_cadlan.


  MESSAGE s836(sd) WITH 'LANÇAMENTO'
                         wg_cadlan-doc_imposto
                         ', CRIADO/MODIFICADO COM SUCESSO!'.
ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      FORM  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM limpa_campos .
  p_bukrs = wg_cadlan-bukrs.
  p_lote = wg_cadlan-lote.
  p_desc = wg_cadlan-descr_lote.
  p_cheq = wg_cadlan-st_fecha.
  CLEAR: wg_cadlan , tg_editor,wg_mensagem, vdt_apuracao, vmes_apuracao,xclasse,vkokrs, it_zib_contabil_err, x_field,xconv, vg_inf_identific.
  REFRESH: tg_itens,tg_itens_c,tg_itens_c2, tg_editor, tg_msg_ret.

  wg_cadlan-bukrs = p_bukrs.
  wg_cadlan-lote  = p_lote.
  wg_cadlan-descr_lote  = p_desc.
  wg_cadlan-st_fecha = p_cheq.

*  IF GRID2 IS NOT INITIAL.
*    CALL METHOD GRID2->FREE.
*  ENDIF.
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
ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      FORM  OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM obtem_proximo .
  DATA: vnum(10)            TYPE c,
        vseq(10)            TYPE p,
        wl_zimp_lanc_impost TYPE zimp_lanc_impost.

  IF wg_cadlan-bukrs IS NOT INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = 'ZT'
        object      = 'RF_BELEG'
        subobject   = wg_cadlan-bukrs
      IMPORTING
        number      = vseq.      "SUBOBJECT = W_ZIMP_CABECALHO-BUK

    vnum = vseq .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vnum
      IMPORTING
        output = vnum.

    wg_cadlan-doc_imposto = vnum.
    SELECT SINGLE *
      FROM zimp_lanc_impost
      INTO wl_zimp_lanc_impost
      WHERE bukrs       = wg_cadlan-bukrs
      AND   doc_imposto =  wg_cadlan-doc_imposto.

    IF sy-subrc = 0.
      CLEAR wg_cadlan-doc_imposto.
      MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'ERRO FATAL NA NUMERAÇÃO, DOCUMENTO JÁ EXISTE.'.
    ENDIF.

  ELSE.
    CLEAR wg_cadlan-doc_imposto.
  ENDIF.
ENDFORM.                    " OBTEM_PROXIMO
*&---------------------------------------------------------------------*
*&      FORM  TRATA_CAMPOS
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*      -->P_SPACE  TEXT
*      -->P_1740   TEXT
*      -->P_C_0  TEXT
*      -->P_C_0  TEXT
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
*&      MODULE  SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE search_lote INPUT.
  DATA: tl_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        tl_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: BEGIN OF tl_lote OCCURS 0,
          lote       TYPE zimp_cad_lote-lote,
          descr_lote TYPE zimp_cad_lote-descr_lote,
          bukrs      TYPE zimp_cad_lote-bukrs,
          usuario    TYPE zimp_cad_lote-usuario,
        END OF tl_lote.

  SELECT lote descr_lote bukrs usuario
    FROM zimp_cad_lote
    INTO TABLE tl_lote
    WHERE loekz = ''
    AND status_lote = ''.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LOTE'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_CAD_LOTE-LOTE'
      value_org       = 'S'
    TABLES
      value_tab       = tl_lote
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_LOTE  INPUT
*&---------------------------------------------------------------------*
*&      MODULE  INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE inicializa_tela OUTPUT.

  IF sy-calld = 'X' AND wg_cadlan-bukrs IS INITIAL.
    GET PARAMETER ID 'BUK' FIELD wg_cadlan-bukrs.
    GET PARAMETER ID 'BLN' FIELD wg_cadlan-doc_imposto.
    wg_acao = c_displa.

    IF ( wg_cadlan-bukrs IS INITIAL ) OR ( wg_cadlan-doc_imposto IS INITIAL ).
      CLEAR: wg_acao.
    ENDIF.
  ENDIF.

  IF wg_acao IS INITIAL.
    REFRESH: tg_fields.
    PERFORM trata_campos USING space
                                 'GR4'
                                    c_0       "INPUT 1     NO INPUT 0
                                    c_1.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                               'GR3'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                               'GR2'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                              'GR1'
                               c_1       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0
  ENDIF.

ENDMODULE.                 " INICIALIZA_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      MODULE  SEARCH_IMPOSTO  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE search_imposto INPUT.


  DATA: BEGIN OF tl_imposto OCCURS 0,
          cod_imposto   TYPE zimp_cad_imposto-cod_imposto,
          descr_imposto TYPE zimp_cad_imposto-descr_imposto,
        END OF tl_imposto.

  SELECT cod_imposto descr_imposto
    FROM zimp_cad_imposto
    INTO TABLE tl_imposto
    WHERE loekz = ''.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'COD_IMPOSTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_CAD_IMPOSTO-COD_IMPOSTO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_imposto
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_IMPOSTO  INPUT
*&---------------------------------------------------------------------*
*&      MODULE  VALIDA_PARAMETROS  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE valida_parametros INPUT.
  IF wg_cadlan-lote IS NOT INITIAL AND wg_cadlan-doc_imposto IS NOT INITIAL AND  wg_acao IS NOT INITIAL.
    PERFORM limpa_campos.
    CLEAR  wg_cadlan-bukrs.
  ENDIF.

ENDMODULE.                 " VALIDA_PARAMETROS  INPUT
*&---------------------------------------------------------------------*
*&      MODULE  SEARCH_FILIAL  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE search_filial INPUT.

  DATA: BEGIN OF tl_branch OCCURS 0,
          branch TYPE j_1bbranch-branch,
          name   TYPE j_1bbranch-name,
        END OF tl_branch.

  SELECT branch name
    FROM j_1bbrancht
    INTO TABLE tl_branch
    WHERE bukrs = wg_cadlan-bukrs
      AND language = sy-langu.


  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'BRANCH'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_LANC_IMPOST-GSBER'
      value_org       = 'S'
    TABLES
      value_tab       = tl_branch
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_FILIAL  INPUT
*&---------------------------------------------------------------------*
*&      MODULE  SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE search_doc INPUT.

  DATA  it_lote TYPE TABLE OF zimp_cad_lote WITH HEADER LINE.

  DATA: BEGIN OF tl_docs OCCURS 0,
          doc_imposto TYPE zimp_lanc_impost-doc_imposto,
          bukrs       TYPE zimp_lanc_impost-bukrs,
          lote        TYPE zimp_lanc_impost-lote,
          dt_venc     TYPE zimp_lanc_impost-dt_venc,
          descr_lote  TYPE zimp_cad_lote-descr_lote,
        END OF tl_docs.

  DATA: l_dynpfields LIKE dynpread OCCURS 0 WITH HEADER LINE.
  REFRESH l_dynpfields.
  CLEAR   l_dynpfields.
  IF wg_cadlan-bukrs IS  INITIAL.
    l_dynpfields-fieldname  = 'WG_CADLAN-BUKRS'.
    APPEND l_dynpfields.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname     = sy-repid
        dynumb     = sy-dynnr
      TABLES
        dynpfields = l_dynpfields.
    READ TABLE l_dynpfields INDEX 1.
    MOVE l_dynpfields-fieldvalue TO wg_cadlan-bukrs.
  ENDIF.

  IF wg_cadlan-bukrs IS NOT INITIAL.
    SELECT doc_imposto bukrs lote dt_venc
      FROM zimp_lanc_impost
      INTO TABLE tl_docs
      WHERE bukrs = wg_cadlan-bukrs.

    SELECT *
      FROM zimp_cad_lote
      INTO TABLE it_lote
      FOR ALL ENTRIES IN tl_docs
      WHERE lote = tl_docs-lote.
    SORT it_lote BY lote.
    LOOP AT tl_docs.
      READ TABLE it_lote
      WITH KEY lote = tl_docs-lote
               BINARY SEARCH.
      tl_docs-descr_lote = it_lote-descr_lote.
      MODIFY tl_docs .
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'DOC_IMPOSTO'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'ZIMP_LANC_IMPOST-DOC_IMPOSTO'
      value_org       = 'S'
    TABLES
      value_tab       = tl_docs
      return_tab      = tl_return_tab
      dynpfld_mapping = tl_dselc.
ENDMODULE.                 " SEARCH_DOC  INPUT
*&---------------------------------------------------------------------*
*&      FORM  MONTAR_LAYOUT_ERR
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM montar_layout_err .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'ZIB_CONTABIL_ERR'         'OBJ_KEY'        'IT_ZIB_CONTABIL_ERR' 'OBJ_KEY'         ' '   '20' ' ' ' ' ' ',
        1 'ZIB_CONTABIL_ERR'         'NR_ITEM'        'IT_ZIB_CONTABIL_ERR' 'NR_ITEM'         ' '   '10' ' ' ' ' ' ',
        2 'ZIB_CONTABIL_ERR'         'INTERFACE'      'IT_ZIB_CONTABIL_ERR' 'INTERFACE'       ' '   '15' ' ' ' ' ' ',
        3 'ZIB_CONTABIL_ERR'         'DT_ATUALIZACAO' 'IT_ZIB_CONTABIL_ERR' 'DT_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ',
        4 'ZIB_CONTABIL_ERR'         'HR_ATUALIZACAO' 'IT_ZIB_CONTABIL_ERR' 'HR_ATUALIZACAO'  ' '   '15' ' ' ' ' ' ',
        5 'ZIB_CONTABIL_ERR'         'TYPE'           'IT_ZIB_CONTABIL_ERR' 'TYPE'            ' '   '08' ' ' ' ' ' ',
        6 'ZIB_CONTABIL_ERR'         'ID'             'IT_ZIB_CONTABIL_ERR' 'ID'              ' '   '10' ' ' ' ' ' ',
        7 'ZIB_CONTABIL_ERR'         'NUM'            'IT_ZIB_CONTABIL_ERR' 'NUM'             ' '   '10' ' ' ' ' ' ',
        "8 'ZIB_CONTABIL_ERR'         'MESSAGE'        'IT_ZIB_CONTABIL_ERR' 'MESSAGE'         ' '   '20' ' ' ' ' ' ',
        8 ' '                        ' '              'IT_ZIB_CONTABIL_ERR' 'MESSAGE'         'MENSAGEM DE ERRO '   '100' ' ' ' ' ' ',
        9 'ZIB_CONTABIL_ERR'         'MESSAGE_V1'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V1'      ' '   '50' ' ' ' ' ' ',
       10 'ZIB_CONTABIL_ERR'         'MESSAGE_V2'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V2'      ' '   '30' ' ' ' ' ' ',
       11 'ZIB_CONTABIL_ERR'         'MESSAGE_V3'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V3'      ' '   '30' ' ' ' ' ' ',
       12 'ZIB_CONTABIL_ERR'         'MESSAGE_V4'     'IT_ZIB_CONTABIL_ERR' 'MESSAGE_V4'      ' '   '30' ' ' ' ' ' '.
ENDFORM.                    " MONTAR_LAYOUT_ERR
*&---------------------------------------------------------------------*
*&      MODULE  BUS_BAN  INPUT
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
MODULE bus_ban INPUT.
  DATA: wl_t012 TYPE t012,
        wl_bnka TYPE bnka.
  IF wg_cadlan-conv_banco = 'X'.
    wg_cadlan-hbkid = 'BBD'.
  ELSE.
    wg_cadlan-hbkid = 'BBRA'.
  ENDIF.
  SELECT SINGLE *
  FROM t012
  INTO wl_t012
  WHERE bukrs = wg_cadlan-bukrs
  AND   hbkid = wg_cadlan-hbkid.

  IF sy-subrc = 0.
    SELECT SINGLE *
      FROM bnka
      INTO wl_bnka
      WHERE bankl = wl_t012-bankl .
    IF sy-subrc = 0.
      MOVE wl_bnka-banka TO wg_cadlan-banka.
    ENDIF.
  ENDIF.
ENDMODULE.                 " BUS_BAN  INPUT
*&---------------------------------------------------------------------*
*&      FORM  MONTAR_LAYOUT_CENTRO
*&---------------------------------------------------------------------*
*       TEXT
*----------------------------------------------------------------------*
*  -->  P1        TEXT
*  <--  P2        TEXT
*----------------------------------------------------------------------*
FORM montar_layout_centro .
  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1 'CSKS'                     'KOSTL'           'TG_ITENS_C' 'KOSTL'            'C.CUSTO '           '20' 'X' ' ' ' ',
        2 'CSKT'                     'KTEXT'           'TG_ITENS_C' 'KTEXT'            ' '                  '30' ' ' ' ' ' ',
        3 'CEPC'                     'PRCTR'           'TG_ITENS_C' 'PRCTR'            'C.LUCRO '           '20' 'X' ' ' ' ',
        4 'AUFK'                     'AUFNR'           'TG_ITENS_C' 'AUFNR'            'ORDEM '             '18' 'X' ' ' ' ',
        5 'ZIMP_LANC_IMP_CT'         'MATNR'           'TG_ITENS_C' 'MATNR'            'ARTIGO '            '18' 'X' ' ' ' ',
        6 'ZIMP_LANC_IMP_CT'         'VALOR_IMP'       'TG_ITENS_C' 'VALOR_CUS'        'VALOR R$'           '15' 'X' ' ' ' '.

ENDFORM.                    " MONTAR_LAYOUT_CENTRO

MODULE completa_campos INPUT.

  PERFORM f_completa_identificador.

ENDMODULE.

FORM f_completa_identificador.

  DATA: v_lifnr TYPE lfa1-lifnr.

  CHECK wg_cadlan-gsber IS NOT INITIAL.

  CHECK ( vg_inf_identific IS INITIAL ) AND ( wg_cadlan-identificador IS INITIAL ).

  SELECT SINGLE *
    FROM j_1bbranch INTO @DATA(_wl_branch)
   WHERE branch = @wg_cadlan-gsber.

  CHECK sy-subrc EQ 0.

  v_lifnr = wg_cadlan-gsber.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = v_lifnr
    IMPORTING
      output = v_lifnr.

  SELECT SINGLE *
    FROM lfa1 INTO @DATA(_wl_lfa1)
   WHERE lifnr = @v_lifnr.

  CHECK sy-subrc EQ 0.

  wg_cadlan-identificador  = _wl_lfa1-stcd1.

  vg_inf_identific = abap_true.


ENDFORM.

FORM f_action_add.

  IF wg_cadlan-bukrs IS NOT INITIAL AND wg_acao NE c_displa.
    CLEAR xmodif.
    wg_acao = c_add.
    PERFORM limpa_campos.
    PERFORM obtem_proximo.
    REFRESH: tg_fields.
    PERFORM trata_campos USING space
                             'GR4'
                              c_0       "INPUT 1     NO INPUT 0
                              c_1.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                               'GR3'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                               'GR2'
                                  c_1       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                              'GR1'
                               c_1       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0

    CALL METHOD obg_descbox->set_text_as_r3table
      EXPORTING
        table = tg_editor.
    CALL METHOD obg_descbox->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
  ELSEIF wg_acao = c_displa.
    wg_acao = c_add.
    PERFORM limpa_campos.
    CLEAR: wg_cadlan-lote, wg_cadlan-bukrs, wg_cadlan-descr_lote.
    REFRESH: tg_fields.
    PERFORM trata_campos USING space
                            'GR4'
                             c_0       "INPUT 1     NO INPUT 0
                             c_1.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                               'GR3'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                               'GR2'
                                  c_0       "INPUT 1     NO INPUT 0
                                  c_0.      "INVISIBLE 1 VISIBLE 0
    PERFORM trata_campos USING space
                              'GR1'
                               c_1       "INPUT 1     NO INPUT 0
                               c_0.      "INVISIBLE 1 VISIBLE 0
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'INFORME A EMPRESA!'.
    CLEAR wg_cadlan-lote.
    LEAVE TO SCREEN 100.
  ENDIF.

ENDFORM.

MODULE inicia_lcto OUTPUT.

  DATA: v_lote_zimp TYPE zimp_cad_lote-lote.

  IF ( sy-calld   EQ 'X'                ) AND
     ( vg_chamada NE 'X'                ) AND
     ( wg_cadlan-doc_imposto IS INITIAL ) AND
     ( wg_cadlan-lote        IS INITIAL ).

    CLEAR: v_lote_zimp.

    vg_chamada = 'X'.

    GET PARAMETER ID 'LOTEZIMP' FIELD v_lote_zimp.
    IF v_lote_zimp IS NOT INITIAL.

      DELETE FROM MEMORY ID 'LOTEZIMP'.

      SELECT SINGLE *
        FROM zimp_cad_lote INTO @DATA(_wl_zimp_cad_lote)
       WHERE lote = @v_lote_zimp.

      CHECK sy-subrc EQ 0.

      wg_cadlan-lote  = _wl_zimp_cad_lote-lote.
      wg_cadlan-bukrs = _wl_zimp_cad_lote-bukrs.

      PERFORM f_action_add.
    ENDIF.

  ENDIF.

ENDMODULE.

FORM f_atualiza_alv.
  DATA: fg_doc(1),
        vlr_toler   TYPE zimp_lanc_imp_ct-vlr_moeda_doc,
        vlr_tot_doc TYPE zimp_lanc_imp_ct-vlr_moeda_doc,
        vlr_tot_for TYPE zimp_lanc_imp_ct-valor_for.

  CALL METHOD grid1->get_selected_cells
    IMPORTING
      et_cell = tg_selectedcell.

  CLEAR : fg_doc, vlr_tot_doc, vlr_tot_for.

  LOOP AT tg_selectedcell INTO wg_selectedcell.
    LOOP AT tg_itens INTO wg_itemmmm.

      IF wg_itemmmm-debcre = 'C'.

        IF wg_itemmmm-valor_for >= 0.
          wg_itemmmm-valor_for  = wg_itemmmm-valor_for * -1.
        ENDIF.

        IF wg_itemmmm-valor_imp >= 0.
          wg_itemmmm-valor_imp  = wg_itemmmm-valor_imp * -1.
        ENDIF.
      ENDIF.
      "Arredondamento
      ADD wg_itemmmm-valor_imp  TO vlr_tot_doc.
      ADD wg_itemmmm-valor_for TO vlr_tot_for.

      IF wg_cadlan-dt_venc IS NOT INITIAL AND ( wg_cadlan-dt_venc <  wg_cadlan-data_atual ).
        CLEAR: wg_cadlan-dt_venc.
        MODIFY tg_itens FROM wg_itemmmm.
        MESSAGE 'Dt.Vencimento deve ser maior que Dt.de Lançamento' TYPE 'I'.
        EXIT.
      ENDIF.

      CLEAR: wa_style.
      REFRESH: wg_itemmmm-style, style.
      IF wg_itemmmm-xclasse = 'X'.

        wa_style-fieldname = 'VALOR_FOR'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .

        wa_style-fieldname = 'VALOR_IMP'.
        wa_style-style = cl_gui_alv_grid=>mc_style_disabled.
        INSERT  wa_style INTO TABLE style .
      ENDIF.
      INSERT LINES OF style INTO TABLE  wg_itemmmm-style.

      MODIFY tg_itens FROM wg_itemmmm.
    ENDLOOP.

  ENDLOOP.

  IF wg_acao = c_displa.
    EXIT.
  ENDIF.


  vlr_toler = 2 / 100.

  IF fg_doc IS INITIAL AND vlr_tot_doc = 0 AND
      abs( vlr_tot_for ) GT 0
      AND  abs( vlr_tot_for ) LE vlr_toler. "Arredondar
    LOOP AT tg_itens INTO wg_itemmmm.
      IF wg_itemmmm-checkbox NE 'X'.
        CONTINUE.
      ENDIF.
      IF vlr_tot_for GT 0.
        IF wg_itemmmm-debcre = 'D'.
          SUBTRACT vlr_tot_for FROM wg_itemmmm-valor_for.
          MODIFY tg_itens FROM wg_itemmmm INDEX sy-tabix TRANSPORTING valor_for.
          IF wg_itemmmm-xclasse = 'X'.
            LOOP AT tg_obj INTO wg_obj WHERE seqitem = wg_itemmmm-seqitem.
              IF wg_obj-vlr_moeda_forte EQ 0.
                CONTINUE.
              ENDIF.
              vlr_tot_for = abs( vlr_tot_for ).
              SUBTRACT vlr_tot_for FROM wg_obj-vlr_moeda_forte.
              MODIFY tg_obj FROM wg_obj INDEX sy-tabix TRANSPORTING vlr_moeda_forte.
              EXIT.
            ENDLOOP.
          ENDIF.
          EXIT.
        ENDIF.
      ELSE.
        IF wg_itemmmm-debcre = 'C'.
          SUBTRACT vlr_tot_for FROM wg_itemmmm-valor_for.
          MODIFY tg_itens FROM wg_itemmmm INDEX sy-tabix TRANSPORTING valor_for.
          IF wg_itemmmm-xclasse = 'X'.
            LOOP AT tg_obj INTO wg_obj WHERE seqitem = wg_itemmmm-seqitem.
              IF wg_obj-vlr_moeda_forte EQ 0.
                CONTINUE.
              ENDIF.
              vlr_tot_for = abs( vlr_tot_for ).
              SUBTRACT vlr_tot_for FROM  wg_obj-vlr_moeda_forte.
              MODIFY tg_obj FROM wg_obj INDEX sy-tabix TRANSPORTING vlr_moeda_forte.
              EXIT.
            ENDLOOP.
          ENDIF.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0600  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0600 INPUT.

  DATA: tl_input_cadlanc  TYPE TABLE OF zimp_lanc_impost WITH HEADER LINE,
        tl_input_cadlancx TYPE TABLE OF zimp_lanc_impost WITH HEADER LINE.

  CASE ok-code.
    WHEN 'CANCEL'.
      FREE: tg_msg_ret.
      LEAVE TO SCREEN 0.
    WHEN c_show_msgre.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS NOT INITIAL.
        CALL FUNCTION 'Z_DOC_CHECK_NEW'
          EXPORTING
            i_screen      = '600'
            i_show        = c_x
            i_repid       = sy-repid
            i_pressed_tab = 'G_TAB_STRIP_IMP_CL-PRESSED_TAB'
            i_set_field   = 'X_FIELD'
          IMPORTING
            e_messagem    = wg_mensagem
          TABLES
            it_msgs       = tg_msg_ret.
      ENDIF.

*       MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
      CALL METHOD grid4->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN 'CANCCL'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'Deseja cancelar todos os lançamentos?'
          text_button_1         = 'SIM'(001)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'NÃO'(002)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = w_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF w_answer = '1'.

        DELETE tg_itens_cl[] WHERE doc_imposto IS INITIAL.
        IF tg_itens_cl[] IS NOT INITIAL.

          SELECT * FROM zimp_lanc_impost
            INTO TABLE tl_input_cadlancx
            FOR ALL ENTRIES IN tg_itens_cl[]
            WHERE doc_imposto EQ tg_itens_cl-doc_imposto
              AND bukrs       EQ tg_itens_cl-bukrs.

          IF sy-subrc IS INITIAL AND tl_input_cadlancx[] IS NOT INITIAL.

            LOOP AT tl_input_cadlancx.
              MOVE-CORRESPONDING tl_input_cadlancx TO tl_input_cadlanc.
              tl_input_cadlanc-loekz = abap_true.
              APPEND tl_input_cadlanc.
              CLEAR tl_input_cadlanc.
            ENDLOOP.
            IF tl_input_cadlanc[] IS NOT INITIAL.
              MODIFY zimp_lanc_impost FROM TABLE tl_input_cadlanc.
              IF sy-subrc IS INITIAL. "AND tg_msg_ret[] IS INITIAL.
                FREE tg_itens_cl.



                MESSAGE s836(sd) WITH 'LANÇAMENTO(S)'(ea1)
                                      'CANCELADOS COM SUCESSO!'(ea2).

*       MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
                CALL METHOD grid4->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.

              ELSE.
*                MESSAGE 'Erro!' TYPE 'E'.
              ENDIF.
            ENDIF.
          ELSE.
            MESSAGE 'Erro documento não encontrado!'(ea4) TYPE 'E'.
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN 'SALVACL'.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          text_question         = 'Deseja gerar todos os lançamentos?'(i01)
          text_button_1         = 'SIM'(001)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'NÃO'(002)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = w_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF w_answer = '1'.

        AUTHORITY-CHECK OBJECT 'A_S_WERK'
        ID 'BUKRS' FIELD wg_cadlan-bukrs
        ID 'WERKS' FIELD wg_cadlan-gsber.
        IF sy-subrc <> 0.
          CONCATENATE  'SEM AUTORIZAÇÃO EMPRESA/FILIAL' wg_cadlan-bukrs '/' wg_cadlan-gsber INTO vtexto SEPARATED BY space .
          MESSAGE vtexto TYPE 'E'.
          EXIT.
        ENDIF.

        CALL METHOD grid4->check_changed_data.
        PERFORM verifica_erros.
        IF tg_msg_ret[] IS INITIAL.
          PERFORM grava_dados_cl.
          REFRESH: tg_fields.
*          PERFORM trata_campos USING space
*                                    'GR1'
*                                     c_1       "INPUT 1     NO INPUT 0
*                                     c_0.      "INVISIBLE 1 VISIBLE 0
*          CALL METHOD obg_descbox->set_readonly_mode
*            EXPORTING
*              readonly_mode = 1.

        ELSE.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'HÁ ERRO NO DOCUMENTO.'.
          CALL FUNCTION 'Z_DOC_CHECK_NEW'
            EXPORTING
              i_screen      = '600'
              i_show        = c_x
              i_repid       = sy-repid
              i_pressed_tab = 'G_TAB_STRIP_IMP_CL-PRESSED_TAB'
              i_set_field   = 'X_FIELD'
            IMPORTING
              e_messagem    = wg_mensagem
            TABLES
              it_msgs       = tg_msg_ret.
        ENDIF.

        IF sy-subrc IS INITIAL AND tg_msg_ret[] IS INITIAL.
          MESSAGE s836(sd) WITH 'LANÇAMENTO(S)'
                                'CRIADO/MODIFICADO COM SUCESSO!'.

        ELSE.
*          MESSAGE 'Erro' TYPE 'E'.
        ENDIF.

      ENDIF.
    WHEN 'BTNCOPY'.

      IF zimp_cad_lote-lote IS NOT INITIAL.
        CLEAR: xtotal_r.

        SELECT SINGLE lote, descr_lote, bukrs, usuario
         FROM zimp_cad_lote
         INTO @DATA(wl_lotex)
         WHERE lote = @zimp_cad_lote-lote
         AND loekz = ''
         AND status_lote = ''.
        IF sy-subrc IS INITIAL AND wl_lotex-lote IS NOT INITIAL.

          SELECT
                 a~doc_imposto,
                 a~bukrs,
                 a~lote,
                 a~dt_venc,
                 a~dt_apuracao,
                 a~mes_apuracao,
                 a~ano_apuracao,
                 a~observacao,
                 a~cod_imposto,
                 a~ref_imposto,
                 a~tp_imposto,
                 a~cod_pgto,
                 a~conv_banco,
                 a~hbkid,
                 a~gsber,
                 a~waers,
                 a~waers_f,
                 a~identificador,
                 a~cod_barras,
                 a~qrcode,
                 a~data_atual,
                 a~hora_atual,
                 a~usuario,
                 a~loekz,
                 a~moeda_gp_hist,
                 a~st_fecha
             FROM zimp_lanc_impost AS a
             INNER JOIN j_1bbranch AS b
                                        ON a~bukrs  EQ b~bukrs
                                        AND a~gsber EQ b~branch
             INTO TABLE @DATA(lt_imp_lanc_impostx)
             WHERE
                   a~lote        EQ @zimp_cad_lote-lote
               AND a~loekz       EQ @abap_false.

          IF lt_imp_lanc_impostx IS NOT INITIAL.

            MOVE-CORRESPONDING lt_imp_lanc_impostx[] TO tg_imp_lanc_impostx[].

            SELECT * FROM zimp_lanc_imp_ct     " Lançamento de Impostos - Item
              INTO TABLE @tg_imp_lanc_imp_ctx  "@DATA(lt_imp_lanc_imp_ct)
              FOR ALL ENTRIES IN @lt_imp_lanc_impostx
              WHERE bukrs        EQ @lt_imp_lanc_impostx-bukrs
                AND doc_imposto  EQ @lt_imp_lanc_impostx-doc_imposto.
            IF sy-subrc IS INITIAL.
              SORT tg_imp_lanc_imp_ctx BY doc_imposto.
            ENDIF.

            IF tg_imp_lanc_imp_ctx[] IS NOT INITIAL.
              SELECT *
               FROM tbsl
               INTO TABLE @DATA(tl_tbslx)
               FOR ALL ENTRIES IN @tg_imp_lanc_imp_ctx
               WHERE bschl EQ @tg_imp_lanc_imp_ctx-bschl.
              IF sy-subrc IS INITIAL.
                SORT tl_tbslx BY bschl.
              ENDIF.

              SELECT * FROM zimp_cad_imp_con
                INTO TABLE @DATA(tl_imp_cad_imp_conx)
                FOR ALL ENTRIES IN @tg_imp_lanc_imp_ctx
                WHERE cod_imposto EQ @tg_imp_lanc_imp_ctx-cod_imposto
                  AND cod_abertura EQ @tg_imp_lanc_imp_ctx-cod_abertura
                  AND agrupamento EQ @abap_true.
              IF sy-subrc IS INITIAL.
                SORT tl_imp_cad_imp_conx BY cod_imposto cod_abertura.
              ELSE.
                " Não é possível realizar o agrupamento
                MESSAGE 'Não é possível realizar o agrupamento'(ee6) TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.

            LOOP AT lt_imp_lanc_impostx INTO DATA(wa_lancx).
              MOVE-CORRESPONDING wa_lancx TO tg_itens_cl.
              IF tg_itens_cl-mes_apuracao EQ 0 OR tg_itens_cl-mes_apuracao IS INITIAL.
                tg_itens_cl-mes_apuracao = wa_lancx-dt_apuracao+4(2).
              ENDIF.
              IF tg_itens_cl-ano_apuracao EQ 0 OR tg_itens_cl-ano_apuracao IS INITIAL.
                tg_itens_cl-ano_apuracao = wa_lanc-dt_apuracao(4).
              ENDIF.
              MOVE icon_led_yellow TO tg_itens_cl-icon.

              LOOP AT tg_imp_lanc_imp_ctx INTO DATA(wa_lanc_imp_ctx) WHERE doc_imposto EQ wa_lancx-doc_imposto.

                IF tl_tbslx[] IS NOT INITIAL.

                  READ TABLE tl_tbslx ASSIGNING FIELD-SYMBOL(<fs_tbslx>)
                    WITH KEY bschl = wa_lanc_imp_ctx-bschl
                    BINARY SEARCH.

                  IF <fs_tbslx> IS ASSIGNED AND <fs_tbslx>-shkzg NE 'H'.
                    tg_itens_cl-bschl = wa_lanc_imp_ctx-bschl.
                    tg_itens_cl-cod_aberturad  = wa_lanc_imp_ctx-cod_abertura.
                    tg_itens_cl-vlr_moeda_docd = wa_lanc_imp_ctx-valor_imp.
                    ADD wa_lanc_imp_ctx-valor_imp TO xtotal_r.
                  ELSE.
                    IF wa_lanc_imp_ctx-valor_imp GT 0.
                      tg_itens_cl-vlr_moeda_docc = wa_lanc_imp_ctx-valor_imp  * ( - 1 ).
                    ELSE.
                      tg_itens_cl-vlr_moeda_docc = wa_lanc_imp_ctx-valor_imp.
                    ENDIF.
                    tg_itens_cl-cod_aberturac  = wa_lanc_imp_ctx-cod_abertura.
                    tg_itens_cl-bschlc = wa_lanc_imp_ctx-bschl.

                    ADD wa_lanc_imp_ctx-valor_imp TO xtotal_r.
                  ENDIF.

                  IF wa_lanc_imp_ctx-seqitem GT 2.
                    IF <fs_tbslx> IS ASSIGNED AND <fs_tbslx>-shkzg NE 'H'.
                      tg_itens_cl-bschldc = wa_lanc_imp_ctx-bschl.
                      tg_itens_cl-cod_aberturadc  = wa_lanc_imp_ctx-cod_abertura.
                      tg_itens_cl-vlr_moeda_docdc = wa_lanc_imp_ctx-valor_imp.
                      ADD wa_lanc_imp_ctx-valor_imp TO xtotal_r.
                    ELSE.
                      IF wa_lanc_imp_ctx-valor_imp GT 0.
                        tg_itens_cl-vlr_moeda_docdc = wa_lanc_imp_ctx-valor_imp  * ( - 1 ).
                      ELSE.
                        tg_itens_cl-vlr_moeda_docdc = wa_lanc_imp_ctx-valor_imp.
                      ENDIF.
                      tg_itens_cl-cod_aberturadc  = wa_lanc_imp_ctx-cod_abertura.
                      tg_itens_cl-bschldc = wa_lanc_imp_ctx-bschl.

                      ADD wa_lanc_imp_ctx-valor_imp TO xtotal_r.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.

              IF xtotal_r NE 0.
                MOVE icon_message_error TO tg_itens_cl-icon.
                READ TABLE tg_imp_lanc_imp_ctx
                ASSIGNING FIELD-SYMBOL(<fs_imp_ctxx>)
                WITH KEY doc_imposto = wa_lancx-doc_imposto
                         valor_imp = xtotal_r.
                IF <fs_imp_ctxx> IS ASSIGNED AND <fs_imp_ctxx> IS NOT INITIAL.
                  tg_itens_cl-cod_aberturadc  = <fs_imp_ctxx>-cod_abertura.
                ENDIF.
              ENDIF.

              MOVE xtotal_r TO tg_itens_cl-vlr_moeda_docdc.
              CLEAR: tg_itens_cl-doc_imposto, xtotal_r.
              APPEND tg_itens_cl TO tg_itens_cl.
              CLEAR tg_itens_cl.
            ENDLOOP.
          ELSE.
            MESSAGE 'Lote não encontrado!'(ee4) TYPE 'E'.
          ENDIF.

*       MÉTODO DE ATUALIZAÇÃO DE DADOS NA TELA
          CALL METHOD grid4->refresh_table_display
            EXPORTING
              is_stable = wa_stable.

        ELSE.
          MESSAGE 'Lote não encontrado!'(i04) TYPE 'E'.
        ENDIF.

      ELSE.
        MESSAGE 'Campo Lote vazio!'(ee5) TYPE 'E'.
      ENDIF.
  ENDCASE.

  CALL METHOD grid4->check_changed_data.

  CALL METHOD grid4->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_CL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_cl .

  REFRESH t_fieldcatalog.
  PERFORM montar_estrutura USING:
        1  ' '                        ' '               'TG_ITENS_CL' 'ICON'              'Status '          '07' ' ' ' ' ' ',
        1  ' '                        ' '               'TG_ITENS_CL' 'CHECKBOX'          'Seleção '         '03' 'X' ' ' ' ',
        1  'TGSB'                     'GSBER_KONS'      'TG_ITENS_CL' 'GSBER'             'Filial '          '07' 'X' ' ' ' ',
        2  'ZIMP_LANC_IMPOST'         'WAERS'           'TG_ITENS_CL' 'WAERS'             'Moeda '           '04' 'X' ' ' ' ',
        3  'ZIMP_CAD_IMPOSTO'         'COD_IMPOSTO'     'TG_ITENS_CL' 'COD_IMPOSTO'       'Cód. Imposto '    '15' 'X' ' ' ' ',
        4  'ZIMP_LANC_IMPOST'         'COD_BARRAS'      'TG_ITENS_CL' 'COD_BARRAS'        'Cód. Barras '     '14' 'X' ' ' ' ',
        5  'ZIMP_LANC_IMPOST'         'OBSERVACAO'      'TG_ITENS_CL' 'OBSERVACAO'        'Texto contábil '  '20' 'X' ' ' ' ',
        6  'ZIMP_LANC_IMPOST'         'MES_APURACAO'    'TG_ITENS_CL' 'MES_APURACAO'      'Mês '             '03' 'X' ' ' ' ',
        7  'ZIMP_LANC_IMPOST'         'ANO_APURACAO'    'TG_ITENS_CL' 'ANO_APURACAO'      'Ano '             '05' 'X' ' ' ' ',
        8  'ZIMP_LANC_IMPOST'         'DT_APURACAO '    'TG_ITENS_CL' 'DT_APURACAO '      'Data apuração  '  'X'  'X' ' ' ' ',
        9  'CSKS            '         'KOSTL       '    'TG_ITENS_CL' 'KOSTL       '      'Centro de custo'  '13' 'X' ' ' ' ',
       10  'ZIMP_LANC_IMPOST'         'LIFNR       '    'TG_ITENS_CL' 'LIFNR       '      'Fornecedor'       '13' ' ' ' ' ' ',
       11  'ZIMP_LANC_IMPOST'         'KUNNR       '    'TG_ITENS_CL' 'KUNNR       '      'Cliente        '  '13' ' ' ' ' ' ',
       12  'ZIMP_LANC_IMPOST'         'HKONT       '    'TG_ITENS_CL' 'HKONT       '      'Conta do Razão '  '13' ' ' ' ' ' ',
       13  'ZIMP_CAD_IMP_CON'         'COD_ABERTURA'    'TG_ITENS_CL' 'COD_ABERTURAD'     'Cod.Aber(D) '     '11' ' ' ' ' ' ', "
       14  'ZIMP_CAD_IMP_CON'         'VLR_MOEDA_DOC'   'TG_ITENS_CL' 'VLR_MOEDA_DOCD'    'Valor(D) '        '09' 'X' ' ' ' ', "
       15  'ZIMP_CAD_IMP_CON'         'COD_ABERTURA'    'TG_ITENS_CL' 'COD_ABERTURAC'     'Cod.Aber(C) '     '11' ' ' ' ' ' ', "
       16  'ZIMP_CAD_IMP_CON'         'VLR_MOEDA_DOC'   'TG_ITENS_CL' 'VLR_MOEDA_DOCC'    'Valor(C) '        '09' 'X' ' ' ' ',
       17  'ZIMP_CAD_IMP_CON'         'COD_ABERTURA'    'TG_ITENS_CL' 'COD_ABERTURADC'    'Cod.Aber(DC) '    '12' ' ' ' ' ' ', "
       18  'ZIMP_CAD_IMP_CON'         'VLR_MOEDA_DOC'   'TG_ITENS_CL' 'VLR_MOEDA_DOCDC'   'Valor(DC) '       '10' 'X' ' ' ' ',
       19  'ZIMP_LANC_IMPOST'         'DOC_IMPOSTO'     'TG_ITENS_CL' 'DOC_IMPOSTO'       'Nro.documento '   '15' ' ' ' ' ' '. "

  MOVE t_fieldcatalog TO t_fieldcatalog_cl.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  TB_STRIP_IMP_ACT_SET_CL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tb_strip_imp_act_set_cl OUTPUT.
  CLEAR zimp_cad_lote-lote.
  tab_strip_imp_cl-activetab = g_tab_strip_imp_cl-pressed_tab.
  CASE g_tab_strip_imp_cl-pressed_tab.
    WHEN c_tab_strip_imp_cl-tab1.
      g_tab_strip_imp_cl-subscreen = '0700'.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS_CL  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos_cl OUTPUT.
  " Grid4 - Agrupamento
  IF g_copia_lote IS INITIAL.
    wa_layout-zebra        = c_x.
    wa_layout-no_rowmark   = c_x.
    wa_stable-row          = c_x.
    wa_layout-sel_mode     = 'A'.
    wa_layout-cwidth_opt   = c_x.
    wa_layout-box_fname    = 'MARK'.

    CREATE OBJECT g_copia_lote
      EXPORTING
        container_name = g_copialote. "G_CONTAINER.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_copia_lote
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_4.

    CREATE OBJECT grid4
      EXPORTING
        i_parent = container_4.

    PERFORM montar_layout_cl.
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid4.

*    Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid4.
    SET HANDLER obg_toolbar->handle_user_command FOR grid4.

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



    CALL METHOD grid4->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_itens_cl[].

    CALL METHOD grid4->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid4->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
              lcl_event_handler=>on_double_click FOR grid4,
              lcl_event_handler=>on_data_changed_finished FOR grid4,
              lcl_event_handler=>on_data_changed FOR grid4.

*    Posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.

    CALL METHOD grid4->check_changed_data.

    PERFORM montar_layout_cl.
    CALL METHOD grid4->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].
    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ELSE.

    PERFORM montar_layout_cl.
    CALL METHOD grid4->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = t_fieldcatalog[].
    CALL METHOD grid4->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0600  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0600 OUTPUT.

  SET PF-STATUS 'Z006'.
  CALL METHOD cl_gui_cfw=>dispatch.
  SET TITLEBAR '600'.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS_CL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados_cl .

  DATA: wl_input_cadlan     TYPE zimp_lanc_impost,
        wl_item_clx         LIKE LINE OF tg_itens_cl,
        tl_zimp_cad_imp_con TYPE TABLE OF zimp_cad_imp_con,
        tl_input_cadlanc    TYPE TABLE OF zimp_lanc_impost WITH HEADER LINE,
        tl_input_cadlan     TYPE TABLE OF zimp_lanc_imp_ct WITH HEADER LINE,
        vflag_cus(1).

*Estrutura de rep. para pegar filiais
  DELETE tg_itens_cl WHERE gsber IS INITIAL.
  MOVE: tg_itens_cl[] TO tg_itens_clx[].

  SORT tg_itens_clx BY gsber cod_imposto.
  DELETE ADJACENT DUPLICATES FROM tg_itens_clx COMPARING gsber cod_imposto.

  FREE: tl_imp_cad_imp_con.
  SELECT * FROM zimp_cad_imp_con INTO TABLE tl_imp_cad_imp_con
    FOR ALL ENTRIES IN tg_itens_cl
    WHERE cod_imposto EQ tg_itens_cl-cod_imposto.
  SORT tl_imp_cad_imp_con BY cod_imposto cod_abertura.

  LOOP AT tg_itens_clx ASSIGNING FIELD-SYMBOL(<wl_itens_clx>)
                       WHERE icon NE icon_message_error. "Cab...

*    MOVE-CORRESPONDING wg_cadlan  TO tl_input_cadlanc.
    tl_input_cadlanc-observacao   = <wl_itens_clx>-observacao.
    tl_input_cadlanc-cod_barras   = <wl_itens_clx>-cod_barras.
    tl_input_cadlanc-qrcode       = <wl_itens_clx>-qrcode.
    tl_input_cadlanc-lote         = wg_cadlan-lote.
    tl_input_cadlanc-bukrs        = vg_bukrs.
    tl_input_cadlanc-waers        = <wl_itens_clx>-waers.
    tl_input_cadlanc-cod_imposto  = <wl_itens_clx>-cod_imposto.
    tl_input_cadlanc-dt_apuracao  = <wl_itens_clx>-dt_apuracao.
    tl_input_cadlanc-dt_venc      = wg_cadlan-dt_venc.
    tl_input_cadlanc-mes_apuracao = <wl_itens_clx>-mes_apuracao.
    tl_input_cadlanc-ano_apuracao = <wl_itens_clx>-ano_apuracao.
    tl_input_cadlanc-kostl        = <wl_itens_clx>-kostl.
    tl_input_cadlanc-gsber        = <wl_itens_clx>-gsber.

    SELECT SINGLE tp_imposto, cod_pgto,conv_banco,hbkid
    FROM zimp_cad_imposto
    INTO ( @tl_input_cadlanc-tp_imposto, @tl_input_cadlanc-cod_pgto, @tl_input_cadlanc-conv_banco, @tl_input_cadlanc-hbkid )
    WHERE  cod_imposto EQ @<wl_itens_clx>-cod_imposto.

    SELECT SINGLE *
        FROM j_1bbranch
        INTO  @DATA(wl_j_1bbranch)
        WHERE bukrs = @vg_bukrs
        AND   branch = @<wl_itens_clx>-gsber.

    tl_input_cadlanc-identificador = wl_j_1bbranch-stcd1.

    IF <wl_itens_clx>-doc_imposto IS INITIAL. "Cria

      PERFORM obtem_prox_cl.

      IF wg_cadlan_cl-doc_imposto IS NOT INITIAL.
        tl_input_cadlanc-doc_imposto = wg_cadlan_cl-doc_imposto.
        <wl_itens_clx>-doc_imposto = tl_input_cadlanc-doc_imposto.

      ELSE.
* marcar verm.
        SORT tg_itens_cl BY gsber.
        READ TABLE tg_itens_cl INTO DATA(wa_itemm) WITH KEY gsber = <wl_itens_clx>-gsber
                               BINARY SEARCH.
        IF sy-subrc IS INITIAL.
* marcar verd.
          MOVE-CORRESPONDING <wl_itens_clx> TO wa_itemm.
          MOVE icon_message_error TO wa_itemm-icon.
          MODIFY tg_itens_cl FROM wa_itemm INDEX sy-tabix.
        ENDIF.
        CONTINUE.
      ENDIF.

    ELSE.
      tl_input_cadlanc-doc_imposto = <wl_itens_clx>-doc_imposto.
    ENDIF.

    MOVE: sy-mandt TO tl_input_cadlanc-mandt,
          sy-uname TO tl_input_cadlanc-usuario,
          sy-datum TO tl_input_cadlanc-data_atual,
          sy-uzeit TO tl_input_cadlanc-hora_atual.

    REFRESH: tg_editor.
*    SORT tl_imp_cad_imp_con BY bschl.
    IF obg_descbox IS NOT INITIAL.
      CALL METHOD obg_descbox->get_text_as_r3table
        IMPORTING
          table = tg_editor.

      LOOP AT tg_editor INTO wg_editor.
        IF sy-tabix EQ 1.
          tl_input_cadlanc-ref_imposto = wg_editor-line.

        ELSEIF sy-tabix GE 2.
          CONCATENATE tl_input_cadlanc-ref_imposto  wg_editor-line INTO tl_input_cadlanc-ref_imposto.

        ENDIF.
      ENDLOOP.
    ENDIF.
    SORT tg_itens_c BY cod_abertura.

    APPEND tl_input_cadlanc.
    CLEAR tl_input_cadlanc.

    vseqitem = 0.
    LOOP AT tg_itens_cl WHERE gsber EQ <wl_itens_clx>-gsber
                          AND cod_imposto EQ <wl_itens_clx>-cod_imposto
                          AND icon NE icon_message_error.
      MOVE-CORRESPONDING tg_itens_cl TO tl_input_cadlan.

      MOVE: sy-mandt               TO tl_input_cadlan-mandt,
            sy-uname               TO tl_input_cadlan-usuario,
            sy-datum               TO tl_input_cadlan-data_atual,
            sy-uzeit               TO tl_input_cadlan-hora_atual,
            vg_bukrs               TO tl_input_cadlan-bukrs.

      IF tl_input_cadlan-doc_imposto IS INITIAL.
        MOVE <wl_itens_clx>-doc_imposto TO tl_input_cadlan-doc_imposto.
      ENDIF.

      READ TABLE tg_itens_c INTO DATA(wa_itens_c) INDEX 1.

      ADD 1 TO vseqitem .
      MOVE:
            wa_itens_c-kostl             TO tl_input_cadlan-kostl,
            wa_itens_c-prctr             TO tl_input_cadlan-prctr,
            vseqitem                     TO tl_input_cadlan-seqitem,
            wa_itens_c-aufnr             TO tl_input_cadlan-aufnr,
            wa_itens_c-matnr             TO tl_input_cadlan-matnr,
            tg_itens_cl-bschl            TO tl_input_cadlan-bschl,
            tg_itens_cl-vlr_moeda_docd   TO tl_input_cadlan-valor_imp,
            tg_itens_cl-cod_aberturad    TO tl_input_cadlan-cod_abertura.

      READ TABLE tl_imp_cad_imp_con INTO DATA(wa_itens_x) WITH KEY cod_imposto = tl_input_cadlan-cod_imposto
                                                                  cod_abertura = tl_input_cadlan-cod_abertura BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE:
                    wa_itens_x-umskz             TO tl_input_cadlan-umskz,
                    wa_itens_x-hkont             TO tl_input_cadlan-hkont,
                    wa_itens_x-lifnr             TO tl_input_cadlan-lifnr,
                    wa_itens_x-kunnr             TO tl_input_cadlan-kunnr.

        IF tl_input_cadlan-hkont IS NOT INITIAL.
          tl_input_cadlan-kostl = tg_itens_cl-kostl.
        ENDIF.

      ENDIF.

      APPEND tl_input_cadlan.
      CLEAR tl_input_cadlan.
    ENDLOOP.

    LOOP AT tg_itens_cl WHERE gsber EQ <wl_itens_clx>-gsber
                          AND cod_imposto EQ <wl_itens_clx>-cod_imposto
                          AND icon NE icon_message_error.

      MOVE-CORRESPONDING tg_itens_cl TO tl_input_cadlan.

      MOVE: sy-mandt               TO tl_input_cadlan-mandt,
            sy-uname               TO tl_input_cadlan-usuario,
            sy-datum               TO tl_input_cadlan-data_atual,
            sy-uzeit               TO tl_input_cadlan-hora_atual,
            vg_bukrs               TO tl_input_cadlan-bukrs.

      IF tl_input_cadlan-doc_imposto IS INITIAL.
        MOVE: <wl_itens_clx>-doc_imposto TO tl_input_cadlan-doc_imposto.
      ENDIF.

      READ TABLE tg_itens_c INTO DATA(wa_itens_cx) INDEX 1.

      ADD 1 TO vseqitem .
      MOVE:
            wa_itens_cx-kostl            TO tl_input_cadlan-kostl,
            wa_itens_cx-prctr            TO tl_input_cadlan-prctr,
            vseqitem                     TO tl_input_cadlan-seqitem,
            wa_itens_cx-aufnr            TO tl_input_cadlan-aufnr,
            wa_itens_cx-matnr            TO tl_input_cadlan-matnr,
            tg_itens_cl-bschlc           TO tl_input_cadlan-bschl,
            tg_itens_cl-vlr_moeda_docc   TO tl_input_cadlan-valor_imp,
            tg_itens_cl-cod_aberturac    TO tl_input_cadlan-cod_abertura.


      READ TABLE tl_imp_cad_imp_con INTO DATA(wa_itens_x1) WITH KEY cod_imposto = tl_input_cadlan-cod_imposto
                                                                   cod_abertura = tl_input_cadlan-cod_abertura BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        MOVE:
                    wa_itens_x1-umskz             TO tl_input_cadlan-umskz,
                    wa_itens_x1-hkont             TO tl_input_cadlan-hkont,
                    wa_itens_x1-lifnr             TO tl_input_cadlan-lifnr,
                    wa_itens_x1-kunnr             TO tl_input_cadlan-kunnr.

        IF tl_input_cadlan-hkont IS NOT INITIAL.
          tl_input_cadlan-kostl = tg_itens_cl-kostl.
        ENDIF.

      ENDIF.



      APPEND tl_input_cadlan.
      CLEAR tl_input_cadlan.
    ENDLOOP.

    LOOP AT tg_itens_cl WHERE gsber EQ <wl_itens_clx>-gsber
                          AND cod_imposto EQ <wl_itens_clx>-cod_imposto
                          AND icon NE icon_message_error
                          AND bschldc IS NOT INITIAL.

      MOVE-CORRESPONDING tg_itens_cl TO tl_input_cadlan.

      MOVE: sy-mandt               TO tl_input_cadlan-mandt,
            sy-uname               TO tl_input_cadlan-usuario,
            sy-datum               TO tl_input_cadlan-data_atual,
            sy-uzeit               TO tl_input_cadlan-hora_atual,
            vg_bukrs               TO tl_input_cadlan-bukrs.

      IF tl_input_cadlan-doc_imposto IS INITIAL.
        MOVE: <wl_itens_clx>-doc_imposto TO tl_input_cadlan-doc_imposto.
      ENDIF.

      READ TABLE tg_itens_c INTO DATA(wa_itens_cxx) INDEX 1.

      ADD 1 TO vseqitem .

      MOVE:
            wa_itens_cxx-kostl            TO tl_input_cadlan-kostl,
            wa_itens_cxx-prctr            TO tl_input_cadlan-prctr,
            vseqitem                      TO tl_input_cadlan-seqitem,
            wa_itens_cxx-aufnr            TO tl_input_cadlan-aufnr,
            wa_itens_cxx-matnr            TO tl_input_cadlan-matnr,
            tg_itens_cl-bschldc           TO tl_input_cadlan-bschl,
            tg_itens_cl-vlr_moeda_docdc   TO tl_input_cadlan-valor_imp,
            tg_itens_cl-cod_aberturadc    TO tl_input_cadlan-cod_abertura.

      READ TABLE tl_imp_cad_imp_con INTO DATA(wa_itens_x2) WITH KEY cod_imposto = tl_input_cadlan-cod_imposto
                                                                   cod_abertura = tl_input_cadlan-cod_abertura BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        MOVE:
                    wa_itens_x2-umskz             TO tl_input_cadlan-umskz,
                    wa_itens_x2-hkont             TO tl_input_cadlan-hkont,
                    wa_itens_x2-lifnr             TO tl_input_cadlan-lifnr,
                    wa_itens_x2-kunnr             TO tl_input_cadlan-kunnr.

        IF tl_input_cadlan-hkont IS NOT INITIAL.
          tl_input_cadlan-kostl = tg_itens_cl-kostl.
        ENDIF.

      ENDIF.

      APPEND tl_input_cadlan.
      CLEAR tl_input_cadlan.
    ENDLOOP.

    SORT tg_itens_cl BY gsber.
    READ TABLE tg_itens_cl INTO DATA(wa_item) WITH KEY gsber = <wl_itens_clx>-gsber
                           BINARY SEARCH.
    IF sy-subrc IS INITIAL.
* marcar verd.
      MOVE-CORRESPONDING <wl_itens_clx> TO wa_item.
      MOVE icon_checked TO wa_item-icon.
      MODIFY tg_itens_cl FROM wa_item INDEX sy-tabix.
    ENDIF.
  ENDLOOP.

  IF tl_input_cadlan[] IS NOT INITIAL AND tl_input_cadlanc[] IS NOT INITIAL.
    MODIFY zimp_lanc_impost FROM TABLE tl_input_cadlanc.
    MODIFY zimp_lanc_imp_ct FROM TABLE tl_input_cadlan.
  ELSE.
    MESSAGE 'Erro no momento da gravação, verificar registros!'(ee7) TYPE 'I'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  OBTEM_PROX_CL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM obtem_prox_cl .
  DATA: vnum(10)            TYPE c,
        vseq(10)            TYPE p,
        wl_zimp_lanc_impost TYPE zimp_lanc_impost.

  IF vg_bukrs IS NOT INITIAL.
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = 'ZT'
        object      = 'RF_BELEG'
        subobject   = vg_bukrs "wg_cadlan-bukrs
      IMPORTING
        number      = vseq.      "SUBOBJECT = W_ZIMP_CABECALHO-BUK

    vnum = vseq .
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = vnum
      IMPORTING
        output = vnum.

    wg_cadlan_cl-doc_imposto = vnum.
    SELECT SINGLE *
      FROM zimp_lanc_impost
      INTO wl_zimp_lanc_impost
      WHERE bukrs       = vg_bukrs
      AND   doc_imposto =  wg_cadlan_cl-doc_imposto.

    IF sy-subrc = 0.
      CLEAR wg_cadlan_cl-doc_imposto.
*      MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'ERRO FATAL NA NUMERAÇÃO, DOCUMENTO JÁ EXISTE.'.
* Colocar nas mensagerias - pendente
    ENDIF.

  ELSE.
    CLEAR wg_cadlan_cl-doc_imposto.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  TB_STRIP_IMP_ACT_GET_CL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE tb_strip_imp_act_get_cl INPUT.

  ok_code = sy-ucomm.
  CASE ok_code.
    WHEN c_tab_strip_imp_cl-tab1.
      g_tab_strip_imp-pressed_tab = c_tab_strip_imp_cl-tab1.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.

ENDMODULE.

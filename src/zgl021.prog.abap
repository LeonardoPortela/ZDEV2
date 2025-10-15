*&--------------------------------------------------------------------&*
*&                        ROLLOUT - Consultoria                       &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Igor Vilela                                             &*
*& Data.....:  /07/                                                   &*
*& Descrição: Realização de reconciliação contábeis                   &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*& ABAP            DEVK917576   20.07.2010                            &*
*&--------------------------------------------------------------------&*

REPORT  zgl021 MESSAGE-ID zctb.
TYPE-POOLS: ustyp, slis, kkblo.
INCLUDE <icon>.

TABLES: zglt042, zglt041, zglt043.
*&--------------------------------------------------------------------&*
*& Estruturas                                                         &*
*&--------------------------------------------------------------------&*
TYPES: BEGIN OF ty_header,
         seq           TYPE num10,
         bukrs         TYPE zglt043-bukrs,
         desc_bukrs    TYPE t001-butxt,
         land1         TYPE t001-land1,
         dep_resp      TYPE zglt042-dep_resp,
         desc_dep_resp TYPE zimp_cad_depto-dep_resp_desc,
         bname         TYPE zglt041-bname2,
         desc_bname    TYPE usrefus-useralias,
         monat         TYPE zglt043-monat,
         gjahr         TYPE zglt043-gjahr,
         saknr         TYPE zglt043-saknr,
         desc_saknr    TYPE skat-txt50,
         codigo        TYPE zglt039-codigo,
         descr         TYPE zglt039-descr,
         cod_nota      TYPE zglt039-cod_nota,
         descr_nota    TYPE zglt039-descr_nota,
         fonte_infor   TYPE zglt043-fonte_infor,
         mitkz         TYPE skb1-mitkz,
         "Moedas
         waers         TYPE t001-waers,
         waer2         TYPE t001-waers,
         waer3         TYPE t001-waers,
         depara        TYPE c LENGTH 10,
         depara2       TYPE c LENGTH 10,
         cta_monet     LIKE zglt041-cta_monet,
       END OF ty_header,

       BEGIN OF ty_totais,
         saldo_mi      TYPE faglflext-hslvt,
         saldo_mi2     TYPE faglflext-kslvt,
         saldo_mi3     TYPE faglflext-oslvt,
         tx_hist1      TYPE zukurs_curr_04,
         tx_hist2      TYPE zukurs_curr_04,
         tx_fech1      TYPE zukurs_curr_04,
         tx_fech2      TYPE zukurs_curr_04,
*Inicio alteração - fmartins -    IR104343 - 31/01/2023
         tx_paridade1  TYPE P DECIMALS 4 LENGTH 11,
         tx_paridade2  TYPE zukurs_curr_04,
*         tx_paridade1  TYPE faglflext-hslvt,
*         tx_paridade2  TYPE faglflext-hslvt,
*Fim alteração - fmartins -    IR104343 - 31/01/2023
         saldo_aux_mi  TYPE faglflext-hslvt,
         saldo_aux_mi2 TYPE faglflext-kslvt,
         saldo_aux_mi3 TYPE faglflext-oslvt,
         dif_mi1       TYPE faglflext-hslvt,
         dif_mi2       TYPE faglflext-kslvt,
         dif_mi3       TYPE faglflext-oslvt,
       END OF ty_totais,

       BEGIN OF ty_ska1,
         ktopl TYPE ska1-ktopl,
         saknr TYPE ska1-saknr,
         ktoks TYPE ska1-ktoks,
       END OF ty_ska1,

       BEGIN OF ty_skat,
         saknr TYPE skat-saknr,
         txt50 TYPE skat-txt50,
       END OF ty_skat,

       BEGIN OF ty_usrefus,
         bname     TYPE usrefus-bname,
         useralias TYPE usrefus-useralias,
       END OF ty_usrefus,

       BEGIN OF ty_skb1,
         saknr TYPE skb1-saknr,
         mitkz TYPE skb1-mitkz,
         xspeb TYPE skb1-xspeb,
       END OF ty_skb1,

       BEGIN OF ty_039,
         codigo     TYPE zglt039-codigo,
         descr      TYPE zglt039-descr,
         cod_nota   TYPE zglt039-cod_nota,
         descr_nota TYPE zglt039-descr_nota,
       END OF ty_039,

       BEGIN OF ty_040,
         codigo TYPE zglt040-codigo,
         descr  TYPE zglt040-descr,
       END OF ty_040,

       BEGIN OF ty_depto,
         dep_resp      TYPE zimp_cad_depto-dep_resp,
         dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
       END OF ty_depto,

       BEGIN OF ty_041,
         bukrs            TYPE zglt041-bukrs,
         saknr            TYPE zglt041-saknr,
         cod_clas_bal     TYPE zglt041-cod_clas_bal,
         cod_clas_not     TYPE zglt041-cod_clas_not2,
         cta_monet        TYPE zglt041-cta_monet,
         cta_intercompany TYPE zglt041-cta_intercompany,
         dep_resp         TYPE zglt041-dep_resp2,
         bname            TYPE zglt041-bname2,
         prazo_entr	      TYPE zglt041-prazo_entr,
         crit_vecto	      TYPE zglt041-crit_vecto,
       END OF ty_041,

       BEGIN OF ty_bsik_bsid,
         bukrs       TYPE bsik-bukrs,
         hkont       TYPE bsik-hkont,
         monat       TYPE bsik-monat,
         gjahr       TYPE bsik-gjahr,
         budat       TYPE bsik-budat,
         bldat       TYPE bsik-bldat,
         lifnr_kunnr TYPE bsik-lifnr,
         dmbtr       TYPE bsik-bdif2, "BSIK-DMBTR,
         dmbe2       TYPE bsik-bdif2, "BSIK-DMBE2,
         dmbe3       TYPE bsik-bdif3, "BSIK-DMBE3,
         zfbdt       TYPE bsik-zfbdt,
         zbd1t       TYPE bsik-zbd1t,
         belnr       TYPE bsik-belnr,
         shkzg       TYPE bsik-shkzg,
       END OF ty_bsik_bsid,

       BEGIN OF ty_lfa1,
         lifnr TYPE lfa1-lifnr,
         name1 TYPE lfa1-name1,
       END OF ty_lfa1,

       BEGIN OF ty_kna1,
         kunnr TYPE kna1-kunnr,
         name1 TYPE kna1-name1,
       END OF ty_kna1,

       BEGIN OF ty_saida,
         mark,
         status_lib(4), "type zglt043-STATUS_LIB,
         statusdesc    TYPE skat-txt50,
         bukrs         TYPE zglt041-bukrs,
         butxt         TYPE t001-butxt,
         land1         TYPE t001-land1,
         waers         TYPE t001-waers,
         waer2         TYPE t001-waers,
         waer3         TYPE t001-waers,
         saknr         TYPE skat-saknr,
         txt50         TYPE skat-txt50,
         mitkz         TYPE skb1-mitkz,
         saldo_mi      TYPE faglflext-hslvt,
         saldo_mi2     TYPE faglflext-kslvt,
         saldo_mi3     TYPE faglflext-oslvt,
         c_balanco(70),
         codigo        TYPE zglt039-codigo,
         descr         TYPE zglt039-descr,
         c_nota(70),
         cod_nota      TYPE zglt039-cod_nota,
         descr_nota    TYPE zglt039-descr_nota,
         cta_monet(7),  "  TYPE ZGLT041-CTA_MONET,
         cta_inter(7),  " TYPE ZGLT041-CTA_INTERCOMPANY,
         dep_resp(70),
         cod_resp      TYPE zglt041-dep_resp2,
         dep_resp_desc TYPE zimp_cad_depto-dep_resp_desc,
         bname         TYPE zglt041-bname2,
         desc_bname    TYPE usrefus-useralias,
         prazo_entr    TYPE zglt041-prazo_entr,
         usuario_recon TYPE xubname,
         criterio(4),
         nivel_aprova  TYPE char3,          "Modificação 08.11.2016
         usuario_respo TYPE c LENGTH 200,   "Modificação 08.11.2016
       END OF ty_saida,

       BEGIN OF ty_aux,
         bukrs    LIKE skb1-bukrs,
         saknr    LIKE skb1-saknr,
         txt50    LIKE skat-txt50,
         mitkz    LIKE skb1-mitkz,
         tdformat TYPE tline-tdformat,
         tdline   TYPE tline-tdline,
       END OF ty_aux,

       BEGIN OF ty_rel_aux,
         data            TYPE sy-datum,
         codigo          TYPE kna1-kunnr,
         name1           TYPE kna1-name1,
         histo(70),
         valor           TYPE faglflext-hslvt,
         valor2          TYPE faglflext-hslvt,
         valor3          TYPE faglflext-hslvt,
         tipo(1),
         contra(10),
         subtotalkey(16),
         sortkey(1),
         count           TYPE i,
         line_color(4)   TYPE c,     "Used to store row color attributes
       END OF ty_rel_aux,

       BEGIN OF ty_reg_ped,
         bldat      TYPE bsik-bldat,
         belnr      TYPE bsik-belnr,
         histo(70),
         codigo     TYPE kna1-kunnr,
         name1      TYPE kna1-name1,
         dmbtr      TYPE bsik-dmbtr,
         dmbe2      TYPE bsik-dmbe2,
         dmbe3      TYPE bsik-dmbe3,
         tipo(1),
         dt_venc    TYPE sy-datum,
         dt_ajus    TYPE sy-datum,
         doc_ajus   TYPE bsik-belnr,
         obs        TYPE zfied029,
         contra(10),
         augdt      type bsak-augdt, "RJF
         auart      TYPE vbak-auart, "------------*11.01.2017 LG
         ebeln      TYPE bsik-ebeln, "------------*11.01.2017 LG
         vbel2      TYPE bsid-vbel2, "------------*11.01.2017 LG
         augbl      TYPE bsak-augbl, "------------*11.01.2017 LG
         buzei      TYPE bsak-buzei, "------------*01.03.2017 LG
       END OF ty_reg_ped.

TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.
*Class definition for ALV toolbar
CLASS:      lcl_alv_toolbar      DEFINITION DEFERRED.
"CLASS:      LCL_ALV_TOOLBAR2     DEFINITION DEFERRED.
CLASS:      lcl_event_receiver   DEFINITION DEFERRED.
*&--------------------------------------------------------------------&*
*& Constantes                                                         &*
*&--------------------------------------------------------------------&*
CONSTANTS: c_x              TYPE c VALUE 'X',
           c_p              TYPE c VALUE 'P',
           c_l              TYPE c VALUE 'L',
           c_a              TYPE c VALUE 'A',
           c_r              TYPE c VALUE 'R',
           c_add(3)         TYPE c VALUE 'ADD',
           c_del(3)         TYPE c VALUE 'DEL',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_0051(4)        TYPE c VALUE '0051',
           c_0052(4)        TYPE c VALUE '0052',
           c_back(4)        TYPE c VALUE 'BACK',
           c_copy(4)        TYPE c VALUE 'COPY',
           c_save(4)        TYPE c VALUE 'SAVE',
           c_atual(5)       TYPE c VALUE 'ATUAL',
           c_modif(5)       TYPE c VALUE 'MODIF',
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_refresh(7)     TYPE c VALUE 'REFRESH',
           c_new_doc(7)     TYPE c VALUE 'NEW_DOC',
           c_motivo_neg(10) TYPE c VALUE 'MOTIVO_NEG',
           c_col_exp(7)     TYPE c VALUE 'COL_EXP',
           c_chg_doc(7)     TYPE c VALUE 'CHG_DOC',
           c_view_doc(8)    TYPE c VALUE 'VIEW_DOC',
           c_print_doc(9)   TYPE c VALUE 'PRINT_DOC',
           c_show_msgre(10) TYPE c VALUE 'SHOW_MSGRE',
           c_auxiliar(08)   TYPE c VALUE 'AUXILIAR',
           objtype          TYPE borident-objtype VALUE 'ZGL026'.

*&--------------------------------------------------------------------&*
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
DATA: g_container          TYPE scrfname VALUE 'CC_ITENS_NOTA',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      container1           TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container2           TYPE REF TO cl_gui_container,       "splitter conteiner 2
      container3           TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      grid3                TYPE REF TO cl_gui_alv_grid,
      dg_events_receiver   TYPE REF TO lcl_event_receiver,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      "OBG_TOOLBAR2          TYPE REF TO LCL_ALV_TOOLBAR2,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      manager              TYPE REF TO cl_gos_manager,
      editor               TYPE REF TO cl_gui_textedit,
      container            TYPE REF TO cl_gui_custom_container,
      obj                  TYPE borident,
      it_hints             TYPE TABLE OF alv_s_qinf,
      gl_tipo_visualiza(1).

*Declaration for toolbar buttons
DATA : ty_toolbar TYPE stb_button.

*----------------------------------------------------------------------*
* ESTRUTURAS ALV
*----------------------------------------------------------------------*
DATA: xs_events    TYPE slis_alv_event,
      events       TYPE slis_t_event,
      t_print      TYPE slis_print_alv,
      estrutura    TYPE TABLE OF ty_estrutura,
      wa_estrutura TYPE ty_estrutura,
      v_report     LIKE sy-repid,
      t_top        TYPE slis_t_listheader,
      lt_sort      TYPE slis_t_sortinfo_alv,
      ls_sort      TYPE slis_sortinfo_alv,
      init.
*&--------------------------------------------------------------------&*
*& Declaração de Variaveis/Tabelas/Workarea                           &*
*&--------------------------------------------------------------------&*
DATA: wg_header           TYPE ty_header,
      wg_header_log       TYPE zglt075,
      wg_totais           TYPE ty_totais,
      wg_tcurr_1          TYPE tcurr,
      wg_tcurr_2          TYPE tcurr,
      wg_tcurr_1_fec      TYPE tcurr,
      wg_tcurr_2_fec      TYPE tcurr,
      wg_ska1             TYPE ty_ska1,
      tg_ska1             TYPE TABLE OF ty_ska1,
      wg_skat             TYPE ty_skat,
      tg_skat             TYPE TABLE OF ty_skat,
      wg_skb1             TYPE ty_skb1,
      tg_skb1             TYPE TABLE OF ty_skb1,
      wg_t001             TYPE t001,
      wg_039              TYPE ty_039,
      tg_039              TYPE TABLE OF ty_039,
      wg_040              TYPE ty_040,
      tg_040              TYPE TABLE OF ty_040,
      wg_usrefus          TYPE ty_usrefus,
      tg_usrefus          TYPE TABLE OF ty_usrefus,
      wg_depto            TYPE ty_depto,
      tg_depto            TYPE TABLE OF ty_depto,
      wg_041              TYPE ty_041,
      tg_041              TYPE TABLE OF ty_041,
      tg_042              TYPE TABLE OF zglt042,
      wg_042              TYPE zglt042,
      wg_flext            TYPE faglflext,
      tg_flext            TYPE TABLE OF faglflext,
      it_bsxk             TYPE TABLE OF bsik WITH HEADER LINE,
      it_bsxd             TYPE TABLE OF bsid WITH HEADER LINE,
      it_contas           TYPE zct_emp_contas,
      wa_contas           TYPE zlc_emp_contas,
      it_saldo_contas     TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
      it_saldo_contas_2   TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
      it_saldo_contas_3   TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
      it_saldo_parceiro   TYPE TABLE OF zde_fi_gl_saldo_faglflext WITH HEADER LINE,
      wg_lfa1             TYPE ty_lfa1,
      tg_lfa1             TYPE TABLE OF ty_lfa1,
      wg_kna1             TYPE ty_kna1,
      tg_kna1             TYPE TABLE OF ty_kna1,
      wg_043              TYPE zglt043,
      tg_043              TYPE TABLE OF zglt043,
      wg_044              TYPE zglt044,
      tg_044              TYPE TABLE OF zglt044,
      wg_045              TYPE zglt045,
      tg_045              TYPE TABLE OF zglt045,
      wg_saida            TYPE ty_saida,
      wa_zglt043          TYPE zglt043,
      tg_saida            TYPE TABLE OF ty_saida,
      wg_rel_aux          TYPE ty_rel_aux,
      tg_rel_aux          TYPE TABLE OF ty_rel_aux,
      wg_reg_ped          TYPE ty_reg_ped,
      tg_reg_ped          TYPE TABLE OF ty_reg_ped,
      tg_texto            TYPE TABLE OF tline WITH HEADER LINE,
      tg_aux              TYPE TABLE OF ty_aux,
      gt_zglt055_log      TYPE TABLE OF zglt055,
      gt_zglt066_log      TYPE TABLE OF zglt066,
      gt_srgbtbrel        TYPE TABLE OF srgbtbrel,
      wa_aux              TYPE ty_aux,
      vg_last_day         TYPE sy-datum,
      vg_first_day        TYPE sy-datum,
      ls_source           TYPE sibflporb,
      ls_target           TYPE sibflporb,
      gt_services         TYPE tgos_sels,
      ls_service          TYPE sgos_sels,
      ls_ident            TYPE borident,
      vg_last_day_aux(8),
      vg_first_day_aux(8),
      wg_dif_conf         TYPE faglflext-hslvt,
      wg_dif_conf2        TYPE faglflext-hslvt,
      wg_dif_conf3        TYPE faglflext-hslvt,
      wa_moedas_empresa   TYPE x001,
      wa_bsxk_aux         TYPE bsik,
      wa_bsxd_aux         TYPE bsid.

DATA: tg_zglt058 TYPE TABLE OF zglt058 WITH HEADER LINE, "Modificação 08.11.2016
      tg_zglt059 TYPE TABLE OF zglt059 WITH HEADER LINE, "Modificação 08.11.2016
      tg_zglt062 TYPE TABLE OF zglt062 WITH HEADER LINE, "Modificação 08.11.2016
      tg_zglt063 TYPE TABLE OF zglt063 WITH HEADER LINE. "Modificação 08.11.2016

*** Declaracoes referente ao template do programa ***
DATA: ok-code         TYPE sy-ucomm,
      wg_display,
      wg_acao(10),
      wg_flag,
      tg_selectedcell TYPE lvc_t_cell,
      wg_selectedcell TYPE lvc_s_cell,
      wg_colaps(4)    VALUE '@K2@',
      wg_sub01        TYPE sy-dynnr,
      wg_exit,
      lb_hist         TYPE char9,
      lb_hist2        TYPE char9.

DATA: it_zglt059_rec TYPE TABLE OF zglt059 WITH HEADER LINE,
      wa_zglt059_rec TYPE zglt059.

***** Funcao de Z_DOC_CHECK_NEW
DATA: x_field(30),
      wg_mensagem(30).
DATA: tg_msg_ret TYPE TABLE OF zfiwrs0002 WITH HEADER LINE,
      wg_cell    TYPE lvc_s_cell,
      tg_cell    TYPE lvc_t_cell.

** Criação de tabela dinamica
DATA: t_fieldcatalog TYPE lvc_t_fcat,
      w_fieldcatalog TYPE lvc_s_fcat,
      wa_layout      TYPE lvc_s_layo,
      wa_stable      TYPE lvc_s_stbl.

DATA: ref1 TYPE REF TO cl_gui_alv_grid.
DEFINE enter.

  CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
    EXPORTING
      functioncode           = '/00'
    EXCEPTIONS
      function_not_supported = 1.

END-OF-DEFINITION.
*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS: handle_on_button_click FOR EVENT button_click OF cl_gui_alv_grid
      IMPORTING es_col_id
                es_row_no.

    CLASS-METHODS: on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.

    CLASS-METHODS: on_data_changed2 FOR EVENT data_changed OF cl_gui_alv_grid
      IMPORTING er_data_changed.

    CLASS-METHODS: subtotal_text FOR EVENT subtotal_text OF cl_gui_alv_grid
      IMPORTING es_subtottxt_info ep_subtot_line e_event_data.

    CLASS-METHODS: on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
      IMPORTING e_modified et_good_cells.

    CLASS-METHODS: handle_on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id e_column_id es_row_no.

  PRIVATE SECTION.
ENDCLASS.                    "lcl_event_receiver DEFINITION
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

      on_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_user_command_reg FOR EVENT user_command OF cl_gui_alv_grid
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
    IF wg_header-mitkz NE 'D' AND wg_header-mitkz NE 'K'.
      ty_toolbar-icon      =  icon_insert_row.
      ty_toolbar-function  =  c_add.
      IF wg_acao EQ c_new_doc
      OR wg_acao EQ c_chg_doc.
        ty_toolbar-disabled  = space.
      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_delete_row.
      ty_toolbar-function  =  c_del.
      IF wg_acao EQ c_new_doc
      OR wg_acao EQ c_chg_doc.
        ty_toolbar-disabled  = space.
      ELSE.
        ty_toolbar-disabled  = 1.
      ENDIF.
      ty_toolbar-butn_type = 0.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      CALL METHOD c_alv_toolbarmanager->reorganize
        EXPORTING
          io_alv_toolbar = e_object.
    ENDIF.
  ENDMETHOD.                    "ON_TOOLBAR

  METHOD handle_user_command.
    DATA : wl_rel_aux TYPE ty_rel_aux.
    CASE e_ucomm.
      WHEN c_add.
        wl_rel_aux-sortkey = 'A'.
        APPEND wl_rel_aux TO tg_rel_aux.
      WHEN c_del.
        CALL METHOD grid1->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.
        LOOP AT tg_selectedcell INTO wg_selectedcell.
          READ TABLE tg_rel_aux INTO wl_rel_aux INDEX  wg_selectedcell-row_id-index.

          IF wl_rel_aux-sortkey EQ 'C'.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-004 TEXT-005. "'A linha selecionada nao pode ser' 'eliminada.'.
          ELSE.
            DELETE tg_rel_aux INDEX wg_selectedcell-row_id-index.
          ENDIF.
        ENDLOOP.

    ENDCASE.

    wa_stable-row = c_x.
    wa_stable-col = c_x.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "handle_user_command
  METHOD handle_user_command_reg.
    DATA : wl_reg_ped TYPE ty_reg_ped.
    CASE e_ucomm.
      WHEN c_add.
*        WL_REL_AUX-SORTKEY = 'A'.
        APPEND wl_reg_ped TO tg_reg_ped.
      WHEN c_del.
        CALL METHOD grid2->get_selected_cells
          IMPORTING
            et_cell = tg_selectedcell.
        LOOP AT tg_selectedcell INTO wg_selectedcell.
          DELETE tg_reg_ped INDEX wg_selectedcell-row_id-index.
        ENDLOOP.

    ENDCASE.

    wa_stable-row = c_x.
    wa_stable-col = c_x.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.                    "handle_user_command
ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS LCL_EVENT_RECEIVER IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_on_button_click.

    DATA: tl_texto TYPE catsxt_longtext_itab,
          wl_texto TYPE LINE OF catsxt_longtext_itab,
          wl_field TYPE lvc_s_col,
          v_cont   TYPE i.

    DATA: ls_sel_hide TYPE slis_sel_hide_alv,
          is_table    TYPE lvc_s_stbl.

    REFRESH tl_texto.
    CLEAR:wl_texto.

    READ TABLE tg_saida INTO wg_saida INDEX es_row_no-row_id.
    LOOP AT tg_aux INTO wa_aux WHERE bukrs EQ wg_saida-bukrs
                                 AND saknr EQ wg_saida-saknr
                                 AND txt50 EQ wg_saida-txt50
                                 AND mitkz EQ wg_saida-mitkz.

      MOVE: wa_aux-tdline   TO wl_texto.

      APPEND wl_texto TO tl_texto.
      CLEAR: wl_texto.
    ENDLOOP.

    CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
      EXPORTING
        im_title        = TEXT-006 "'Critérios Reconciliação'  " Título
        im_display_mode = 'X'              "" Definição do Display
      CHANGING
        ch_text         = tl_texto.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = ls_sel_hide
        e_grid      = ref1.

    CALL METHOD ref1->refresh_table_display
      EXPORTING
        is_stable = is_table.

    CALL METHOD cl_gui_cfw=>dispatch.
    CALL METHOD cl_gui_cfw=>flush.

  ENDMETHOD.                    "HANDLE_ON_BUTTON_CLICK.

  METHOD on_data_changed.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
*Inicio alteração - fmartins -    IR104343 - 28/12/2022
*          wl_valor TYPE zglt045-dmbtr.
          wl_valor TYPE zed_dmbtr.
*Fim alteração - fmartins -    IR104343 - 28/12/2022


    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'VALOR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wl_valor = lv_value.
      IF wl_valor LT 0.
        lv_value = 'C'.
      ELSE.
        lv_value = 'D'.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TIPO'
          i_value     = lv_value.

    ENDLOOP.
  ENDMETHOD.                    "on_data_chaged
  METHOD on_data_changed2.
    DATA: ls_good  TYPE lvc_s_modi,
          lv_value TYPE lvc_value,
          vl_value TYPE lvc_value,
*Inicio alteração - fmartins -    IR104343 - 28/12/2022
*          wl_valor TYPE zglt045-dmbtr.
          wl_valor TYPE zed_dmbtr.
*Fim alteração - fmartins -    IR104343 - 28/12/2022
    LOOP AT er_data_changed->mt_good_cells
                             INTO ls_good
                             WHERE fieldname = 'DMBTR'.
      lv_value = ls_good-value.
      CONDENSE lv_value NO-GAPS.
      wl_valor = lv_value.
      IF wl_valor LT 0.
        lv_value = 'C'.
      ELSE.
        lv_value = 'D'.
      ENDIF.
      CALL METHOD er_data_changed->modify_cell
        EXPORTING
          i_row_id    = ls_good-row_id
          i_fieldname = 'TIPO'
          i_value     = lv_value.

    ENDLOOP.
  ENDMETHOD.                    "on_data_chaged2
  METHOD subtotal_text.
    DATA: wl_rel_aux TYPE ty_rel_aux,
          wl_clear.

    CLEAR: wl_rel_aux, wl_clear.
    FIELD-SYMBOLS: <fs> TYPE any.
    FIELD-SYMBOLS: <fs2> TYPE any.
    ASSIGN e_event_data->m_data->* TO <fs>.
    IF sy-subrc EQ 0.

      IF es_subtottxt_info EQ 'SORTKEY'.
        ASSIGN ep_subtot_line->* TO <fs2>.
        IF <fs2> IS ASSIGNED.
          wl_rel_aux = <fs2>.

          IF wl_rel_aux-sortkey EQ 'A'.
            <fs> = TEXT-007. "'Saldo Final Relatório Auxiliar'.
          ELSEIF wl_rel_aux-sortkey EQ 'C'.
            <fs> = TEXT-008. "'Saldo Contábil Final'.
          ENDIF.
        ENDIF.
      ELSEIF es_subtottxt_info EQ 'SUBTOTALKEY '.
        <fs> = TEXT-009. "'Diferença Conciliação'.
        wl_clear = c_x.
      ENDIF.
    ENDIF.
    ASSIGN ep_subtot_line->* TO <fs>.
    IF sy-subrc EQ 0.
      wl_rel_aux = <fs>.
*Inicio de alteração - Fmartins - 22/11/2022 -  RMNI - IR104343
**
*     if wg_header-bukrs = '0101' and sy-ucomm = 'NEW_DOC'.
*       wl_rel_aux-valor = wl_rel_aux-valor * 100.
*      endif.
*Fim de alteração - Fmartins - 22/11/2022 -  RMNI - IR104343
      IF wl_clear IS INITIAL.
        IF wl_rel_aux-valor LT 0.
          wl_rel_aux-tipo = 'C'.
        ELSE.
          wl_rel_aux-tipo = 'D'.
        ENDIF.

        "1ª Moeda
        IF wg_dif_conf IS INITIAL.
          IF wl_rel_aux-sortkey EQ 'A'.
            wg_dif_conf = ( wl_rel_aux-valor ) * -1.
          ELSE.
            wg_dif_conf = wl_rel_aux-valor.
          ENDIF.
        ELSE.
          IF wg_header-mitkz EQ 'K'
          OR wg_header-mitkz EQ 'D'.
            IF wl_rel_aux-sortkey EQ 'A'.
              wg_dif_conf  = wg_dif_conf + ( wl_rel_aux-valor * -1 ).
            ELSE.
              wg_dif_conf  = wg_dif_conf + wl_rel_aux-valor.
            ENDIF.
          ELSE.
            IF wl_rel_aux-sortkey EQ 'A'.
              wg_dif_conf  = wg_dif_conf + ( wl_rel_aux-valor  * -1 ).
            ELSE.
              wg_dif_conf  = wg_dif_conf + wl_rel_aux-valor.
            ENDIF.
          ENDIF.
        ENDIF.

        "2ª Moeda
        IF wg_dif_conf2 IS INITIAL.
          IF wl_rel_aux-sortkey EQ 'A'.
            wg_dif_conf2 = ( wl_rel_aux-valor2 ) * -1.
          ELSE.
            wg_dif_conf2 =  wl_rel_aux-valor2.
          ENDIF.
        ELSE.
          IF wg_header-mitkz EQ 'K'
          OR wg_header-mitkz EQ 'D'.
            IF wl_rel_aux-sortkey EQ 'A'.
              wg_dif_conf2 = wg_dif_conf2 + ( wl_rel_aux-valor2 * -1 ).
            ELSE.
              wg_dif_conf2 = wg_dif_conf2 + wl_rel_aux-valor2.
            ENDIF.
          ELSE.
            IF wl_rel_aux-sortkey EQ 'A'.
              wg_dif_conf2 = wg_dif_conf2 + ( wl_rel_aux-valor2 * -1 ).
            ELSE.
              wg_dif_conf2 = wg_dif_conf2 + wl_rel_aux-valor2.
            ENDIF.
          ENDIF.
        ENDIF.

        "3ª Moeda
        IF wg_dif_conf3 IS INITIAL.
          IF wl_rel_aux-sortkey EQ 'A'.
            wg_dif_conf3 = ( wl_rel_aux-valor3 ) * -1.
          ELSE.
            wg_dif_conf3 =  wl_rel_aux-valor3.
          ENDIF.
        ELSE.
          IF wg_header-mitkz EQ 'K'
          OR wg_header-mitkz EQ 'D'.
            IF wl_rel_aux-sortkey EQ 'A'.
              wg_dif_conf3 = wg_dif_conf3 + ( wl_rel_aux-valor3 * -1 ).
            ELSE.
              wg_dif_conf3 = wg_dif_conf3 + wl_rel_aux-valor3.
            ENDIF.
          ELSE.
            IF wl_rel_aux-sortkey EQ 'A'.
              wg_dif_conf3 = wg_dif_conf3 + ( wl_rel_aux-valor3 * -1 ).
            ELSE.
              wg_dif_conf3 = wg_dif_conf3 + wl_rel_aux-valor3.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        wl_rel_aux-valor  = wg_dif_conf.
        wl_rel_aux-valor2 = wg_dif_conf2.
        wl_rel_aux-valor3 = wg_dif_conf3.
      ENDIF.

      <fs> = wl_rel_aux.
    ENDIF.

    IF wl_clear IS NOT INITIAL.
      CLEAR: wg_dif_conf, wg_dif_conf2, wg_dif_conf3.
    ENDIF.

  ENDMETHOD.                    "subtotal_text
  METHOD on_data_changed_finished.

    DATA: lc_value TYPE p DECIMALS 4.

    CLEAR: wg_totais-saldo_aux_mi,
           wg_totais-saldo_aux_mi2,
           wg_totais-saldo_aux_mi3.

    LOOP AT tg_rel_aux INTO wg_rel_aux WHERE sortkey NE 'C'.
      ADD wg_rel_aux-valor  TO wg_totais-saldo_aux_mi.
      ADD wg_rel_aux-valor2 TO wg_totais-saldo_aux_mi2.
      ADD wg_rel_aux-valor3 TO wg_totais-saldo_aux_mi3.
    ENDLOOP.

    wg_totais-dif_mi1 = wg_totais-saldo_mi  - wg_totais-saldo_aux_mi.
    wg_totais-dif_mi2 = wg_totais-saldo_mi2 - wg_totais-saldo_aux_mi2.
    wg_totais-dif_mi3 = wg_totais-saldo_mi3 - wg_totais-saldo_aux_mi3.

    TRY.
        lc_value = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
        IF lc_value GT 99999.
          wg_totais-tx_paridade1 = 99999.
        ELSE.
          wg_totais-tx_paridade1 = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
        ENDIF.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    TRY.
        lc_value = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi3.
        IF lc_value GT 99999.
          wg_totais-tx_paridade2 = 99999.
        ELSEIF lc_value LT -99999.                                "Modificação 22.02.2017
          wg_totais-tx_paridade2 = -99999.
        ELSE.
          wg_totais-tx_paridade2 = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi3.
        ENDIF.
      CATCH cx_sy_zerodivide.
    ENDTRY.

    wa_stable-row = c_x.
    wa_stable-col = c_x.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

    IF e_modified IS NOT INITIAL.
      enter.
    ENDIF.
  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD handle_on_hotspot_click.
    DATA : wl_reg_ped TYPE ty_reg_ped.
    READ TABLE tg_reg_ped INTO wl_reg_ped INDEX es_row_no-row_id.

    IF wl_reg_ped-dt_ajus  IS NOT INITIAL AND
       wl_reg_ped-doc_ajus IS NOT INITIAL.
      SET PARAMETER ID: 'BLN' FIELD wl_reg_ped-doc_ajus,
                        'BUK' FIELD wg_header-bukrs,
                        'GJR' FIELD wl_reg_ped-dt_ajus(4).
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.

  ENDMETHOD.                    "HANDLE_ON_HOTSPOT_CLICK

ENDCLASS.                    "LCL_EVENT_RECEIVER IMPLEMENTATION

*----------------------------------------------------------------------*
* TELA DE SELECAO.
*----------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs  FOR zglt042-empresa_de NO-EXTENSION NO INTERVALS OBLIGATORY,
                  s_depre  FOR zglt042-dep_resp   NO-EXTENSION NO INTERVALS OBLIGATORY,
                  s_mes    FOR zglt042-mes_de     NO-EXTENSION NO INTERVALS OBLIGATORY,
                  s_ano    FOR zglt042-ano_de     NO-EXTENSION NO INTERVALS OBLIGATORY,
                  s_contas FOR zglt041-saknr      NO-DISPLAY,
                  s_seq    FOR zglt043-seq        NO-DISPLAY.
SELECTION-SCREEN: END OF BLOCK b1.

*AT SELECTION-SCREEN.                                        "Modificação 08.11.2016
*  AUTHORITY-CHECK OBJECT 'ZFI_BUKRS'                        "Modificação 08.11.2016
*    ID 'BUKRS' FIELD S_BUKRS-LOW.                           "Modificação 08.11.2016
*  IF SY-SUBRC <> 0.                                         "Modificação 08.11.2016
*    SET CURSOR FIELD 'BUKRS-LOW'.                           "Modificação 08.11.2016
*    MESSAGE E091(8B) WITH S_BUKRS-LOW.                      "Modificação 08.11.2016
*  ENDIF.                                                    "Modificação 08.11.2016

*----------------------------------------------------------------------*
* Start-of-selection
*----------------------------------------------------------------------*
START-OF-SELECTION.

  DATA: lc_continua(1).

  PERFORM valida_acesso_usuario CHANGING lc_continua.

  PERFORM selecionar_dados.
  PERFORM iniciar_variaveis.
  PERFORM organizacao_dados.
  PERFORM imprimir_dados.

*&---------------------------------------------------------------------*
*&      Form  INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM iniciar_variaveis.

*  CLEAR: WG_T001.
*  READ TABLE TG_T001 INTO WG_T001 INDEX 1.
*  IF WG_T001-LAND1 EQ 'BR'.
*    IF P_FORN IS NOT INITIAL.
*      PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-003.
*    ELSE.
*      PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-004.
*    ENDIF.
*  ELSEIF WG_T001-LAND1 EQ 'AR'
*      OR WG_T001-LAND1 EQ 'PY'.
*    IF P_FORN IS NOT INITIAL.
*      PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-005.
*    ELSE.
*      PERFORM F_CONSTRUIR_CABECALHO USING 'H' TEXT-006.
*    ENDIF.
*  ENDIF.

ENDFORM.                    " INICIAR_VARIAVES
*---------------------------------------------------------------------*
*       FORM x_top_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top
      i_logo             = ''.

ENDFORM. "X_TOP_PAGE
*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0181   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.


*  LS_LINE-TYP = 'S'.
*  IF WG_T001-LAND1 EQ 'BR'.
*    LS_LINE-KEY = 'Empresa:'.
*    CONCATENATE  WG_T001-BUKRS '-' WG_T001-BUTXT INTO LS_LINE-INFO SEPARATED BY SPACE.
*  ELSEIF WG_T001-LAND1 EQ 'AR'
*      OR WG_T001-LAND1 EQ 'PY'.
*    LS_LINE-KEY = 'Sociedad:'.
*    CONCATENATE  WG_T001-BUKRS '-' WG_T001-BUTXT INTO LS_LINE-INFO SEPARATED BY SPACE.
*  ENDIF.
*  APPEND LS_LINE TO T_TOP.
*
*  IF WG_T001-LAND1 EQ 'BR'.
*    LS_LINE-KEY = 'Mês/Ano:'.
*    CONCATENATE  S_MES-LOW(2) '/' S_MES-LOW+2(4)  INTO LS_LINE-INFO SEPARATED BY SPACE.
*  ELSEIF WG_T001-LAND1 EQ 'AR'
*      OR WG_T001-LAND1 EQ 'PY'.
*    LS_LINE-KEY = 'Ejercicio:'.
*    CONCATENATE  S_MES-LOW(2) '/' S_MES-LOW+2(4)  INTO LS_LINE-INFO SEPARATED BY SPACE.
*  ENDIF.
*  APPEND LS_LINE TO T_TOP.

ENDFORM.                    " F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprimir_dados .
  DATA: wl_layout TYPE slis_layout_alv,
        wa_hints  TYPE alv_s_qinf.

  PERFORM definir_eventos.
  PERFORM montar_layout USING 'TG_SAIDA'.

  wl_layout-box_tabname = 'TG_SAIDA'.
  wl_layout-box_fieldname = 'MARK'.

  CLEAR: it_hints[].

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_light_out.
  wa_hints-text      = TEXT-010. "'Não Iniciado'.
  wa_hints-fieldname = 'STATUS_LIB'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_yellow_light.
  wa_hints-text      = TEXT-011. "'Aguardando liberação'.
  wa_hints-fieldname = 'STATUS_LIB'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_green_light.
  wa_hints-text      = TEXT-012. "'Liberado'.
  wa_hints-fieldname = 'STATUS_LIB'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_release.
  wa_hints-text      = TEXT-013. "'Aprovado'.
  wa_hints-fieldname = 'STATUS_LIB'.
  APPEND wa_hints TO it_hints.

  wa_hints-type      = cl_salv_tooltip=>c_type_symbol.
  wa_hints-value     = icon_defect.
  wa_hints-text      = TEXT-014. "'Rejeitado'.
  wa_hints-fieldname = 'STATUS_LIB'.
  APPEND wa_hints TO it_hints.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = sy-repid
      i_callback_top_of_page = 'TOP_OF_PAGE' "Modificação 03.11.2016 - Reconc. Internacionais
      it_fieldcat            = estrutura[]
      is_layout              = wl_layout
      i_save                 = 'A'
      it_events              = events
      is_print               = t_print
      it_sort                = lt_sort
      it_except_qinfo        = it_hints
    TABLES
      t_outtab               = tg_saida.

ENDFORM.                    "imprimir_dados
*&---------------------------------------------------------------------*
*&      Form  DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM definir_eventos.

  PERFORM f_carregar_eventos USING:
                                   slis_ev_user_command 'XUSER_COMMAND',
                                   slis_ev_pf_status_set 'XPF_STATUS_SET'.
*                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.


ENDFORM.                    " DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*&      Form  F_CARREGAR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0290   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.

ENDFORM.                    " F_CARREGAR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM top_of_page.

  DATA: it_header TYPE slis_t_listheader,
        wa_header TYPE slis_listheader.

  DATA: s_bukrs_desc TYPE t001-butxt,
        s_depre_desc TYPE zimp_cad_depto-dep_resp_desc.

  SELECT SINGLE butxt
    FROM t001
    INTO s_bukrs_desc
    WHERE bukrs EQ s_bukrs-low.

  SELECT SINGLE dep_resp_desc
    FROM zimp_cad_depto
    INTO s_depre_desc
    WHERE dep_resp EQ s_depre-low.

  wa_header-typ  = 'S'.
  wa_header-key = TEXT-015. "'Empresa: '.
  CONCATENATE s_bukrs-low '-' s_bukrs_desc INTO wa_header-info SEPARATED BY space.
  APPEND wa_header TO it_header.
  CLEAR: wa_header.

  wa_header-typ  = 'S'.
  wa_header-key = TEXT-016. "'Departamento: '.
  CONCATENATE s_depre-low '-' s_depre_desc INTO wa_header-info SEPARATED BY space.
  APPEND wa_header TO it_header.
  CLEAR: wa_header.

  wa_header-typ  = 'S'.
  wa_header-key = TEXT-017. "'Responsável: '.
  wa_header-info = '-'.
  APPEND wa_header TO it_header.
  CLEAR: wa_header.

  wa_header-typ  = 'S'.
  wa_header-key = TEXT-018. "'Mês/Ano: '.
  CONCATENATE s_mes-low '/' s_ano-low INTO wa_header-info.
  APPEND wa_header TO it_header.
  CLEAR: wa_header.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header.

ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout USING p_tabname.

  DATA: wl_edit,
        p_scrtext_1       LIKE dd03p-scrtext_l,
        p_scrtext_2       LIKE dd03p-scrtext_l,
        p_scrtext_3       LIKE dd03p-scrtext_l,
        vg_moeda_1        TYPE dd03p-scrtext_l,
        vg_moeda_2        TYPE dd03p-scrtext_l,
        vg_moeda_3        TYPE dd03p-scrtext_l,
        wa_moedas_empresa TYPE x001.

  CLEAR: wl_edit.
  REFRESH: estrutura, t_fieldcatalog.

  IF p_tabname EQ 'TG_SAIDA'.

    CONCATENATE TEXT-019 wg_t001-waers INTO vg_moeda_1 SEPARATED BY space. "'Total'

    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs = wg_t001-bukrs
      IMPORTING
        e_x001  = wa_moedas_empresa.

    CONCATENATE TEXT-019 wa_moedas_empresa-hwae2 INTO vg_moeda_2 SEPARATED BY space. "'Total'
    CONCATENATE TEXT-019 wa_moedas_empresa-hwae3 INTO vg_moeda_3 SEPARATED BY space. "'Total'

    "// Quando S_SEQ for alimentado, é pq a transação zgl025 está chamando a zgl026
    "// para exibir registros inativos (log).

    IF ( s_seq[] IS NOT INITIAL ).
      PERFORM montar_estrutura USING:
                   01  ' '         ' '              'TG_SAIDA' 'STATUS_LIB'   TEXT-020              ' '  space, "'Status lib.'
                   02  ' '         ' '              'TG_SAIDA' 'STATUSDESC'   TEXT-079              '20'  space, "'Status lib.'
                   03  'SKAT'      'SAKNR'          'TG_SAIDA' 'SAKNR'        ' '                   ' '  space,
                   04  'SKAT'      'TXT50'          'TG_SAIDA' 'TXT50'        ' '                   '15' space,
                   07  ' '         ' '              'TG_SAIDA' 'C_BALANCO'    TEXT-021              '14' space, "'Classificação Balanço'
                   08  ' '         ' '              'TG_SAIDA' 'C_NOTA'       TEXT-022              '14' space, "'Classificação Nota  '
                   09  'FAGLFLEXT' 'HSL16'          'TG_SAIDA' 'SALDO_MI'     vg_moeda_1            '13' 'R',
                   10  'FAGLFLEXT' 'KSL16'          'TG_SAIDA' 'SALDO_MI2'    vg_moeda_2            '13' 'R',
                   11  ' '         ' '              'TG_SAIDA' 'DEP_RESP'      TEXT-023             '14' space, "'Departamento'
                   12  'ZGLT041'   'BNAME2'         'TG_SAIDA' 'BNAME'         TEXT-024             '14' space. "'Responsável'
    ELSE.

      PERFORM montar_estrutura USING:
                    1  ' '         ' '              'TG_SAIDA' 'STATUS_LIB'   TEXT-020              ' '  space, "'Status lib.'
                    2  ' '         ' '              'TG_SAIDA' 'STATUSDESC'   TEXT-079              '20'  space, "'Status lib.'
                    3  'SKAT'      'SAKNR'          'TG_SAIDA' 'SAKNR'        ' '                   ' '  space,
                    4  'SKAT'      'TXT50'          'TG_SAIDA' 'TXT50'        ' '                   '15' space,
                    5  'SKB1'      'MITKZ'          'TG_SAIDA' 'MITKZ'        TEXT-025              '7'  space, "'Tp.Cta'
                    7  ' '         ' '              'TG_SAIDA' 'C_BALANCO'    TEXT-076              '14' space, "'Classificação Balanço'
                    8  ' '         ' '              'TG_SAIDA' 'C_NOTA'       TEXT-022              '14' space, "'Classificação Nota  '
                    9  'ZGLT041'   'CTA_MONET'      'TG_SAIDA' 'CTA_MONET'    TEXT-026              '13' space, "'Monetária'
                   10  'ZGLT041'   'CTA_INTER'      'TG_SAIDA' 'CTA_INTER'    TEXT-027              '10' space, "'Intercompany'
                   11  'FAGLFLEXT' 'HSL16'          'TG_SAIDA' 'SALDO_MI'     vg_moeda_1            '13' 'R',
                   12  'FAGLFLEXT' 'KSL16'          'TG_SAIDA' 'SALDO_MI2'    vg_moeda_2            '13' 'R'.

      IF wg_t001-land1 NE 'BR'.
        PERFORM montar_estrutura USING:
                   12  'FAGLFLEXT' 'OSL16'          'TG_SAIDA' 'SALDO_MI3'    vg_moeda_3                 '13' 'R'.
      ENDIF.

      PERFORM montar_estrutura USING:
                   13  ' '         ' '              'TG_SAIDA' 'DEP_RESP'      TEXT-023             '14'  space, "'Departamento'
                   14  'ZGLT041'   'BNAME2'         'TG_SAIDA' 'BNAME'         TEXT-024             '14'  space, "'Responsável'
                   15  ' '         ' '              'TG_SAIDA' 'NIVEL_APROVA'  TEXT-028             '10'  space, "Modificação 08.11.2016 "'Nível Aprovação'
                   16  ' '         ' '              'TG_SAIDA' 'USUARIO_RESPO' TEXT-029             '100' space, "Modificação 08.11.2016 "'Usuário(s) Resp. Reconciliação'
                   17  'ZGLT041'   'PRAZO_ENTR'     'TG_SAIDA' 'PRAZO_ENTR'    TEXT-030             '14'  space, "'Prazo Entrega'
                   18  ' '         ' '              'TG_SAIDA' 'USUARIO_RECON' TEXT-077             '12'  space, "'Usuário Reconciliante'
                   19  ' '         ' '              'TG_SAIDA' 'CRITERIO'      TEXT-078             '10'  space. "'Critérios Reconciliação'
    ENDIF.

  ELSEIF p_tabname EQ 'TG_REL_AUX'.

    p_scrtext_1 = wg_header-waers.
    p_scrtext_2 = wg_header-waer2.
    p_scrtext_3 = wg_header-waer3.

    IF  wg_header-mitkz NE 'K'
    AND wg_header-mitkz NE 'D'.
      wl_edit = c_x.
    ENDIF.
    PERFORM montar_estrutura_oo USING:
           1  'BSIK'      'BUDAT' 'TG_REL_AUX' 'DATA'   TEXT-033       '12' wl_edit abap_false, "'Data'
           2  'KNA1'      'KUNNR' 'TG_REL_AUX' 'CODIGO' TEXT-034       '15' wl_edit abap_false, "'Código'
           3  'KNA1'      'NAME1' 'TG_REL_AUX' 'NAME1'  TEXT-035       '40' wl_edit abap_false, "'Descrição'
           4  'FAGLFLEXT' 'HSLVT' 'TG_REL_AUX' 'VALOR'  p_scrtext_1    '20' wl_edit abap_false,
           5  'FAGLFLEXT' 'KSLVT' 'TG_REL_AUX' 'VALOR2' p_scrtext_2    '20' wl_edit abap_false.
*    IF WG_T001-LAND1 NE 'BR'.
*      PERFORM MONTAR_ESTRUTURA_OO USING:
*             5  'FAGLFLEXT'          'OSLVT'                'TG_REL_AUX' 'VALOR3'                  P_SCRTEXT_3         '20'         WL_EDIT ABAP_FALSE.
*    ENDIF.
    PERFORM montar_estrutura_oo USING:
          07  ' '  ' '  'TG_REL_AUX' 'TIPO'        TEXT-040           ' '          ' ' abap_false, "'D/C'
          08  ' '  ' '  'TG_REL_AUX' 'SUBTOTALKEY' ' '                ' '          ' ' abap_false,
          09  ' '  ' '  'TG_REL_AUX' 'SORTKEY'     ' '                ' '          ' ' abap_false,
          10  ' '  ' '  'TG_REL_AUX' 'COUNT'       ' '                ' '          ' ' abap_false.
  ELSEIF p_tabname EQ 'TG_REG_PED'.

    p_scrtext_1 = wg_header-waers.
    p_scrtext_2 = wg_header-waer2.
    p_scrtext_3 = wg_header-waer3.

    PERFORM montar_estrutura_oo USING:
           1  'BSIK'               'BLDAT'                'TG_REG_PED' 'BLDAT'                   TEXT-036     '10'         wl_edit abap_false, "'Dt.Documento'
           2  'BSIK'               'BELNR'                'TG_REG_PED' 'BELNR'                   TEXT-037     '15'         wl_edit abap_false, "'Nro.Documento'
           3  'KNA1'               'KUNNR'                'TG_REG_PED' 'CODIGO'                  TEXT-038     '15'         wl_edit abap_false, "'Código'
           4  'KNA1'               'NAME1'                'TG_REG_PED' 'NAME1'                   TEXT-039     '20'         wl_edit abap_false, "'Descrição'
           5  'FAGLFLEXT'          'HSLVT'                'TG_REG_PED' 'DMBTR'                   p_scrtext_1  '15'         wl_edit abap_false,
           6  'FAGLFLEXT'          'HSLVT'                'TG_REG_PED' 'DMBE2'                   p_scrtext_2  '15'         wl_edit abap_false.

*    IF WG_T001-LAND1 NE 'BR'.
*      PERFORM MONTAR_ESTRUTURA_OO USING:
*             7  'FAGLFLEXT'          'HSLVT'              'TG_REG_PED' 'DMBE3'                   P_SCRTEXT_3         '15'         WL_EDIT ABAP_FALSE.
*    ENDIF.
    PERFORM montar_estrutura_oo USING:
          08  ' '                  ' '                    'TG_REG_PED' 'TIPO'                    TEXT-040         ' '          ' '  abap_false, "'D/C'
          09  'BSIK'               'BLDAT'                'TG_REG_PED' 'DT_AJUS'                 TEXT-041         '10'         'X'  abap_false, "'Dt.Ajuste'
          10  'BSIK'               'BELNR'                'TG_REG_PED' 'DOC_AJUS'                TEXT-042         '13'         'X'  abap_true,  "'Doc.Ajuste'
          11  'ZGLT045'            'CTA_CONTRA_PART'      'TG_REG_PED' 'CONTRA'                  TEXT-043         '15'         'X'  abap_false, "'Contrapartida'
          12  'BSIK'               'BLDAT'                'TG_REG_PED' 'DT_VENC'                 TEXT-044         '10'         'X'  abap_false, "'Dt.Vencimento'
          13  'ZGLT045'            'OBSERV'               'TG_REG_PED' 'OBS'                     TEXT-045         '40'         'X'  abap_false, "'Observação'
          14  'VBAK'               'AUART'                'TG_REG_PED' 'AUART'                   TEXT-046         '09'         ' '  abap_false, "modificação 11.01.2017 "'Tipo Doc. Compras'
          15  'BSIK'               'EBELN'                'TG_REG_PED' 'EBELN'                   TEXT-047         '11'         ' '  abap_false, "modificação 11.01.2017 "'Doc. Compras'
          16  'BSID'               'VBEL2'                'TG_REG_PED' 'VBEL2'                   TEXT-048         '10'         ' '  abap_false, "modificação 11.01.2017 "'Ordem'
          17  'BSIK'               'AUGBL'                'TG_REG_PED' 'AUGBL'                   TEXT-049         '16'         ' '  abap_false, "modificação 11.01.2017 "'Doc. Compensação'
          18  'BSIK'               'AUGDT'                'TG_REG_PED' 'AUGDT'                   TEXT-079         '16'         ' '  abap_false. "modificação 04.01.2024 "'Data. Compensação RJF'


  ENDIF.

*  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
*                        USING SY-UNAME.
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
                            VALUE(p_just).

  CLEAR wa_estrutura.

  wa_estrutura-outputlen     = p_outputlen.
  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-just          = p_just.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  IF p_field = 'STATUS_LIB'.
    wa_estrutura-icon = 'X'.
    wa_estrutura-just = 'C'.
  ENDIF.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM selecionar_dados .

  DATA: wl_last_day_aux(8),
        wl_first_day_aux(8),
        vg_kurst_curr       TYPE kurst_curr VALUE 'B   ',
        it_zglt063          TYPE TABLE OF zglt063 WITH HEADER LINE.

  RANGES lc_saknr_usuario FOR zglt063-saknr.

  CONCATENATE s_ano-low s_mes-low '01' INTO vg_last_day_aux.
  vg_last_day = vg_last_day_aux.

  CALL FUNCTION 'BKK_GET_MONTH_LASTDAY'
    EXPORTING
      i_date = vg_last_day
    IMPORTING
      e_date = vg_last_day.

  CONCATENATE vg_last_day+6(2) vg_last_day+4(2) vg_last_day(4) INTO vg_last_day_aux.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = vg_last_day_aux
    IMPORTING
      output = wl_last_day_aux.

  vg_first_day = vg_last_day + 1.
  CONCATENATE vg_first_day+6(2) vg_first_day+4(2) vg_first_day(4) INTO vg_first_day_aux.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = vg_first_day_aux
    IMPORTING
      output = wl_first_day_aux.

  SELECT * FROM zglt042 INTO TABLE tg_042.

  LOOP AT tg_042 INTO wg_042.
    IF wg_042-empresa_ate IS NOT INITIAL.
      IF s_bukrs-low BETWEEN wg_042-empresa_de AND wg_042-empresa_ate.
**       Se o valor da conta estiver entre o range de valores da tabela ZFIT0081 então a linha
**       deve aparecer no relatório.
*        WL_FLAG = C_X.
*        EXIT.
      ELSE.
        CLEAR: wg_042.
        CONTINUE.
      ENDIF.
    ELSEIF wg_042-empresa_de IS NOT INITIAL.
      IF s_bukrs-low EQ wg_042-empresa_de.
*        WL_FLAG = C_X.
*        EXIT.
      ELSE.
        CLEAR: wg_042.
        CONTINUE.
      ENDIF.
    ENDIF.

    IF wg_042-dep_resp IS NOT INITIAL.
      IF s_depre-low EQ wg_042-dep_resp.
*        WL_FLAG = C_X.
*        EXIT.
      ELSE.
        CLEAR: wg_042.
        CONTINUE.
      ENDIF.
    ENDIF.

** Valida Mês
    IF wg_042-mes_ate IS NOT INITIAL.
      IF s_mes-low BETWEEN wg_042-mes_de AND wg_042-mes_ate.
**       Se o valor da conta estiver entre o range de valores da tabela ZFIT0081 então a linha
**       deve aparecer no relatório.
*        WL_FLAG = C_X.
*        EXIT.
      ELSE.
        CLEAR: wg_042.
        CONTINUE.
      ENDIF.
    ELSEIF wg_042-mes_de IS NOT INITIAL.
      IF s_mes-low EQ wg_042-mes_de.
*        WL_FLAG = C_X.
*        EXIT.
      ELSE.
        CLEAR: wg_042.
        CONTINUE.
      ENDIF.
    ENDIF.

** Valida Ano
    IF wg_042-ano_ate IS NOT INITIAL.
      IF s_ano-low BETWEEN wg_042-ano_de AND wg_042-ano_ate.
**       Se o valor da conta estiver entre o range de valores da tabela ZFIT0081 então a linha
**       deve aparecer no relatório.
*        WL_FLAG = C_X.
        EXIT.
      ELSE.
        CLEAR: wg_042.
        CONTINUE.
      ENDIF.
    ELSEIF wg_042-ano_de IS NOT INITIAL.
      IF s_ano-low EQ wg_042-ano_de.
*        WL_FLAG = C_X.
        EXIT.
      ELSE.
        CLEAR: wg_042.
        CONTINUE.
      ENDIF.
    ENDIF.
    CLEAR:wg_042.
  ENDLOOP.

  IF ( wg_042 IS NOT INITIAL ) OR ( s_contas IS NOT INITIAL ).

    SELECT SINGLE * INTO wg_t001
      FROM t001
     WHERE bukrs EQ s_bukrs-low.

    CALL FUNCTION 'FI_CURRENCY_INFORMATION'
      EXPORTING
        i_bukrs = s_bukrs-low
      IMPORTING
        e_x001  = wa_moedas_empresa.

    SELECT SINGLE *
      FROM tcurr
      INTO wg_tcurr_1
     WHERE gdatu EQ wl_last_day_aux
       AND kurst EQ vg_kurst_curr
       AND fcurr EQ wa_moedas_empresa-hwae2
       AND tcurr EQ wg_t001-waers.

    SELECT SINGLE *
      FROM tcurr
      INTO wg_tcurr_2
     WHERE gdatu EQ wl_last_day_aux
       AND kurst EQ vg_kurst_curr
       AND fcurr EQ wa_moedas_empresa-hwae3
       AND tcurr EQ wg_t001-waers.

    SELECT SINGLE *
      FROM tcurr
      INTO wg_tcurr_1_fec
     WHERE gdatu EQ wl_first_day_aux
       AND kurst EQ vg_kurst_curr
       AND fcurr EQ wa_moedas_empresa-hwae2
       AND tcurr EQ wg_t001-waers.

    SELECT SINGLE *
      FROM tcurr
      INTO wg_tcurr_2_fec
     WHERE gdatu EQ wl_first_day_aux
       AND kurst EQ vg_kurst_curr
       AND fcurr EQ wa_moedas_empresa-hwae3
       AND tcurr EQ wg_t001-waers.

    IF s_contas IS INITIAL.

      SELECT * INTO TABLE it_zglt063
        FROM zglt063
       WHERE bukrs    IN s_bukrs
         AND dep_resp IN s_depre
         AND bname    EQ sy-uname.

      LOOP AT it_zglt063.
        lc_saknr_usuario-sign   = 'I'.
        lc_saknr_usuario-option = 'EQ'.
        lc_saknr_usuario-low    = it_zglt063-saknr.
        lc_saknr_usuario-high   = it_zglt063-saknr.
        APPEND lc_saknr_usuario.
      ENDLOOP.

    ENDIF.

    SELECT r~bukrs r~saknr r~cod_clas_bal r~cod_clas_not2
           r~cta_monet r~cta_intercompany r~dep_resp2 r~bname2
           r~prazo_entr r~crit_vecto
      INTO TABLE tg_041
      FROM zglt041 AS r
     WHERE r~bukrs     IN s_bukrs
       AND r~dep_resp2 IN s_depre
       AND r~saknr     IN s_contas
       AND r~saknr     IN lc_saknr_usuario
*       AND R~GJAHR     EQ S_ANO-LOW "/Modificação CS2017000372
       AND EXISTS ( SELECT * FROM skb1 AS k WHERE k~bukrs EQ r~bukrs AND k~saknr EQ r~saknr )
       AND EXISTS ( SELECT * FROM ska1 AS t WHERE t~ktopl EQ wg_t001-ktopl AND t~saknr EQ r~saknr )
       AND NOT EXISTS ( SELECT * FROM zglt043b AS b WHERE b~ktopl EQ wg_t001-ktopl AND b~saknr EQ r~saknr AND b~bukrs EQ r~bukrs ) "/Modificação 11.11.2016/
       AND NOT EXISTS ( SELECT *       "#EC CI_DB_OPERATION_OK[2389136]
                          FROM ska1     AS c "#EC CI_DB_OPERATION_OK[2431747]
                         INNER JOIN zglt043a AS a ON a~ktopl EQ c~ktopl
                                                 AND a~ktoks EQ c~ktoks
                         WHERE c~ktopl EQ wg_t001-ktopl
                           AND c~saknr EQ r~saknr
                           AND a~bukrs EQ r~bukrs ).  "/Modificação 11.11.2016/

    IF ( sy-subrc IS INITIAL ).

      SELECT bname useralias
        FROM usrefus
  INTO TABLE tg_usrefus
     FOR ALL ENTRIES IN tg_041
        WHERE bname EQ tg_041-bname.

      SELECT ktopl saknr ktoks         "#EC CI_DB_OPERATION_OK[2431747]
        FROM ska1                      "#EC CI_DB_OPERATION_OK[2389136]
  INTO TABLE tg_ska1
     FOR ALL ENTRIES IN tg_041
       WHERE ktopl EQ wg_t001-ktopl
         AND saknr EQ tg_041-saknr.

      SELECT *                                    "Modificação 08.11.2016
        FROM zglt058                              "Modificação 08.11.2016
        INTO TABLE tg_zglt058                     "Modificação 08.11.2016
        FOR ALL ENTRIES IN tg_041                 "Modificação 08.11.2016
        WHERE bukrs EQ tg_041-bukrs               "Modificação 08.11.2016
        AND dep_resp EQ tg_041-dep_resp.          "Modificação 08.11.2016

      SORT tg_zglt058 BY bukrs dep_resp.          "Modificação 26.01.2017

      SELECT *                                    "Modificação 08.11.2016
        FROM zglt062                              "Modificação 08.11.2016
        INTO TABLE tg_zglt062                     "Modificação 08.11.2016
        FOR ALL ENTRIES IN tg_041                 "Modificação 08.11.2016
        WHERE bukrs EQ tg_041-bukrs               "Modificação 08.11.2016
        AND dep_resp EQ tg_041-dep_resp.          "Modificação 08.11.2016

      SORT tg_zglt062 BY bukrs dep_resp.          "Modificação 26.01.2017

      SELECT *                                    "Modificação 08.11.2016
        FROM zglt063                              "Modificação 08.11.2016
        INTO TABLE tg_zglt063                     "Modificação 08.11.2016
        FOR ALL ENTRIES IN tg_041                 "Modificação 08.11.2016
        WHERE bukrs EQ tg_041-bukrs               "Modificação 08.11.2016
        AND dep_resp EQ tg_041-dep_resp.          "Modificação 08.11.2016

      SORT tg_zglt063 BY bukrs dep_resp saknr.    "Modificação 26.01.2017

      SELECT *                                    "Modificação 08.11.2016
        FROM zglt059                              "Modificação 08.11.2016
        INTO TABLE tg_zglt059                     "Modificação 08.11.2016
        FOR ALL ENTRIES IN tg_ska1                "Modificação 08.11.2016
        WHERE bukrs        IN s_bukrs             "Modificação 08.11.2016
        AND monat          IN s_mes               "Modificação 08.11.2016
        AND gjahr          IN s_ano               "Modificação 08.11.2016
        AND ck_ultimo_log  EQ 'S'                 "Modificação 08.11.2016
        AND saknr          EQ tg_ska1-saknr.      "Modificação 08.11.2016

      SORT tg_zglt059 BY bukrs gjahr monat saknr. "Modificação 08.11.2016

      SELECT saknr txt50
        FROM skat
  INTO TABLE tg_skat
     FOR ALL ENTRIES IN tg_ska1
       WHERE saknr EQ tg_ska1-saknr
         AND spras EQ sy-langu
         AND ktopl EQ wg_t001-ktopl.

      SELECT saknr mitkz xspeb         "#EC CI_DB_OPERATION_OK[2431747]
        FROM skb1
  INTO TABLE tg_skb1
     FOR ALL ENTRIES IN tg_ska1
       WHERE saknr EQ tg_ska1-saknr
         AND bukrs IN s_bukrs.

      SELECT codigo descr cod_nota descr_nota
        FROM zglt039
        INTO TABLE tg_039
         FOR ALL ENTRIES IN tg_041
       WHERE codigo    EQ tg_041-cod_clas_bal
         AND cod_nota  EQ tg_041-cod_clas_not.

      SELECT dep_resp dep_resp_desc
        FROM zimp_cad_depto
        INTO TABLE tg_depto
     FOR ALL ENTRIES IN tg_041
       WHERE dep_resp EQ tg_041-dep_resp.

      IF ( sy-tcode = 'ZGL025' ).
        SELECT *
          FROM zglt075
          INTO CORRESPONDING FIELDS OF TABLE tg_043
       FOR ALL ENTRIES IN tg_041
         WHERE seq   IN s_seq
           AND bukrs IN s_bukrs
           AND monat IN s_mes
           AND gjahr IN s_ano
           AND saknr EQ tg_041-saknr.

        SELECT *
          FROM zglt055
          INTO TABLE tg_044
       FOR ALL ENTRIES IN tg_043
         WHERE seq   IN s_seq
           AND bukrs EQ tg_043-bukrs
           AND monat EQ tg_043-monat
           AND gjahr EQ tg_043-gjahr
           AND saknr EQ tg_043-saknr.

        SELECT *
          FROM zglt066
          INTO TABLE tg_045
       FOR ALL ENTRIES IN tg_043
         WHERE seq   IN s_seq
           AND bukrs EQ tg_043-bukrs
           AND monat EQ tg_043-monat
           AND gjahr EQ tg_043-gjahr
           AND saknr EQ tg_043-saknr.
      ELSE.

        SELECT *
          FROM zglt043
          INTO TABLE tg_043
           FOR ALL ENTRIES IN tg_041
           WHERE bukrs IN s_bukrs
             AND monat IN s_mes
             AND gjahr IN s_ano
             AND saknr EQ tg_041-saknr.

        IF ( sy-subrc IS INITIAL ).
          SELECT *
            FROM zglt044
            INTO TABLE tg_044
             FOR ALL ENTRIES IN tg_043
             WHERE bukrs EQ tg_043-bukrs
               AND monat EQ tg_043-monat
               AND gjahr EQ tg_043-gjahr
               AND saknr EQ tg_043-saknr.

          SELECT *
            FROM zglt045
            INTO TABLE tg_045
             FOR ALL ENTRIES IN tg_043
             WHERE bukrs EQ tg_043-bukrs
               AND monat EQ tg_043-monat
               AND gjahr EQ tg_043-gjahr
               AND saknr EQ tg_043-saknr.
        ENDIF.

        DATA: it_contas TYPE zct_emp_contas,
              wa_contas TYPE zlc_emp_contas.

        LOOP AT tg_041 INTO wg_041.
          wa_contas-bukrs = wg_041-bukrs.
          wa_contas-saknr = wg_041-saknr.
          APPEND wa_contas TO it_contas.
        ENDLOOP.

        CALL FUNCTION 'Z_FI_GL_SALDO_FAGLFLEXT'
          EXPORTING
            ryear         = s_ano-low
            contas        = it_contas
            p_gerar_todas = abap_true
          TABLES
            it_saldos     = it_saldo_contas
            it_saldos_2   = it_saldo_contas_2
            it_saldos_3   = it_saldo_contas_3
          EXCEPTIONS
            moeda_nao_adm = 1
            erro_ledger   = 2
            OTHERS        = 3.

        IF sy-subrc IS NOT INITIAL.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        CLEAR: it_zglt059_rec[].

        SELECT * INTO TABLE it_zglt059_rec
          FROM zglt059
           FOR ALL ENTRIES IN tg_041
         WHERE bukrs EQ tg_041-bukrs
           AND saknr EQ tg_041-saknr
           AND monat IN s_mes
           AND gjahr IN s_ano
           AND status_lib EQ 'L'
           AND nivel      EQ 0.

      ENDIF.
    ENDIF.

  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-050  "'Para os dados informados nos parametros,'
                                           TEXT-051. "'não esta liberado para reconciliação'.
    STOP.
  ENDIF.

ENDFORM.                    " SELECIONAR_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZACAO_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organizacao_dados .

  DATA: wl_saldo_mi  TYPE faglflext-hslvt,
        wl_saldo_mi2 TYPE faglflext-kslvt,
        wl_saldo_mi3 TYPE faglflext-oslvt.

  DATA: refe1	     TYPE hslxx12,
        refe2	     TYPE hslxx12,
        vmes       TYPE monat,
        vexiste(1).

  DATA: wl_name      TYPE thead-tdname.

  SORT: tg_flext   BY racct,
        tg_043     BY saknr bukrs,
        tg_skat    BY saknr,
        tg_skb1    BY saknr,
        tg_usrefus BY bname,
        tg_039     BY codigo cod_nota,
        tg_depto   BY dep_resp,
        tg_043     BY bukrs saknr monat gjahr,
        tg_044     BY bukrs saknr monat gjahr,
        tg_045     BY bukrs saknr monat gjahr.

  DATA: lt_faglflext TYPE TABLE OF faglflext.
  vmes = s_mes-low.
  IF vmes = 12.
    vmes = 16.
  ENDIF.


  SELECT * FROM faglflext
    INTO TABLE @DATA(tg_faglflext)
    FOR ALL ENTRIES IN @tg_041
    WHERE ryear = @s_ano-low
      AND racct = @tg_041-saknr
      AND rbukrs = @tg_041-bukrs.

  lt_faglflext[] = tg_faglflext[].

  LOOP AT tg_041 INTO wg_041.
    IF ( sy-tcode = 'ZGL025' ).
      READ TABLE tg_043 INTO wg_043 WITH KEY bukrs = wg_041-bukrs
                                             saknr = wg_041-saknr
                                             monat = s_mes-low
                                             gjahr = s_ano-low
                                             BINARY SEARCH.

      READ TABLE tg_skat INTO wg_skat
        WITH KEY saknr = wg_041-saknr
                 BINARY SEARCH.

      READ TABLE tg_skb1 INTO wg_skb1
        WITH KEY saknr = wg_041-saknr
                 BINARY SEARCH.

      READ TABLE tg_usrefus INTO wg_usrefus
        WITH KEY bname = wg_041-bname
                 BINARY SEARCH.

      READ TABLE tg_039 INTO wg_039
        WITH KEY codigo   = wg_041-cod_clas_bal
                 cod_nota = wg_041-cod_clas_not
                  BINARY SEARCH.

      READ TABLE tg_depto INTO wg_depto
        WITH KEY dep_resp = wg_041-dep_resp
                    BINARY SEARCH.

      SELECT SINGLE responsavel
        FROM zglt075
        INTO wg_saida-bname
       WHERE bukrs = wg_043-bukrs
         AND saknr = wg_043-saknr
         AND monat = wg_043-monat
         AND gjahr = wg_043-gjahr.

      IF s_bukrs-low  EQ '0101'.

        wl_saldo_mi  =  wg_043-sdo_mi * 100.
        wl_saldo_mi2 =  wg_043-sdo_mi2 * 100.
        "WL_SALDO_MI3 =  WL_SALDO_MI3 * 100.

      ENDIF.

      wg_saida-status_lib        = icon_led_inactive.
      wg_saida-bukrs             = wg_043-bukrs.
      wg_saida-butxt             = wg_t001-butxt.
      wg_saida-land1             = wg_t001-land1.
*        WG_SAIDA-WAERS             = WG_T001-WAERS.
*        WG_SAIDA-WAER2             = WA_MOEDAS_EMPRESA-HWAE2.
*        WG_SAIDA-WAER3             = WA_MOEDAS_EMPRESA-HWAE3.
      wg_saida-saknr             = wg_041-saknr.
      wg_saida-prazo_entr        = wg_041-prazo_entr.
      wg_saida-txt50             = wg_skat-txt50.
      wg_saida-mitkz             = wg_skb1-mitkz.
      wg_saida-saldo_mi          = wg_043-sdo_mi.
      wg_saida-saldo_mi2         = wg_043-sdo_mi2.
*        WG_SAIDA-SALDO_MI3         = WL_SALDO_MI3.

      IF ( wg_039-codigo IS NOT INITIAL ).
        CONCATENATE wg_039-codigo '-' wg_039-descr INTO wg_saida-c_balanco SEPARATED BY space.
      ENDIF.

      IF ( wg_039-cod_nota IS NOT INITIAL ).
        CONCATENATE wg_039-cod_nota '-' wg_039-descr_nota INTO wg_saida-c_nota SEPARATED BY space.
      ENDIF.

      wg_saida-codigo         = wg_039-codigo .
      wg_saida-descr          = wg_039-descr .
      wg_saida-cod_nota       = wg_039-cod_nota.
      wg_saida-descr_nota     = wg_039-descr_nota.

      IF ( wg_depto-dep_resp IS NOT INITIAL ).
        CONCATENATE wg_depto-dep_resp '-' wg_depto-dep_resp_desc INTO wg_saida-dep_resp SEPARATED BY space.
      ENDIF.

      wg_saida-cod_resp       = wg_depto-dep_resp.
      wg_saida-dep_resp_desc  = wg_depto-dep_resp_desc.

      APPEND wg_saida TO tg_saida.

      CLEAR: wg_041,
             wg_skat,
             wg_039,
             wg_040,
             wg_041,
             wg_depto,
             wg_saida,
             wg_043.
    ELSE.

      wl_saldo_mi = 0.
      READ TABLE it_saldo_contas WITH KEY ryear  = s_ano-low
                                          rbukrs = wg_041-bukrs
                                          racct  = wg_041-saknr.
      IF sy-subrc IS INITIAL.
        ADD it_saldo_contas-slvt TO wl_saldo_mi.
        DO vmes TIMES VARYING refe1 FROM it_saldo_contas-sl01 NEXT it_saldo_contas-sl02.
          ADD refe1 TO wl_saldo_mi.
        ENDDO.
      ENDIF.

      wl_saldo_mi2 = 0.
      READ TABLE it_saldo_contas_2 WITH KEY ryear  = s_ano-low
                                            rbukrs = wg_041-bukrs
                                            racct  = wg_041-saknr.
      IF sy-subrc IS INITIAL.
        ADD it_saldo_contas_2-slvt TO wl_saldo_mi2.

        IF vmes => 1.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl01.
        ENDIF.
        IF vmes => 2.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl02.
        ENDIF.
        IF vmes => 3.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl03.
        ENDIF.
        IF vmes => 4.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl04.
        ENDIF.
        IF vmes => 5.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl05.
        ENDIF.
        IF vmes => 6.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl06.
        ENDIF.
        IF vmes => 7.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl07.
        ENDIF.
        IF vmes => 8.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl08.
        ENDIF.
        IF vmes => 9.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl09.
        ENDIF.
        IF vmes => 10.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl10.
        ENDIF.
        IF vmes => 11.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl11.
        ENDIF.
        IF vmes => 12.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl12.
        ENDIF.
        IF vmes => 13.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl13.
        ENDIF.
        IF vmes => 14.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl14.
        ENDIF.
        IF vmes => 15.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl15.
        ENDIF.
        IF vmes => 16.
          wl_saldo_mi2 = wl_saldo_mi2 + it_saldo_contas_2-sl16.
        ENDIF.
*        alterado por Gr 24.07.23
*        DO vmes TIMES VARYING refe1 FROM it_saldo_contas_2-sl01 NEXT it_saldo_contas_2-sl02.
*          ADD refe1 TO wl_saldo_mi2.
*        ENDDO.
*        alterado por Gr 24.07.23
      ENDIF.

      wl_saldo_mi3 = 0.
      READ TABLE it_saldo_contas_3 WITH KEY ryear  = s_ano-low
                                            rbukrs = wg_041-bukrs
                                            racct  = wg_041-saknr.
      IF sy-subrc IS INITIAL.
        ADD it_saldo_contas_3-slvt TO wl_saldo_mi3.





        IF vmes => 1.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl01.
        ENDIF.
        IF vmes => 2.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl02.
        ENDIF.
        IF vmes => 3.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl03.
        ENDIF.
        IF vmes => 4.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl04.
        ENDIF.
        IF vmes => 5.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl05.
        ENDIF.
        IF vmes => 6.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl06.
        ENDIF.
        IF vmes => 7.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl07.
        ENDIF.
        IF vmes => 8.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl08.
        ENDIF.
        IF vmes => 9.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl09.
        ENDIF.
        IF vmes => 10.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl10.
        ENDIF.
        IF vmes => 11.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl11.
        ENDIF.
        IF vmes => 12.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl12.
        ENDIF.
        IF vmes => 13.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl13.
        ENDIF.
        IF vmes => 14.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl14.
        ENDIF.
        IF vmes => 15.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl15.
        ENDIF.
        IF vmes => 16.
          wl_saldo_mi3 = wl_saldo_mi3 + it_saldo_contas_3-sl16.
        ENDIF.
*        DO vmes TIMES VARYING refe1 FROM it_saldo_contas_3-sl01 NEXT it_saldo_contas_3-sl02.
*          ADD refe1 TO wl_saldo_mi3.
*        ENDDO.
      ENDIF.

      CLEAR vexiste.
      READ TABLE tg_043 INTO wg_043
        WITH KEY bukrs = wg_041-bukrs
                 saknr = wg_041-saknr
                 monat = s_mes-low
                 gjahr = s_ano-low
                 BINARY SEARCH.
      IF sy-subrc = 0.
        vexiste = 'X'.
      ENDIF.

      READ TABLE tg_skat INTO wg_skat
        WITH KEY saknr = wg_041-saknr
                 BINARY SEARCH.

      READ TABLE tg_skb1 INTO wg_skb1
        WITH KEY saknr = wg_041-saknr
                 BINARY SEARCH.

      READ TABLE tg_usrefus INTO wg_usrefus
        WITH KEY bname = wg_041-bname
                 BINARY SEARCH.

      READ TABLE tg_039 INTO wg_039
        WITH KEY codigo   = wg_041-cod_clas_bal
                 cod_nota = wg_041-cod_clas_not
                  BINARY SEARCH.

      READ TABLE tg_depto INTO wg_depto
        WITH KEY dep_resp = wg_041-dep_resp
                    BINARY SEARCH.

      CASE wg_043-status_lib.
        WHEN ' '.
          wg_saida-status_lib = icon_light_out.
        WHEN 'S'.
          wg_saida-status_lib = icon_booking_ok.
        WHEN 'P'.
          wg_saida-status_lib = icon_yellow_light.
        WHEN 'L'.
          wg_saida-status_lib = icon_green_light.
        WHEN 'A'.
          wg_saida-status_lib = icon_release.
        WHEN 'R'.
          wg_saida-status_lib = icon_defect.

      ENDCASE.

*      alterado por Guilherme Rabelo inicio

      CASE wg_043-status_lib.
        WHEN ' '.
          wg_saida-statusdesc = 'Não Iniciado'.
        WHEN 'P'.
          wg_saida-statusdesc = 'Aguardando Liberação'.
        WHEN 'L'.
          wg_saida-statusdesc = 'Liberado'.
        WHEN 'A'.
          wg_saida-statusdesc = 'Aprovado'.
        WHEN 'R'.
          wg_saida-statusdesc = 'Reprovado'.
        WHEN 'S'.
          wg_saida-statusdesc = 'Sem Movimento'.
      ENDCASE.
* alterado por guilherme rabelo fim
      " USER STORY 77886
      IF s_bukrs-low  EQ '0101'.

        wg_saida-saldo_mi  =  wl_saldo_mi * 100.
        "  wl_saldo_mi2 =  wl_saldo_mi2 * 100.
        "  wl_saldo_mi3 =  wl_saldo_mi3 * 100.
      ELSE.
        wg_saida-saldo_mi          = wl_saldo_mi.
      ENDIF.
      wg_saida-saldo_mi2         = wl_saldo_mi2.
      wg_saida-saldo_mi3         = wl_saldo_mi3.

      wg_saida-bukrs             = wg_041-bukrs.
      wg_saida-butxt             = wg_t001-butxt.
      wg_saida-land1             = wg_t001-land1.
      wg_saida-waers             = wg_t001-waers.
      wg_saida-waer2             = wa_moedas_empresa-hwae2.
      wg_saida-waer3             = wa_moedas_empresa-hwae3.
      wg_saida-saknr             = wg_041-saknr.
      wg_saida-prazo_entr        = wg_041-prazo_entr.
      wg_saida-txt50             = wg_skat-txt50.
      wg_saida-mitkz             = wg_skb1-mitkz.

      IF wg_039-codigo IS NOT INITIAL.
        CONCATENATE wg_039-codigo '-' wg_039-descr INTO wg_saida-c_balanco SEPARATED BY space.
      ENDIF.
      IF wg_039-cod_nota IS NOT INITIAL.
        CONCATENATE wg_039-cod_nota '-' wg_039-descr_nota INTO wg_saida-c_nota SEPARATED BY space.
      ENDIF.
      wg_saida-codigo         = wg_039-codigo .
      wg_saida-descr          = wg_039-descr .
      wg_saida-cod_nota       = wg_039-cod_nota.
      wg_saida-descr_nota     = wg_039-descr_nota.
      IF wg_depto-dep_resp IS NOT INITIAL.
        CONCATENATE wg_depto-dep_resp '-' wg_depto-dep_resp_desc INTO wg_saida-dep_resp SEPARATED BY space.
      ENDIF.
      wg_saida-cod_resp          = wg_depto-dep_resp.
      wg_saida-dep_resp_desc     = wg_depto-dep_resp_desc.
      wg_saida-bname             = wg_041-bname.
      wg_saida-desc_bname        = wg_usrefus-useralias.

      IF wg_041-cta_monet EQ 'S'.
        wg_saida-cta_monet = TEXT-052. "'S - SIM'.
      ELSEIF wg_041-cta_monet EQ 'N'.
        wg_saida-cta_monet = TEXT-053. "'N - NÃO'.
      ENDIF.

      IF wg_041-cta_intercompany EQ 'S'.
        wg_saida-cta_inter = TEXT-052. "'S - SIM'.
      ELSEIF wg_041-cta_intercompany EQ 'N'.
        wg_saida-cta_inter = TEXT-053. "'N - NÃO'.
      ENDIF.

      REFRESH: tg_texto.
      CLEAR: wl_name.
      CONCATENATE wg_saida-bukrs wg_saida-saknr INTO wl_name.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = 'ZCRI'
          language                = sy-langu
          name                    = wl_name
          object                  = 'ZCRITERIO'
        TABLES
          lines                   = tg_texto
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc IS INITIAL.
        wg_saida-criterio = icon_text_act.
        LOOP AT tg_texto.
          MOVE: wg_saida-bukrs    TO wa_aux-bukrs,
                wg_saida-saknr    TO wa_aux-saknr,
                wg_saida-txt50    TO wa_aux-txt50,
                wg_saida-mitkz    TO wa_aux-mitkz,
                tg_texto-tdformat TO wa_aux-tdformat,
                tg_texto-tdline   TO wa_aux-tdline.

          APPEND wa_aux TO tg_aux.
          CLEAR: wa_aux.

        ENDLOOP.
      ELSE.
        wg_saida-criterio = icon_text_ina.
      ENDIF.

      "Usuário Reconciliante """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
      ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      CLEAR: wa_zglt059_rec.

      LOOP AT it_zglt059_rec WHERE bukrs EQ wg_saida-bukrs
                               AND saknr EQ wg_saida-saknr.
        IF wa_zglt059_rec IS INITIAL.
          MOVE it_zglt059_rec TO wa_zglt059_rec.
        ELSEIF wa_zglt059_rec-dt_liberacao GT wa_zglt059_rec-dt_liberacao.
          MOVE it_zglt059_rec TO wa_zglt059_rec.
        ELSEIF it_zglt059_rec-dt_liberacao EQ wa_zglt059_rec-dt_liberacao AND it_zglt059_rec-hr_liberacao GT wa_zglt059_rec-hr_liberacao.
          MOVE it_zglt059_rec TO wa_zglt059_rec.
        ENDIF.
      ENDLOOP.

      wg_saida-usuario_recon = wa_zglt059_rec-bn_liberacao.
      "Usuário Reconciliante """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
      "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      PERFORM busca_nivel_aprovacao CHANGING wg_saida-nivel_aprova      "Modificação 08.11.2016
                                             wg_saida-usuario_respo.    "Modificação 08.11.2016




*alterado por guilherme rabelo inicio
*valida o status S

      DATA: v_mes    TYPE zglt042-mes_de,
            v_ano    TYPE zglt042-ano_de,
            v_dt(07) TYPE c.

      v_mes = s_mes-low.
      v_ano = s_ano-low.

*      CONCATENATE v_mes '.' v_ano INTO v_dt.
      CONCATENATE v_ano v_mes INTO v_dt.

      SELECT * FROM tvarvc
        INTO @DATA(ls_tvarvc)
        WHERE name = 'ZGL026_DT_CORTE'
          AND low <= @v_dt.
      ENDSELECT.

      IF sy-subrc = 0.

        IF v_mes = '1'.

          v_ano = v_ano - 1.
          v_mes = '12'.

        ELSE.

          v_mes = v_mes - 1.

        ENDIF.

        SELECT * FROM zglt043
          INTO @DATA(ls_check)
          WHERE monat = @v_mes
           AND  gjahr = @v_ano
           AND  bukrs = @wg_saida-bukrs
           AND  saknr = @wg_saida-saknr.
        ENDSELECT.

        IF sy-subrc = 0.
          IF ls_check-sdo_mi   = wg_saida-saldo_mi  AND ls_check-sdo_mi2   = wg_saida-saldo_mi2.

            IF s_mes-low <> '12'.
              wg_saida-status_lib = icon_booking_ok.
              wg_saida-statusdesc = 'Sem Movimento'.
              ls_check-status_lib = 'S'.
              ls_check-monat = s_mes-low.
              ls_check-gjahr = s_ano-low.
              MODIFY zglt043 FROM ls_check.
            ELSEIF vexiste IS INITIAL.
              "Mes 12 não grava ALRS
              wg_saida-status_lib = icon_light_out.
              wg_saida-statusdesc = 'Não Iniciado'.
              ls_check-status_lib = ' '.
              ls_check-monat = s_mes-low.
              ls_check-gjahr = s_ano-low.
*
*              MODIFY zglt043 FROM ls_check.

            ENDIF.

          ELSEIF s_mes-low <> '12'.
            v_mes = s_mes-low.
            v_ano = s_ano-low.
            IF wg_saida-statusdesc = 'Sem Movimento' OR (  vexiste = ' ' AND s_mes-low = '01' ).
              DELETE  FROM zglt043  " Apaga para refazer ALRS
                WHERE monat = v_mes
                 AND  gjahr = v_ano
                 AND  bukrs = wg_saida-bukrs
                 AND  saknr = wg_saida-saknr.
              COMMIT WORK.
              wg_saida-status_lib = icon_light_out.
              wg_saida-statusdesc = 'Não Iniciado'.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSEIF wg_saida-statusdesc = 'Sem Movimento' OR (  vexiste = ' ' AND s_mes-low = '01' ).
        IF s_mes-low <> '12'.
          wg_saida-status_lib = icon_light_out.
          wg_saida-statusdesc = 'Não Iniciado'.
          ls_check-status_lib = ' '.
          ls_check-monat = s_mes-low.

          MODIFY zglt043 FROM ls_check.
        ENDIF.
      ENDIF.

*alterado por guilherme rabelo fim
      APPEND wg_saida TO tg_saida.
      CLEAR: wg_041, wg_skat, wg_039, wg_040, wg_041, wg_depto, wg_saida, wg_043.

    ENDIF.
  ENDLOOP.

ENDFORM.                    " ORGANIZACAO_DADOS
*---------------------------------------------------------------------*
*       FORM XUSER_COMMAND                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xuser_command USING ucomm    LIKE sy-ucomm
                         selfield TYPE kkblo_selfield..     "#EC CALLED
  DATA: BEGIN OF tl_saknr OCCURS 0,
          saknr TYPE zglt041-saknr,
          mitkz TYPE skb1-mitkz,
        END OF tl_saknr.

  DATA: lc_value TYPE p DECIMALS 4.

  DATA: tl_saknr_aux         LIKE TABLE OF tl_saknr,
        wa_saknr_aux         LIKE LINE OF tl_saknr,
        wl_saldo_mi          TYPE faglflext-hslvt,
        wl_saldo_mi2         TYPE faglflext-kslvt,
        wl_saldo_mi3         TYPE faglflext-oslvt,
        wl_dmbtr             TYPE bdif2,
        wl_dmbe2             TYPE bdif2,
        wl_dmbe3             TYPE bdif2,
        tl_044               TYPE TABLE OF zglt044 WITH HEADER LINE,
        wl_count             TYPE sy-tabix,
        vg_razao_especial(1),
        it_t074              TYPE TABLE OF t074 WITH HEADER LINE,
        answer               TYPE c LENGTH 1.

  CLEAR: wl_count.

  CASE ucomm.
    WHEN c_new_doc.

      REFRESH: tg_flext,
               "TG_BSIK_BSID,
               tl_saknr,
               tg_rel_aux,
               tg_reg_ped,
               tg_lfa1,
               tg_kna1.

      LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL.
        MOVE wg_saida-saknr TO tl_saknr-saknr.
        MOVE wg_saida-mitkz TO tl_saknr-mitkz.
        APPEND tl_saknr.
        CLEAR : tl_saknr.
      ENDLOOP.

      IF tl_saknr[] IS NOT INITIAL.
        SORT: tl_saknr BY saknr.

        SELECT *
          FROM faglflext
          INTO TABLE tg_flext
           FOR ALL ENTRIES IN tl_saknr
         WHERE racct  EQ tl_saknr-saknr
           AND rbukrs IN s_bukrs
           AND ryear  IN s_ano
           AND rldnr  EQ '0L'.

** Fornecedores
        tl_saknr_aux[] = tl_saknr[].
        DELETE tl_saknr_aux WHERE mitkz NE 'K'.
        IF tl_saknr_aux[] IS NOT INITIAL.

          CLEAR: it_contas.

          LOOP AT tl_saknr_aux INTO wa_saknr_aux.
            wa_contas-bukrs = s_bukrs-low.
            wa_contas-saknr = wa_saknr_aux-saknr.
            APPEND wa_contas TO it_contas.
          ENDLOOP.

          CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
            EXPORTING
              p_dt_posicao      = vg_last_day
              contas            = it_contas
              p_verifica_rz_esp = abap_true
            TABLES
              it_bsxk           = it_bsxk
              it_saldo_contas   = it_saldo_contas
              it_saldo_parceiro = it_saldo_parceiro.

          IF it_bsxk[] IS NOT INITIAL.
            SELECT lifnr name1
              FROM lfa1
              INTO TABLE tg_lfa1
               FOR ALL ENTRIES IN it_bsxk
             WHERE lifnr EQ it_bsxk-lifnr.

            SORT: tg_lfa1 BY lifnr.
          ENDIF.
        ENDIF.

** Clientes
        tl_saknr_aux[] = tl_saknr[].
        DELETE tl_saknr_aux WHERE mitkz NE 'D'.
        IF tl_saknr_aux[] IS NOT INITIAL.

          CLEAR: it_contas.

          LOOP AT tl_saknr_aux INTO wa_saknr_aux.
            wa_contas-bukrs = s_bukrs-low.
            wa_contas-saknr = wa_saknr_aux-saknr.
            APPEND wa_contas TO it_contas.
          ENDLOOP.

          CALL FUNCTION 'Z_FI_GL_SALDO_BSIX'
            EXPORTING
              p_dt_posicao      = vg_last_day
              contas            = it_contas
              p_verifica_rz_esp = abap_true
            TABLES
              it_bsxd           = it_bsxd
              it_saldo_contas   = it_saldo_contas
              it_saldo_parceiro = it_saldo_parceiro.

          IF it_bsxd[] IS NOT INITIAL.
            SELECT kunnr name1
              FROM kna1
              INTO TABLE tg_kna1
              FOR ALL ENTRIES IN it_bsxd
               WHERE kunnr EQ it_bsxd-kunnr.

            SORT: tg_kna1 BY kunnr.
          ENDIF.
        ENDIF.

        SORT: tg_flext BY racct,
              it_bsxk  BY hkont,
              it_bsxd  BY hkont.

        wg_acao = c_new_doc.
        LOOP AT tg_saida INTO wg_saida
           WHERE mark IS NOT INITIAL.

          SELECT SINGLE * INTO wa_zglt043 FROM zglt043
           WHERE bukrs  EQ wg_saida-bukrs
             AND saknr  EQ wg_saida-saknr
             AND monat  EQ s_mes-low
             AND gjahr  EQ s_ano-low.

          IF sy-subrc IS INITIAL.
*Inicio Alteração - Leandro Valentim Ferreira - 21.08.23 - #120565
            IF wg_saida-statusdesc EQ 'Sem Movimento'.
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-080 "Conta sem movimento, não é necessário
                                                     TEXT-081." a conciliação.
              EXIT.
            ELSE.
*Fim Alteração - Leandro Valentim Ferreira - 21.08.23 - #120565
              MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-054 "'Já existem documento(s) criados de reconciliação'
                                                     TEXT-055."'para esses dados'.
              EXIT.
            ENDIF.
          ENDIF.
          PERFORM busca_saldos_contabil USING wg_saida-saknr
                                     CHANGING wl_saldo_mi
                                              wl_saldo_mi2
                                              wl_saldo_mi3.

          PERFORM busca_saldo_cliente_fornec USING wg_saida-saknr
                                          CHANGING wl_dmbtr
                                                   wl_dmbe2
                                                   wl_dmbe3.

          CLEAR: wg_header, wg_totais.

          wg_043-monat = s_mes-low.
          wg_043-gjahr = s_ano-low.
          wg_043-bukrs = wg_saida-bukrs.
          wg_043-saknr = wg_saida-saknr.

          MOVE:
                wg_saida-bukrs          TO wg_header-bukrs,
                wg_saida-butxt          TO wg_header-desc_bukrs,
                wg_saida-land1          TO wg_header-land1,
                wg_saida-waers          TO wg_header-waers,
                wg_saida-waer2          TO wg_header-waer2,
                wg_saida-waer3          TO wg_header-waer3,
                wg_saida-cod_resp       TO wg_header-dep_resp,
                wg_saida-dep_resp_desc  TO wg_header-desc_dep_resp,
                wg_saida-bname          TO wg_header-bname,
                wg_saida-desc_bname     TO wg_header-desc_bname,
                s_mes-low               TO wg_header-monat,
                s_ano-low               TO wg_header-gjahr,
                wg_saida-saknr          TO wg_header-saknr,
                wg_saida-txt50          TO wg_header-desc_saknr,
                wg_saida-codigo         TO wg_header-codigo,
                wg_saida-descr          TO wg_header-descr,
                wg_saida-cod_nota       TO wg_header-cod_nota,
                wg_saida-descr_nota     TO wg_header-descr_nota,
                wg_saida-mitkz          TO wg_header-mitkz,
                wg_saida-cta_monet(1)   TO wg_header-cta_monet.

          CONCATENATE wg_header-waers '/' wg_header-waer2 INTO wg_header-depara.
          CONCATENATE wg_header-waers '/' wg_header-waer3 INTO wg_header-depara2.

          " USER STORY 77886
          IF s_bukrs-low  EQ '0101'.

            wg_totais-saldo_mi  = wl_saldo_mi * 100.
            "   wg_totais-saldo_mi2 =  wl_saldo_mi2 * 100.
            " wg_totais-saldo_mi3 = wl_saldo_mi3 * 100.
          ELSE.
            wg_totais-saldo_mi      = wl_saldo_mi.


          ENDIF.
          wg_totais-saldo_mi2     = wl_saldo_mi2.
          wg_totais-saldo_mi3     = wl_saldo_mi3.

          wg_totais-saldo_aux_mi  = wl_dmbtr.
          wg_totais-saldo_aux_mi2 = wl_dmbe2.
          wg_totais-saldo_aux_mi3 = wl_dmbe3.
          wg_totais-tx_fech1      = wg_tcurr_1_fec-ukurs.
          wg_totais-tx_fech2      = wg_tcurr_2_fec-ukurs.
          wg_totais-dif_mi1       = wg_totais-saldo_mi  - wg_totais-saldo_aux_mi.
          wg_totais-dif_mi2       = wg_totais-saldo_mi2 - wg_totais-saldo_aux_mi2.
          wg_totais-dif_mi3       = wg_totais-saldo_mi3 - wg_totais-saldo_aux_mi3.

          TRY.
              lc_value = wg_totais-saldo_mi / wg_totais-saldo_mi2.
              IF lc_value GT 99999.
                wg_totais-tx_hist1 = 99999.
              ELSE.
                wg_totais-tx_hist1 = wg_totais-saldo_mi / wg_totais-saldo_mi2.
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY.
              lc_value = wg_totais-saldo_mi / wg_totais-saldo_mi3.
              IF lc_value GT 99999.
                wg_totais-tx_hist2 = 99999.
              ELSEIF lc_value LT -99999.                                "Modificação 22.02.2017
                wg_totais-tx_paridade2 = -99999.
              ELSE.
                wg_totais-tx_hist2 = wg_totais-saldo_mi / wg_totais-saldo_mi3.
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY.
              lc_value = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
              IF lc_value GT 99999.
                wg_totais-tx_paridade1 = 99999.
              ELSE.
                wg_totais-tx_paridade1 = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY.
              lc_value = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi3.
              IF lc_value GT 99999.
                wg_totais-tx_paridade2 = 99999.
              ELSEIF lc_value LT -99999.                                "Modificação 22.02.2017
                wg_totais-tx_paridade2 = -99999.
              ELSE.
                wg_totais-tx_paridade2 = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi3.
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          "Partidas em Aberto de Fornecedor
          LOOP AT it_bsxk WHERE hkont EQ wg_saida-saknr.

            READ TABLE tl_saknr WITH KEY saknr = it_bsxk-hkont BINARY SEARCH.

            READ TABLE tg_lfa1 INTO wg_lfa1 WITH KEY lifnr = it_bsxk-lifnr BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              wg_rel_aux-codigo = wg_lfa1-lifnr.
              wg_rel_aux-name1  = wg_lfa1-name1.
            ENDIF.

            IF it_bsxk-shkzg EQ 'H'.
              MULTIPLY it_bsxk-dmbtr BY -1.
              MULTIPLY it_bsxk-dmbe2 BY -1.
            ENDIF.

            wg_rel_aux-data   = vg_last_day.
            wg_rel_aux-valor  = it_bsxk-dmbtr.
            wg_rel_aux-valor2 = it_bsxk-dmbe2.
            wg_rel_aux-sortkey = 'A'.

            COLLECT wg_rel_aux INTO tg_rel_aux.

            wg_reg_ped-bldat  = it_bsxk-bldat.
            wg_reg_ped-belnr  = it_bsxk-belnr.
            wg_reg_ped-codigo = wg_rel_aux-codigo.
            wg_reg_ped-name1  = wg_rel_aux-name1.
            wg_reg_ped-dmbtr  = it_bsxk-dmbtr.
            wg_reg_ped-dmbe2  = it_bsxk-dmbe2.
            wg_reg_ped-buzei  = it_bsxk-buzei.    "------------*01.03.2017 LG
            IF wg_reg_ped-dmbtr LT 0.
              wg_reg_ped-tipo = 'C'.
            ELSE.
              wg_reg_ped-tipo = 'D'.
            ENDIF.


            PERFORM preenche_doc_compras USING it_bsxk-bukrs
                                               wg_reg_ped-belnr          "------------*11.01.2017 LG
                                               wg_reg_ped-codigo
                                               wg_saida-saknr
                                               it_bsxk-buzei
                                         CHANGING wg_reg_ped-auart
                                                  wg_reg_ped-ebeln
                                                  wg_reg_ped-vbel2
                                                  wg_reg_ped-augbl
                                                  wg_reg_ped-augdt. "RJF

            APPEND wg_reg_ped TO tg_reg_ped.

            CLEAR: wg_rel_aux, wg_reg_ped.

          ENDLOOP.

          "Partidas em Aberto de Clente
          LOOP AT it_bsxd WHERE hkont EQ wg_saida-saknr.

            READ TABLE tl_saknr WITH KEY saknr = it_bsxd-hkont BINARY SEARCH.

            READ TABLE tg_kna1 INTO wg_kna1 WITH KEY kunnr = it_bsxd-kunnr BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              wg_rel_aux-codigo = wg_kna1-kunnr.
              wg_rel_aux-name1  = wg_kna1-name1.
            ENDIF.

            IF it_bsxd-shkzg EQ 'H'.
              MULTIPLY it_bsxd-dmbtr BY -1.
              MULTIPLY it_bsxd-dmbe2 BY -1.
            ENDIF.

            wg_rel_aux-data    = vg_last_day.
            wg_rel_aux-valor   = it_bsxd-dmbtr.
            wg_rel_aux-valor2  = it_bsxd-dmbe2.
            wg_rel_aux-sortkey = 'A'.

            COLLECT wg_rel_aux INTO tg_rel_aux.

            wg_reg_ped-bldat  = it_bsxd-bldat.
            wg_reg_ped-belnr  = it_bsxd-belnr.
            wg_reg_ped-codigo = wg_rel_aux-codigo.
            wg_reg_ped-name1  = wg_rel_aux-name1.
            wg_reg_ped-dmbtr  = it_bsxd-dmbtr.
            wg_reg_ped-dmbe2  = it_bsxd-dmbe2.
            wg_reg_ped-buzei  = it_bsxd-buzei.    "------------*01.03.2017 LG
            IF wg_reg_ped-dmbtr LT 0.
              wg_reg_ped-tipo = 'C'.
            ELSE.
              wg_reg_ped-tipo = 'D'.
            ENDIF.

            PERFORM preenche_doc_compras USING it_bsxd-bukrs
                                               wg_reg_ped-belnr          "------------*11.01.2017
                                               wg_reg_ped-codigo
                                               wg_saida-saknr
                                               it_bsxd-buzei
                                         CHANGING wg_reg_ped-auart
                                                  wg_reg_ped-ebeln
                                                  wg_reg_ped-vbel2
                                                  wg_reg_ped-augbl
                                                  wg_reg_ped-augdt. "RJF

            APPEND wg_reg_ped TO tg_reg_ped.

            CLEAR: wg_rel_aux, wg_reg_ped.

          ENDLOOP.

*Inicio de alteração - Fmartins - 16/12/2022 -  RMNI - IR104343
          IF s_bukrs-low  EQ '0101'.
            LOOP AT tg_rel_aux INTO wg_rel_aux. "Corrige valores individuais para empresa 0101
              wg_rel_aux-valor = wg_rel_aux-valor * 100.
              MODIFY tg_rel_aux FROM wg_rel_aux TRANSPORTING valor.
            ENDLOOP.
          ENDIF.
*Fim de alteração - Fmartins - 16/12/2022  -  RMNI - IR104343


          LOOP AT tg_rel_aux INTO wg_rel_aux.
            IF wg_rel_aux-valor LT 0.
              wg_rel_aux-tipo = 'C'.
            ELSE.
              wg_rel_aux-tipo = 'D'.
            ENDIF.

            MODIFY tg_rel_aux FROM wg_rel_aux TRANSPORTING tipo.
          ENDLOOP.

          wg_rel_aux-data = vg_last_day.
*          CONCATENATE WG_SAIDA-SAKNR '-' WG_SAIDA-TXT50 INTO  WG_REL_AUX-HISTO SEPARATED BY SPACE.
          wg_rel_aux-codigo = wg_saida-saknr.
          wg_rel_aux-name1  = wg_saida-txt50.
          wg_rel_aux-valor  = wg_totais-saldo_mi.
          wg_rel_aux-valor2 = wg_totais-saldo_mi2.
          wg_rel_aux-valor3 = wg_totais-saldo_mi3.
          wg_rel_aux-line_color = 'C305'.

          IF wg_rel_aux-valor LT 0.
            wg_rel_aux-tipo = 'C'.
          ELSE.
            wg_rel_aux-tipo = 'D'.
          ENDIF.
          wg_rel_aux-sortkey = 'C'.
          APPEND wg_rel_aux TO tg_rel_aux.
          CLEAR : wg_rel_aux.

          CLEAR: wg_dif_conf, wg_dif_conf2.
          CALL SCREEN 100.
          MODIFY tg_saida FROM wg_saida.
          IF wg_exit IS NOT INITIAL.
            CLEAR : wg_exit.
            EXIT.
          ENDIF.

        ENDLOOP.
      ENDIF.
      CLEAR: wg_acao.
    WHEN c_motivo_neg.

      DATA: it_zglt059_aux TYPE TABLE OF zglt059 WITH HEADER LINE,
            wa_zglt059_aux TYPE zglt059,
            it_texto       TYPE TABLE OF tline WITH HEADER LINE,
            longtext_tab   TYPE catsxt_longtext_itab.

      LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL.

        CLEAR: it_zglt059_aux[].

        SELECT * INTO TABLE it_zglt059_aux
          FROM zglt059
         WHERE bukrs EQ wg_saida-bukrs
           AND saknr EQ wg_saida-saknr
           AND monat EQ s_mes-low
           AND gjahr EQ s_ano-low
           AND status_lib EQ 'R'
         ORDER BY dt_liberacao hr_liberacao.

        LOOP AT it_zglt059_aux.
          IF wa_zglt059_aux IS INITIAL.
            MOVE it_zglt059_aux TO wa_zglt059_aux.
          ELSEIF it_zglt059_aux-dt_liberacao GT wa_zglt059_aux-dt_liberacao.
            MOVE it_zglt059_aux TO wa_zglt059_aux.
          ELSEIF ( it_zglt059_aux-dt_liberacao EQ wa_zglt059_aux-dt_liberacao AND
                   it_zglt059_aux-hr_liberacao GT wa_zglt059_aux-hr_liberacao ).
            MOVE it_zglt059_aux TO wa_zglt059_aux.
          ENDIF.
        ENDLOOP.

        IF wa_zglt059_aux-texto_tdname IS NOT INITIAL.

          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = wa_zglt059_aux-texto_tdid
              language                = wa_zglt059_aux-texto_tdspras
              name                    = wa_zglt059_aux-texto_tdname
              object                  = wa_zglt059_aux-texto_tdobject
            TABLES
              lines                   = it_texto
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.

          IF sy-subrc IS INITIAL.
            LOOP AT it_texto.
              APPEND it_texto-tdline TO longtext_tab.
            ENDLOOP.

            CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
              EXPORTING
                im_title        = TEXT-056 "'Motivo de Recusa'
                im_display_mode = 'X'
              CHANGING
                ch_text         = longtext_tab.
          ENDIF.

        ENDIF.
      ENDLOOP.
      CLEAR: wg_acao.

    WHEN  c_chg_doc.
      REFRESH: tg_rel_aux, tg_reg_ped, tg_lfa1, tg_kna1, tl_044.

      tl_044[] = tg_044[].
      DELETE tl_044 WHERE lifnr IS INITIAL.

      IF ( tl_044[] IS NOT INITIAL ).
        SELECT lifnr name1
          FROM lfa1
          INTO TABLE tg_lfa1
          FOR ALL ENTRIES IN tl_044
           WHERE lifnr EQ tl_044-lifnr.

        SORT: tg_lfa1 BY lifnr.
      ENDIF.

      tl_044[] = tg_044[].
      DELETE tl_044 WHERE kunnr IS INITIAL.

      IF ( tl_044[] IS NOT INITIAL ).
        SELECT kunnr name1
          FROM kna1
          INTO TABLE tg_kna1
          FOR ALL ENTRIES IN tl_044
           WHERE kunnr EQ tl_044-kunnr.

        SORT: tg_kna1 BY kunnr.
      ENDIF.

      MOVE c_chg_doc TO wg_acao.

      LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL.
        CLEAR: gt_zglt055_log, gt_zglt066_log, gt_srgbtbrel, gt_services.

        SELECT SINGLE * INTO wa_zglt043 FROM zglt043
         WHERE bukrs  EQ wg_saida-bukrs
           AND saknr  EQ wg_saida-saknr
           AND monat  EQ s_mes-low
           AND gjahr  EQ s_ano-low.

        IF ( sy-subrc IS INITIAL ).

          MOVE-CORRESPONDING wa_zglt043 TO wg_043.
          IF wa_zglt043-status_lib EQ ' ' OR
             wa_zglt043-status_lib EQ 'L' OR
             wa_zglt043-status_lib EQ 'A'.
            MESSAGE w007 WITH wg_saida-saknr.
            CONTINUE.
          ENDIF.

          CLEAR: wg_header, wg_totais.

          MOVE:
                wg_saida-bukrs          TO wg_header-bukrs,
                wg_saida-butxt          TO wg_header-desc_bukrs,
                wg_saida-land1          TO wg_header-land1,
                wg_saida-waers          TO wg_header-waers,
                wg_saida-waer2          TO wg_header-waer2,
                wg_saida-waer3          TO wg_header-waer3,
                wg_saida-cod_resp       TO wg_header-dep_resp,
                wg_saida-dep_resp_desc  TO wg_header-desc_dep_resp,
                wg_saida-bname          TO wg_header-bname,
                wg_saida-desc_bname     TO wg_header-desc_bname,
                s_mes-low               TO wg_header-monat,
                s_ano-low               TO wg_header-gjahr,
                wg_saida-saknr          TO wg_header-saknr,
                wg_saida-txt50          TO wg_header-desc_saknr,
                wg_saida-codigo         TO wg_header-codigo,
                wg_saida-descr          TO wg_header-descr,
                wg_saida-cod_nota       TO wg_header-cod_nota,
                wg_saida-descr_nota     TO wg_header-descr_nota,
                wg_saida-mitkz          TO wg_header-mitkz,
                wg_saida-cta_monet(1)   TO wg_header-cta_monet.

          CONCATENATE wg_header-waers '/' wg_header-waer2 INTO wg_header-depara.
          CONCATENATE wg_header-waers '/' wg_header-waer3 INTO wg_header-depara2.

          " USER STORY 77886
          IF s_bukrs-low  EQ '0101' AND
             sy-ucomm     NE 'CHG_DOC'.

            wg_totais-saldo_mi      = wa_zglt043-sdo_mi * 100.
            "  wg_totais-saldo_mi2     = wa_zglt043-sdo_mi2 * 100.
            "  wg_totais-saldo_mi3     = wa_zglt043-sdo_mi3 * 100.
          ELSE.
            wg_totais-saldo_mi      = wa_zglt043-sdo_mi.
          ENDIF.
          wg_totais-saldo_mi2     = wa_zglt043-sdo_mi2.
          wg_totais-saldo_mi3     = wa_zglt043-sdo_mi3.

          wg_header-fonte_infor   = wa_zglt043-fonte_infor.

          wg_totais-saldo_aux_mi  = wa_zglt043-sdo_rel_aux_mi.
          wg_totais-saldo_aux_mi2 = wa_zglt043-sdo_rel_aux_mi2.
          wg_totais-saldo_aux_mi3 = wa_zglt043-sdo_rel_aux_mi3.
          wg_totais-tx_fech1      = wa_zglt043-tx_fech1.
          wg_totais-tx_fech2      = wa_zglt043-tx_fech2.
          wg_totais-dif_mi1       = wg_totais-saldo_mi  - wg_totais-saldo_aux_mi.
          wg_totais-dif_mi2       = wg_totais-saldo_mi2 - wg_totais-saldo_aux_mi2.
          wg_totais-dif_mi3       = wg_totais-saldo_mi3 - wg_totais-saldo_aux_mi3.

          TRY.
              lc_value = wg_totais-saldo_mi / wg_totais-saldo_mi2.
              IF lc_value GT 99999.
                wg_totais-tx_hist1 = 99999.
              ELSE.
                wg_totais-tx_hist1 = wg_totais-saldo_mi / wg_totais-saldo_mi2.
              ENDIF.

            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY.
              lc_value = wg_totais-saldo_mi / wg_totais-saldo_mi3.
              IF lc_value GT 99999.
                wg_totais-tx_hist2 = 99999.
              ELSEIF lc_value LT -99999.                                "Modificação 22.02.2017
                wg_totais-tx_paridade2 = -99999.
              ELSE.
                wg_totais-tx_hist2 = wg_totais-saldo_mi / wg_totais-saldo_mi3.
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY.
              lc_value = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
              IF lc_value GT 99999.
                wg_totais-tx_paridade1 = 99999.
              ELSE.
                wg_totais-tx_paridade1 = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          TRY.
              lc_value = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi3.
              IF lc_value GT 99999.
                wg_totais-tx_paridade2 = 99999.
              ELSEIF lc_value LT -99999.                                "Modificação 22.02.2017
                wg_totais-tx_paridade2 = -99999.
              ELSE.
                wg_totais-tx_paridade2 = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi3.
              ENDIF.
            CATCH cx_sy_zerodivide.
          ENDTRY.

          LOOP AT tg_044 INTO wg_044 WHERE bukrs EQ wa_zglt043-bukrs
                                       AND saknr EQ wa_zglt043-saknr
                                       AND monat EQ wa_zglt043-monat
                                       AND gjahr EQ wa_zglt043-gjahr.

            READ TABLE tg_skb1 INTO wg_skb1
              WITH KEY saknr = wg_044-saknr
                       BINARY SEARCH.

            IF ( wg_044-lifnr IS NOT INITIAL ).
              READ TABLE tg_lfa1 INTO wg_lfa1
                WITH KEY lifnr = wg_044-lifnr
                         BINARY SEARCH.

              IF sy-subrc IS INITIAL.
                wg_rel_aux-codigo = wg_lfa1-lifnr.
                wg_rel_aux-name1  = wg_lfa1-name1.
              ENDIF.

            ELSEIF ( wg_044-kunnr IS NOT INITIAL ).
              READ TABLE tg_kna1 INTO wg_kna1
                  WITH KEY kunnr = wg_044-kunnr
                           BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wg_rel_aux-codigo = wg_kna1-kunnr.
                wg_rel_aux-name1  = wg_kna1-name1.
              ENDIF.

            ELSEIF ( wg_044-kunnr IS INITIAL
               AND   wg_044-lifnr IS INITIAL
               AND   wg_044-cod   IS INITIAL ).

              READ TABLE tg_skat INTO wg_skat
              WITH KEY saknr = wg_044-saknr
                       BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                wg_rel_aux-codigo = wg_skat-saknr.
                wg_rel_aux-name1  = wg_skat-txt50.
              ENDIF.

            ELSEIF ( wg_044-cod IS NOT INITIAL ).
              wg_rel_aux-codigo = wg_044-cod.
              wg_rel_aux-name1  = wg_044-descr.
            ENDIF.

            MOVE: wg_044-bldat           TO wg_rel_aux-data,
                  wg_044-dmbtr           TO wg_rel_aux-valor,
                  wg_044-dmbe2           TO wg_rel_aux-valor2,
                  wg_044-sortkey         TO wg_rel_aux-sortkey,
                  wg_044-cta_contra_part TO wg_rel_aux-contra.

            IF ( wg_rel_aux-valor LT 0 ).
              wg_rel_aux-tipo = 'C'.
            ELSE.
              wg_rel_aux-tipo = 'D'.
            ENDIF.

            APPEND: wg_rel_aux TO tg_rel_aux,
                    wg_044     TO gt_zglt055_log. "//Guarda uma cópia dos registros;

            CLEAR: wg_skat, wg_rel_aux.
          ENDLOOP.

          LOOP AT tg_045 INTO wg_045 WHERE bukrs EQ wa_zglt043-bukrs
                                       AND saknr EQ wa_zglt043-saknr
                                       AND monat EQ wa_zglt043-monat
                                       AND gjahr EQ wa_zglt043-gjahr.

            wg_reg_ped-bldat    = wg_045-bldat.
            wg_reg_ped-belnr    = wg_045-belnr.
            wg_reg_ped-dmbtr    = wg_045-dmbtr.
            wg_reg_ped-dmbe2    = wg_045-dmbe2.
            wg_reg_ped-dt_venc  = wg_045-dt_vcto.
            wg_reg_ped-dt_ajus  = wg_045-dt_ajuste.
            wg_reg_ped-doc_ajus = wg_045-nro_doc_aj.
            wg_reg_ped-obs      = wg_045-observ.
            wg_reg_ped-contra   = wg_045-cta_contra_part.
            wg_reg_ped-buzei    = wg_045-buzei2.         "/Modificação CS2017000645

            IF ( wg_reg_ped-dmbtr LT 0 ).
              wg_reg_ped-tipo = 'C'.
            ELSE.
              wg_reg_ped-tipo = 'D'.
            ENDIF.

            IF ( wg_saida-mitkz EQ 'K' ).
              READ TABLE tg_lfa1 INTO wg_lfa1
                WITH KEY lifnr = wg_045-lifnr
                         BINARY SEARCH.

              IF sy-subrc IS INITIAL.
                wg_reg_ped-codigo = wg_lfa1-lifnr.
                wg_reg_ped-name1  = wg_lfa1-name1.
              ENDIF.

            ELSEIF ( wg_saida-mitkz EQ 'D' ).
              READ TABLE tg_kna1 INTO wg_kna1
                  WITH KEY kunnr = wg_045-kunnr
                           BINARY SEARCH.

              IF sy-subrc IS INITIAL.
                wg_reg_ped-codigo = wg_kna1-kunnr.
                wg_reg_ped-name1  = wg_kna1-name1.
              ENDIF.

            ELSE.
              wg_reg_ped-codigo = wg_045-cod.
              wg_reg_ped-name1  = wg_045-descr.
            ENDIF.

            PERFORM preenche_doc_compras USING wg_045-bukrs
                                               wg_reg_ped-belnr                 "------------*11.01.2017 LG
                                               wg_reg_ped-codigo
                                               wg_045-saknr
                                               wg_045-buzei2                    "------------*01.03.2017 LG
                                         CHANGING wg_reg_ped-auart
                                                  wg_reg_ped-ebeln
                                                  wg_reg_ped-vbel2
                                                  wg_reg_ped-augbl
                                                  wg_reg_ped-augdt. "RJF

            APPEND: wg_reg_ped TO tg_reg_ped,
                    wg_045     TO gt_zglt066_log. "//Guarda uma cópia dos registros;

            CLEAR: wg_reg_ped.
          ENDLOOP.

          "//-- Guarda uma cópia temporária dos anexos--;

          "//Source
          CONCATENATE wg_header-monat wg_header-gjahr wg_header-bukrs wg_header-saknr
          INTO ls_source-instid.
          ls_source-typeid = 'ZGL026'.
          ls_source-catid  = 'BO'.

          "//Target
          CONCATENATE wg_header-monat wg_header-gjahr wg_header-bukrs wg_header-saknr
          'TEMPZGL026' INTO ls_target-instid.
          ls_target-typeid = 'ZTEMP'.
          ls_target-catid  = 'BO'.

          ls_service-sign   = 'I'.
          ls_service-option = 'EQ'.
          ls_service-low    = 'PCATTA_CREA'.
          APPEND ls_service TO gt_services.

          CALL METHOD cl_gos_service_tools=>copy_linked_objects
            EXPORTING
              is_source            = ls_source
              is_target            = ls_target
              it_service_selection = gt_services.

          COMMIT WORK.
          "//---

          CALL SCREEN 100.

          IF ( wg_exit IS NOT INITIAL ).
            CLEAR : wg_exit.
            EXIT.
          ENDIF.

        ELSE.
          MESSAGE s006 WITH wg_saida-saknr.
        ENDIF.
      ENDLOOP.
      CLEAR: wg_acao.
    WHEN 'VIEW_DOC'.
      REFRESH: tg_rel_aux, tg_reg_ped, tg_lfa1, tg_kna1, tl_044.
      tl_044[] = tg_044[].
      DELETE tl_044 WHERE lifnr IS INITIAL.
      IF tl_044[] IS NOT INITIAL.
        SELECT lifnr name1
          FROM lfa1
          INTO TABLE tg_lfa1
          FOR ALL ENTRIES IN tl_044
           WHERE lifnr EQ tl_044-lifnr.

        SORT: tg_lfa1 BY lifnr.
      ENDIF.

      tl_044[] = tg_044[].
      DELETE tl_044 WHERE kunnr IS INITIAL.
      IF tl_044[] IS NOT INITIAL.
        SELECT kunnr name1
          FROM kna1
          INTO TABLE tg_kna1
          FOR ALL ENTRIES IN tl_044
           WHERE kunnr EQ tl_044-kunnr.

        SORT: tg_kna1 BY kunnr.
      ENDIF.

      MOVE c_view_doc TO wg_acao.

      LOOP AT tg_saida INTO wg_saida
         WHERE mark IS NOT INITIAL.

        READ TABLE tg_043 INTO wg_043
           WITH KEY bukrs = wg_saida-bukrs
                    saknr = wg_saida-saknr
                    monat = s_mes-low
                    gjahr = s_ano-low
                    BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-057 "'Não existem documento(s) criados de reconciliação'
                                                 TEXT-055 "'para esses dados'
                                                 wg_saida-bukrs
                                                 wg_saida-saknr.
          EXIT.
        ENDIF.

        CLEAR: wg_header, wg_totais.
        MOVE: wg_043-seq              TO wg_header-seq,
              wg_saida-bukrs          TO wg_header-bukrs,
              wg_saida-butxt          TO wg_header-desc_bukrs,
              wg_saida-land1          TO wg_header-land1,
              wg_saida-waers          TO wg_header-waers,
              wg_saida-waer2          TO wg_header-waer2,
              wg_saida-waer3          TO wg_header-waer3,
              wg_saida-cod_resp       TO wg_header-dep_resp,
              wg_saida-dep_resp_desc  TO wg_header-desc_dep_resp,
              wg_saida-bname          TO wg_header-bname,
              wg_saida-desc_bname     TO wg_header-desc_bname,
              s_mes-low               TO wg_header-monat,
              s_ano-low               TO wg_header-gjahr,
              wg_saida-saknr          TO wg_header-saknr,
              wg_saida-txt50          TO wg_header-desc_saknr,
              wg_saida-codigo         TO wg_header-codigo,
              wg_saida-descr          TO wg_header-descr,
              wg_saida-cod_nota       TO wg_header-cod_nota,
              wg_saida-descr_nota     TO wg_header-descr_nota,
              wg_saida-mitkz          TO wg_header-mitkz,
              wg_saida-cta_monet(1)   TO wg_header-cta_monet.

        CONCATENATE wg_header-waers '/' wg_header-waer2 INTO wg_header-depara.
        CONCATENATE wg_header-waers '/' wg_header-waer3 INTO wg_header-depara2.
*Inicio de alteração - Fmartins - 22/11/2022 -  RMNI - IR104343
*        " USER STORY 77886
*        IF s_bukrs-low  EQ '0101'.
**          wg_totais-saldo_mi      = wg_043-sdo_mi * 100.
*          "  wg_totais-saldo_mi2     = wg_043-sdo_mi2 * 100.
*          " wg_totais-saldo_mi3     = wg_043-sdo_mi3 * 100.
*        ELSE.

*Fim de alteração - Fmartins - 22/11/2022 -  RMNI - IR104343
        wg_totais-saldo_mi      = wg_043-sdo_mi.
*Inicio de alteração - Fmartins - 22/11/2022 -  RMNI - IR104343
*        ENDIF.
*Fim de alteração - Fmartins - 22/11/2022 -  RMNI - IR104343
        wg_header-fonte_infor   = wg_043-fonte_infor.
        wg_totais-saldo_mi2     = wg_043-sdo_mi2.
        wg_totais-saldo_mi3     = wg_043-sdo_mi3.
        wg_totais-saldo_aux_mi  = wg_043-sdo_rel_aux_mi.
        wg_totais-saldo_aux_mi2 = wg_043-sdo_rel_aux_mi2.
        wg_totais-saldo_aux_mi3 = wg_043-sdo_rel_aux_mi3.
        wg_totais-tx_fech1      = wg_043-tx_fech1.
        wg_totais-tx_fech2      = wg_043-tx_fech2.
        wg_totais-dif_mi1       = wg_totais-saldo_mi  - wg_totais-saldo_aux_mi.
        wg_totais-dif_mi2       = wg_totais-saldo_mi2 - wg_totais-saldo_aux_mi2.
        wg_totais-dif_mi3       = wg_totais-saldo_mi3 - wg_totais-saldo_aux_mi3.

        TRY.
            lc_value = wg_totais-saldo_mi / wg_totais-saldo_mi2.
            IF lc_value GT 99999.
              wg_totais-tx_hist1 = 99999.
            ELSE.
              wg_totais-tx_hist1 = wg_totais-saldo_mi / wg_totais-saldo_mi2.
            ENDIF.

          CATCH cx_sy_zerodivide.
        ENDTRY.

        TRY.
            lc_value = wg_totais-saldo_mi / wg_totais-saldo_mi3.
            IF lc_value GT 99999.
              wg_totais-tx_hist2 = 99999.
            ELSEIF lc_value LT -99999.                                "Modificação 22.02.2017
              wg_totais-tx_paridade2 = -99999.
            ELSE.
              wg_totais-tx_hist2 = wg_totais-saldo_mi / wg_totais-saldo_mi3.
            ENDIF.
          CATCH cx_sy_zerodivide.
        ENDTRY.

        TRY.
            lc_value = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
            IF lc_value GT 99999.
              wg_totais-tx_paridade1 = 99999.
            ELSE.
              wg_totais-tx_paridade1 = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
            ENDIF.
          CATCH cx_sy_zerodivide.
        ENDTRY.

        TRY.
            lc_value = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi3.
            IF lc_value GT 99999.
              wg_totais-tx_paridade2 = 99999.
            ELSEIF lc_value LT -99999.                                "Modificação 22.02.2017
              wg_totais-tx_paridade2 = -99999.
            ELSE.
              wg_totais-tx_paridade2 = wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi3.
            ENDIF.
          CATCH cx_sy_zerodivide.
        ENDTRY.

        LOOP AT tg_044 INTO wg_044
           WHERE bukrs EQ wg_043-bukrs
             AND saknr EQ wg_043-saknr
             AND monat EQ wg_043-monat
             AND gjahr EQ wg_043-gjahr.

          READ TABLE tg_skb1 INTO wg_skb1
            WITH KEY saknr = wg_044-saknr
                     BINARY SEARCH.

          IF wg_044-lifnr IS NOT INITIAL.
            READ TABLE tg_lfa1 INTO wg_lfa1
              WITH KEY lifnr = wg_044-lifnr
                       BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wg_rel_aux-codigo = wg_lfa1-lifnr.
              wg_rel_aux-name1  = wg_lfa1-name1.
            ENDIF.
          ELSEIF wg_044-kunnr IS NOT INITIAL.
            READ TABLE tg_kna1 INTO wg_kna1
                WITH KEY kunnr = wg_044-kunnr
                         BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wg_rel_aux-codigo = wg_kna1-kunnr.
              wg_rel_aux-name1  = wg_kna1-name1.
            ENDIF.
          ELSEIF wg_044-kunnr IS INITIAL
             AND wg_044-lifnr IS INITIAL
             AND wg_044-cod   IS INITIAL.
            READ TABLE tg_skat INTO wg_skat
            WITH KEY saknr = wg_044-saknr
                     BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wg_rel_aux-codigo = wg_skat-saknr.
              wg_rel_aux-name1  = wg_skat-txt50.
            ENDIF.
          ELSEIF wg_044-cod IS NOT INITIAL.
            wg_rel_aux-codigo = wg_044-cod.
            wg_rel_aux-name1  = wg_044-descr.
          ENDIF.


          MOVE: wg_044-bldat TO wg_rel_aux-data,
                wg_044-dmbtr TO wg_rel_aux-valor,
                wg_044-dmbe2 TO wg_rel_aux-valor2,
                wg_044-sortkey TO wg_rel_aux-sortkey,
                wg_044-cta_contra_part TO wg_rel_aux-contra.

          IF wg_rel_aux-valor LT 0.
            wg_rel_aux-tipo = 'C'.
          ELSE.
            wg_rel_aux-tipo = 'D'.
          ENDIF.

          APPEND wg_rel_aux TO tg_rel_aux.
          CLEAR: wg_skat, wg_rel_aux.
        ENDLOOP.

        LOOP AT tg_045 INTO wg_045
           WHERE bukrs EQ wg_043-bukrs
             AND saknr EQ wg_043-saknr
             AND monat EQ wg_043-monat
             AND gjahr EQ wg_043-gjahr.

          wg_reg_ped-bldat       = wg_045-bldat.
          wg_reg_ped-belnr       = wg_045-belnr.
          wg_reg_ped-dmbtr       = wg_045-dmbtr.
          wg_reg_ped-dmbe2       = wg_045-dmbe2.
          wg_reg_ped-dt_venc     = wg_045-dt_vcto.
          wg_reg_ped-dt_ajus     = wg_045-dt_ajuste.
          wg_reg_ped-doc_ajus    = wg_045-nro_doc_aj.
          wg_reg_ped-obs         = wg_045-observ.
          wg_reg_ped-contra      = wg_045-cta_contra_part.
          IF wg_reg_ped-dmbtr LT 0.
            wg_reg_ped-tipo = 'C'.
          ELSE.
            wg_reg_ped-tipo = 'D'.
          ENDIF.

          IF wg_saida-mitkz EQ 'K'.
            READ TABLE tg_lfa1 INTO wg_lfa1
              WITH KEY lifnr = wg_045-lifnr
                       BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wg_reg_ped-codigo = wg_lfa1-lifnr.
              wg_reg_ped-name1  = wg_lfa1-name1.
            ENDIF.
          ELSEIF wg_saida-mitkz EQ 'D'.
            READ TABLE tg_kna1 INTO wg_kna1
                WITH KEY kunnr = wg_045-kunnr
                         BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              wg_reg_ped-codigo = wg_kna1-kunnr.
              wg_reg_ped-name1  = wg_kna1-name1.
            ENDIF.
          ELSE.
            wg_reg_ped-codigo  = wg_045-cod.
            wg_reg_ped-name1   = wg_045-descr.
          ENDIF.

          PERFORM preenche_doc_compras USING wg_045-bukrs
                                             wg_reg_ped-belnr                 "------------*11.01.2017 LG
                                             wg_reg_ped-codigo
                                             wg_045-saknr
                                             wg_045-buzei2                    "------------*01.03.2017 LG
                                       CHANGING wg_reg_ped-auart
                                                wg_reg_ped-ebeln
                                                wg_reg_ped-vbel2
                                                wg_reg_ped-augbl
                                                wg_reg_ped-augdt. "RJF

          APPEND wg_reg_ped TO tg_reg_ped.

          CLEAR: wg_reg_ped.
        ENDLOOP.
        CALL SCREEN 100.
        IF wg_exit IS NOT INITIAL.
          CLEAR : wg_exit.
          EXIT.
        ENDIF.

      ENDLOOP.
      CLEAR: wg_acao.
    WHEN 'DEL_DOC'.

      CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
        EXPORTING
          titel     = TEXT-001
          textline1 = TEXT-002
          textline2 = TEXT-003
        IMPORTING
          answer    = answer.

      IF answer EQ 'J'.
        LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL.

          SELECT SINGLE *
            INTO wa_zglt043 FROM zglt043
           WHERE bukrs  EQ wg_saida-bukrs
             AND saknr  EQ wg_saida-saknr
             AND monat  EQ s_mes-low
             AND gjahr  EQ s_ano-low.

          IF sy-subrc IS INITIAL.

            IF wa_zglt043-status_lib EQ 'A'.
              MESSAGE w001 WITH wg_saida-saknr.
            ELSEIF wa_zglt043-status_lib EQ 'L'.
              MESSAGE w015 WITH wg_saida-saknr.
            ELSEIF wa_zglt043-status_lib EQ 'R'.
              MESSAGE w016 WITH wg_saida-saknr.
            ELSE.
              "Somente pode ser excluido itens pendentes
              IF "WA_ZGLT043-STATUS_LIB EQ 'L' OR
                 "WA_ZGLT043-STATUS_LIB EQ 'R' OR
                 wa_zglt043-status_lib EQ 'P'.
                DELETE FROM zglt043 WHERE bukrs  EQ wg_saida-bukrs
                                      AND saknr  EQ wg_saida-saknr
                                      AND monat  EQ s_mes-low
                                      AND gjahr  EQ s_ano-low.

                ADD 1 TO wl_count.
                DELETE FROM zglt044 WHERE bukrs  EQ wg_saida-bukrs
                                      AND saknr  EQ wg_saida-saknr
                                      AND monat  EQ s_mes-low
                                      AND gjahr  EQ s_ano-low.

                DELETE FROM zglt045 WHERE bukrs  EQ wg_saida-bukrs
                                      AND saknr  EQ wg_saida-saknr
                                      AND monat  EQ s_mes-low
                                      AND gjahr  EQ s_ano-low.

                wg_saida-status_lib = icon_light_out.
                MODIFY tg_saida FROM wg_saida TRANSPORTING status_lib.

                DATA: objeto_t TYPE thead.

                CONCATENATE wa_zglt043-bukrs wa_zglt043-gjahr wa_zglt043-monat wa_zglt043-saknr INTO objeto_t-tdname.
                objeto_t-tdobject = 'ZRECONCILI'.
                objeto_t-tdid     = 'ZREC'.
                objeto_t-tdspras  = sy-langu.

                CALL FUNCTION 'DELETE_TEXT'
                  EXPORTING
                    id        = objeto_t-tdid
                    language  = objeto_t-tdspras
                    name      = objeto_t-tdname
                    object    = objeto_t-tdobject
                  EXCEPTIONS
                    not_found = 1
                    OTHERS    = 2.

              ENDIF.
            ENDIF.

          ENDIF.

        ENDLOOP.

        IF wl_count IS NOT INITIAL.
          MESSAGE s002 WITH wl_count.
        ENDIF.
      ENDIF.

*    WHEN 'SHOW_HIST'.
      "//Implementar




*    WHEN 'REJECT_DOC'.
      "// Botão removido do sistema mediante á solicitação da Luciana Batista
      "// no dia 05.02.2016 - Abap.: Enio Jesus
      "//

    WHEN 'RELEASE'.

      LOOP AT tg_saida INTO wg_saida WHERE mark IS NOT INITIAL.

        SELECT SINGLE * INTO wa_zglt043 FROM zglt043
         WHERE bukrs  EQ wg_saida-bukrs
           AND saknr  EQ wg_saida-saknr
           AND monat  EQ s_mes-low
           AND gjahr  EQ s_ano-low.

        IF sy-subrc IS INITIAL.
          "Somente pode ser libero contas Pendentes
          IF wa_zglt043-status_lib EQ 'P'.
            PERFORM modifica_status USING 'L' wg_saida CHANGING sy-subrc.
            IF sy-subrc IS INITIAL.
              wg_saida-status_lib = icon_green_light.
              MODIFY tg_saida FROM wg_saida TRANSPORTING status_lib.
              ADD 1 TO wl_count.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF wl_count IS NOT INITIAL.
        MESSAGE s004 WITH wl_count.
      ENDIF.

  ENDCASE.
  selfield-refresh = c_x.
  PERFORM limpa_variavel USING c_refresh.
  PERFORM selecionar_dados.
  PERFORM organizacao_dados.
ENDFORM. "XUSER_COMMAND
*---------------------------------------------------------------------*
*       FORM XPF_STATUS_SET                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xpf_status_set USING ucomm TYPE kkblo_t_extab.         "#EC CALLED
  DATA: tl_fcode TYPE TABLE OF sy-ucomm,
        wl_fcode TYPE sy-ucomm.

  DATA: gr_events       TYPE REF TO lcl_event_receiver,
        ls_sel_hide     TYPE slis_sel_hide_alv,
*        REF1              TYPE REF TO CL_GUI_ALV_GRID,
        it_fieldcatalog TYPE lvc_t_fcat,
        wa_fieldcatalog TYPE lvc_s_fcat,
        is_table        TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      es_sel_hide = ls_sel_hide
      e_grid      = ref1.

  CALL METHOD ref1->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcatalog.

  LOOP AT it_fieldcatalog INTO wa_fieldcatalog
    WHERE fieldname EQ 'CRITERIO'.

    wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
    MODIFY it_fieldcatalog FROM wa_fieldcatalog.
  ENDLOOP.

  CALL METHOD ref1->set_frontend_fieldcatalog
    EXPORTING
      it_fieldcatalog = it_fieldcatalog.


  is_table-row = 'X'.
  is_table-col = 'X'.

  CALL METHOD ref1->refresh_table_display
    EXPORTING
      is_stable      = is_table
      i_soft_refresh = 'X'.

  IF init IS INITIAL.
    CALL METHOD ref1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ref1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CREATE OBJECT gr_events.
    SET HANDLER: gr_events->handle_on_button_click FOR ref1,
                 gr_events->on_data_changed FOR ref1.
    init = 'X'.
  ENDIF.

  IF sy-tcode NE 'ZGL026'.
    wl_fcode = 'NEW_DOC'.
    APPEND wl_fcode TO tl_fcode.
    wl_fcode = 'CHG_DOC'.
    APPEND wl_fcode TO tl_fcode.
    wl_fcode = 'DEL_DOC'.
    APPEND wl_fcode TO tl_fcode.
    wl_fcode = 'RELEASE'.
    APPEND wl_fcode TO tl_fcode.
    wl_fcode = 'MOTIVO_NEG'.
    APPEND wl_fcode TO tl_fcode.
    wl_fcode = '&SAVE'.
    APPEND wl_fcode TO tl_fcode.
*   if sy-tcode = zgl037
*    WL_FCODE = 'VIEW_HIST'.
*    APPEND WL_FCODE TO TL_FCODE.
  ENDIF.

  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING tl_fcode.
ENDFORM. "XPF_STATUS_SET
*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.
  DATA: wl_repid    TYPE sy-repid,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE,
        lt_f4       TYPE lvc_t_f4 WITH HEADER LINE,
        fs_sort     TYPE lvc_s_sort,
        gt_sort     TYPE lvc_t_sort,
        ip_mode     TYPE sgs_rwmod.

*  REFRESH: gt_sort.
  wl_repid = sy-repid.
  PERFORM verifica_erros.

  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen    = '100'
      i_show      = space
      i_repid     = sy-repid
      i_popup     = 0
*     i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
      i_set_field = 'X_FIELD'
    IMPORTING
      e_messagem  = wg_mensagem
    TABLES
      it_msgs     = tg_msg_ret.

  "//Caixa de texto;

  IF ( editor IS INITIAL ).
    CREATE OBJECT container
      EXPORTING
        container_name              = 'LONGTEXT'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.

    CHECK sy-subrc IS INITIAL.

    CREATE OBJECT editor
      EXPORTING
        parent                 = container
        wordwrap_mode          = '2'
        wordwrap_position      = '70'
      EXCEPTIONS
        error_cntl_create      = 1
        error_cntl_init        = 2
        error_cntl_link        = 3
        error_dp_create        = 4
        gui_type_not_supported = 5
        OTHERS                 = 6.

    CHECK sy-subrc IS INITIAL.

    DATA: it_texto     TYPE TABLE OF tline WITH HEADER LINE,
          longtext_tab TYPE catsxt_longtext_itab,
          wl_name      LIKE thead-tdname.

    IF ( wg_header-seq IS INITIAL ).
      CONCATENATE wg_header-bukrs wg_header-gjahr wg_header-monat wg_header-saknr INTO wl_name.
    ELSE.
      CONCATENATE wg_header-bukrs wg_header-gjahr wg_header-monat wg_header-saknr wg_header-seq INTO wl_name.
    ENDIF.

    CLEAR: it_texto[], longtext_tab.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'ZREC'
        language                = sy-langu
        name                    = wl_name
        object                  = 'ZRECONCILI'
      TABLES
        lines                   = it_texto
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF ( sy-subrc IS INITIAL ).
      LOOP AT it_texto.
        APPEND it_texto-tdline TO longtext_tab.
      ENDLOOP.

      CALL METHOD editor->set_text_as_r3table
        EXPORTING
          table           = longtext_tab
        EXCEPTIONS
          error_dp        = 1
          error_dp_create = 2
          OTHERS          = 3.
    ENDIF.
  ENDIF.

  IF ( editor IS NOT INITIAL ).
    IF wg_acao EQ c_view_doc.
      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->true.
    ELSE.
      CALL METHOD editor->set_readonly_mode
        EXPORTING
          readonly_mode = editor->false.
    ENDIF.
  ENDIF.

  "// ---

  "//Gerenciador de anexos;

  IF ( manager IS INITIAL ).
    IF wg_acao EQ c_view_doc.
      ip_mode = 'D'.
    ELSE.
      ip_mode = 'E'.
    ENDIF.

    CLEAR obj.
    obj-objtype = objtype.

    IF ( wg_header-seq IS INITIAL ).
      CONCATENATE wg_header-monat wg_header-gjahr wg_header-bukrs wg_header-saknr INTO obj-objkey.
    ELSE.
      CONCATENATE wg_header-monat wg_header-gjahr wg_header-bukrs wg_header-saknr wg_header-seq INTO obj-objkey.
    ENDIF.

    CREATE OBJECT manager
      EXPORTING
        is_object        = obj
        ip_no_commit     = 'R'
        ip_mode          = ip_mode
      EXCEPTIONS
        object_invalid   = 1
        callback_invalid = 2
        OTHERS           = 3.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDIF.

  "// ---

  IF g_custom_container IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_stable-row        = c_x.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'CC_01'.

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
        container = container1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container1.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.
*
**      * Register event handler
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

*    LT_F4-FIELDNAME = 'MATNR'.
*    LT_F4-REGISTER = 'X' .
*    LT_F4-GETBEFORE = 'X' .
**    lt_f4-chngeafter = 'X' .
*    APPEND LT_F4 .

*    WA_LAYOUT-STYLEFNAME = 'STYLE'.
*    WA_LAYOUT-STYLEFNAME = 'STYLE'.

    fs_sort-spos = 1.    "first sorting key
    fs_sort-fieldname = 'SUBTOTALKEY'. "fieldname for sort
    fs_sort-up = 'X'. "sort ascending
    fs_sort-subtot = 'X'. "do subtotal
    fs_sort-no_out = 'X'. "no display
    fs_sort-obligatory = 'X'. "sort is obligatory
    INSERT fs_sort INTO TABLE gt_sort. "insert to sort table

    fs_sort-spos = 2. "second sortign key
    fs_sort-fieldname = 'SORTKEY'. "sort fieldname
    fs_sort-up = 'X'. "sort ascending
    fs_sort-no_out = 'X'. "no display
    INSERT fs_sort INTO TABLE gt_sort. "insert to sort table


    wa_layout-no_totline = c_x.
    wa_layout-no_totexp = c_x.
    wa_layout-no_rowmark  = c_x.
    wa_layout-cwidth_opt  = space.
    wa_layout-grid_title  = TEXT-058. "'Conciliação Relatório Auxiliar'.

    PERFORM montar_layout USING 'TG_REL_AUX'.
**    PERFORM BUILD_DROPDOWN.

    CREATE OBJECT dg_events_receiver.
    SET HANDLER dg_events_receiver->subtotal_text
                  FOR grid1.

    wa_layout-info_fname =  'LINE_COLOR'.
    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_rel_aux[]
        it_sort              = gt_sort[].

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*    CALL METHOD GRID1->REGISTER_F4_FOR_FIELDS
*      EXPORTING
*        IT_F4 = LT_F4[].

    SET HANDLER:
**              LCL_EVENT_RECEIVER=>ON_DOUBLE_CLICK FOR GRID1,
**              LCL_EVENT_RECEIVER=>ON_HOTSPOT_CLICK FOR GRID1,
              lcl_event_receiver=>on_data_changed_finished FOR grid1,
              lcl_event_receiver=>on_data_changed FOR grid1.
**              LCL_EVENT_RECEIVER=>ON_ONF4 FOR GRID1.
*               LCL_EVENT_RECEIVER=>SUBTOTAL_TEXT FOR GRID1.
  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

  IF grid2 IS INITIAL.
    wa_layout-zebra      = c_x.
    wa_layout-no_rowmark = c_x.
    wa_stable-row        = c_x.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = container2.

    CREATE OBJECT grid2
      EXPORTING
        i_parent = container2.

*    CREATE OBJECT OBG_TOOLBAR2
*      EXPORTING
*        IO_ALV_GRID = GRID2.
*
**      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid2.
    SET HANDLER obg_toolbar->handle_user_command_reg FOR grid2.

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

*    LT_F4-FIELDNAME = 'MATNR'.
*    LT_F4-REGISTER = 'X' .
*    LT_F4-GETBEFORE = 'X' .
**    lt_f4-chngeafter = 'X' .
*    APPEND LT_F4 .

*    WA_LAYOUT-STYLEFNAME = 'STYLE'.
*    WA_LAYOUT-STYLEFNAME = 'STYLE'.
    wa_layout-cwidth_opt  = space.
    wa_layout-grid_title  = TEXT-059. "'Ações para regularização das Pendências'.
    CLEAR:  wa_layout-info_fname .
    PERFORM montar_layout USING 'TG_REG_PED'.
**    PERFORM BUILD_DROPDOWN.
    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wa_layout
*       i_save               = 'X'
      CHANGING
        it_fieldcatalog      = t_fieldcatalog[]
        it_outtab            = tg_reg_ped[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
    SET HANDLER: lcl_event_receiver=>on_data_changed2 FOR grid2,
                 lcl_event_receiver=>handle_on_hotspot_click FOR grid2.

  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

  CLEAR: wg_totais-saldo_aux_mi,
         wg_totais-saldo_aux_mi2,
         wg_totais-saldo_aux_mi3.

  LOOP AT tg_rel_aux INTO wg_rel_aux WHERE sortkey NE 'C'.
    ADD wg_rel_aux-valor  TO wg_totais-saldo_aux_mi.
    ADD wg_rel_aux-valor2 TO wg_totais-saldo_aux_mi2.
    ADD wg_rel_aux-valor3 TO wg_totais-saldo_aux_mi3.
**Inicio de alteração - fmartins - 24/01/2023 -  RMNI - IR104343
    IF wg_totais-saldo_aux_mi2 IS NOT INITIAL AND wg_totais-saldo_aux_mi IS NOT INITIAL.
      wg_totais-tx_paridade1 =  wg_totais-saldo_aux_mi / wg_totais-saldo_aux_mi2.
    ENDIF.
**Fim de alteração - fmartins - 24/01/2023 -  RMNI - IR104343
  ENDLOOP.
**Inicio de alteração - fmartins - 22/11/2022 -  RMNI - IR104343
*  IF s_bukrs-low = '0101' AND sy-ucomm = 'NEW_DOC' .
*    wg_totais-saldo_aux_mi =  wg_totais-saldo_aux_mi * 100.
*  ENDIF.
**Fim de alteração - fmartins - 22/11/2022 -  RMNI - IR104343
  wg_totais-dif_mi1 = wg_totais-saldo_mi  - wg_totais-saldo_aux_mi.
  wg_totais-dif_mi2 = wg_totais-saldo_mi2 - wg_totais-saldo_aux_mi2.
  wg_totais-dif_mi3 = wg_totais-saldo_mi3 - wg_totais-saldo_aux_mi3.

ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verifica_erros .
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen      = '100'
*     I_SHOW        = C_X
      i_repid       = sy-repid
      i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
      i_set_field   = 'X_FIELD'
    IMPORTING
      e_messagem    = wg_mensagem
    TABLES
      it_msgs       = tg_msg_ret.
ENDFORM.                    " VERIFICA_ERROS

*&---------------------------------------------------------------------*
*&      Form  montar_estrutura_OO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_1      text
*      -->P_0332   text
*      -->P_0333   text
*      -->P_0334   text
*      -->P_0335   text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM montar_estrutura_oo USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_edit)
                            VALUE(p_hotspot).

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.
  w_fieldcatalog-key           = ' '.
  w_fieldcatalog-hotspot       = p_hotspot.

*  w_fieldcatalog-key_sel       = 'X'.
  IF wg_display IS INITIAL.
    w_fieldcatalog-edit          = p_edit.
  ENDIF.
*  W_FIELDCATALOG-DO_SUM        = P_SUM.

  w_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
*  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  IF p_field EQ 'SUBTOTALKEY'
  OR p_field EQ 'SORTKEY'
  OR p_field EQ 'COUNT'.
    w_fieldcatalog-no_out = c_x.
*    IF P_FIELD EQ 'COUNT'.
*      W_FIELDCATALOG-NO_OUT = SPACE.
*      W_FIELDCATALOG-DO_SUM = C_X.
*      W_FIELDCATALOG-COL_OPT = C_X.
*      W_FIELDCATALOG-NO_ZERO = C_X.
*    ENDIF.
  ENDIF.

  IF p_field EQ 'VALOR'
  OR p_field EQ 'VALOR2'
  OR p_field EQ 'VALOR3'
  OR p_field EQ 'DMBTR'
  OR p_field EQ 'DMBE2'
  OR p_field EQ 'DMBE3'.
    w_fieldcatalog-do_sum  = c_x.
    w_fieldcatalog-col_opt = c_x.
*    W_FIELDCATALOG-NO_ZERO = C_X.
  ENDIF.

  IF p_field EQ 'TIPO'.
    w_fieldcatalog-just = 'C'.
  ENDIF.

  APPEND w_fieldcatalog TO t_fieldcatalog.

ENDFORM.                    " montar_estrutura_OO
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

*  data: values       type vrm_values with header line.
  FIELD-SYMBOLS: <fs_campo> TYPE any.

  IF wg_colaps EQ '@K1@'.
    LOOP AT SCREEN.
      IF screen-group4 EQ 'B1'.
        screen-active    = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name EQ 'TITULO'.
        screen-invisible = 0.
        screen-active = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSEIF wg_t001-land1 IS NOT INITIAL. "EQ 'BR'. Modificação 08.11.2016
    LOOP AT SCREEN.
      IF screen-group3 EQ 'EX'.
        screen-active    = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ( wg_colaps NE '@K1@' ) AND ( wg_header-cta_monet NE 'S' ).
    LOOP AT SCREEN.
      IF screen-group1 EQ 'MO'.
        screen-active    = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF ( wg_header-cta_monet NE 'S' ).
    lb_hist  = TEXT-060. "'Tx. Hist.'.
    lb_hist2 = TEXT-060. "'Tx. Hist.'.
  ELSE.
    lb_hist  = TEXT-061. "'Tx. Pari.'.
    lb_hist2 = TEXT-061. "'Tx. Pari.'.
  ENDIF.
*  PERFORM VALIDA_LAYOUT TABLES T_FIELDCATALOG
*                        USING SY-UNAME.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  PERFORM verifica_erros.
  CALL FUNCTION 'Z_DOC_CHECK_NEW'
    EXPORTING
      i_screen      = '100'
      i_show        = 'X'
      i_repid       = sy-repid
      i_popup       = 0
      i_pressed_tab = 'G_TAB_STRIP-PRESSED_TAB'
      i_set_field   = 'X_FIELD'
      i_set_cell    = 'WG_CELL'
      i_set_obj     = 'WG_OBJ'
    IMPORTING
      e_messagem    = wg_mensagem
    TABLES
      it_msgs       = tg_msg_ret.

  IF x_field IS NOT INITIAL.
    SET CURSOR FIELD x_field."'WG_DESC_OPERACAO'.
  ENDIF.

  IF wg_cell IS NOT INITIAL .
    REFRESH: tg_cell.
    APPEND wg_cell TO tg_cell.
    CALL METHOD grid1->set_selected_cells
      EXPORTING
        it_cells = tg_cell[].
  ENDIF.

ENDMODULE.                 " MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SY_UNAME  text
*      -->P_T_FIELDCATALOG  text
*----------------------------------------------------------------------*
FORM valida_layout  TABLES   tl_fieldcatalog STRUCTURE lvc_s_fcat
                     USING   uname.

  DATA: tl_parametros        TYPE ustyp_t_parameters,
        wl_parametros        TYPE ustyp_parameters,
        wl_fieldcatalog      TYPE lvc_s_fcat,
        wl_variante01        TYPE zvariante01,
        tl_variante02_alv    TYPE TABLE OF zvariante02 WITH HEADER LINE,
        tl_variante02_screen TYPE TABLE OF zvariante02 WITH HEADER LINE,
        wl_tabix             TYPE sy-tabix,
        wl_atributo(30).

  REFRESH: tl_parametros, tl_variante02_alv, tl_variante02_screen.
  FIELD-SYMBOLS: <fs_atributos> TYPE any.

  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = uname
*     WITH_TEXT           =
    TABLES
      user_parameters     = tl_parametros
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
  READ TABLE tl_parametros INTO wl_parametros
    WITH KEY parid = 'ZVARIANTE'.
  IF sy-subrc IS INITIAL.
    SELECT SINGLE *
      FROM zvariante01
      INTO wl_variante01
       WHERE grpva EQ wl_parametros-parva
         AND tcode EQ sy-tcode.

    IF sy-subrc IS INITIAL.
      CONDENSE wl_variante01-grpva NO-GAPS.
      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_alv
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip EQ 'ALV'
           AND dynnr   EQ sy-dynnr.

      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_screen
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip NE 'ALV'
           AND dynnr   EQ sy-dynnr.

    ENDIF.
    IF tl_variante02_screen[] IS NOT INITIAL
    AND ( sy-tcode NE 'SE38'
       AND sy-tcode NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE tl_variante02_screen
          WITH KEY field = screen-name.

        IF sy-subrc IS INITIAL.
          IF ( tl_variante02_screen-acao IS NOT INITIAL
          AND tl_variante02_screen-acao EQ wg_acao )
            OR tl_variante02_screen-acao IS INITIAL.
            UNASSIGN <fs_atributos>.
            CONCATENATE 'SCREEN' tl_variante02_screen-atr_tip INTO wl_atributo SEPARATED BY '-'.
            ASSIGN (wl_atributo) TO <fs_atributos>.
            IF <fs_atributos> IS ASSIGNED.
              <fs_atributos> = tl_variante02_screen-fatr_value.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
    IF tl_variante02_alv[] IS INITIAL
    AND ( sy-tcode EQ 'SE38'
       OR sy-tcode EQ 'SE80' ).
      EXIT.
    ENDIF.
    LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
      wl_tabix = sy-tabix.
      READ TABLE tl_variante02_alv
        WITH KEY field = wl_fieldcatalog-fieldname.
      IF sy-subrc IS NOT  INITIAL.
        IF ( tl_variante02_screen-acao IS NOT INITIAL
            AND tl_variante02_screen-acao EQ wg_acao )
              OR tl_variante02_screen-acao IS INITIAL.
          DELETE tl_fieldcatalog INDEX wl_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSE.
    SELECT SINGLE *
      FROM zvariante01
      INTO wl_variante01
       WHERE default_var EQ c_x
         AND tcode EQ sy-tcode.

    IF sy-subrc IS INITIAL.
      SELECT *
        FROM zvariante02
        INTO TABLE tl_variante02_alv
         WHERE grpva   EQ wl_variante01-grpva
           AND tcode   EQ sy-tcode
           AND atr_tip EQ 'ALV'
           AND dynnr   EQ sy-dynnr.

      SELECT *
         FROM zvariante02
         INTO TABLE tl_variante02_screen
          WHERE grpva   EQ wl_variante01-grpva
            AND tcode   EQ sy-tcode
            AND atr_tip NE 'ALV'
            AND dynnr   EQ sy-dynnr.
    ENDIF.
    IF tl_variante02_screen[] IS NOT INITIAL
        AND ( sy-tcode NE 'SE38'
           AND sy-tcode NE 'SE80' ).
      LOOP AT SCREEN.
        READ TABLE tl_variante02_screen
          WITH KEY field = screen-name.

        IF sy-subrc IS INITIAL.
          IF ( tl_variante02_screen-acao IS NOT INITIAL
            AND tl_variante02_screen-acao EQ wg_acao )
              OR tl_variante02_screen-acao IS INITIAL.
            UNASSIGN <fs_atributos>.
            CONCATENATE 'SCREEN' tl_variante02_screen-atr_tip INTO wl_atributo SEPARATED BY '-'.
            ASSIGN (wl_atributo) TO <fs_atributos>.
            IF <fs_atributos> IS ASSIGNED.
              <fs_atributos> = tl_variante02_screen-fatr_value.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF tl_variante02_alv[] IS INITIAL
    AND ( sy-tcode EQ 'SE38'
       OR sy-tcode EQ 'SE80' ).
      EXIT.
    ENDIF.
    LOOP AT tl_fieldcatalog INTO wl_fieldcatalog.
      wl_tabix = sy-tabix.
      READ TABLE tl_variante02_alv
        WITH KEY field = wl_fieldcatalog-fieldname.
      IF sy-subrc IS NOT  INITIAL.
        IF ( tl_variante02_alv-acao IS NOT INITIAL
            AND tl_variante02_alv-acao EQ wg_acao )
              OR tl_variante02_alv-acao IS INITIAL.
          DELETE tl_fieldcatalog INDEX wl_tabix.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.                    " VALIDA_LAYOUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  DATA: BEGIN OF tl_ucomm OCCURS 0,
          ucomm TYPE  sy-ucomm,
        END OF tl_ucomm.

  REFRESH: tl_ucomm.

  IF wg_acao EQ c_new_doc
  OR wg_acao EQ c_chg_doc.

    gl_tipo_visualiza = 'A'.

    LOOP AT SCREEN.
      IF screen-group2 EQ 'A2'.
        screen-input = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    IF grid1 IS NOT INITIAL.
      CALL METHOD grid1->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.

        IF wg_header-mitkz NE 'K'
       AND wg_header-mitkz NE 'D'.
          IF w_fieldcatalog-fieldname EQ 'DATA'
          OR w_fieldcatalog-fieldname EQ 'CODIGO'
          OR w_fieldcatalog-fieldname EQ 'NAME1'
          OR w_fieldcatalog-fieldname EQ 'VALOR'
          OR w_fieldcatalog-fieldname EQ 'VALOR2'
*          OR W_FIELDCATALOG-FIELDNAME EQ 'TIPO'
          OR w_fieldcatalog-fieldname EQ 'CONTRA'.
            w_fieldcatalog-edit = c_x.
          ENDIF.

        ELSE.
          w_fieldcatalog-edit = space.
        ENDIF.

        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit hotspot.
      ENDLOOP.
      SORT t_fieldcatalog BY col_pos.
      CALL METHOD grid1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

    IF grid2 IS NOT INITIAL.
      CALL METHOD grid2->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.

        IF w_fieldcatalog-fieldname EQ 'DOC_AJUS'.
          w_fieldcatalog-hotspot = abap_false.
        ENDIF.

        IF wg_header-mitkz NE 'K'
       AND wg_header-mitkz NE 'D'.
          IF w_fieldcatalog-fieldname EQ 'BLDAT'
          OR w_fieldcatalog-fieldname EQ 'BELNR'
          OR w_fieldcatalog-fieldname EQ 'CODIGO'
          OR w_fieldcatalog-fieldname EQ 'NAME1'
          OR w_fieldcatalog-fieldname EQ 'DMBTR'
          OR w_fieldcatalog-fieldname EQ 'DMBE2'
          OR w_fieldcatalog-fieldname EQ 'DT_VENC'
          OR w_fieldcatalog-fieldname EQ 'DT_AJUS'
          OR w_fieldcatalog-fieldname EQ 'DOC_AJUS'
          OR w_fieldcatalog-fieldname EQ 'CONTRA'
          OR w_fieldcatalog-fieldname EQ 'OBS'.
            w_fieldcatalog-edit = c_x.
          ELSE.
            w_fieldcatalog-edit = space.
          ENDIF.

        ELSE.
          IF w_fieldcatalog-fieldname EQ 'DT_VENC'
          OR w_fieldcatalog-fieldname EQ 'DT_AJUS'
          OR w_fieldcatalog-fieldname EQ 'DOC_AJUS'
          OR w_fieldcatalog-fieldname EQ 'CONTRA'
          OR w_fieldcatalog-fieldname EQ 'OBS'.
            w_fieldcatalog-edit = c_x.
          ELSE.
            w_fieldcatalog-edit = space.
          ENDIF.
        ENDIF.

        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit hotspot.
      ENDLOOP.
      SORT t_fieldcatalog BY col_pos.
      CALL METHOD grid2->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

  ELSEIF wg_acao EQ c_view_doc.

    gl_tipo_visualiza = 'C'.
    APPEND c_save TO tl_ucomm.

    LOOP AT SCREEN.
      IF screen-group2 EQ 'A2'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

    IF grid1 IS NOT INITIAL.
      CALL METHOD grid1->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.
        w_fieldcatalog-edit = space.

        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit hotspot.
      ENDLOOP.
      SORT t_fieldcatalog BY col_pos.
      CALL METHOD grid1->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.

    IF grid2 IS NOT INITIAL.
      CALL METHOD grid2->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = t_fieldcatalog.

      LOOP AT t_fieldcatalog INTO w_fieldcatalog.

        IF w_fieldcatalog-fieldname EQ 'DOC_AJUS'.
          w_fieldcatalog-hotspot = abap_true.
        ENDIF.

        w_fieldcatalog-edit = space.
        MODIFY t_fieldcatalog FROM w_fieldcatalog TRANSPORTING edit hotspot.
      ENDLOOP.
      SORT t_fieldcatalog BY col_pos.
      CALL METHOD grid2->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = t_fieldcatalog.
    ENDIF.
  ENDIF.

  IF wg_colaps EQ '@K1@'.
    wg_sub01 = c_0051.
  ELSE.
    wg_sub01 = c_0052.
  ENDIF.
  CALL METHOD cl_gui_cfw=>dispatch.

  SET PF-STATUS 'Z002' EXCLUDING tl_ucomm  .
  SET TITLEBAR 'Z001'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CALL METHOD grid1->check_changed_data.
  CALL METHOD grid2->check_changed_data.
  CASE sy-ucomm.
    WHEN c_auxiliar.
      PERFORM visualizar_acoes_pedencias.
    WHEN c_show_msgre.
      PERFORM verifica_erros.
      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen    = '100'
          i_show      = 'X'
          i_repid     = sy-repid
          i_popup     = 0
*         i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
          i_set_field = 'X_FIELD'
          i_set_cell  = 'WG_CELL'
          i_set_obj   = 'WG_OBJ'
        IMPORTING
          e_messagem  = wg_mensagem
        TABLES
          it_msgs     = tg_msg_ret.

    WHEN c_save.
*      CALL METHOD GRID1->CHECK_CHANGED_DATA.
      PERFORM verifica_erros.
      IF tg_msg_ret[] IS INITIAL.
        PERFORM grava_dados.
      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH TEXT-062. "'Há erro no documento.'.
      ENDIF.

      CALL FUNCTION 'Z_DOC_CHECK_NEW'
        EXPORTING
          i_screen    = '100'
          i_show      = 'X'
          i_repid     = sy-repid
          i_popup     = 0
*         i_pressed_tab = 'G_TAB_STRIP_NF-PRESSED_TAB'
          i_set_field = 'X_FIELD'
          i_set_cell  = 'WG_CELL'
          i_set_obj   = 'WG_OBJ'
        IMPORTING
          e_messagem  = wg_mensagem
        TABLES
          it_msgs     = tg_msg_ret.

    WHEN c_col_exp.
      IF wg_colaps EQ '@K1@'.
        wg_colaps = '@K2@'.
      ELSE.
        wg_colaps = '@K1@'.
      ENDIF.

    WHEN c_cancel
      OR c_exit.
*      CALL FUNCTION 'DEQUEUE_EZSDT0040'
*        EXPORTING
*          DOC_SIMULACAO = WG_HEADER-DOC_SIMULACAO.
      wg_exit = c_x.
      CALL METHOD manager->unpublish.
      CLEAR: manager.
      LEAVE TO SCREEN 0.
    WHEN c_print_doc.
      PERFORM print_doc USING wg_043.
    WHEN c_back.
      CALL METHOD manager->unpublish.
      CALL METHOD: editor->free,
                   container->free.
      CLEAR: editor, container.
      CLEAR: manager.
      LEAVE TO SCREEN 0.
  ENDCASE.

  "//Deleta o anexo temporario;
  CONCATENATE wg_header-monat wg_header-gjahr wg_header-bukrs wg_header-saknr
  'TEMPZGL026' INTO ls_ident-objkey.
  ls_ident-objtype = 'ZTEMP'.

  CALL METHOD cl_gos_service_tools=>delete_linked_objects
    EXPORTING
      is_object = ls_ident.

  COMMIT WORK.
ENDMODULE.                 " USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDOS_CONTABIL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WL_SALDO_MI  text
*      <--P_WL_SALDO_MI2  text
*      <--P_WL_SALDO_MI3  text
*----------------------------------------------------------------------*
FORM busca_saldos_contabil  USING wl_saknr
                         CHANGING wl_saldo_mi
                                  wl_saldo_mi2
                                  wl_saldo_mi3.

  DATA: refe1  TYPE hslxx12,
        refe2  TYPE hslxx12,
        refe3  TYPE hslxx12,
        vg_mes TYPE i.

  wl_saldo_mi  = 0.
  wl_saldo_mi2 = 0.
  wl_saldo_mi3 = 0.

  vg_mes = s_mes-low.
  IF vg_mes EQ 12.
    vg_mes = 16.
  ENDIF.

  LOOP AT tg_flext INTO wg_flext WHERE racct = wl_saknr.
    ADD wg_flext-hslvt TO wl_saldo_mi.
    ADD wg_flext-kslvt TO wl_saldo_mi2.
    ADD wg_flext-oslvt TO wl_saldo_mi3.

    DO vg_mes TIMES
      VARYING refe1 FROM wg_flext-hsl01 NEXT wg_flext-hsl02
      VARYING refe2 FROM wg_flext-ksl01 NEXT wg_flext-ksl02
      VARYING refe3 FROM wg_flext-osl01 NEXT wg_flext-osl02.
      ADD refe1 TO wl_saldo_mi.
      ADD refe2 TO wl_saldo_mi2.
      ADD refe3 TO wl_saldo_mi3.
    ENDDO.
  ENDLOOP.

ENDFORM.                    " BUSCA_SALDOS_CONTABIL
*&---------------------------------------------------------------------*
*&      Form  BUSCA_SALDO_CLIENTE_FORNEC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_SAIDA_SAKNR  text
*      <--P_WL_DMBTR  text
*      <--P_WL_DMBE2  text
*----------------------------------------------------------------------*
FORM busca_saldo_cliente_fornec  USING    wl_saknr
                                 CHANGING wl_dmbtr
                                          wl_dmbe2
                                          wl_dmbe3.

  CLEAR:  wl_dmbtr, wl_dmbe2, wl_dmbe3.

  READ TABLE it_bsxk WITH KEY hkont = wl_saknr BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    LOOP AT it_bsxk FROM sy-tabix WHERE hkont NE wl_saknr.
      IF it_bsxk-shkzg EQ 'H'.
        MULTIPLY it_bsxk-dmbtr BY -1.
        MULTIPLY it_bsxk-dmbe2 BY -1.
      ENDIF.
      ADD it_bsxk-dmbtr TO wl_dmbtr.
      ADD it_bsxk-dmbe2 TO wl_dmbe2.
      ADD it_bsxk-dmbe3 TO wl_dmbe3.
    ENDLOOP.
  ENDIF.

  READ TABLE it_bsxd WITH KEY hkont = wl_saknr BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    LOOP AT it_bsxd FROM sy-tabix WHERE hkont NE wl_saknr.
      IF it_bsxd-shkzg EQ 'H'.
        MULTIPLY it_bsxd-dmbtr BY -1.
        MULTIPLY it_bsxd-dmbe2 BY -1.
      ENDIF.
      ADD it_bsxd-dmbtr TO wl_dmbtr.
      ADD it_bsxd-dmbe2 TO wl_dmbe2.
      ADD it_bsxd-dmbe3 TO wl_dmbe3.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " BUSCA_SALDO_CLIENTE_FORNEC
*&---------------------------------------------------------------------*
*&      Form  GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM grava_dados .
  DATA: wl_input_043 TYPE zglt043,
        tl_input_044 TYPE TABLE OF zglt044 WITH HEADER LINE,
        tl_input_045 TYPE TABLE OF zglt045 WITH HEADER LINE,
        tl_tlines    LIKE tline OCCURS 0 WITH HEADER LINE,
        longtext_tab TYPE catsxt_longtext_itab,
        wa_line      TYPE txline,
        wl_header    TYPE thead,
        wl_buzei     TYPE zglt044-buzei,
        lv_seq_log   TYPE num10.

  CLEAR: tl_input_044,
         tl_input_045,
         wl_input_043.

  FIELD-SYMBOLS: <fs_zglt055_log> TYPE zglt055,
                 <fs_zglt066_log> TYPE zglt066,
                 <fs_srgbtbrel>   TYPE srgbtbrel.

  CONCATENATE wg_header-bukrs wg_header-gjahr wg_header-monat wg_header-saknr
  INTO wl_header-tdname.
  wl_header-tdobject = 'ZRECONCILI'.
  wl_header-tdid     = 'ZREC'.
  wl_header-tdspras  = sy-langu.

  IF ( wg_acao = c_chg_doc ).
    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'Z_NUM_RECO'
        quantity    = '1'
      IMPORTING
        number      = lv_seq_log.

    "//Salva uma cópia da caixa de texto
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = wl_header-tdid
        language                = wl_header-tdspras
        name                    = wl_header-tdname
        object                  = wl_header-tdobject
      TABLES
        lines                   = tl_tlines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    CONCATENATE wg_header-bukrs wg_header-gjahr wg_header-monat wg_header-saknr lv_seq_log
    INTO wl_header-tdname.

    IF tl_tlines[] IS NOT INITIAL.
      CALL FUNCTION 'SAVE_TEXT'
        EXPORTING
          header          = wl_header
          insert          = abap_true
          savemode_direct = abap_true
        TABLES
          lines           = tl_tlines
        EXCEPTIONS
          id              = 1
          language        = 2
          name            = 3
          object          = 4
          OTHERS          = 5.
    ENDIF.
    "--

    "//Faz uma cópia dos anexos que foram salvos temporario;
    CONCATENATE wg_header-monat wg_header-gjahr wg_header-bukrs wg_header-saknr
   'TEMPZGL026' INTO ls_source-instid.
    ls_source-typeid  = 'ZTEMP'. "Origem
    ls_source-catid   = 'BO'.

    CONCATENATE wg_header-monat wg_header-gjahr wg_header-bukrs wg_header-saknr lv_seq_log
    INTO ls_target-instid.
    ls_target-typeid  = 'ZGL026'.
    ls_target-catid   = 'BO'.

    CALL METHOD cl_gos_service_tools=>copy_linked_objects
      EXPORTING
        is_source            = ls_source
        is_target            = ls_target
        it_service_selection = gt_services.

    COMMIT WORK.
    "--

    "Salva uma cópia dos registros;
    wa_zglt043-seq      = lv_seq_log.
    wa_zglt043-dt_atual = sy-datum.
    wa_zglt043-hr_atual = sy-uzeit.

    "//Reconciliação Contábil  - Relatório Auxiliar (Logs)
    LOOP AT gt_zglt055_log ASSIGNING <fs_zglt055_log>.
      <fs_zglt055_log>-seq = lv_seq_log.
    ENDLOOP.

    "//Reconciliação Contábil  - Regularização pendências (Logs)
    LOOP AT gt_zglt066_log ASSIGNING <fs_zglt066_log>.
      <fs_zglt066_log>-seq = lv_seq_log.
    ENDLOOP.

    INSERT zglt055 FROM TABLE gt_zglt055_log.
    INSERT zglt066 FROM TABLE gt_zglt066_log.

    MOVE-CORRESPONDING wa_zglt043 TO wg_header_log.
    wg_header_log-responsavel = sy-uname.

    INSERT zglt075 FROM wg_header_log.
    "// --

  ENDIF.
  "// __

  PERFORM atribuir_valores_043 CHANGING wl_input_043.
  MODIFY TABLE tg_043 FROM wl_input_043.

  "//Obtem novo texto para salvar;
  CALL METHOD editor->get_text_as_r3table
    IMPORTING
      table           = longtext_tab
    EXCEPTIONS
      error_dp        = 1
      error_dp_create = 2
      OTHERS          = 3.

  CLEAR: tl_tlines[].

  CONCATENATE wg_header-bukrs wg_header-gjahr wg_header-monat wg_header-saknr
  INTO wl_header-tdname.

  LOOP AT longtext_tab INTO wa_line.
    tl_tlines-tdline = wa_line.
    APPEND tl_tlines.
  ENDLOOP.

  IF ( tl_tlines[] IS NOT INITIAL ).
    CALL FUNCTION 'SAVE_TEXT'
      EXPORTING
        header          = wl_header
        insert          = abap_true
        savemode_direct = abap_true
      TABLES
        lines           = tl_tlines
      EXCEPTIONS
        id              = 1
        language        = 2
        name            = 3
        object          = 4
        OTHERS          = 5.
  ENDIF.
  "//___

  DELETE tg_rel_aux WHERE data   IS INITIAL
                      AND valor  IS INITIAL
                      AND valor2 IS INITIAL
                      AND contra IS INITIAL
                      AND codigo IS INITIAL
                      AND name1  IS INITIAL.

  PERFORM atribuir_valores_044 TABLES tl_input_044.
  MODIFY TABLE tg_044 FROM tl_input_044.

  DELETE tg_reg_ped WHERE bldat    IS INITIAL
                      AND dmbtr    IS INITIAL
                      AND dmbe2    IS INITIAL
                      AND codigo   IS INITIAL
                      AND name1    IS INITIAL
                      AND dt_venc  IS INITIAL
                      AND dt_ajus  IS INITIAL
                      AND doc_ajus IS INITIAL
                      AND contra   IS INITIAL
                      AND obs      IS INITIAL.

  PERFORM atribuir_valores_045 TABLES tl_input_045.
  MODIFY TABLE tg_045 FROM tl_input_045.

  DELETE FROM zglt043 WHERE monat EQ wl_input_043-monat
                        AND gjahr EQ wl_input_043-gjahr
                        AND bukrs EQ wl_input_043-bukrs
                        AND saknr EQ wl_input_043-saknr.

  DELETE FROM zglt044 WHERE monat EQ wl_input_043-monat
                        AND gjahr EQ wl_input_043-gjahr
                        AND bukrs EQ wl_input_043-bukrs
                        AND saknr EQ wl_input_043-saknr.

  DELETE FROM zglt045 WHERE monat EQ wl_input_043-monat
                        AND gjahr EQ wl_input_043-gjahr
                        AND bukrs EQ wl_input_043-bukrs
                        AND saknr EQ wl_input_043-saknr.

  MODIFY zglt043 FROM wl_input_043.
  MODIFY zglt044 FROM TABLE tl_input_044.
  MODIFY zglt045 FROM TABLE tl_input_045.

  MESSAGE s005.
  wg_saida-status_lib = icon_yellow_light.

  PERFORM limpa_variavel USING c_atual.
  wg_acao = c_view_doc.
  LEAVE TO SCREEN 100.

ENDFORM.                    " GRAVA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_variavel USING p_acao.

  IF p_acao EQ c_refresh.
    REFRESH: tg_ska1, tg_skat, tg_skb1, tg_039, tg_040, tg_usrefus, tg_depto,
             tg_041, tg_042, tg_flext, tg_lfa1, tg_kna1, tg_043, tg_044,
             tg_045, tg_saida, tg_rel_aux, tg_reg_ped, tg_texto, tg_aux.

    CLEAR: wg_header, wg_totais, wg_tcurr_1, wg_tcurr_2, wg_ska1, wg_skat, wg_skb1, wg_t001, wg_039,
           wg_040, wg_usrefus, wg_depto, wg_041, wg_042, wg_flext, wg_lfa1,
           wg_kna1, wg_043, wg_044, wg_045, wg_rel_aux, wg_reg_ped, wa_aux, vg_last_day,
           vg_last_day_aux, wg_dif_conf, wg_dif_conf2, wg_tcurr_1_fec, wg_tcurr_2_fec.
  ENDIF.
ENDFORM.                    " LIMPA_VARIAVEL
*&---------------------------------------------------------------------*
*&      Form  MODIFICA_STATUS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_STATUS   text
*----------------------------------------------------------------------*
FORM modifica_status  USING  p_status
                             p_saida TYPE ty_saida
                    CHANGING rc.
  DATA : wl_043 TYPE zglt043.

*   Não Iniciado
*P  Aguardando liberação
*L  Liberado
*A  Aprovado
*R  Rejeitado

  SELECT SINGLE *
    FROM zglt043
     INTO wl_043
     WHERE bukrs EQ p_saida-bukrs
       AND saknr EQ p_saida-saknr
       AND monat EQ s_mes-low
       AND gjahr EQ s_ano-low.

  IF sy-subrc IS INITIAL.
    UPDATE zglt043 SET status_lib = p_status
                   WHERE bukrs EQ p_saida-bukrs
                     AND saknr EQ p_saida-saknr
                     AND monat EQ s_mes-low
                     AND gjahr EQ s_ano-low.
    IF sy-subrc IS INITIAL.
      rc = sy-subrc.

      IF wl_043-status_lib EQ 'P' AND p_status = 'L'.
        PERFORM registar_log_liberacao USING wl_043 'L'.
      ENDIF.

      IF wl_043-status_lib EQ 'L' AND p_status = 'P'.
        PERFORM registar_log_liberacao USING wl_043 'R'.
      ENDIF.

      "Reconciliação Liberada
      IF p_status EQ 'L'.
        PERFORM envia_email USING wl_043.
      ENDIF.

    ENDIF.

  ENDIF.
ENDFORM.                    " MODIFICA_STATUS
*&---------------------------------------------------------------------*
*&      Form  PRINT_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WG_043  text
*----------------------------------------------------------------------*
FORM print_doc  USING    wl_043 TYPE zglt043.

  DATA: vl_form      TYPE tdsfname,
        vl_name      TYPE rs38l_fnam,
        i_zglt043    TYPE zglt043,
        it_zglt044   TYPE TABLE OF zglt044 WITH HEADER LINE,
        i_zglt044    TYPE zglt044_t,
        it_zglt045   TYPE TABLE OF zglt045 WITH HEADER LINE,
        i_zglt045    TYPE zglt045_t,
        longtext_tab TYPE catsxt_longtext_itab,
        tl_tlines    LIKE tline OCCURS 0 WITH HEADER LINE,
        wa_line      LIKE tline,
        i_gravado(1).

  vl_form = 'ZGLS0001'.

  IF gl_tipo_visualiza NE 'C'.

    PERFORM atribuir_valores_043 CHANGING i_zglt043.

    PERFORM atribuir_valores_044 TABLES it_zglt044.

    PERFORM atribuir_valores_045 TABLES it_zglt045.

    wl_043-bukrs = i_zglt043-bukrs.
    wl_043-saknr = i_zglt043-saknr.
    wl_043-monat = i_zglt043-monat.
    wl_043-gjahr = i_zglt043-gjahr.

    IF editor IS NOT INITIAL.
      CALL METHOD editor->get_text_as_r3table
        IMPORTING
          table           = longtext_tab
        EXCEPTIONS
          error_dp        = 1
          error_dp_create = 2
          OTHERS          = 3.

      CLEAR: tl_tlines[].

      LOOP AT longtext_tab INTO wa_line.
        tl_tlines-tdline = wa_line.
        APPEND tl_tlines.
      ENDLOOP.
    ENDIF.

    i_gravado = 'N'.
  ELSE.
    i_gravado = 'S'.
  ENDIF.

  LOOP AT it_zglt044.
    APPEND it_zglt044 TO i_zglt044.
  ENDLOOP.

  LOOP AT it_zglt045.
    APPEND it_zglt045 TO i_zglt045.
  ENDLOOP.

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

  CALL FUNCTION vl_name
    EXPORTING
      i_bukrs          = wl_043-bukrs
      i_saknr          = wl_043-saknr
      i_monat          = wl_043-monat
      i_gjahr          = wl_043-gjahr
      i_zglt043        = i_zglt043
      i_gravado        = i_gravado
    TABLES
      i_zglt044        = i_zglt044
      i_zglt045        = i_zglt045
      i_tlines         = tl_tlines
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.                    " PRINT_DOC
*
**&---------------------------------------------------------------------*
**&      Form  VERIFICA_RAZAO_ESPECIAL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*FORM VERIFICA_RAZAO_ESPECIAL  TABLES P_T074 STRUCTURE T074
*                               USING P_CONTA     TYPE SAKNR
*                                     P_RAZAO_ESP TYPE CHAR01.
*
*  DATA: VG_KTOPL TYPE KTOPL.
*
*  SELECT SINGLE KTOPL
*    INTO VG_KTOPL
*    FROM T001
*   WHERE BUKRS EQ S_BUKRS-LOW.
*
*  CLEAR: P_T074.
*
*  SELECT *
*    INTO TABLE P_T074
*    FROM T074
*   WHERE KTOPL EQ VG_KTOPL
*     AND SKONT EQ P_CONTA.
*
*  IF SY-SUBRC IS INITIAL.
*    P_RAZAO_ESP = ABAP_TRUE.
*  ENDIF.
*
*ENDFORM.                    " VERIFICA_RAZAO_ESPECIAL

*&---------------------------------------------------------------------*
*&      Form  ATRIBUIR_VALORES_043
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_WL_INPUT_043  text
*----------------------------------------------------------------------*
FORM atribuir_valores_043 CHANGING p_043 TYPE zglt043.

  p_043-monat            = wg_header-monat.
  p_043-gjahr            = wg_header-gjahr.
  p_043-bukrs            = wg_header-bukrs.
  p_043-saknr            = wg_header-saknr.
  p_043-sdo_mi           = wg_totais-saldo_mi.
  p_043-sdo_mi2          = wg_totais-saldo_mi2.
  p_043-sdo_mi3          = wg_totais-saldo_mi3.
  p_043-sdo_rel_aux_mi   = wg_totais-saldo_aux_mi.
  p_043-sdo_rel_aux_mi2  = wg_totais-saldo_aux_mi2.
  p_043-sdo_rel_aux_mi3  = wg_totais-saldo_aux_mi3.
  p_043-tx_fech1         = wg_totais-tx_fech1.
  p_043-tx_fech2         = wg_totais-tx_fech2.
  p_043-fonte_infor      = wg_header-fonte_infor.
  "Status de Pendente de Liberação
  p_043-status_lib       = c_p.
  p_043-dt_atual         = sy-datum.
  p_043-hr_atual         = sy-uzeit.

ENDFORM.                    " ATRIBUIR_VALORES_043

*&---------------------------------------------------------------------*
*&      Form  ATRIBUIR_VALORES_044
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TL_INPUT_044  text
*----------------------------------------------------------------------*
FORM atribuir_valores_044 TABLES p_044 STRUCTURE zglt044.

  DATA: wa_zglt044 TYPE zglt044,
        wl_buzei   TYPE zglt044-buzei.

  CLEAR: wl_buzei.

  LOOP AT tg_rel_aux INTO wg_rel_aux.

    IF wg_rel_aux-data   IS INITIAL AND
       wg_rel_aux-valor  IS INITIAL AND
       wg_rel_aux-valor2 IS INITIAL AND
       wg_rel_aux-contra IS INITIAL AND
       wg_rel_aux-codigo IS INITIAL AND
       wg_rel_aux-name1  IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR: wa_zglt044.

    ADD 1 TO wl_buzei.

    wa_zglt044-monat            =  wg_header-monat.
    wa_zglt044-gjahr            =  wg_header-gjahr.
    wa_zglt044-bukrs            =  wg_header-bukrs.
    wa_zglt044-saknr            =  wg_header-saknr.
    wa_zglt044-buzei            =  wl_buzei.
    wa_zglt044-bldat            =  wg_rel_aux-data.
    wa_zglt044-dmbtr            =  wg_rel_aux-valor.
    wa_zglt044-dmbe2            =  wg_rel_aux-valor2.
    wa_zglt044-dmbe3            =  wg_rel_aux-valor3.
    wa_zglt044-sortkey          =  wg_rel_aux-sortkey.
    wa_zglt044-cta_contra_part  =  wg_rel_aux-contra.

    IF wg_rel_aux-sortkey NE 'C'.
      IF wg_header-mitkz EQ 'K'.
        wa_zglt044-lifnr           =  wg_rel_aux-codigo.
      ELSEIF wg_header-mitkz EQ 'D'.
        wa_zglt044-kunnr           =  wg_rel_aux-codigo.
      ELSE.
        wa_zglt044-cod             = wg_rel_aux-codigo.
        wa_zglt044-descr           = wg_rel_aux-name1.
      ENDIF.
    ENDIF.

    APPEND wa_zglt044 TO p_044.
    CLEAR: wa_zglt044.
  ENDLOOP.
ENDFORM.                    " ATRIBUIR_VALORES_044

*&---------------------------------------------------------------------*
*&      Form  ATRIBUIR_VALORES_045
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_ZGLT045  text
*----------------------------------------------------------------------*
FORM atribuir_valores_045 TABLES p_45 STRUCTURE zglt045.

  DATA: wa_zglt045 TYPE zglt045,
        wl_buzei   TYPE zglt045-buzei.

  CLEAR: wl_buzei.
  LOOP AT tg_reg_ped INTO wg_reg_ped.

    IF wg_reg_ped-bldat    IS INITIAL AND
       wg_reg_ped-dmbtr    IS INITIAL AND
       wg_reg_ped-dmbe2    IS INITIAL AND
       wg_reg_ped-codigo   IS INITIAL AND
       wg_reg_ped-name1    IS INITIAL AND
       wg_reg_ped-dt_venc  IS INITIAL AND
       wg_reg_ped-dt_ajus  IS INITIAL AND
       wg_reg_ped-doc_ajus IS INITIAL AND
       wg_reg_ped-obs      IS INITIAL AND
       wg_reg_ped-contra   IS INITIAL.
      CONTINUE.
    ENDIF.

    CLEAR: wa_zglt045.

    ADD 1 TO wl_buzei.

    wa_zglt045-monat            =  wg_header-monat.
    wa_zglt045-gjahr            =  wg_header-gjahr.
    wa_zglt045-bukrs            =  wg_header-bukrs.
    wa_zglt045-saknr            =  wg_header-saknr.
    wa_zglt045-belnr            =  wg_reg_ped-belnr.
    wa_zglt045-buzei            =  wl_buzei.
    wa_zglt045-buzei2           =  wg_reg_ped-buzei.    "------------*01.03.2017 LG
    wa_zglt045-bldat            =  wg_reg_ped-bldat.
    wa_zglt045-dmbtr            =  wg_reg_ped-dmbtr.
    wa_zglt045-dmbe2            =  wg_reg_ped-dmbe2.
    wa_zglt045-dmbe3            =  wg_reg_ped-dmbe3.
    wa_zglt045-dt_vcto          =  wg_reg_ped-dt_venc.
    wa_zglt045-dt_ajuste        =  wg_reg_ped-dt_ajus.
    wa_zglt045-nro_doc_aj       =  wg_reg_ped-doc_ajus.
    wa_zglt045-observ           =  wg_reg_ped-obs.
    wa_zglt045-cta_contra_part  =  wg_reg_ped-contra.

    IF wg_header-mitkz EQ 'K'.
      wa_zglt045-lifnr           =  wg_reg_ped-codigo.
    ELSEIF wg_header-mitkz EQ 'D'.
      wa_zglt045-kunnr           =  wg_reg_ped-codigo.
    ELSE.
      wa_zglt045-cod             = wg_reg_ped-codigo.
      wa_zglt045-descr           = wg_reg_ped-name1.
    ENDIF.
    APPEND wa_zglt045 TO p_45.
    CLEAR: wa_zglt045.
  ENDLOOP.

ENDFORM.                    " ATRIBUIR_VALORES_045

*&---------------------------------------------------------------------*
*&      Form  ENVIA_EMAIL
*&---------------------------------------------------------------------*
FORM envia_email  USING p_043 TYPE zglt043.

  DATA: it_zglt058 TYPE TABLE OF zglt058 WITH HEADER LINE,
        wa_zglt041 TYPE zglt041,
        lc_nivel   TYPE ze_nivel.

  SELECT SINGLE * INTO wa_zglt041
    FROM zglt041
   WHERE bukrs EQ p_043-bukrs
     AND saknr EQ p_043-saknr.
*     AND GJAHR EQ P_043-GJAHR. "/Modificação CS2017000372

  IF sy-subrc IS INITIAL.
    SELECT *
      INTO TABLE it_zglt058
      FROM zglt058
     WHERE bukrs    EQ wa_zglt041-bukrs
       AND dep_resp EQ wa_zglt041-dep_resp2
     ORDER BY nivel.

    IF sy-subrc IS INITIAL.
      READ TABLE it_zglt058 INDEX 1.
      lc_nivel = it_zglt058-nivel.
      LOOP AT it_zglt058 WHERE nivel EQ lc_nivel.
        PERFORM enviar_email_usuario USING p_043 wa_zglt041 it_zglt058.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                    " ENVIA_EMAIL

*&---------------------------------------------------------------------*
*&      Form  ENVIAR_EMAIL_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_043  text
*      -->P_WA_ZGLT041  text
*      -->P_IT_ZGLT058  text
*----------------------------------------------------------------------*
FORM enviar_email_usuario  USING p_043 TYPE zglt043
                                 p_041 TYPE zglt041
                                 p_058 TYPE zglt058.

* Objetos para enviar email
  DATA: objpack           LIKE sopcklsti1 OCCURS  2 WITH HEADER LINE,
        objhead           LIKE solisti1   OCCURS  1 WITH HEADER LINE,
        objbin_ord        LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin_log        LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin            LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        bjtxt             LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        reclist           LIKE somlreci1  OCCURS  5 WITH HEADER LINE,
        content_hex       TYPE STANDARD TABLE OF solix WITH HEADER LINE,
        it_shortcut_param LIKE zst_shortcut_par OCCURS 0 WITH HEADER LINE,
        objtxt            LIKE solisti1   OCCURS 10 WITH HEADER LINE,
        objbin_ann        TYPE solisti1,
        objbin1           TYPE soli_tab, "   OCCURS 10 WITH HEADER LINE.
        wa_objbin         LIKE LINE OF objbin,
        doc_chng          LIKE sodocchgi1,
        tab_lines         LIKE sy-tabix,
        l_anex            TYPE string,
        l_leng            TYPE i,
        l_arq             TYPE string,
        l_tam             TYPE i,
        l_tam_ord         TYPE i,
        l_tam_log         TYPE i,
        l_email(300)      TYPE c,
        vlinha            TYPE i,
        vuser             TYPE sy-uname,
        content           TYPE string,
        wa_skat           TYPE skat,
        wa_t001           TYPE t001,
        wa_zimp_cad_depto TYPE zimp_cad_depto,
        e_x001            TYPE x001.

  DATA: bsmtp_addr TYPE adr6-smtp_addr.

  SELECT SINGLE adr6~smtp_addr INTO bsmtp_addr
    FROM usr21
   INNER JOIN adr6 ON  usr21~addrnumber = adr6~addrnumber AND usr21~persnumber = adr6~persnumber
   WHERE usr21~bname = p_058-bname.

* Criação do documento de Email
  doc_chng-obj_name = 'LOG_RECONC'.

* Assunto do Email
  doc_chng-obj_descr = TEXT-063. "'Liberação de Reconciliação'.

  SELECT SINGLE * INTO wa_t001
    FROM t001
   WHERE bukrs EQ p_043-bukrs.

  SELECT SINGLE * INTO wa_skat
    FROM skat
   WHERE spras EQ sy-langu
     AND saknr EQ p_043-saknr
     AND ktopl EQ wa_t001-ktopl.

  SELECT SINGLE * INTO wa_zimp_cad_depto
    FROM zimp_cad_depto
   WHERE dep_resp EQ p_041-dep_resp2.

* Texto
  objtxt-line = TEXT-064. "'Está disponível para aprovação no sistema SAP a Conta de Reconciliação'.
  APPEND objtxt.

  objtxt-line = ''. APPEND objtxt.
  objtxt-line = TEXT-065. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CONCATENATE TEXT-066 p_043-bukrs '-' wa_t001-butxt INTO objtxt-line SEPARATED BY space. "'Empresa:'
  APPEND objtxt.
  CONCATENATE TEXT-067 wa_zimp_cad_depto-dep_resp '-' wa_zimp_cad_depto-dep_resp_desc INTO objtxt-line SEPARATED BY space. "'Departamento:'
  APPEND objtxt.
  CONCATENATE TEXT-068 p_043-saknr '-' wa_skat-txt50 INTO objtxt-line SEPARATED BY space. "'Conta:'
  APPEND objtxt.
  CONCATENATE p_043-monat '-' p_043-gjahr INTO objtxt-line.
  CONCATENATE TEXT-069 objtxt-line INTO objtxt-line SEPARATED BY space. "'Mês-Ano:'
  APPEND objtxt.

  CALL FUNCTION 'FI_CURRENCY_INFORMATION'
    EXPORTING
      i_bukrs                = p_043-bukrs
    IMPORTING
      e_x001                 = e_x001
    EXCEPTIONS
      currency_2_not_defined = 1
      currency_3_not_defined = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  DATA: ctotal(20),
        vdata(10).

  "1ª Moeda da Empresa
  WRITE p_043-sdo_mi TO ctotal CURRENCY wa_t001-waers.
  CONDENSE ctotal NO-GAPS.
  CONCATENATE TEXT-070 wa_t001-waers ctotal INTO objtxt-line SEPARATED BY space. "'1ª Moeda: '
  APPEND objtxt.

  "2ª Moeda da Empresa
  WRITE p_043-sdo_mi2 TO ctotal CURRENCY e_x001-hwae2.
  CONDENSE ctotal NO-GAPS.
  CONCATENATE TEXT-071 e_x001-hwae2 ctotal INTO objtxt-line SEPARATED BY space. "'2ª Moeda: '
  APPEND objtxt.

  "Somente 3º moeda em paises diferente de Brasil
  IF wa_t001-land1 NE 'BR'.
    "3ª Moeda da Empresa
    WRITE p_043-sdo_mi3 TO ctotal CURRENCY e_x001-hwae3.
    CONDENSE ctotal NO-GAPS.
    CONCATENATE TEXT-072 e_x001-hwae3 ctotal INTO objtxt-line SEPARATED BY space. "'3ª Moeda: '
    APPEND objtxt.
  ENDIF.
  objtxt-line = TEXT-065. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.

  objtxt-line = ''. APPEND objtxt.
  objtxt-line = TEXT-073. "'Para aprovar clique no link "Estratégia" em anexo.' .
  APPEND objtxt.
  objtxt-line = TEXT-065. "'-------------------------------------------------------------------------------------------------------' .
  APPEND objtxt.
  CLEAR objtxt.

  objtxt-line = TEXT-074. "'Para APROVAÇÃO/RECUSA da Reconciliação Acesse Transação: ZGL037' .
  APPEND objtxt.
  objtxt-line = ''. APPEND objtxt.

* Setar tamanho da mensagem
  DESCRIBE TABLE objtxt LINES tab_lines.
  READ TABLE objtxt INDEX tab_lines.
  doc_chng-doc_size = ( tab_lines - 1 ) * 255 + strlen( objtxt ).

* Criar entrada de documento comprimido
  CLEAR objpack-transf_bin.
  objpack-head_start = 1.
  objpack-head_num   = 0.
  objpack-body_start = 1.
  objpack-body_num   = tab_lines.
  objpack-doc_type   = 'RAW'.
  APPEND objpack.

  CALL FUNCTION 'ZFM_CREATE_SHORTCUT'
    EXPORTING
      recipient_user_id = p_058-bname
      transaction       = 'ZGL037'
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
    MESSAGE TEXT-075 TYPE 'I'. "'O aprovador seguinte não tem e-mail cadastrado, por favor contacte a T.I.'
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
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      OTHERS                     = 8.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  sy-uname = vuser.

ENDFORM.                    " ENVIAR_EMAIL_USUARIO

*&---------------------------------------------------------------------*
*&      Form  REGISTAR_LOG_LIBERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WL_043  text
*----------------------------------------------------------------------*
FORM registar_log_liberacao  USING p_043  TYPE zglt043
                                   status TYPE zfied022.
  DATA: wa_zglt059 TYPE zglt059,
        wa_zglt041 TYPE zglt041.

  SELECT SINGLE * INTO wa_zglt041
    FROM zglt041
   WHERE bukrs EQ p_043-bukrs
     AND saknr EQ p_043-saknr.
*     AND GJAHR EQ P_043-GJAHR. "/Modificação CS2017000372.

  CLEAR: wa_zglt059.
  wa_zglt059-bukrs         = p_043-bukrs.
  wa_zglt059-saknr         = p_043-saknr.
  wa_zglt059-monat         = p_043-monat.
  wa_zglt059-gjahr         = p_043-gjahr.
  wa_zglt059-dep_resp      = wa_zglt041-dep_resp2.
  wa_zglt059-dt_liberacao  = sy-datum.
  wa_zglt059-hr_liberacao  = sy-uzeit.
  wa_zglt059-bn_liberacao  = sy-uname.
  wa_zglt059-status_lib    = status.
  wa_zglt059-ck_ultimo_log = 'S'.
  IF status EQ 'L'.
    wa_zglt059-ck_recusa  = 'N'.
  ELSEIF status EQ 'R'.
    wa_zglt059-ck_recusa  = 'S'.
  ENDIF.

  UPDATE zglt059
     SET ck_ultimo_log = 'N'
   WHERE bukrs EQ p_043-bukrs
     AND saknr EQ p_043-saknr
     AND monat EQ p_043-monat
     AND gjahr EQ p_043-gjahr.

  MODIFY zglt059 FROM wa_zglt059.

ENDFORM.                    " REGISTAR_LOG_LIBERACAO

*&---------------------------------------------------------------------*
*&      Form  VERIFICA_NIVEL_LIBERACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZGLT043  text
*      <--P_SY_SUBRC  text
*----------------------------------------------------------------------*
FORM verifica_nivel_liberacao  USING    p_043 TYPE zglt043
                               CHANGING rc    TYPE sysubrc.

  DATA: wa_zglt059 TYPE zglt059,
        wa_zglt041 TYPE zglt041,
        it_zglt058 TYPE TABLE OF zglt058 WITH HEADER LINE.

  SELECT SINGLE * INTO wa_zglt059
    FROM zglt059
   WHERE bukrs EQ p_043-bukrs
     AND saknr EQ p_043-saknr
     AND monat EQ p_043-monat
     AND gjahr EQ p_043-gjahr
     AND ck_ultimo_log EQ 'S'.

  IF rc IS INITIAL.
    IF wa_zglt059-status_lib EQ 'A'.
      "Aprovado Ultimo Nível -- Não é permitido alteração
      rc = 1.
      MESSAGE s008 WITH p_043-saknr wa_zglt059-bn_liberacao.
    ELSEIF wa_zglt059-status_lib EQ 'L' AND wa_zglt059-nivel IS INITIAL.
      "Liberado por conferência
      CLEAR: rc.
    ELSEIF wa_zglt059-status_lib EQ 'L' AND wa_zglt059-nivel IS NOT INITIAL AND wa_zglt059-ck_recusa = 'N'.
      "Liberado/Aprovado por Nivel Intermediário -- Não é permitido alteração
      rc = 1.
      MESSAGE s009 WITH p_043-saknr wa_zglt059-bn_liberacao.
    ELSEIF wa_zglt059-status_lib EQ 'L' AND wa_zglt059-nivel IS NOT INITIAL AND wa_zglt059-ck_recusa = 'S'.

      SELECT SINGLE * INTO wa_zglt041
        FROM zglt041
       WHERE bukrs EQ p_043-bukrs
         AND saknr EQ p_043-saknr.
*         AND GJAHR EQ P_043-GJAHR. "/Modificação CS2017000372.

      SELECT * INTO TABLE it_zglt058
        FROM zglt058
       WHERE bukrs    = p_043-bukrs
         AND dep_resp = wa_zglt041-dep_resp2
       ORDER BY nivel.

      IF sy-subrc IS INITIAL.
        READ TABLE it_zglt058 INDEX 1.
        IF it_zglt058-nivel NE wa_zglt059-nivel.
          "Recusado por Nível Intermediário de Aprovação/Liberação - Não Permitido Ajuste
          "Deve ser recusado por nível anterior
          rc = 1.
          MESSAGE s010 WITH p_043-saknr wa_zglt059-bn_liberacao.
        ELSE.
          "Recusado por Primeiro Nível de Aprovação/Liberação - Permitido Ajuste
          CLEAR: rc.
        ENDIF.
      ELSE.
        "Não tem Nivel de Liberação Cadastrado
        CLEAR: rc.
      ENDIF.

    ENDIF.
  ELSE.
    CLEAR: rc.
  ENDIF.

ENDFORM.                    " VERIFICA_NIVEL_LIBERACAO

*&---------------------------------------------------------------------*
*&      Form  VISUALIZAR_ACOES_PEDENCIAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM visualizar_acoes_pedencias .

  CALL SCREEN 0101 STARTING AT 04 05.

ENDFORM.                    " VISUALIZAR_ACOES_PEDENCIAS

DATA: container_0101  TYPE REF TO cl_gui_custom_container,
      alv_0101        TYPE REF TO cl_gui_alv_grid,
      gs_layout_0101  TYPE lvc_s_layo,
      is_variant_0101 TYPE disvariant,
      es_row_no_0101  TYPE lvc_s_roid,
      es_row_inf_0101 TYPE lvc_s_row,
      es_col_inf_0101 TYPE lvc_s_col.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.

  DATA: wa_layout_alv TYPE slis_layout_alv,
        p_scrtext_1   TYPE dd03p-scrtext_l,
        p_scrtext_2   TYPE dd03p-scrtext_l,
        p_scrtext_3   TYPE dd03p-scrtext_l.

  wa_layout_alv-zebra = c_x.

  SET PF-STATUS 'PFDOCS'.
  SET TITLEBAR  'TLDOCS'.

  IF container_0101 IS INITIAL.

    CREATE OBJECT container_0101
      EXPORTING
        container_name = 'ALV_0101'.

    CREATE OBJECT alv_0101
      EXPORTING
        i_parent = container_0101.

    p_scrtext_1 = wg_header-waers.
    p_scrtext_2 = wg_header-waer2.
    p_scrtext_3 = wg_header-waer3.

    CLEAR: gs_layout_0101.
    gs_layout_0101-zebra      = abap_true.
    gs_layout_0101-sel_mode   = 'A'.

    is_variant_0101-report = sy-repid.
    is_variant_0101-handle = '0101'.

    CLEAR: t_fieldcatalog.

    PERFORM montar_estrutura_oo USING:
           1  'BSIK'               'BLDAT'                'TG_REG_PED' 'BLDAT'                   TEXT-036      '10'         abap_false abap_false, "'Dt.Documento'
           2  'BSIK'               'BELNR'                'TG_REG_PED' 'BELNR'                   TEXT-037      '15'         abap_false abap_false, "'Nro.Documento'
           3  'KNA1'               'KUNNR'                'TG_REG_PED' 'CODIGO'                  TEXT-038      '15'         abap_false abap_false, "'Código'
           4  'KNA1'               'NAME1'                'TG_REG_PED' 'NAME1'                   TEXT-039      '20'         abap_false abap_false, "'Descrição'
           5  'FAGLFLEXT'          'HSLVT'                'TG_REG_PED' 'DMBTR'                   p_scrtext_1   '15'         abap_false abap_false,
           6  'FAGLFLEXT'          'HSLVT'                'TG_REG_PED' 'DMBE2'                   p_scrtext_2   '15'         abap_false abap_false.

*    IF WG_T001-LAND1 NE 'BR'.
*      PERFORM MONTAR_ESTRUTURA_OO USING:
*             7  'FAGLFLEXT'          'HSLVT'                'TG_REG_PED' 'DMBE3'                   P_SCRTEXT_3         '15'       ABAP_FALSE ABAP_FALSE.
*    ENDIF.
    PERFORM montar_estrutura_oo USING:
           8  ' '                  ' '                    'TG_REG_PED' 'TIPO'                    TEXT-040      ' '          abap_false abap_false, "'D/C'
           9  'BSIK'               'BLDAT'                'TG_REG_PED' 'DT_AJUS'                 TEXT-041      '10'         abap_false abap_false, "'Dt.Ajuste'
          10  'BSIK'               'BELNR'                'TG_REG_PED' 'DOC_AJUS'                TEXT-042      '13'         abap_false abap_false, "'Doc.Ajuste'
          11  'ZGLT045'            'CTA_CONTRA_PART'      'TG_REG_PED' 'CONTRA'                  TEXT-043      '15'         abap_false abap_true, "'Contrapartida'
          12  'BSIK'               'BLDAT'                'TG_REG_PED' 'DT_VENC'                 TEXT-044      '10'         abap_false abap_false, "'Dt.Vencimento'
          13  'ZGLT045'            'OBSERV'               'TG_REG_PED' 'OBS'                     TEXT-045      '40'         abap_false abap_false, "'Observação'
          14  'VBAK'               'AUART'               'TG_REG_PED'  'AUART'                   TEXT-046      '09'         abap_false abap_false,    "------------*11.01.2017 LG "'Tipo Doc. Compras'
          15  'BSIK'               'EBELN'               'TG_REG_PED'  'EBELN'                   TEXT-047      '11'         abap_false abap_false,    "------------*11.01.2017 LG "'Doc. Compras'
          16  'BSID'               'VBEL2'               'TG_REG_PED'  'VBEL2'                   TEXT-048      '10'         abap_false abap_false,    "------------*11.01.2017 LG "'Ordem'
          17  'BSAK'               'AUGBL'               'TG_REG_PED'  'AUGBL'                   TEXT-049      '16'         abap_false abap_false,    "------------*11.01.2017 LG "'Doc. Compensação'
          18  'BSAK'               'AUGDT'               'TG_REG_PED'  'AUGDT'                   TEXT-079      '16'         abap_false abap_false.    "------------*04.01.2024 RJF "'Data. Compensação'


    CALL METHOD alv_0101->set_table_for_first_display
      EXPORTING
        i_default       = abap_true
        is_layout       = gs_layout_0101
        is_variant      = is_variant_0101
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = t_fieldcatalog
        it_outtab       = tg_reg_ped.

  ENDIF.

  CALL METHOD alv_0101->refresh_table_display.

  CALL METHOD alv_0101->get_scroll_info_via_id
    IMPORTING
      es_row_no   = es_row_no_0101
      es_row_info = es_row_inf_0101
      es_col_info = es_col_inf_0101.

ENDMODULE.                 " STATUS_0101  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0101  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0101 INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0101  INPUT

*&---------------------------------------------------------------------*
*&      Form  VALIDA_ACESSO_USUARIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LC_CONTINUA  text
*----------------------------------------------------------------------*
FORM valida_acesso_usuario  CHANGING p_lc_continua .

  DATA: wa_zglt062 TYPE zglt062.

  p_lc_continua = abap_true.

  CHECK s_contas[] IS INITIAL.

  SELECT SINGLE * INTO wa_zglt062
    FROM zglt062
   WHERE bukrs    IN s_bukrs
     AND dep_resp IN s_depre
     AND bname    EQ sy-uname.

  IF NOT sy-subrc IS INITIAL.
    p_lc_continua = abap_false.
    MESSAGE s013 DISPLAY LIKE 'E' WITH sy-uname s_bukrs-low s_depre-low.
    STOP.
  ENDIF.

ENDFORM.                    " VALIDA_ACESSO_USUARIO

*&---------------------------------------------------------------------*
*&      Form  BUSCA_NIVEL_APROVACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*
*
*----------------------------------------------------------------------*
FORM busca_nivel_aprovacao CHANGING wg_saida-nivel_aprova
                                    wg_saida-usuario_respo.

*------------------

  "NIVEL_APROVA     type ZGLT058-NIVEL "/Adicionar no type TY_SAIDA
  "USUARIO_RESPO    TYPE C LENGTH 200 "/Adicionar no type TY_SAIDA


*  DATA: TG_ZGLT058 TYPE TABLE OF ZGLT058 WITH HEADER LINE,
*        TG_ZGLT059 TYPE TABLE OF ZGLT059 WITH HEADER LINE,
*        TG_ZGLT062 TYPE TABLE OF ZGLT062 WITH HEADER LINE,
*        TG_ZGLT063 TYPE TABLE OF ZGLT063 WITH HEADER LINE.

  DATA: nivel TYPE zglt058-nivel.

*  SELECT *
*    FROM ZGLT058
*    INTO TABLE TG_ZGLT058
*    FOR ALL ENTRIES IN TG_041
*    WHERE BUKRS EQ TG_041-BUKRS
*    AND DEP_RESP EQ TG_041-DEP_RESP.
*
*  SELECT *
*    FROM ZGLT062
*    INTO TABLE TG_ZGLT062
*    FOR ALL ENTRIES IN TG_041
*    WHERE BUKRS EQ TG_041-BUKRS
*    AND DEP_RESP EQ TG_041-DEP_RESP.
*
*  SELECT *
*    FROM ZGLT063
*    INTO TABLE TG_ZGLT063
*    FOR ALL ENTRIES IN TG_041
*    WHERE BUKRS EQ TG_041-BUKRS
*    AND DEP_RESP EQ TG_041-DEP_RESP.
*
*  SELECT *
*    FROM ZGLT059
*    INTO TABLE TG_ZGLT059
*    FOR ALL ENTRIES IN TG_SKA1
*    WHERE BUKRS        EQ S_BUKRS
*    AND MONAT          EQ S_MES
*    AND GJAHR          EQ S_ANO
*    AND CK_ULTIMO_LOG  EQ 'S'
*    AND SAKNR          EQ TG_SKA1-SAKNR.
*
*  SORT TG_ZGLT059 BY BUKRS GJAHR MONAT SAKNR.

*------------------

  READ TABLE tg_zglt059 WITH KEY bukrs = wg_saida-bukrs
                                 gjahr = s_ano-low                          "Modificação 23.01.2016
                                 monat = s_mes-low                          "Modificação 23.01.2016
                                 saknr = wg_saida-saknr BINARY SEARCH.
  CLEAR: nivel.

  IF sy-subrc IS INITIAL.
    CASE tg_zglt059-status_lib.
      WHEN 'A'. "/Aprovado/
        wg_saida-usuario_respo = tg_zglt059-bn_liberacao.
        nivel = tg_zglt059-nivel.
      WHEN 'L'. "/Liberado para o próximo nível/
        IF tg_zglt059-nivel IS INITIAL.
          nivel = 1.
        ELSE.
          TRY.
              tg_zglt058 = tg_zglt058[ bukrs    = s_bukrs-low
                                       dep_resp = s_depre-low
                                       nivel    = tg_zglt059-nivel + 1 ].

              nivel = tg_zglt059-nivel + 1.

            CATCH cx_sy_itab_line_not_found.
              nivel = tg_zglt059-nivel.
          ENDTRY.
        ENDIF.

        LOOP AT tg_zglt058 WHERE bukrs    IN s_bukrs
                             AND dep_resp IN s_depre
                             AND nivel    EQ nivel.

          IF wg_saida-usuario_respo IS INITIAL.
            wg_saida-usuario_respo = tg_zglt058-bname.
          ELSE.
            CONCATENATE wg_saida-usuario_respo ',' INTO wg_saida-usuario_respo.
            CONCATENATE wg_saida-usuario_respo tg_zglt058-bname INTO wg_saida-usuario_respo SEPARATED BY space.
          ENDIF.
        ENDLOOP.
      WHEN 'R'. "/Rejeitado/
        IF tg_zglt059-nivel EQ 1.
          nivel = 000.
          PERFORM busca_usuarios_reconciliacao USING wg_saida-saknr
                                               CHANGING wg_saida-usuario_respo.
        ELSE.
          nivel = tg_zglt059-nivel - 1.
          LOOP AT tg_zglt058 WHERE bukrs  IN s_bukrs
                             AND dep_resp IN s_depre
                             AND nivel    EQ nivel.

            IF wg_saida-usuario_respo IS INITIAL.
              wg_saida-usuario_respo = tg_zglt058-bname.
            ELSE.
              CONCATENATE wg_saida-usuario_respo ',' INTO wg_saida-usuario_respo.
              CONCATENATE wg_saida-usuario_respo tg_zglt058-bname INTO wg_saida-usuario_respo SEPARATED BY space.
            ENDIF.
          ENDLOOP.
        ENDIF.
    ENDCASE.
  ELSE.
    "/Status inicial/
    nivel = 000.

    PERFORM busca_usuarios_reconciliacao USING wg_saida-saknr
                                         CHANGING wg_saida-usuario_respo.
  ENDIF.

  IF nivel IS NOT INITIAL.
    SHIFT nivel LEFT DELETING LEADING '0'.
    wg_saida-nivel_aprova = nivel.
  ELSE.
    wg_saida-nivel_aprova = nivel(1).
  ENDIF.

ENDFORM.                    " VERIFICA_NIVEL_LIBERACAO


*&---------------------------------------------------------------------*
*&      Form  BUSCA_USUARIOS_RECONCILIACAO
*&---------------------------------------------------------------------*
*       Retorna usuário responsável pela reconsiliação
*----------------------------------------------------------------------*
FORM busca_usuarios_reconciliacao  USING    p_saknr    TYPE saknr
                                   CHANGING r_usuarios.

  CLEAR: r_usuarios.

  LOOP AT tg_zglt063 WHERE bukrs    IN s_bukrs
                     AND dep_resp IN s_depre
                     AND saknr    EQ p_saknr.
    IF r_usuarios IS INITIAL.
      r_usuarios = tg_zglt063-bname.
    ELSE.
      CONCATENATE r_usuarios ',' INTO r_usuarios.
      CONCATENATE r_usuarios tg_zglt063-bname INTO r_usuarios SEPARATED BY space.
    ENDIF.
  ENDLOOP.

  LOOP AT tg_zglt062 WHERE bukrs    IN s_bukrs
                       AND dep_resp IN s_depre.

    READ TABLE tg_zglt063 WITH KEY bukrs    = s_bukrs-low
                                   dep_resp = s_depre-low
                                   saknr    = p_saknr
                                   BINARY SEARCH.
    IF sy-subrc IS NOT INITIAL.
      IF r_usuarios IS INITIAL.
        r_usuarios = tg_zglt062-bname.
      ELSE.
        CONCATENATE r_usuarios ',' INTO r_usuarios.
        CONCATENATE r_usuarios tg_zglt062-bname INTO r_usuarios SEPARATED BY space.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " BUSCA_USUARIOS_RECONCILIACAO
*&---------------------------------------------------------------------*
*&      Form  PREENCHE_DOC_COMPRAS
*&---------------------------------------------------------------------*
*       "------------*11.01.2017 LG
*----------------------------------------------------------------------*
*      <--P_WG_REG_PED_NUM_XBELN  text
*----------------------------------------------------------------------*
FORM preenche_doc_compras USING p_empresa TYPE bukrs
                                p_wg_reg_ped_num_belnr
                                p_wg_reg_ped_num_codigo
                                p_saknr
                                p_buzei
                          CHANGING p_wg_reg_ped_num_auart
                                   p_wg_reg_ped_num_ebeln
                                   p_wg_reg_ped_num_vbel2
                                   p_wg_reg_ped_num_augbl
                                   p_wg_reg_ped_num_augdt.

  DATA: v_mitkz TYPE skb1-mitkz.

  IF p_saknr IS NOT INITIAL.

    CLEAR: v_mitkz.

    SELECT SINGLE mitkz
      FROM skb1
      INTO v_mitkz
      WHERE saknr EQ p_saknr.

    IF v_mitkz EQ 'K'.

      "Documento de Compra (se não compensado)
      SELECT SINGLE ebeln
        FROM bsik
        INTO p_wg_reg_ped_num_ebeln
        WHERE bukrs EQ p_empresa
          AND belnr EQ p_wg_reg_ped_num_belnr
          AND lifnr EQ p_wg_reg_ped_num_codigo
          AND buzei EQ p_buzei.

      IF sy-subrc IS NOT INITIAL.
        "Documento de Compra (se compensado)
        SELECT SINGLE ebeln
          FROM bsak
          INTO p_wg_reg_ped_num_ebeln
          WHERE bukrs EQ p_empresa
          AND belnr EQ p_wg_reg_ped_num_belnr
            AND lifnr EQ p_wg_reg_ped_num_codigo
            AND buzei EQ p_buzei.
        "Documento de compensação
        SELECT SINGLE AUGDT augbl
          FROM bsak
          INTO ( p_wg_reg_ped_num_augdt, p_wg_reg_ped_num_augbl )
          WHERE bukrs EQ p_empresa
            AND belnr EQ p_wg_reg_ped_num_belnr
            AND lifnr EQ p_wg_reg_ped_num_codigo
            AND buzei EQ p_buzei.
      ENDIF.
      " Tipo de Documento de compra
      IF p_wg_reg_ped_num_ebeln IS NOT INITIAL.
        SELECT SINGLE bsart
          FROM ekko
          INTO p_wg_reg_ped_num_auart
          WHERE ebeln EQ p_wg_reg_ped_num_ebeln.
      ENDIF.

    ELSEIF v_mitkz EQ 'D'.

      "Ordem (se não compensado)
      SELECT SINGLE vbel2
        FROM bsid
        INTO p_wg_reg_ped_num_vbel2
        WHERE bukrs EQ p_empresa
          AND belnr EQ p_wg_reg_ped_num_belnr
          AND kunnr EQ p_wg_reg_ped_num_codigo
          AND buzei EQ p_buzei.

      IF sy-subrc IS NOT INITIAL.
        "Ordem (se compensado)
        SELECT SINGLE vbel2
          FROM bsad
          INTO p_wg_reg_ped_num_vbel2
          WHERE bukrs EQ p_empresa
            AND belnr EQ p_wg_reg_ped_num_belnr
            AND kunnr EQ p_wg_reg_ped_num_codigo
            AND buzei EQ p_buzei.
        "Documento de compensação
        SELECT SINGLE AUGDT augbl
          FROM bsad
          INTO ( p_wg_reg_ped_num_augdt, p_wg_reg_ped_num_augbl )
          WHERE bukrs EQ p_empresa
            AND belnr EQ p_wg_reg_ped_num_belnr
            AND kunnr EQ p_wg_reg_ped_num_codigo
            AND buzei EQ p_buzei.
      ENDIF.
      " Tipo de Documento de compra
      IF p_wg_reg_ped_num_vbel2 IS NOT INITIAL.
        SELECT SINGLE auart
          FROM vbak
          INTO p_wg_reg_ped_num_auart
          WHERE vbeln EQ p_wg_reg_ped_num_vbel2.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

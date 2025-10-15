*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 09.12.2024                                              &*
*& Descrição: Automatização Cut Off - Base de Cálculo                 &*
*& Transação: ZGL0092                                                 &*
*&--------------------------------------------------------------------&*
*& Projeto  : Os Ninjas Evolution                                     &*
*& Código Espec.Funcional/Técnica: Carolini Santos / Ronaldo Freitas  &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

REPORT  zgl0001_teste NO STANDARD PAGE HEADING MESSAGE-ID fi.
INCLUDE <icon>.

TABLES: t001, skb1, bsis, sscrfields, zfit0198, j_1bnfdoc, mara, zib_contabil, tzb27.

TYPES:
  BEGIN OF ty_saidan,
    mark,
    bukrs     TYPE    j_1bnfdoc-bukrs,
    gjahr     TYPE    j_1bnfdoc-gjahr,
    poper     TYPE    acdoca-poper,
    itmnum    TYPE    j_1bnflin-itmnum,
    branch    TYPE    j_1bnfdoc-branch,
    cfop      TYPE    j_1bnflin-cfop,
    tp_m      TYPE    zegl_tpm,
    nfenum    TYPE    j_1bnfdoc-nfenum,
    docnum    TYPE    j_1bnfdoc-docnum,
    parid     TYPE    j_1bnfdoc-parid,
    name1     TYPE    kna1-name1,
    matkl     TYPE    j_1bnflin-matkl,
    rassc     TYPE    acdoca-rassc,
    shpunt    TYPE    j_1bnfdoc-shpunt,
    matnr     TYPE    j_1bnflin-matnr,
    maktx     TYPE    makt-maktx,
    pstdat    TYPE    j_1bnfdoc-pstdat,
    docdat    TYPE    j_1bnfdoc-docdat,
    anzpk     TYPE    j_1bnfdoc-anzpk,
    hsl       TYPE    acdoca-hsl,
    tsl       TYPE    acdoca-tsl,
    kursf     TYPE    bkpf-kursf,
    aubel     TYPE    vbrp-aubel,
    refkey    TYPE    j_1bnflin-refkey,
    belnr     TYPE    bkpf-belnr,
    ort01_p   TYPE    j_1bnfnad-ort01,
    regio     TYPE    j_1bnfnad-regio,
    ort01     TYPE    j_1bnfnad-ort01,
    regio_l   TYPE    j_1bnfnad-regio,
    inco1     TYPE    j_1bnfdoc-inco1,
    racct     TYPE    acdoca-racct,
    saknr_cc  TYPE    tzb27-saknr,
    saknr_r   TYPE    tzb27-saknr,
    saknr_rc  TYPE    tzb27-saknr,
    saknr_c   TYPE    tzb27-saknr,
    saknr_p   TYPE    tzb27-saknr,
    saknr_e   TYPE    tzb27-saknr,
    saknr_ec  TYPE    tzb27-saknr,
    waers     TYPE    bkpf-waers,
    uf        TYPE    char10,
    route     TYPE    vbap-route,
    traztd    TYPE    char10,
    docdat_e  TYPE    j_1bnfdoc-docdat,
    gera      TYPE    char3,
    nr_conhec TYPE   char20,
  END OF ty_saidan,

  BEGIN OF ty_acdoca,
    mark,
    rbukrs TYPE    acdoca-rbukrs,
    gjahr  TYPE    acdoca-gjahr,
    belnr  TYPE    acdoca-belnr,
    rassc  TYPE    acdoca-rassc,
    ksl    TYPE    acdoca-ksl,
    tsl    TYPE    acdoca-tsl,
    msl    TYPE    acdoca-msl,
    matnr  TYPE    acdoca-matnr,
    racct  TYPE    acdoca-racct,
    precob TYPE    acdoca-hsl,
    preco  TYPE    acdoca-hsl,
    poper  TYPE acdoca-poper,
  END OF ty_acdoca.

*zglt0004
TYPES: BEGIN OF ty_zglt0004,
         status_1  TYPE char4,
         status_2  TYPE char4,
         doc_rec   TYPE vbeln_va,
         doc_rev_r TYPE vbeln_va,
         doc_cust  TYPE vbeln_va,
         doc_rev_c TYPE vbeln_va.
         INCLUDE TYPE zglt0004.
TYPES: END OF ty_zglt0004.

CLASS:lcl_alv_toolbar DEFINITION DEFERRED.

DATA: g_container          TYPE scrfname VALUE 'CC_GRID1',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      grid3                TYPE REF TO cl_gui_alv_grid,
      _stable              TYPE lvc_s_stbl VALUE 'XX',
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container.

DATA : ty_toolbar      TYPE stb_button,
       tl_index_rows   TYPE lvc_t_row,
       it_zib_contabil TYPE STANDARD TABLE OF zib_contabil,
       wa_zib_contabil TYPE zib_contabil,
       wl_index_rows   TYPE lvc_s_row.
DATA: functxt TYPE smp_dyntxt.

DATA: git_fieldcatalog  TYPE lvc_t_fcat,
      gwa_fieldcatalog  TYPE lvc_s_fcat,
      git_fieldcatalog2 TYPE lvc_t_fcat,
      gwa_fieldcatalog2 TYPE lvc_s_fcat,
      gv_ucommx         TYPE sy-ucomm,
      gwa_layout        TYPE lvc_s_layo,
      gwa_layout2       TYPE lvc_s_layo,
      gwa_layout3       TYPE lvc_s_layo,
      gwa_stable        TYPE lvc_s_stbl,
      gwa_stable2       TYPE lvc_s_stbl,
      gv_ukurs          TYPE tcurr-ukurs.

CONSTANTS: c_x              TYPE c VALUE 'X',
           c_a              TYPE c VALUE 'A',
           c_r              TYPE c VALUE 'R',
           c_br(2)          TYPE c VALUE 'BR',
           c_lf(2)          TYPE c VALUE 'LF',
           c_ag(2)          TYPE c VALUE 'AG',
           c_exit(4)        TYPE c VALUE 'EXIT',
           c_back(4)        TYPE c VALUE 'BACK',
           c_aprov(5)       TYPE c VALUE 'APROV',
           c_all(3)         TYPE c VALUE 'ALL',
           c_dall(4)        TYPE c VALUE 'DALL',
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_atuali(6)      TYPE c VALUE 'ATUALI',
           c_search(6)      TYPE c VALUE 'SEARCH',
           c_p_mat(6)       TYPE c VALUE 'P_MAT',
           c_cont(6)        TYPE c VALUE 'P_CONT',
           c_g_rec(6)       TYPE c VALUE 'G_REC',
           c_ge_rec(6)      TYPE c VALUE 'GE_REC',
           c_gr_rec(6)      TYPE c VALUE 'GR_REC',
           c_er_rec(6)      TYPE c VALUE 'ER_REC',
           c_g_cus(6)       TYPE c VALUE 'G_CUS',
           c_ge_cus(6)      TYPE c VALUE 'GE_CUS',
           c_gr_cus(6)      TYPE c VALUE 'GR_CUS',
           c_er_cus(6)      TYPE c VALUE 'ER_CUS',
           c_buscar(6)      TYPE c VALUE 'BUSCAR',
           c_doc_cont(8)    TYPE c VALUE 'DOC_CONT',
           c_doc_est(7)     TYPE c VALUE 'DOC_EST',
           c_clos_itens(10) TYPE c VALUE 'CLOS_ITENS'.

DATA: lr_poper TYPE RANGE OF poper WITH HEADER LINE,
      lr_gjahr TYPE RANGE OF gjahr WITH HEADER LINE,
      lr_racct TYPE RANGE OF racct WITH HEADER LINE.

DATA: git_saidac   TYPE TABLE OF ty_zglt0004, "zglt0004,                  "zglt0004,
      it_bdc       TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      gwa_saidac   LIKE LINE  OF git_saidac,
      gwa_saidacx  LIKE LINE  OF git_saidac,
      git_zglt0004 TYPE TABLE OF zglt0004,
      gwa_zglt0004 LIKE LINE  OF git_saidac,
      wa_zglt0004  TYPE zglt0004,
      git_saidan   TYPE TABLE OF ty_saidan,
      git_saidand  TYPE TABLE OF ty_saidan,
      gwa_saidan   LIKE LINE  OF git_saidan,
      git_acdoca   TYPE TABLE OF ty_acdoca,
      git_acdocaf  TYPE TABLE OF ty_acdoca,
      git_acdocam  TYPE TABLE OF ty_acdoca,
      gwa_acdocam  LIKE LINE  OF git_acdocam,
      git_zglt0003 TYPE TABLE OF zglt0003,
      gwa_acdoca   LIKE LINE  OF git_acdoca,
      gaw_acdocaf  LIKE LINE  OF git_acdocaf.

DATA: git_zglt0104     TYPE TABLE OF zglt0104  WITH HEADER LINE,
      git_zfit0160_atu TYPE TABLE OF zfit0160  WITH HEADER LINE,
      git_zfit0160_ant TYPE TABLE OF zfit0160  WITH HEADER LINE,
      git_tcurr        TYPE TABLE OF tcurr     WITH HEADER LINE,
      git_zfit0197     TYPE TABLE OF zfit0197 WITH HEADER LINE,
      git_zfit0197_aux TYPE TABLE OF zfit0197 WITH HEADER LINE,
      git_dta          TYPE TABLE OF bdcdata,
      opt              TYPE ctu_params,
      git_msg          TYPE TABLE OF bdcmsgcoll,
      w_zglt0112       TYPE zglt0112.

DATA: gva_variant  TYPE varid-variant,
      gva_data_ini TYPE sy-datum,
      gva_data_fim TYPE sy-datum,
      gva_belnr    TYPE zfit0197-belnr.

TYPES: ty_selection TYPE STANDARD TABLE OF rsparams.
DATA: gwa_selection TYPE rsparams,
      git_selection TYPE ty_selection.

FIELD-SYMBOLS: <t_data>      TYPE ANY TABLE,
               <t_data_line> TYPE ANY TABLE,
               <w_data>      TYPE any,
               <w_data_line> TYPE any.

DATA: l_data            TYPE REF TO data,
      l_data_line       TYPE REF TO data,
      l_data_descr      TYPE REF TO cl_abap_datadescr,
      l_data_line_descr TYPE REF TO cl_abap_datadescr.

DATA: ok_code   TYPE sy-ucomm,
      msg_error TYPE string.

DATA:
  lv_dia     TYPE sy-datum,
  lv_ult_dia TYPE sy-datum,
  p_poper    TYPE poper.

*&--------------------------------------------------------------------&*
*& Selection                                                          &*
*&--------------------------------------------------------------------&*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.

    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS s_bukrs FOR t001-bukrs NO-EXTENSION NO INTERVALS. "DEFAULT '0100'.
    SELECTION-SCREEN SKIP 1.
    PARAMETERS p_mes TYPE month VALUE CHECK.
    SELECTION-SCREEN SKIP 1.
    PARAMETERS p_ano TYPE gjahr.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS s_budat FOR bsis-budat NO-DISPLAY.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS s_docnum FOR j_1bnfdoc-docnum NO-EXTENSION NO INTERVALS.  "NO-DISPLAY. "Retirar teste RJF
    SELECTION-SCREEN SKIP 1.
    PARAMETERS:p_gera(5) AS LISTBOX VISIBLE LENGTH 5 DEFAULT ''.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS p_matnr FOR mara-matnr NO-EXTENSION NO INTERVALS.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS p_waers FOR zib_contabil-waers NO-EXTENSION NO INTERVALS.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS p_saknr FOR tzb27-saknr NO-EXTENSION NO INTERVALS.
    SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN END OF BLOCK a2.
  SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK a1.
*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
INITIALIZATION.


  TYPES: BEGIN OF ty_simnao,
           name TYPE char3,
           id   TYPE char1,
         END OF ty_simnao.

  DATA: list          TYPE vrm_values,
        value         LIKE LINE OF list,
        it_f1_sim_nao TYPE STANDARD TABLE OF ty_simnao INITIAL SIZE 0,
        p_pedagio     TYPE char3.

  SELECT name,id FROM zi_f1_sim_nao INTO TABLE @it_f1_sim_nao.

  LOOP AT it_f1_sim_nao INTO DATA(wa_f1_sim_nao).
    value-key = wa_f1_sim_nao-name.
    APPEND value TO list.
    CLEAR: wa_f1_sim_nao.
  ENDLOOP.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = 'P_GERA'
      values          = list
    EXCEPTIONS
      id_illegal_name = 0
      OTHERS          = 0.

  SELECTION-SCREEN : FUNCTION KEY 1.
*  sscrfields-functxt_01 = 'Cadastro de CFOP'.
  functxt-icon_id = icon_submit.
  functxt-quickinfo = 'Cadastro de CFOP'.
  functxt-icon_text = 'Cadastro de CFOP'.
  sscrfields-functxt_01 = functxt.

  SELECTION-SCREEN : FUNCTION KEY 2.
*  sscrfields-functxt_02 = 'Cadastro de Sociedade Parceira'.
  FREE functxt.
  functxt-icon_id = icon_submit.
  functxt-quickinfo = 'Cadastro de Sociedade Parceira'.
  functxt-icon_text = 'Cadastro de Sociedade Parceira'.
  sscrfields-functxt_02 = functxt.

  SELECTION-SCREEN : FUNCTION KEY 3.
*  sscrfields-functxt_03 = 'De/Para Contas CutOff'.
  FREE functxt.
  functxt-icon_id = icon_submit.
  functxt-quickinfo = 'De/Para Contas CutOff'.
  functxt-icon_text = 'De/Para Contas CutOff'.
  sscrfields-functxt_03 = functxt.

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.


  IF sy-ucomm = 'FC01'.
    CALL TRANSACTION 'ZGL089'.
  ENDIF.

  IF sy-ucomm = 'FC02'.
    CALL TRANSACTION 'ZGL090'.
  ENDIF.

  IF sy-ucomm = 'FC03'.
    CALL TRANSACTION 'ZGL091'.
  ENDIF.

START-OF-SELECTION.

  PERFORM z_valida_campo.

  IF msg_error IS NOT INITIAL.
    MESSAGE msg_error TYPE 'I' DISPLAY LIKE 'E'.
    EXIT.
  ELSE.

    CLEAR:lv_dia,lv_ult_dia.

    lv_dia = p_ano && p_mes && '01'.

    s_budat-sign   = 'I'.
    s_budat-option = 'BT'.
    s_budat-low    = lv_dia.

    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = lv_dia
      IMPORTING
        last_day_of_month = lv_ult_dia.

    s_budat-high   = lv_ult_dia.
    APPEND s_budat TO s_budat.

    p_poper = p_mes.

    IF p_matnr-low IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
        EXPORTING
          input  = p_matnr-low "campo de 400char
        IMPORTING
          output = p_matnr-low.

    ENDIF.

    IF p_saknr.
      CONDENSE p_saknr-low NO-GAPS.
    ENDIF.

    PERFORM z_seleciona_dados.
    IF git_saidan[] IS NOT INITIAL.
      CALL SCREEN 100.
    ENDIF.

  ENDIF.



*-----------------------------------------------------------------------
* Classe
*-----------------------------------------------------------------------
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column,

      on_hotsopt_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION
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
*       CLASS lcl_event_handler IMPLEMENTATION
*---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.
* Método de  execução para Duplo-click
  METHOD on_double_click.

  ENDMETHOD.              "ON_DOUBLE_CLICK
  METHOD on_hotsopt_click.

    READ TABLE git_saidac INTO DATA(gw_saidac) INDEX e_row_id-index.
    IF sy-subrc IS INITIAL.

      CASE e_column_id.  "Necessidade de seprarar
        WHEN 'STATUS_1'.
          PERFORM f_exibe_erro_zib USING e_row_id-index 'STATUS_1'.
        WHEN 'STATUS_2'.
          PERFORM f_exibe_erro_zib USING e_row_id-index 'STATUS_2'.
        WHEN 'DOC_REC'.
          IF gw_saidac-doc_rec IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saidac-doc_rec.
            SET PARAMETER ID 'BUK' FIELD gw_saidac-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saidac-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'DOC_CUST'.
          IF gw_saidac-doc_rec IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saidac-doc_cust.
            SET PARAMETER ID 'BUK' FIELD gw_saidac-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saidac-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'DOC_REV_R'.
          IF gw_saidac-doc_rec IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saidac-doc_rev_r.
            SET PARAMETER ID 'BUK' FIELD gw_saidac-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saidac-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'DOC_REV_C'.
          IF gw_saidac-doc_rec IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saidac-doc_rev_c.
            SET PARAMETER ID 'BUK' FIELD gw_saidac-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saidac-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
    ENDIF.
  ENDMETHOD.                    "on_hotsopt_click
ENDCLASS.           "LCL_EVENT_HANDLER IMPLEMENTATION
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

    IF sy-dynnr EQ '0200'.
      EXIT.
    ENDIF.

    IF sy-dynnr EQ '0100'.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_submit. "icon_execute_object.
      ty_toolbar-function  =  c_cont.
      ty_toolbar-quickinfo = 'Contabilização'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Contabilização'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_submit. "icon_system_undo.
      ty_toolbar-function  =  c_p_mat.
      ty_toolbar-quickinfo = 'Preço Unitário Material'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Preço Unitário Material'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

    ENDIF.

    IF sy-dynnr EQ '0300'.


      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_execute_object.
      ty_toolbar-function  =  c_g_rec.
      ty_toolbar-quickinfo = 'Gerar Documento Receita'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Gerar Doc. Receita'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_execute_object.
      ty_toolbar-function  =  c_g_cus.
      ty_toolbar-quickinfo = 'Gerar Documento Custo'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Gerar Doc. Custo'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_system_redo.
      ty_toolbar-function  =  c_ge_rec.
      ty_toolbar-quickinfo = 'Estornar Documento Receita'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Est. Doc. Receita'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_system_redo.
      ty_toolbar-function  =  c_ge_cus.
      ty_toolbar-quickinfo = 'Estornar Documento Custo'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Est. Doc. Custo'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_generate.
      ty_toolbar-function  =  c_gr_rec.
      ty_toolbar-quickinfo = 'Reverter Documento Receita'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Rev. Doc. Receita'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_generate.
      ty_toolbar-function  =  c_gr_cus.
      ty_toolbar-quickinfo = 'Reverter Documento Custo'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Rev. Doc. Custo'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_system_redo.
      ty_toolbar-function  =  c_er_rec.
      ty_toolbar-quickinfo = 'Estornar Reverter Documento Receita'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Est. Rev. Doc. Receita'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_system_redo.
      ty_toolbar-function  =  c_er_cus.
      ty_toolbar-quickinfo = 'Estornar Reverter Documento Custo'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Est. Rev. Doc. Custo'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

    ENDIF.

  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    DATA: lv_tabix TYPE sy-tabix.
    gv_ucommx = e_ucomm.

    CASE e_ucomm.
      WHEN c_p_mat.
        PERFORM f_preco_material.
      WHEN c_cont.
        PERFORM f_contabilizacao.
        IF git_saidac IS NOT INITIAL.
          CALL SCREEN 300.
        ENDIF.
      WHEN c_g_rec.
        PERFORM f_ger_doc_receita.
      WHEN c_ge_rec.
        PERFORM f_ger_est_receita.
      WHEN c_gr_rec.
        PERFORM f_ger_rev_doc_receita. "Reversão Receita
      WHEN c_er_rec.
        PERFORM f_ger_rev_est_doc_receita.
      WHEN c_g_cus.
        PERFORM f_ger_doc_custo.
      WHEN c_ge_cus.
        PERFORM f_ger_est_custo.
      WHEN c_gr_cus. "Reversão Custo
        PERFORM f_ger_rev_doc_custo.
      WHEN c_er_cus.
        PERFORM f_ger_rev_est_doc_custo.
    ENDCASE.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.
    CLEAR gv_ucommx.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.

  DATA: event         TYPE cntl_simple_event,
        events        TYPE cntl_simple_events,
        p_text        TYPE sdydo_text_element,
        dg_splitter_1 TYPE REF TO cl_gui_splitter_container,
        i_filtros	    TYPE zif_screen_linha_filtro_t,
        tl_filter     TYPE lvc_t_filt,
        wl_filter     TYPE lvc_s_filt,
        tl_function   TYPE ui_functions,
        wl_function   LIKE tl_function WITH HEADER LINE,
        v_valor(60),
        v_datum(10)   TYPE c,
        v_uzeit(10)   TYPE c.


  IF grid1 IS INITIAL.

    CLEAR: i_filtros.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO v_datum.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit.
    DESCRIBE TABLE git_saidan LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'Data:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros.

  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
  EXPORTING
    i_titulo  = CONV #( p_text )
    i_filtros = i_filtros
  CHANGING
    split     = dg_splitter_1
    alv       = grid1 ) = abap_true.
    gwa_layout-zebra      = c_x.
    gwa_layout-stylefname = 'STYLE2'.
    gwa_layout-box_fname  = 'SELECTED'.
    gwa_layout-sel_mode   = c_a.

    PERFORM montar_layout_grid.
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

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
    wl_function = cl_gui_alv_grid=>mc_fc_maintain_variant.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    DATA: gs_layout  TYPE disvariant,
          gs_layout2 TYPE disvariant,
          gs_layout3 TYPE disvariant,
          g_repid    LIKE sy-repid.

    gwa_layout-sel_mode   = 'A'.
    gwa_layout-cwidth_opt = 'X'.
    gwa_layout-col_opt    = 'X'.
    gwa_layout-box_fname  = 'SELECTED'.

    gs_layout-report = sy-repid.

    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_layout       = gwa_layout
        is_variant      = gs_layout
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = git_fieldcatalog[]
        it_outtab       = git_saidan[].

    SET HANDLER:
    lcl_event_handler=>on_double_click FOR grid1.
    SET HANDLER:
    lcl_event_handler=>on_hotsopt_click FOR grid1.
  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.
  ENDIF.


ENDMODULE.                 " CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'Z001'.
  SET TITLEBAR 'Z001'.

  CALL METHOD cl_gui_cfw=>flush.
  CALL METHOD cl_gui_cfw=>dispatch.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA : fnam TYPE char40,
         fval TYPE char40.



  CASE ok_code.
    WHEN c_search OR c_buscar.
*      PERFORM f_atualiza_doc.
    WHEN c_atuali.
*      PERFORM f_atualiza_doc.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_cancel.
      LEAVE PROGRAM.
    WHEN c_exit.
      LEAVE PROGRAM.
    WHEN c_doc_cont.
      GET CURSOR FIELD fnam VALUE fval.
  ENDCASE.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = gwa_stable.

  FREE ok_code.
ENDMODULE.                 " USER_COMMAND_0100  INPUT

FORM montar_layout_grid .

  REFRESH git_fieldcatalog.

  PERFORM montar_estrutura USING:
        01    ' '   'BUKRS      ' 'GIT_SAIDAN' 'BUKRS      ' 'Empresa                   '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        02    ' '   'BRANCH     ' 'GIT_SAIDAN' 'BRANCH     ' 'Filial                    '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        03    ' '   'CFOP       ' 'GIT_SAIDAN' 'CFOP       ' 'CFOP                      '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        04    ' '   'TP_M       ' 'GIT_SAIDAN' 'TP_M       ' 'Tipo Mercado              '                  '04'    ' ' ' ' ' ' ' ' ' ' ,
        05    ' '   'NFENUM     ' 'GIT_SAIDAN' 'NFENUM     ' 'Nr. Nota                  '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        06    ' '   'DOCNUM     ' 'GIT_SAIDAN' 'DOCNUM     ' 'Nº Documento              '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        07    ' '   'PARID      ' 'GIT_SAIDAN' 'PARID      ' 'Cod. Cliente              '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        08    ' '   'NAME1      ' 'GIT_SAIDAN' 'NAME1      ' 'Cliente                   '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        09    ' '   'MATKL      ' 'GIT_SAIDAN' 'MATKL      ' 'Grp. Mercadoria           '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        10    ' '   'RASSC      ' 'GIT_SAIDAN' 'RASSC      ' 'Sociedade Parceira        '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        11    ' '   'SHPUNT     ' 'GIT_SAIDAN' 'SHPUNT     ' 'Un                        '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        12    ' '   'MATNR      ' 'GIT_SAIDAN' 'MATNR      ' 'Produto                   '                  '20'    ' ' ' ' ' ' ' ' ' ' ,
        13    ' '   'MAKTX      ' 'GIT_SAIDAN' 'MAKTX      ' 'Descr. Material           '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        14    ' '   'PSTDAT     ' 'GIT_SAIDAN' 'PSTDAT     ' 'Dt. Lançamento            '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        15    ' '   'DOCDAT     ' 'GIT_SAIDAN' 'DOCDAT     ' 'Dt. Documento             '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        16    ' '   'ANZPK      ' 'GIT_SAIDAN' 'ANZPK      ' 'Quantidade                '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        17    ' '   'HSL        ' 'GIT_SAIDAN' 'HSL        ' 'Valor NF                  '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        18    ' '   'TSL        ' 'GIT_SAIDAN' 'TSL        ' 'Valor Dolar               '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        19    ' '   'KURSF      ' 'GIT_SAIDAN' 'KURSF      ' 'Taxa Dolar                '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        20    ' '   'AUBEL      ' 'GIT_SAIDAN' 'AUBEL      ' 'Ordem de Venda            '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        21    ' '   'REFKEY     ' 'GIT_SAIDAN' 'REFKEY     ' 'Doc. Faturamento          '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        22    ' '   'BELNR      ' 'GIT_SAIDAN' 'BELNR      ' 'Doc. Contabil             '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        23    ' '   'ORT01_P    ' 'GIT_SAIDAN' 'ORT01_P    ' 'Cidade PC                 '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        24    ' '   'REGIO      ' 'GIT_SAIDAN' 'REGIO      ' 'UF                        '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        25    ' '   'ORT01      ' 'GIT_SAIDAN' 'ORT01      ' 'Local de entrega          '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        26    ' '   'REGIO_L    ' 'GIT_SAIDAN' 'REGIO_L    ' 'UF                        '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        27    ' '   'INCO1      ' 'GIT_SAIDAN' 'INCO1      ' 'Frete                     '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        28    ' '   'RACCT      ' 'GIT_SAIDAN' 'RACCT      ' 'Conta cliente             '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        29    ' '   'SAKNR_CC   ' 'GIT_SAIDAN' 'SAKNR_CC   ' 'Conta cliente CutOff      '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        30    ' '   'SAKNR_R    ' 'GIT_SAIDAN' 'SAKNR_R    ' 'Conta receita             '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        31    ' '   'SAKNR_RC   ' 'GIT_SAIDAN' 'SAKNR_RC   ' 'Conta receita CutOff      '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        32    ' '   'SAKNR_C    ' 'GIT_SAIDAN' 'SAKNR_C    ' 'Conta custo               '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        33    ' '   'SAKNR_P    ' 'GIT_SAIDAN' 'SAKNR_P    ' 'Custo PP ou Ver CutOff    '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        34    ' '   'SAKNR_E    ' 'GIT_SAIDAN' 'SAKNR_E    ' 'Conta estoque             '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        35    ' '   'SAKNR_EC   ' 'GIT_SAIDAN' 'SAKNR_EC   ' 'Conta estoque CutOff      '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        36    ' '   'WAERS      ' 'GIT_SAIDAN' 'WAERS      ' 'Moeda                     '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        37    ' '   'UF         ' 'GIT_SAIDAN' 'UF         ' 'Mesma UF ?                '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        38    ' '   'ROUTE      ' 'GIT_SAIDAN' 'ROUTE      ' 'Itinerário                '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        39    ' '   'TRAZTD     ' 'GIT_SAIDAN' 'TRAZTD     ' 'Tempo Gasto Itinerário    '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        40    ' '   'DOCDAT_E   ' 'GIT_SAIDAN' 'DOCDAT_E   ' 'Data de entrega           '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
        41    ' '   'GERA       ' 'GIT_SAIDAN' 'GERA       ' 'Gera CutOff no mês ?      '                  '10'    ' ' ' ' ' ' ' ' ' ' .


ENDFORM.                    " MONTAR_LAYOUT_GRID1
*&---------------------------------------------------------------------*
*&      Form  montar_estrutura
*&---------------------------------------------------------------------*
FORM montar_estrutura USING VALUE(p_col_pos)       TYPE i
      VALUE(p_ref_tabname)   LIKE dd02d-tabname
      VALUE(p_ref_fieldname) LIKE dd03d-fieldname
      VALUE(p_tabname)       LIKE dd02d-tabname
      VALUE(p_field)         LIKE dd03d-fieldname
      VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
      VALUE(p_outputlen)
      VALUE(p_edit)
      VALUE(p_sum)
      VALUE(p_hotspot)
      VALUE(p_no_out)
      VALUE(p_just).

  CLEAR gwa_fieldcatalog.
  gwa_fieldcatalog-fieldname     = p_field.
  gwa_fieldcatalog-tabname       = p_tabname.
  gwa_fieldcatalog-ref_table     = p_ref_tabname.
  gwa_fieldcatalog-ref_field     = p_ref_fieldname.
  gwa_fieldcatalog-key           = ' '.
  gwa_fieldcatalog-key_sel       = 'X'.
  gwa_fieldcatalog-edit          = p_edit.
  gwa_fieldcatalog-do_sum        = p_sum.

  gwa_fieldcatalog-col_pos         = p_col_pos.
  IF p_outputlen IS NOT INITIAL.
    gwa_fieldcatalog-outputlen      = p_outputlen.
  ENDIF.
  gwa_fieldcatalog-no_out        = p_no_out.
  gwa_fieldcatalog-reptext       = p_scrtext_l.
  gwa_fieldcatalog-scrtext_s     = p_scrtext_l.
  gwa_fieldcatalog-scrtext_m     = p_scrtext_l.
  gwa_fieldcatalog-scrtext_l     = p_scrtext_l.
  gwa_fieldcatalog-hotspot       = p_hotspot.

  gwa_fieldcatalog-just          = p_just.

  IF p_field EQ 'MARK'.
    gwa_fieldcatalog-checkbox = abap_on.
  ENDIF.

  APPEND gwa_fieldcatalog TO git_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form F_PRECO_MATERIAL
*&---------------------------------------------------------------------*
FORM f_preco_material.

  PERFORM f_processo_material.

  IF git_acdocaf IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados não encontrado para seleção!'.
    EXIT.
  ELSE.
    DELETE git_acdocaf WHERE matnr IS INITIAL.
    CALL SCREEN 200.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  ZF_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zf_shdb USING p_program p_dynpro p_dynbegin p_fnam p_fval.

  DATA: lwa_dta   TYPE bdcdata.

  lwa_dta-program   = p_program.
  lwa_dta-dynpro    = p_dynpro.
  lwa_dta-dynbegin  = p_dynbegin.
  lwa_dta-fnam      = p_fnam.
  lwa_dta-fval      = p_fval.
  APPEND lwa_dta TO git_dta.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PREPARE_RUN_TIME_INFO
*&---------------------------------------------------------------------*
FORM f_prepare_run_time_info .

  cl_salv_bs_runtime_info=>clear_all( ).

  IF <t_data> IS ASSIGNED.
    CLEAR <t_data>[].
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR <t_data_line>[].
  ENDIF.

  IF <t_data> IS ASSIGNED.
    CLEAR <t_data>.
  ENDIF.

  IF <t_data_line> IS ASSIGNED.
    CLEAR <t_data_line>.
  ENDIF.

  FREE: l_data, l_data_line, l_data_descr, l_data_line_descr.

  cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                          metadata = abap_true
                                          data     = abap_true ).
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  Z_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_seleciona_dados .

  TYPES: BEGIN OF ty_parid,
           sign   TYPE c,
           option TYPE char2,
           low    TYPE kunnr,
           high   TYPE kunnr,
         END OF ty_parid.
  DATA: r_parid  TYPE TABLE OF ty_parid,
        lr_parid LIKE LINE OF r_parid.

  TYPES: BEGIN OF ty_docnum,
           sign   TYPE c,
           option TYPE char2,
           low    TYPE docnum,
           high   TYPE docnum,
         END OF ty_docnum.
  DATA: r_docnum  TYPE TABLE OF ty_docnum,
        lr_docnum LIKE LINE OF r_docnum.


  DATA: it_1bnflinx TYPE TABLE OF j_1bnflin,
        wa_1bnflinx TYPE j_1bnflin.

*Buscar documentos da ZCONF:
  SELECT * FROM j_1bnfdoc
  INTO TABLE @DATA(it_1bnfdoc_me)
        WHERE bukrs  IN @s_bukrs
        AND pstdat IN @s_budat
        AND docnum IN @s_docnum
*     AND doctyp NE '5'
        AND cancel NE @abap_true.
*     AND inco1  NE 'FOB'. "NOT IN ('FOB', 'CFR').

  IF it_1bnfdoc_me[] IS NOT INITIAL.
    SELECT docnum, cfop, matkl, matnr, refkey, refitm, itmnum, werks, nfnet, menge FROM j_1bnflin
    INTO TABLE @DATA(it_1bnflin)
          FOR ALL ENTRIES IN @it_1bnfdoc_me
          WHERE docnum EQ @it_1bnfdoc_me-docnum
          AND cfop IN ( SELECT cfop FROM zglt0001 WHERE tp_m EQ 'ME' ).

    IF sy-subrc IS INITIAL.

      DATA(it_1bnflin_me) = it_1bnflin[].

      SORT it_1bnflin_me BY docnum.
      DELETE ADJACENT DUPLICATES FROM it_1bnflin_me COMPARING docnum.

      IF it_1bnflin_me IS NOT INITIAL.
        LOOP AT it_1bnflin_me ASSIGNING FIELD-SYMBOL(<fs_lin_me>).
          lr_docnum-sign = 'I'.
          lr_docnum-option = 'EQ'.
          lr_docnum-low = <fs_lin_me>-docnum.
          APPEND lr_docnum TO r_docnum.
          CLEAR lr_docnum.
        ENDLOOP.
      ENDIF.

      DELETE it_1bnfdoc_me WHERE docnum NOT IN r_docnum.

      IF it_1bnfdoc_me IS NOT INITIAL.
        DATA(it_1bnfdoc) = it_1bnfdoc_me[].
      ENDIF.

    ENDIF.

  ENDIF.
*Buscar documentos da ZCONF:
  SELECT * FROM j_1bnfdoc
  APPENDING TABLE @it_1bnfdoc
  WHERE bukrs  IN @s_bukrs
  AND pstdat IN @s_budat
  AND docnum IN @s_docnum
  AND doctyp NE '5'
  AND cancel NE @abap_true
  AND inco1  NE 'FOB'. "NOT IN ('FOB', 'CFR').

*  IF sy-subrc IS INITIAL.
  IF it_1bnfdoc IS NOT INITIAL.
    SORT it_1bnfdoc BY docnum.

    LOOP AT it_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_doc>).
      lr_parid-sign = 'I'.
      lr_parid-option = 'EQ'.
      lr_parid-low = <fs_doc>-parid.
      APPEND lr_parid TO r_parid.
      CLEAR lr_parid.
    ENDLOOP.
    IF r_parid[] IS NOT INITIAL.
      SORT r_parid BY low.
      DELETE ADJACENT DUPLICATES FROM r_parid COMPARING low.
    ENDIF.
* Filtrar documentos por CFOP:
    IF it_1bnfdoc[] IS NOT INITIAL.
      SELECT docnum, cfop, matkl, matnr, refkey, refitm, itmnum, werks, nfnet, menge FROM j_1bnflin
      APPENDING TABLE @it_1bnflin
      FOR ALL ENTRIES IN @it_1bnfdoc
      WHERE docnum EQ @it_1bnfdoc-docnum
      AND cfop IN ( SELECT cfop FROM zglt0001 ).

      IF it_1bnflin IS NOT INITIAL.
        SORT it_1bnflin BY docnum cfop matkl matnr refkey refitm itmnum werks nfnet menge.
        DELETE ADJACENT DUPLICATES FROM it_1bnflin COMPARING ALL FIELDS.
        SORT it_1bnflin BY docnum.
      ENDIF.

    ENDIF.
*Buscar dados de cadastro na BKPF:
    IF it_1bnflin[] IS NOT INITIAL.
      SELECT * FROM bkpf
      INTO TABLE @DATA(it_bkpf)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE bukrs IN @s_bukrs
            AND awkey EQ @it_1bnflin-refkey(20).

      IF it_bkpf IS NOT INITIAL.
        SORT it_bkpf BY bukrs awkey.

        SELECT bukrs, belnr, kunnr, xref3, vbel2, vbeln FROM bseg
        INTO TABLE @DATA(it_bseg)
              FOR ALL ENTRIES IN @it_bkpf
              WHERE bukrs EQ @it_bkpf-bukrs
              AND belnr EQ @it_bkpf-belnr
              AND kunnr IN @r_parid.

        IF sy-subrc IS INITIAL.
          SORT it_bseg BY bukrs belnr kunnr.
        ENDIF.
      ENDIF.

    ENDIF.
*Buscar dados de cadastro na ADOCA:
    IF it_bkpf[] IS NOT INITIAL.
*      SELECT rbukrs, gjahr, belnr, rassc, hsl, tsl, racct FROM acdoca
*      INTO TABLE @DATA(it_acdoca)
*        FOR ALL ENTRIES IN @it_bkpf
*      WHERE rldnr  EQ '0L'
*        AND rbukrs IN @s_bukrs
*        AND rassc  IN ( SELECT tipo FROM zglt0002 )
*        AND gjahr  EQ @it_bkpf-gjahr
*        AND belnr  EQ @it_bkpf-belnr
*        AND koart  EQ 'D'.

      SELECT
      bukrs, gjahr, belnr, rassc, hsl, tsl, racct
      FROM zi_zgl0001_03
      FOR ALL ENTRIES IN @it_bkpf
      WHERE rldnr  EQ '0L'
      AND bukrs IN @s_bukrs
      AND rassc  IN ( SELECT tipo FROM zglt0002 )
      AND gjahr  EQ @it_bkpf-gjahr
      AND belnr  EQ @it_bkpf-belnr
      AND koart  EQ 'D'
      INTO TABLE @DATA(it_acdoca).

*      SELECT rbukrs, gjahr, belnr, rassc, hsl, tsl, racct FROM acdoca
*      APPENDING TABLE @it_acdoca
*        FOR ALL ENTRIES IN @it_bkpf
*      WHERE rldnr  EQ '0L'
*        AND rbukrs IN @s_bukrs
*        AND rassc  EQ @space
*        AND gjahr  EQ @it_bkpf-gjahr
*        AND belnr  EQ @it_bkpf-belnr
*        AND koart  EQ 'D'.

      SELECT
      bukrs, gjahr, belnr, rassc, hsl, tsl, racct
      APPENDING TABLE @it_acdoca
      FROM zi_zgl0001_03
      FOR ALL ENTRIES IN @it_bkpf
      WHERE rldnr  EQ '0L'
      AND bukrs IN @s_bukrs
      AND rassc  EQ @space
      AND gjahr  EQ @it_bkpf-gjahr
      AND belnr  EQ @it_bkpf-belnr
      AND koart  EQ 'D'.

      IF it_acdoca[] IS NOT INITIAL.
        SORT it_acdoca BY bukrs gjahr belnr.

        SELECT * FROM zglt0003
        INTO TABLE @git_zglt0003.

        IF sy-subrc IS INITIAL.
          SORT git_zglt0003 BY tipo conta_ori.
        ENDIF.
      ENDIF.

*      SELECT rbukrs, kunnr, gjahr, belnr, rassc, hsl, tsl, racct FROM acdoca
*      INTO TABLE @DATA(it_acdoca_c)
*        FOR ALL ENTRIES IN @it_bkpf
*      WHERE rldnr  EQ '0L'
*        AND rbukrs IN @s_bukrs
*        AND gjahr  EQ @it_bkpf-gjahr
*        AND belnr  EQ @it_bkpf-belnr
*        AND koart  EQ 'S'.
      SELECT
      bukrs, kunnr, gjahr, belnr, rassc, hsl, tsl, racct
      FROM zi_zgl0001_03
      FOR ALL ENTRIES IN @it_bkpf
      WHERE rldnr  EQ '0L'
      AND bukrs IN @s_bukrs
      AND gjahr  EQ @it_bkpf-gjahr
      AND belnr  EQ @it_bkpf-belnr
      AND koart  EQ 'S'
      INTO TABLE @DATA(it_acdoca_c).

      IF it_acdoca_c IS NOT INITIAL.
        SORT it_acdoca_c BY bukrs kunnr. "gjahr belnr.
      ENDIF.
    ENDIF.
*Buscar ponto de coleta:
    IF it_1bnfdoc[] IS NOT INITIAL.

      SELECT docnum, ort01, regio, parvw FROM j_1bnfnad
      INTO TABLE @DATA(it_1bnfnad)
            FOR ALL ENTRIES IN @it_1bnfdoc
            WHERE docnum EQ @it_1bnfdoc-docnum
            AND parvw = 'PC'.

      IF it_1bnfnad IS NOT INITIAL.
        SORT it_1bnfnad BY docnum parvw.
      ENDIF.
*Buscar local de entrega:
      SELECT docnum, ort01, regio, parvw FROM j_1bnfnad
      APPENDING TABLE @it_1bnfnad
      FOR ALL ENTRIES IN @it_1bnfdoc
      WHERE docnum EQ @it_1bnfdoc-docnum
      AND parvw = 'LR'.

      IF it_1bnfnad IS NOT INITIAL.
        SORT it_1bnfnad BY docnum parvw.
      ENDIF.

*Buscar nome do cliente:
      SELECT kunnr, name1 FROM kna1
      INTO TABLE @DATA(it_kna1)
            FOR ALL ENTRIES IN @it_1bnfdoc
            WHERE kunnr EQ @it_1bnfdoc-parid.

      IF it_kna1 IS NOT INITIAL.
        SORT it_kna1 BY kunnr.
      ENDIF.

    ENDIF.

*Buscar descrição do material:
    IF it_1bnflin[] IS NOT INITIAL.

      SELECT vbeln, posnn, vbtyp_n, vbelv, sonum FROM vbfa
      INTO TABLE @DATA(it_vbfa)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE vbeln EQ @it_1bnflin-refkey(10)
            AND posnn EQ @it_1bnflin-refitm
            AND vbtyp_n EQ 'M'
            AND vbtyp_v EQ 'J'.
    ENDIF.

    IF it_vbfa[] IS NOT INITIAL.

      SORT it_vbfa BY vbeln posnn.

      DATA: it_vbfax TYPE TABLE OF vbfa,
            wa_vbfax TYPE vbfa.

      LOOP AT it_vbfa INTO DATA(wa_vbfa).
        wa_vbfax = CORRESPONDING #( wa_vbfa ).
        FREE wa_vbfax-sonum.
        wa_vbfax-sonum = wa_vbfa-vbelv.
        APPEND wa_vbfax TO it_vbfax.
        CLEAR wa_vbfax.
      ENDLOOP.

      IF it_vbfax[] IS NOT INITIAL.

        SORT it_vbfax BY vbeln posnn.

        SELECT mblnr, mjahr, xblnr FROM mkpf
        INTO TABLE @DATA(it_mkpf)
              FOR ALL ENTRIES IN @it_vbfax
              WHERE xblnr EQ @it_vbfax-sonum.
        IF it_mkpf IS NOT INITIAL.
          SORT it_mkpf BY xblnr.
        ENDIF.
      ENDIF.
    ENDIF.

    DATA: it_mkpf_c TYPE TABLE OF mkpf,
          wa_mkpf_c TYPE mkpf.

    LOOP AT it_mkpf INTO DATA(wa_mkpf).
      wa_mkpf_c = CORRESPONDING #( wa_mkpf ).
      FREE wa_mkpf_c-zch_referencia.
      wa_mkpf_c-zch_referencia = wa_mkpf-mblnr && wa_mkpf-mjahr.
      APPEND wa_mkpf_c TO it_mkpf_c.
      CLEAR wa_mkpf_c.
    ENDLOOP.

    IF it_mkpf_c[] IS NOT INITIAL.
      SORT it_mkpf_c BY xblnr.

      SELECT bukrs, belnr, gjahr, awkey FROM bkpf
      INTO TABLE @DATA(it_bkpf_c)
            FOR ALL ENTRIES IN @it_mkpf_c
            WHERE awkey = @it_mkpf_c-zch_referencia.
    ENDIF.

    IF it_bkpf_c[] IS NOT INITIAL.
      SORT it_bkpf_c BY awkey.


*      SELECT rbukrs, gjahr, belnr, rassc, hsl, tsl, racct, ktosl FROM acdoca
*      INTO TABLE @DATA(it_acdoca_c2)
*        FOR ALL ENTRIES IN @it_bkpf_c
*      WHERE rldnr  EQ '0L'
*        AND rbukrs EQ @it_bkpf_c-bukrs
*        AND gjahr  EQ @it_bkpf_c-gjahr
*        AND belnr  EQ @it_bkpf_c-belnr.

      SELECT bukrs, gjahr, belnr, rassc, hsl, tsl, racct, ktosl FROM zi_zgl0001_03
      FOR ALL ENTRIES IN @it_bkpf_c
      WHERE bukrs EQ @it_bkpf_c-bukrs
      AND gjahr  EQ @it_bkpf_c-gjahr
      AND belnr  EQ @it_bkpf_c-belnr
      INTO TABLE @DATA(it_acdoca_c2).

      IF it_acdoca_c2 IS NOT INITIAL.
        SORT it_acdoca_c2 BY bukrs gjahr belnr ktosl.
      ENDIF.
    ENDIF.

    IF it_1bnflin[] IS NOT INITIAL.
      SELECT matnr, maktx FROM makt
      INTO TABLE @DATA(it_makt)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE matnr EQ @it_1bnflin-matnr
            AND spras EQ @sy-langu.

      IF sy-subrc IS INITIAL.
        SORT it_makt BY matnr.
      ENDIF.

*Buscar Ordem de venda:
      LOOP AT it_1bnflin INTO DATA(wa_1bnflin).
        DATA(lv_tabix) = sy-tabix.
        wa_1bnflinx = CORRESPONDING #( wa_1bnflin ).
        FREE wa_1bnflinx-matnr.
        wa_1bnflinx-matnr = wa_1bnflin-refkey.
        APPEND wa_1bnflinx TO it_1bnflinx.
        CLEAR wa_1bnflinx.
      ENDLOOP.
    ENDIF.

    IF it_1bnflin[] IS NOT INITIAL.
      SORT it_1bnflin BY refkey.
      SELECT matnr, vbeln, aubel FROM vbrp
      INTO TABLE @DATA(it_vbrp)
            FOR ALL ENTRIES IN @it_1bnflin
            WHERE vbeln EQ @it_1bnflin-refkey(10).

      SORT it_vbrp BY vbeln.
    ENDIF.

    IF it_vbrp[] IS NOT INITIAL.
      SORT it_vbrp BY aubel.
      SELECT vbeln, route FROM vbap
      INTO TABLE @DATA(it_vbap)
            FOR ALL ENTRIES IN @it_vbrp
            WHERE vbeln EQ @it_vbrp-aubel.
      IF it_vbap IS NOT INITIAL.
        SORT it_vbap BY vbeln.
      ENDIF.
      SORT it_vbrp BY vbeln.
    ENDIF.

    IF it_vbap[] IS NOT INITIAL.
      SORT it_vbap BY route.
      SELECT * FROM tvro
      INTO TABLE @DATA(it_tvro)
            FOR ALL ENTRIES IN @it_vbap
            WHERE route EQ @it_vbap-route.
      IF it_tvro IS NOT INITIAL.
        SORT it_tvro BY route.
      ENDIF.
      SORT it_vbap BY vbeln.
    ENDIF.

    SELECT tp_m, cfop
    INTO TABLE @DATA(git_zglt0001x)
          FROM zglt0001.

    IF sy-subrc IS INITIAL.
      SORT git_zglt0001x BY tp_m cfop.
    ENDIF.

*Incluir na saída o campo Nº Conhec. BL
    SELECT tp_m, cfop
    INTO TABLE @DATA(git_zglt0001)
          FROM zglt0001
          WHERE tp_m EQ 'ME'.

    IF sy-subrc IS INITIAL.
      SORT git_zglt0001 BY tp_m cfop.
    ENDIF.

*Ir na tabela vbfa com
*vbeln =   BSEG-VBELN
*vbtyp_V = 'J'
*AND vf~vbtyp_n  IN ('M', 'O','P')
*somente dos das linhas em que o CFOP for do Tipo ME - Mercado Externo
*Selecionar vbelv

    IF it_1bnflin IS NOT INITIAL.
      SELECT vbeln, posnn, vbtyp_n, vbelv, sonum FROM vbfa
      INTO TABLE @DATA(it_vbfa_bl)
*        FOR ALL ENTRIES IN @it_1bnflin
            FOR ALL ENTRIES IN @it_bseg
            WHERE vbeln EQ @it_bseg-vbeln "@it_bseg-xref3(10)
*         WHERE vbelv EQ @it_bseg-vbel2
            AND vbtyp_v EQ 'J'
            AND vbtyp_n IN ( 'M', 'O', 'P' ).
*          AND vbtyp_n EQ 'J'.
      IF it_vbfa IS NOT INITIAL.
        SORT it_vbfa_bl BY vbeln.

        SELECT vbeln, id_due
        INTO TABLE @DATA(it_zdoc_exp)
              FROM zdoc_exp
              FOR ALL ENTRIES IN @it_vbfa
              WHERE vbeln EQ @it_vbfa-vbelv.
        IF sy-subrc IS INITIAL.
          SORT it_zdoc_exp BY vbeln.

          SELECT id_due, id_nomeacao_tran
          INTO TABLE @DATA(it_zsdt0170)
                FROM zsdt0170
                FOR ALL ENTRIES IN @it_zdoc_exp
                WHERE id_due EQ @it_zdoc_exp-id_due.
          IF sy-subrc IS INITIAL.
            SORT it_zsdt0170 BY id_due.
            SELECT id_nomeacao_tran, nr_conhec, dt_data
            INTO TABLE @DATA(it_znom_conhec)
                  FROM znom_conhec
                  FOR ALL ENTRIES IN @it_zsdt0170
                  WHERE id_nomeacao_tran = @it_zsdt0170-id_nomeacao_tran.
            IF sy-subrc IS INITIAL.
              SORT it_znom_conhec BY id_nomeacao_tran.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

* Processando informaçoes selecionadas para ALV
*  LOOP AT it_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_1bnfdoc>).

  LOOP AT it_1bnflin INTO wa_1bnflin.

    IF it_1bnfdoc IS NOT INITIAL.
      READ TABLE it_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_1bnfdoc>) WITH KEY docnum = wa_1bnflin-docnum
      BINARY SEARCH.


      IF sy-subrc IS INITIAL.

        gwa_saidan-bukrs   	 = <fs_1bnfdoc>-bukrs.
        gwa_saidan-branch  	 = <fs_1bnfdoc>-branch.
        gwa_saidan-nfenum    = <fs_1bnfdoc>-nfenum.
        gwa_saidan-docnum    = <fs_1bnfdoc>-docnum.
        gwa_saidan-parid     = <fs_1bnfdoc>-parid.
        gwa_saidan-shpunt    = <fs_1bnfdoc>-shpunt.
        gwa_saidan-pstdat    = <fs_1bnfdoc>-pstdat.
        gwa_saidan-docdat    = <fs_1bnfdoc>-docdat.
*      gwa_saidan-anzpk     = <fs_1bnfdoc>-anzpk.
        gwa_saidan-anzpk     = wa_1bnflin-menge.
        gwa_saidan-inco1     = <fs_1bnfdoc>-inco1.
        gwa_saidan-poper     = <fs_1bnfdoc>-pstdat+4(2).


        READ TABLE it_kna1 INTO DATA(wa_kna1) WITH KEY kunnr = <fs_1bnfdoc>-parid
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-name1     = wa_kna1-name1.
        ENDIF.

*    READ TABLE it_bkpf INTO DATA(wa_bkpf) WITH KEY bukrs = <fs_1bnfdoc>-bukrs
*                                                   awkey = wa_1bnflin-refkey(20)
*                                          BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*
*      gwa_saidan-kursf     = wa_bkpf-kursf.
*      gwa_saidan-belnr     = wa_bkpf-belnr.
*      gwa_saidan-waers     = wa_bkpf-waers.
*
*    ENDIF.
*    READ TABLE it_acdoca INTO DATA(wa_acdoca) WITH KEY rbukrs = <fs_1bnfdoc>-bukrs
*                                                       gjahr  = wa_bkpf-gjahr
*                                                       belnr  = wa_bkpf-belnr
*                                                       BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*      gwa_saidan-gjahr     = wa_acdoca-gjahr.
*      gwa_saidan-rassc     = wa_acdoca-rassc.
*      gwa_saidan-hsl       = wa_acdoca-hsl.
*      gwa_saidan-tsl       = wa_acdoca-tsl.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = wa_acdoca-racct
*        IMPORTING
*          output = gwa_saidan-racct.
*
*    ENDIF.

        READ TABLE it_1bnfnad INTO DATA(wa_1bnfnad) WITH KEY docnum = <fs_1bnfdoc>-docnum
              parvw = 'PC'
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-ort01_p   = wa_1bnfnad-ort01.
          gwa_saidan-regio     = wa_1bnfnad-regio.
        ENDIF.

        READ TABLE it_1bnfnad INTO DATA(wa_1bnfnadl) WITH KEY docnum = <fs_1bnfdoc>-docnum
              parvw = 'LR'
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-ort01     = wa_1bnfnadl-ort01.
          gwa_saidan-regio_l   = wa_1bnfnadl-regio.
        ENDIF.

        READ TABLE it_acdoca_c INTO DATA(wa_acdoca_c) WITH KEY bukrs = <fs_1bnfdoc>-bukrs
              kunnr  = <fs_1bnfdoc>-parid
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_acdoca_c-racct
            IMPORTING
              output = gwa_saidan-saknr_r.

          READ TABLE git_zglt0003 INTO DATA(wa_zglt0003) WITH KEY tipo = '2'
                conta_ori = gwa_saidan-saknr_r
                BINARY SEARCH.

          IF sy-subrc IS INITIAL.
            gwa_saidan-saknr_rc  = wa_zglt0003-conta_cut.
          ENDIF.
        ENDIF.



*    READ TABLE it_1bnflin INTO wa_1bnflin WITH KEY docnum = <fs_1bnfdoc>-docnum
*                                          BINARY SEARCH.

*    LOOP AT it_1bnflin INTO wa_1bnflin WHERE docnum = <fs_1bnfdoc>-docnum.

*    IF sy-subrc IS INITIAL.


        READ TABLE it_bkpf INTO DATA(wa_bkpf) WITH KEY bukrs = <fs_1bnfdoc>-bukrs
              awkey = wa_1bnflin-refkey(20)
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          IF wa_bkpf-waers EQ 'USD'.
            gwa_saidan-kursf     = abs( wa_bkpf-kursf ).
          ELSE.
            gwa_saidan-kursf     = abs( wa_bkpf-kurs2 ).
          ENDIF.
          gwa_saidan-belnr     = wa_bkpf-belnr.
          gwa_saidan-waers     = wa_bkpf-waers.

        ENDIF.

        READ TABLE it_acdoca INTO DATA(wa_acdoca) WITH KEY bukrs = <fs_1bnfdoc>-bukrs
              gjahr  = wa_bkpf-gjahr
              belnr  = wa_bkpf-belnr
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-gjahr     = wa_acdoca-gjahr.
          gwa_saidan-rassc     = wa_acdoca-rassc.
*        gwa_saidan-hsl       = wa_acdoca-hsl.
          gwa_saidan-hsl       = wa_1bnflin-nfnet.
*        gwa_saidan-tsl       = wa_acdoca-tsl.
*        IF wa_bkpf-kurs2 NE 0.
*          gwa_saidan-tsl       = wa_1bnflin-nfnet / wa_bkpf-kurs2.
*        ELSE.
*          CLEAR gwa_saidan-tsl.
*        ENDIF.

          IF gwa_saidan-kursf NE 0.
            gwa_saidan-tsl       = wa_1bnflin-nfnet / abs( gwa_saidan-kursf ).
          ELSE.
            CLEAR gwa_saidan-tsl.
          ENDIF.


          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = wa_acdoca-racct
            IMPORTING
              output = gwa_saidan-racct.

        ENDIF.


        READ TABLE git_zglt0003 INTO wa_zglt0003 WITH KEY tipo = '1'
        conta_ori = gwa_saidan-racct
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saidan-saknr_cc  = wa_zglt0003-conta_cut.
        ENDIF.

*   Incluir na saída o campo Nº Conhec. BL
        READ TABLE git_zglt0001 INTO DATA(wa_zglt0001) WITH KEY cfop = wa_1bnflin-cfop.

        IF sy-subrc IS INITIAL.

          READ TABLE it_bseg INTO DATA(wa_bseg) WITH KEY bukrs = wa_bkpf-bukrs
                belnr = wa_bkpf-belnr
                kunnr = <fs_1bnfdoc>-parid
                BINARY SEARCH.
          IF sy-subrc IS INITIAL.


            READ TABLE it_vbfa_bl INTO DATA(wa_vbfa_bl) WITH KEY vbeln = wa_bseg-vbeln "vbeln = wa_bseg-xref3(10) "vbelv = wa_bseg-vbel2 " vbeln = wa_bseg-xref3(10)
                  BINARY SEARCH.

*          READ TABLE it_vbfa INTO DATA(wa_vbfa_bl) WITH KEY vbeln = wa_bseg-vbeln "vbeln = wa_bseg-xref3(10) "vbelv = wa_bseg-vbel2 " vbeln = wa_bseg-xref3(10)
*                                        BINARY SEARCH.

            IF sy-subrc IS INITIAL.

              READ TABLE it_zdoc_exp INTO DATA(wa_zdoc_exp) WITH KEY vbeln = wa_vbfa_bl-vbelv
                    BINARY SEARCH.

              IF sy-subrc IS INITIAL.

                READ TABLE it_zsdt0170 INTO DATA(wa_zsdt0170) WITH KEY id_due = wa_zdoc_exp-id_due
                      BINARY SEARCH.

                IF sy-subrc IS INITIAL.

                  READ TABLE it_znom_conhec INTO DATA(wa_znom_conhec) WITH KEY id_nomeacao_tran = wa_zsdt0170-id_nomeacao_tran
                        BINARY SEARCH.

                  IF sy-subrc IS INITIAL AND wa_znom_conhec-dt_data IS NOT INITIAL AND s_budat-high GT wa_znom_conhec-dt_data.
                    CONTINUE.
                  ENDIF.

                  IF sy-subrc IS INITIAL AND wa_znom_conhec-nr_conhec IS NOT INITIAL.
                    CONTINUE.
                  ENDIF.
                ENDIF..
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    gwa_saidan-cfop    	 = wa_1bnflin-cfop.
    gwa_saidan-matkl     = wa_1bnflin-matkl.
    gwa_saidan-matnr     = wa_1bnflin-matnr.
    gwa_saidan-refkey    = wa_1bnflin-refkey.

    READ TABLE git_zglt0001x INTO DATA(wa_zglt0001x) WITH KEY cfop = wa_1bnflin-cfop.
    IF sy-subrc IS INITIAL.
      gwa_saidan-tp_m      = wa_zglt0001x-tp_m.
    ENDIF.

    READ TABLE it_makt INTO DATA(wa_makt) WITH KEY matnr = wa_1bnflin-matnr
          BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      gwa_saidan-maktx     = wa_makt-maktx.
    ENDIF.

    READ TABLE it_vbrp INTO DATA(wa_vbrp) WITH KEY vbeln = wa_1bnflin-refkey
          BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      gwa_saidan-aubel     = wa_vbrp-aubel.
    ENDIF.

    READ TABLE it_vbfax INTO wa_vbfax WITH KEY vbeln = wa_1bnflin-refkey(10)
    posnn = wa_1bnflin-refitm
    BINARY SEARCH.

    READ TABLE it_mkpf_c INTO wa_mkpf_c WITH KEY xblnr = wa_vbfax-sonum
    BINARY SEARCH.

    READ TABLE it_bkpf_c INTO DATA(wa_bkpf_c) WITH KEY awkey = wa_mkpf_c-zch_referencia
          BINARY SEARCH.

    READ TABLE it_acdoca_c2 INTO DATA(wa_acdoca_c2) WITH KEY bukrs = wa_bkpf_c-bukrs
          gjahr  = wa_bkpf_c-gjahr
          belnr  = wa_bkpf_c-belnr
          ktosl  = 'GBB'
          BINARY SEARCH.

    IF sy-subrc IS INITIAL. " AND wa_acdoca_c2-ktosl = 'GBB'.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_acdoca_c2-racct
        IMPORTING
          output = gwa_saidan-saknr_c.
    ENDIF.

    READ TABLE git_zglt0003 INTO wa_zglt0003 WITH KEY tipo = '3'
    conta_ori = gwa_saidan-saknr_c
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      gwa_saidan-saknr_p   = wa_zglt0003-conta_cut.
    ENDIF.

    gwa_saidan-saknr_e = wa_1bnflin-matkl.
    READ TABLE git_zglt0003 INTO wa_zglt0003 WITH KEY tipo = '4'
    conta_ori = wa_1bnflin-matkl
    BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      gwa_saidan-saknr_ec  = wa_zglt0003-conta_cut.
    ENDIF.

    IF  gwa_saidan-regio EQ gwa_saidan-regio_l.
      gwa_saidan-uf        = 'SIM'.
    ELSE.
      gwa_saidan-uf        = 'NAO'.
    ENDIF.

    READ TABLE it_vbap INTO DATA(wa_vbap) WITH KEY vbeln = wa_vbrp-aubel
          BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      gwa_saidan-route     = wa_vbap-route.
    ENDIF.

    READ TABLE it_tvro INTO DATA(wa_tvro) WITH KEY route = wa_vbap-route
          BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      IF wa_tvro-traztd GT 0.
        DATA(lv_calc) = ( wa_tvro-traztd / 10000 ).
      ENDIF.
      gwa_saidan-traztd    = lv_calc.
    ENDIF.

    gwa_saidan-docdat_e  = <fs_1bnfdoc>-pstdat + lv_calc.

    IF gwa_saidan-docdat_e GE s_budat-low AND gwa_saidan-docdat_e LE s_budat-high.
      gwa_saidan-gera      = 'NAO'. "'SIM'.
    ELSEIF s_budat-high IS INITIAL.
      DATA: v_ultimo_dia TYPE sy-datum.

      CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
        EXPORTING
          day_in            = s_budat-low
        IMPORTING
          last_day_of_month = v_ultimo_dia.

      IF gwa_saidan-docdat_e LE v_ultimo_dia.
        gwa_saidan-gera      = 'NAO'. "'SIM'.
      ENDIF.
    ELSE.
      gwa_saidan-gera      = 'SIM'. "'NAO'.
    ENDIF.

    IF gwa_saidan-tp_m EQ 'ME'.
      gwa_saidan-gera      = 'SIM'.
    ENDIF.

    APPEND gwa_saidan TO git_saidan.
    CLEAR gwa_saidan.
*    ENDIF.
*    ENDLOOP.
  ENDLOOP.

*  SELECT * FROM zglt0004
*    into table @data(it_zglt0004). "Buscar quais info?

  IF git_saidan[] IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros com os parâmetros informado!'(004).
    EXIT.
  ELSE.
    SORT git_saidan[] BY gera ASCENDING.

    IF p_gera = abap_false.

    ELSEIF p_gera = 'SIM'.
      DELETE git_saidan[] WHERE gera <> 'SIM'.
    ELSE.
      DELETE git_saidan[] WHERE gera <> 'NAO'.
    ENDIF.

    IF p_matnr = abap_false.

    ELSE.
      DELETE git_saidan[] WHERE matnr <> p_matnr-low.
    ENDIF.

    IF p_waers = abap_false.

    ELSE.
      DELETE git_saidan[] WHERE waers <> p_waers-low.
    ENDIF.

    IF p_saknr = abap_false.

    ELSE.
      DATA: _saknr_cc TYPE saknr.
      CLEAR:_saknr_cc.
      _saknr_cc = p_saknr-low.
      PACK _saknr_cc TO _saknr_cc.
      CONDENSE _saknr_cc NO-GAPS.
      DELETE git_saidan[] WHERE saknr_cc <> _saknr_cc.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE ok_code.
    WHEN c_search OR c_buscar.
*      PERFORM f_atualiza_doc.
    WHEN c_atuali.
*      PERFORM f_atualiza_doc.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_cancel.
      LEAVE PROGRAM.
    WHEN c_exit.
      LEAVE PROGRAM.
    WHEN c_doc_cont.
      GET CURSOR FIELD fnam VALUE fval.
  ENDCASE.

  CALL METHOD grid2->refresh_table_display
    EXPORTING
      is_stable = gwa_stable.

  FREE ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
  SET PF-STATUS 'Z002'.
  SET TITLEBAR 'Z002'.

  CALL METHOD cl_gui_cfw=>flush.
  CALL METHOD cl_gui_cfw=>dispatch.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CRIA_OBJETOS_200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE cria_objetos_200 OUTPUT.
  DATA:
    event2        TYPE cntl_simple_event,
    events2       TYPE cntl_simple_events,
    p_text2       TYPE sdydo_text_element,
    dg_splitter_2 TYPE REF TO cl_gui_splitter_container,
    i_filtros2    TYPE zif_screen_linha_filtro_t,
    tl_filter2    TYPE lvc_t_filt,
    wl_filter2    TYPE lvc_s_filt,
    tl_function2  TYPE ui_functions,
    wl_function2  LIKE tl_function WITH HEADER LINE,
    v_valor2(60),
    v_datum2(10)  TYPE c,
    v_uzeit2(10)  TYPE c.

  IF grid2 IS INITIAL.

    CLEAR: i_filtros2.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO v_datum2.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit2.
    DESCRIBE TABLE git_acdocaf LINES v_lines.
    APPEND VALUE #( parametro = 'Data:' valor = v_datum2 ) TO i_filtros2.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit2 ) TO i_filtros2.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros2.

  ENDIF.

  CLEAR: zcl_screen=>zif_screen~split.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
  EXPORTING
    i_titulo  = CONV #( p_text2 )
    i_filtros = i_filtros2
  CHANGING
    split     = dg_splitter_2
    alv       = grid2 ) = abap_true.

    gwa_layout2-zebra      = c_x.
    gwa_layout2-stylefname = 'STYLE2'.
    gwa_layout2-box_fname  = 'SELECTED'.
    gwa_layout2-sel_mode   = c_a.

    PERFORM montar_layout_grid_200.
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid2.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid2.
    SET HANDLER obg_toolbar->handle_user_command FOR grid2.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_maintain_variant.
    APPEND wl_function TO tl_function2.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function2.
    wl_function2 = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function2.

    gwa_layout2-sel_mode   = 'A'.
    gwa_layout2-cwidth_opt = 'X'.
    gwa_layout2-col_opt    = 'X'.
    gwa_layout2-box_fname  = 'SELECTED'.
    gs_layout2-report = sy-repid.

    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout       = gwa_layout2
        is_variant      = gs_layout2
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = git_fieldcatalog[]
        it_outtab       = git_acdocaf[].

    SET HANDLER:
    lcl_event_handler=>on_double_click FOR grid2.
    SET HANDLER:
    lcl_event_handler=>on_hotsopt_click FOR grid2.
  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = gwa_stable2.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Form montar_layout_grid_200
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM montar_layout_grid_200 .

  REFRESH git_fieldcatalog.

  PERFORM montar_estrutura USING:
        1  ' '   'RACCT     ' 'GIT_ACDOCAF' 'RACCT      ' 'Conta Razão              '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        2  ' '   'MATNR     ' 'GIT_ACDOCAF' 'MATNR      ' 'Material                 '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
        3  ' '   'MSL       ' 'GIT_ACDOCAF' 'MSL        ' 'Soma Qtd. UM de registo  '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        4  ' '   'TSL       ' 'GIT_ACDOCAF' 'TSL        ' 'Soma Montante MI         '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        5  ' '   'KSL       ' 'GIT_ACDOCAF' 'KSL        ' 'Soma Montante MI2        '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        6  ' '   'PRECOB    ' 'GIT_ACDOCAF' 'PRECOB     ' 'Preço BRL                '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        7  ' '   'PRECO     ' 'GIT_ACDOCAF' 'PRECO      ' 'Preço USD                '                  '15'    ' ' ' ' ' ' ' ' ' ' .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form z_valida_campo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM z_valida_campo .


  IF p_ano IS INITIAL.
    msg_error = |Ano é obrigatórios!|.
    EXIT.
  ENDIF.

  IF p_mes IS INITIAL.
    msg_error = |Mês é obrigatórios!|.
    EXIT.
  ENDIF.

  IF p_mes GT 12 OR p_mes LT 1.
    msg_error = |Mês inválido!|.
    EXIT.
  ENDIF.

  IF p_ano GT sy-datum(4) OR p_ano LT 1500.
    msg_error = |Ano fora de validade!|.
    EXIT.
  ENDIF.


  IF s_bukrs[] IS INITIAL. " OR s_budat[] IS INITIAL.
    msg_error = |Empresa campo obrigatório!|.
    EXIT.
  ENDIF.

ENDFORM.

FORM f_contabilizacao .

  SELECT * FROM zglt0004
  WHERE bukrs = @s_bukrs-low
  AND poper = @p_poper
  AND gjahr = @p_ano
  INTO TABLE @DATA(it_zglt0004).

  " DELETE it_zglt0004 WHERE obj_key_1 IS NOT INITIAL AND obj_key_2 IS NOT INITIAL.

  IF it_zglt0004 IS NOT INITIAL.

    MOVE-CORRESPONDING it_zglt0004 TO git_saidac.

    FREE: it_zglt0004.

  ELSE.

    DATA: lv_data      TYPE datum,
          lv_mes(2)    TYPE n,
          lv_data_wrt  TYPE char10,
          lv_data_conv TYPE datum.

    PERFORM f_processo_material.

    SORT git_acdocaf BY racct matnr.

    lv_mes = ( s_budat-high+4(2) ) + 1.
    lv_data = s_budat-high(4) && lv_mes && '01'."s_budat-high+6(2).

    WRITE lv_data TO lv_data_wrt.

    " Função standard para converter data em formato gravado na TCURR
    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lv_data_wrt
      IMPORTING
        output = lv_data_conv.

    SELECT ukurs
    UP TO 1 ROWS
    FROM tcurr
    INTO gv_ukurs
    WHERE kurst EQ 'B'
    AND fcurr EQ 'USD'
    AND tcurr EQ 'BRL'
    AND gdatu EQ lv_data_conv.
    ENDSELECT.

    SORT git_saidan BY waers racct matnr matkl.

    FREE: git_saidac. "PSA

    DATA(git_saidanx) = git_saidan[].

    SORT git_saidanx BY waers racct matnr matkl gera.
    DELETE git_saidanx WHERE gera <> 'SIM'.
    DELETE ADJACENT DUPLICATES FROM git_saidanx COMPARING nfenum docnum belnr.
    "waers racct matnr matkl.

    MOVE-CORRESPONDING git_saidanx TO git_saidac.

    FREE: git_saidanx.

    DATA:
      _qtdS   TYPE decfloat34,
      _qtd    TYPE decfloat34,
      _hsl    TYPE decfloat34,
      _vlrbrl TYPE decfloat34,
      _tsl    TYPE decfloat34,
      _vlrusd TYPE decfloat34.
    CLEAR: _hsl,_tsl,_qtd,_vlrbrl,_vlrusd.

    IF git_saidac IS NOT INITIAL.
      LOOP AT git_saidac ASSIGNING FIELD-SYMBOL(<fs_saidac>).

        CLEAR:gwa_saidan.

        LOOP AT git_saidan INTO gwa_saidan WHERE gera  EQ 'SIM' AND waers EQ <fs_saidac>-waers AND racct EQ <fs_saidac>-racct AND matkl EQ <fs_saidac>-matkl AND matnr EQ <fs_saidac>-matnr.

          CONDENSE gwa_saidan-anzpk NO-GAPS.
          _qtd = gwa_saidan-anzpk.
          _qtdS = _qtdS + _qtd.

          _vlrbrl = gwa_saidan-hsl.
          _hsl = _hsl + _vlrbrl.


          _vlrusd = gwa_saidan-tsl.
          _tsl = _tsl + _vlrusd.

          CLEAR: _qtd,_vlrbrl,_vlrusd,gwa_saidan.

        ENDLOOP.

        <fs_saidac>-anzpk = _qtdS.

        <fs_saidac>-vlr_brl    = _hsl.
        <fs_saidac>-vlr_dolar   = _tsl.

        READ TABLE git_saidan INTO gwa_saidan WITH KEY gera  = 'SIM' waers = <fs_saidac>-waers racct = <fs_saidac>-racct matkl = <fs_saidac>-matkl.
        <fs_saidac>-gewei       = gwa_saidan-shpunt.

        IF <fs_saidac>-status_1 IS INITIAL.
          <fs_saidac>-status_1 = icon_led_yellow.
        ENDIF.

        IF <fs_saidac>-status_2 IS INITIAL.
          <fs_saidac>-status_2 = icon_led_yellow.
        ENDIF.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_saidac>-saknr_c
          IMPORTING
            output = <fs_saidac>-saknr_c.

        IF <fs_saidac>-gjahr IS INITIAL.
          <fs_saidac>-gjahr = p_ano.
        ENDIF.


        READ TABLE git_acdocaf INTO DATA(wa_acdocaf) WITH KEY racct = <fs_saidac>-saknr_c
              matnr = <fs_saidac>-matnr
              BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_saidac>-preco   = wa_acdocaf-preco. "gwa_saidan-hsl. " Analisar com a Carol pendente parte 3
        ELSE.
          CLEAR <fs_saidac>-preco.
        ENDIF.

        <fs_saidac>-c_dolar   = _qtdS * _tsl.
        <fs_saidac>-c_brl   = _qtdS * _hsl."wa_acdocaf-precob.


        CLEAR: _hsl,_tsl,_qtd,_vlrbrl,_vlrusd,_qtdS.
      ENDLOOP.

      SORT git_saidac BY bukrs waers racct matnr matkl.

      DELETE ADJACENT DUPLICATES FROM git_saidac COMPARING bukrs waers racct matnr matkl.
      DELETE git_saidac WHERE matnr IS INITIAL.

      IF git_saidac IS NOT INITIAL.
        DATA: _uuid TYPE guid_16.
        LOOP AT git_saidac ASSIGNING <fs_saidac>.
          CLEAR: _uuid.
          CALL FUNCTION 'GUID_CREATE'
            IMPORTING
              ev_guid_16 = _uuid.
          <fs_saidac>-id = _uuid.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDIF.

  IF git_saidac[] IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados não encontrado para seleção!'.
    EXIT.
  ELSE.

    LOOP AT git_saidac[] ASSIGNING <fs_saidac>.

      IF <fs_saidac>-obj_key_1 IS NOT INITIAL.
        SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @<fs_saidac>-obj_key_1 INTO @DATA(_erro1).
        IF sy-subrc = 0.
          <fs_saidac>-status_1 = icon_led_red.
        ELSE.
          SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_saidac>-obj_key_1 INTO @<fs_saidac>-doc_rec.
          IF sy-subrc = 0.
            <fs_saidac>-status_1 = icon_led_green.
          ELSE.
            <fs_saidac>-status_1 = icon_led_yellow.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <fs_saidac>-obj_key_2 IS NOT INITIAL.
        SELECT SINGLE * FROM zib_contabil_err WHERE obj_key = @<fs_saidac>-obj_key_2 INTO @DATA(_erro2).
        IF sy-subrc = 0.
          <fs_saidac>-status_2 = icon_led_red.
        ELSE.
          SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_saidac>-obj_key_2 INTO @<fs_saidac>-doc_cust.
          IF sy-subrc = 0.
            <fs_saidac>-status_2 = icon_led_green.
          ELSE.
            <fs_saidac>-status_2 = icon_led_yellow.
          ENDIF.
        ENDIF.

      ENDIF.

      IF <fs_saidac>-obj_key_3 IS NOT INITIAL.
        SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_saidac>-obj_key_3 INTO @<fs_saidac>-doc_rev_r.
      ENDIF.

      IF <fs_saidac>-obj_key_4 IS NOT INITIAL.
        SELECT SINGLE belnr FROM zib_contabil_chv WHERE obj_key = @<fs_saidac>-obj_key_4 INTO @<fs_saidac>-doc_rev_c.
      ENDIF.

    ENDLOOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module CRIA_OBJETOS_300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE cria_objetos_300 OUTPUT.

  DATA:
    event3        TYPE cntl_simple_event,
    events3       TYPE cntl_simple_events,
    p_text3       TYPE sdydo_text_element,
    dg_splitter_3 TYPE REF TO cl_gui_splitter_container,
    i_filtros3    TYPE zif_screen_linha_filtro_t,
    tl_filter3    TYPE lvc_t_filt,
    wl_filter3    TYPE lvc_s_filt,
    tl_function3  TYPE ui_functions,
    wl_function3  LIKE tl_function WITH HEADER LINE,
    v_valor3(60),
    v_datum3(10)  TYPE c,
    v_uzeit3(10)  TYPE c.

  IF grid3 IS INITIAL.

    CLEAR: i_filtros3.
    CONCATENATE s_budat-high+06(02) '/' s_budat-high+04(02) '/' s_budat-high(04) INTO v_datum3.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit3.
    DESCRIBE TABLE git_saidac LINES v_lines.
    APPEND VALUE #( parametro = 'Empresa:' valor = s_bukrs-low ) TO i_filtros3.
    APPEND VALUE #( parametro = 'Data:' valor = v_datum3 ) TO i_filtros3.
    APPEND VALUE #( parametro = 'Taxa:' valor = gv_ukurs ) TO i_filtros3.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros3.

  ENDIF.

  CLEAR: zcl_screen=>zif_screen~split.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
  EXPORTING
    i_titulo  = CONV #( p_text3 )
    i_filtros = i_filtros3
  CHANGING
    split     = dg_splitter_3
    alv       = grid3 ) = abap_true.

    gwa_layout2-zebra      = c_x.
    gwa_layout2-stylefname = 'STYLE2'.
    gwa_layout2-box_fname  = 'SELECTED'.
    gwa_layout2-sel_mode   = c_a.

    PERFORM montar_layout_grid_300.
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid3.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid3.
    SET HANDLER obg_toolbar->handle_user_command FOR grid3.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_maintain_variant.
    APPEND wl_function TO tl_function3.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function3.
    wl_function2 = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function3.

    gwa_layout3-sel_mode   = 'A'.
    gwa_layout3-cwidth_opt = 'X'.
    gwa_layout3-col_opt    = 'X'.
    gwa_layout3-box_fname  = 'SELECTED'.
    gs_layout3-report = sy-repid.

    CALL METHOD grid3->set_table_for_first_display
      EXPORTING
        is_layout       = gwa_layout3
        is_variant      = gs_layout3
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = git_fieldcatalog[]
        it_outtab       = git_saidac[].

    SET HANDLER:
    lcl_event_handler=>on_double_click FOR grid3.
    SET HANDLER:
    lcl_event_handler=>on_hotsopt_click FOR grid3.
  ELSE.
    PERFORM f_contabilizacao.
    CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0300 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0300 OUTPUT.
  SET PF-STATUS 'Z003'.
  SET TITLEBAR 'Z003'.

  CALL METHOD cl_gui_cfw=>flush.
  CALL METHOD cl_gui_cfw=>dispatch.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  CASE ok_code.
    WHEN c_search OR c_buscar.
      PERFORM f_contabilizacao.
      CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.
    WHEN c_atuali.
      PERFORM f_contabilizacao.
      CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_cancel.
      LEAVE PROGRAM.
    WHEN c_exit.
      LEAVE PROGRAM.
    WHEN c_doc_cont.
      GET CURSOR FIELD fnam VALUE fval.
  ENDCASE.

  FREE ok_code.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form montar_layout_grid_300
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM montar_layout_grid_300 .

  REFRESH git_fieldcatalog.

  PERFORM montar_estrutura USING:
        01   ' '   'WAERS       ' 'GIT_SAIDAC' 'WAERS       ' 'Moeda                    '                  '20'    ' ' ' ' ' ' ' ' ' ' ,
        02   ' '   'SAKNR_CC    ' 'GIT_SAIDAC' 'SAKNR_CC    ' 'Conta cliente CutOff     '                  '20'    ' ' ' ' ' ' ' ' ' ' ,
        03   ' '   'SAKNR_RC    ' 'GIT_SAIDAC' 'SAKNR_RC    ' 'Conta receita CutOff     '                  '20'    ' ' ' ' ' ' ' ' ' ' ,
        04   ' '   'MATNR       ' 'GIT_SAIDAC' 'MATNR       ' 'Produto                  '                  '20'    ' ' ' ' ' ' ' ' ' ' ,
        05   ' '   'PARID       ' 'GIT_SAIDAC' 'PARID       ' 'Cod. Cliente             '                  '20'    ' ' ' ' ' ' ' ' ' ' ,
        06   ' '   'NAME1       ' 'GIT_SAIDAC' 'NAME1       ' 'Cliente                  '                  '30'    ' ' ' ' ' ' ' ' ' ' ,
        07   ' '   'ANZPK       ' 'GIT_SAIDAC' 'ANZPK       ' 'Quantidade               '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        08   ' '   'GEWEI       ' 'GIT_SAIDAC' 'GEWEI       ' 'Un                       '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        09   ' '   'VLR_DOLAR   ' 'GIT_SAIDAC' 'VLR_DOLAR   ' 'Valor Dolar              '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        10   ' '   'VLR_BRL     ' 'GIT_SAIDAC' 'VLR_BRL     ' 'Valor BRL                '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        11   ' '   'PRECO       ' 'GIT_SAIDAC' 'PRECO       ' 'Preço                    '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        12   ' '   'C_DOLAR     ' 'GIT_SAIDAC' 'C_DOLAR     ' 'Custo Dolar              '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        13   ' '   'C_BRL       ' 'GIT_SAIDAC' 'C_BRL       ' 'Custo BRL                '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        14   ' '   'SAKNR_P     ' 'GIT_SAIDAC' 'SAKNR_P     ' 'Custo PP ou Ver CutOff   '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        15   ' '   'SAKNR_EC    ' 'GIT_SAIDAC' 'SAKNR_EC    ' 'Conta estoque CutOff     '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        16   ' '   'MATKL       ' 'GIT_SAIDAC' 'MATKL       ' 'Grp. Mercadoria          '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        17   ' '   'RASSC       ' 'GIT_SAIDAC' 'RASSC       ' 'Sociedade Parceira       '                  '15'    ' ' ' ' ' ' ' ' ' ' ,
        18   ' '   'STATUS_1    ' 'GIT_SAIDAC' 'STATUS_1    ' 'Status Rec.              '                  '10'    ' ' ' ' 'X' ' ' 'C' ,
        19   ' '   'DOC_REC     ' 'GIT_SAIDAC' 'DOC_REC     ' 'Doc. Receita             '                  '15'    ' ' ' ' 'X' ' ' ' ' ,
        20   ' '   'ESTORNO_1   ' 'GIT_SAIDAC' 'ESTORNO_1   ' 'Estorno Receita          '                  '15'    ' ' ' ' 'X' ' ' ' ' ,
        21   ' '   'STATUS_2    ' 'GIT_SAIDAC' 'STATUS_2    ' 'Status Custo             '                  '10'    ' ' ' ' 'X' ' ' 'C' ,
        22   ' '   'DOC_CUST    ' 'GIT_SAIDAC' 'DOC_CUST    ' 'Doc. Custo               '                  '15'    ' ' ' ' 'X' ' ' ' ' ,
        23   ' '   'ESTORNO_2   ' 'GIT_SAIDAC' 'ESTORNO_2   ' 'Estorno Custo            '                  '15'    ' ' ' ' 'X' ' ' ' ' ,
        24   ' '   'DOC_REV_R   ' 'GIT_SAIDAC' 'DOC_REV_R   ' 'Doc. Rev. Receita        '                  '15'    ' ' ' ' 'X' ' ' ' ' ,
        25   ' '   'ESTORNO_3   ' 'GIT_SAIDAC' 'ESTORNO_3   ' 'Estorno Custo            '                  '15'    ' ' ' ' 'X' ' ' ' ' ,
        26   ' '   'DOC_REV_C   ' 'GIT_SAIDAC' 'DOC_REV_C   ' 'Doc. Rev. Custo          '                  '15'    ' ' ' ' 'X' ' ' ' ' ,
        27   ' '   'ESTORNO_4   ' 'GIT_SAIDAC' 'ESTORNO_4   ' 'Estorno Custo            '                  '15'    ' ' ' ' 'X' ' ' ' ' .


ENDFORM.

FORM f_ger_doc_receita . "Gerar Documento Receita

  READ TABLE git_saidac INTO DATA(_notexist_obj_key_4) WITH KEY obj_key_1 = ''.
  IF sy-subrc = 0.
    FREE: git_zglt0004,it_zib_contabil.

    DATA: lv_index(6) TYPE n.
    DATA: lv_seq(6)   TYPE n.
    DATA: vseq(6)     TYPE p.

    CLEAR:gwa_saidac,wa_zib_contabil.
    LOOP AT git_saidac INTO gwa_saidac WHERE waers = 'BRL' OR waers = 'USD'.

      gwa_saidac-estorno_1 = abap_false.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_OFF'
        IMPORTING
          number      = vseq.

      lv_seq = vseq.

      DATA(lv_lin) = abap_off.

      DATA(_obj_key)    =  'CUTOFF' && lv_seq && gwa_saidac-bukrs && gwa_saidac-gjahr.
      DATA(_gsber) = gwa_saidac-bukrs+2(2) && '01'.
      DO 2 TIMES.

        lv_index = lv_index + 1.
        lv_lin = lv_lin + 1.
        wa_zib_contabil-obj_key    =  _obj_key.
        wa_zib_contabil-seqitem    =  lv_index.
        wa_zib_contabil-gsber = _gsber.
        wa_zib_contabil-bukrs      =  gwa_saidac-bukrs.
        wa_zib_contabil-interface  =  '0'.
        wa_zib_contabil-bktxt      =  'CUT OFF'.
        CONCATENATE  s_budat-high+6(2) s_budat-high+4(2) s_budat-high+0(4) INTO wa_zib_contabil-bldat SEPARATED BY '.'.
        CONCATENATE  s_budat-high+6(2) s_budat-high+4(2) s_budat-high+0(4) INTO wa_zib_contabil-budat SEPARATED BY '.'.
        wa_zib_contabil-gjahr      = gwa_saidac-gjahr.
        wa_zib_contabil-monat      = s_budat-high+4(2).
        wa_zib_contabil-blart      = 'LM'.
        wa_zib_contabil-vbund          = gwa_saidac-rassc.
        wa_zib_contabil-rg_atualizado  = 'N'.
        wa_zib_contabil-bupla      = gwa_saidac-bukrs.
        wa_zib_contabil-sgtxt      = 'CUT OFF CLIENTE X RECEITA'.

        IF lv_lin EQ '1'.
          wa_zib_contabil-matnr          = gwa_saidac-matnr.
          wa_zib_contabil-quantity       = gwa_saidac-anzpk.
          wa_zib_contabil-base_uom       = gwa_saidac-gewei.
          wa_zib_contabil-prctr      = '0000009900'.
          wa_zib_contabil-hkont      = gwa_saidac-saknr_rc.
          wa_zib_contabil-bschl    =  '50'.
        ELSE.
          wa_zib_contabil-matnr          = ''.
          wa_zib_contabil-quantity       = ''.
          wa_zib_contabil-base_uom       = ''.
          wa_zib_contabil-prctr      = ''.
          wa_zib_contabil-hkont      = gwa_saidac-saknr_cc.
          wa_zib_contabil-bschl    =  '40'.
        ENDIF.

        IF gwa_saidac-waers EQ 'BRL'.
          wa_zib_contabil-wrbtr      = gwa_saidac-vlr_brl.
          wa_zib_contabil-waers      = gwa_saidac-waers. "gwa_saidac-waers.
          wa_zib_contabil-waers_i    = gwa_saidac-waers.
          wa_zib_contabil-dmbtr      = gwa_saidac-vlr_brl.
          wa_zib_contabil-waers_f    = 'USD'.
          wa_zib_contabil-dmbe2      = gwa_saidac-vlr_dolar.
        ELSEIF gwa_saidac-waers EQ 'USD'.
          wa_zib_contabil-wrbtr      = gwa_saidac-vlr_dolar.
          wa_zib_contabil-waers      = gwa_saidac-waers."gwa_saidac-waers.
          wa_zib_contabil-waers_i    = 'BRL'.
          wa_zib_contabil-dmbtr      = gwa_saidac-vlr_brl.
          wa_zib_contabil-waers_f    = gwa_saidac-waers.
          wa_zib_contabil-dmbe2      = gwa_saidac-vlr_dolar.
        ENDIF.

        APPEND wa_zib_contabil TO it_zib_contabil.

        CLEAR: wa_zib_contabil.

      ENDDO.

      MOVE-CORRESPONDING gwa_saidac TO wa_zglt0004.
      wa_zglt0004-obj_key_1 = _obj_key.
      APPEND wa_zglt0004 TO git_zglt0004.
      CLEAR: wa_zglt0004,gwa_saidac,lv_index,lv_seq,vseq.
    ENDLOOP.

    IF git_zglt0004 IS NOT INITIAL AND it_zib_contabil IS NOT INITIAL.

      MODIFY zib_contabil  FROM TABLE it_zib_contabil.
      COMMIT WORK AND WAIT.

      LOOP AT git_zglt0004 ASSIGNING FIELD-SYMBOL(<fs_zglt0004>).

        SELECT SINGLE * FROM zglt0004 WHERE id = @<fs_zglt0004>-id INTO @DATA(ls_zglt0004).

        IF sy-subrc = 0.
          UPDATE zglt0004 SET obj_key_1 = <fs_zglt0004>-obj_key_1 WHERE id = <fs_zglt0004>-id.
        ELSE.
          INSERT zglt0004 FROM <fs_zglt0004>.
        ENDIF.

      ENDLOOP.

      COMMIT WORK AND WAIT.

*    MODIFY zglt0004  FROM TABLE git_zglt0004.
*    COMMIT WORK AND WAIT.

      FREE: git_zglt0004,it_zib_contabil.
      PERFORM f_contabilizacao.
      CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.

    ENDIF.
  ELSE.

    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'já gerado Documento Receita!'.

  ENDIF.
ENDFORM.

FORM f_ger_est_receita . "Gerar Estorno Documento Receita

  READ TABLE git_saidac INTO DATA(_notexist_obj_key_3) WITH KEY obj_key_3 = ''.

  IF sy-subrc = 0.

    DATA: messtab TYPE bdcmsgcoll OCCURS 0,
          wa_mess TYPE bdcmsgcoll,
          ok      TYPE c,
          data    TYPE n LENGTH 10.

    DATA:
      vl_budat    TYPE char10,
      vl_data_doc TYPE char6,
      vl_erro     TYPE char1.


    DATA: lv_index(6) TYPE n.

    LOOP AT git_saidac INTO gwa_saidac.

      data = sy-datum.
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4)   INTO data SEPARATED BY '.'.

      "mesmo mes
      PERFORM batch_input USING: 'X' 'SAPMF05A'     '0105',
            ''  'BDC_CURSOR'   'UF05A-STGRD',
            ''  'BDC_OKCODE'   '=BU',
            ''  'RF05A-BELNS'  gwa_saidac-doc_rec,
            ''  'BKPF-BUKRS'   gwa_saidac-bukrs,
            ''  'RF05A-GJAHS'  gwa_saidac-gjahr(4),
            ''  'UF05A-STGRD'  '01'.

      CALL TRANSACTION 'FB08' USING it_bdc MODE 'N' MESSAGES INTO messtab."OPTIONS FROM OPT.

      IF ( sy-subrc EQ 0 ) .


        CLEAR: vl_erro.
        FREE: git_zglt0004.
        DATA: lva_budat(10) TYPE c,
              lva_stblg     TYPE bkpf-stblg.

        SELECT SINGLE stblg
        FROM bkpf INTO lva_stblg
        WHERE bukrs = gwa_saidac-bukrs
        AND belnr = gwa_saidac-doc_rec
        AND gjahr = gwa_saidac-gjahr(4).

        CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

        gwa_saidac-estorno_3      = lva_stblg.

        UPDATE zglt0004 SET obj_key_3 = gwa_saidac-estorno_3 WHERE id = gwa_saidac-id.
        COMMIT WORK AND WAIT.

        PERFORM f_contabilizacao.
        CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.

      ELSE.

        READ TABLE messtab INTO wa_mess WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL.
          MESSAGE ID wa_mess-msgid TYPE 'S' NUMBER wa_mess-msgnr WITH wa_mess-msgv1 wa_mess-msgv2 wa_mess-msgv3 wa_mess-msgv4
          DISPLAY LIKE wa_mess-msgtyp.
        ELSE.
          MESSAGE s888(sabapdocu) WITH 'Documento já compensado.'.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ELSE.

    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existe um Estorno de Receita!'.

  ENDIF.
ENDFORM.

FORM f_ger_rev_doc_receita . "Gerar Documento Reversão Receita

  READ TABLE git_saidac INTO DATA(_notexist_obj_key_3) WITH KEY obj_key_4 = ''.

  IF sy-subrc = 0.

    FREE: git_zglt0004.
    "DATA: lv_index(6) TYPE n.
    DATA: lv_seq(6)   TYPE n.
    DATA: vseq(6)   TYPE p.
    CLEAR:gwa_saidac.

    LOOP AT git_saidac ASSIGNING FIELD-SYMBOL(<fs_saidac>).

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_OFF'
        IMPORTING
          number      = vseq.

      lv_seq = vseq.

      DATA(_obj_key)    =  'CUTOFF' && lv_seq && gwa_saidac-bukrs && gwa_saidac-gjahr.

      SELECT * FROM zib_contabil WHERE obj_key EQ @<fs_saidac>-obj_key_1 INTO TABLE @DATA(it_zib_contabil).
      IF it_zib_contabil IS NOT INITIAL.
        LOOP AT it_zib_contabil ASSIGNING FIELD-SYMBOL(<fs_zib_contabil>).

          IF sy-tabix = 1.
            <fs_zib_contabil>-bschl = '40'.
          ELSEIF sy-tabix = 2.
            <fs_zib_contabil>-bschl = '50'.
          ENDIF.

          <fs_zib_contabil>-obj_key = _obj_key.
          <fs_zib_contabil>-rg_atualizado = 'N'.
          <fs_zib_contabil>-xblnr = <fs_saidac>-obj_key_1.
        ENDLOOP.
        MODIFY zib_contabil  FROM TABLE it_zib_contabil.
        COMMIT WORK AND WAIT.

        UPDATE zglt0004 SET obj_key_3 = _obj_key WHERE id = <fs_saidac>-id.
        COMMIT WORK AND WAIT.

      ENDIF.

    ENDLOOP.

    PERFORM f_contabilizacao.
    CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.

  ELSE.

    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'já gerado Documento Reversão Receita!'.

  ENDIF.
ENDFORM.

FORM f_ger_rev_est_doc_receita . " Gerar estorno documento receita
  READ TABLE git_saidac INTO DATA(_notexist_obj_key_3) WITH KEY obj_key_3 = ''.

  IF sy-subrc = 0.

    DATA: messtab TYPE bdcmsgcoll OCCURS 0,
          wa_mess TYPE bdcmsgcoll,
          ok      TYPE c,
          data    TYPE n LENGTH 10.

    DATA:
      vl_budat    TYPE char10,
      vl_data_doc TYPE char6,
      vl_erro     TYPE char1.


    DATA: lv_index(6) TYPE n.

    LOOP AT git_saidac INTO gwa_saidac.

      data = sy-datum.
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4)   INTO data SEPARATED BY '.'.

      "mesmo mes
      PERFORM batch_input USING: 'X' 'SAPMF05A'     '0105',
            ''  'BDC_CURSOR'   'UF05A-STGRD',
            ''  'BDC_OKCODE'   '=BU',
            ''  'RF05A-BELNS'  gwa_saidac-doc_rev_r,
            ''  'BKPF-BUKRS'   gwa_saidac-bukrs,
            ''  'RF05A-GJAHS'  gwa_saidac-gjahr(4),
            ''  'UF05A-STGRD'  '01'.

      CALL TRANSACTION 'FB08' USING it_bdc MODE 'N' MESSAGES INTO messtab."OPTIONS FROM OPT.

      IF ( sy-subrc EQ 0 ) .


        CLEAR: vl_erro.
        FREE: git_zglt0004.
        DATA: lva_budat(10) TYPE c,
              lva_stblg     TYPE bkpf-stblg.

        SELECT SINGLE stblg
        FROM bkpf INTO lva_stblg
        WHERE bukrs = gwa_saidac-bukrs
        AND belnr = gwa_saidac-doc_rev_r
        AND gjahr = gwa_saidac-gjahr(4).

        CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

        gwa_saidac-estorno_3      = lva_stblg.

        UPDATE zglt0004 SET obj_key_3 = gwa_saidac-estorno_3 WHERE id = gwa_saidac-id.
        COMMIT WORK AND WAIT.

        PERFORM f_contabilizacao.
        CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.

      ELSE.

        READ TABLE messtab INTO wa_mess WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL.
          MESSAGE ID wa_mess-msgid TYPE 'S' NUMBER wa_mess-msgnr WITH wa_mess-msgv1 wa_mess-msgv2 wa_mess-msgv3 wa_mess-msgv4
          DISPLAY LIKE wa_mess-msgtyp.
        ELSE.
          MESSAGE s888(sabapdocu) WITH 'Documento já compensado.'.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ELSE.

    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existe um Estorno de Receita!'.

  ENDIF.
ENDFORM.

FORM f_ger_doc_custo .  " Gerar Documento de Custo

  READ TABLE git_saidac INTO DATA(_notexist_obj_key_4) WITH KEY obj_key_2 = ''.

  IF sy-subrc = 0.

    FREE: git_zglt0004,it_zib_contabil.

    DATA: lv_index(6) TYPE n.
    DATA: lv_seq(6)   TYPE n.
    DATA: vseq(6)     TYPE p.

    CLEAR:gwa_saidac,wa_zib_contabil.
    LOOP AT git_saidac INTO gwa_saidac WHERE waers EQ 'BRL'.

      gwa_saidac-estorno_2 = abap_false.

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_OFF'
        IMPORTING
          number      = vseq.

      lv_seq = vseq.

      DATA(lv_lin) = abap_off.

      DATA(_obj_key)    =  'CUTOFF' && lv_seq && gwa_saidac-bukrs && gwa_saidac-gjahr.
      DATA(_gsber) = gwa_saidac-bukrs+2(2) && '01'.
      DO 2 TIMES.

        lv_index = lv_index + 1.
        lv_lin = lv_lin + 1.
        wa_zib_contabil-obj_key    =  _obj_key.
        wa_zib_contabil-seqitem    =  lv_index.
        wa_zib_contabil-gsber = _gsber.
        wa_zib_contabil-bukrs      =  gwa_saidac-bukrs.
        wa_zib_contabil-interface  =  '0'.
        wa_zib_contabil-bktxt      =  'CUT OFF'.
        CONCATENATE  s_budat-high+6(2) s_budat-high+4(2) s_budat-high+0(4) INTO wa_zib_contabil-bldat SEPARATED BY '.'.
        CONCATENATE  s_budat-high+6(2) s_budat-high+4(2) s_budat-high+0(4) INTO wa_zib_contabil-budat SEPARATED BY '.'.
        wa_zib_contabil-gjahr      = gwa_saidac-gjahr.
        wa_zib_contabil-monat      = s_budat-high+4(2).
        wa_zib_contabil-blart      = 'LM'.
        wa_zib_contabil-vbund          = gwa_saidac-rassc.
        wa_zib_contabil-rg_atualizado  = 'N'.
        wa_zib_contabil-bupla      = gwa_saidac-bukrs.
        wa_zib_contabil-sgtxt      = 'CUT OFF CLIENTE X RECEITA'.

        IF lv_lin EQ '1'.
          wa_zib_contabil-matnr          = gwa_saidac-matnr.
          wa_zib_contabil-quantity       = gwa_saidac-anzpk.
          wa_zib_contabil-base_uom       = gwa_saidac-gewei.
          wa_zib_contabil-prctr      = '0000009900'.
          wa_zib_contabil-hkont      = gwa_saidac-saknr_p.
          wa_zib_contabil-bschl    =  '50'.
        ELSE.
          wa_zib_contabil-matnr          = ''.
          wa_zib_contabil-quantity       = ''.
          wa_zib_contabil-base_uom       = ''.
          wa_zib_contabil-prctr      = ''.
          wa_zib_contabil-hkont      = gwa_saidac-saknr_ec.
          wa_zib_contabil-bschl    =  '40'.
        ENDIF.

        "Somente BRL
        wa_zib_contabil-wrbtr      = gwa_saidac-vlr_brl.
        wa_zib_contabil-waers      = gwa_saidac-waers. "'BRL'.
        wa_zib_contabil-waers_i    = gwa_saidac-waers. "'BRL'.
        wa_zib_contabil-dmbtr      = gwa_saidac-vlr_brl.
        wa_zib_contabil-waers_f    = 'USD'.
        wa_zib_contabil-dmbe2      = gwa_saidac-vlr_dolar.


        APPEND wa_zib_contabil TO it_zib_contabil.

        CLEAR: wa_zib_contabil.

      ENDDO.

      MOVE-CORRESPONDING gwa_saidac TO wa_zglt0004.
      wa_zglt0004-obj_key_2 = _obj_key.
      APPEND wa_zglt0004 TO git_zglt0004.
      CLEAR: wa_zglt0004,gwa_saidac,lv_index,lv_seq,vseq.
    ENDLOOP.

    IF git_zglt0004 IS NOT INITIAL AND it_zib_contabil IS NOT INITIAL.

      MODIFY zib_contabil  FROM TABLE it_zib_contabil.
      COMMIT WORK AND WAIT.

      LOOP AT git_zglt0004 ASSIGNING FIELD-SYMBOL(<fs_zglt0004>).
        SELECT SINGLE * FROM zglt0004 WHERE id = @<fs_zglt0004>-id INTO @DATA(ls_zglt0004).
        IF sy-subrc = 0.
          UPDATE zglt0004 SET obj_key_2 = <fs_zglt0004>-obj_key_2 WHERE id = <fs_zglt0004>-id.
        ELSE.
          INSERT zglt0004 FROM <fs_zglt0004>.
        ENDIF.
        COMMIT WORK AND WAIT.
      ENDLOOP.

*    MODIFY zglt0004  FROM TABLE git_zglt0004.
*    COMMIT WORK AND WAIT.

      FREE: git_zglt0004,it_zib_contabil.

      PERFORM f_contabilizacao.
      CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.

    ENDIF.
  ELSE.

    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'já gerado Documento de Custo!'.

  ENDIF.
ENDFORM.

FORM f_ger_est_custo .  " Gerar Estorno de Custo "mexendo

  READ TABLE git_saidac INTO DATA(_notexist_obj_key_4) WITH KEY obj_key_4 = ''.

  IF sy-subrc = 0.

    DATA: messtab TYPE bdcmsgcoll OCCURS 0,
          wa_mess TYPE bdcmsgcoll,
          ok      TYPE c,
          data    TYPE n LENGTH 10.

    DATA:
      vl_budat    TYPE char10,
      vl_data_doc TYPE char6,
      vl_erro     TYPE char1.


    DATA: lv_index(6) TYPE n.

    LOOP AT git_saidac INTO gwa_saidac.

      data = sy-datum.
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4)   INTO data SEPARATED BY '.'.

      "mesmo mes
      PERFORM batch_input USING: 'X' 'SAPMF05A'     '0105',
            ''  'BDC_CURSOR'   'UF05A-STGRD',
            ''  'BDC_OKCODE'   '=BU',
            ''  'RF05A-BELNS'  gwa_saidac-doc_cust,
            ''  'BKPF-BUKRS'   gwa_saidac-bukrs,
            ''  'RF05A-GJAHS'  gwa_saidac-gjahr(4),
            ''  'UF05A-STGRD'  '01'.

      CALL TRANSACTION 'FB08' USING it_bdc MODE 'N' MESSAGES INTO messtab."OPTIONS FROM OPT.

      IF ( sy-subrc EQ 0 ) .


        CLEAR: vl_erro.
        FREE: git_zglt0004.
        DATA: lva_budat(10) TYPE c,
              lva_stblg     TYPE bkpf-stblg.

        SELECT SINGLE stblg
        FROM bkpf INTO lva_stblg
        WHERE bukrs = gwa_saidac-bukrs
        AND belnr = gwa_saidac-doc_cust
        AND gjahr = gwa_saidac-gjahr(4).

        CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

        gwa_saidac-estorno_4      = lva_stblg.

        UPDATE zglt0004 SET obj_key_4 = gwa_saidac-estorno_4 WHERE id = gwa_saidac-id.
        COMMIT WORK AND WAIT.

        PERFORM f_contabilizacao.
        CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.

      ELSE.

        READ TABLE messtab INTO wa_mess WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL.
          MESSAGE ID wa_mess-msgid TYPE 'S' NUMBER wa_mess-msgnr WITH wa_mess-msgv1 wa_mess-msgv2 wa_mess-msgv3 wa_mess-msgv4
          DISPLAY LIKE wa_mess-msgtyp.
        ELSE.
          MESSAGE s888(sabapdocu) WITH 'Documento já compensado.'.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ELSE.

    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existe um Estorno de Custo!'.

  ENDIF.

ENDFORM.

FORM f_ger_rev_doc_custo . " Gerar reversão documento custo

  READ TABLE git_saidac INTO DATA(_notexist_obj_key_4) WITH KEY obj_key_4 = ''.
  IF sy-subrc = 0.

    FREE: git_zglt0004.
    "DATA: lv_index(6) TYPE n.
    DATA: lv_seq(6)   TYPE n.
    DATA: vseq(6)   TYPE p.
    CLEAR:gwa_saidac.

    LOOP AT git_saidac ASSIGNING FIELD-SYMBOL(<fs_saidac>).

      CALL FUNCTION 'NUMBER_GET_NEXT'
        EXPORTING
          nr_range_nr = '01'
          object      = 'ZID_OFF'
        IMPORTING
          number      = vseq.

      lv_seq = vseq.

      DATA(_obj_key)    =  'CUTOFF' && lv_seq && gwa_saidac-bukrs && gwa_saidac-gjahr.

      SELECT * FROM zib_contabil WHERE obj_key EQ @<fs_saidac>-obj_key_2 INTO TABLE @DATA(it_zib_contabil).
      IF it_zib_contabil IS NOT INITIAL.
        LOOP AT it_zib_contabil ASSIGNING FIELD-SYMBOL(<fs_zib_contabil>).

          IF sy-tabix = 1.
            <fs_zib_contabil>-bschl = '40'.
          ELSEIF sy-tabix = 2.
            <fs_zib_contabil>-bschl = '50'.
          ENDIF.

          <fs_zib_contabil>-obj_key = _obj_key.
          <fs_zib_contabil>-rg_atualizado = 'N'.
          <fs_zib_contabil>-xblnr = <fs_saidac>-obj_key_2.
        ENDLOOP.
        MODIFY zib_contabil  FROM TABLE it_zib_contabil.
        COMMIT WORK AND WAIT.

        UPDATE zglt0004 SET obj_key_4 = _obj_key WHERE id = <fs_saidac>-id.
        COMMIT WORK AND WAIT.

      ENDIF.
    ENDLOOP.

    PERFORM f_contabilizacao.
    CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.
  ELSE.

    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'já gerado Reversão de Documento de Custo!'.

  ENDIF.
ENDFORM.

FORM f_ger_rev_est_doc_custo . " Gerar reversão estorno documento custo

  DATA: messtab TYPE bdcmsgcoll OCCURS 0,
        wa_mess TYPE bdcmsgcoll,
        ok      TYPE c,
        data    TYPE n LENGTH 10.

  DATA:
    vl_budat    TYPE char10,
    vl_data_doc TYPE char6,
    vl_erro     TYPE char1.

  DATA: lv_index(6) TYPE n.

  LOOP AT git_saidac INTO wl_index_rows.

    DATA(lv_tabix) = sy-tabix.

    READ TABLE git_saidac INTO gwa_saidac INDEX lv_tabix."wl_index_rows-index.
    IF sy-subrc IS INITIAL AND gwa_saidac-doc_rev_c IS NOT INITIAL.

      CLEAR: ok.

      data = sy-datum.
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4)   INTO data SEPARATED BY '.'.

      "mesmo mes
      PERFORM batch_input USING: 'X' 'SAPMF05A'     '0105',
            ''  'BDC_CURSOR'   'UF05A-STGRD',
            ''  'BDC_OKCODE'   '=BU',
            ''  'RF05A-BELNS'  gwa_saidac-doc_rev_c,
            ''  'BKPF-BUKRS'   gwa_saidac-bukrs,
            ''  'RF05A-GJAHS'  gwa_saidac-gjahr(4),
            ''  'UF05A-STGRD'  '01'.

      CALL TRANSACTION 'FB08' USING it_bdc MODE 'N' MESSAGES INTO messtab."OPTIONS FROM OPT.

      IF ( sy-subrc EQ 0 ) .

        LOOP AT messtab INTO wa_mess.
          IF ( wa_mess-msgtyp EQ 'S' ) AND NOT ( wa_mess-msgv1 IS INITIAL ) .
            ok = 'X'.
          ENDIF.
        ENDLOOP.


        CASE ok.
          WHEN: 'X'.
            CLEAR: vl_erro.
            FREE: git_zglt0004.
            DATA: lva_budat(10) TYPE c,
                  lva_stblg     TYPE bkpf-stblg.

            SELECT SINGLE stblg
            FROM bkpf INTO lva_stblg
            WHERE bukrs = gwa_saidac-bukrs
            AND belnr = gwa_saidac-doc_rev_c
            AND gjahr = gwa_saidac-gjahr(4).

            CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

            gwa_saidac-estorno_4      = lva_stblg.
            MOVE: icon_led_yellow TO gwa_saidac-status_2.
            MODIFY git_saidac FROM gwa_saidac INDEX lv_tabix TRANSPORTING status_2 estorno_4.
            COMMIT WORK AND WAIT.

            gwa_zglt0004 = CORRESPONDING #( gwa_saidac ).
            MOVE-CORRESPONDING gwa_zglt0004 TO wa_zglt0004.
            APPEND wa_zglt0004 TO git_zglt0004.
            CLEAR: gwa_zglt0004,wa_zglt0004.

            MODIFY zglt0004  FROM TABLE git_zglt0004.
            COMMIT WORK AND WAIT.
            FREE: git_zglt0004.

            PERFORM f_contabilizacao.
            CALL METHOD grid3->refresh_table_display EXPORTING is_stable = _stable.

        ENDCASE.

      ELSE.

        READ TABLE messtab INTO wa_mess WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL.
          MESSAGE ID wa_mess-msgid TYPE 'S' NUMBER wa_mess-msgnr WITH wa_mess-msgv1 wa_mess-msgv2 wa_mess-msgv3 wa_mess-msgv4
          DISPLAY LIKE wa_mess-msgtyp.
        ELSE.
          MESSAGE s888(sabapdocu) WITH 'Documento já compensado.'.
        ENDIF.

      ENDIF.

    ELSEIF gwa_saidac-doc_rev_c IS INITIAL.

      DATA(lv_erro) = abap_true.

    ENDIF.
  ENDLOOP.

  IF lv_erro IS NOT INITIAL.
    MESSAGE e888(sabapdocu) WITH 'Documento(s) não existe(m) ou não gerado.' 'Verificar!'.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  BATCH_INPUT
*&---------------------------------------------------------------------*
FORM batch_input  USING     VALUE(p_flag)
      VALUE(p_fnam)
      VALUE(p_fval).

  CLEAR it_bdc.

  IF NOT p_flag IS INITIAL.
    it_bdc-program  = p_fnam.
    it_bdc-dynpro   = p_fval.
    it_bdc-dynbegin = 'X'.
  ELSE.
    it_bdc-fnam = p_fnam.
    it_bdc-fval = p_fval.
  ENDIF.

  APPEND it_bdc.

ENDFORM.                    "batch_input

FORM f_exibe_erro_zib  USING index  TYPE lvc_index
      status TYPE char10.

  TYPES: BEGIN OF ty_zib_err,
           obj_key        TYPE zib_contabil_err-obj_key,
           dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
           hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
           message        TYPE zib_contabil_err-message,
         END OF ty_zib_err.

  DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

  READ TABLE git_saidac INTO DATA(wa_saida) INDEX index.

  CASE status.
    WHEN 'STATUS_1'.
      IF wa_saida-obj_key_1 IS NOT INITIAL.
        SELECT obj_key, dt_atualizacao, hr_atualizacao, message
        FROM zib_contabil_err INTO TABLE @lit_zib_err
        WHERE obj_key = @wa_saida-obj_key_1.
      ENDIF.
    WHEN 'STATUS_2'.
      IF wa_saida-obj_key_2 IS NOT INITIAL.
        SELECT obj_key, dt_atualizacao, hr_atualizacao, message
        FROM zib_contabil_err APPENDING TABLE @lit_zib_err
        WHERE obj_key = @wa_saida-obj_key_2.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  IF lit_zib_err[] IS NOT INITIAL.
    cl_demo_output=>new(
    )->begin_section( `ZIB_CONTABIL_ERR:`
    )->write_text( |Erros encontrados NA crição DO documento: \n|
    ")->WRITE_DATA( SY-DATUM
    )->write_data( lit_zib_err[]
    )->end_section(
    )->display( ).
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nenhum resultado encontrado!'.
  ENDIF.
ENDFORM.

FORM f_processo_material .

  FREE:git_acdocaf[].
  CLEAR: lr_poper.

  LOOP AT git_zglt0003 INTO DATA(wa_zglt0003) WHERE tipo EQ '3'.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zglt0003-conta_ori
      IMPORTING
        output = wa_zglt0003-conta_ori.

    lr_racct-sign = 'I'.
    lr_racct-option = 'EQ'.
    lr_racct-low = wa_zglt0003-conta_ori.
    APPEND lr_racct TO lr_racct.
    CLEAR lr_racct.
  ENDLOOP.

  SORT lr_racct.
  DELETE ADJACENT DUPLICATES FROM lr_racct.

  SELECT DISTINCT * FROM zi_zgl0001_02(
  p_bukrs = @s_bukrs-low
  , p_gjahr = @p_ano
  , p_poper = @p_poper
  ) WHERE racct IN @lr_racct[]
  INTO TABLE @DATA(getList_acdoca).

  IF getList_acdoca IS NOT INITIAL.
    SORT getList_acdoca.

    LOOP AT getList_acdoca ASSIGNING FIELD-SYMBOL(<fs_acdocax>).
      gaw_acdocaf-rbukrs = <fs_acdocax>-bukrs.
      gaw_acdocaf-gjahr = <fs_acdocax>-gjahr.
      gaw_acdocaf-matnr = <fs_acdocax>-matnr.
      gaw_acdocaf-racct = <fs_acdocax>-racct.
      gaw_acdocaf-poper = <fs_acdocax>-poper.

      SELECT SINGLE * FROM zi_zgl0001_01(
      p_racct = @<fs_acdocax>-racct
      , p_bukrs = @<fs_acdocax>-bukrs
      , p_gjahr = @<fs_acdocax>-gjahr
      , p_poper = @<fs_acdocax>-poper
      , p_matnr = @<fs_acdocax>-matnr
      ) INTO @DATA(get_valores).

      gaw_acdocaf-ksl   =  get_valores-ksl.
      gaw_acdocaf-msl   =  get_valores-msl.
      gaw_acdocaf-tsl   =  get_valores-tsl.

      IF get_valores-msl NE 0 AND get_valores-tsl NE 0.
        gaw_acdocaf-precob =  get_valores-tsl / get_valores-msl.
      ENDIF.
      IF get_valores-msl NE 0 AND get_valores-tsl EQ 0.
        gaw_acdocaf-precob =  get_valores-tsl.
      ENDIF.
      IF get_valores-ksl NE 0 AND get_valores-msl NE 0.
        gaw_acdocaf-preco =  get_valores-ksl / get_valores-msl.
      ENDIF.
      IF get_valores-ksl NE 0 AND get_valores-msl EQ 0.
        gaw_acdocaf-preco =  get_valores-ksl.
      ENDIF.

      APPEND gaw_acdocaf TO git_acdocaf.
      CLEAR:gaw_acdocaf.
    ENDLOOP.

    FREE: getList_acdoca.
  ENDIF.

ENDFORM.

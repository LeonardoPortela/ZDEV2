*&--------------------------------------------------------------------&*
*& Projeto..: AMAGGI                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 10.02.2025                                              &*
*& Descrição: Calculo de PIS/Cofins sobre a Apropriação Mensal        &*
*& Transação: ZFIS35                                                  &*
*&--------------------------------------------------------------------&*
*& Projeto  : Os Ninjas Evolution                                     &*
*& Código Espec.Funcional/Técnica: Carolini Santos / Ronaldo Freitas  &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*
REPORT  zfis0003 NO STANDARD PAGE HEADING MESSAGE-ID fi.
INCLUDE <icon>.

TABLES: t001, skb1, ska1, bsis, sscrfields, zfit0198, bseg, j_1bnfdoc.

TYPES:
  BEGIN OF ty_saidas,
    mark,
    bukrs          TYPE bseg-bukrs,
    gjahr          TYPE gjahr,
    desc_bukrs     TYPE t001-butxt,
    gsber          TYPE bseg-gsber,             "*Filial   GSBER
    mesano         TYPE num6,
    desc_gsber     TYPE t001w-name1,
    belnr          TYPE bseg-belnr,             "*Documento   BELNR
    buzei          TYPE bseg-buzei,             "Item   BUZEI             "<<<------"190717 - NMS ------->>>
    hkont          TYPE bseg-hkont,             "*Conta   HKONT
    doc_lcto       TYPE zglt073-doc_lcto,       "*Seq. Lcto   DOC_LCTO
    nr_item        TYPE zglt073-nr_item,        "*Item   NR_ITEM
    nro_parc       TYPE zglt073-nro_parc,       "*Nº Parcela NRO_PARC
    nro_apolice    TYPE zglt050-nro_apolice,    "*Nº Apolice  NRO_APOLICE
    cod_seguradora TYPE zglt050-cod_seguradora, "*Seguradora   COD_SEGURADORA
    anln1          TYPE zglt073-anln1,          "*Imobilizado  ANLN1
    anln2          TYPE zglt073-anln2,          "*Sub-número  ANLN2
    matnr          TYPE zglt073-matnr,          "*Material  MATNR
    descr_bens     TYPE zglt073-descr_bens,     "*Desc. Bens   DESCR_BENS
    kostl          TYPE bseg-kostl,             "*Centro Custo   KOSTL
    h_waers        TYPE bseg-h_waers,           "*Moeda Doc  H_WAERS
    vlr_premio_brl TYPE zglt050-vlr_premio_brl, "*Valor Prêmio BRL  VLR_PREMIO_BRL
    vlr_premio_usd TYPE zglt050-vlr_premio_usd, "*Valor Prêmio USD   VLR_PREMIO_USD
    dmbtr          TYPE bseg-dmbtr,             "*Valor Despesa BRL   DMBTR
    dmbe2          TYPE bseg-dmbe2,             "*Valor Despesa USD   BSEG-DMBE2
    pis            TYPE bseg-dmbtr,             "*PIS  VLR_PIS
    cofins         TYPE bseg-dmbtr,             "*COFINS   VLR_COFINS
    status         TYPE char4,
    doc            TYPE bseg-belnr,             "DOC_CONTABIL
    est            TYPE bseg-belnr,             "DOC_ESTORNO
    obj_key        TYPE awkey,
    marca          TYPE c,
  END OF ty_saidas.

CLASS:lcl_alv_toolbar DEFINITION DEFERRED.

DATA: g_container          TYPE scrfname VALUE 'CC_GRID1',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      grid3                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container.

DATA : ty_toolbar      TYPE stb_button,
       tl_index_rows   TYPE lvc_t_row,
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
      gv_ukurs          TYPE tcurr-ukurs,
      gwa_stable3       TYPE lvc_s_stbl.

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
           c_limp_d(6)      TYPE c VALUE 'LIMP_D',

           c_gr_cus(6)      TYPE c VALUE 'GR_CUS',
           c_er_cus(6)      TYPE c VALUE 'ER_CUS',

           cg_repros(6)     TYPE c VALUE 'REPROS',    "<<<------"189147 - NMS ------->>>

           c_buscar(6)      TYPE c VALUE 'BUSCAR',
           c_doc_cont(8)    TYPE c VALUE 'DOC_CONT',
           c_doc_est(7)     TYPE c VALUE 'DOC_EST',
           c_clos_itens(10) TYPE c VALUE 'CLOS_ITENS'.

DATA: lr_poper TYPE RANGE OF poper WITH HEADER LINE,
      lr_gjahr TYPE RANGE OF gjahr WITH HEADER LINE,
      lr_racct TYPE RANGE OF racct WITH HEADER LINE.

DATA:
  it_bdc         TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
  git_saidas     TYPE TABLE OF ty_saidas,
  git_saidas_2   TYPE TABLE OF ty_saidas,
  git_saidas_3   TYPE TABLE OF ty_saidas,
  git_saidas_4   TYPE TABLE OF ty_saidas,
  gwa_saidas     LIKE LINE  OF git_saidas,
  gwa_saidas_aux LIKE LINE  OF git_saidas, "US - 179242 - CBRAND
  git_zfit0016   TYPE TABLE OF zfit0016,
  git_zfit0016x  TYPE TABLE OF ty_saidas,
  gwa_zfit0016   LIKE LINE  OF git_zfit0016.

DATA:
  git_tcurr TYPE TABLE OF tcurr     WITH HEADER LINE,
  git_dta   TYPE TABLE OF bdcdata,
  opt       TYPE ctu_params,
  git_msg   TYPE TABLE OF bdcmsgcoll.

DATA: gva_variant  TYPE varid-variant,
      gva_data_ini TYPE sy-datum,
      gv_pis       TYPE zdeco_perc,
      gv_cofins    TYPE zdeco_perc,
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

DATA: ok_code                 TYPE sy-ucomm.

*&--------------------------------------------------------------------&*
*& Selection                                                          &*
*&--------------------------------------------------------------------&*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.

    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS s_bukrs FOR t001-bukrs NO-EXTENSION NO INTERVALS. "DEFAULT '0100'.
    SELECTION-SCREEN SKIP 1.
    PARAMETERS p_mesan TYPE num6.
    SELECTION-SCREEN SKIP 1.
    PARAMETERS p_mes TYPE num2 NO-DISPLAY.
    PARAMETERS p_ano TYPE num4 NO-DISPLAY.
    SELECT-OPTIONS s_budat FOR bsis-budat NO-DISPLAY.
    SELECT-OPTIONS s_saknr FOR ska1-saknr NO INTERVALS."NO-EXTENSION.
    SELECT-OPTIONS s_belnr FOR bseg-belnr NO INTERVALS NO-DISPLAY.
    SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN END OF BLOCK a2.
  SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK a1.
*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
INITIALIZATION.
*** BUG - 178383 - CBRAND - Inicio
  DATA: lva_button TYPE char1.
  GET PARAMETER ID 'ZFIS35' FIELD lva_button.

  IF lva_button = 'X'.
*** BUG - 178383 - CBRAND - Fim
    SELECTION-SCREEN : FUNCTION KEY 1.
    functxt-icon_id = icon_submit.
    functxt-quickinfo = 'Parâmetro % PIS/COFINS'.
    functxt-icon_text = 'Parâmetro % PIS/COFINS'.
    sscrfields-functxt_01 = functxt.

    SELECTION-SCREEN : FUNCTION KEY 2.
    FREE functxt.
    functxt-icon_id = icon_submit.
    functxt-quickinfo = 'Parâmetro Seleção de Dados'.
    functxt-icon_text = 'Parâmetro Seleção de Dados'.
    sscrfields-functxt_02 = functxt.
  ENDIF.
*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF sy-ucomm = 'FC01'.
    CALL TRANSACTION 'ZFIS56'.
  ENDIF.
  IF sy-ucomm = 'FC02'.
    CALL TRANSACTION 'ZFIS57'.
  ENDIF.

START-OF-SELECTION.

  PERFORM z_valida_campo.
* Seleção Dados
  PERFORM z_seleciona_dados.
  PERFORM f_atualiza_doc.
  IF git_saidas[] IS NOT INITIAL.
    CALL SCREEN 300.
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
* Constructor
    METHODS: constructor
      IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
* Event for toolbar
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

    READ TABLE git_saidas INTO DATA(gw_saidac) INDEX e_row_id-index.
    IF sy-subrc IS INITIAL.

      CASE e_column_id.  "Necessidade de seprarar
        WHEN 'STATUS'.
          PERFORM f_exibe_erro_zib USING e_row_id-index
                                         'STATUS'.
        WHEN 'BELNR'.
          IF gw_saidac-belnr IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saidac-belnr.
            SET PARAMETER ID 'BUK' FIELD gw_saidac-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saidac-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'DOC'.
          IF gw_saidac-doc IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saidac-doc.
            SET PARAMETER ID 'BUK' FIELD gw_saidac-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saidac-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'EST'.
          IF gw_saidac-est IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saidac-est.
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

    IF sy-dynnr EQ '0300'.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_execute_object.
      ty_toolbar-function  =  c_g_rec.
      ty_toolbar-quickinfo = 'Gerar Documento'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Gerar Documento'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_system_redo.
      ty_toolbar-function  =  c_ge_rec.
      ty_toolbar-quickinfo = 'Estornar Documento'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Estornar Documento'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-icon      =  icon_erase.
      ty_toolbar-function  =  c_limp_d.
      ty_toolbar-quickinfo = 'Limpar Dados'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Limpar Dados'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
**<<<------"189147 - NMS - INI------>>>
      ty_toolbar-icon      =  icon_system_undo.
      ty_toolbar-function  =  cg_repros.
      ty_toolbar-quickinfo = 'Reprocessamento de Dados'.
      ty_toolbar-disabled  = wl_desactive.
      ty_toolbar-butn_type = 0.
      ty_toolbar-text      = 'Reprocessar Dados'.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.

      ty_toolbar-butn_type = 3.
      APPEND ty_toolbar TO e_object->mt_toolbar.
      CLEAR ty_toolbar.
**<<<------"189147 - NMS - FIM------>>>
    ENDIF.

  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    DATA: lv_tabix TYPE sy-tabix.
    gv_ucommx = e_ucomm.

    CASE e_ucomm.
* Receita
      WHEN c_g_rec.
        PERFORM f_ger_doc.
      WHEN c_ge_rec.
        PERFORM f_ger_est.
      WHEN c_limp_d.
        PERFORM f_limpa_dados.
**<<<------"189147 - NMS - INI------>>>
      WHEN cg_repros. "Reprocessamento de dados
        PERFORM zf_reprocessa_dados.
**<<<------"189147 - NMS - FIM------>>>
    ENDCASE.

    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = gwa_stable3.

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
    DESCRIBE TABLE git_saidas LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'Data Lançamento:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Taxa Fechamento:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = '%PIS:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = '%COFINS:' valor = v_uzeit ) TO i_filtros.
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
    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
        is_layout       = gwa_layout
        is_variant      = gs_layout
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = git_fieldcatalog[]
        it_outtab       = git_saidas[].

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
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_GRID1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM montar_layout_grid .

  REFRESH git_fieldcatalog.

  PERFORM montar_estrutura USING:
1    ' '   'BUKRS      ' 'GIT_SAIDAN' 'BUKRS      ' 'Empresa            '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
2    ' '   'BRANCH     ' 'GIT_SAIDAN' 'BRANCH     ' 'Filial             '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
3    ' '   'CFOP       ' 'GIT_SAIDAN' 'CFOP       ' 'CFOP               '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
4    ' '   'NFENUM     ' 'GIT_SAIDAN' 'NFENUM     ' 'Nr. Nota           '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
5    ' '   'DOCNUM     ' 'GIT_SAIDAN' 'DOCNUM     ' 'Nº Documento       '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
6    ' '   'PARID      ' 'GIT_SAIDAN' 'PARID      ' 'Cod. Cliente       '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
7    ' '   'NAME1      ' 'GIT_SAIDAN' 'NAME1      ' 'Cliente            '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
8    ' '   'MATKL      ' 'GIT_SAIDAN' 'MATKL      ' 'Grp. Mercadoria    '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
9    ' '   'RASSC      ' 'GIT_SAIDAN' 'RASSC      ' 'Sociedade Parceira       '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
10   ' '   'SHPUNT     ' 'GIT_SAIDAN' 'SHPUNT     ' 'Un                       '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
11   ' '   'MATNR      ' 'GIT_SAIDAN' 'MATNR      ' 'Produto                  '                  '20'    ' ' ' ' ' ' ' ' ' ' ,
12   ' '   'MAKTX      ' 'GIT_SAIDAN' 'MAKTX      ' 'Descr. Material          '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
13   ' '   'PSTDAT     ' 'GIT_SAIDAN' 'PSTDAT     ' 'Dt. Lançamento           '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
14   ' '   'DOCDAT     ' 'GIT_SAIDAN' 'DOCDAT     ' 'Dt. Documento            '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
15   ' '   'ANZPK      ' 'GIT_SAIDAN' 'ANZPK      ' 'Quantidade               '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
16   ' '   'HSL        ' 'GIT_SAIDAN' 'HSL        ' 'Valor NF           '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
17   ' '   'TSL        ' 'GIT_SAIDAN' 'TSL        ' 'Valor Dolar        '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
18   ' '   'KURSF      ' 'GIT_SAIDAN' 'KURSF      ' 'Taxa Dolar         '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
19   ' '   'AUBEL      ' 'GIT_SAIDAN' 'AUBEL      ' 'Ordem de Venda     '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
20   ' '   'REFKEY     ' 'GIT_SAIDAN' 'REFKEY     ' 'Doc. Faturamento   '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
21   ' '   'BELNR      ' 'GIT_SAIDAN' 'BELNR      ' 'Doc. Contabil      '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
22   ' '   'ORT01_P    ' 'GIT_SAIDAN' 'ORT01_P    ' 'Cidade PC          '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
23   ' '   'REGIO      ' 'GIT_SAIDAN' 'REGIO      ' 'UF                 '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
24   ' '   'ORT01      ' 'GIT_SAIDAN' 'ORT01      ' 'Local de entrega       '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
25   ' '   'REGIO_L    ' 'GIT_SAIDAN' 'REGIO_L    ' 'UF                     '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
26   ' '   'INCO1      ' 'GIT_SAIDAN' 'INCO1      ' 'Frete                  '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
27   ' '   'RACCT      ' 'GIT_SAIDAN' 'RACCT      ' 'Conta cliente          '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
28   ' '   'SAKNR_CC   ' 'GIT_SAIDAN' 'SAKNR_CC   ' 'Conta cliente CutOff   '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
29   ' '   'SAKNR_R    ' 'GIT_SAIDAN' 'SAKNR_R    ' 'Conta receita          '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
30   ' '   'SAKNR_RC   ' 'GIT_SAIDAN' 'SAKNR_RC   ' 'Conta receita CutOff   '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
31   ' '   'SAKNR_C    ' 'GIT_SAIDAN' 'SAKNR_C    ' 'Conta custo            '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
32   ' '   'SAKNR_P    ' 'GIT_SAIDAN' 'SAKNR_P    ' 'Custo PP ou Ver CutOff       '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
33   ' '   'SAKNR_E    ' 'GIT_SAIDAN' 'SAKNR_E    ' 'Conta estoque                '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
34   ' '   'SAKNR_EC   ' 'GIT_SAIDAN' 'SAKNR_EC   ' 'Conta estoque CutOff         '                  '25'    ' ' ' ' ' ' ' ' ' ' ,
35   ' '   'WAERS      ' 'GIT_SAIDAN' 'WAERS      ' 'Moeda                        '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
36   ' '   'UF         ' 'GIT_SAIDAN' 'UF         ' 'Mesma UF ?                   '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
37   ' '   'ROUTE      ' 'GIT_SAIDAN' 'ROUTE      ' 'Itinerário                   '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
38   ' '   'TRAZTD     ' 'GIT_SAIDAN' 'TRAZTD     ' 'Tempo Gasto Itinerário       '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
39   ' '   'DOCDAT_E   ' 'GIT_SAIDAN' 'DOCDAT_E   ' 'Data de entrega            '                  '10'    ' ' ' ' ' ' ' ' ' ' ,
40   ' '   'GERA       ' 'GIT_SAIDAN' 'GERA       ' 'Gera CutOff no mês ?       '                  '10'    ' ' ' ' ' ' ' ' ' ' .


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

  TYPES: BEGIN OF ty_bsegx,
           doc_lcto TYPE num10,
           gsber    TYPE char4,
           awkey    TYPE char10,
         END OF ty_bsegx.

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

  DATA: it_bsegx TYPE TABLE OF ty_bsegx,
        wa_bsegx TYPE ty_bsegx.


  DATA: it_1bnflinx TYPE TABLE OF j_1bnflin,
        wa_1bnflinx TYPE j_1bnflin.

*** BUG - 178383 - CBRAND - Inicio
  DATA filter_saknr TYPE SORTED TABLE OF  zfit0015-saknr  WITH UNIQUE KEY table_line.
*** BUG - 178383 - CBRAND - Fim

*** BUG - 181033 - CBRAND - Inicio
  CLEAR: git_saidas[].
*** BUG - 181033 - CBRAND - Fim

  SELECT * FROM zfit0014
    INTO TABLE @DATA(it_zfit0014).

  IF sy-subrc IS INITIAL.

    SORT it_zfit0014 BY data_fim.

    LOOP AT it_zfit0014 ASSIGNING FIELD-SYMBOL(<fs_zfit0014>).

      IF <fs_zfit0014>-tipo_i EQ '1'.
        IF ( s_budat-low GE <fs_zfit0014>-data_ini AND s_budat-low LE <fs_zfit0014>-data_fim )
        OR ( s_budat-high GE <fs_zfit0014>-data_ini AND s_budat-high LE <fs_zfit0014>-data_fim )
        OR ( s_budat-low GE <fs_zfit0014>-data_ini AND s_budat-high LE <fs_zfit0014>-data_fim )
        OR ( s_budat-low LE <fs_zfit0014>-data_ini AND s_budat-high GE <fs_zfit0014>-data_fim ).
          DATA(lv_pis) = <fs_zfit0014>-percentual.
        ENDIF.
      ENDIF.

      IF <fs_zfit0014>-tipo_i EQ '2'.

        IF ( s_budat-low GE <fs_zfit0014>-data_ini AND s_budat-low LE <fs_zfit0014>-data_fim )
        OR ( s_budat-high GE <fs_zfit0014>-data_ini AND s_budat-high LE <fs_zfit0014>-data_fim )
        OR ( s_budat-low GE <fs_zfit0014>-data_ini AND s_budat-high LE <fs_zfit0014>-data_fim )
        OR ( s_budat-low LE <fs_zfit0014>-data_ini AND s_budat-high GE <fs_zfit0014>-data_fim ).
          DATA(lv_cofins) = <fs_zfit0014>-percentual.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDIF.

*** BUG - 178383 - CBRAND - Inicio
*  SELECT * FROM zfit0015
*    INTO TABLE @DATA(it_zfit0015)
*    WHERE
*           saknr IN @s_saknr.
*
*  DATA(it_zfit0015_1) = it_zfit0015[].
*  DATA(it_zfit0015_2) = it_zfit0015[].
*  DATA(it_zfit0015_3) = it_zfit0015[].
*  DATA(it_zfit0015_4) = it_zfit0015[].
*
*  DELETE it_zfit0015_1 WHERE bukrs IS NOT INITIAL
*                          OR werks IS NOT INITIAL
*                          OR kostl IS NOT INITIAL.
*
*  DELETE it_zfit0015_2 WHERE
*                             werks IS NOT INITIAL  OR
*                             kostl IS NOT INITIAL.
*
*  DELETE it_zfit0015_2 WHERE
**                             bukrs IS INITIAL OR
*                             werks IS INITIAL
*                          OR kostl IS INITIAL.
*
*
*  DELETE it_zfit0015_3 WHERE kostl IS NOT INITIAL.
*
*  DELETE it_zfit0015_3 WHERE bukrs IS INITIAL
*                          OR werks IS INITIAL.
**                          OR kostl IS INITIAL.
*
*  DELETE it_zfit0015_4 WHERE bukrs IS INITIAL
*                          OR werks IS INITIAL
*                          OR kostl IS INITIAL.

*  IF it_zfit0015 IS NOT INITIAL.
*    SELECT * FROM bseg
*      INTO TABLE @DATA(it_bseg)
*      FOR ALL ENTRIES IN @it_zfit0015
*      WHERE hkont   EQ @it_zfit0015-saknr
*        AND awkey   NOT LIKE 'ZFIS0003%'
*        AND belnr   IN @s_belnr
*        AND h_budat IN @s_budat.
*  ENDIF.
*** BUG - 178383 - CBRAND - Fim


** BUG - 178383 - CBRAND - Inicio
*** Primeiro busco na tabela todas as contas.
  SELECT * FROM zfit0015
    INTO TABLE @DATA(it_zfit0015)
    WHERE saknr IN @s_saknr.

  SORT it_zfit0015 BY saknr.
**<<<------"189147 - NMS - INI------>>>
*  READ TABLE it_zfit0015 INTO DATA(lwa_zfit0015) WITH KEY bukrs = s_bukrs-low.
*  IF sy-subrc = 0.
*    DELETE it_zfit0015 WHERE bukrs NOT IN s_bukrs.
*    DATA(it_zfit0015_s) = it_zfit0015.
*  ELSE.
*    DELETE it_zfit0015 WHERE bukrs IS NOT INITIAL.
*    it_zfit0015_s = it_zfit0015.
*    SORT it_zfit0015_s BY kostl.
*  ENDIF.
* Preparação dos dados de parâmetro para busca das Partidas na tabela BSEG.
  DATA(it_zfit0015_s) = it_zfit0015. CLEAR it_zfit0015_s.
  it_zfit0015_s       = VALUE #( BASE it_zfit0015_s FOR el_zfit0015 IN it_zfit0015 WHERE ( bukrs IN s_bukrs ) ( el_zfit0015 ) ).
  it_zfit0015_s       = VALUE #( BASE it_zfit0015_s FOR el_zfit0015 IN it_zfit0015 WHERE ( bukrs IS INITIAL ) ( el_zfit0015 ) ).
  SORT it_zfit0015_s BY saknr.
**<<<------"189147 - NMS - FIM------>>>
  DELETE ADJACENT DUPLICATES FROM it_zfit0015_s COMPARING saknr.
**<<<------"189147 - NMS - INI------>>>
*  IF it_zfit0015 IS NOT INITIAL.
  IF it_zfit0015_s IS NOT INITIAL.
**<<<------"189147 - NMS - FIM------>>>
    SELECT * FROM bseg
      INTO TABLE @DATA(it_bseg)
      FOR ALL ENTRIES IN @it_zfit0015_s
      WHERE hkont   EQ @it_zfit0015_s-saknr
        AND awkey   NOT LIKE 'ZFIS0003%'
        AND belnr   IN @s_belnr
        AND h_budat IN @s_budat.
**<<<------"189147 - NMS - INI------>>>
  ELSE.
* Não há dados para serem processados. VerificarpParâmetros de PIS e COFINS.
    MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
**<<<------"189147 - NMS - FIM------>>>
  ENDIF.
*** BUG - 178383 - CBRAND - Fim

  IF it_bseg IS NOT INITIAL.

*** BUG - 178383 - CBRAND - Inicio
    SORT it_zfit0015 BY saknr bukrs werks kostl .
**<<<------"189844 - NMS - INI------>>>
*    DATA(it_bseg_s) = it_bseg[].
*    CLEAR: it_bseg_s.
*
*    TYPES: BEGIN OF ty_bseg,
*             include TYPE bseg,
*           END OF ty_bseg,
*           tt_bseg TYPE SORTED TABLE OF ty_bseg.
*
*    TYPES   ttbseg  TYPE STANDARD TABLE OF bseg WITH DEFAULT KEY.
*
*    DATA(lt_bseg_k) = it_bseg[].
*    DATA(lt_bseg_w) = it_bseg[].
*    DATA(lt_bseg_f) = it_bseg[].
*    CLEAR: lt_bseg_k, lt_bseg_w, lt_bseg_f.
*
*
*    LOOP AT it_zfit0015_s INTO DATA(lwa_it_zfit0015_s).
*
*      filter_saknr = VALUE #( ( lwa_it_zfit0015_s-saknr  ) ).
*
*      DATA(lit_saknr)   = FILTER #( it_zfit0015  IN filter_saknr  WHERE saknr = table_line ).
*
*      SORT lit_saknr BY bukrs werks kostl.    "<<<------"189147 - NMS ------->>>
*      LOOP AT lit_saknr INTO DATA(lwa_saknr).  "Maybe Read?
*
*        DATA(lit_werks) = lit_saknr[].
*        DELETE lit_werks WHERE werks IS INITIAL.
*        DELETE ADJACENT DUPLICATES FROM lit_werks COMPARING werks.
*
*        IF lwa_saknr-bukrs IS NOT INITIAL.
*
*          IF lwa_saknr-werks IS INITIAL AND lwa_saknr-kostl IS  INITIAL.
*            "Massa de dados por empresa.
*            DATA(lt_bseg_s) =
*             VALUE ttbseg( FOR line IN it_bseg WHERE ( bukrs = lwa_saknr-bukrs AND hkont = lwa_saknr-saknr ) ( line ) ).     "<<<------"189147 - NMS ------->>>
*
*            LOOP AT lit_werks INTO DATA(lwa_werks).
*              READ TABLE lit_saknr INTO DATA(lwa_saknr_w) WITH KEY werks = lwa_werks-werks.
*              IF  lwa_saknr_w-kostl IS INITIAL.
*                lt_bseg_w =
*                VALUE ttbseg( BASE lt_bseg_w  FOR line IN lt_bseg_s WHERE ( gsber = lwa_werks-werks AND hkont = lwa_saknr-saknr ) ( line ) ).     "<<<------"189147 - NMS ------->>>
*              ELSE.
*                lt_bseg_k =
*                 VALUE ttbseg( BASE lt_bseg_k FOR line IN lt_bseg_s WHERE ( gsber = lwa_werks-werks AND  kostl = lwa_werks-kostl AND hkont = lwa_saknr-saknr ) ( line ) ).     "<<<------"189147 - NMS ------->>>
*                DELETE lt_bseg_s WHERE gsber = lwa_werks-werks AND kostl = lwa_werks-kostl.
*              ENDIF.
*            ENDLOOP.
*            CLEAR: lit_saknr.
*          ELSE.
*            LOOP AT lit_werks INTO lwa_werks.
*              READ TABLE lit_saknr INTO lwa_saknr_w WITH KEY werks = lwa_werks-werks.
*              IF  lwa_saknr_w-kostl IS INITIAL.
*                lt_bseg_w =
*                VALUE ttbseg( BASE lt_bseg_w  FOR line IN it_bseg WHERE ( gsber = lwa_werks-werks AND hkont = lwa_saknr-saknr ) ( line ) ).     "<<<------"189147 - NMS ------->>>
*              ELSE.
*                lt_bseg_k =
*                 VALUE ttbseg( BASE lt_bseg_k FOR line IN it_bseg WHERE ( gsber = lwa_werks-werks AND  kostl = lwa_werks-kostl AND hkont = lwa_saknr-saknr ) ( line ) ).     "<<<------"189147 - NMS ------->>>
*                DELETE lt_bseg_s WHERE gsber = lwa_werks-werks AND kostl = lwa_werks-kostl.
*              ENDIF.
*            ENDLOOP.
*            CLEAR: lit_saknr.
*          ENDIF.
*
**** Remover na massa o que não é de parametro.
*          IF lt_bseg_k IS NOT INITIAL AND lt_bseg_s IS NOT INITIAL.
*            RANGES rg_kostl FOR bseg-kostl.
*            DATA(lt_bseg_kf) = lt_bseg_k[].
*            SORT lt_bseg_kf BY gsber.
*            DELETE ADJACENT DUPLICATES FROM lt_bseg_kf COMPARING gsber.
*
*            LOOP AT lt_bseg_kf INTO DATA(lwa_bseg_kf).
*              LOOP AT lt_bseg_k INTO DATA(lwa_bseg_k) WHERE gsber = lwa_bseg_kf-gsber.
*                rg_kostl-sign   = 'I'.
*                rg_kostl-option = 'EQ'.
*                rg_kostl-low = lwa_bseg_k-kostl.
*                APPEND rg_kostl.
*                CLEAR rg_kostl.
*              ENDLOOP.
*              SORT rg_kostl BY low.
*              DELETE ADJACENT DUPLICATES FROM rg_kostl COMPARING low.
*              DELETE lt_bseg_s WHERE gsber = lwa_bseg_kf-gsber AND kostl NOT IN rg_kostl.
*              CLEAR: rg_kostl[].
*            ENDLOOP.
*          ENDIF.
*
*          lt_bseg_f = CORRESPONDING #( BASE ( lt_bseg_f ) lt_bseg_s  ).
*          lt_bseg_f = CORRESPONDING #( BASE ( lt_bseg_f ) lt_bseg_w  ).
*          lt_bseg_f = CORRESPONDING #( BASE ( lt_bseg_f ) lt_bseg_k  ).
*
*        ELSE.
*
*          IF lwa_saknr-bukrs IS INITIAL AND lwa_saknr-werks IS INITIAL AND lwa_saknr-kostl IS  INITIAL.
***<<<------"189147 - NMS - INI------>>>
*            DATA(lt_bseg_bkp) = it_bseg[].
***<<<------"189147 - NMS - FIM------>>>
*            "Massa de dados por conta.
*            DELETE it_bseg WHERE bukrs NOT IN s_bukrs.
*
*            lt_bseg_s =
*             VALUE ttbseg( BASE lt_bseg_s FOR line IN it_bseg WHERE (  hkont = lwa_saknr-saknr )  ( line ) ).
***<<<------"189147 - NMS - INI------>>>
*            it_bseg = lt_bseg_bkp.
*            CLEAR lt_bseg_bkp.
***<<<------"189147 - NMS - FIM------>>>
**            LOOP AT lit_werks INTO lwa_werks.
**              READ TABLE lit_saknr INTO lwa_saknr_w WITH KEY werks = lwa_werks-werks.
**              IF  lwa_saknr_w-kostl IS INITIAL.
**                lt_bseg_w =
**                VALUE ttbseg( BASE lt_bseg_w  FOR line IN lt_bseg_s WHERE (   gsber = lwa_werks-werks ) ( line ) ).
**              ELSE.
**                lt_bseg_k =
**                 VALUE ttbseg( BASE lt_bseg_k FOR line IN lt_bseg_s WHERE (  gsber = lwa_werks-werks   AND  kostl = lwa_werks-kostl ) ( line ) ).
**                DELETE lt_bseg_s WHERE gsber = lwa_werks-werks AND kostl = lwa_werks-kostl.
**              ENDIF.
**            ENDLOOP.
*
**            CLEAR: lit_saknr.    "<<<------"189147 - NMS ------->>>
**** Remover na massa o que não é de parametro.
**            IF lt_bseg_k IS NOT INITIAL.
**              lt_bseg_kf = lt_bseg_k[].
**              SORT lt_bseg_kf BY gsber.
**              DELETE ADJACENT DUPLICATES FROM lt_bseg_kf COMPARING gsber.
**
**              LOOP AT lt_bseg_kf INTO lwa_bseg_kf.
**                LOOP AT lt_bseg_k INTO lwa_bseg_k WHERE gsber = lwa_bseg_kf-gsber.
**                  rg_kostl-option = 'EQ'.
**                  rg_kostl-low = lwa_bseg_k-kostl.
**                  APPEND rg_kostl.
**                  CLEAR rg_kostl.
**                ENDLOOP.
**                DELETE lt_bseg_s WHERE gsber = lwa_bseg_kf-gsber AND kostl NOT IN rg_kostl.
**                CLEAR: rg_kostl[].
**              ENDLOOP.
**            ENDIF.
*
**            lt_bseg_f = CORRESPONDING #( BASE ( lt_bseg_f ) lt_bseg_w  ).
**            lt_bseg_f = CORRESPONDING #( BASE ( lt_bseg_f ) lt_bseg_k  ).
*            lt_bseg_f = CORRESPONDING #( BASE ( lt_bseg_f ) lt_bseg_s  ).
*
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      CLEAR: lit_saknr, it_bseg_s, lt_bseg_s, lt_bseg_k, lt_bseg_w, filter_saknr, lit_werks.
*    ENDLOOP.
**<<<------"189844 - NMS - FIM------>>>
**<<<------"189147 - NMS - INI------>>>
*    CLEAR: it_bseg.
*    it_bseg = lt_bseg_f.
**<<<------"189844 - NMS - INI------>>>
    TYPES: BEGIN OF ty_werk_kostl,
             werks   TYPE werks_d,
             r_kostl TYPE ranges_kostl_tt,
           END   OF ty_werk_kostl.

    DATA: tl_werk_kostl TYPE TABLE OF ty_werk_kostl,
          el_werk_kostl TYPE          ty_werk_kostl,
          el_kostl      TYPE          range_kostl_s.

    DATA(lt_bseg_f) = it_bseg[].
**<<<------"189844 - NMS - FIM------>>>
    SORT lt_bseg_f BY bukrs belnr gjahr buzei.
    DELETE lt_bseg_f WHERE bukrs NOT IN s_bukrs.
    DELETE ADJACENT DUPLICATES FROM lt_bseg_f COMPARING bukrs belnr gjahr buzei.
* Verifica se há dados para processamento.
    IF lt_bseg_f IS INITIAL.
* Não há dados para serem processados. VerificarpParâmetros de PIS e COFINS.
      MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
**<<<------"189844 - NMS - INI------>>>
    ELSE.
      SORT lt_bseg_f BY bukrs gsber kostl hkont.
      CLEAR: it_bseg.
*** Nivel 3 de processamento - Conta Razão, Empresa, Centro e Centro de Custo.
      it_bseg = VALUE #( FOR el_zfit0015 IN it_zfit0015 WHERE ( bukrs IN s_bukrs )
                         FOR el_bseg_f   IN lt_bseg_f   WHERE ( bukrs EQ el_zfit0015-bukrs
                                                          AND   gsber EQ el_zfit0015-werks
                                                          AND   kostl EQ el_zfit0015-kostl
                                                          AND   hkont EQ el_zfit0015-saknr ) ( el_bseg_f ) ).

      SORT: it_bseg   BY bukrs belnr gjahr buzei,
            lt_bseg_f BY bukrs hkont.
*** Nivel 0 de processamento - Conta Razão.
* Verifica se há Conta Razão Cadastrada sem Empresa, Centro e Centro de Custo.
      IF line_exists( it_zfit0015[ bukrs = space werks = space kostl = space ] ).
        it_bseg = VALUE #( BASE it_bseg FOR el_zfit0015 IN it_zfit0015 WHERE ( bukrs EQ space
                                                                         AND   werks EQ space
                                                                         AND   kostl EQ space )
                                        FOR el_bseg_f   IN lt_bseg_f   WHERE ( bukrs IN s_bukrs
                                                                         AND   hkont EQ el_zfit0015-saknr ) ( el_bseg_f ) ).

      ENDIF.
*** Nivel 1 de processamento - Conta Razão e Empresa.
* Processa se há Conta Razão Cadastrada com Empresa e sem Centro e Centro de Custo.
      it_bseg = VALUE #( BASE it_bseg FOR el_zfit0015 IN it_zfit0015 WHERE ( bukrs IN s_bukrs
                                                                       AND   werks EQ space
                                                                       AND   kostl EQ space )
                                      FOR el_bseg_f   IN lt_bseg_f   WHERE ( bukrs EQ el_zfit0015-bukrs
                                                                       AND   hkont EQ el_zfit0015-saknr ) ( el_bseg_f ) ).

      SORT lt_bseg_f BY bukrs gsber hkont.
*** Nivel 2 de processamento - Conta Razão, Empresa e Centro.
      DATA(rl_werks) = VALUE werks_t_range( FOR el_zfit0015 IN it_zfit0015 WHERE ( bukrs IN s_bukrs
                                                                             AND   werks NE space ) ( sign = 'I' option = 'EQ' low = el_zfit0015-werks ) ).
**<<<------"190717 - NMS - INI------>>>
      LOOP AT it_zfit0015 INTO DATA(el_zfit00151) WHERE bukrs IN s_bukrs
                                                    AND werks NE space.
        LOOP AT it_zfit0015 INTO DATA(el_zfit00152) WHERE bukrs IN s_bukrs
                                                      AND werks EQ el_zfit00151-werks
                                                      AND kostl NE space.
          el_werk_kostl-werks = el_zfit00151-werks.
          el_kostl-sign   = 'I'.
          el_kostl-option = 'EQ'.
          el_kostl-low    = el_zfit00152-kostl.
          APPEND el_kostl      TO el_werk_kostl-r_kostl.
          APPEND el_werk_kostl TO tl_werk_kostl.

        ENDLOOP.

      ENDLOOP.
**<<<------"190717 - NMS - FIM------>>>
* Processa se há Conta Razão Cadastrada com Empresa e Centro sem Centro de Custo.
      it_bseg = VALUE #( BASE it_bseg FOR el_bukrs    IN s_bukrs
                                      FOR el_zfit0015 IN it_zfit0015 WHERE ( bukrs EQ el_bukrs-low
                                                                       AND   werks IN rl_werks
                                                                       AND   kostl EQ space )
                                      FOR el_bseg_f   IN lt_bseg_f   WHERE ( bukrs EQ el_zfit0015-bukrs
                                                                       AND   gsber EQ el_zfit0015-werks
                                                                       AND   hkont EQ el_zfit0015-saknr ) ( el_bseg_f ) ).
* Verifica se há dados para processamento.
      IF it_bseg IS INITIAL.
* Não há dados para serem processados. VerificarpParâmetros de PIS e COFINS.
        MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

      ELSE.
* Elimina Linhas Duplicadas.
        SORT it_bseg BY bukrs belnr gjahr buzei.
        DELETE ADJACENT DUPLICATES FROM it_bseg COMPARING bukrs belnr gjahr buzei.
* Elimina Linhas de Centros que não estão Cadastrados.
        IF NOT rl_werks[] IS INITIAL.
          SORT it_bseg BY gsber.
          DELETE it_bseg WHERE gsber NOT IN rl_werks.
**<<<------"190717 - NMS - INI------>>>
* Elimina Linhas de Centros de Custo que não estão Cadastrados.
          IF NOT tl_werk_kostl[] IS INITIAL.
            SORT it_bseg BY gsber kostl.
            LOOP AT tl_werk_kostl INTO el_werk_kostl.
              DELETE it_bseg WHERE gsber EQ     el_werk_kostl-werks
                               AND kostl NOT IN el_werk_kostl-r_kostl.

            ENDLOOP.

          ENDIF.
**<<<------"190717 - NMS - FIM------>>>
        ENDIF.

        SORT it_bseg BY bukrs belnr gjahr buzei.

      ENDIF.
**<<<------"189844 - NMS - FIM------>>>
    ENDIF.
**<<<------"189147 - NMS - FIM------>>>
*** BUG - 178383 - CBRAND - Fim

    SELECT * FROM t001
      INTO TABLE @DATA(it_t001)
      FOR ALL ENTRIES IN @it_bseg
      WHERE bukrs EQ @it_bseg-bukrs.

    SELECT * FROM t001w
      INTO TABLE @DATA(it_t001w)
      FOR ALL ENTRIES IN @it_bseg
      WHERE werks EQ @it_bseg-gsber.
**<<<------"189844 - NMS - INI------>>>
*    LOOP AT it_bseg ASSIGNING FIELD-SYMBOL(<fs_bsegx>).
*      wa_bsegx-doc_lcto = <fs_bsegx>-awkey+5(10).
*      wa_bsegx-gsber    = <fs_bsegx>-gsber.
*      wa_bsegx-awkey    = <fs_bsegx>-awkey+5(10).
*      APPEND wa_bsegx TO it_bsegx.
*      CLEAR wa_bsegx.
*
*    ENDLOOP.
    it_bsegx = VALUE #( FOR el_bsegx IN it_bseg ( doc_lcto = el_bsegx-awkey+5(10)
                                                  gsber    = el_bsegx-gsber
                                                  awkey    = el_bsegx-awkey+5(10) ) ).

    IF NOT it_bsegx[] IS INITIAL.
**<<<------"189844 - NMS - FIM------>>>
      SELECT * FROM zglt073  " Tabela Apropriações - Seguros
        INTO TABLE @DATA(it_zglt073)
       FOR ALL ENTRIES IN @it_bsegx
        WHERE doc_lcto EQ @it_bsegx-doc_lcto
          AND werks    EQ @it_bsegx-gsber.
**<<<------"189844 - NMS - INI------>>>
      IF NOT it_zglt073[] IS INITIAL.
**<<<------"189844 - NMS - FIM------>>>
        SELECT * FROM zglt050  " Tabela Apropriações Cabeçalho - Seguros
         INTO TABLE @DATA(it_zglt050)
         FOR ALL ENTRIES IN @it_zglt073
          WHERE seq_lcto EQ @it_zglt073-seq_lcto.
**<<<------"189844 - NMS - INI------>>>
      ENDIF.
**<<<------"189844 - NMS - FIM------>>>
      SELECT * FROM zfit0016 " Tabela principal dados apuração PIS e COFINS s/ seguro
       INTO TABLE @DATA(it_zfit0016)
       FOR ALL ENTRIES IN @it_bseg
        WHERE bukrs EQ @it_bseg-bukrs
          AND gsber EQ @it_bseg-gsber
          AND belnr EQ @it_bseg-belnr
          AND buzei EQ @it_bseg-buzei.
**<<<------"189844 - NMS - INI------>>>
    ENDIF.

  ELSE.
    DATA(vl_texto) = CONV string( TEXT-005 ).
    SPLIT vl_texto AT '.' INTO vl_texto DATA(vl_tail).
    vl_texto = |{ vl_texto }.|.
* Não há dados para serem processados. VerificarpParâmetros de PIS e COFINS.
    MESSAGE vl_texto TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
**<<<------"189844 - NMS - FIM------>>>
  ENDIF.
**<<<------"190717 - NMS - INI------>>>
  SORT: it_t001 BY bukrs,
        it_t001w BY werks,
        it_zglt073 BY werks,
        it_zglt050 BY seq_lcto,
        it_zfit0016 BY bukrs gsber belnr buzei.
**<<<------"190717 - NMS - FIM------>>>
*** BUG - 178383 - CBRAND - Inicio
  LOOP AT it_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg>).

    gwa_saidas = CORRESPONDING #( <fs_bseg> ).
    gwa_saidas-mesano = p_mesan.

    READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = <fs_bseg>-bukrs BINARY SEARCH.    "<<<------"190717 - NMS ------->>>
    IF sy-subrc IS INITIAL.
      gwa_saidas-bukrs      = wa_t001-bukrs.
      gwa_saidas-desc_bukrs = wa_t001-butxt.
    ENDIF.

    gwa_saidas-gsber = <fs_bseg>-gsber.
    READ TABLE it_t001w INTO DATA(wa_t001w) WITH KEY werks = <fs_bseg>-gsber BINARY SEARCH.    "<<<------"190717 - NMS ------->>>
    IF sy-subrc IS INITIAL.
      gwa_saidas-desc_gsber = wa_t001w-name1.
    ELSE.
      gwa_saidas-desc_gsber = 'Descrição não encontrada.'.
    ENDIF.

    READ TABLE it_zglt073 INTO DATA(wa_zglt073) WITH KEY werks = <fs_bseg>-gsber BINARY SEARCH.    "<<<------"190717 - NMS ------->>>
    IF sy-subrc IS INITIAL.

      gwa_saidas-doc_lcto       = wa_zglt073-doc_lcto.
      gwa_saidas-nr_item        = wa_zglt073-nr_item.
      gwa_saidas-nro_parc       = wa_zglt073-nro_parc.
      gwa_saidas-anln1          = wa_zglt073-anln1.
      gwa_saidas-anln2          = wa_zglt073-anln2.
      gwa_saidas-matnr          = wa_zglt073-matnr.
      gwa_saidas-descr_bens     = wa_zglt073-descr_bens.

    ENDIF.

    READ TABLE it_zglt050 INTO DATA(wa_zglt050) WITH KEY seq_lcto = wa_zglt073-seq_lcto BINARY SEARCH.    "<<<------"190717 - NMS ------->>>

    IF sy-subrc IS INITIAL.

      gwa_saidas-nro_apolice     = wa_zglt050-nro_apolice.
      gwa_saidas-cod_seguradora  = wa_zglt050-cod_seguradora.
      gwa_saidas-vlr_premio_brl  = wa_zglt050-vlr_premio_brl.
      gwa_saidas-vlr_premio_usd  = wa_zglt050-vlr_premio_usd.

    ENDIF.

    gwa_saidas-pis    = ( <fs_bseg>-dmbtr * lv_pis ) / 100.
    gwa_saidas-cofins = ( <fs_bseg>-dmbtr * lv_cofins ) / 100.
    gv_pis = lv_pis.
    gv_cofins = lv_cofins.

    READ TABLE it_zfit0016 INTO DATA(wa_zfit0016) WITH KEY bukrs = <fs_bseg>-bukrs
                                                           gsber = <fs_bseg>-gsber
                                                           belnr = <fs_bseg>-belnr
                                                           buzei = <fs_bseg>-buzei BINARY SEARCH.    "<<<------"190717 - NMS ------->>>
    IF sy-subrc IS INITIAL.
      gwa_saidas = CORRESPONDING #( wa_zfit0016 ).
      gwa_saidas-mesano     = p_mesan.
      gwa_saidas-desc_bukrs = wa_t001-butxt.
      gwa_saidas-desc_gsber = wa_t001w-name1.
      gwa_saidas-hkont = <fs_bseg>-hkont.
      gwa_saidas-gjahr = <fs_bseg>-gjahr.
**<<<------"190717 - NMS - INI------>>>
*    ENDIF.
    ELSE.
**<<<------"190717 - NMS - FIM------>>>
**<<<------"189844 - NMS - INI------>>>
* Verifiva se o Código débito/crédito se é de crédito.
      IF <fs_bseg>-shkzg EQ sy-abcde+7(1). "H - Crédito
        gwa_saidas-dmbtr  *= -1.
        gwa_saidas-dmbe2  *= -1.
        gwa_saidas-pis    *= -1.
        gwa_saidas-cofins *= -1.

      ENDIF.
**<<<------"189844 - NMS - FIM------>>>
**<<<------"190717 - NMS - INI------>>>
    ENDIF.
**<<<------"190717 - NMS - FIM------>>>
    APPEND gwa_saidas TO git_saidas.

  ENDLOOP.

  IF git_saidas[] IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros com os parâmetros informado!'(004).
    EXIT.
  ENDIF.
*** BUG - 178383 - CBRAND - Fim
*  LOOP AT it_bseg ASSIGNING FIELD-SYMBOL(<fs_bseg>).
*
** Filtros 1
*    READ TABLE it_zfit0015_1 INTO DATA(wa_zfit0015_1) WITH KEY saknr = <fs_bseg>-hkont.
*    IF sy-subrc IS INITIAL AND wa_zfit0015_1-bukrs IS INITIAL AND wa_zfit0015_1-werks IS INITIAL AND wa_zfit0015_1-kostl IS INITIAL.
*      gwa_saidas = CORRESPONDING #( <fs_bseg> ).
*      gwa_saidas-mesano = p_mesan.
*
*      READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = <fs_bseg>-bukrs.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas-bukrs      = wa_t001-bukrs.
*        gwa_saidas-desc_bukrs = wa_t001-butxt.
*      ENDIF.
*
*      gwa_saidas-gsber = <fs_bseg>-gsber.
*      READ TABLE it_t001w INTO DATA(wa_t001w) WITH KEY werks = <fs_bseg>-gsber.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas-desc_gsber = wa_t001w-name1.
*      ELSE.
*        gwa_saidas-desc_gsber = 'Descrição não encontrada.'.
*      ENDIF.
*
*      READ TABLE it_zglt073 INTO DATA(wa_zglt073) WITH KEY werks = <fs_bseg>-gsber.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-doc_lcto       = wa_zglt073-doc_lcto.
*        gwa_saidas-nr_item        = wa_zglt073-nr_item.
*        gwa_saidas-nro_parc       = wa_zglt073-nro_parc.
*        gwa_saidas-anln1          = wa_zglt073-anln1.
*        gwa_saidas-anln2          = wa_zglt073-anln2.
*        gwa_saidas-matnr          = wa_zglt073-matnr.
*        gwa_saidas-descr_bens     = wa_zglt073-descr_bens.
*
*      ENDIF.
*
*      READ TABLE it_zglt050 INTO DATA(wa_zglt050) WITH KEY seq_lcto = wa_zglt073-seq_lcto.
**                                                           belnr = <fs_bseg>-belnr.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-nro_apolice     = wa_zglt050-nro_apolice.
*        gwa_saidas-cod_seguradora  = wa_zglt050-cod_seguradora.
*        gwa_saidas-vlr_premio_brl  = wa_zglt050-vlr_premio_brl.
*        gwa_saidas-vlr_premio_usd  = wa_zglt050-vlr_premio_usd.
*
*      ENDIF.
*
*      gwa_saidas-pis    = ( <fs_bseg>-dmbtr * lv_pis ) / 100.
*      gwa_saidas-cofins = ( <fs_bseg>-dmbtr * lv_cofins ) / 100.
*      gv_pis = lv_pis.
*      gv_cofins = lv_cofins.
*
*      READ TABLE it_zfit0016 INTO DATA(wa_zfit0016) WITH KEY bukrs = <fs_bseg>-bukrs
*                                                             gsber = <fs_bseg>-gsber
*                                                             belnr = <fs_bseg>-belnr.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas = CORRESPONDING #( wa_zfit0016 ).
*        gwa_saidas-mesano     = p_mesan.
*        gwa_saidas-desc_bukrs = wa_t001-butxt.
*        gwa_saidas-desc_gsber = wa_t001w-name1.
*        gwa_saidas-hkont = <fs_bseg>-hkont.
*        gwa_saidas-gjahr = <fs_bseg>-gjahr.
*      ENDIF.
*
*      APPEND gwa_saidas TO git_saidas.
*    ENDIF.
*
** Filtros 2
*    READ TABLE it_zfit0015_2 INTO DATA(wa_zfit0015_2) WITH KEY saknr = <fs_bseg>-hkont
*                                                               bukrs = <fs_bseg>-bukrs.
*    IF sy-subrc IS INITIAL AND wa_zfit0015_2-kostl IS INITIAL.
*
*      gwa_saidas = CORRESPONDING #( <fs_bseg> ).
*      gwa_saidas-mesano     = p_mesan.
*
*      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = <fs_bseg>-bukrs.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas-bukrs      = wa_t001-bukrs.
*        gwa_saidas-desc_bukrs = wa_t001-butxt.
*      ENDIF.
*
*      gwa_saidas-gsber = <fs_bseg>-gsber.
*      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = <fs_bseg>-gsber.
*      IF sy-subrc IS INITIAL.
**        gwa_saidas-gsber = wa_t001w-werks.
*        gwa_saidas-desc_gsber = wa_t001w-name1.
*      ELSE.
*        gwa_saidas-desc_gsber = 'Descrição não encontrada.'.
*      ENDIF.
*
*      READ TABLE it_zglt073 INTO wa_zglt073 WITH KEY werks = <fs_bseg>-gsber.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-doc_lcto       = wa_zglt073-doc_lcto.
*        gwa_saidas-nr_item        = wa_zglt073-nr_item.
*        gwa_saidas-nro_parc       = wa_zglt073-nro_parc.
*        gwa_saidas-anln1          = wa_zglt073-anln1.
*        gwa_saidas-anln2          = wa_zglt073-anln2.
*        gwa_saidas-matnr          = wa_zglt073-matnr.
*        gwa_saidas-descr_bens     = wa_zglt073-descr_bens.
*
*      ENDIF.
*
*
*      READ TABLE it_zglt050 INTO wa_zglt050 WITH KEY seq_lcto = wa_zglt073-seq_lcto.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-nro_apolice     = wa_zglt050-nro_apolice.
*        gwa_saidas-cod_seguradora  = wa_zglt050-cod_seguradora.
*        gwa_saidas-vlr_premio_brl  = wa_zglt050-vlr_premio_brl.
*        gwa_saidas-vlr_premio_usd  = wa_zglt050-vlr_premio_usd.
*
*      ENDIF.
*
*      gwa_saidas-pis    = ( <fs_bseg>-dmbtr * lv_pis ) / 100.
*      gwa_saidas-cofins = ( <fs_bseg>-dmbtr * lv_cofins ) / 100.
*      gv_pis = lv_pis.
*      gv_cofins = lv_cofins.
*
*      READ TABLE it_zfit0016 INTO wa_zfit0016 WITH KEY bukrs = <fs_bseg>-bukrs
*                                                             gsber = <fs_bseg>-gsber
*                                                             belnr = <fs_bseg>-belnr.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas = CORRESPONDING #( wa_zfit0016 ).
*        gwa_saidas-mesano     = p_mesan.
*        gwa_saidas-desc_bukrs = wa_t001-butxt.
*        gwa_saidas-desc_gsber = wa_t001w-name1.
*        gwa_saidas-hkont      = <fs_bseg>-hkont.
*        gwa_saidas-gjahr      = <fs_bseg>-gjahr.
*      ENDIF.
*
*      APPEND gwa_saidas TO git_saidas.
*    ENDIF.
*
** Filtros 3
*    READ TABLE it_zfit0015_3 INTO DATA(wa_zfit0015_3) WITH KEY saknr = <fs_bseg>-hkont
*                                                               bukrs = <fs_bseg>-bukrs
*                                                               werks = <fs_bseg>-gsber.
*    IF sy-subrc IS INITIAL AND wa_zfit0015_3-kostl IS INITIAL.
*
*      gwa_saidas = CORRESPONDING #( <fs_bseg> ).
*      gwa_saidas-mesano     = p_mesan.
*
*      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = <fs_bseg>-bukrs.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas-bukrs      = wa_t001-bukrs.
*        gwa_saidas-desc_bukrs = wa_t001-butxt.
*      ENDIF.
*
*      gwa_saidas-gsber = <fs_bseg>-gsber.
*      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = <fs_bseg>-gsber.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas-desc_gsber = wa_t001w-name1.
*      ELSE.
*        gwa_saidas-desc_gsber = 'Descrição não encontrada.'.
*      ENDIF.
*
*      READ TABLE it_zglt073 INTO wa_zglt073 WITH KEY werks = <fs_bseg>-gsber.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-doc_lcto       = wa_zglt073-doc_lcto.
*        gwa_saidas-nr_item        = wa_zglt073-nr_item.
*        gwa_saidas-nro_parc       = wa_zglt073-nro_parc.
*        gwa_saidas-anln1          = wa_zglt073-anln1.
*        gwa_saidas-anln2          = wa_zglt073-anln2.
*        gwa_saidas-matnr          = wa_zglt073-matnr.
*        gwa_saidas-descr_bens     = wa_zglt073-descr_bens.
*
*      ENDIF.
*
*
*      READ TABLE it_zglt050 INTO wa_zglt050 WITH KEY seq_lcto = wa_zglt073-seq_lcto.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-nro_apolice     = wa_zglt050-nro_apolice.
*        gwa_saidas-cod_seguradora  = wa_zglt050-cod_seguradora.
*        gwa_saidas-vlr_premio_brl  = wa_zglt050-vlr_premio_brl.
*        gwa_saidas-vlr_premio_usd  = wa_zglt050-vlr_premio_usd.
*
*      ENDIF.
*
*      gwa_saidas-pis    = ( <fs_bseg>-dmbtr * lv_pis ) / 100.
*      gwa_saidas-cofins = ( <fs_bseg>-dmbtr * lv_cofins ) / 100.
*      gv_pis = lv_pis.
*      gv_cofins = lv_cofins.
*
*      READ TABLE it_zfit0016 INTO wa_zfit0016 WITH KEY bukrs = <fs_bseg>-bukrs
*                                                       gsber = <fs_bseg>-gsber
*                                                       belnr = <fs_bseg>-belnr.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas = CORRESPONDING #( wa_zfit0016 ).
*        gwa_saidas-mesano     = p_mesan.
*        gwa_saidas-desc_bukrs = wa_t001-butxt.
*        gwa_saidas-desc_gsber = wa_t001w-name1.
*        gwa_saidas-hkont = <fs_bseg>-hkont.
*        gwa_saidas-gjahr = <fs_bseg>-gjahr.
*      ENDIF.
*
*      APPEND gwa_saidas TO git_saidas.
*    ENDIF.
*
** Filtros 4
*    READ TABLE it_zfit0015_4 INTO DATA(wa_zfit0015_4) WITH KEY saknr = <fs_bseg>-hkont
*                                                               bukrs = <fs_bseg>-bukrs
*                                                               werks = <fs_bseg>-gsber
*                                                               kostl = <fs_bseg>-kostl.
*    IF sy-subrc IS INITIAL.
*
*      gwa_saidas = CORRESPONDING #( <fs_bseg> ).
*      gwa_saidas-mesano     = p_mesan.
*
*      READ TABLE it_t001 INTO wa_t001 WITH KEY bukrs = <fs_bseg>-bukrs.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas-bukrs      = wa_t001-bukrs.
*        gwa_saidas-desc_bukrs = wa_t001-butxt.
*      ENDIF.
*      gwa_saidas-gsber = <fs_bseg>-gsber.
*      READ TABLE it_t001w INTO wa_t001w WITH KEY werks = <fs_bseg>-gsber.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-desc_gsber = wa_t001w-name1.
*      ELSE.
*        gwa_saidas-desc_gsber = 'Descrição não encontrada.'.
*      ENDIF.
*
*      READ TABLE it_zglt073 INTO wa_zglt073 WITH KEY werks = <fs_bseg>-gsber.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-doc_lcto       = wa_zglt073-doc_lcto.
*        gwa_saidas-nr_item        = wa_zglt073-nr_item.
*        gwa_saidas-nro_parc       = wa_zglt073-nro_parc.
*        gwa_saidas-anln1          = wa_zglt073-anln1.
*        gwa_saidas-anln2          = wa_zglt073-anln2.
*        gwa_saidas-matnr          = wa_zglt073-matnr.
*        gwa_saidas-descr_bens     = wa_zglt073-descr_bens.
*      ENDIF.
*
*      READ TABLE it_zglt050 INTO wa_zglt050 WITH KEY seq_lcto = wa_zglt073-seq_lcto.
*      IF sy-subrc IS INITIAL.
*
*        gwa_saidas-nro_apolice     = wa_zglt050-nro_apolice.
*        gwa_saidas-cod_seguradora  = wa_zglt050-cod_seguradora.
*        gwa_saidas-vlr_premio_brl  = wa_zglt050-vlr_premio_brl.
*        gwa_saidas-vlr_premio_usd  = wa_zglt050-vlr_premio_usd.
*
*      ENDIF.
*
*      gwa_saidas-pis    = ( <fs_bseg>-dmbtr * lv_pis ) / 100.
*      gwa_saidas-cofins = ( <fs_bseg>-dmbtr * lv_cofins ) / 100.
*      gv_pis = lv_pis.
*      gv_cofins = lv_cofins.
*
*      READ TABLE it_zfit0016 INTO wa_zfit0016 WITH KEY bukrs = <fs_bseg>-bukrs
*                                                             gsber = <fs_bseg>-gsber
*                                                             belnr = <fs_bseg>-belnr.
*      IF sy-subrc IS INITIAL.
*        gwa_saidas = CORRESPONDING #( wa_zfit0016 ).
*        gwa_saidas-mesano     = p_mesan.
*        gwa_saidas-desc_bukrs = wa_t001-butxt.
*        gwa_saidas-desc_gsber = wa_t001w-name1.
*        gwa_saidas-hkont = <fs_bseg>-hkont.
*        gwa_saidas-gjahr = <fs_bseg>-gjahr.
*      ENDIF.
*
*      APPEND gwa_saidas TO git_saidas.
*    ENDIF.
*  ENDLOOP.


*  IF git_saidas[] IS INITIAL.
*    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros com os parâmetros informado!'(004).
*    EXIT.
*  ELSE.
*
*    git_saidas_2[] = git_saidas[].
*    git_saidas_3[] = git_saidas[].
*    git_saidas_4[] = git_saidas[].
*
**types: BEGIN OF ty_r_bukrs,
*    TYPES:
**---------------------------------------------------------------------*
** Declarações
**---------------------------------------------------------------------*
*      BEGIN OF ty_r_bukrs,
*        sign   TYPE char1,
*        option TYPE char2,
*        low    TYPE bukrs,
*        high   TYPE bukrs,
*      END OF ty_r_bukrs,
*
*      BEGIN OF ty_r_werks,
*        sign   TYPE char1,
*        option TYPE char2,
*        low    TYPE werks_d,
*        high   TYPE werks_d,
*      END OF ty_r_werks,
*
*      BEGIN OF ty_r_kostl,
*        sign   TYPE char1,
*        option TYPE char2,
*        low    TYPE kostl,
*        high   TYPE kostl,
*      END OF ty_r_kostl.
*
*    DATA:
*      lt_bukrs TYPE TABLE OF ty_r_bukrs,
*      lt_werks TYPE TABLE OF ty_r_werks,
*      lt_kostl TYPE TABLE OF ty_r_kostl.
*
*    DATA: ls_bukrs TYPE ty_r_bukrs,
*          ls_werks TYPE ty_r_werks,
*          ls_kostl TYPE ty_r_kostl.
*
*    CONSTANTS: lc_i     TYPE c VALUE 'I',
*               lc_eq(2) TYPE c VALUE 'EQ'.
*
*    ls_bukrs = VALUE #( sign = lc_i option = lc_eq low = space high = space ).
*    ls_werks = ls_bukrs.
*    ls_kostl = ls_bukrs.
*
*    FREE lt_bukrs.
**it_zfit0015_2 - range empresa
*    IF it_zfit0015_2 IS NOT INITIAL.
*
*      SORT git_saidas_2 BY hkont.
*      DELETE ADJACENT DUPLICATES FROM git_saidas_2 COMPARING hkont.
*
*      LOOP AT git_saidas_2 ASSIGNING FIELD-SYMBOL(<fs_saidas>).
*
*        LOOP AT it_zfit0015_2 ASSIGNING FIELD-SYMBOL(<fs_zfit0015_2>) WHERE saknr EQ <fs_saidas>-hkont.
*
*          ls_bukrs = VALUE #( sign = lc_i option = lc_eq low = space high = space ).
** bukrs
*          ls_bukrs-low = <fs_zfit0015_2>-bukrs.
*          APPEND ls_bukrs TO lt_bukrs.
*        ENDLOOP.
*
*        LOOP AT it_zfit0015_3 ASSIGNING FIELD-SYMBOL(<fs_zfit0015_3>) WHERE saknr EQ <fs_saidas>-hkont.
** bukrs
*          ls_bukrs = VALUE #( sign = lc_i option = lc_eq low = space high = space ).
*          ls_bukrs-low = <fs_zfit0015_3>-bukrs.
*          APPEND ls_bukrs TO lt_bukrs.
*          CLEAR ls_bukrs.
*        ENDLOOP.
*
*        IF lt_bukrs IS NOT INITIAL.
*          DELETE git_saidas WHERE hkont EQ <fs_saidas>-hkont
*                              AND bukrs NOT IN lt_bukrs.
*          FREE lt_bukrs.
*        ENDIF.
*
*      ENDLOOP.
*    ENDIF.
*
*    FREE: lt_bukrs, lt_werks.
*    IF it_zfit0015_3 IS NOT INITIAL.
**it_zfit0015_3 - range filial
*      SORT git_saidas_3 BY hkont bukrs.
*      DELETE ADJACENT DUPLICATES FROM git_saidas_3 COMPARING hkont bukrs.
*
*      LOOP AT git_saidas_3 ASSIGNING <fs_saidas>.
*
*        LOOP AT it_zfit0015_3 ASSIGNING <fs_zfit0015_3> WHERE saknr EQ <fs_saidas>-hkont
*                                                          AND bukrs EQ <fs_saidas>-bukrs.
*
*          ls_werks = VALUE #( sign = lc_i option = lc_eq low = space high = space ).
*
*          ls_werks-low = <fs_zfit0015_3>-werks.
*          APPEND ls_werks TO lt_werks.
*          CLEAR ls_werks.
*        ENDLOOP.
*
*        LOOP AT it_zfit0015_4 ASSIGNING FIELD-SYMBOL(<fs_zfit0015_4>) WHERE saknr EQ <fs_saidas>-hkont
*                                AND bukrs EQ <fs_saidas>-bukrs.
*
*          ls_werks = VALUE #( sign = lc_i option = lc_eq low = space high = space ).
*
*          ls_werks-low = <fs_zfit0015_4>-werks.
*          APPEND ls_werks TO lt_werks.
*          CLEAR ls_werks.
*        ENDLOOP.
*
*        IF lt_werks IS NOT INITIAL.
*          DELETE git_saidas WHERE hkont  EQ <fs_saidas>-hkont
*                              AND  bukrs EQ <fs_saidas>-bukrs
*                              AND  gsber NOT IN lt_werks.
*          FREE: lt_werks.
*        ENDIF.
*
*      ENDLOOP.
*    ENDIF.
*
*    FREE: lt_bukrs, lt_werks.
**it_zfit0015_4 - range
*    IF it_zfit0015_4 IS NOT INITIAL.
*
*      SORT git_saidas_4 BY hkont bukrs gsber.
*      DELETE ADJACENT DUPLICATES FROM git_saidas_4 COMPARING hkont bukrs gsber.
*
*      LOOP AT git_saidas_4 ASSIGNING <fs_saidas>.
*
*        LOOP AT it_zfit0015_4 ASSIGNING <fs_zfit0015_4> WHERE saknr EQ <fs_saidas>-hkont
*                                AND bukrs EQ <fs_saidas>-bukrs
*                                AND werks EQ <fs_saidas>-gsber.
*
*          ls_kostl = VALUE #( sign = lc_i option = lc_eq low = space high = space ).
*
*          ls_kostl-low = <fs_zfit0015_4>-kostl.
*          APPEND ls_kostl TO lt_kostl.
*          CLEAR ls_kostl.
*        ENDLOOP.
*
*        IF sy-subrc IS INITIAL.
*          DELETE git_saidas WHERE hkont EQ <fs_saidas>-hkont
*                              AND bukrs EQ <fs_saidas>-bukrs
*                              AND gsber EQ <fs_saidas>-gsber
*                              AND kostl NOT IN lt_kostl.
*          FREE lt_kostl.
*        ENDIF.
*
*
*      ENDLOOP.
*    ENDIF.
*
** @s_bukrs filtrar empresa tela
*    IF s_saknr IS INITIAL.
*      DELETE git_saidas WHERE bukrs NOT IN s_bukrs.
*    ELSE.
*      DELETE git_saidas WHERE hkont IN s_saknr
*                          AND bukrs NOT IN s_bukrs.
*    ENDIF.
*
*    SORT git_saidas BY belnr  ASCENDING
*                       hkont  ASCENDING
*                       obj_key DESCENDING.
*    DELETE ADJACENT DUPLICATES FROM git_saidas COMPARING belnr hkont.
*  ENDIF.
*** BUG - 178383 - CBRAND - Fim
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

  DATA:
    lv_dia     TYPE sy-datum,
    lv_ult_dia TYPE sy-datum.

  IF p_mesan IS NOT INITIAL.
    p_mes = p_mesan(2).
    p_ano = p_mesan+2(4).
  ENDIF.

  IF p_mes IS INITIAL OR p_ano IS INITIAL.
    MESSAGE 'Mês e Ano são campos obrigatórios!' TYPE 'I' DISPLAY LIKE 'E'.
**<<<------"189147 - NMS - INI------>>>
*    EXIT.
    LEAVE LIST-PROCESSING.
**<<<------"189147 - NMS - FIM------>>>
  ELSE.
    IF p_mes GT 12 OR p_mes LT 1.
      MESSAGE 'Mês inválido!' TYPE 'I' DISPLAY LIKE 'E'.
**<<<------"189147 - NMS - INI------>>>
*      EXIT.
      LEAVE LIST-PROCESSING.
**<<<------"189147 - NMS - FIM------>>>
    ENDIF.
    IF p_ano GT sy-datum(4) OR p_ano LT 1500.
      MESSAGE 'Ano fora de validade!' TYPE 'I' DISPLAY LIKE 'E'.
**<<<------"189147 - NMS - INI------>>>
*      EXIT.
      LEAVE LIST-PROCESSING.
**<<<------"189147 - NMS - FIM------>>>
    ENDIF.
  ENDIF.

  IF s_bukrs[] IS INITIAL.
    MESSAGE 'Empresa campo obrigatório!' TYPE 'I' DISPLAY LIKE 'E'.
**<<<------"189147 - NMS - INI------>>>
*    EXIT.
    LEAVE LIST-PROCESSING.
**<<<------"189147 - NMS - FIM------>>>
  ENDIF.

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

  DATA: lv_data      TYPE datum,
        lv_data_wrt  TYPE char10,
        lv_data_conv TYPE datum.

  IF grid3 IS INITIAL.

    lv_data = s_budat-high.
    lv_data = lv_data + 1.
    WRITE lv_data TO lv_data_wrt.

* Função standard para converter data em formato gravado na TCURR
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
        AND fcurr EQ 'BRL'
        AND tcurr EQ 'USD'
        AND gdatu EQ lv_data_conv.
    ENDSELECT.

    CLEAR: i_filtros3.
    CONCATENATE s_budat-high+04(02) '/' s_budat-high(04) INTO v_datum3.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit3.
    DESCRIBE TABLE git_saidas LINES v_lines.
    APPEND VALUE #( parametro = 'Data:' valor = v_datum3 ) TO i_filtros3.
    APPEND VALUE #( parametro = 'Taxa:' valor = gv_ukurs ) TO i_filtros3.
    APPEND VALUE #( parametro = '%PIS:' valor = gv_pis ) TO i_filtros3.
    APPEND VALUE #( parametro = '%COFINS:' valor = gv_cofins ) TO i_filtros3.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros3.

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
      wl_function = cl_gui_alv_grid=>mc_fc_refresh.
      APPEND wl_function TO tl_function3.

      gwa_layout3-sel_mode   = 'A'.

      CALL METHOD grid3->set_table_for_first_display
        EXPORTING
          is_layout       = gwa_layout3
          is_variant      = gs_layout3
          i_save          = 'A'
        CHANGING
          it_fieldcatalog = git_fieldcatalog[]
          it_outtab       = git_saidas[].

      SET HANDLER:
                    lcl_event_handler=>on_double_click FOR grid3.
      SET HANDLER:
                    lcl_event_handler=>on_hotsopt_click FOR grid3.

    ENDIF.
  ELSE.
    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = gwa_stable3.
  ENDIF.

*    ENDIF.
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
      PERFORM z_seleciona_dados.
      PERFORM f_atualiza_doc.
    WHEN c_atuali.
      PERFORM z_seleciona_dados.
      PERFORM f_atualiza_doc.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_cancel.
      LEAVE PROGRAM.
    WHEN c_exit.
      LEAVE PROGRAM.
    WHEN c_doc_cont.

      GET CURSOR FIELD fnam VALUE fval.
  ENDCASE.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = gwa_stable3.

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
1   ' ' 'BUKRS          ' 'GIT_SAIDAS ' 'BUKRS          ' 'Empresa  ' '10'    ' ' ' ' ' ' ' ' ' ' ,
2   ' ' 'DESC_BUKRS     ' 'GIT_SAIDAS ' 'DESC_BUKRS     ' 'Desc. Empresa  ' '30'    ' ' ' ' ' ' ' ' ' ' ,
3   ' ' 'GSBER          ' 'GIT_SAIDAS ' 'GSBER          ' 'Filial   ' '20'    ' ' ' ' ' ' ' ' ' ' ,
4	  ' ' 'DESC_GSBER    	' 'GIT_SAIDAS	' 'DESC_GSBER    	' 'Desc. Filial	' '30'    ' ' ' ' ' ' ' ' ' ' ,
5	  ' ' 'BELNR         	' 'GIT_SAIDAS	' 'BELNR         	' 'Documento 	' '20'    ' ' ' ' ' ' ' ' ' ' ,
5   ' ' 'BUZEI          ' 'GIT_SAIDAS ' 'BUZEI          ' 'Item       ' '5'     ' ' ' ' ' ' ' ' ' ' ,    "<<<------"190717 - NMS ------->>>
6	  ' ' 'HKONT         	' 'GIT_SAIDAS	' 'HKONT         	' 'Conta 	' '20'    ' ' ' ' ' ' ' ' ' ' ,
7	  ' ' 'DOC_LCTO      	' 'GIT_SAIDAS	' 'DOC_LCTO      	' 'Seq. Lcto 	' '20'    ' ' ' ' ' ' ' ' ' ' ,
8   ' ' 'NR_ITEM        ' 'GIT_SAIDAS ' 'NR_ITEM        ' 'Item   ' '20'    ' ' ' ' ' ' ' ' ' ' ,
9	  ' ' 'NRO_PARC      	' 'GIT_SAIDAS	' 'NRO_PARC      	' 'Nº Parcela	' '20'    ' ' ' ' ' ' ' ' ' ' ,
10  ' ' 'NRO_APOLICE    ' 'GIT_SAIDAS ' 'NRO_APOLICE    ' 'Nº Apolice ' '20'    ' ' ' ' ' ' ' ' ' ' ,
11  ' ' 'COD_SEGURADORA ' 'GIT_SAIDAS ' 'COD_SEGURADORA ' 'Seguradora   ' '20'    ' ' ' ' ' ' ' ' ' ' ,
12  ' ' 'ANLN1          ' 'GIT_SAIDAS ' 'ANLN1          ' 'Imobilizado  ' '20'    ' ' ' ' ' ' ' ' ' ' ,
13  ' ' 'ANLN2          ' 'GIT_SAIDAS ' 'ANLN2          ' 'Sub-número ' '20'    ' ' ' ' ' ' ' ' ' ' ,
14  ' ' 'MATNR          ' 'GIT_SAIDAS ' 'MATNR          ' 'Material ' '20'    ' ' ' ' ' ' ' ' ' ' ,
15  ' ' 'DESCR_BENS     ' 'GIT_SAIDAS ' 'DESCR_BENS     ' 'Desc. Bens   ' '20'    ' ' ' ' ' ' ' ' ' ' ,
16  ' ' 'KOSTL          ' 'GIT_SAIDAS ' 'KOSTL          ' 'Centro Custo   ' '20'    ' ' ' ' ' ' ' ' ' ' ,
17  ' ' 'H_WAERS        ' 'GIT_SAIDAS ' 'H_WAERS        ' 'Moeda Doc  ' '20'    ' ' ' ' ' ' ' ' ' ' ,
18  ' ' 'VLR_PREMIO_BRL ' 'GIT_SAIDAS ' 'VLR_PREMIO_BRL ' 'Valor Prêmio BRL ' '20'    ' ' ' ' ' ' ' ' ' ' ,
19  ' ' 'VLR_PREMIO_USD ' 'GIT_SAIDAS ' 'VLR_PREMIO_USD ' 'Valor Prêmio USD   ' '20'    ' ' ' ' ' ' ' ' ' ' ,
20  ' ' 'DMBTR          ' 'GIT_SAIDAS ' 'DMBTR          ' 'Valor Despesa BRL  ' '20'    ' ' ' ' ' ' ' ' ' ' ,
21  ' ' 'DMBE2          ' 'GIT_SAIDAS ' 'DMBE2          ' 'Valor Despesa USD  ' '20'    ' ' ' ' ' ' ' ' ' ' ,
22  ' ' 'PIS            ' 'GIT_SAIDAS ' 'PIS            ' 'PIS  ' '20'    ' ' ' ' ' ' ' ' ' ' ,
23  ' ' 'COFINS         ' 'GIT_SAIDAS ' 'COFINS         ' 'COFINS   ' '20'    ' ' ' ' ' ' ' ' ' ' ,
24  ' ' 'STATUS         ' 'GIT_SAIDAS ' 'STATUS         ' 'Status' '08'    ' ' ' ' 'X' ' ' 'C' ,
25  ' ' 'DOC            ' 'GIT_SAIDAS ' 'DOC            ' 'Documento  ' '20'    ' ' ' ' 'X' ' ' ' ' ,
26  ' ' 'EST            ' 'GIT_SAIDAS ' 'EST            ' 'Estorno  '   '20'    ' ' ' ' 'X' ' ' ' ' .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ger_doc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_ger_doc.

  DATA(lv_lines) = lines( git_saidas ).
  DATA: lv_index(6) TYPE n.
  DATA: lv_seq(8)   TYPE n.
  DATA: vseq(10)   TYPE p.



  CLEAR lv_index.
*** US - 179242 - Inicio - CBRAND
  DATA lit_saida_aux LIKE git_saidas.
  DATA(git_saidas_sum) = git_saidas[].
  DELETE git_saidas_sum WHERE obj_key IS NOT INITIAL.
  SORT git_saidas_sum BY bukrs gsber hkont kostl h_waers gjahr.    "<<<------"190717 - NMS ------->>>

  LOOP AT git_saidas_sum ASSIGNING FIELD-SYMBOL(<fs_saida>)
    GROUP BY ( bukrs   =  <fs_saida>-bukrs
               gsber   =  <fs_saida>-gsber
               hkont   =  <fs_saida>-hkont
               kostl   =  <fs_saida>-kostl
               h_waers =  <fs_saida>-h_waers
               gjahr   =  <fs_saida>-gjahr )
    ASCENDING
    ASSIGNING FIELD-SYMBOL(<fs_group_saida>).

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr = '01'
        object      = 'ZID_FIR'
      IMPORTING
        number      = vseq.

    lv_seq = vseq.

    LOOP AT GROUP <fs_group_saida> ASSIGNING FIELD-SYMBOL(<group_line>).
      lit_saida_aux = VALUE #( BASE lit_saida_aux ( <group_line> ) ).

      gwa_zfit0016 = CORRESPONDING #( <group_line> ).
      APPEND gwa_zfit0016 TO git_zfit0016.
      CLEAR gwa_zfit0016.

    ENDLOOP.

    DATA(lva_sum_pis) = REDUCE dmbtr( INIT val_p TYPE dmbtr
                              FOR wa_p IN lit_saida_aux
                              NEXT val_p = val_p + wa_p-pis ).

    DATA(lva_sum_cofins) = REDUCE dmbtr( INIT val_c TYPE dmbtr
                              FOR wa_c IN lit_saida_aux
                              NEXT val_c = val_c + wa_c-cofins ).

    DATA(lv_lin) = abap_off.

    DO 3 TIMES.
      lv_index = lv_index + 1.
      lv_lin   = lv_lin + 1.

      wa_zib_contabil-obj_key    =  sy-cprog && lv_seq && <fs_group_saida>-gjahr.
      wa_zib_contabil-seqitem    =  lv_index.
      IF lv_lin EQ '1'.
        wa_zib_contabil-bschl    =  '40'.
      ELSEIF lv_lin EQ '2'.
        wa_zib_contabil-bschl    =  '40'.
      ELSEIF lv_lin EQ '3'.
        wa_zib_contabil-bschl    =  '50'.
      ENDIF.
      wa_zib_contabil-gsber      =  <fs_group_saida>-gsber.
      wa_zib_contabil-bukrs      =  <fs_group_saida>-bukrs.
      wa_zib_contabil-interface  =  '0'.
      wa_zib_contabil-bktxt      =  'CRED PISCOF SEGUROS'.
      CONCATENATE  s_budat-high+6(2) s_budat-high+4(2) s_budat-high+0(4) INTO wa_zib_contabil-bldat SEPARATED BY '.'.
      CONCATENATE  s_budat-high+6(2) s_budat-high+4(2) s_budat-high+0(4) INTO wa_zib_contabil-budat SEPARATED BY '.'.
      wa_zib_contabil-gjahr      = <fs_group_saida>-gjahr.
      wa_zib_contabil-monat      = s_budat-high+4(2).
      wa_zib_contabil-blart      = 'LM'.

      "   wa_zib_contabil-xblnr      =  gwa_saidas-belnr.

      IF lv_lin EQ '1'.
        wa_zib_contabil-hkont      = '0000113214'.
        wa_zib_contabil-wrbtr      = abs( lva_sum_pis ).
        wa_zib_contabil-dmbtr      = abs( lva_sum_pis ).
        wa_zib_contabil-dmbe2      = abs( lva_sum_pis * gv_ukurs ).
      ELSEIF lv_lin EQ '2'.
        wa_zib_contabil-hkont      = '0000113216'.
        wa_zib_contabil-wrbtr      = abs( lva_sum_cofins ).
        wa_zib_contabil-dmbtr      = abs( lva_sum_cofins ).
        wa_zib_contabil-dmbe2      = abs( lva_sum_cofins * gv_ukurs ).
      ELSEIF lv_lin EQ '3'.
        DATA: lv_pis    TYPE kslxx12,
              lv_cofins TYPE kslxx12.
        wa_zib_contabil-hkont      = <fs_group_saida>-hkont.
        wa_zib_contabil-wrbtr      = abs( ( lva_sum_pis + lva_sum_cofins ) ).
        wa_zib_contabil-dmbtr      = abs( ( lva_sum_pis + lva_sum_cofins ) ).

        lv_pis    = abs( lva_sum_pis * gv_ukurs ).
        lv_cofins = abs( lva_sum_cofins * gv_ukurs ).
        wa_zib_contabil-dmbe2      = lv_pis + lv_cofins.

      ENDIF.

      wa_zib_contabil-waers          = 'BRL'.
      wa_zib_contabil-sgtxt          = <fs_group_saida>-bukrs && '- CRED PISCOF SEGUROS -' && s_budat-high+4(2) && s_budat-high+0(4).
      wa_zib_contabil-bupla          = <fs_group_saida>-gsber.
      wa_zib_contabil-kostl          = <fs_group_saida>-kostl.
      wa_zib_contabil-waers_i        = 'BRL'.
      wa_zib_contabil-waers_f        = 'USD'.
      wa_zib_contabil-rg_atualizado  = 'N'.

*** REGRA PIS/COFINS = 0.
** NOVA REGGRA BUG - 181033 - CBRAND - Inicio
      IF ( lv_lin EQ '1' AND lva_sum_pis <= 0 ) OR ( lv_lin EQ '2' AND lva_sum_cofins <= 0 ) OR ( lv_lin EQ '3' AND lva_sum_cofins <= 0 AND lva_sum_pis <= 0 ) .
        CONTINUE.
      ELSE.
        MODIFY zib_contabil  FROM wa_zib_contabil.
        COMMIT WORK AND WAIT.
      ENDIF.
** NOVA REGGRA BUG - 181033 - CBRAND - Fim
    ENDDO.

    MODIFY git_zfit0016 FROM VALUE #( obj_key = wa_zib_contabil-obj_key ) TRANSPORTING obj_key
     WHERE obj_key IS INITIAL.

    MODIFY git_saidas FROM VALUE #( obj_key = wa_zib_contabil-obj_key ) TRANSPORTING obj_key
    WHERE bukrs   =  <fs_group_saida>-bukrs
      AND gsber   =  <fs_group_saida>-gsber
      AND hkont   =  <fs_group_saida>-hkont
      AND kostl   =  <fs_group_saida>-kostl
      AND h_waers =  <fs_group_saida>-h_waers
      AND gjahr   =  <fs_group_saida>-gjahr
      AND obj_key IS INITIAL.


    IF <group_line> IS ASSIGNED.
      CLEAR: <group_line>.
    ENDIF.

    CLEAR: lit_saida_aux[],
           lva_sum_pis,
           lva_sum_cofins,
           vseq,
           lv_seq.
  ENDLOOP.

  IF git_zfit0016[] IS NOT INITIAL.
    MODIFY zfit0016  FROM TABLE git_zfit0016.
    COMMIT WORK AND WAIT.
  ENDIF.

  IF git_saidas[] IS NOT INITIAL.
    PERFORM f_atualiza_doc.

    CALL METHOD grid3->refresh_table_display
      EXPORTING
        is_stable = gwa_stable3.
  ENDIF.
*** US - 179242 - Fim - CBRAND

*************************** US - 179242 - CBRAND - Inicio **************
** Código original comentado abaixo.
*************************************************************************
*  FORM f_ger_doc.
*
*  DATA(lv_lines) = lines( git_saidas ).
*
*  DATA: lv_index(6) TYPE n.
*  DATA: lv_seq(8)   TYPE n.
*  DATA: vseq(10)     TYPE p.
*
*  CALL FUNCTION 'NUMBER_GET_NEXT'
*    EXPORTING
*      nr_range_nr = '01'
*      object      = 'ZID_FIR'
*    IMPORTING
*      number      = vseq.
*
*  lv_seq = vseq.
*
*  CLEAR lv_index.

*  loop AT git_saidas INTO gwa_saidas.
*    IF gwa_saidas-obj_key IS NOT INITIAL.
**** US - 179242 - Inicio - CBRAND
***        EXIT.
*      CONTINUE.
**** US - 179242 - Fim - CBRAND
*    ENDIF.
*
**** BUG - 179211 - CBRAND - Inicio
*    IF gwa_saidas-pis <= 0 AND gwa_saidas-cofins <= 0.
*      CONTINUE.
*    ENDIF.
**** BUG - 179211 - CBRAND - Fim
*
*
*    DATA(lv_tabix) = sy-tabix.
*    DATA(lv_lin) = abap_off.
*
*    DO 3 TIMES.
*      lv_index = lv_index + 1.
*      lv_lin = lv_lin + 1.
*
*      wa_zib_contabil-obj_key    =  sy-cprog && lv_seq && gwa_saidas-gjahr.
*      wa_zib_contabil-seqitem    =  lv_index.
*      IF lv_lin EQ '1'.
*        wa_zib_contabil-bschl    =  '40'.
*      ELSEIF lv_lin EQ '2'.
*        wa_zib_contabil-bschl    =  '40'.
*      ELSEIF lv_lin EQ '3'.
*        wa_zib_contabil-bschl    =  '50'.
*      ENDIF.
*      wa_zib_contabil-gsber      =  gwa_saidas-gsber.
*      wa_zib_contabil-bukrs      =  gwa_saidas-bukrs.
*      wa_zib_contabil-interface  =  '0'.
*      wa_zib_contabil-bktxt      =  'CRED PISCOF SEGUROS'.
*      CONCATENATE  s_budat-high+6(2) s_budat-high+4(2) s_budat-high+0(4) INTO wa_zib_contabil-bldat SEPARATED BY '.'.
*      CONCATENATE  s_budat-high+6(2) s_budat-high+4(2) s_budat-high+0(4) INTO wa_zib_contabil-budat SEPARATED BY '.'.
*      wa_zib_contabil-gjahr      = gwa_saidas-gjahr.
*      wa_zib_contabil-monat      = s_budat-high+4(2).
*      wa_zib_contabil-blart      = 'LM'.
*
*      wa_zib_contabil-xblnr      =  gwa_saidas-belnr.
*
*      IF lv_lin EQ '1'.
*        wa_zib_contabil-hkont      = '0000113214'.
*        wa_zib_contabil-wrbtr      = abs( gwa_saidas-pis ).
*        wa_zib_contabil-dmbtr      = abs( gwa_saidas-pis ).
*        wa_zib_contabil-dmbe2      = abs( gwa_saidas-pis * gv_ukurs ).
*      ELSEIF lv_lin EQ '2'.
*        wa_zib_contabil-hkont      = '0000113216'.
*        wa_zib_contabil-wrbtr      = abs( gwa_saidas-cofins ).
*        wa_zib_contabil-dmbtr      = abs( gwa_saidas-cofins ).
*        wa_zib_contabil-dmbe2      = abs( gwa_saidas-cofins * gv_ukurs ).
*      ELSEIF lv_lin EQ '3'.
*        DATA: lv_pis    TYPE kslxx12,
*              lv_cofins TYPE kslxx12.
*        wa_zib_contabil-hkont      = gwa_saidas-hkont.
*        wa_zib_contabil-wrbtr      = abs( ( gwa_saidas-pis + gwa_saidas-cofins ) ).
*        wa_zib_contabil-dmbtr      = abs( ( gwa_saidas-pis + gwa_saidas-cofins ) ).
*
*        lv_pis    = abs( gwa_saidas-pis * gv_ukurs ).
*        lv_cofins = abs( gwa_saidas-cofins * gv_ukurs ).
*        wa_zib_contabil-dmbe2      = lv_pis + lv_cofins.
**        wa_zib_contabil-dmbe2      = ( abs( gwa_saidas-pis * gv_ukurs ) + abs( gwa_saidas-cofins * gv_ukurs ) ).
*      ENDIF.
*
*      wa_zib_contabil-waers      = 'BRL'. "gwa_saidac-waers.
*      wa_zib_contabil-sgtxt      = gwa_saidas-bukrs && '- CRED PISCOF SEGUROS -' && s_budat-high+4(2) && s_budat-high+0(4).
*      wa_zib_contabil-bupla      = gwa_saidas-gsber.
*      wa_zib_contabil-kostl      = gwa_saidas-kostl.
*      wa_zib_contabil-waers_i    = 'BRL'.
*      wa_zib_contabil-waers_f    = 'USD'.
*      wa_zib_contabil-rg_atualizado  = 'N'.
*
*      MODIFY zib_contabil  FROM wa_zib_contabil.
*      COMMIT WORK AND WAIT.
*
*    ENDDO.
*
*    gwa_zfit0016 = CORRESPONDING #( gwa_saidas ).
*    gwa_zfit0016-obj_key = wa_zib_contabil-obj_key.
*    APPEND gwa_zfit0016 TO git_zfit0016.
*    CLEAR gwa_zfit0016.
*    MODIFY git_saidas FROM gwa_saidas INDEX lv_tabix TRANSPORTING obj_key.
*  ENDLOOP.
*ENDLOOP.
*
*
*IF git_zfit0016[] IS NOT INITIAL.
*  MODIFY zfit0016  FROM TABLE git_zfit0016.
*  COMMIT WORK AND WAIT.
*ENDIF.
*
*IF git_saidas[] IS NOT INITIAL.
*  CALL METHOD grid3->refresh_table_display
*    EXPORTING
*      is_stable = gwa_stable3.
*ENDIF.
*
*ENDFORM.
*************************** US - 179242 - CBRAND - Fim**** **************
*************************************************************************
*************************************************************************
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ger_est
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_ger_est.

  DATA: messtab TYPE bdcmsgcoll OCCURS 0,
        wa_mess TYPE bdcmsgcoll,
        ok      TYPE c,
        data    TYPE n LENGTH 10.

  DATA:
    vl_budat    TYPE char10,
    vl_data_doc TYPE char6,
    vl_erro     TYPE char1.

  DATA(lv_lines) = lines( git_saidas ).

  DATA: lv_index(6) TYPE n.

  LOOP AT git_saidas INTO gwa_saidas.

    DATA(lv_tabix) = sy-tabix.

    IF gwa_saidas-doc IS NOT INITIAL.

      CLEAR: ok.

      data = sy-datum.
      CONCATENATE sy-datum+6(2) sy-datum+4(2) sy-datum(4)   INTO data SEPARATED BY '.'.

      IF lv_tabix LT 2.

        PERFORM batch_input USING: 'X' 'SAPMF05A'     '0105',
                                   ''  'BDC_CURSOR'   'UF05A-STGRD',
                                   ''  'BDC_OKCODE'   '=BU',
                                   ''  'RF05A-BELNS'  gwa_saidas-doc,
                                   ''  'BKPF-BUKRS'   gwa_saidas-bukrs,
                                   ''  'RF05A-GJAHS'  s_budat-low(4),
                                   ''  'UF05A-STGRD'  '01'.

        CALL TRANSACTION 'FB08' USING it_bdc MODE 'N' MESSAGES INTO messtab."OPTIONS FROM OPT.

      ENDIF.

      IF ( sy-subrc EQ 0 ) .

        LOOP AT messtab INTO wa_mess.
          IF ( wa_mess-msgtyp EQ 'S' ) AND NOT ( wa_mess-msgv1 IS INITIAL ) .
            ok = 'X'.
          ENDIF.
        ENDLOOP.

        CASE ok.
          WHEN: 'X'.
            CLEAR: vl_erro.

            DATA: lva_budat(10) TYPE c,
                  lva_stblg     TYPE bkpf-stblg.

            SELECT SINGLE stblg
              FROM bkpf INTO lva_stblg
             WHERE bukrs = gwa_saidas-bukrs
               AND belnr = gwa_saidas-doc
               AND gjahr = gwa_saidas-gjahr(4).

            CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

            gwa_saidas-est      = lva_stblg.
            MOVE: icon_led_yellow TO gwa_saidas-status.
            MODIFY git_saidas FROM gwa_saidas INDEX lv_tabix TRANSPORTING status est.
            COMMIT WORK AND WAIT.

            gwa_zfit0016 = CORRESPONDING #( gwa_saidas ).
            APPEND gwa_zfit0016 TO git_zfit0016.
            CLEAR gwa_zfit0016.

            MODIFY zfit0016  FROM TABLE git_zfit0016.
            COMMIT WORK AND WAIT.

            CALL METHOD grid3->refresh_table_display
              EXPORTING
                is_stable = gwa_stable3.
        ENDCASE.
      ELSE.

        READ TABLE messtab INTO wa_mess WITH KEY msgtyp = 'E'.
        IF sy-subrc IS INITIAL.
          MESSAGE ID wa_mess-msgid TYPE 'S' NUMBER wa_mess-msgnr WITH wa_mess-msgv1 wa_mess-msgv2 wa_mess-msgv3 wa_mess-msgv4
          DISPLAY LIKE wa_mess-msgtyp.
          EXIT.
        ELSE.
          MESSAGE s888(sabapdocu) WITH 'Documento já compensado.'.
        ENDIF.

      ENDIF.
    ELSEIF gwa_saidas-doc IS INITIAL.
      DATA(lv_erro) = abap_true.

    ENDIF.
  ENDLOOP.

*  IF lv_erro IS NOT INITIAL.
*    MESSAGE s888(sabapdocu) WITH 'Documento(s) não existe(m) ou não gerado.' 'Verificar!'.
*  ENDIF.
  MESSAGE s888(sabapdocu) WITH 'Processo de Estorno executado!'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_ger_est_custo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_limpa_dados .  " Gerar estorno custo

  DATA: messtab TYPE bdcmsgcoll OCCURS 0,
        wa_mess TYPE bdcmsgcoll,
        ok      TYPE c,
        data    TYPE n LENGTH 10.

  DATA:
    vl_budat    TYPE char10,
    vl_data_doc TYPE char6,
    vl_erro     TYPE char1.

  DATA(lv_lines) = lines( git_saidas ).

  DATA: lv_index(6) TYPE n.

  LOOP AT git_saidas INTO gwa_saidas.

    DATA(lv_tabix) = sy-tabix.

    IF  gwa_saidas-est     IS NOT INITIAL
    AND gwa_saidas-obj_key IS NOT INITIAL.

      DELETE FROM zfit0016 WHERE bukrs   EQ gwa_saidas-bukrs
                             AND gsber   EQ gwa_saidas-gsber
                             AND obj_key EQ gwa_saidas-obj_key.

      COMMIT WORK AND WAIT.

      gwa_saidas-doc     = abap_false.
      gwa_saidas-est     = abap_false.
      gwa_saidas-obj_key = abap_false.
      gwa_saidas-status  = abap_false.


      MODIFY git_saidas FROM gwa_saidas INDEX lv_tabix TRANSPORTING doc est obj_key status.
    ENDIF.
  ENDLOOP.

  IF sy-subrc IS INITIAL.
    MESSAGE s888(sabapdocu) WITH 'Limpeza realizada com sucesso!'.
  ENDIF.

  CALL METHOD grid3->refresh_table_display
    EXPORTING
      is_stable = gwa_stable3.

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
*&---------------------------------------------------------------------*
*& Form f_atualiza_doc
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_atualiza_doc .

  DATA: lva_budat(10) TYPE c,
        lva_stblg     TYPE bkpf-stblg.
  DATA(it_saida) = git_saidas[].
  SORT it_saida BY obj_key.
  DELETE it_saida WHERE obj_key IS INITIAL.

  IF it_saida[] IS NOT INITIAL.
    SELECT * FROM zib_contabil_chv
      INTO TABLE @DATA(it_zib_contabil1)
    FOR ALL ENTRIES IN @it_saida
    WHERE obj_key EQ @it_saida-obj_key
      AND bukrs   EQ @it_saida-bukrs
      AND gjahr   EQ @it_saida-gjahr.

    SELECT * FROM zib_contabil_err
      INTO TABLE @DATA(it_zib_contabil1_err)
    FOR ALL ENTRIES IN @it_saida
    WHERE obj_key EQ @it_saida-obj_key.

  ENDIF.

  LOOP AT git_saidas ASSIGNING FIELD-SYMBOL(<fs_saidac>).

    DATA(lv_tabix) = sy-tabix.

    READ TABLE it_zib_contabil1 INTO DATA(wa_conta1) WITH KEY obj_key = <fs_saidac>-obj_key.
    IF sy-subrc IS INITIAL.
      <fs_saidac>-doc   = wa_conta1-belnr.
      <fs_saidac>-status   = icon_led_green.
      MODIFY git_saidas FROM <fs_saidac> INDEX lv_tabix TRANSPORTING status doc.
      DATA(lv_found) = abap_true.
    ENDIF.

    READ TABLE it_zib_contabil1_err INTO DATA(wa_conta1_err) WITH KEY obj_key = <fs_saidac>-obj_key.
    IF sy-subrc IS INITIAL.
      lv_found = abap_true.

      IF sy-subrc IS INITIAL AND <fs_saidac>-doc IS INITIAL.
        <fs_saidac>-status   = icon_led_red.
      ELSEIF <fs_saidac>-doc IS INITIAL.
        <fs_saidac>-status   = icon_led_yellow.
      ELSEIF <fs_saidac>-doc IS NOT INITIAL.
        <fs_saidac>-status   = icon_led_green.
      ENDIF.
    ENDIF.
    IF <fs_saidac>-obj_key IS NOT INITIAL AND <fs_saidac>-status IS INITIAL.
      <fs_saidac>-status   = icon_led_yellow.
    ENDIF.

    IF lv_found IS NOT INITIAL.
      MODIFY git_saidas FROM <fs_saidac> INDEX lv_tabix TRANSPORTING status.
    ENDIF.

*    READ TABLE it_bkpf INTO DATA(gwa_bkpf) WITH KEY belnr = <fs_saidac>-doc.
*    IF sy-subrc IS INITIAL AND gwa_bkpf-stblg IS NOT INITIAL.
*      gwa_saidas-est      = gwa_bkpf-stblg.
*      MOVE: icon_led_yellow TO gwa_saidas-status.
*      MODIFY git_saidas FROM gwa_saidas INDEX lv_tabix TRANSPORTING status est.
*
*    ENDIF.

    COMMIT WORK AND WAIT.

  ENDLOOP.

  IF it_saida IS NOT INITIAL.
    SELECT  belnr, stblg, awref_rev
      FROM bkpf INTO TABLE @DATA(it_bkpf)
      FOR ALL ENTRIES IN @git_saidas
     WHERE bukrs = @git_saidas-bukrs
       AND belnr = @git_saidas-doc
       AND gjahr = @git_saidas-gjahr(4).

    SELECT  belnr, stblg, awref_rev
      FROM bkpf INTO TABLE @DATA(it_bkpff)
      FOR ALL ENTRIES IN @git_saidas
     WHERE bukrs = @git_saidas-bukrs
       AND belnr = @git_saidas-belnr
       AND gjahr = @git_saidas-gjahr(4).

    LOOP AT git_saidas ASSIGNING <fs_saidac>.
      lv_tabix = sy-tabix.

      READ TABLE it_bkpff INTO DATA(gwa_bkpff) WITH KEY belnr = <fs_saidac>-belnr.
      IF gwa_bkpff-awref_rev(8) EQ 'ZFIS0003'.
        gwa_saidas-marca      = abap_true.
        MODIFY git_saidas FROM gwa_saidas INDEX lv_tabix TRANSPORTING marca.
        COMMIT WORK AND WAIT.
      ENDIF.

      READ TABLE it_bkpf INTO DATA(gwa_bkpf) WITH KEY belnr = <fs_saidac>-doc.
      IF sy-subrc IS INITIAL AND gwa_bkpf-stblg IS NOT INITIAL AND gwa_bkpf-stblg NE <fs_saidac>-belnr.
        gwa_saidas-est      = gwa_bkpf-stblg.
        MOVE: icon_led_yellow TO gwa_saidas-status.
        MODIFY git_saidas FROM gwa_saidas INDEX lv_tabix TRANSPORTING status est.
        COMMIT WORK AND WAIT.
      ELSEIF gwa_bkpf-stblg EQ <fs_saidac>-belnr.
        gwa_saidas-marca      = abap_true.
        MODIFY git_saidas FROM gwa_saidas INDEX lv_tabix TRANSPORTING marca.
        COMMIT WORK AND WAIT.
      ENDIF.

    ENDLOOP.
  ENDIF.

  DELETE git_saidas WHERE marca IS NOT INITIAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_erro_zib
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> E_ROW_ID_INDEX
*&---------------------------------------------------------------------*
FORM f_exibe_erro_zib  USING index  TYPE lvc_index
                             status TYPE char10.

  TYPES: BEGIN OF ty_zib_err,
           obj_key        TYPE zib_contabil_err-obj_key,
           dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
           hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
           message        TYPE zib_contabil_err-message,
         END OF ty_zib_err.

  DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

  READ TABLE git_saidas INTO DATA(wa_saida) INDEX index.

  IF status EQ 'STATUS'.

    IF wa_saida-obj_key IS NOT INITIAL.

      SELECT obj_key, dt_atualizacao, hr_atualizacao, message
         FROM zib_contabil_err INTO TABLE @lit_zib_err
      WHERE obj_key = @wa_saida-obj_key.
    ENDIF.
  ENDIF.

  IF lit_zib_err[] IS NOT INITIAL.
    cl_demo_output=>new(
      )->begin_section( `ZIB_CONTABIL_ERR:`
      )->write_text( |Erros encontrados na crição do documento: \n|
      ")->WRITE_DATA( SY-DATUM
      )->write_data( lit_zib_err[]
      )->end_section(
      )->display( ).
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Nenhum resultado encontrado!'.
  ENDIF.
ENDFORM.
**<<<------"189147 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form zf_reprocessa_dados
*&---------------------------------------------------------------------*
*& Reprocessamento de dados
*&---------------------------------------------------------------------*
FORM zf_reprocessa_dados.

  DATA: tl_saidas TYPE TABLE OF ty_saidas.

  DATA: el_saidas TYPE          ty_saidas.

  tl_saidas = VALUE #( FOR el_saida IN git_saidas WHERE ( status EQ icon_led_red ) ( el_saida ) ).

  IF tl_saidas[] IS INITIAL.
    MESSAGE |Não há ddos para serem reprocessados.| TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ELSE.
    SORT tl_saidas BY obj_key.
    DELETE ADJACENT DUPLICATES FROM tl_saidas COMPARING obj_key.

    SELECT * FROM zib_contabil
      INTO TABLE @DATA(lt_zib_contabil)
      FOR ALL ENTRIES IN @tl_saidas
    WHERE obj_key EQ @tl_saidas-obj_key.

    IF sy-subrc IS INITIAL.
      LOOP AT tl_saidas ASSIGNING FIELD-SYMBOL(<fs_saidas>).
        READ TABLE lt_zib_contabil ASSIGNING FIELD-SYMBOL(<fs_zib_contabil>) WITH KEY obj_key = <fs_saidas>-obj_key.
        IF sy-subrc IS INITIAL.
          <fs_zib_contabil>-rg_atualizado = sy-abcde+13(1). "N - Não Lido
          MODIFY lt_zib_contabil FROM <fs_zib_contabil> TRANSPORTING rg_atualizado WHERE obj_key       EQ <fs_zib_contabil>-obj_key
                                                                                     AND rg_atualizado EQ sy-abcde+18(1). "S - Lido
          <fs_saidas>-status = icon_led_green.
          MODIFY git_saidas FROM <fs_saidas> TRANSPORTING status WHERE obj_key = <fs_saidas>-obj_key.

        ENDIF.

      ENDLOOP.

      MODIFY zib_contabil FROM TABLE lt_zib_contabil.

      IF sy-subrc IS INITIAL.
        COMMIT WORK.
        MESSAGE |Dados Reprocessados com sucesso.| TYPE 'S'.

      ELSE.
        ROLLBACK WORK.
        MESSAGE |Erro no Reprocessados e Dados.| TYPE 'S'.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
**<<<------"189147 - NMS - FIM------>>>

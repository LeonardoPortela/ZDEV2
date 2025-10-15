*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Ronaldo Freitas                                         &*
*& Data.....: 09.08.2023                                              &*
*& Descrição: Controle de Gastos Bancários                            &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  : Os Ninjas Evolution                                     &*
*& Código Espec.Funcional/Técnica: Carolini Santos / Ronaldo Freitas  &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

REPORT  zfir089 NO STANDARD PAGE HEADING MESSAGE-ID fi.
INCLUDE <icon>.

TABLES: t001, skb1, bsis, sscrfields, zfit0198.

TYPES:
  BEGIN OF ty_saida,
    mark,
    icons   TYPE char4,
    butxt   TYPE sgtxt,
    tp_lcto TYPE ztp_lcto,
    gsber   TYPE bsis-gsber,
    del     TYPE c,
    ivamk   TYPE c,
    docmk   TYPE c,
    wrbtrx  TYPE wrbtr,
    dmbe2x  TYPE dmbe2,
    sgtxtc  TYPE sgtxt.
    INCLUDE         TYPE zfit0197.
TYPES:       END OF ty_saida.

TYPES:
  BEGIN OF ty_zgl033,
    id           TYPE  i,
    subid        TYPE i,
    npage        TYPE i,
    tlevel       TYPE i,
    ergsl(10)    TYPE c,
    text(65)     TYPE c,
    waers(5)     TYPE c,
    waer2(4)     TYPE c,
    repval       TYPE p LENGTH 9 DECIMALS 2,
    compval      TYPE p LENGTH 9 DECIMALS 2,
    absvar       TYPE p LENGTH 9 DECIMALS 2,
    relvar(10)   TYPE c,
    repval2      TYPE p LENGTH 9 DECIMALS 2,
    compval2     TYPE p LENGTH 9 DECIMALS 2,
    absvar2      TYPE p LENGTH 9 DECIMALS 2,
    relvar2(10)  TYPE c,
    repvalv(15)  TYPE c,
    repcal2v(15) TYPE c,
    repcal3v(15) TYPE c,
  END OF ty_zgl033.

*Class definition for ALV toolbar
CLASS:lcl_alv_toolbar DEFINITION DEFERRED.

DATA: g_container          TYPE scrfname VALUE 'CC_GRID1',
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      grid1                TYPE REF TO cl_gui_alv_grid,
      grid2                TYPE REF TO cl_gui_alv_grid,
      obg_toolbar          TYPE REF TO lcl_alv_toolbar,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,       "splitter conteiner 1
      container_2          TYPE REF TO cl_gui_container,       "splitter conteiner 2
      splitter             TYPE REF TO cl_gui_splitter_container.

*Declaration for toolbar buttons
DATA : ty_toolbar    TYPE stb_button,
       tl_index_rows TYPE lvc_t_row,
       wl_index_rows TYPE lvc_s_row.


*Criação de tabela dinamica
DATA: git_fieldcatalog TYPE lvc_t_fcat,
      gwa_fieldcatalog TYPE lvc_s_fcat,
      it_zfit0199      TYPE TABLE OF zfit0199,
      it_zfit0198      TYPE TABLE OF zfit0198,
      gv_ucommx        TYPE sy-ucomm,
      gwa_layout       TYPE lvc_s_layo,
      gwa_stable       TYPE lvc_s_stbl.

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
           c_reprov(6)      TYPE c VALUE 'REPROV',
           c_buscar(6)      TYPE c VALUE 'BUSCAR',
           c_doc_cont(8)    TYPE c VALUE  'DOC_CONT',
           c_doc_est(7)     TYPE c VALUE  'DOC_EST',
           c_clos_itens(10) TYPE c VALUE 'CLOS_ITENS'.

DATA: git_saida      TYPE TABLE OF ty_saida WITH HEADER LINE.
DATA: git_belnr      TYPE TABLE OF ty_saida WITH HEADER LINE.
DATA: git_saida_aux  TYPE TABLE OF ty_saida WITH HEADER LINE.
DATA: git_saida_pass TYPE TABLE OF ty_saida WITH HEADER LINE.

DATA: gwa_bapiret      LIKE bapiret2.

DATA: git_zglt0104       TYPE TABLE OF zglt0104  WITH HEADER LINE,
      git_zfit0160_atu   TYPE TABLE OF zfit0160  WITH HEADER LINE,
      git_zfit0160_ant   TYPE TABLE OF zfit0160  WITH HEADER LINE,
      git_tcurr          TYPE TABLE OF tcurr     WITH HEADER LINE,
      git_zgl033         TYPE TABLE OF ty_zgl033 WITH HEADER LINE,
      git_zgl033_aux     TYPE TABLE OF ty_zgl033 WITH HEADER LINE,
      git_zgl033_pas     TYPE TABLE OF ty_zgl033 WITH HEADER LINE,
      git_zgl033_pas_aux TYPE TABLE OF ty_zgl033 WITH HEADER LINE,
      git_zgl033_aval    TYPE TABLE OF ty_zgl033 WITH HEADER LINE,
      git_zfit0197       TYPE TABLE OF zfit0197 WITH HEADER LINE,
      git_zfit0197_aux   TYPE TABLE OF zfit0197 WITH HEADER LINE,
      git_dta            TYPE TABLE OF bdcdata,
      opt                TYPE ctu_params,
      git_msg            TYPE TABLE OF bdcmsgcoll,
      git_bapiret        LIKE STANDARD TABLE OF gwa_bapiret,
      gwa_zglt0104       LIKE LINE OF git_zglt0104,
      gwa_zfit0160_atu   LIKE LINE OF git_zfit0160_atu,
      gwa_tcurr          LIKE LINE OF git_tcurr,
      gwa_zfit0160_ant   LIKE LINE OF git_zfit0160_ant,
      gwa_zfit0197       LIKE LINE OF git_zfit0197,
      gwa_zfit0197_ant   LIKE LINE OF git_zfit0197,
      gwa_saida          LIKE LINE OF git_saida,
      gwa_saidax         LIKE LINE OF git_saida,
      gwa_saida_aux      LIKE LINE OF git_zfit0197_aux,
      w_zglt0112         TYPE zglt0112.

DATA: gva_variant  TYPE varid-variant,
      gva_data_ini TYPE sy-datum,
      gva_data_fim TYPE sy-datum,
      gva_belnr    TYPE zfit0197-belnr.
*      gva_belnr    TYPE zglt0108-belnr.

TYPES: ty_selection TYPE STANDARD TABLE OF rsparams.

*     Define internal table and work area for select-options
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

DATA: tl_bdc TYPE TABLE OF bdcdata,
      wl_bdc TYPE bdcdata.

DATA: ok_code                 TYPE sy-ucomm,
      p_bukrs_invra           TYPE zglt0104-investidora,
      p_bukrs_invda           TYPE zglt0104-investida,
      p_taxa_part             TYPE zglt0104-part_perc,
      p_mes                   TYPE zfit0160-mes,
      p_ano                   TYPE zfit0160-ano,
      p_mes_ant               TYPE zfit0160-mes,
      p_ano_ant               TYPE zfit0160-ano,
      p_taxa_media            TYPE zglt0104-part_perc,
      p_taxa_media_ant        TYPE zglt0104-part_perc,
      wg_taxa_part(20),
      wg_taxa_media(20),
      wg_taxa_media_ant(20),
      wg_taxa_v_dolar(20),
      wg_desc_bukrs_invda(30),
      wg_desc_bukrs_invra(30),
      wg_doc_cont(20),
      wg_estorno(20),
      wg_eq_ref_ant(20),
      gva_eq_ref_ant          TYPE p LENGTH 9 DECIMALS 2,
      p_doc_lcto              TYPE zfit0197-doc_lcto,
      p_objkey                TYPE zib_contabil-obj_key,
      p_doc_cont              TYPE zfit0197-belnr,
      p_estorno               TYPE zib_contabil-obj_key.

*&--------------------------------------------------------------------&*
*& Selection                                                          &*
*&--------------------------------------------------------------------&*
SELECTION-SCREEN BEGIN OF BLOCK a1 WITH FRAME TITLE TEXT-001.
  SELECTION-SCREEN SKIP 2.
  SELECTION-SCREEN BEGIN OF BLOCK a2 WITH FRAME.

    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS s_bukrs FOR t001-bukrs OBLIGATORY DEFAULT '0100'.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS s_saknr FOR zfit0198-hkont."skb1-saknr.
    SELECTION-SCREEN SKIP 1.
    SELECT-OPTIONS s_budat FOR bsis-budat.
    SELECTION-SCREEN SKIP 1.

  SELECTION-SCREEN END OF BLOCK a2.
  SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN END OF BLOCK a1.
*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
INITIALIZATION.

  SELECTION-SCREEN : FUNCTION KEY 1.
  sscrfields-functxt_01 = 'Parametrizar Conta'(002).

  SELECTION-SCREEN : FUNCTION KEY 2.
  sscrfields-functxt_02 = 'Parametrizar Texto/Modelo ZGL'(003).

*----------------------------------------------------------------------*
*                         AT SELECTION SCREEN                          *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF sy-ucomm = 'FC01'.
    CALL TRANSACTION 'ZFI048'.
  ENDIF.

  IF sy-ucomm = 'FC02'.
    CALL TRANSACTION 'ZFI049'.
  ENDIF.

START-OF-SELECTION.

* Seleção Dados
  PERFORM z_seleciona_dados.
  IF git_saida[] IS NOT INITIAL.
    PERFORM f_atualiza_doc.
    CALL SCREEN 100.
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
    DATA opt TYPE ctu_params.

    DATA: lr_lote TYPE RANGE OF zlote_num,
          lv_lote TYPE zlote_num,
          lw_lote LIKE LINE OF lr_lote.

    REFRESH: tl_bdc.

    READ TABLE git_saida INTO DATA(gw_saida) INDEX e_row_id-index.
    IF sy-subrc IS INITIAL.

      CASE e_column_id.
        WHEN 'ICONS'.
          PERFORM f_exibe_erro_zib USING e_row_id-index.
        WHEN 'BELNR'.

          IF gw_saida-belnr IS NOT INITIAL AND gw_saida-belnr NE '0000000000'.
            SET PARAMETER ID 'BLN' FIELD gw_saida-belnr.
            SET PARAMETER ID 'BUK' FIELD gw_saida-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saida-budat(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'LOTE'.
          IF gw_saida-lote IS NOT INITIAL.
            lw_lote-sign = 'I'.
            lw_lote-option = 'EQ'.
            lw_lote-low = gw_saida-lote.
            APPEND lw_lote TO lr_lote.
            CLEAR lw_lote.

            SUBMIT zgl018   WITH p_bukrs    EQ gw_saida-bukrs
                            WITH p_lote     IN lr_lote
            AND RETURN.
          ENDIF.

        WHEN 'BELNR_C'.

          IF gw_saida-belnr_c IS NOT INITIAL AND gw_saida-belnr NE '0000000000'.
            SET PARAMETER ID 'BLN' FIELD gw_saida-belnr_c.
            SET PARAMETER ID 'BUK' FIELD gw_saida-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saida-budat(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.


        WHEN 'BELNR_E'.
          IF gw_saida-belnr_e IS NOT INITIAL AND gw_saida-belnr NE '0000000000'.
            SET PARAMETER ID 'BLN' FIELD gw_saida-belnr_e.
            SET PARAMETER ID 'BUK' FIELD gw_saida-bukrs.
            SET PARAMETER ID 'GJR' FIELD gw_saida-budat(4).
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'DOC_LCTO'.
          IF NOT gw_saida-doc_lcto IS INITIAL.
            FREE lv_lote.
            SET PARAMETER ID 'BLN' FIELD gw_saida-doc_lcto.
            SET PARAMETER ID 'LOT' FIELD lv_lote. "gw_saida-lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
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

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_select_all.
    ty_toolbar-function  =  c_all.
    ty_toolbar-quickinfo = 'Seleciona todos'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    ty_toolbar-text      = ''.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_deselect_all.
    ty_toolbar-function  =  c_dall.
    ty_toolbar-quickinfo = 'Desmarca todos'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    ty_toolbar-text      = ''.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_execute_object.
    ty_toolbar-function  =  c_aprov.
    ty_toolbar-quickinfo = 'Gerar documentos'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    ty_toolbar-text      = 'Gerar documento'.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_system_undo.
    ty_toolbar-function  =  c_reprov.
    ty_toolbar-quickinfo = 'Estornar documentos'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    ty_toolbar-text      = 'Estornar documento'.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.
*
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-icon      =  icon_refresh.
    ty_toolbar-function  =  c_atuali.
    ty_toolbar-quickinfo = 'Atualizar documento'.
    ty_toolbar-disabled  = wl_desactive.
    ty_toolbar-butn_type = 0.
    ty_toolbar-text      = 'Atualizar Status'.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

**   variable for Toolbar Button
*    ty_toolbar-icon      =  icon_view_close.
*    ty_toolbar-function  =  c_clos_itens.
*    ty_toolbar-disabled  = space.
*    ty_toolbar-butn_type = 0.
*    APPEND ty_toolbar TO e_object->mt_toolbar.
*    CLEAR ty_toolbar.
**   Call reorganize method of toolbar manager to
**   display the toolbar

*    CALL METHOD c_alv_toolbarmanager->reorganize
*      EXPORTING
*        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.
    DATA: lv_tabix TYPE sy-tabix.

    gv_ucommx = e_ucomm.

    CASE e_ucomm.

      WHEN c_all.

        LOOP AT git_saida INTO gwa_saida.
          lv_tabix = sy-tabix.
          gwa_saida-mark = abap_on.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING mark.
*        COMMIT WORK AND WAIT.
          FREE: gwa_saida.
        ENDLOOP.

      WHEN c_dall.

        LOOP AT git_saida INTO gwa_saida.
          lv_tabix = sy-tabix.
          gwa_saida-mark = abap_off.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING mark.
*        COMMIT WORK AND WAIT.
          FREE: gwa_saida.
        ENDLOOP.

      WHEN c_aprov.
        PERFORM f_gerar_documento.
        PERFORM f_atualiza_doc.

      WHEN c_reprov.
        PERFORM f_estornar_documento.
        PERFORM f_atualiza_doc.

      WHEN c_atuali.
        PERFORM f_atualiza_doc.
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

  DATA: event       TYPE cntl_simple_event,
        events      TYPE cntl_simple_events,
        tl_filter   TYPE lvc_t_filt,
        wl_filter   TYPE lvc_s_filt,
        tl_function TYPE ui_functions,
        wl_function LIKE tl_function WITH HEADER LINE.

  IF g_custom_container IS INITIAL.
    gwa_layout-zebra      = c_x.
*    gwa_layout-box_fname = 'MARK'.
*    gwa_layout-no_rowmark = abap_on.
    gwa_layout-stylefname = 'STYLE2'.
*    gwa_layout-cwidth_opt = 'X'. "Ajusta tamanho na coluna
*    gwa_layout-col_opt    = 'X'. "Ajusta tamanho na coluna
    gwa_layout-box_fname  = 'SELECTED'.
*    gwa_layout-no_toolbar = c_x.
*    gwa_layout-box_tabname  = 'GIT_SAIDA'.
    gwa_layout-sel_mode   = c_a.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name              = g_container
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

*    CREATE OBJECT splitter
*      EXPORTING
*        parent  = g_custom_container
*        rows    = 2
*        columns = 1.
*
*    CALL METHOD splitter->set_row_height
*      EXPORTING
*        id     = 1
*        height = 100.
*
*    CALL METHOD splitter->set_row_sash
*      EXPORTING
*        id    = 1
*        type  = 0
*        value = 0.
*
*    CALL METHOD splitter->get_container
*      EXPORTING
*        row       = 1
*        column    = 1
*      RECEIVING
*        container = container_1.
*
*    CREATE OBJECT grid1
*      EXPORTING
*        i_parent = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = g_custom_container.


    PERFORM montar_layout_grid.
    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = grid1.

*      * Register event handler
    SET HANDLER obg_toolbar->on_toolbar FOR grid1.
    SET HANDLER obg_toolbar->handle_user_command FOR grid1.

*    wl_function = cl_gui_alv_grid=>mc_fc_select_all.
*    APPEND wl_function TO tl_function.
*    wl_function = cl_gui_alv_grid=>mc_fc_deselect_all.
*    APPEND wl_function TO tl_function.
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
*    wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
*    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_check.
    APPEND wl_function TO tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wl_function TO tl_function.

    DATA: gs_layout TYPE disvariant,
          g_repid   LIKE sy-repid.

    g_repid = sy-repid.
    gs_layout-report = g_repid.


    CALL METHOD grid1->set_table_for_first_display
      EXPORTING
*       it_toolbar_excluding = tl_function
*       is_layout       = gwa_layout
        is_variant      = gs_layout "&see below
        i_save          = 'A'
        i_default       = 'X'
      CHANGING
        it_fieldcatalog = git_fieldcatalog[]
        it_outtab       = git_saida[].

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
      PERFORM f_atualiza_doc.
    WHEN c_atuali.
      PERFORM f_atualiza_doc.
    WHEN c_back.
      SET SCREEN 0.
    WHEN c_cancel.
      LEAVE PROGRAM.
    WHEN c_exit.
      LEAVE PROGRAM.
    WHEN c_doc_cont.

      GET CURSOR FIELD fnam VALUE fval.

      CASE fnam.
        WHEN 'WG_DOC_CONT'.
          CHECK ( wg_doc_cont IS NOT INITIAL ).

          MOVE wg_doc_cont TO gva_belnr.

          SET PARAMETER ID 'BLN' FIELD gva_belnr.
          SET PARAMETER ID 'BUK' FIELD p_bukrs_invra .
          SET PARAMETER ID 'GJR' FIELD p_ano.

          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
        WHEN 'WG_ESTORNO'.
          CLEAR: gva_belnr.
          MOVE wg_estorno  TO gva_belnr.
          SET PARAMETER ID 'BLN' FIELD gva_belnr.
          SET PARAMETER ID 'BUK' FIELD p_bukrs_invra .
          SET PARAMETER ID 'GJR' FIELD p_ano.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDCASE.
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
     1  ' '     'MARK'           'GIT_SAIDA'   'MARK'              'Seleção'                  '4'    'X' ' ' ' ' ' ' ' ' ,
     1  ' '     'ICONS'          'GIT_SAIDA'   'ICONS'             'Status'                   '7'    ' ' ' ' 'X' ' ' 'C' ,
     2  ' '     'BUKRS'          'GIT_SAIDA'   'BUKRS'             'Cód. Empresa '            '10'   ' ' ' ' ' ' ' ' ' ' ,
     2  ' '     'BUTXT'          'GIT_SAIDA'   'BUTXT'             'Empresa '                 '20'   ' ' ' ' ' ' ' ' ' ' ,
     3  ' '     'HKONT'          'GIT_SAIDA'   'HKONT'             'Conta Contábil'           '10'   ' ' ' ' ' ' ' ' ' ' ,
     4  ' '     'BELNR'          'GIT_SAIDA'   'BELNR'             'Doc. Contábil'            '10'   ' ' ' ' 'X' ' ' ' ' ,
     5  ' '     'XBLNR'          'GIT_SAIDA'   'XBLNR'             'Referência'               '10'   ' ' ' ' ' ' ' ' ' ' ,
     6  ' '     'BUDAT'          'GIT_SAIDA'   'BUDAT'             'Data lçto '               '10'   ' ' ' ' ' ' ' ' ' ' ,
     7  ' '     'WRBTR'          'GIT_SAIDA'   'WRBTR'             'Montante MI (ARS)'        '10'   ' ' ' ' ' ' ' ' ' ' ,
     8  ' '     'DMBE2'          'GIT_SAIDA'   'DMBE2'             'Montante MI2(USD)'        '10'   ' ' ' ' ' ' ' ' ' ' ,
     9  ' '     'IVA'            'GIT_SAIDA'   'IVA'               '% IVA'                    '10'   ' ' ' ' ' ' ' ' ' ' ,
    10  ' '     'WAERS'          'GIT_SAIDA'   'WAERS'             'Moeda Doc.'               '10'   ' ' ' ' ' ' ' ' ' ' ,
    11  ' '     'SGTXT'          'GIT_SAIDA'   'SGTXT'             'Texto Documento'          '40'   ' ' ' ' ' ' ' ' ' ' ,
    12  ' '     'LOTE'           'GIT_SAIDA'   'LOTE'              'Lote'                     '10'   ' ' ' ' 'X' ' ' ' ' ,
    13  ' '     'DOC_LCTO'       'GIT_SAIDA'   'DOC_LCTO'          'Doc. Lanç.'               '10'   ' ' ' ' 'X' ' ' ' ' ,
    14  ' '     'BELNR_C'        'GIT_SAIDA'   'BELNR_C'           'Doc. Contábil'            '10'   ' ' ' ' 'X' ' ' ' ' ,
    15  ' '     'BELNR_E'        'GIT_SAIDA'   'BELNR_E'           'Doc. Estorno'             '10'   ' ' ' ' 'X' ' ' ' ' .

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
*&      Form GERAR_DOCUMENTO
*&---------------------------------------------------------------------*
FORM f_gerar_documento.

  DATA: lva_num_lote  TYPE zlote_num,
        lva_dp_resp   TYPE char2,
        lva_desc      TYPE zglt031-descricao,
        lva_descx     TYPE zdescr_lote,
        lva_blart     TYPE zglt031-blart,
        lva_bukrs     TYPE zglt031-bukrs,
        lva_dpto_resp TYPE zglt031-dpto_resp,
        lva_bktxt     TYPE zglt031-bktxt,
        lva_prov_est  TYPE zglt031-prov_est,
        lva_liberado  TYPE char01,
        lv_belnrant   TYPE belnr_d,
        lva_objkey    TYPE zib_contabil_chv-obj_key.

  DATA:
    lit_zglt036  TYPE TABLE OF zglt036,
    lit_zglt036x TYPE TABLE OF zglt036,
    lit_zglt032  TYPE TABLE OF zglt032,
    lit_zglt0108 TYPE TABLE OF zglt0108.

  DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
        wa_zglt036_flg TYPE zde_zglt036_flg.

  DATA: lwa_saida    TYPE ty_saida,
        lwa_zglt035  TYPE zglt035,
        lwa_zglt0105 TYPE zglt0105,
        lwa_zglt036  TYPE zglt036,
        lv_tabix     TYPE sy-tabix,
        lv_tabix2    TYPE sy-tabix,
        lv_tabix3    TYPE sy-tabix,
        chave_saldo  TYPE c,
        lwa_zglt032  TYPE zglt032,
        lwa_zglt0108 TYPE zglt0108.

  CLEAR: lv_tabix.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = tl_index_rows.

  LOOP AT tl_index_rows INTO wl_index_rows.
    READ TABLE git_saida INTO gwa_saida INDEX wl_index_rows-index.
    IF sy-subrc IS INITIAL.
      IF gwa_saida-mark = ''.
        gwa_saida-mark = 'X'.
*    ELSE.
*      gwa_saida-mark = ' '.
      ENDIF.
      MODIFY git_saida FROM gwa_saida INDEX wl_index_rows-index  TRANSPORTING mark.
      CLEAR gwa_saida.
    ENDIF.
  ENDLOOP.

  CLEAR lv_belnrant.
  LOOP AT git_saida INTO gwa_saida WHERE mark EQ abap_true.
    lv_tabix = sy-tabix.

    IF lv_belnrant NE gwa_saida-belnr.
      lv_belnrant = gwa_saida-belnr.
    ELSE.
      CONTINUE.
    ENDIF.

    IF gwa_saida-del IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    IF ( gwa_saida-belnr_c IS INITIAL ) .

      IF gwa_saida-tp_lcto IS NOT INITIAL.

        SELECT SINGLE bukrs blart bktxt prov_est dpto_resp FROM zglt031 INTO ( lva_bukrs, lva_blart, lva_bktxt, lva_prov_est, lva_dpto_resp ) WHERE tp_lcto = gwa_saida-tp_lcto.

        SELECT SINGLE dpto_resp FROM zglt031 "122451 CS2023000041 - Erro departamento contabilização
          INTO lva_dp_resp
          WHERE tp_lcto = gwa_saida-tp_lcto.

        "lva_dp_resp = '83'. "Contabilidade

* RJF - Ini
*Ir na tabela ZGLT031 com TP_LCTO e selecionar campo DESCRICAO
        SELECT SINGLE descricao FROM zglt031 "122451 CS2023000041 - Erro departamento contabilização
          INTO lva_desc
          WHERE tp_lcto = gwa_saida-tp_lcto.
        IF sy-subrc IS INITIAL.
          lva_descx = lva_desc.
        ENDIF.
* RJF - Fim

** Criar lote na ZGLT034
*        CALL METHOD zcl_gerar_lote=>create_lote
*          EXPORTING
*            i_bukrs       = gwa_saida-bukrs
*            i_descr_lote  = lva_descx "'Gastos bancários'
*            i_user_resp   = sy-uname
*            i_dep_resp    = lva_dp_resp
*            i_status_lote = ' '
*          IMPORTING
*            e_num_lote    = lva_num_lote.

* Criar cabeçalho na ZGLT035
        IF lva_bukrs IS NOT INITIAL.
          MOVE lva_bukrs        TO lwa_zglt035-bukrs.
        ELSE.
          MOVE gwa_saida-bukrs         TO lwa_zglt035-bukrs.
        ENDIF.

        MOVE:    lva_num_lote            TO lwa_zglt035-lote,
                 gwa_saida-tp_lcto       TO lwa_zglt035-tp_lcto,
*                 "" Inicio - KCM - 12.10.2025
                 gwa_saida-xblnr         TO lwa_zglt035-xblnr,
*                 "" Fim - KCM - 12.10.2025
                 lva_dp_resp             TO lwa_zglt035-dpto_resp,
                 'BRL'                   TO lwa_zglt035-moeda_doc,
                 lva_dpto_resp           TO lwa_zglt035-dpto_resp,
                 lva_blart               TO lwa_zglt035-blart,
                 lva_bktxt               TO lwa_zglt035-bktxt,
                 gwa_saida-budat         TO lwa_zglt035-bldat,
                 gwa_saida-budat         TO lwa_zglt035-budat,
                 sy-datum                TO lwa_zglt035-dt_lcto,
                 lva_prov_est            TO lwa_zglt035-prov_est,
                 gwa_saida-budat+4(2)    TO lwa_zglt035-monat,
                 gwa_saida-budat(4)      TO lwa_zglt035-gjahr,
                 gwa_saida-waers         TO lwa_zglt035-moeda_doc,
                 sy-uname                TO lwa_zglt035-usnam,
                 sy-datum                TO lwa_zglt035-dt_entrada,
                 sy-uzeit                TO lwa_zglt035-hr_entrada.

* Criar item na ZGLT036
        SELECT *
          FROM zglt032 INTO TABLE lit_zglt032
        WHERE tp_lcto EQ  lwa_zglt035-tp_lcto.

        IF lit_zglt032 IS NOT INITIAL.

          MOVE:
                lwa_zglt035-lote          TO gwa_saida-lote,
                lwa_zglt035-doc_lcto      TO gwa_saida-doc_lcto,
                ''                        TO gwa_saida-doc_lcto_est,
                sy-uname                  TO gwa_saida-usnam,
                sy-datum                  TO gwa_saida-dt_atual,
                sy-uzeit                  TO gwa_saida-hr_atual.


          LOOP AT git_saida INTO DATA(gwa_saidax) WHERE mark  EQ abap_true
                                                    AND ( belnr EQ gwa_saida-belnr OR ( ivamk EQ abap_true AND sgtxt(12) EQ gwa_saida-sgtxt(12) ) ).
            LOOP AT lit_zglt032 INTO lwa_zglt032.

              lwa_zglt036-tp_lcto = lwa_zglt035-tp_lcto.

              MOVE: lwa_zglt032-buzei           TO lwa_zglt036-seqitem,
*                  sy-tabix                    TO lwa_zglt036-seqitem,
                    lwa_zglt032-bschl           TO lwa_zglt036-bschl,
                    lwa_zglt032-hkont           TO lwa_zglt036-hkont,
*                  lwa_zglt032-sgtxt           TO lwa_zglt036-sgtxt,   "
                    gwa_saidax-sgtxtc            TO lwa_zglt036-sgtxt,   "
                    lwa_zglt032-anbwa           TO lwa_zglt036-anbwa,
                    lwa_zglt032-kostl           TO lwa_zglt036-kostl,
                    lwa_zglt032-prctr           TO lwa_zglt036-prctr,
                    lwa_zglt032-aufnr           TO lwa_zglt036-aufnr,
                    lwa_zglt032-matnr           TO lwa_zglt036-matnr,
                    lwa_zglt032-matnr_fi        TO lwa_zglt036-matnr_fi,
                    lwa_zglt032-zuonr           TO lwa_zglt036-zuonr,
                    lwa_zglt032-umskz           TO lwa_zglt036-umskz,
                    lwa_zglt032-vbund           TO lwa_zglt036-vbund,
                    gwa_saidax-gsber             TO lwa_zglt036-gsber.

              READ TABLE it_zfit0198 WITH KEY hkont = gwa_saidax-hkont
                                     TRANSPORTING NO FIELDS
                                     BINARY SEARCH.
              IF sy-subrc IS NOT INITIAL.
                CONTINUE.
              ENDIF.

              IF gwa_saidax-iva EQ '21.00' OR gwa_saidax-iva EQ '10.50'.
                IF gwa_saidax-iva EQ '21.00' AND lwa_zglt036-bschl EQ '40'.
                  lwa_zglt036-tax_code = 'C1'.
                ELSEIF gwa_saidax-iva EQ '10.50' AND lwa_zglt036-bschl EQ '40'.
                  lwa_zglt036-tax_code = 'C2'.
                ENDIF.
              ENDIF.
*              MOVE: abs( gwa_saidax-wrbtr )    TO lwa_zglt036-vlr_moeda_doc,
*                    abs( gwa_saidax-wrbtr )    TO lwa_zglt036-vlr_moeda_int,
*                    abs( gwa_saidax-dmbe2 )    TO lwa_zglt036-vlr_moeda_forte.

              IF gwa_saidax-docmk IS NOT INITIAL OR gwa_saidax-ivamk IS NOT INITIAL.
                lwa_zglt036-vlr_moeda_doc = lwa_zglt036-vlr_moeda_doc + abs( gwa_saidax-wrbtrx ).
                lwa_zglt036-vlr_moeda_int = lwa_zglt036-vlr_moeda_int + abs( gwa_saidax-wrbtrx ).
                lwa_zglt036-vlr_moeda_forte = lwa_zglt036-vlr_moeda_forte + abs( gwa_saidax-dmbe2x ).
              ELSE.
                lwa_zglt036-vlr_moeda_doc = lwa_zglt036-vlr_moeda_doc + abs( gwa_saidax-wrbtr ).
                lwa_zglt036-vlr_moeda_int = lwa_zglt036-vlr_moeda_int + abs( gwa_saidax-wrbtr ).
                lwa_zglt036-vlr_moeda_forte = lwa_zglt036-vlr_moeda_forte + abs( gwa_saidax-dmbe2 ).
              ENDIF.

**              ELSE.
**
**                LOOP AT git_saida INTO DATA(gwa_s) WHERE belnr EQ gwa_saidax-belnr
**                                                     AND sgtxt+13(3) NE 'IVA'.
**                  MOVE gwa_s-sgtxt+13(37)            TO lwa_zglt036-sgtxt.
**                ENDLOOP.
**
**                MOVE: abs( gwa_saidax-wrbtr )    TO lwa_zglt036-vlr_moeda_doc,
**                      abs( gwa_saidax-wrbtr )    TO lwa_zglt036-vlr_moeda_int,
**                      abs( gwa_saidax-dmbe2 )    TO lwa_zglt036-vlr_moeda_forte.
**
              APPEND lwa_zglt036 TO lit_zglt036.
              CLEAR: lwa_zglt036, lwa_zglt032.
*            ENDIF.

            ENDLOOP.

            EXIT.
**            lit_zglt036x[] = lit_zglt036[].
**            SORT lit_zglt036 BY hkont.
**            DELETE ADJACENT DUPLICATES FROM lit_zglt036 COMPARING hkont.
**
**            LOOP AT lit_zglt036 INTO DATA(wa_zglt036).
**              lv_tabix3 = sy-tabix.
**              CLEAR: wa_zglt036-vlr_moeda_doc, wa_zglt036-vlr_moeda_int, wa_zglt036-vlr_moeda_forte.
**              LOOP AT lit_zglt036x INTO DATA(wa_zglt036x).
**                wa_zglt036-vlr_moeda_doc   = wa_zglt036-vlr_moeda_doc   +  abs( wa_zglt036x-vlr_moeda_doc ).
**                wa_zglt036-vlr_moeda_int   = wa_zglt036-vlr_moeda_int   +  abs( wa_zglt036x-vlr_moeda_int ).
**                wa_zglt036-vlr_moeda_forte = wa_zglt036-vlr_moeda_forte +  abs( wa_zglt036x-vlr_moeda_forte ).
**              ENDLOOP.
**
**              MODIFY lit_zglt036 FROM wa_zglt036 INDEX lv_tabix3 TRANSPORTING vlr_moeda_doc vlr_moeda_int vlr_moeda_forte.
**              COMMIT WORK AND WAIT.
**              CLEAR: wa_zglt036.
**            ENDLOOP.

**            IF gwa_saidax-iva EQ '21.00' OR gwa_saidax-iva EQ '10.50'.
*            APPEND lwa_zglt036 TO lit_zglt036.
*            CLEAR: lwa_zglt036, lwa_zglt032.
**            ENDIF.

          ENDLOOP.



        ELSE.
          MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Tipo de lançamento não econtrados ZGLT032!'.
          EXIT.
        ENDIF.

*            CLEAR lwa_zglt036.
*            LOOP AT lit_zglt036 INTO lwa_zglt036.
*
*              wa_zglt036_flg-doc_lcto        = lwa_zglt036-doc_lcto.
*              wa_zglt036_flg-seqitem         = lwa_zglt036-seqitem.
*              wa_zglt036_flg-seqsub          = lwa_zglt036-seqsub.
*              " wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
*              " wa_zglt036_flg-fl_cv_moeda_int = abap_true.
*              " wa_zglt036_flg-fl_cv_moeda_for = abap_true.
*              wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
*              APPEND  wa_zglt036_flg TO it_zglt036_flg.
*            ENDLOOP.

        IF lit_zglt036 IS NOT INITIAL AND lwa_zglt035 IS NOT INITIAL.
* Criar lote na ZGLT034
          CALL METHOD zcl_gerar_lote=>create_lote
            EXPORTING
              i_bukrs       = gwa_saida-bukrs
              i_descr_lote  = lva_descx "'Gastos bancários'
              i_user_resp   = sy-uname
              i_dep_resp    = lva_dp_resp
              i_status_lote = ' '
            IMPORTING
              e_num_lote    = lva_num_lote.

          IF lva_num_lote IS NOT INITIAL.
            MOVE:    lva_num_lote            TO lwa_zglt035-lote.
          ENDIF.

* Contabilizar
          CALL METHOD zcl_gerar_lote=>contabilizar_lote(
            EXPORTING
              i_arredonda = abap_true
            CHANGING
              i_zglt036   = lit_zglt036
              i_zglt035   = lwa_zglt035 ).

          CLEAR: lva_liberado.
          CALL FUNCTION 'Z_GL_LIBERAR_LOTE'
            EXPORTING
              p_num_lote = lwa_zglt035-lote
            IMPORTING
              p_liberado = lva_liberado.

          CHECK lva_liberado EQ abap_true.
          COMMIT WORK AND WAIT.

          MOVE lwa_zglt035-doc_lcto TO p_doc_lcto.

          CLEAR: lva_objkey.
          CONCATENATE 'ZGL17'  p_doc_lcto gwa_saida-budat(4) INTO lva_objkey.

          LOOP AT git_saida INTO gwa_saidax WHERE mark EQ abap_true
                                              AND ( belnr EQ gwa_saida-belnr OR ( ivamk EQ abap_true AND sgtxt(12) EQ gwa_saida-sgtxt(12) ) ).
            lv_tabix2 = sy-tabix.
            gwa_saidax-obj_key      = lva_objkey.
            gwa_saidax-doc_lcto     = p_doc_lcto.
            gwa_saidax-lote         = lwa_zglt035-lote.
            gwa_saidax-doc_lcto_est = ''.
            MODIFY git_saida FROM gwa_saidax INDEX lv_tabix2 TRANSPORTING obj_key doc_lcto lote doc_lcto_est.
            COMMIT WORK AND WAIT.
            CLEAR: gwa_zfit0197.

            gwa_saida_aux = CORRESPONDING #( gwa_saidax ).
            APPEND gwa_saida_aux TO git_zfit0197.
            CLEAR: gwa_saida_aux.
            FREE: lit_zglt036.
          ENDLOOP.

          CLEAR: lwa_zglt035, lva_num_lote, p_doc_lcto.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF git_zfit0197[] IS NOT INITIAL.
    MODIFY zfit0197 FROM TABLE git_zfit0197[].
    FREE: git_zfit0197.
    COMMIT WORK AND WAIT.
  ENDIF.

ENDFORM.                    "
*&---------------------------------------------------------------------*
*&      Form GERAR_DOCUMENTO
*&---------------------------------------------------------------------*
FORM f_estornar_documento.

  DATA: lva_budat(10) TYPE c,
        lva_stblg     TYPE bkpf-stblg.

  CALL METHOD grid1->get_selected_rows
    IMPORTING
      et_index_rows = tl_index_rows.

  LOOP AT tl_index_rows INTO wl_index_rows.
    READ TABLE git_saida INTO gwa_saida INDEX wl_index_rows-index.
    IF sy-subrc IS INITIAL.
      IF gwa_saida-mark = ''.
        gwa_saida-mark = 'X'.
*      ELSE.
*        gwa_saida-mark = ' '.
      ENDIF.
      MODIFY git_saida FROM gwa_saida INDEX wl_index_rows-index  TRANSPORTING mark.
      CLEAR gwa_saida.
    ENDIF.
  ENDLOOP.

  FREE: gwa_saida, git_zfit0197.

  LOOP AT git_saida INTO gwa_saida WHERE mark EQ abap_true.

    DATA(lv_tabix) = sy-tabix.

    IF gwa_saida-belnr_c IS NOT INITIAL.

      CONCATENATE gwa_saida-budat+6(2) gwa_saida-budat+4(2) gwa_saida-budat(4) INTO lva_budat.

      REFRESH: git_dta, git_msg.
      PERFORM zf_shdb USING: 'SAPMF05A' '0105' 'X'  ' '           ' ',
                             ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
                             ' '        ' '    ' '  'BDC_OKCODE'   '=BU',
                             ' '        ' '    ' '  'RF05A-BELNS'  gwa_saida-belnr_c,
                             ' '        ' '    ' '  'BKPF-BUKRS'   gwa_saida-bukrs,
                             ' '        ' '    ' '  'RF05A-GJAHS'  gwa_saida-budat(4),
                             ' '        ' '    ' '  'UF05A-STGRD'  '01',
                             ' '        ' '    ' '   'BSIS-BUDAT'  lva_budat.
      opt-dismode = 'N'.

      CALL TRANSACTION 'FB08' USING git_dta OPTIONS FROM opt
              MESSAGES INTO git_msg.

      READ TABLE git_msg ASSIGNING FIELD-SYMBOL(<fs_msg>)
                                        WITH KEY msgtyp = 'E'.

      IF sy-subrc IS INITIAL.

        REFRESH: git_bapiret.
        LOOP AT git_msg ASSIGNING <fs_msg>.

          APPEND INITIAL LINE TO git_bapiret ASSIGNING FIELD-SYMBOL(<fs_bapiret>).

          <fs_bapiret>-type       = <fs_msg>-msgtyp.
          <fs_bapiret>-id         = <fs_msg>-msgid.
          <fs_bapiret>-number     = <fs_msg>-msgnr.
          <fs_bapiret>-message_v1 = <fs_msg>-msgv1.
          <fs_bapiret>-message_v2 = <fs_msg>-msgv2.
          <fs_bapiret>-message_v3 = <fs_msg>-msgv3.
          <fs_bapiret>-message_v4 = <fs_msg>-msgv4.

        ENDLOOP.

        DELETE ADJACENT DUPLICATES FROM git_bapiret COMPARING ALL FIELDS.

        CALL FUNCTION 'FINB_BAPIRET2_DISPLAY'
          EXPORTING
            it_message = git_bapiret.

        LEAVE LIST-PROCESSING.

      ELSE.

        SELECT SINGLE stblg
          FROM bkpf INTO lva_stblg
         WHERE bukrs = gwa_saida-bukrs
           AND belnr = gwa_saida-belnr_c
           AND gjahr = gwa_saida-budat(4).

        CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

        CLEAR: gwa_saida-belnr_c.
        gwa_saida-doc_lcto     = abap_off.
        gwa_saida-obj_key      = abap_off.
        gwa_saida-lote         = abap_off.
        gwa_saida-belnr_e = lva_stblg.
        MOVE: icon_led_yellow TO gwa_saida-icons.
        MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING icons belnr_e belnr_c doc_lcto doc_lcto_est lote obj_key.
        COMMIT WORK AND WAIT.

        gwa_saida_aux = CORRESPONDING #( gwa_saida ).
        APPEND gwa_saida_aux TO git_zfit0197.
        CLEAR gwa_saida_aux.

        MODIFY zfit0197  FROM TABLE git_zfit0197.
        COMMIT WORK AND WAIT.

*        MOVE: '@02@' TO gwa_saida-icons.
*        MOVE: icon_led_yellow TO gwa_saida-icons.
*        MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING icons .
*        COMMIT WORK AND WAIT.
        FREE: gwa_saida, git_zfit0197, lva_stblg, git_bapiret, git_msg, lva_budat.

      ENDIF.
    ENDIF.
  ENDLOOP.
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
*&      Form  f_preencher_dynpro
*&---------------------------------------------------------------------*
FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.                    " f_preencher_dynpro
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
*&      Form  F_GET_RUNTIME_INFO
*&---------------------------------------------------------------------*
FORM f_get_runtime_info .

  FIELD-SYMBOLS: <lit_data> TYPE ANY TABLE,
                 <lwa_data> TYPE any.

  DATA lr_pay_data TYPE REF TO data.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data = lr_pay_data
      ).

      ASSIGN lr_pay_data->* TO <lit_data>.

    CATCH cx_salv_bs_sc_runtime_info.

      MESSAGE 'Unable to retrieve ALV data' TYPE 'E'.
  ENDTRY.

  IF <lit_data> IS ASSIGNED.
    MOVE-CORRESPONDING <lit_data> TO git_zgl033[].
  ENDIF.

  cl_salv_bs_runtime_info=>clear_all( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_DOC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_P_DOC_CONT  text
*----------------------------------------------------------------------*
FORM f_atualiza_doc.

  DATA: lwa_zibchv   TYPE zib_contabil_chv,
        lwa_ziberr   TYPE zib_contabil_err,
        lwa_zglt0108 TYPE zglt0108,
        lwa_zglt034  TYPE zglt034,
        lwa_saida    LIKE LINE OF git_saida.

  SELECT * FROM zfit0197
    INTO TABLE @DATA(it_zfit0197)
    FOR ALL ENTRIES IN @git_saida
    WHERE bukrs EQ @git_saida-bukrs
      AND hkont EQ @git_saida-hkont
      AND belnr EQ @git_saida-belnr.

  LOOP AT git_saida INTO gwa_saida." WHERE mark EQ abap_true.
    DATA(lv_tabix) = sy-tabix.

    READ TABLE it_zfit0197 INTO DATA(wa_zfit0197) WITH KEY bukrs = gwa_saida-bukrs
                                                     hkont = gwa_saida-hkont
                                                     belnr = gwa_saida-belnr
                                            BINARY SEARCH.

    IF sy-subrc IS INITIAL AND wa_zfit0197-lote IS NOT INITIAL AND wa_zfit0197-obj_key IS NOT INITIAL.
      gwa_saida = CORRESPONDING #( wa_zfit0197 ).

      SELECT SINGLE *
        FROM zib_contabil_chv
        INTO lwa_zibchv
      WHERE obj_key = gwa_saida-obj_key.

      IF ( sy-subrc IS NOT INITIAL ).
        CLEAR lwa_zibchv.
        SELECT SINGLE *
          FROM zib_contabil_err
          INTO lwa_ziberr
        WHERE obj_key = gwa_saida-obj_key.

        IF sy-subrc IS NOT INITIAL.
          CLEAR lwa_ziberr.
        ENDIF.
      ENDIF.

      IF ( lwa_zibchv IS NOT INITIAL AND lwa_zibchv-belnr IS NOT INITIAL ).

        MOVE: lwa_zibchv-belnr   TO wg_doc_cont.

        SELECT SINGLE *
        FROM zib_contabil_chv
        INTO @DATA(lwa_zibchv_aux)
        WHERE obj_key = @gwa_saida-obj_key.
        IF sy-subrc IS NOT INITIAL.
          FREE lwa_zibchv_aux.
        ENDIF.

        gwa_saida-belnr_c =  lwa_zibchv_aux-belnr.
        gwa_saida-belnr_e = ''.
        gwa_saida-mark = ''.
        MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING mark belnr_c belnr_e lote doc_lcto doc_lcto_est.

        gwa_saida_aux = CORRESPONDING #( gwa_saida ).
        APPEND gwa_saida_aux TO git_zfit0197.
        CLEAR gwa_saida_aux.

        MODIFY zfit0197  FROM TABLE git_zfit0197.
        COMMIT WORK AND WAIT.

        IF lwa_ziberr IS NOT INITIAL AND lwa_ziberr-obj_key EQ gwa_saida-obj_key AND gwa_saida-belnr_c IS INITIAL.
          MOVE: icon_led_red TO gwa_saida-icons.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING icons.
        ELSEIF gwa_saida-belnr_c IS INITIAL AND gwa_saida-belnr_e IS INITIAL AND lwa_ziberr IS INITIAL.
          MOVE: icon_led_yellow TO gwa_saida-icons.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING icons.
        ELSEIF gwa_saida-belnr_c IS NOT INITIAL AND lwa_zibchv IS NOT INITIAL AND lwa_ziberr IS INITIAL.
          MOVE: icon_led_green TO gwa_saida-icons.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING icons.
        ELSEIF lwa_ziberr IS INITIAL AND gwa_saida-belnr_c IS INITIAL.
          MOVE: icon_led_yellow TO gwa_saida-icons.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING icons.
        ELSEIF lwa_ziberr IS NOT INITIAL AND gwa_saida-belnr_c IS INITIAL.
          MOVE: icon_led_red TO gwa_saida-icons.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING icons.
        ELSEIF gwa_saida-belnr_c IS NOT INITIAL.
          MOVE: icon_led_green TO gwa_saida-icons.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING icons.
        ENDIF.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF git_saida[] IS NOT INITIAL.
    LOOP AT git_saida[] INTO DATA(wa_sai) WHERE doc_lcto IS NOT INITIAL.
      EXIT.
    ENDLOOP.
    IF wa_sai-doc_lcto IS NOT INITIAL.
      SELECT bukrs, doc_lcto, lote, loekz FROM zglt035
        INTO TABLE @DATA(it_zglt035x)
        FOR ALL ENTRIES IN @git_saida
        WHERE bukrs     EQ @git_saida-bukrs
          AND doc_lcto  EQ @git_saida-doc_lcto
          AND loekz     EQ @abap_true.
      IF sy-subrc IS INITIAL.
        SORT it_zglt035x BY bukrs doc_lcto lote.
      ENDIF.
    ENDIF.
    IF it_zglt035x IS NOT INITIAL.
      DATA: lv_lot_num TYPE zlote_num.
      LOOP AT git_saida INTO gwa_saida.
        lv_tabix = sy-tabix.
        lv_lot_num = gwa_saida-lote.
        READ TABLE it_zglt035x INTO DATA(wa_zgl35) WITH KEY bukrs    = gwa_saida-bukrs
                                                            doc_lcto = gwa_saida-doc_lcto
                                                            lote     = lv_lot_num
                                                            BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          gwa_saida-lote = abap_false.
          gwa_saida-doc_lcto = abap_false.
          MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING lote doc_lcto.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  IF gv_ucommx EQ c_atuali.
    LOOP AT git_saida INTO gwa_saida.
      lv_tabix = sy-tabix.
      gwa_saida-mark = ''.
      MODIFY git_saida FROM gwa_saida INDEX lv_tabix TRANSPORTING mark.
    ENDLOOP.
  ENDIF.
  SORT git_saida BY bukrs hkont belnr ASCENDING.
  DELETE ADJACENT DUPLICATES FROM git_saida COMPARING belnr bukrs hkont.
  SORT git_saida BY belnr ASCENDING.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ERRO_ZIB
*&---------------------------------------------------------------------*
FORM f_exibe_erro_zib USING index TYPE lvc_index.

  TYPES: BEGIN OF ty_zib_err,
           obj_key        TYPE zib_contabil_err-obj_key,
           dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
           hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
           message        TYPE zib_contabil_err-message,
         END OF ty_zib_err.

  DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

  READ TABLE git_saida INTO DATA(wa_saida) INDEX index.

  CHECK ( wa_saida-obj_key IS NOT INITIAL ).

  SELECT obj_key, dt_atualizacao, hr_atualizacao, message
     FROM zib_contabil_err INTO TABLE @lit_zib_err
  WHERE obj_key = @wa_saida-obj_key.

  IF ( sy-subrc = 0 ).
    cl_demo_output=>new(
      )->begin_section( `ZIB_CONTABIL_ERR:`
      )->write_text( |Erros encontrados na crição do documento: \n|
      ")->WRITE_DATA( SY-DATUM
      )->write_data( lit_zib_err[]
      )->end_section(
      )->display( ).
  ENDIF.

ENDFORM.
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
                              us_value.
  "CHANGING ch_subrc.

  DATA: da_dynpfield_tab LIKE dynpread OCCURS 0 WITH HEADER LINE,
        da_stepl         LIKE sy-stepl,
        da_repid         LIKE d020s-prog,
        da_dynnr         LIKE d020s-dnum.

  "ch_subrc = 4.
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
    "ch_subrc = 0.
  ENDIF.

ENDFORM.                    " DYNP_VALUES_UPDATE
*&---------------------------------------------------------------------*
*&      Form  F_GET_DOC_CONT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_show_fb03 .

  DATA: cursorfield(20),
        g_cursor        LIKE bkpf-belnr,
        lv_length       TYPE i.

  GET CURSOR FIELD cursorfield VALUE g_cursor.

  IF g_cursor IS NOT INITIAL.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = g_cursor
      IMPORTING
        output = gva_belnr.
  ENDIF.

  IF wg_doc_cont IS NOT INITIAL.
    CLEAR: gva_belnr.
    MOVE wg_doc_cont TO gva_belnr.
    SET PARAMETER ID 'BLN' FIELD gva_belnr.
    SET PARAMETER ID 'BUK' FIELD p_bukrs_invra .
    SET PARAMETER ID 'GJR' FIELD p_ano.
    CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
  ELSE.
    IF wg_estorno IS NOT INITIAL.
      CLEAR: gva_belnr.
      MOVE wg_estorno  TO gva_belnr.
      SET PARAMETER ID 'BLN' FIELD gva_belnr.
      SET PARAMETER ID 'BUK' FIELD p_bukrs_invra .
      SET PARAMETER ID 'GJR' FIELD p_ano.
      CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDIF.
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


  DATA: lwa_zibchv TYPE zib_contabil_chv,
        lwa_ziberr TYPE zib_contabil_err.

* Tabela bsis
  SELECT * FROM bsis
    INTO TABLE @DATA(it_bsis)
    WHERE bukrs IN @s_bukrs
      AND hkont IN @s_saknr
      AND hkont IN ( SELECT hkont FROM zfit0198 )
      AND budat IN @s_budat.
*      AND blart EQ 'ZR'.

  IF sy-subrc IS INITIAL AND it_bsis IS NOT INITIAL.

    SELECT * FROM t001
     INTO TABLE @DATA(it_t001)
     FOR ALL ENTRIES IN @it_bsis
     WHERE bukrs EQ @it_bsis-bukrs.

    DATA(it_bsisx) =  it_bsis.
    DELETE it_bsisx WHERE sgtxt IS INITIAL.

    SELECT * FROM zfit0199
      INTO TABLE @it_zfit0199.

    IF sy-subrc IS INITIAL.
      SORT it_zfit0199 BY texto.
    ENDIF.

    SELECT * FROM zfit0198
      INTO TABLE @DATA(it_zfit0198x).
    IF sy-subrc IS INITIAL.
      SORT it_zfit0198x BY hkont.
    ENDIF.


    SELECT * FROM zfit0198
      INTO TABLE @it_zfit0198
      FOR ALL ENTRIES IN @it_bsis
      WHERE hkont EQ @it_bsis-hkont.
    IF sy-subrc IS INITIAL.
      SORT it_zfit0198 BY hkont.
    ENDIF.

    IF it_zfit0198[] IS NOT INITIAL.
      SELECT * FROM zfit0197
        INTO TABLE @DATA(it_zfit0197)
        FOR ALL ENTRIES IN @it_bsis
        WHERE bukrs EQ @it_bsis-bukrs
          AND hkont EQ @it_bsis-hkont
          AND belnr EQ @it_bsis-belnr.

      IF sy-subrc IS INITIAL.
        SORT it_zfit0197 BY bukrs hkont belnr.
      ENDIF.

      LOOP AT it_bsis INTO DATA(wa_bsis).

        READ TABLE it_zfit0198 WITH KEY hkont = wa_bsis-hkont
                               TRANSPORTING NO FIELDS
                               BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_zfit0198x WITH KEY hkont = wa_bsis-hkont
                               TRANSPORTING NO FIELDS
                               BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        gwa_saida = CORRESPONDING #( wa_bsis ).

        IF wa_bsis-shkzg EQ 'H'.
          gwa_saida-wrbtr = gwa_saida-wrbtr * ( -1 ).
          gwa_saida-dmbe2 = gwa_saida-dmbe2 * ( -1 ).
        ENDIF.

        READ TABLE it_zfit0197 INTO DATA(wa_zfit197) WITH KEY bukrs = wa_bsis-bukrs
                                        hkont = wa_bsis-hkont
                                        belnr = wa_bsis-belnr BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING wa_zfit197 TO gwa_saida.
        ENDIF.

        LOOP AT it_zfit0199 INTO DATA(wa_fit199).

          IF wa_bsis-sgtxt IS NOT INITIAL.
            DATA(lv_sgtxt) = wa_bsis-sgtxt.
            TRANSLATE lv_sgtxt TO UPPER CASE.

            FIND wa_fit199-texto IN lv_sgtxt.
            IF sy-subrc IS INITIAL.
              gwa_saida-tp_lcto = wa_fit199-tp_lcto.
              gwa_saida-sgtxtc  = wa_fit199-descricao.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF gwa_saida-tp_lcto IS INITIAL.
          gwa_saida-del = abap_true.
        ENDIF.

        READ TABLE it_t001 INTO DATA(wa_t001) WITH KEY bukrs = wa_bsis-bukrs.
        IF sy-subrc IS INITIAL.
          gwa_saida-butxt = wa_t001-butxt.
        ENDIF.

        IF gwa_saida-belnr_c IS INITIAL.
          gwa_saida-icons = '@02@'.
        ENDIF.

        "" Inicio - KCM - 12.10.2025 - Conforme a EF ""ZGLT035 será preenchida com os dados e o campo ZGLT035-DOC_LCTO deve receber o valor do campo IT_BSIS-XBLNR
        gwa_saida-xblnr = wa_bsis-xblnr.
        "" Fim - KCM - 12.10.2025 - Conforme a EF ""ZGLT035 será preenchida com os dados e o campo ZGLT035-DOC_LCTO deve receber o valor do campo IT_BSIS-XBLNR

        APPEND gwa_saida TO git_saida.
        CLEAR gwa_saida.
      ENDLOOP.
    ENDIF.
  ENDIF.

  LOOP AT git_saida INTO gwa_saida.

*    SELECT SINGLE *
*      FROM zglt031
*      INTO @DATA(wa_zglt031)
*      WHERE tp_lcto EQ @gwa_saida-tp_lcto
*        AND blart   EQ 'ZR'.
*    IF sy-subrc IS INITIAL.
*      CONTINUE.
*    ENDIF.

    IF gwa_saida-obj_key IS NOT INITIAL.
      SELECT SINGLE *
        FROM zib_contabil_chv
        INTO lwa_zibchv
      WHERE obj_key = gwa_saida-obj_key.

      IF ( sy-subrc IS NOT INITIAL ).
        FREE lwa_zibchv.
        SELECT SINGLE *
          FROM zib_contabil_err
          INTO lwa_ziberr
        WHERE obj_key = gwa_saida-obj_key.

        IF sy-subrc IS NOT INITIAL.
          FREE lwa_ziberr.
        ENDIF.
      ENDIF.

      IF lwa_ziberr-obj_key EQ gwa_saida-obj_key AND gwa_saida-belnr_c IS INITIAL.
        MOVE: icon_led_red TO gwa_saida-icons.
        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
      ELSEIF gwa_saida-belnr_c IS INITIAL AND gwa_saida-belnr_e IS INITIAL.
        MOVE: icon_led_yellow TO gwa_saida-icons.
        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
      ELSEIF gwa_saida-belnr_c IS NOT INITIAL AND lwa_zibchv IS NOT INITIAL.
        MOVE: icon_led_green TO gwa_saida-icons.
        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
      ELSE.
        MOVE: icon_led_yellow TO gwa_saida-icons.
        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
      ENDIF.

    ELSE.
      MOVE: icon_led_yellow TO gwa_saida-icons.
      MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
    ENDIF.
  ENDLOOP.

  DATA(git_saidax) = git_saida[].
  LOOP AT git_saidax[] INTO DATA(wa_said) WHERE belnr_c IS NOT INITIAL.
    DELETE git_saida WHERE belnr EQ wa_said-belnr_c.
  ENDLOOP.

  FREE git_saidax.
  git_saidax[] = git_saida[].

* iva linhas
  LOOP AT git_saidax[] INTO DATA(wa_saidx) WHERE sgtxt+13(3) EQ 'IVA'.
    DATA(lv_tabp) = sy-tabix.

    LOOP AT git_saida INTO wa_said WHERE sgtxt(12) EQ wa_saidx-sgtxt(12)
                                           AND sgtxt+13(3) NE 'IVA'.

      DATA:int_sgtxt  TYPE p.
      CLEAR: int_sgtxt.
      DATA(text_sgtxt) = wa_saidx-sgtxt(12).
      CONDENSE text_sgtxt NO-GAPS.
      int_sgtxt = text_sgtxt.

      ""Erro ao Calcular IVA - PSA #DEVK9A23EZ
      IF int_sgtxt > 0.
        DATA(lv_tabs) = sy-tabix.
        wa_said-iva = ( wa_saidx-wrbtr / wa_said-wrbtr ) * 100.
        wa_said-wrbtrx = wa_saidx-wrbtr + wa_said-wrbtr.
        wa_said-dmbe2x = wa_saidx-dmbe2 + wa_said-dmbe2.
        wa_said-ivamk = abap_true.
        MODIFY git_saida FROM wa_said INDEX lv_tabs TRANSPORTING iva ivamk wrbtrx dmbe2x.
        COMMIT WORK.
        MODIFY git_saida FROM wa_said INDEX lv_tabp TRANSPORTING iva ivamk wrbtrx dmbe2x.
        COMMIT WORK.
      ELSE.

      ENDIF.


      CLEAR: int_sgtxt.
    ENDLOOP.
  ENDLOOP.

  FREE git_saidax.
  git_saidax[] = git_saida[].

  DATA: lv_cont TYPE i.

* duas linhas doc
  LOOP AT git_saidax[] INTO wa_saidx WHERE ivamk IS INITIAL AND wrbtrx IS INITIAL.
    lv_tabp = sy-tabix.
    CLEAR lv_cont.
    LOOP AT git_saida INTO wa_said WHERE belnr EQ wa_saidx-belnr AND ivamk IS INITIAL AND wrbtrx IS INITIAL.
      lv_tabs = sy-tabix.
      IF lv_tabp NE lv_tabs.
*        lv_cont = lv_cont + 1.
        wa_said-wrbtrx = ( wa_saidx-wrbtr ) + ( wa_said-wrbtr ).
        wa_said-dmbe2x = ( wa_saidx-dmbe2 ) + ( wa_said-dmbe2 ).
*        IF lv_cont EQ '2'.
        wa_said-docmk = abap_true.
*        ENDIF.
        MODIFY git_saida FROM wa_said INDEX lv_tabs TRANSPORTING docmk wrbtrx dmbe2x.
        COMMIT WORK.
        MODIFY git_saida FROM wa_said INDEX lv_tabp TRANSPORTING docmk wrbtrx dmbe2x.
        COMMIT WORK.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  IF git_saida[] IS INITIAL.
*    Não foram encontrados registros com os parâmetros informado.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não foram encontrados registros com os parâmetros informado!'(004).
    EXIT.
  ENDIF.

ENDFORM.

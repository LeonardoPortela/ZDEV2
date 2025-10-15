*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Camila Brand                                            &*
*& Data.....: 12.09.2022                                              &*
*& Descrição: Equivalencia patrimonial - 3 - Executar                 &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

REPORT  zglr074.
INCLUDE <icon>.
TYPES: BEGIN OF ty_saida,
         mark,
         icons          TYPE char4,
         id_mov_pl      TYPE i,
         mov_pl(65)     TYPE c,
         sld_brl_ant    TYPE p LENGTH 9 DECIMALS 2,
         sld_usd_ant    TYPE p LENGTH 9 DECIMALS 2,
         sld_brl_atu    TYPE p LENGTH 9 DECIMALS 2,
         sld_usd_atu    TYPE p LENGTH 9 DECIMALS 2,
         result_brl     TYPE p LENGTH 9 DECIMALS 2,
         result_usd     TYPE p LENGTH 9 DECIMALS 2,
         equiv_brl      TYPE p LENGTH 9 DECIMALS 2,
         equiv_usd      TYPE p LENGTH 9 DECIMALS 2,
**  Begin of " CS2023000082   #103662  FF  28.02.2023
         belnr          TYPE belnr_d,
         doc_lcto_est   TYPE stblg,
         doc_lcto       TYPE zglt0107-doc_lcto,
         belnr_2        TYPE belnr_d,
         doc_lcto_est_2 TYPE stblg,
         doc_lcto_2     TYPE zglt0107-doc_lcto_2,
** End of FF  28.02.2023

       END OF ty_saida,

       BEGIN OF ty_zgl033,
         id           TYPE  i, "n6
         subid        TYPE i,  " n6
         npage        TYPE i,  " n5
         tlevel       TYPE i,  " n2
         ergsl(10)    TYPE c,
         text(65)     TYPE c,
         waers(5)     TYPE c,
         repval       TYPE p LENGTH 9 DECIMALS 2,
         compval      TYPE p LENGTH 9 DECIMALS 2,
         absvar       TYPE p LENGTH 9 DECIMALS 2,
         relvar(10)   TYPE c,
         waer2(5)     TYPE c,
         repval2      TYPE p LENGTH 9 DECIMALS 2,
         compval2     TYPE p LENGTH 9 DECIMALS 2,
         absvar2      TYPE p LENGTH 9 DECIMALS 2,
         relvar2(10)  TYPE c,
         repvalv(15)  TYPE c,
         repcal2v(15) TYPE c,
         repcal3v(15) TYPE c,
       END OF ty_zgl033.

*Class definition for ALV toolbar
CLASS:lcl_alv_toolbar   DEFINITION DEFERRED.

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
DATA : ty_toolbar TYPE stb_button.


** Criação de tabela dinamica
DATA: git_fieldcatalog TYPE lvc_t_fcat,
      gwa_fieldcatalog TYPE lvc_s_fcat,
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
           c_cancel(6)      TYPE c VALUE 'CANCEL',
           c_atuali(6)      TYPE c VALUE 'ATUALI',
           c_search(6)      TYPE c VALUE 'SEARCH',
           c_reprov(6)      TYPE c VALUE 'REPROV',
           c_buscar(6)      TYPE c VALUE 'BUSCAR',
           c_doc_cont(8)    TYPE c VALUE  'DOC_CONT',
           c_doc_est(7)     TYPE c VALUE  'DOC_EST',
           c_clos_itens(10) TYPE c VALUE 'CLOS_ITENS'.


DATA: git_saida TYPE TABLE OF ty_saida WITH HEADER LINE.

DATA: gwa_bapiret      LIKE bapiret2.

DATA: git_zglt0104     TYPE TABLE OF zglt0104  WITH HEADER LINE,
      git_zfit0160_atu TYPE TABLE OF zfit0160  WITH HEADER LINE,
      git_zfit0160_ant TYPE TABLE OF zfit0160  WITH HEADER LINE,
      git_zgl033       TYPE TABLE OF ty_zgl033 WITH HEADER LINE,
      git_zgl033_aux   TYPE TABLE OF ty_zgl033 WITH HEADER LINE,
      git_zglt0107     TYPE TABLE OF zglt0107 WITH HEADER LINE,
      git_zglt0107_aux TYPE TABLE OF zglt0107 WITH HEADER LINE,
      git_dta          TYPE TABLE OF bdcdata,
      opt              TYPE ctu_params,
      git_msg          TYPE TABLE OF bdcmsgcoll,
      git_bapiret      LIKE STANDARD TABLE OF gwa_bapiret.


DATA: gwa_zglt0104     LIKE LINE OF git_zglt0104,
      gwa_zfit0160_atu LIKE LINE OF git_zfit0160_atu,
      gwa_zfit0160_ant LIKE LINE OF git_zfit0160_ant,
      gwa_zglt0107     LIKE LINE OF git_zglt0107,
      gwa_saida        LIKE LINE OF git_saida.


DATA: gva_variant  TYPE varid-variant,
      gva_data_ini TYPE sy-datum,
      gva_data_fim TYPE sy-datum,
      gva_belnr    TYPE zglt0107-belnr.

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
      wg_desc_bukrs_invda(30),
      wg_desc_bukrs_invra(30),
      wg_doc_cont(20),
      wg_estorno(20),
      p_doc_lcto              TYPE zglt0107-doc_lcto,
      p_objkey                TYPE zib_contabil-obj_key,
      p_doc_cont              TYPE zglt0107-belnr,
      p_estorno               TYPE zib_contabil-obj_key.

*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
INITIALIZATION.

START-OF-SELECTION.

  CALL SCREEN 100.


*** CALL
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

    DATA: vg_lote TYPE zglt034-lote.

    REFRESH: tl_bdc.

* RJF - Ini - 2023.03.28
    READ TABLE git_saida INTO DATA(gw_saida) INDEX e_row_id-index.
    IF sy-subrc IS INITIAL.

      CASE e_column_id.
        WHEN 'ICONS'.

          PERFORM f_exibe_erro_zib.

        WHEN 'BELNR'.

          IF gw_saida-belnr IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saida-belnr.
            SET PARAMETER ID 'BUK' FIELD p_bukrs_invra .
            SET PARAMETER ID 'GJR' FIELD p_ano.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'DOC_LCTO_EST'.

          IF gw_saida-doc_lcto_est IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saida-doc_lcto_est.
            SET PARAMETER ID 'BUK' FIELD p_bukrs_invra .
            SET PARAMETER ID 'GJR' FIELD p_ano.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'BELNR_2'.

          IF gw_saida-belnr_2 IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saida-belnr_2.
            SET PARAMETER ID 'BUK' FIELD p_bukrs_invra .
            SET PARAMETER ID 'GJR' FIELD p_ano.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'DOC_LCTO_EST_2'.

          IF gw_saida-doc_lcto_est_2 IS NOT INITIAL.
            SET PARAMETER ID 'BLN' FIELD gw_saida-doc_lcto_est_2.
            SET PARAMETER ID 'BUK' FIELD p_bukrs_invra .
            SET PARAMETER ID 'GJR' FIELD p_ano.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ENDIF.

        WHEN 'DOC_LCTO'.

          IF NOT gw_saida-doc_lcto IS INITIAL." AND
*             NOT cursorvalue IS INITIAL AND cursorfield = 'GW_SAIDA-DOC_LCTO'.
*        CLEAR VG_LOTE.
            SET PARAMETER ID 'BLN' FIELD gw_saida-doc_lcto.
            SET PARAMETER ID 'LOT' FIELD  vg_lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'DOC_LCTO_2'.

          IF NOT gw_saida-doc_lcto_2 IS INITIAL. " AND
*             NOT cursorvalue IS INITIAL AND cursorfield = 'GW_SAIDA-DOC_LCTO_2'.
*        CLEAR VG_LOTE.
            SET PARAMETER ID 'BLN' FIELD gw_saida-doc_lcto_2.
            SET PARAMETER ID 'LOT' FIELD vg_lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
          ENDIF.
      ENDCASE.
    ENDIF.

*    IF e_column_id EQ 'ICONS'.
*      PERFORM f_exibe_erro_zib.
*    ENDIF.
* RJF - Fim - 2023.03.28

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

    CALL METHOD c_alv_toolbarmanager->reorganize
      EXPORTING
        io_alv_toolbar = e_object.
  ENDMETHOD.                    "on_toolbar
  METHOD handle_user_command.

    CASE e_ucomm.
      WHEN c_aprov.
        PERFORM f_gerar_documento.
        PERFORM f_atualiza_doc.

      WHEN c_reprov.
        PERFORM f_estornar_documento.
*        PERFORM dynp_values_update USING  sy-repid
*                                          sy-dynnr
*                                          'WG_ESTORNO'
*                                          wg_estorno.

      WHEN c_clos_itens.
        CALL METHOD splitter->set_row_height
          EXPORTING
            id     = 1
            height = 100.

        CALL METHOD splitter->set_row_sash
          EXPORTING
            id    = 1
            type  = 0
            value = 0.

      WHEN c_atuali.
        PERFORM f_atualiza_doc.
    ENDCASE.
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
    "gwa_layout-box_fname  = 'MARK'.
    "gwa_layout-sel_mode   = c_a.
    gwa_stable-row        = c_x.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.

    CALL METHOD splitter->set_row_sash
      EXPORTING
        id    = 1
        type  = 0
        value = 0.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.


    PERFORM montar_layout_grid.
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
        is_layout            = gwa_layout
      CHANGING
        it_fieldcatalog      = git_fieldcatalog[]
        it_outtab            = git_saida[].

    SET HANDLER:
                  lcl_event_handler=>on_double_click FOR grid1.
    SET HANDLER:
                  lcl_event_handler=>on_hotsopt_click FOR grid1.
  ELSE.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

  ENDIF.


  READ TABLE git_zglt0104 INTO gwa_zglt0104 INDEX 1. " RJF

  IF sy-subrc IS INITIAL AND gwa_zglt0104-moeda_funcional EQ 'USD'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'X'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
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
      PERFORM limpa_campos.
      PERFORM busca_descricoes.
      PERFORM busca_dados.
    WHEN c_atuali.
      PERFORM busca_dados.
    WHEN c_back.
      LEAVE PROGRAM.
    WHEN c_cancel.
      LEAVE PROGRAM.
      "LEAVE LIST-PROCESSING AND RETURN TO SCREEN 100.
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
*      CALL METHOD cl_gui_cfw=>set_new_ok_code
*        EXPORTING
*          new_code = 'DUMMY'.  " triggers PAI of the screen
*      PERFORM f_show_fb03.
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













     1  ' '     'ICONS'            'GIT_SAIDA'   'ICONS'          'Status'                 '7'    ' ' ' ' 'X' ' ' 'C' ,
     2  ' '     'ID_MOV_PL'        'GIT_SAIDA'   'ID_MOV_PL'      'ID '                    '4'    ' ' ' ' ' ' 'X' ' ' ,
     3  ' '     'MOV_PL'           'GIT_SAIDA'   'MOV_PL'         'Movimentação do PL'     '40'   ' ' ' ' ' ' ' ' ' ' ,
     4  ' '     'SLD_BRL_ANT'      'GIT_SAIDA'   'SLD_BRL_ANT'    'Saldo BRL Anterior '    '20'   ' ' 'X' ' ' ' ' ' ' ,
     5  ' '     'SLD_USD_ANT'      'GIT_SAIDA'   'SLD_USD_ANT'    'Saldo USD Anterior'     '20'   ' ' 'X' ' ' ' ' ' ' ,
     6  ' '     'SLD_BRL_ATU'      'GIT_SAIDA'   'SLD_BRL_ATU'    'Saldo BRL Atual'        '20'   ' ' 'X' ' ' ' ' ' ' ,
     7  ' '     'SLD_USD_ATU'      'GIT_SAIDA'   'SLD_USD_ATU'    'Saldo USD Atual '       '20'   ' ' 'X' ' ' ' ' ' ' ,
     8  ' '     'RESULT_BRL'       'GIT_SAIDA'   'RESULT_BRL'     'Resultado BRL'          '20'   ' ' 'X' ' ' ' ' ' ' ,
     9  ' '     'RESULT_USD'       'GIT_SAIDA'   'RESULT_USD'     'Resultado USD'          '20'   ' ' 'X' ' ' ' ' ' ' ,
     10 ' '     'EQUIV_BRL'        'GIT_SAIDA'   'EQUIV_BRL'      'Equivalência BRL'       '20'   ' ' 'X' ' ' ' ' ' ' ,
     11 ' '     'EQUIV_USD'        'GIT_SAIDA'   'EQUIV_USD'      'Equivalência USD'       '20'   ' ' 'X' ' ' ' ' ' ' ,
**  Begin of " CS2023000082   #103662  FF  28.02.2023
     12  ' '     'BELNR'           'GIT_SAIDA'   'BELNR'          'Documento'              '10'   ' ' ' ' 'X' ' ' ' ' ,
     13  ' '     'DOC_LCTO_EST'    'GIT_SAIDA'   'DOC_LCTO_EST'   'Estorno'                '10'   ' ' ' ' 'X' ' ' ' ' ,
     14  ' '     'DOC_LCTO'        'GIT_SAIDA'   'DOC_LCTO'       'Doc. Lanç.'             '10'   ' ' ' ' 'X' ' ' ' ' ,
     15  ' '     'BELNR_2'         'GIT_SAIDA'   'BELNR_2'        'Documento'              '10'   ' ' ' ' 'X' ' ' ' ' ,
     16  ' '     'DOC_LCTO_EST_2'  'GIT_SAIDA'   'DOC_LCTO_EST_2' 'Estorno'                '10'   ' ' ' ' 'X' ' ' ' ' ,
     17  ' '     'DOC_LCTO_2'      'GIT_SAIDA'   'DOC_LCTO_2'     'Doc. Lanç.'             '10'   ' ' ' ' 'X' ' ' ' ' .
** End of FF  28.02.2023

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
*  gwa_fieldcatalog-key_sel       = 'X'.
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

  IF p_field EQ 'ESTORNO'.
*    gwa_fieldcatalog-CHECKBOX = C_X.
  ENDIF.

*  IF P_FIELD EQ 'DMBTR'.
*
*  ENDIF.

  APPEND gwa_fieldcatalog TO git_fieldcatalog.

ENDFORM.                    " montar_estrutura
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
FORM  busca_descricoes .
  DATA:
    wl_t001_ra TYPE t001,
    wl_t001_da TYPE t001.


  IF  p_bukrs_invra IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Investidora é Obrigatório!'.
    LEAVE TO SCREEN 0100.
  ENDIF.

  IF p_bukrs_invda IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Investida é Obrigatório!'.
    LEAVE TO SCREEN 0100.
  ENDIF.

  IF p_mes <= 0.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Mês é Obrigatório!'.
    LEAVE TO SCREEN 0100.
  ENDIF.

  IF p_ano <= 0.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Campo Ano é Obrigatório!'.
    LEAVE TO SCREEN 0100.
  ENDIF.

  SELECT SINGLE *
        FROM t001
        INTO wl_t001_ra
  WHERE bukrs EQ p_bukrs_invra.

  IF sy-subrc IS INITIAL.
    MOVE: wl_t001_ra-butxt TO wg_desc_bukrs_invra.
  ELSE.
    CLEAR : wg_desc_bukrs_invra.
  ENDIF.

  SELECT SINGLE *
        FROM t001
        INTO wl_t001_da
  WHERE bukrs EQ p_bukrs_invda.

  IF sy-subrc IS INITIAL.
    MOVE: wl_t001_da-butxt TO wg_desc_bukrs_invda.
  ELSE.
    CLEAR : wg_desc_bukrs_invda.
  ENDIF.

**  Begin of " CS2023000082   #103662  FF  28.02.2023
  IF p_mes = '12'.
    SELECT * FROM zglt0107
     INTO TABLE git_zglt0107
    WHERE monat       GE p_mes
      AND gjahr       EQ p_ano
      AND investidora EQ p_bukrs_invra
      AND investida   EQ p_bukrs_invda
      AND doc_lcto    NE ''
      AND objkey      NE '' .

  ELSE.
    SELECT * FROM zglt0107
     INTO TABLE git_zglt0107
    WHERE monat       EQ p_mes
      AND gjahr       EQ p_ano
      AND investidora EQ p_bukrs_invra
      AND investida   EQ p_bukrs_invda
      AND doc_lcto    NE ''
      AND objkey      NE '' .
  ENDIF.
** End of FF  28.02.2023


  IF sy-subrc IS INITIAL.

    READ TABLE git_zglt0107 INTO gwa_zglt0107 INDEX 1.

    MOVE: gwa_zglt0107-belnr        TO wg_doc_cont,
          gwa_zglt0107-belnr        TO p_doc_cont,
          gwa_zglt0107-doc_lcto_est TO wg_estorno.
  ELSE.
    CLEAR : wg_doc_cont.
  ENDIF.

  CONCATENATE p_ano p_mes '01' INTO gva_data_ini.

  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = gva_data_ini
    IMPORTING
      last_day_of_month = gva_data_fim
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  IF p_mes = '01'.
    p_ano_ant = p_ano - 1.
    p_mes_ant = '12'.
  ELSE.
    p_ano_ant = p_ano.
    p_mes_ant =  p_mes - 1.
  ENDIF.

*** Busca taxa mês atual TX_MED_MEDV
  SELECT *  FROM zfit0160
    INTO TABLE git_zfit0160_atu
    WHERE mes EQ p_mes
  AND ano EQ p_ano.

  IF git_zfit0160_atu[] IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Taxa Dólar não econtrada!'.
    LEAVE TO SCREEN 0100.
  ELSE.
    READ TABLE git_zfit0160_atu INTO gwa_zfit0160_atu INDEX 1.
    MOVE gwa_zfit0160_atu-tx_med_medv TO  p_taxa_media .
    MOVE gwa_zfit0160_atu-tx_med_medv TO  wg_taxa_media .

    CONDENSE wg_taxa_media NO-GAPS.
  ENDIF.

*** Busca taxa mês anterior TX_MED_MEDV
  SELECT *  FROM zfit0160
    INTO TABLE git_zfit0160_ant
    WHERE mes EQ p_mes_ant
      AND ano EQ p_ano_ant.

  IF git_zfit0160_ant[] IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Taxa Dólar não econtrada!'.
    LEAVE TO SCREEN 0100.
  ELSE.
    READ TABLE git_zfit0160_ant INTO gwa_zfit0160_ant INDEX 1.
    MOVE gwa_zfit0160_ant-tx_med_medv TO  p_taxa_media_ant.
    MOVE gwa_zfit0160_ant-tx_med_medv TO  wg_taxa_media_ant.

    CONDENSE wg_taxa_media_ant NO-GAPS.
  ENDIF.

  SELECT *
    FROM zglt0104
    INTO TABLE git_zglt0104
     WHERE investidora EQ p_bukrs_invra
      AND investida EQ p_bukrs_invda
      AND inicio_validade <= gva_data_fim
      AND fim_validade >= gva_data_fim.

  READ TABLE git_zglt0104 INTO gwa_zglt0104 INDEX 1.
  IF sy-subrc IS INITIAL.
    IF gwa_zglt0104-moeda_funcional = 'BRL'.
      gva_variant = 'SALDO BRL'.
    ELSE.
      IF gwa_zglt0104-moeda_funcional = 'USD'.
        gva_variant = 'SALDO USD'.
      ELSE.
        IF gwa_zglt0104-moeda_funcional = 'OUTRAS'.
          gva_variant = 'SALDO OUTRAS'.
        ENDIF.
      ENDIF.
    ENDIF.

    MOVE gwa_zglt0104-part_perc TO p_taxa_part .
    MOVE gwa_zglt0104-part_perc TO wg_taxa_part .

    CONDENSE wg_taxa_part NO-GAPS.

  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Parâmetros ZGL078 não encontrados!'.
    LEAVE TO SCREEN 0100.
  ENDIF.

ENDFORM.                    " BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
FORM busca_dados .

  CLEAR: gwa_zglt0107.
  READ TABLE git_zglt0107 INTO gwa_zglt0107 INDEX 1.

  IF gwa_zglt0107-doc_lcto_est IS INITIAL AND gwa_zglt0107-objkey IS NOT INITIAL .

    "Verifico se ainda existe documento na ZIB sem estar salvo na tabela.
    PERFORM f_atualiza_doc.

    LOOP AT git_zglt0107 INTO gwa_zglt0107.

      IF gwa_zglt0107-belnr IS NOT INITIAL.
        gwa_saida-icons = '@01@'.
      ELSE.
        gwa_saida-icons = '@02@'.
      ENDIF.

      gwa_saida-id_mov_pl     =    gwa_zglt0107-item_balanco  .
      gwa_saida-mov_pl        =    gwa_zglt0107-desc_cta_equ  .
      gwa_saida-sld_brl_ant   =    gwa_zglt0107-sld_brl_ant   .
      gwa_saida-sld_usd_ant   =    gwa_zglt0107-sld_usd_ant   .
      gwa_saida-sld_brl_atu   =    gwa_zglt0107-sld_brl_atu   .
      gwa_saida-sld_usd_atu   =    gwa_zglt0107-sld_usd_atu   .
      gwa_saida-equiv_brl     =    gwa_zglt0107-equiv_brl     .
*---> 10/06/2023 - Migração S4 - JS
*      gwa_saida-equiv_usd     =    gwa_zglt0107-equiv_usd     .
      gwa_saida-equiv_usd  = CONV #( gwa_zglt0107-equiv_usd  ).
*<--- 10/06/2023 - Migração S4 - JS
      gwa_saida-result_brl    =    gwa_zglt0107-result_brl    .
      gwa_saida-result_usd    =    gwa_zglt0107-result_usd    .

**  Begin of " CS2023000082   #103662  FF  28.02.2023
      gwa_saida-belnr         =    gwa_zglt0107-belnr.
      gwa_saida-doc_lcto_est  =    gwa_zglt0107-doc_lcto_est.
** End of FF  28.02.2023

*  Begin of " CS2023000082   #117124  FF  04.07.2023
      gwa_saida-doc_lcto      =    gwa_zglt0107-doc_lcto.
      gwa_saida-doc_lcto_2    =    gwa_zglt0107-doc_lcto_2.
*  End of " CS2023000082   #117124  RJF  04.07.2023

      APPEND gwa_saida TO git_saida.
      CLEAR: gwa_saida,
            gwa_zglt0107.
    ENDLOOP.

    SORT  git_saida BY id_mov_pl .

  ELSE.

    CLEAR: git_zglt0107[], gwa_zglt0107.

    PERFORM f_prepare_run_time_info.
    PERFORM f_get_range.

    SUBMIT zrfbila00 USING SELECTION-SET gva_variant WITH SELECTION-TABLE  git_selection  WITH bilatree EQ ''
                                                                                          WITH bilagrid EQ 'X' AND RETURN.
    PERFORM f_get_runtime_info.
    PERFORM organiza_dados TABLES git_zgl033
                                  git_zfit0160_atu
                                  git_zfit0160_ant   USING gwa_zglt0104 .

    CALL SCREEN 0100.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

  ENDIF.
ENDFORM.                    " BUSCA_DADOS

*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
FORM organiza_dados TABLES  p_git_zgl033   STRUCTURE  git_zgl033
                            p_zfit0160_atu STRUCTURE  git_zfit0160_atu
                            p_zfit0160_ant STRUCTURE  git_zfit0160_ant  USING p_zglt0104 STRUCTURE git_zglt0104.

  DATA: lwa_zgl033_aux LIKE LINE OF git_zgl033,
        lwa_zgl033     LIKE LINE OF git_zgl033,
        lwa_saida      LIKE LINE OF git_saida.

  DATA lit_obj_range TYPE RANGE OF e071-obj_name.
  DATA lwa_obj_range LIKE LINE OF lit_obj_range.

  IF  p_zglt0104-moeda_funcional = 'OUTRAS'.
    READ TABLE git_zfit0160_ant INTO gwa_zfit0160_ant INDEX 1.
    READ TABLE git_zfit0160_atu INTO gwa_zfit0160_atu INDEX 1.
  ENDIF.


  MOVE-CORRESPONDING p_git_zgl033[] TO git_zgl033_aux[].

  DELETE git_zgl033_aux WHERE tlevel <> 2.
  DELETE p_git_zgl033  WHERE tlevel <> 2.

  SORT git_zgl033_aux BY ergsl.

  DELETE ADJACENT DUPLICATES FROM git_zgl033_aux COMPARING npage.


  LOOP AT git_zgl033_aux INTO lwa_zgl033 .

    LOOP AT p_git_zgl033 INTO lwa_zgl033_aux WHERE npage =  lwa_zgl033-npage AND id  <> lwa_zgl033-id.

      IF gwa_zglt0107-belnr IS NOT INITIAL.
        lwa_saida-icons = '@01@'.
      ELSE.
        lwa_saida-icons = '@02@'.
      ENDIF.

      lwa_saida-id_mov_pl     = lwa_zgl033_aux-npage.
      lwa_saida-mov_pl        = lwa_zgl033-text.

      lwa_saida-result_brl    = 0.
      lwa_saida-result_usd    = 0.
      lwa_saida-equiv_brl     = 0.
      lwa_saida-equiv_usd     = 0.


*--------se moeda brl ou moeda usd --------*
      CASE p_zglt0104-moeda_funcional.
        WHEN 'USD' OR 'BRL'.

          lwa_saida-sld_brl_ant   = lwa_zgl033_aux-compval.
          lwa_saida-sld_brl_atu   = lwa_zgl033_aux-repval.

          IF p_zglt0104-moeda_funcional = 'USD'.
            lwa_saida-sld_usd_ant   = lwa_zgl033_aux-compval2.
            lwa_saida-sld_usd_atu   = lwa_zgl033_aux-repval2.
          ELSE.
            lwa_saida-sld_usd_ant = lwa_saida-sld_brl_ant / gwa_zfit0160_ant-tx_med_medv.
            lwa_saida-sld_usd_atu = lwa_saida-sld_brl_atu / gwa_zfit0160_atu-tx_med_medv.
          ENDIF.

* Resuldado

          lwa_saida-result_brl =  lwa_saida-sld_brl_atu - lwa_saida-sld_brl_ant.
          lwa_saida-result_usd =  lwa_saida-sld_usd_atu - lwa_saida-sld_usd_ant.

* Equivalência Patrimonial

*ajuste BUG 99244 - BG - INICIO
          ""Se moeda funcional = BRL Equivalencia USD" =  ( "Resultado USD"  *  "taxa participação" )
          " MOEDA FUNCIONAL = USD = Equivalência BRL = ("Resultado BRL" * " Taxa participação")

*          IF  p_zglt0104-moeda_funcional = 'USD'.
*
*            lwa_saida-equiv_brl =  ( ( lwa_saida-result_brl * gwa_zfit0160_atu-tx_med_medv ) * p_zglt0104-part_perc ) / 100.
*
*          ELSE.
*            lwa_saida-equiv_brl =  ( lwa_saida-result_brl * p_zglt0104-part_perc ) / 100.
*          ENDIF.
*
*          IF  p_zglt0104-moeda_funcional = 'BRL'.
*            lwa_saida-equiv_usd =  ( lwa_saida-equiv_brl / gwa_zfit0160_atu-tx_med_medv ).
*
*          ELSE.
*            lwa_saida-equiv_usd =  ( lwa_saida-result_usd * p_zglt0104-part_perc ) / 100.
*          ENDIF.

          lwa_saida-equiv_brl =  ( lwa_saida-result_brl * p_zglt0104-part_perc ) / 100.

*          IF  p_zglt0104-moeda_funcional = 'BRL'.
*
*            lwa_saida-equiv_usd =  ( lwa_saida-result_brl / gwa_zfit0160_atu-tx_med_medv ) * ( p_zglt0104-part_perc  / 100 ).
*
*          ELSE.

          lwa_saida-equiv_usd =  ( lwa_saida-result_usd * p_zglt0104-part_perc ) / 100.

*          ENDIF.

*lwa_saida-equiv_brl =  ( lwa_saida-result_brl * p_zglt0104-part_perc ) / 100.
*lwa_saida-equiv_usd =  ( lwa_saida-result_usd * p_zglt0104-part_perc ) / 100.

*ajuste BUG 99244 - BG - FIM

        WHEN OTHERS.

          lwa_saida-sld_brl_ant = 0.
          lwa_saida-sld_usd_ant   = lwa_zgl033_aux-compval2. "lwa_zgl033_aux-compval. BUG 99244
          lwa_saida-sld_brl_atu = 0.
          lwa_saida-sld_usd_atu   = lwa_zgl033_aux-repval2.

* Anterior BRL
          lwa_saida-sld_brl_ant = lwa_saida-sld_usd_ant * gwa_zfit0160_ant-tx_med_medv.

* Atual BRL
          lwa_saida-sld_brl_atu =   lwa_saida-sld_usd_atu * gwa_zfit0160_atu-tx_med_medv.

* Resuldado
          lwa_saida-result_brl = lwa_saida-sld_brl_atu - lwa_saida-sld_brl_ant.
          lwa_saida-result_usd = lwa_saida-sld_usd_atu - lwa_saida-sld_usd_ant.

* Equivalência Patrimonial

          "lwa_saida-equiv_brl =  ( lwa_saida-result_brl * p_zglt0104-part_perc ) / 100.
          "lwa_saida-equiv_brl =  ( ( lwa_saida-result_brl * gwa_zfit0160_atu-tx_med_medv ) * p_zglt0104-part_perc ) / 100. "BUG 99244
          lwa_saida-equiv_brl =  (  lwa_saida-result_brl  * p_zglt0104-part_perc ) / 100.

          lwa_saida-equiv_usd =  ( lwa_saida-result_usd * p_zglt0104-part_perc ) / 100.

      ENDCASE.

      APPEND lwa_saida TO git_saida.

      CLEAR:  lwa_saida,
              lwa_zgl033_aux,
              lwa_zgl033.

    ENDLOOP.
  ENDLOOP.

  lwa_obj_range-sign = 'I'.
  lwa_obj_range-option = 'CP'.
  CONCATENATE 'RESULTADO DO PERIODO' '*' INTO lwa_obj_range-low.
  APPEND lwa_obj_range TO lit_obj_range.

  lwa_obj_range-sign = 'I'.
  lwa_obj_range-option = 'CP'.
  CONCATENATE 'PROVISOES P/ IRPJ E CSLL' '*' INTO lwa_obj_range-low.
  APPEND lwa_obj_range TO lit_obj_range.

  lwa_obj_range-sign = 'I'.
  lwa_obj_range-option = 'CP'.
  CONCATENATE 'PROVISOES P/PARTICIPAÇOES NOS RESULTADOS' '*' INTO lwa_obj_range-low.
  APPEND lwa_obj_range TO lit_obj_range.

  DELETE git_saida WHERE mov_pl NOT IN lit_obj_range.

  SORT git_saida BY id_mov_pl.

  IF git_saida[] IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL033 não econtrados'
                                           'com parâmetros informados!'.
    EXIT.
  ENDIF.

ENDFORM.                    " ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*&      Form  LIMPA_CAMPOS
*&---------------------------------------------------------------------*
FORM limpa_campos.

  CLEAR: p_taxa_part,
         p_taxa_media,
         p_taxa_media_ant,
         p_doc_cont,
         p_doc_lcto,
         p_estorno,
         wg_desc_bukrs_invda,
         wg_desc_bukrs_invra,
         wg_taxa_part,
         wg_taxa_media,
         wg_taxa_media_ant,
         wg_doc_cont,
         wg_estorno.

  CLEAR: git_zglt0104[],
         git_zfit0160_atu[],
         git_zfit0160_ant[],
         git_selection[],
         git_zgl033[],
         git_zgl033[],
         git_saida[].

  CLEAR: gwa_zfit0160_ant,
         gwa_zfit0160_atu.


  CLEAR: gva_variant.

ENDFORM.                    " LIMPA_CAMPOS
*&---------------------------------------------------------------------*
*&      Form GERAR_DOCUMENTO
*&---------------------------------------------------------------------*
FORM f_gerar_documento.

  DATA: lva_num_lote  TYPE zlote_num,
        lva_dp_resp   TYPE char2,
        lva_blart     TYPE zglt031-blart,
        lva_bktxt     TYPE zglt031-bktxt,
        lva_prov_est  TYPE zglt031-prov_est,
        lva_liberado  TYPE char01,
        lv_tabix      TYPE i,
        lva_equiv_brl TYPE ty_saida-equiv_brl,
        lva_equiv_usd TYPE ty_saida-equiv_usd,
        chave_saldo   TYPE c,
        lva_objkey    TYPE zib_contabil_chv-obj_key.

  DATA: lit_zglt036  TYPE TABLE OF zglt036,
        lit_zglt032  TYPE TABLE OF zglt032,
        lit_zglt0107 TYPE TABLE OF zglt0107.

  DATA: lwa_saida    TYPE ty_saida,
        lwa_zglt035  TYPE zglt035,
        lwa_zglt0105 TYPE zglt0105,
        lwa_zglt036  TYPE zglt036,
        lwa_zglt032  TYPE zglt032,
        lwa_zglt0107 TYPE zglt0107.


  READ TABLE git_zglt0107 INTO gwa_zglt0107 INDEX 1.

  IF ( gwa_zglt0107-belnr IS INITIAL AND gwa_zglt0107-objkey IS INITIAL ) OR
      ( gwa_zglt0107-objkey IS NOT INITIAL AND gwa_zglt0107-doc_lcto_est IS NOT INITIAL ) .

    DATA: sum_equiv_usd TYPE zglt0107-equiv_usd,
          sum_equiv_brl TYPE zglt0107-equiv_brl.


    LOOP AT git_saida INTO DATA(w_saida).
      ADD w_saida-equiv_usd TO sum_equiv_usd.
      ADD w_saida-equiv_brl TO sum_equiv_brl.
    ENDLOOP.

**  Begin of CS2023000082  #103662 FF   23.02.2023

*    IF sum_equiv_usd < 0.
*      chave_negativo = '2'.  "Negativo
*    ELSE.
*      chave_negativo = '1'.  "Positivo
*    ENDIF.

    IF sum_equiv_usd >= 0 AND sum_equiv_brl >= 0.
      chave_saldo = '1'.  "Positivo
      IF sum_equiv_usd EQ 0 AND sum_equiv_brl EQ 0.
        DATA(lv_n_gerar) = abap_true.
      ENDIF.
    ELSEIF sum_equiv_usd < 0 AND sum_equiv_brl < 0.
      chave_saldo = '2'.  "Negativo
    ELSE.
      DATA(lv_n_lanc) = abap_true.
    ENDIF.
    IF chave_saldo IS NOT INITIAL.

      SELECT *
        FROM zglt0105
        INTO TABLE @DATA(lt_zglt0105)
      WHERE investidora EQ @p_bukrs_invra
        AND investida EQ @p_bukrs_invda
        AND moeda_funcional EQ @gwa_zglt0104-moeda_funcional
        AND tp_lancamento EQ 1
        AND saldo EQ @chave_saldo.

    ELSE.
      lv_n_lanc = 2.

      IF sum_equiv_brl >= 0.
        chave_saldo = '1'.  "Positivo
      ELSE.
        chave_saldo = '0'.  "Negativo
      ENDIF.

      SELECT * UP TO 2 ROWS "Selecionar as 2 primeiras linhas
        FROM zglt0105 INTO TABLE lt_zglt0105
      WHERE investidora EQ p_bukrs_invra
        AND investida EQ p_bukrs_invda
        AND moeda_funcional = 'BRL'
        AND tp_lancamento EQ 1
        AND saldo EQ chave_saldo.


      IF sum_equiv_usd >= 0.
        chave_saldo = '1'.  "Positivo
      ELSE.
        chave_saldo = '0'.  "Negativo
      ENDIF.

      SELECT * UP TO 2 ROWS "Selecionar as 2 ultimas linhas
        FROM zglt0105 INTO TABLE @DATA(lt_zglt0105x)
      WHERE investidora EQ @p_bukrs_invra
        AND investida EQ @p_bukrs_invda
        AND moeda_funcional = 'USD'
        AND tp_lancamento EQ 1
        AND saldo EQ @chave_saldo
        ORDER BY investidora,
                 investida,
                 moeda_funcional,
                 saldo,
                 tp_lancamento DESCENDING.

    ENDIF.

    IF lv_n_lanc NE 2.
      lv_n_lanc = 1.
    ENDIF.

* RJF - Ini - 2023.03.21
    DO lv_n_lanc TIMES. " RJF verificar modificações ...
      lv_tabix = lv_tabix + 1.

      IF lv_n_lanc EQ 2 AND lv_tabix EQ 2.
        FREE lt_zglt0105.
        lt_zglt0105[] = lt_zglt0105x[].
      ENDIF.

*    IF lwa_zglt0105 IS NOT INITIAL.
      IF lt_zglt0105[] IS NOT INITIAL.

        LOOP AT lt_zglt0105 INTO lwa_zglt0105.
**  End of FF  23.02.2023

          SELECT SINGLE blart  bktxt prov_est FROM zglt031 INTO ( lva_blart, lva_bktxt, lva_prov_est ) WHERE tp_lcto = lwa_zglt0105-modelo_zgl.
          lva_dp_resp = '83'. "Contabilidade

* Criar lote na ZGLT034
          CALL METHOD zcl_gerar_lote=>create_lote
            EXPORTING
              i_bukrs       = p_bukrs_invra
              i_descr_lote  = 'Equivalencia Patrimonial'
              i_user_resp   = sy-uname
              i_dep_resp    = lva_dp_resp
              i_status_lote = 'L'
            IMPORTING
              e_num_lote    = lva_num_lote.

* Criar cabeçalho na ZGLT035
          MOVE:    lva_num_lote            TO lwa_zglt035-lote,
                   p_bukrs_invra           TO lwa_zglt035-bukrs,
                   lwa_zglt0105-modelo_zgl TO lwa_zglt035-tp_lcto,
                   lva_dp_resp             TO lwa_zglt035-dpto_resp,
                   'BRL'                   TO lwa_zglt035-moeda_doc,
                   ''                      TO lwa_zglt035-st_lc_moeda,
                   lva_blart               TO lwa_zglt035-blart,
                   lva_bktxt               TO lwa_zglt035-bktxt,
                   gva_data_fim            TO lwa_zglt035-bldat,
                   gva_data_fim            TO lwa_zglt035-budat,
                   sy-datum                TO lwa_zglt035-dt_lcto,
                   lva_prov_est            TO lwa_zglt035-prov_est,
                   p_mes                   TO lwa_zglt035-monat,
                   p_ano                   TO lwa_zglt035-gjahr,
                   sy-uname                TO lwa_zglt035-usnam,
                   sy-datum                TO lwa_zglt035-dt_entrada,
                   sy-uzeit                TO lwa_zglt035-hr_entrada.

          CONCATENATE p_bukrs_invra '-' p_bukrs_invda INTO lwa_zglt035-xblnr.

          IF lv_n_lanc EQ 2 AND  lv_tabix EQ 2.
            MOVE: 'X'    TO lwa_zglt035-st_lc_moeda,
                  'USD'  TO lwa_zglt035-moeda_doc.
          ENDIF.

* Criar item na ZGLT036

          SELECT *
            FROM zglt032 INTO TABLE lit_zglt032
          WHERE tp_lcto EQ  lwa_zglt035-tp_lcto.

          IF lit_zglt032 IS NOT INITIAL.

            CLEAR: git_zglt0107[].
            LOOP AT git_saida INTO gwa_saida.

              MOVE:
                    p_mes                     TO gwa_zglt0107-monat,
                    p_ano                     TO gwa_zglt0107-gjahr,
                    p_bukrs_invra             TO gwa_zglt0107-investidora,
                    p_bukrs_invda             TO gwa_zglt0107-investida,
                    lwa_zglt035-lote          TO gwa_zglt0107-lote,
                    lwa_zglt035-doc_lcto      TO gwa_zglt0107-doc_lcto,
                    ''                        TO gwa_zglt0107-doc_lcto_est,
                    sy-uname                  TO gwa_zglt0107-usnam,
                    sy-datum                  TO gwa_zglt0107-dt_atual,
                    sy-uzeit                  TO gwa_zglt0107-hr_atual.


              gwa_zglt0107-item_balanco  = gwa_saida-id_mov_pl    .










*            gwa_zglt0107-moeda_lanc  = 'BRL'.

              gwa_zglt0107-desc_cta_equ  = gwa_saida-mov_pl       .
              gwa_zglt0107-sld_brl_ant   = gwa_saida-sld_brl_ant  .
              gwa_zglt0107-sld_usd_ant   = gwa_saida-sld_usd_ant  .
              gwa_zglt0107-sld_brl_atu   = gwa_saida-sld_brl_atu  .
              gwa_zglt0107-sld_usd_atu   = gwa_saida-sld_usd_atu  .
              gwa_zglt0107-equiv_brl     = gwa_saida-equiv_brl    .
              gwa_zglt0107-equiv_usd     = gwa_saida-equiv_usd    .
              gwa_zglt0107-result_brl    = gwa_saida-result_brl   .
              gwa_zglt0107-result_usd    = gwa_saida-result_usd   .




              CONDENSE gwa_zglt0107-item_balanco NO-GAPS.

              lva_equiv_brl = lva_equiv_brl + gwa_saida-equiv_brl.
              lva_equiv_usd = lva_equiv_usd + gwa_saida-equiv_usd.




              APPEND gwa_zglt0107 TO git_zglt0107.
              APPEND gwa_zglt0107 TO git_zglt0107_aux.
              CLEAR: gwa_saida,
                     gwa_zglt0107.

            ENDLOOP.



            LOOP AT lit_zglt032 INTO lwa_zglt032.













              lwa_zglt036-tp_lcto = lwa_zglt035-tp_lcto.





              MOVE: sy-tabix                   TO lwa_zglt036-seqitem,
                    lwa_zglt032-bschl          TO lwa_zglt036-bschl,
                    lwa_zglt032-hkont          TO lwa_zglt036-hkont,
                    lwa_zglt032-sgtxt          TO lwa_zglt036-sgtxt,
                    lwa_zglt032-anbwa           TO lwa_zglt036-anbwa,
                    lwa_zglt032-kostl           TO lwa_zglt036-kostl,
                    lwa_zglt032-prctr           TO lwa_zglt036-prctr,
                    lwa_zglt032-aufnr           TO lwa_zglt036-aufnr,
                    lwa_zglt032-matnr           TO lwa_zglt036-matnr,
                    lwa_zglt032-matnr_fi        TO lwa_zglt036-matnr_fi,
                    lwa_zglt032-zuonr           TO lwa_zglt036-zuonr,
                    lwa_zglt032-umskz           TO lwa_zglt036-umskz,
                    lwa_zglt032-vbund           TO lwa_zglt036-vbund.

              CASE p_bukrs_invra.
                WHEN '0100'. MOVE 'T001' TO lwa_zglt036-gsber.
                WHEN OTHERS.
                  CONCATENATE p_bukrs_invra+2(2) '01' INTO lwa_zglt036-gsber.
              ENDCASE.

              MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc,
                    abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int,
                    abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.



              APPEND lwa_zglt036 TO lit_zglt036.
              CLEAR: lwa_zglt036, lwa_zglt032.

            ENDLOOP.
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Tipo de lançamento não econtrados ZGLT032!'.
            EXIT.
          ENDIF.
*
** RJF - Ini - 2023.03.21
*        DO lv_n_lanc TIMES. " RJF verificar modificações ...
*          DATA(lv_tabix) = sy-tabix.
*          IF lv_n_lanc EQ 2 AND lv_tabix EQ 2.
*            MOVE: 'X'    TO lwa_zglt035-st_lc_moeda,
*                  'USD'  TO lwa_zglt035-moeda_doc.
*
*            CALL METHOD zcl_gerar_lote=>create_lote
*              EXPORTING
*                i_bukrs      = p_bukrs_invra
*                i_descr_lote = 'Equivalencia Patrimonial'
*                i_user_resp  = sy-uname
*                i_dep_resp   = lva_dp_resp
*              IMPORTING
*                e_num_lote   = lva_num_lote.
*
** Criar cabeçalho na ZGLT035
*            MOVE:    lva_num_lote            TO lwa_zglt035-lote.
*
*          ENDIF.
* RJF - Fim - 2023.03.21

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
          COMMIT WORK.
          MOVE lwa_zglt035-doc_lcto TO p_doc_lcto.



          CLEAR: lva_objkey.
          CONCATENATE 'ZGL17'  p_doc_lcto p_ano INTO lva_objkey.

          IF lv_tabix EQ 1.
            LOOP AT git_zglt0107 INTO gwa_zglt0107.
              gwa_zglt0107-objkey   = lva_objkey.
              gwa_zglt0107-lote     = lwa_zglt035-lote.
              gwa_zglt0107-doc_lcto = p_doc_lcto.
              gwa_zglt0107-doc_lcto_est = ''.
              MODIFY git_zglt0107 FROM gwa_zglt0107 INDEX sy-tabix TRANSPORTING objkey doc_lcto doc_lcto_est.
              CLEAR: gwa_zglt0107.
            ENDLOOP.

          ELSEIF lv_tabix EQ 2.







            LOOP AT git_zglt0107 INTO gwa_zglt0107.
              gwa_zglt0107-objkey_2       = lva_objkey.
              gwa_zglt0107-lote_2         = lwa_zglt035-lote.
              gwa_zglt0107-doc_lcto_2     = p_doc_lcto.
              gwa_zglt0107-doc_lcto_est_2 = ''.
              MODIFY git_zglt0107 FROM gwa_zglt0107 INDEX sy-tabix TRANSPORTING objkey_2 doc_lcto_2 doc_lcto_est_2.
              CLEAR: gwa_zglt0107.
            ENDLOOP.

          ENDIF.






          DELETE FROM zglt0107 WHERE monat       = gwa_zglt0107-monat
                                AND  gjahr       = gwa_zglt0107-gjahr
                                AND  investidora = gwa_zglt0107-investidora
                                AND  investida   = gwa_zglt0107-investida.
          COMMIT WORK.



          MODIFY zglt0107 FROM TABLE git_zglt0107.
          COMMIT WORK.

          MOVE: '' TO wg_estorno.

*        MESSAGE s836(sd) WITH 'O documento foi gerado com sucesso!'.
*
*        CALL SCREEN 0100.
*
*        CALL METHOD grid1->refresh_table_display
*          EXPORTING
*            is_stable = gwa_stable.


**  Begin of CS2023000082  #103662 FF   23.02.2023



        ENDLOOP.

      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL079 não econtrados'
                                                'com parâmetros informados!'.
        EXIT.
      ENDIF.

    ENDDO.

    IF lva_liberado IS NOT INITIAL.
      MESSAGE s836(sd) WITH 'Documento(s) gerado(s) com sucesso!'.
    ENDIF.

    CALL SCREEN 0100.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.
*  ELSE.
*    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL079 não econtrados'
*                                            'com parâmetros informados!'.
*    EXIT.
*  ENDIF.
**  End of FF  23.02.2023

  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existe documento gerado!'.
    EXIT.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form GERAR_DOCUMENTO
*&---------------------------------------------------------------------*
FORM f_estornar_documento.

  DATA: lva_budat(10) TYPE c,
        lv_tabixt     TYPE i,
        lva_stblg     TYPE bkpf-stblg,
        lva_stblg2    TYPE bkpf-stblg.

  CLEAR: gwa_zglt0107.

  READ TABLE git_zglt0107 INTO gwa_zglt0107 INDEX 1.

* RJF - Ini - 2023.03.21
  IF gwa_zglt0107-belnr_2 IS NOT INITIAL AND gwa_zglt0107-belnr IS NOT INITIAL.
    DATA(lv_time) = 2.
  ELSEIF gwa_zglt0107-belnr IS NOT INITIAL.
    lv_time = 1.
  ENDIF.
* RJF - Fim - 2023.03.24

* RJF - Ini - 2023.03.21
  DO lv_time TIMES.
    lv_tabixt = lv_tabixt + 1.

    IF lv_tabixt EQ 1.
      DATA(lv_belnr) = gwa_zglt0107-belnr.
    ELSEIF lv_tabixt EQ 2.
      lv_belnr = gwa_zglt0107-belnr_2.
    ENDIF.

    IF lv_belnr IS NOT INITIAL.









*  IF gwa_zglt0107-belnr IS NOT INITIAL.
* RJF - Fim - 202303.21


      CONCATENATE gva_data_fim+6(2) gva_data_fim+4(2) gva_data_fim(4) INTO lva_budat.




      REFRESH: git_dta, git_msg.
      PERFORM zf_shdb USING: 'SAPMF05A' '0105' 'X'  ' '           ' ',
                             ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
                             ' '        ' '    ' '  'BDC_OKCODE'   '=BU',
*                           ' '        ' '    ' '  'RF05A-BELNS'  gwa_zglt0107-belnr,
                             ' '        ' '    ' '  'RF05A-BELNS'  lv_belnr, " RJF
                             ' '        ' '    ' '  'BKPF-BUKRS'   gwa_zglt0107-investidora,
                             ' '        ' '    ' '  'RF05A-GJAHS'  gwa_zglt0107-gjahr,
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





        IF lv_tabixt EQ '1'.
          SELECT SINGLE stblg
            FROM bkpf INTO lva_stblg
           WHERE bukrs = gwa_zglt0107-investidora
             AND belnr = gwa_zglt0107-belnr
             AND gjahr = gwa_zglt0107-gjahr.

          CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

          LOOP AT git_zglt0107 INTO gwa_zglt0107.
            CLEAR: gwa_zglt0107-belnr.
            gwa_zglt0107-doc_lcto_est = lva_stblg.

            MODIFY git_zglt0107 FROM gwa_zglt0107 INDEX sy-tabix  TRANSPORTING belnr doc_lcto_est doc_lcto.
            MOVE lva_stblg TO gwa_saida-doc_lcto_est.

            MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING doc_lcto_est doc_lcto.

          ENDLOOP.
        ELSEIF lv_tabixt EQ '2'.
          SELECT SINGLE stblg
            FROM bkpf INTO lva_stblg
           WHERE bukrs = gwa_zglt0107-investidora
             AND belnr = gwa_zglt0107-belnr_2
             AND gjahr = gwa_zglt0107-gjahr.

          CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

          LOOP AT git_zglt0107 INTO gwa_zglt0107.
            CLEAR: gwa_zglt0107-belnr_2.
            gwa_zglt0107-doc_lcto_est_2 = lva_stblg.
            MODIFY git_zglt0107 FROM gwa_zglt0107 INDEX sy-tabix  TRANSPORTING belnr_2 doc_lcto_est_2 doc_lcto_2.

            MOVE lva_stblg TO gwa_saida-doc_lcto_est_2.

            MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING doc_lcto_est_2 doc_lcto_2.
          ENDLOOP.
        ENDIF.


        MODIFY zglt0107  FROM TABLE git_zglt0107.
        COMMIT WORK.

        MOVE: lva_stblg TO wg_estorno,
              ''        TO wg_doc_cont.

        LOOP AT git_saida INTO gwa_saida.
          MOVE: '@02@' TO gwa_saida-icons.
          MOVE lva_stblg TO gwa_saida-doc_lcto_est.

          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons doc_lcto_est.
        ENDLOOP.
*
*      CALL METHOD grid1->refresh_table_display
*        EXPORTING
*          is_stable = gwa_stable.

        CALL SCREEN 0100.

        CALL METHOD grid1->refresh_table_display
          EXPORTING
            is_stable = gwa_stable.

        MESSAGE s836(sd) WITH 'O documento foi estornado com sucesso!'.

      ENDIF.
    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não existe documento para estorno!'.
      EXIT.
    ENDIF.

  ENDDO.



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
      IMPORTING r_data = lr_pay_data
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
*&      Form  F_GET_RANGE
*&---------------------------------------------------------------------*
FORM f_get_range .

  CLEAR: git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'SD_KTOPL'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '0050'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'SD_BUKRS'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = p_bukrs_invda.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'SD_CURTP'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '10'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'SD_CURT2'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '40'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'B-MONATE'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'BT'.
  gwa_selection-low     = p_mes.
  gwa_selection-high    = p_mes.
**  Begin of " CS2023000082   #103662  FF  28.02.2023
  IF p_mes = '12'.
    gwa_selection-high = '16'. "'13'. " RJF
  ENDIF.
** End of FF  28.02.2023
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILBJAHR'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = p_ano.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'V-MONATE'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'BT'.
  gwa_selection-low     =  p_mes_ant.
  gwa_selection-high    =  p_mes_ant.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILVJAHR'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = p_ano_ant.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILAVERS'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '0010'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILASPRA'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = 'PT'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILABTYP'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '1'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'STICHTAG'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low    = sy-datum.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'KURS_TYP'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = 'M'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILABKON'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '3'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILAGKON'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '3'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILAVART'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '1'.
  APPEND gwa_selection TO git_selection.

  CLEAR: gwa_selection .
  gwa_selection-selname = 'BILASKAL'.
  gwa_selection-kind    = 'S'.
  gwa_selection-sign    = 'I'.
  gwa_selection-option  = 'EQ'.
  gwa_selection-low     = '0/0'.
  APPEND gwa_selection TO git_selection.

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
        lwa_zglt0107 TYPE zglt0107,
        lwa_zglt034  TYPE zglt034,
        lwa_saida    LIKE LINE OF git_saida.


  READ TABLE git_zglt0107 INTO gwa_zglt0107 INDEX 1.

  IF gwa_zglt0107-belnr IS INITIAL AND gwa_zglt0107-doc_lcto_est IS INITIAL.

    SELECT SINGLE *
      FROM zglt034
      INTO lwa_zglt034
     WHERE bukrs = gwa_zglt0107-investidora
       AND lote  = gwa_zglt0107-lote.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE *
      FROM zib_contabil_chv
      INTO lwa_zibchv
    WHERE obj_key = gwa_zglt0107-objkey.

    IF ( sy-subrc IS NOT INITIAL ).

      SELECT SINGLE *
        FROM zib_contabil_err
        INTO lwa_ziberr
      WHERE obj_key = gwa_zglt0107-objkey.

    ENDIF.

    IF ( lwa_zibchv IS NOT INITIAL AND lwa_zibchv-belnr IS NOT INITIAL ).

      MOVE: lwa_zibchv-belnr   TO wg_doc_cont.

      LOOP AT git_zglt0107 INTO gwa_zglt0107.
        gwa_zglt0107-belnr =  lwa_zibchv-belnr.
        MODIFY git_zglt0107 FROM gwa_zglt0107 INDEX sy-tabix TRANSPORTING belnr.
      ENDLOOP.

      MODIFY zglt0107  FROM TABLE git_zglt0107.
      COMMIT WORK.

      MOVE: gwa_zglt0107-belnr TO wg_doc_cont,
            gwa_zglt0107-belnr TO p_doc_cont.

      LOOP AT git_saida INTO gwa_saida.
        MOVE: '@01@' TO gwa_saida-icons.

        IF gwa_zglt0107-belnr IS NOT INITIAL.
          MOVE: gwa_zglt0107-belnr TO gwa_saida-belnr.
        ENDIF.

        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
      ENDLOOP.

    ELSEIF ( lwa_ziberr IS NOT INITIAL ).

      LOOP AT git_saida INTO gwa_saida.
        MOVE: '@02@' TO gwa_saida-icons.

        IF gwa_zglt0107-belnr IS NOT INITIAL.
          MOVE: gwa_zglt0107-belnr TO gwa_saida-belnr.
        ENDIF.

        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
      ENDLOOP.

    ENDIF.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

    IF git_saida[] IS NOT INITIAL.
      CALL SCREEN 0100.
      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = gwa_stable.
    ENDIF.

  ENDIF.

* RJF - Ini
  IF gwa_zglt0107-belnr_2 IS INITIAL AND gwa_zglt0107-doc_lcto_est_2 IS INITIAL.

    SELECT SINGLE *
      FROM zglt034
      INTO lwa_zglt034
     WHERE bukrs = gwa_zglt0107-investidora
       AND lote  = gwa_zglt0107-lote_2.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE *
      FROM zib_contabil_chv
      INTO lwa_zibchv
    WHERE obj_key = gwa_zglt0107-objkey_2.

    IF ( sy-subrc IS NOT INITIAL ).

      SELECT SINGLE *
        FROM zib_contabil_err
        INTO lwa_ziberr
      WHERE obj_key = gwa_zglt0107-objkey_2.

    ENDIF.

    IF ( lwa_zibchv IS NOT INITIAL AND lwa_zibchv-belnr IS NOT INITIAL ).

      MOVE: lwa_zibchv-belnr   TO wg_doc_cont.

      LOOP AT git_zglt0107 INTO gwa_zglt0107.
        gwa_zglt0107-belnr_2 =  lwa_zibchv-belnr.
        MODIFY git_zglt0107 FROM gwa_zglt0107 INDEX sy-tabix TRANSPORTING belnr_2.
      ENDLOOP.

      MODIFY zglt0107  FROM TABLE git_zglt0107.
      COMMIT WORK.

      MOVE: gwa_zglt0107-belnr_2 TO wg_doc_cont,
            gwa_zglt0107-belnr_2 TO p_doc_cont.

      LOOP AT git_saida INTO gwa_saida.
        MOVE: '@01@' TO gwa_saida-icons.

        IF gwa_zglt0107-belnr_2 IS NOT INITIAL.
          MOVE: gwa_zglt0107-belnr_2 TO gwa_saida-belnr_2.
        ENDIF.

        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons belnr_2 doc_lcto_2.
      ENDLOOP.

    ELSEIF ( lwa_ziberr IS NOT INITIAL ).

      LOOP AT git_saida INTO gwa_saida.
        MOVE: '@02@' TO gwa_saida-icons.

        IF gwa_zglt0107-belnr_2 IS NOT INITIAL.
          MOVE: gwa_zglt0107-belnr_2 TO gwa_saida-belnr_2.
        ENDIF.

        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons belnr_2 doc_lcto_2.
      ENDLOOP.

    ENDIF.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

    IF git_saida[] IS NOT INITIAL.
      CALL SCREEN 0100.
      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = gwa_stable.
    ENDIF.
  ENDIF.
* RJF - Fim

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ERRO_ZIB
*&---------------------------------------------------------------------*
FORM f_exibe_erro_zib .

  READ TABLE git_zglt0107 INTO gwa_zglt0107 INDEX 1.

  TYPES: BEGIN OF ty_zib_err,
           obj_key        TYPE zib_contabil_err-obj_key,
           dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
           hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
           message        TYPE zib_contabil_err-message,
         END OF ty_zib_err.

  DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

  CHECK ( gwa_zglt0107-objkey IS NOT INITIAL ).

  SELECT obj_key, dt_atualizacao, hr_atualizacao, message
     FROM zib_contabil_err INTO TABLE @lit_zib_err
  WHERE obj_key = @gwa_zglt0107-objkey.

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

*&--------------------------------------------------------------------&*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Camila Brand                                            &*
*& Data.....: 10.10.2022                                              &*
*& Descrição: Equivalencia patrimonial Executar - Conversão Balanço   &*
*& Transação:                                                         &*
*&--------------------------------------------------------------------&*
*& Projeto  :                                                         &*
*& Código Espec.Funcional/Técnica:                                    &*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor           Request      Data         Descrição                &*
*&--------------------------------------------------------------------&*

REPORT  zglr075.
INCLUDE <icon>.
TYPES: BEGIN OF ty_saida,
         mark,
         icons          TYPE char4,
         id_mov_pl      TYPE i,
         mov_pl(65)     TYPE c,
         moeda_per(5)   TYPE c,
         moeda_comp(5)  TYPE c,
         vlr_brl        TYPE p LENGTH 9 DECIMALS 2,
         desv_brl       TYPE p LENGTH 9 DECIMALS 2,
         vlr_usd        TYPE p LENGTH 9 DECIMALS 2,
         desv_usd       TYPE p LENGTH 9 DECIMALS 2,
         taxa           TYPE p LENGTH 9 DECIMALS 2,
         equiv_ref_usd  TYPE p LENGTH 9 DECIMALS 2,
         equiv_ref_brl  TYPE p LENGTH 9 DECIMALS 2,
**  Begin of " CS2023000082   #103662  FF  28.02.2023
         belnr          TYPE belnr_d,
         doc_lcto_est   TYPE stblg,
         doc_lcto       TYPE zglt0107-doc_lcto,
         belnr_2        TYPE belnr_d,
         doc_lcto_est_2 TYPE stblg,
         doc_lcto_2     TYPE zglt0107-doc_lcto_2,
         id_ord         TYPE i,
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


DATA: git_saida      TYPE TABLE OF ty_saida WITH HEADER LINE.
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
      git_zglt0108       TYPE TABLE OF zglt0108  WITH HEADER LINE,
      git_zglt0108_ant   TYPE TABLE OF zglt0108  WITH HEADER LINE,
      git_dta            TYPE TABLE OF bdcdata,
      opt                TYPE ctu_params,
      git_msg            TYPE TABLE OF bdcmsgcoll,
      git_bapiret        LIKE STANDARD TABLE OF gwa_bapiret.


DATA: gwa_zglt0104     LIKE LINE OF git_zglt0104,
      gwa_zfit0160_atu LIKE LINE OF git_zfit0160_atu,
      gwa_tcurr        LIKE LINE OF git_tcurr,
      gwa_zfit0160_ant LIKE LINE OF git_zfit0160_ant,
      gwa_zglt0108     LIKE LINE OF git_zglt0108,
      gwa_zglt0108_ant LIKE LINE OF git_zglt0108,
      gwa_saida        LIKE LINE OF git_saida,
      w_zglt0112       TYPE zglt0112.


DATA: gva_variant  TYPE varid-variant,
      gva_data_ini TYPE sy-datum,
      gva_data_fim TYPE sy-datum,
      gva_belnr    TYPE zglt0108-belnr.

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
      p_doc_lcto              TYPE zglt0108-doc_lcto,
      p_objkey                TYPE zib_contabil-obj_key,
      p_doc_cont              TYPE zglt0108-belnr,
      p_estorno               TYPE zib_contabil-obj_key.

*&--------------------------------------------------------------------&*
*& Inicialization                                                     &*
*&--------------------------------------------------------------------&*
INITIALIZATION.

START-OF-SELECTION.

  CALL SCREEN 100.

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

          IF gw_saida-belnr IS NOT INITIAL AND gw_saida-belnr NE '0000000000'.
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

          IF gw_saida-belnr_2 IS NOT INITIAL AND gw_saida-belnr_2 NE '0000000000'.
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

* RJF - Ini
        WHEN 'DOC_LCTO'.

          IF NOT gw_saida-doc_lcto IS INITIAL." AND
            SET PARAMETER ID 'BLN' FIELD gw_saida-doc_lcto.
            SET PARAMETER ID 'LOT' FIELD  vg_lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
          ENDIF.
        WHEN 'DOC_LCTO_2'.

          IF NOT gw_saida-doc_lcto_2 IS INITIAL. " AND
            SET PARAMETER ID 'BLN' FIELD gw_saida-doc_lcto_2.
            SET PARAMETER ID 'LOT' FIELD vg_lote.
            CALL TRANSACTION 'ZGL016' AND SKIP FIRST SCREEN.
          ENDIF.
* RJF - Fim
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


    IF p_mes = 12.

      SELECT * FROM zglt0108
       INTO TABLE git_zglt0108
      WHERE monat       GE p_mes
        AND gjahr       EQ p_ano
        AND investidora EQ p_bukrs_invra
        AND investida   EQ p_bukrs_invda
        AND doc_lcto    NE ''
        AND objkey      NE '' .

    ELSE.
** End of FF  28.02.2023

      SELECT * FROM zglt0108
       INTO TABLE git_zglt0108
      WHERE monat       EQ p_mes
        AND gjahr       EQ p_ano
        AND investidora EQ p_bukrs_invra
        AND investida   EQ p_bukrs_invda
        AND doc_lcto    NE ''
        AND objkey      NE '' .
    ENDIF.

    CASE e_ucomm.
      WHEN c_aprov.

* RJF - Ini - 2023.03.21
        READ TABLE git_zglt0104 INTO gwa_zglt0104 INDEX 1.
        IF sy-subrc IS INITIAL.

          CASE gwa_zglt0104-moeda_funcional.
            WHEN 'OUTRAS' OR 'BRL'.
* RJF - Fim - 2023.03.21
              PERFORM f_gerar_documento.
              PERFORM f_atualiza_doc.
* RJF - Ini - 2023.03.21
            WHEN 'USD'.
              PERFORM f_gerar_documento_u.
              PERFORM f_atualiza_doc_u.
          ENDCASE.
        ENDIF.
* RJF - Fim - 2023.03.21

      WHEN c_reprov.

* RJF - Ini - 2023.03.21
        READ TABLE git_zglt0104 INTO gwa_zglt0104 INDEX 1.
        IF sy-subrc IS INITIAL.

          CASE gwa_zglt0104-moeda_funcional.
            WHEN 'OUTRAS' OR 'BRL'.
* RJF - Fim - 2023.03.21

              PERFORM f_estornar_documento.

* RJF - Ini - 2023.03.21
            WHEN 'USD'.
              PERFORM f_estornar_documento_u.
          ENDCASE.
        ENDIF.
* RJF - Fim - 2023.03.21

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

* RJF - Ini - 2023.03.21
        READ TABLE git_zglt0104 INTO gwa_zglt0104 INDEX 1.
        IF sy-subrc IS INITIAL.

          CASE gwa_zglt0104-moeda_funcional.
            WHEN 'OUTRAS' OR 'BRL'.
* RJF - Fim - 2023.03.21
              PERFORM f_atualiza_doc.

* RJF - Ini - 2023.03.21
            WHEN 'USD'.
              PERFORM f_atualiza_doc_u.
          ENDCASE.
        ENDIF.
* RJF - Fim - 2023.03.21

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


  READ TABLE git_zglt0104 INTO gwa_zglt0104 INDEX 1.

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
      PERFORM montar_layout_grid.
      PERFORM busca_dados.
    WHEN c_atuali.
      "PERFORM busca_dados.
      PERFORM limpa_campos.
      PERFORM busca_descricoes.
      PERFORM montar_layout_grid.
      PERFORM busca_dados.
    WHEN c_back.
      LEAVE PROGRAM.
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

  IF  gwa_zglt0104-moeda_funcional = 'USD' .

    LOOP AT SCREEN.
      IF screen-group1 EQ 'X'.
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.


    PERFORM montar_estrutura USING:
       1  ' '     'ICONS'          'GIT_SAIDA'   'ICONS'             'Status'                   '7'    ' ' ' ' 'X' ' ' 'C' ,
       2  ' '     'ID_MOV_PL'      'GIT_SAIDA'   'ID_MOV_PL'         'ID '                      '4'    ' ' ' ' ' ' 'X' ' ' ,
       3  ' '     'MOV_PL'         'GIT_SAIDA'   'MOV_PL'            'Movimentação do PL'       '40'   ' ' ' ' ' ' ' ' ' ' ,
       4  ' '     'DESV_BRL'       'GIT_SAIDA'   'DESV_BRL'          'Desvio BRL '              '20'   ' ' ' ' ' ' ' ' ' ' ,
       5  ' '     'DESV_USD'       'GIT_SAIDA'   'DESV_USD'          'Desvio USD'               '20'   ' ' ' ' ' ' ' ' ' ' ,
       6  ' '     'EQUIV_REF_BRL'  'GIT_SAIDA'   'EQUIV_REF_BRL'     'E. Reflexa BRL '          '20'   ' ' ' ' ' ' ' ' ' ' ,
       7  ' '     'EQUIV_REF_USD'  'GIT_SAIDA'   'EQUIV_REF_USD'     'E. Reflexa USD'           '20'   ' ' ' ' ' ' ' ' ' ' ,
**  Begin of " CS2023000082   #103662  FF  28.02.2023
       8  ' '     'BELNR'          'GIT_SAIDA'    'BELNR'          'Documento'                '10'   ' ' ' ' 'X' ' ' ' ' ,
       9  ' '     'DOC_LCTO_EST'   'GIT_SAIDA'    'DOC_LCTO_EST'      'Estorno'                  '10'   ' ' ' ' 'X' ' ' ' ' ,
      10  ' '     'DOC_LCTO'       'GIT_SAIDA'    'DOC_LCTO'        'Doc. Lanç.'                  '10'   ' ' ' ' 'X' ' ' ' ' , " RJF
      11  ' '     'BELNR_2'        'GIT_SAIDA'    'BELNR_2'        'Documento'                '10'   ' ' ' ' 'X' ' ' ' ' ,
      12  ' '     'DOC_LCTO_EST_2' 'GIT_SAIDA'    'DOC_LCTO_EST_2'    'Estorno'                  '10'   ' ' ' ' 'X' ' ' ' ' ,
      13  ' '     'DOC_LCTO_2'     'GIT_SAIDA'    'DOC_LCTO_2'        'Doc. Lanç.'                  '10'   ' ' ' ' 'X' ' ' ' ' . " RJF
** End of FF  28.02.2023

  ELSE.
    IF  gwa_zglt0104-moeda_funcional IS INITIAL OR gwa_zglt0104-moeda_funcional = 'BRL' .
      PERFORM montar_estrutura USING:
       1  ' '     'ICONS'          'GIT_SAIDA'   'ICONS'             'Status'                   '7'    ' ' ' ' 'X' ' ' 'C' ,
       2  ' '     'ID_MOV_PL'      'GIT_SAIDA'   'ID_MOV_PL'         'ID '                      '4'    ' ' ' ' ' ' 'X' ' ' ,
       3  ' '     'MOV_PL'         'GIT_SAIDA'   'MOV_PL'            'Movimentação do PL'       '40'   ' ' ' ' ' ' ' ' ' ' ,
       4  ' '     'MOEDA_PER'      'GIT_SAIDA'   'MOEDA_PER'         'Moeda Per '               '20'   ' ' ' ' ' ' ' ' ' ' ,
       5  ' '     'MOEDA_COMP'     'GIT_SAIDA'   'MOEDA_COMP'        'Moeda Comp'               '20'   ' ' ' ' ' ' ' ' ' ' ,
       6  ' '     'VLR_BRL'        'GIT_SAIDA'   'VLR_BRL'           'BRL'                      '20'   ' ' ' ' ' ' ' ' ' ' ,
       7  ' '     'VLR_USD'        'GIT_SAIDA'   'VLR_USD'           'USD'                      '20'   ' ' ' ' ' ' ' ' ' ' ,
       8  ' '     'EQUIV_REF_USD'  'GIT_SAIDA'   'EQUIV_REF_USD'     'Equivalência Reflexa'     '20'   ' ' ' ' ' ' ' ' ' ' .

    ELSE.
      IF   gwa_zglt0104-moeda_funcional = 'OUTRAS'.
        PERFORM montar_estrutura USING:
         1  ' '     'ICONS'          'GIT_SAIDA'   'ICONS'             'Status'                   '7'    ' ' ' ' 'X' ' ' 'C' ,
         2  ' '     'ID_MOV_PL'      'GIT_SAIDA'   'ID_MOV_PL'         'ID '                      '4'    ' ' ' ' ' ' 'X' ' ' ,
         3  ' '     'MOV_PL'         'GIT_SAIDA'   'MOV_PL'            'Movimentação do PL'       '40'   ' ' ' ' ' ' ' ' ' ' ,
         4  ' '     'MOEDA_PER'      'GIT_SAIDA'   'MOEDA_PER'         'Moeda Per '               '20'   ' ' ' ' ' ' ' ' ' ' ,
         5  ' '     'MOEDA_COMP'     'GIT_SAIDA'   'MOEDA_COMP'        'Moeda Comp'               '20'   ' ' ' ' ' ' ' ' ' ' ,
         6  ' '     'VLR_BRL'        'GIT_SAIDA'   'VLR_BRL'           'BRL'                      '20'   ' ' ' ' ' ' ' ' ' ' ,
         7  ' '     'VLR_USD'        'GIT_SAIDA'   'VLR_USD'           'USD'                      '20'   ' ' ' ' ' ' ' ' ' ' ,
         8  ' '     'EQUIV_REF_USD'  'GIT_SAIDA'   'EQUIV_REF_BRL'     'Equivalência Reflexa'     '20'   ' ' ' ' ' ' ' ' ' ' .
      ENDIF.
    ENDIF.
  ENDIF.
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

  DATA: lva_data_gdatu TYPE sy-datum,
        lva_data       TYPE c LENGTH 10,
        lva_gdatu      TYPE tcurr-gdatu.


  IF             p_bukrs_invra IS INITIAL.
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
  IF p_mes = 12.

    SELECT * FROM zglt0108
     INTO TABLE git_zglt0108
    WHERE monat       GE p_mes
      AND gjahr       EQ p_ano
      AND investidora EQ p_bukrs_invra
      AND investida   EQ p_bukrs_invda
      AND doc_lcto    NE ''
      AND objkey      NE '' .

  ELSE.
** End of FF  28.02.2023

    SELECT * FROM zglt0108
     INTO TABLE git_zglt0108
    WHERE monat       EQ p_mes
      AND gjahr       EQ p_ano
      AND investidora EQ p_bukrs_invra
      AND investida   EQ p_bukrs_invda
      AND doc_lcto    NE ''
      AND objkey      NE '' .
  ENDIF.

  IF sy-subrc IS INITIAL.

    READ TABLE git_zglt0108 INTO gwa_zglt0108 INDEX 1.

    MOVE: gwa_zglt0108-belnr        TO wg_doc_cont,
          gwa_zglt0108-belnr        TO p_doc_cont,
          gwa_zglt0108-doc_lcto_est TO wg_estorno.
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

*** Busca Taxa Venda Dolar 1º dia DO mês subsequente ao parâmetro
  "wg_taxa_v_dolar = ukurs

  CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
    EXPORTING
      date      = gva_data_ini "gva_data_fim
      days      = 00
      months    = 01
      years     = 00
    IMPORTING
      calc_date = lva_data_gdatu.

*  CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*    EXPORTING
*      day_in            = lva_data_gdatu
*    IMPORTING
*      last_day_of_month = lva_data_gdatu
*    EXCEPTIONS
*      day_in_no_date    = 1
*      OTHERS            = 2.


  CONCATENATE lva_data_gdatu+6(2) '.' lva_data_gdatu+4(2) '.' lva_data_gdatu(4) INTO lva_data.

  CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
    EXPORTING
      input  = lva_data
    IMPORTING
      output = lva_gdatu.

  SELECT *  FROM tcurr
    INTO TABLE git_tcurr
      WHERE gdatu EQ lva_gdatu
        AND kurst = 'B'
        AND fcurr = 'BRL'
        AND tcurr = 'USD'.

  IF git_tcurr[] IS INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Taxa Dólar não econtrada!'.
    LEAVE TO SCREEN 0100.
  ELSE.
    READ TABLE git_tcurr INTO gwa_tcurr INDEX 1.
    wg_taxa_v_dolar = abs( gwa_tcurr-ukurs ).
*    MOVE gwa_tcurr-ukurs TO  wg_taxa_v_dolar .
    CONDENSE wg_taxa_v_dolar NO-GAPS.
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

  SELECT * FROM zglt0108
   INTO TABLE git_zglt0108_ant
  WHERE monat       EQ p_mes_ant
    AND gjahr       EQ p_ano_ant
    AND investidora EQ p_bukrs_invra
    AND investida   EQ p_bukrs_invda.

  IF sy-subrc IS INITIAL.

    READ TABLE git_zglt0108_ant INTO gwa_zglt0108_ant WITH KEY item_balanco = '99'.
    IF gwa_zglt0104-moeda_funcional = 'BRL'.
      MOVE:   gwa_zglt0108_ant-equiv_ref_usd TO wg_eq_ref_ant,
              gwa_zglt0108_ant-equiv_ref_usd TO gva_eq_ref_ant.
    ELSE.
      IF  gwa_zglt0104-moeda_funcional = 'OUTRAS'.
        MOVE: gwa_zglt0108_ant-equiv_ref_brl TO wg_eq_ref_ant,
              gwa_zglt0108_ant-equiv_ref_brl TO gva_eq_ref_ant.
      ENDIF.
    ENDIF.
    CONDENSE wg_eq_ref_ant NO-GAPS.
  ELSE.
    CLEAR : gva_eq_ref_ant, wg_eq_ref_ant.
  ENDIF.


ENDFORM.                    " BUSCA_DESCRICOES
*&---------------------------------------------------------------------*
*&      Form  BUSCA_DADOS
*&---------------------------------------------------------------------*
FORM busca_dados .

  CLEAR: gwa_zglt0108.
  READ TABLE git_zglt0108 INTO gwa_zglt0108 INDEX 1.

  IF gwa_zglt0108-objkey IS NOT INITIAL ."IF gwa_zglt0108-doc_lcto_est IS INITIAL AND gwa_zglt0108-objkey IS NOT INITIAL .

    "Verifico se ainda existe documento na ZIB sem estar salvo na tabela.
    PERFORM f_atualiza_doc.

    LOOP AT git_zglt0108 INTO gwa_zglt0108.

      IF gwa_zglt0108-belnr IS NOT INITIAL.
        gwa_saida-icons = '@01@'.
      ELSE.
        gwa_saida-icons = '@02@'.
      ENDIF.

      CASE gwa_zglt0108-desc_cta_equ.
        WHEN 'ATIVO'.
          gwa_saida-id_ord = 1.
        WHEN 'PASSIVO'.
          gwa_saida-id_ord = 2.
        WHEN 'PATRIMÔNIO LÍQUIDO'.
          gwa_saida-id_ord = 3.
        WHEN 'RESULTADO DO PERIODO'.
          gwa_saida-id_ord = 4.
        WHEN 'PROVISOES P/IR E CSLL'.
          gwa_saida-id_ord = 5.
        WHEN 'PROVISOES P/PARTICIPAÇOES NOS RESULTADOS'.
          gwa_saida-id_ord = 6.
        WHEN 'CTA'.
          gwa_saida-id_ord = 7.
        WHEN OTHERS.
          gwa_saida-id_ord = 99.
      ENDCASE.

      gwa_saida-id_mov_pl        =    gwa_zglt0108-item_balanco  .
      gwa_saida-mov_pl           =    gwa_zglt0108-desc_cta_equ  .
      gwa_saida-moeda_per        =    gwa_zglt0108-moeda_per      .
      gwa_saida-moeda_comp       =    gwa_zglt0108-moeda_comp     .
      gwa_saida-vlr_brl          =    gwa_zglt0108-vlr_brl        .
      gwa_saida-desv_brl         =    gwa_zglt0108-desvio_brl     .
      gwa_saida-vlr_usd          =    gwa_zglt0108-vlr_usd        .
      gwa_saida-desv_usd         =    gwa_zglt0108-desvio_usd     .
      gwa_saida-taxa             =    gwa_zglt0108-taxa           .
      gwa_saida-equiv_ref_brl    =    gwa_zglt0108-equiv_ref_brl  .
      gwa_saida-equiv_ref_usd    =    gwa_zglt0108-equiv_ref_usd  .
**  Begin of " CS2023000082   #103662  FF  28.02.2023
      gwa_saida-belnr              =    gwa_zglt0108-belnr.
      gwa_saida-doc_lcto_est       =    gwa_zglt0108-doc_lcto_est.
      gwa_saida-belnr_2            =    gwa_zglt0108-belnr_2.
      gwa_saida-doc_lcto_est_2     =    gwa_zglt0108-doc_lcto_est_2.
** End of FF  28.02.2023

*  Begin of " CS2023000082   #117124  RJF  04.07.2023
      gwa_saida-doc_lcto           =    gwa_zglt0108-doc_lcto.
      gwa_saida-doc_lcto_2         =    gwa_zglt0108-doc_lcto_2.
*  End of " CS2023000082   #117124  RJF  04.07.2023

      APPEND gwa_saida TO git_saida.
      CLEAR: gwa_saida,
            gwa_zglt0108.
    ENDLOOP.

*    SORT  git_saida BY id_mov_pl . "RJF 2023.05.17
    SORT  git_saida BY id_ord. "RJF 2023.05.17

    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = git_fieldcatalog.

    CALL METHOD grid1->refresh_table_display( ).

  ELSE.

    CLEAR: git_zglt0108[], gwa_zglt0108.

    PERFORM f_prepare_run_time_info.
    PERFORM f_get_range.

    SUBMIT zrfbila00 USING SELECTION-SET gva_variant WITH SELECTION-TABLE  git_selection  WITH bilatree EQ ''
                                                                                          WITH bilagrid EQ 'X' AND RETURN.
    PERFORM f_get_runtime_info.
    PERFORM organiza_dados TABLES git_zgl033
                                  git_zfit0160_atu
                                  git_zfit0160_ant   USING gwa_zglt0104 .


    CALL METHOD grid1->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = git_fieldcatalog.

    CALL METHOD grid1->refresh_table_display( ).


    CALL SCREEN 0100.

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
        lwa_saida      LIKE LINE OF git_saida,
        lwa_saida_aux  LIKE LINE OF git_saida,
        lwa_saida_pass LIKE LINE OF git_saida.


  DATA lit_obj_range TYPE RANGE OF e071-obj_name.
  DATA lwa_obj_range LIKE LINE OF lit_obj_range.

  DATA: lva_index TYPE sy-tabix.

  DATA:
    lva_ativo_usd   TYPE  ty_saida-vlr_usd,
    lva_passivo_usd TYPE  ty_saida-vlr_usd,
    lva_patrimo_usd TYPE  ty_saida-vlr_usd,
    lva_result_usd  TYPE  ty_saida-vlr_usd,
    lva_ativo_brl   TYPE  ty_saida-vlr_usd,
    lva_passivo_brl TYPE  ty_saida-vlr_usd,
    lva_patrimo_brl TYPE  ty_saida-vlr_usd,
    lva_result_brl  TYPE  ty_saida-vlr_usd.



  IF  p_zglt0104-moeda_funcional = 'USD'.
    READ TABLE git_zfit0160_atu INTO gwa_zfit0160_atu INDEX 1.
  ENDIF.

  CLEAR: git_zgl033_aux[],
         git_zgl033_pas[],
         git_zgl033_pas_aux[],
         git_zgl033_aval[].

  MOVE-CORRESPONDING p_git_zgl033[] TO git_zgl033_aux[].
  MOVE-CORRESPONDING p_git_zgl033[] TO git_zgl033_pas[].
  MOVE-CORRESPONDING p_git_zgl033[] TO git_zgl033_pas_aux[].
  MOVE-CORRESPONDING p_git_zgl033[] TO git_zgl033_aval[].

  IF  p_zglt0104-moeda_funcional = 'OUTRAS' OR p_zglt0104-moeda_funcional = 'BRL'.

* BUSCAR AS CONTAS PRINCIPAIS NIVEL 2
* Aqui eu pego só o nível 2  e os totais das contas.
    DELETE git_zgl033_aux WHERE tlevel <> 2.
    DELETE p_git_zgl033   WHERE tlevel <> 2.

    SORT git_zgl033_aux  BY ergsl.
    DELETE ADJACENT DUPLICATES FROM git_zgl033_aux COMPARING npage.

    LOOP AT git_zgl033_aux INTO lwa_zgl033 .

      LOOP AT p_git_zgl033 INTO lwa_zgl033_aux WHERE npage =  lwa_zgl033-npage AND id  <> lwa_zgl033-id.

        lwa_saida-icons = '@02@'.
*        lwa_saida-id_mov_pl     = lwa_zgl033_aux-npage.
        lwa_saida-id_mov_pl     = lwa_zgl033_aux-id.
        lwa_saida-mov_pl        = lwa_zgl033-text.
        lwa_saida-moeda_per     = lwa_zgl033_aux-waers.
        lwa_saida-moeda_comp    = lwa_zgl033_aux-waer2.

        lwa_saida-vlr_brl       = 0.
        lwa_saida-desv_brl      = 0.
        lwa_saida-vlr_usd       = 0.
        lwa_saida-desv_usd      = 0.
        lwa_saida-equiv_ref_usd = 0.
        lwa_saida-equiv_ref_brl = 0.

**-------- Se moeda BRL --------*
        IF p_zglt0104-moeda_funcional = 'BRL'.
*          lwa_saida-vlr_brl =  abs( lwa_zgl033_aux-repval ).
          lwa_saida-vlr_brl =  lwa_zgl033_aux-repval.
        ELSE.
*-------- Se moeda OUTRAS --------*
          IF  p_zglt0104-moeda_funcional = 'OUTRAS'.
*            lwa_saida-vlr_usd   = abs( lwa_zgl033_aux-repval2 ).
            lwa_saida-vlr_usd   = lwa_zgl033_aux-repval2.
            " lwa_saida-vlr_brl =  abs( lwa_zgl033_aux-repval ).
          ENDIF.
        ENDIF.
        APPEND lwa_saida TO git_saida.

        CLEAR:  lwa_saida,
                lwa_zgl033_aux,
                lwa_zgl033.
      ENDLOOP.
    ENDLOOP.

*** MANTER SO AS CONTAS QUE FORAM SOLICIDADAS.
    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'ATIVO' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'PASSIVO' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'RESULTADO DO PERIODO' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'PROVISOES P/IR E CSLL' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'PROVISOES P/PARTICIPAÇOES NOS RESULTADOS' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    DELETE git_saida WHERE mov_pl NOT IN lit_obj_range.

*** BUSCAR CONTA NIVEL 4 SOMENTE MOEDA BRL E OUTRAS
* Aqui eu preciso pegar uma conta especifica ( Patrimônio Liquido )
    DELETE git_zgl033_pas     WHERE tlevel <> 4.
    DELETE git_zgl033_pas_aux WHERE tlevel <> 4.

    CLEAR: lit_obj_range[],lwa_obj_range.
    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'PATRIMÔNIO LÍQUIDO' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    DELETE git_zgl033_pas     WHERE text NOT IN lit_obj_range.

****     ZGL-CS2022000307-EquiPatr - Cad Val Pat Líquido - BUG 99244 BG - INICIO
    DATA:
          v_data TYPE sy-datum.
    "V_DATA = '01.'
    CONCATENATE p_ano p_mes '01' INTO v_data.

    SELECT SINGLE * INTO @DATA(wa_zglt0111) FROM zglt0111
      WHERE investida = @p_bukrs_invda AND
      investidora = @p_bukrs_invra  AND
      per_inicial <= @v_data AND per_final >= @v_data.


    "SELECT * FROM

    CLEAR: lwa_zgl033 , lwa_zgl033_aux.
    LOOP AT git_zgl033_pas INTO lwa_zgl033 .

      LOOP AT git_zgl033_pas_aux INTO lwa_zgl033_aux WHERE npage =  lwa_zgl033-npage AND ergsl = lwa_zgl033-ergsl AND id  <> lwa_zgl033-id.

        lwa_saida_pass-icons = '@02@'.
*        lwa_saida_pass-id_mov_pl     = lwa_zgl033_aux-npage.
        lwa_saida_pass-id_mov_pl     = lwa_zgl033_aux-id.
        lwa_saida_pass-mov_pl        = lwa_zgl033-text.


        lwa_saida_pass-moeda_per    = lwa_zgl033_aux-waers.
        lwa_saida_pass-moeda_comp   = lwa_zgl033_aux-waer2.

        lwa_saida_pass-vlr_brl       = 0.
        lwa_saida_pass-desv_brl      = 0.
        lwa_saida_pass-vlr_usd       = 0.
        lwa_saida_pass-desv_usd      = 0.
        lwa_saida_pass-taxa          = 0.
        lwa_saida_pass-equiv_ref_usd = 0.
        lwa_saida_pass-equiv_ref_brl = 0.

        IF  lwa_zgl033-text = 'PATRIMÔNIO LÍQUIDO'.

          IF wa_zglt0111-moeda EQ 'BRL'.
            lwa_saida_pass-vlr_usd =  wa_zglt0111-vl_pat_liq.
          ELSE.
            lwa_saida_pass-vlr_usd =  lwa_zgl033_aux-repval2.
          ENDIF.

          IF  wa_zglt0111-moeda EQ 'OUTRAS'.
*            lwa_saida_pass-vlr_brl   = wa_zglt0111-vl_pat_liq.
            lwa_saida_pass-vlr_brl   = lwa_zgl033_aux-repval.
          ELSE.
            lwa_saida_pass-vlr_brl   = lwa_zgl033_aux-repval.
          ENDIF.


        ELSE.
          lwa_saida_pass-vlr_brl   = abs( lwa_zgl033_aux-repval ).

          lwa_saida_pass-vlr_usd =  abs( lwa_zgl033_aux-repval2 ).

        ENDIF.

        lwa_saida_pass-desv_brl  = abs( lwa_zgl033_aux-absvar ).

        lwa_saida_pass-desv_usd = abs( lwa_zgl033_aux-absvar2 ).

        APPEND lwa_saida_pass TO git_saida_pass.

        CLEAR:  lwa_saida_pass,
                lwa_zgl033_aux,
                lwa_zgl033.
      ENDLOOP.
    ENDLOOP.

    DELETE git_saida_pass WHERE mov_pl NOT IN lit_obj_range.
    SORT git_saida_pass BY id_mov_pl.

**** AGORA QUE TEM A SAIDA PRECISA APLICAR A SEGUINTE REGRA:
* ATIVO
* PASSIVO = PASSIVO - PATRIMONIO LIQUIDO
* PATRIMONIO LIQUIDO
* RESULTADO DO PERIDO = RESULTADO DO PERIODO + PROVISÕES I RENDA + PROVISÕES PARA PARTICIPAÇÃO

    CLEAR: lit_obj_range[],lwa_obj_range.
    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'ATIVO' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'PASSIVO' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'PATRIMÔNIO LÍQUIDO' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'RESULTADO DO PERIODO' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

**  Begin of CS2023000082  #103662 FF   22.02.2023
    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'PROVISOES P/IR E CSLL' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.

    lwa_obj_range-sign = 'I'.
    lwa_obj_range-option = 'CP'.
    CONCATENATE 'PROVISOES P/PARTICIPAÇOES NOS RESULTADOS' '*' INTO lwa_obj_range-low.
    APPEND lwa_obj_range TO lit_obj_range.
**  End of FF  22.02.2023

    CLEAR: git_saida_aux[].
    MOVE-CORRESPONDING git_saida[] TO git_saida_aux[].

    DELETE git_saida  WHERE mov_pl NOT IN lit_obj_range.

    IF  p_zglt0104-moeda_funcional = 'BRL'.

      LOOP AT git_saida INTO lwa_saida.
        MOVE sy-tabix TO lva_index.

        IF lwa_saida-mov_pl = 'ATIVO'.
*          lwa_saida-vlr_usd = abs( lwa_saida-vlr_brl ) / gwa_tcurr-ukurs.
          lwa_saida-vlr_usd = lwa_saida-vlr_brl / wg_taxa_v_dolar.
          lva_ativo_usd = lwa_saida-vlr_usd.
          MODIFY git_saida FROM lwa_saida INDEX lva_index TRANSPORTING vlr_brl vlr_usd.
        ELSE.
          IF lwa_saida-mov_pl = 'PASSIVO'.

            LOOP AT git_saida_pass INTO lwa_saida_pass.
              IF lwa_saida_pass-mov_pl = 'PATRIMÔNIO LÍQUIDO'.
*                lwa_saida-vlr_brl = abs( lwa_saida-vlr_brl ) - ( abs( lwa_saida_pass-vlr_brl ) ).
*                lva_patrimo_usd = abs( lwa_saida_pass-vlr_usd ) .

                lwa_saida-vlr_brl = lwa_saida-vlr_brl - lwa_saida_pass-vlr_brl.
                lva_patrimo_usd = lwa_saida_pass-vlr_usd.

              ENDIF.
            ENDLOOP.

            lwa_saida-vlr_usd = lwa_saida-vlr_brl / wg_taxa_v_dolar.

            MODIFY git_saida FROM lwa_saida INDEX lva_index TRANSPORTING vlr_brl vlr_usd taxa.
            APPEND lwa_saida_pass TO git_saida. " Append no patrimonio liquido

            lva_passivo_usd = lwa_saida-vlr_usd.


          ELSE.

**  Begin of CS2023000082  #103662 FF   22.02.2023
            IF lwa_saida-mov_pl <> 'PATRIMÔNIO LÍQUIDO'.
**            IF lwa_saida-mov_pl = 'RESULTADO DO PERIODO'.
**              LOOP AT git_saida_aux INTO lwa_saida_aux.
**                IF lwa_saida_aux-mov_pl = 'PROVISOES P/IR E CSLL'.
***                  lwa_saida-vlr_brl = abs( lwa_saida-vlr_brl ) - ( abs( lwa_saida_aux-vlr_brl ) ).
***                  lwa_saida-vlr_brl = lwa_saida-vlr_brl - lwa_saida_aux-vlr_brl.
**                  ADD lwa_saida_aux-vlr_brl TO lwa_saida-vlr_brl.
**                ELSE.
**                  IF lwa_saida_aux-mov_pl = 'PROVISOES P/PARTICIPAÇOES NOS RESULTADOS'.
***                    lwa_saida-vlr_brl = abs( lwa_saida-vlr_brl ) - ( abs( lwa_saida_aux-vlr_brl ) ).
***                    lwa_saida-vlr_usd = abs( lwa_saida-vlr_usd ) - abs(  lwa_saida_aux-vlr_usd ).
**                    ADD lwa_saida_aux-vlr_brl TO lwa_saida_aux-vlr_brl.
**                    ADD lwa_saida_aux-vlr_usd TO lwa_saida_aux-vlr_usd.
***                    lwa_saida-vlr_brl = lwa_saida-vlr_brl - lwa_saida_aux-vlr_brl.
***                    lwa_saida-vlr_usd = lwa_saida-vlr_usd - lwa_saida_aux-vlr_usd.
**                  ENDIF.
**                ENDIF.
**              ENDLOOP.

**  End of FF  22.02.2023

              lwa_saida-vlr_usd = ( lwa_saida-vlr_brl / gwa_zfit0160_atu-tx_med_medv  ).

              MODIFY git_saida FROM lwa_saida INDEX lva_index TRANSPORTING vlr_brl vlr_usd taxa.

              lva_result_usd =  lwa_saida-vlr_usd.

            ENDIF.

          ENDIF.
        ENDIF.
        CLEAR: lwa_saida.
      ENDLOOP.

* RESULTADO CTA.
      CLEAR lwa_saida.
      LOOP AT git_saida INTO DATA(wa_saida).
        ADD wa_saida-vlr_usd TO lwa_saida-vlr_usd.
      ENDLOOP.

      lwa_saida-icons         =  '@02@'.
      lwa_saida-id_mov_pl     = '99'.
      lwa_saida-mov_pl(65)    = 'CTA'.
      lwa_saida-equiv_ref_usd =  ( ( lwa_saida-vlr_usd - gva_eq_ref_ant ) *  ( p_zglt0104-part_perc ) ) / 100.
      APPEND  lwa_saida TO git_saida.

    ELSE.
      IF  p_zglt0104-moeda_funcional = 'OUTRAS'.
        LOOP AT git_saida INTO lwa_saida.
          MOVE sy-tabix TO lva_index.

          IF lwa_saida-mov_pl = 'ATIVO'.
*            lwa_saida-vlr_brl = lwa_saida-vlr_usd * gwa_tcurr-ukurs.
            lwa_saida-vlr_brl = lwa_saida-vlr_usd * wg_taxa_v_dolar.

            lva_ativo_brl = lwa_saida-vlr_brl.
            MODIFY git_saida FROM lwa_saida INDEX lva_index TRANSPORTING vlr_brl.
          ELSE.
            IF lwa_saida-mov_pl = 'PASSIVO'.

              LOOP AT git_saida_pass INTO lwa_saida_pass.
                IF lwa_saida_pass-mov_pl = 'PATRIMÔNIO LÍQUIDO'.
*                  lwa_saida-vlr_usd = abs( lwa_saida-vlr_usd ) - ( abs( lwa_saida_pass-vlr_usd ) ).
*                  lva_patrimo_brl = abs( lwa_saida_pass-vlr_brl ).
                  lwa_saida-vlr_usd = lwa_saida-vlr_usd - ( lwa_saida_pass-vlr_usd ). "1818
                  lva_patrimo_brl = lwa_saida_pass-vlr_brl.

                ENDIF.
              ENDLOOP.

              lwa_saida-vlr_brl = lwa_saida-vlr_usd * wg_taxa_v_dolar.

              MODIFY git_saida FROM lwa_saida INDEX lva_index TRANSPORTING vlr_brl vlr_usd.
              APPEND lwa_saida_pass TO git_saida. " Append no patrimonio liquido

              lva_passivo_brl = lwa_saida-vlr_brl.

            ELSE.

**  Begin of CS2023000082  #103662 FF   22.02.2023
              IF lwa_saida_pass-mov_pl <> 'PATRIMÔNIO LÍQUIDO'.
**              IF lwa_saida-mov_pl = 'RESULTADO DO PERIODO'.
**                LOOP AT git_saida_aux INTO lwa_saida_aux.
**                  IF lwa_saida_aux-mov_pl = 'PROVISOES P/IR E CSLL'.
***                    lwa_saida-vlr_usd = abs( lwa_saida-vlr_usd ) - ( abs( lwa_saida_aux-vlr_usd ) ).
***                    lwa_saida-vlr_usd = lwa_saida-vlr_usd - lwa_saida_aux-vlr_usd.
**                    ADD lwa_saida_aux-vlr_usd TO lwa_saida-vlr_usd."1803
**
**                  ELSE.
**                    IF lwa_saida_aux-mov_pl = 'PROVISOES P/PARTICIPAÇOES NOS RESULTADOS'.
***                      lwa_saida-vlr_usd = abs( lwa_saida-vlr_usd ) + lwa_saida_aux-vlr_usd.
**                      ADD lwa_saida_aux-vlr_usd TO lwa_saida-vlr_usd.
**                    ENDIF.
**                  ENDIF.
**                ENDLOOP.

**  End of FF  22.02.2023







                lwa_saida-vlr_brl = ( lwa_saida-vlr_usd * gwa_zfit0160_atu-tx_med_medv  ).

                MODIFY git_saida FROM lwa_saida INDEX lva_index TRANSPORTING vlr_brl vlr_usd.

                lva_result_brl =  lwa_saida-vlr_brl.

              ENDIF.

            ENDIF.
          ENDIF.
          CLEAR: lwa_saida.
        ENDLOOP.
* RESULTADO CTA.

        CLEAR: lwa_saida, wa_saida.
        LOOP AT git_saida INTO wa_saida.
          ADD wa_saida-vlr_usd TO lwa_saida-vlr_usd.
          ADD wa_saida-vlr_brl TO lwa_saida-vlr_brl.

        ENDLOOP.

        lwa_saida-icons         =  '@02@'.
        lwa_saida-id_mov_pl     = '99'.
        lwa_saida-mov_pl(65)    = 'CTA'.
        lwa_saida-equiv_ref_brl =  ( lwa_saida-vlr_brl - gva_eq_ref_ant ) *  ( p_zglt0104-part_perc ) / 100.
        APPEND  lwa_saida TO git_saida.
*
*        lwa_saida-icons         =  '@02@'.
*        lwa_saida-id_mov_pl     = '99'.
*        lwa_saida-mov_pl(65)    = 'CTA'.
*        lwa_saida-vlr_brl       =   lva_ativo_brl - ( lva_passivo_brl + lva_patrimo_brl + lva_result_brl ).
*        lwa_saida-equiv_ref_brl =  ( lwa_saida-vlr_brl - gva_eq_ref_ant ) *  ( p_zglt0104-participacao_perc ) / 100.
*        APPEND  lwa_saida TO git_saida.

      ENDIF.
    ENDIF.

  ELSE.

    CLEAR: lit_obj_range[],lwa_obj_range.

**  Begin of CS2023000082  #103662 FF   23.02.2023





    SELECT * FROM zglt0112 INTO TABLE @DATA(lt_zglt0112)
      WHERE investidora = @p_bukrs_invra
        AND investida   = @p_bukrs_invda.

    IF sy-subrc = 0.


      DATA: lr_matnr TYPE RANGE OF matnr.


      lit_obj_range = VALUE #( FOR ls_value IN lt_zglt0112
                                      ( sign   = 'I'
                                        option = 'CP'
                                        low    = |*{ ls_value-hkont_nome }|
                                      )
                              ).

      DELETE git_zgl033 WHERE text NOT IN lit_obj_range.

    ELSE.
**  End of FF  23.02.2023

      lit_obj_range =
      VALUE #(
               ( sign = 'I' option = 'CP' low = 'AVALIAÇÃO PATRIMONIAL' )
               ( sign = 'I' option = 'CP' low = 'LUCROS (PREJUÍZOS) ACUMULADOS' )
             ).

      DELETE git_zgl033_aval[] WHERE tlevel <> 5.
      DELETE p_git_zgl033 WHERE tlevel <> 5.

    ENDIF.


    DELETE git_zgl033_aval WHERE text NOT IN lit_obj_range.




*    LOOP AT git_zgl033_aval INTO lwa_zgl033 .
    LOOP AT p_git_zgl033 INTO lwa_zgl033_aux. "WHERE npage =  lwa_zgl033-npage  AND ergsl = lwa_zgl033-ergsl AND text = lwa_zgl033-text AND id  <> lwa_zgl033-id.




      lwa_saida-icons         = '@02@'.
      lwa_saida-id_mov_pl     = lwa_zgl033_aux-id.
      lwa_saida-mov_pl        = lwa_zgl033_aux-text. "RJF
      lwa_saida-moeda_per     = lwa_zgl033_aux-waers.
      lwa_saida-moeda_comp    = lwa_zgl033_aux-waer2.


      lwa_saida-vlr_brl       = 0.
      lwa_saida-desv_brl      = 0.
      lwa_saida-vlr_usd       = 0.
      lwa_saida-desv_usd      = 0.
      lwa_saida-equiv_ref_usd = 0.
      lwa_saida-equiv_ref_brl = 0.


      CASE lwa_saida-mov_pl.
        WHEN 'AVALIAÇÃO PATRIMONIAL'.


          lwa_saida-desv_brl      = lwa_zgl033_aux-absvar.
          lwa_saida-desv_usd      = lwa_zgl033_aux-absvar2.
          lwa_saida-equiv_ref_usd =   ( lwa_saida-desv_usd * ( p_zglt0104-part_perc ) ) / 100.
          lwa_saida-equiv_ref_brl =   ( lwa_saida-desv_brl * ( p_zglt0104-part_perc ) ) / 100.

        WHEN 'LUCROS (PREJUÍZOS) ACUMULADOS'.




          lwa_saida-desv_brl      = lwa_zgl033_aux-absvar.
          lwa_saida-desv_usd      = lwa_zgl033_aux-absvar2.
          lwa_saida-equiv_ref_usd =   ( lwa_saida-desv_usd * ( p_zglt0104-part_perc ) ) / 100.
          lwa_saida-equiv_ref_brl =   ( lwa_saida-desv_brl * ( p_zglt0104-part_perc ) ) / 100.

**  Begin of CS2023000082  #103662 FF   23.02.2023
        WHEN OTHERS.
          lwa_saida-desv_brl      = lwa_zgl033_aux-absvar.
          lwa_saida-desv_usd      = lwa_zgl033_aux-absvar2.
          lwa_saida-equiv_ref_usd =   ( lwa_saida-desv_usd * ( p_zglt0104-part_perc ) ) / 100.
          lwa_saida-equiv_ref_brl =   ( lwa_saida-desv_brl * ( p_zglt0104-part_perc ) ) / 100.
**  End of FF  23.02.2023

      ENDCASE.

      APPEND lwa_saida TO git_saida.

      CLEAR lwa_saida.


    ENDLOOP.

    CLEAR:  lwa_saida,
            lwa_zgl033.

*    ENDLOOP.

  ENDIF.



  LOOP AT git_saida INTO lwa_saida.
    MOVE sy-tabix TO lva_index.

*lwa_saida-mov_pl
    CASE lwa_saida-mov_pl.
      WHEN 'ATIVO'.
        lwa_saida-id_ord = 1.
      WHEN 'PASSIVO'.
        lwa_saida-id_ord = 2.
      WHEN 'PATRIMÔNIO LÍQUIDO'.
        lwa_saida-id_ord = 3.
      WHEN 'RESULTADO DO PERIODO'.
        lwa_saida-id_ord = 4.
      WHEN 'PROVISOES P/IR E CSLL'.
        lwa_saida-id_ord = 5.
      WHEN 'PROVISOES P/PARTICIPAÇOES NOS RESULTADOS'.
        lwa_saida-id_ord = 6.
      WHEN 'CTA'.
        lwa_saida-id_ord = 7.
      WHEN OTHERS.
        lwa_saida-id_ord = 99.
    ENDCASE.

    MODIFY git_saida FROM lwa_saida INDEX lva_index TRANSPORTING id_ord.
  ENDLOOP.

*  SORT  git_saida BY id_mov_pl . " RJF 2023.05.17
  SORT  git_saida BY id_ord. " RJF 2023.05.17

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
         git_tcurr[],
         git_zfit0160_atu[],
         git_zfit0160_ant[],
         git_selection[],
         git_zgl033[],
         git_zgl033[],
         git_zgl033_aux[],
         git_zgl033_pas[],
         git_zgl033_pas_aux[],
         git_saida[],
         git_saida_pass[],
         git_zgl033_aval[],
         git_zglt0108[],
         git_zglt0108_ant[],
         git_dta[],
         git_msg[],
         git_bapiret[].

  CLEAR:  gwa_zglt0104,
          gwa_zfit0160_atu,
          gwa_tcurr,
          gwa_zfit0160_ant,
          gwa_zglt0108,
          gwa_zglt0108_ant,
          gwa_saida.

  CLEAR: gva_variant.

  CLEAR:  opt.

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
        lva_equiv_brl TYPE ty_saida-vlr_brl,
        lva_equiv_usd TYPE ty_saida-vlr_usd,
        lva_objkey    TYPE zib_contabil_chv-obj_key.

  DATA: lit_zglt036  TYPE TABLE OF zglt036,
        lit_zglt032  TYPE TABLE OF zglt032,
        lit_zglt0108 TYPE TABLE OF zglt0108.

  DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
        wa_zglt036_flg TYPE zde_zglt036_flg.

  DATA: lwa_saida    TYPE ty_saida,
        lwa_zglt035  TYPE zglt035,
        lwa_zglt0105 TYPE zglt0105,
        lwa_zglt036  TYPE zglt036,
        chave_saldo  TYPE c,
        lwa_zglt032  TYPE zglt032,
        lwa_zglt0108 TYPE zglt0108.


  READ TABLE git_zglt0108 INTO gwa_zglt0108 INDEX 1.
  CLEAR: gwa_zglt0104.
  READ TABLE git_zglt0104 INTO gwa_zglt0104 INDEX 1.

  IF ( gwa_zglt0108-belnr IS INITIAL AND gwa_zglt0108-objkey IS INITIAL ) OR
      ( gwa_zglt0108-objkey IS NOT INITIAL AND gwa_zglt0108-doc_lcto_est IS NOT INITIAL ) .

    READ TABLE git_saida INTO DATA(w_saida) WITH KEY mov_pl = 'CTA'.

    IF w_saida-equiv_ref_usd < 0.
      chave_saldo = '2'.  "Negativo
    ELSE.
      chave_saldo = '1'.  "Positivo
    ENDIF.

    CASE gwa_zglt0104-moeda_funcional.
      WHEN 'OUTRAS' OR 'BRL'.

        SELECT SINGLE *
          FROM zglt0105 INTO lwa_zglt0105
        WHERE investidora EQ p_bukrs_invra
          AND investida EQ p_bukrs_invda
          AND moeda_funcional EQ gwa_zglt0104-moeda_funcional
          AND tp_lancamento EQ 2
          AND saldo EQ chave_saldo.

      WHEN 'USD'.

**  Begin of " CS2023000082   #103662  FF  28.02.2023
*        SELECT SINGLE *
*          FROM zglt0105 INTO lwa_zglt0105
*      WHERE investidora EQ p_bukrs_invra
*          AND investida EQ p_bukrs_invda
*          AND moeda_funcional EQ gwa_zglt0104-moeda_funcional
*          AND tp_lancamento EQ 2.

        SELECT SINGLE *
        FROM zglt0105 INTO lwa_zglt0105
          WHERE investidora EQ p_bukrs_invra
            AND investida EQ p_bukrs_invda
            AND moeda_funcional EQ gwa_zglt0104-moeda_funcional
            AND tp_lancamento EQ 3. "Reflexa do PL

        SELECT * FROM zglt0112 INTO TABLE @DATA(lt_zglt0112) "Empresas Exceção Equivalencia Patrimonial
        WHERE investidora = @p_bukrs_invra
          AND investida   = @p_bukrs_invda.
        IF sy-subrc <> 0.
          CLEAR lt_zglt0112[].
        ENDIF.
** End of FF  28.02.2023

    ENDCASE.


    IF lwa_zglt0105 IS NOT INITIAL.

      SELECT SINGLE blart bktxt prov_est FROM zglt031 INTO ( lva_blart, lva_bktxt, lva_prov_est ) WHERE tp_lcto = lwa_zglt0105-modelo_zgl.
      lva_dp_resp = '83'. "Contabilidade

* Criar lote na ZGLT034
      CALL METHOD zcl_gerar_lote=>create_lote
        EXPORTING
          i_bukrs       = p_bukrs_invra
          i_descr_lote  = 'Equivalencia Reflexa'
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


      IF gwa_zglt0104-moeda_funcional = 'OUTRAS' OR   gwa_zglt0104-moeda_funcional = 'BRL'.
        MOVE 'X'                      TO lwa_zglt035-st_lc_moeda.
      ELSE.
        MOVE ''                      TO lwa_zglt035-st_lc_moeda.
      ENDIF.

      CONCATENATE p_bukrs_invra '-' p_bukrs_invda INTO lwa_zglt035-xblnr.

* Criar item na ZGLT036

      SELECT *
        FROM zglt032 INTO TABLE lit_zglt032
      WHERE tp_lcto EQ  lwa_zglt035-tp_lcto.

      IF lit_zglt032 IS NOT INITIAL.

        CLEAR: git_zglt0108[].
        LOOP AT git_saida INTO gwa_saida.

          MOVE:
                p_mes                     TO gwa_zglt0108-monat,
                p_ano                     TO gwa_zglt0108-gjahr,
                p_bukrs_invra             TO gwa_zglt0108-investidora,
                p_bukrs_invda             TO gwa_zglt0108-investida,
                lwa_zglt035-lote          TO gwa_zglt0108-lote,
                lwa_zglt035-doc_lcto      TO gwa_zglt0108-doc_lcto,
                ''                        TO gwa_zglt0108-doc_lcto_est,
                sy-uname                  TO gwa_zglt0108-usnam,
                sy-datum                  TO gwa_zglt0108-dt_atual,
                sy-uzeit                  TO gwa_zglt0108-hr_atual.


          gwa_zglt0108-item_balanco  = gwa_saida-id_mov_pl    .
          gwa_zglt0108-desc_cta_equ  = gwa_saida-mov_pl       .
          gwa_zglt0108-moeda_per     = gwa_saida-moeda_per       .
          gwa_zglt0108-moeda_comp    = gwa_saida-moeda_comp      .
          gwa_zglt0108-vlr_brl       = gwa_saida-vlr_brl         .
          gwa_zglt0108-desvio_brl    = gwa_saida-desv_brl        .
          gwa_zglt0108-vlr_usd       = gwa_saida-vlr_usd         .
          gwa_zglt0108-desvio_usd    = gwa_saida-desv_usd        .
          gwa_zglt0108-taxa          = gwa_saida-taxa            .
          gwa_zglt0108-equiv_ref_brl = gwa_saida-equiv_ref_brl   .
          gwa_zglt0108-equiv_ref_usd = gwa_saida-equiv_ref_usd   .

          CONDENSE gwa_zglt0108-item_balanco NO-GAPS.


          IF gwa_saida-id_mov_pl = '99'.
            IF gwa_zglt0104-moeda_funcional = 'OUTRAS'.
              lva_equiv_brl = gwa_saida-equiv_ref_brl.
              lva_equiv_usd = gwa_saida-equiv_ref_brl.
            ELSE.
              IF gwa_zglt0104-moeda_funcional = 'BRL'.
                lva_equiv_brl = gwa_saida-equiv_ref_usd.
                lva_equiv_usd = gwa_saida-equiv_ref_usd.
              ENDIF.
            ENDIF.
          ELSE.
            IF gwa_zglt0104-moeda_funcional = 'USD' .
              lva_equiv_brl = gwa_saida-equiv_ref_brl.
              lva_equiv_usd = gwa_saida-equiv_ref_usd.
            ENDIF.
          ENDIF.

          APPEND gwa_zglt0108 TO git_zglt0108.
          CLEAR: gwa_saida,
                 gwa_zglt0108.

        ENDLOOP.

        LOOP AT lit_zglt032 INTO lwa_zglt032.

          lwa_zglt036-tp_lcto = lwa_zglt035-tp_lcto.

          MOVE: sy-tabix                    TO lwa_zglt036-seqitem,
                lwa_zglt032-bschl           TO lwa_zglt036-bschl,
                lwa_zglt032-hkont           TO lwa_zglt036-hkont,
                lwa_zglt032-sgtxt           TO lwa_zglt036-sgtxt,
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

          IF gwa_zglt0104-moeda_funcional = 'OUTRAS'.
            MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc,
                  abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int.

          ELSE.
            IF gwa_zglt0104-moeda_funcional = 'BRL'.
              MOVE: abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.

            ELSE.
              MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc,
                    abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int,
                    abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.
            ENDIF.
          ENDIF.

          APPEND lwa_zglt036 TO lit_zglt036.
          CLEAR: lwa_zglt036, lwa_zglt032.

        ENDLOOP.
      ELSE.
        MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Tipo de lançamento não econtrados ZGLT032!'.
        EXIT.
      ENDIF.


      CASE gwa_zglt0104-moeda_funcional.
        WHEN 'BRL'.

          CLEAR lwa_zglt036.
          LOOP AT lit_zglt036 INTO lwa_zglt036.

            wa_zglt036_flg-doc_lcto        = lwa_zglt036-doc_lcto.
            wa_zglt036_flg-seqitem         = lwa_zglt036-seqitem.
            wa_zglt036_flg-seqsub          = lwa_zglt036-seqsub.
            " wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
            " wa_zglt036_flg-fl_cv_moeda_int = abap_true.
            " wa_zglt036_flg-fl_cv_moeda_for = abap_true.
            wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
            APPEND  wa_zglt036_flg TO it_zglt036_flg.
          ENDLOOP.
      ENDCASE.
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

      LOOP AT git_zglt0108 INTO gwa_zglt0108.
        gwa_zglt0108-objkey   = lva_objkey.
        gwa_zglt0108-doc_lcto = p_doc_lcto.
        gwa_zglt0108-doc_lcto_est = ''.
        MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX sy-tabix TRANSPORTING objkey doc_lcto.
        CLEAR: gwa_zglt0108.
      ENDLOOP.


      DELETE FROM zglt0108 WHERE monat       = gwa_zglt0108-monat
                            AND  gjahr       = gwa_zglt0108-gjahr
                            AND  investidora = gwa_zglt0108-investidora
                            AND  investida   = gwa_zglt0108-investida.
      COMMIT WORK.


      MODIFY zglt0108 FROM TABLE git_zglt0108.
      COMMIT WORK.

      MOVE: '' TO wg_estorno.

      MESSAGE s836(sd) WITH 'O documento foi gerado com sucesso!'.   "RJF Analisar controle

      CALL SCREEN 0100.

      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = gwa_stable.

    ELSE.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL079 não econtrados'
                                              'com parâmetros informados!'.
      EXIT.
    ENDIF.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existe documento gerado!'.
    EXIT.
  ENDIF.
ENDFORM.                    " MODIFICA_STATUS
*&---------------------------------------------------------------------*
*&      Form GERAR_DOCUMENTO
*&---------------------------------------------------------------------*
FORM f_estornar_documento.

  DATA: lva_budat(10) TYPE c,
        lva_stblg     TYPE bkpf-stblg.

  CLEAR: gwa_zglt0108.
  READ TABLE git_zglt0108 INTO gwa_zglt0108 INDEX 1.

  IF gwa_zglt0108-belnr IS NOT INITIAL.

    CONCATENATE gva_data_fim+6(2) gva_data_fim+4(2) gva_data_fim(4) INTO lva_budat.


    REFRESH: git_dta, git_msg.
    PERFORM zf_shdb USING: 'SAPMF05A' '0105' 'X'  ' '           ' ',
                           ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
                           ' '        ' '    ' '  'BDC_OKCODE'   '=BU',
                           ' '        ' '    ' '  'RF05A-BELNS'  gwa_zglt0108-belnr,
                           ' '        ' '    ' '  'BKPF-BUKRS'   gwa_zglt0108-investidora,
                           ' '        ' '    ' '  'RF05A-GJAHS'  gwa_zglt0108-gjahr,
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
       WHERE bukrs = gwa_zglt0108-investidora
         AND belnr = gwa_zglt0108-belnr
         AND gjahr = gwa_zglt0108-gjahr.

      CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

      LOOP AT git_zglt0108 INTO gwa_zglt0108.
        CLEAR: gwa_zglt0108-belnr.
        gwa_zglt0108-doc_lcto_est = lva_stblg.
        MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX sy-tabix  TRANSPORTING belnr doc_lcto_est.
      ENDLOOP.

      MODIFY zglt0108  FROM TABLE git_zglt0108.
      COMMIT WORK.

      MOVE: lva_stblg TO wg_estorno,
            ''        TO wg_doc_cont.

      LOOP AT git_saida INTO gwa_saida.
        MOVE: '@02@' TO gwa_saida-icons.
        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons .
      ENDLOOP.

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

**  Begin of CS2023000082  #103662 FF   22.02.2023
  IF gwa_zglt0104-moeda_funcional = 'USD'.
    SELECT * FROM zglt0112 INTO TABLE @DATA(lt_zglt0112)
      WHERE investidora = @p_bukrs_invra
        AND investida   = @p_bukrs_invda.
    IF sy-subrc = 0.
      LOOP AT lt_zglt0112 ASSIGNING FIELD-SYMBOL(<fs_112>).

        CLEAR: gwa_selection .
        gwa_selection-selname = 'SD_SAKNR'.
        gwa_selection-kind    = 'S'.
        gwa_selection-sign    = 'I'.
        gwa_selection-option  = 'EQ'.
        gwa_selection-low     = <fs_112>-hkont.
        APPEND gwa_selection TO git_selection.

      ENDLOOP.
    ENDIF.
  ENDIF.
**  End of FF  22.02.2023

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
        lwa_zglt0108 TYPE zglt0108,
        lwa_zglt034  TYPE zglt034,
        lwa_saida    LIKE LINE OF git_saida.


  READ TABLE git_zglt0108 INTO gwa_zglt0108 INDEX 1.

  IF gwa_zglt0108-belnr IS INITIAL AND gwa_zglt0108-doc_lcto_est IS INITIAL.

    SELECT SINGLE *
      FROM zglt034
      INTO lwa_zglt034
     WHERE bukrs = gwa_zglt0108-investidora
       AND lote  = gwa_zglt0108-lote.

    CHECK sy-subrc IS INITIAL.

    SELECT SINGLE *
      FROM zib_contabil_chv
      INTO lwa_zibchv
    WHERE obj_key = gwa_zglt0108-objkey.

    IF ( sy-subrc IS NOT INITIAL ).

      SELECT SINGLE *
        FROM zib_contabil_err
        INTO lwa_ziberr
      WHERE obj_key = gwa_zglt0108-objkey.

    ENDIF.

    IF ( lwa_zibchv IS NOT INITIAL AND lwa_zibchv-belnr IS NOT INITIAL ).

      MOVE: lwa_zibchv-belnr   TO wg_doc_cont.

      LOOP AT git_zglt0108 INTO gwa_zglt0108.
        SELECT SINGLE *
        FROM zib_contabil_chv
        INTO @DATA(lwa_zibchv_aux)
        WHERE obj_key = @gwa_zglt0108-objkey.
        IF sy-subrc IS NOT INITIAL.
          FREE lwa_zibchv_aux-belnr.
        ENDIF.
        gwa_zglt0108-belnr =  lwa_zibchv_aux-belnr.
        MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX sy-tabix TRANSPORTING belnr.
      ENDLOOP.

      MODIFY zglt0108  FROM TABLE git_zglt0108.
      COMMIT WORK.

      MOVE: gwa_zglt0108-belnr TO wg_doc_cont,
            gwa_zglt0108-belnr TO p_doc_cont.

      LOOP AT git_saida INTO gwa_saida.

        IF gwa_saida-belnr IS INITIAL.
          MOVE: '@02@' TO gwa_saida-icons.
          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
        ELSE.
          MOVE: '@01@' TO gwa_saida-icons.
          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
        ENDIF.
      ENDLOOP.

      IF ( lwa_ziberr IS NOT INITIAL ).

        LOOP AT git_saida INTO gwa_saida.

          IF gwa_saida-belnr IS INITIAL.
            MOVE: '@02@' TO gwa_saida-icons.
            MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
          ELSE.
            MOVE: '@01@' TO gwa_saida-icons.
            MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
          ENDIF.

*        MOVE: '@02@' TO gwa_saida-icons.
*        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
        ENDLOOP.
      ENDIF.

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

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ERRO_ZIB
*&---------------------------------------------------------------------*
FORM f_exibe_erro_zib .

  READ TABLE git_zglt0108 INTO gwa_zglt0108 INDEX 1.

  TYPES: BEGIN OF ty_zib_err,
           obj_key        TYPE zib_contabil_err-obj_key,
           dt_atualizacao TYPE zib_contabil_err-dt_atualizacao,
           hr_atualizacao TYPE zib_contabil_err-hr_atualizacao,
           message        TYPE zib_contabil_err-message,
         END OF ty_zib_err.

  DATA: lit_zib_err TYPE TABLE OF ty_zib_err.

  CHECK ( gwa_zglt0108-objkey IS NOT INITIAL ).

  SELECT obj_key, dt_atualizacao, hr_atualizacao, message
     FROM zib_contabil_err INTO TABLE @lit_zib_err
  WHERE obj_key = @gwa_zglt0108-objkey.

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
*&      Form  F_GERAR_DOCUMENTO_U
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_gerar_documento_u .

  DATA: lva_num_lote  TYPE zlote_num,
        lva_dp_resp   TYPE char2,
        lva_blart     TYPE zglt031-blart,
        lva_bktxt     TYPE zglt031-bktxt,
        lva_erro      TYPE c,
        lv_tabix      TYPE i,
        lv_n_lanc     TYPE i,
        lva_prov_est  TYPE zglt031-prov_est,
        lva_liberado  TYPE char01,
        lva_equiv_brl TYPE ty_saida-vlr_brl,
        lva_equiv_usd TYPE ty_saida-vlr_usd,
        lv_tabixi     TYPE i,
        lva_objkey    TYPE zib_contabil_chv-obj_key.

  DATA: lit_zglt036  TYPE TABLE OF zglt036,
        lit_zglt032  TYPE TABLE OF zglt032,
        lit_zglt0108 TYPE TABLE OF zglt0108.

  DATA: it_zglt036_flg TYPE zde_zglt036_flg_t,
        wa_zglt036_flg TYPE zde_zglt036_flg.

  DATA: lwa_saida    TYPE ty_saida,
        lwa_zglt035  TYPE zglt035,
        lwa_zglt0105 TYPE zglt0105,
        lwa_zglt036  TYPE zglt036,
        chave_saldo  TYPE c,
        lwa_zglt032  TYPE zglt032,
        lwa_zglt0108 TYPE zglt0108,
        v_hkont      TYPE hkont.


  READ TABLE git_zglt0108 INTO gwa_zglt0108 INDEX 1.

  IF sy-subrc IS NOT INITIAL.
    DELETE FROM zglt0108 WHERE monat            = gwa_zglt0108-monat
                               AND  gjahr       = gwa_zglt0108-gjahr
                               AND  investidora = gwa_zglt0108-investidora
                               AND  investida   = gwa_zglt0108-investida.
    COMMIT WORK.
  ENDIF.

  CLEAR: gwa_zglt0104.
  READ TABLE git_zglt0104 INTO gwa_zglt0104 INDEX 1.

*  IF ( gwa_zglt0108-belnr IS INITIAL AND gwa_zglt0108-objkey IS INITIAL ) OR
*      ( gwa_zglt0108-objkey IS NOT INITIAL AND gwa_zglt0108-doc_lcto_est IS NOT INITIAL ) .

  IF ( gwa_zglt0108-belnr IS INITIAL ) OR
    ( gwa_zglt0108-belnr IS NOT INITIAL AND gwa_zglt0108-doc_lcto_est IS NOT INITIAL ) .

    LOOP AT git_saida INTO gwa_saida.

      v_hkont = gwa_saida-mov_pl(6).

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_hkont
        IMPORTING
          output = v_hkont.

* RJF - Ini - 2023.03.21
      IF gwa_saida-equiv_ref_brl LT 0 AND gwa_saida-equiv_ref_usd LT 0.
        lv_n_lanc = 1.
        chave_saldo = '2'.  "Negativo
      ELSEIF gwa_saida-equiv_ref_brl GE 0 AND gwa_saida-equiv_ref_usd GE 0.
        IF gwa_saida-equiv_ref_brl EQ 0 AND gwa_saida-equiv_ref_usd EQ 0.
          FREE lv_n_lanc.
        ELSE.
          lv_n_lanc = 1.
          chave_saldo = '1'.  "Positivo
        ENDIF.
      ELSE.
        lv_n_lanc = 2.

        IF gwa_saida-equiv_ref_usd LT 0.
          DATA(chave_saldou) = '2'.  "Negativo
        ELSE.
          chave_saldou = '1'.  "Positivo
        ENDIF.

        IF gwa_saida-equiv_ref_brl LT 0.
          DATA(chave_saldob) = '2'.  "Negativo
        ELSE.
          chave_saldob = '1'.  "Positivo
        ENDIF.

      ENDIF.
* RJF - Fim - 2023.03.21

      CASE gwa_zglt0104-moeda_funcional.
        WHEN 'OUTRAS' OR 'BRL'.

          SELECT SINGLE *
            FROM zglt0105 INTO lwa_zglt0105
          WHERE investidora EQ p_bukrs_invra
            AND investida EQ p_bukrs_invda
            AND moeda_funcional EQ gwa_zglt0104-moeda_funcional
            AND tp_lancamento EQ 2
            AND saldo EQ chave_saldo.

        WHEN 'USD'.

          IF lv_n_lanc EQ 1.

            SELECT SINGLE * FROM zglt0112 INTO w_zglt0112 "Empresas Exceção Equivalencia Patrimonial
            WHERE investidora = p_bukrs_invra
              AND investida   = p_bukrs_invda
              AND hkont EQ v_hkont.
*            IF sy-subrc <> 0.
*              CLEAR w_zglt0112[].
*            ENDIF.


            SELECT SINGLE *
            FROM zglt0105
              INTO lwa_zglt0105
              WHERE investidora EQ p_bukrs_invra
                AND investida EQ p_bukrs_invda
                AND moeda_funcional EQ gwa_zglt0104-moeda_funcional
                AND tp_lancamento EQ 3 "Reflexa do PL
              AND saldo EQ chave_saldo
              AND tipo_reflexa_pl EQ w_zglt0112-tipo_reflexa_pl.


          ELSE.

            SELECT SINGLE * FROM zglt0112 INTO w_zglt0112 "Empresas Exceção Equivalencia Patrimonial
            WHERE investidora = p_bukrs_invra
              AND investida   = p_bukrs_invda
              AND hkont EQ v_hkont.
            "AND tipo_reflexa_pl = @lwa_zglt0105-tipo_reflexa_pl.
*            IF sy-subrc <> 0.
*              CLEAR lt_zglt0112[].
*            ENDIF.

            SELECT SINGLE *
            FROM zglt0105 INTO lwa_zglt0105
              WHERE investidora EQ p_bukrs_invra
                AND investida EQ p_bukrs_invda
                AND moeda_funcional EQ gwa_zglt0104-moeda_funcional
                AND tp_lancamento EQ 3 "Reflexa do PL
              AND saldo EQ chave_saldob
              AND tipo_reflexa_pl EQ w_zglt0112-tipo_reflexa_pl.



          ENDIF.
      ENDCASE.


* RJF - Ini - 2023.03.21
      DO lv_n_lanc TIMES. " RJF verificar modificações ...
        lv_tabix = lv_tabix + 1.
        IF lv_n_lanc EQ 2 AND lv_tabix EQ 2.

          SELECT SINGLE * FROM zglt0112 INTO w_zglt0112 "Empresas Exceção Equivalencia Patrimonial
                   WHERE investidora = p_bukrs_invra
                     AND investida   = p_bukrs_invda
                     "AND tipo_reflexa_pl = @lwa_zglt0105-tipo_reflexa_pl.
                     AND hkont EQ v_hkont.
*          IF sy-subrc <> 0.
*            CLEAR lt_zglt0112[].
*          ENDIF.

          SELECT SINGLE *
          FROM zglt0105 INTO lwa_zglt0105
            WHERE investidora EQ p_bukrs_invra
              AND investida EQ p_bukrs_invda
              AND moeda_funcional EQ gwa_zglt0104-moeda_funcional
              AND tp_lancamento EQ 3 "Reflexa do PL
            AND saldo EQ chave_saldou
            AND tipo_reflexa_pl EQ w_zglt0112-tipo_reflexa_pl.

          "CLEAR w_zglt0112[].

        ENDIF.
* RJF - Fim - 2023.03.21

        IF lwa_zglt0105 IS NOT INITIAL.

          SELECT SINGLE blart bktxt prov_est FROM zglt031 INTO ( lva_blart, lva_bktxt, lva_prov_est ) WHERE tp_lcto = lwa_zglt0105-modelo_zgl.
          lva_dp_resp = '83'. "Contabilidade

* Criar lote na ZGLT034
          CALL METHOD zcl_gerar_lote=>create_lote
            EXPORTING
              i_bukrs       = p_bukrs_invra
              i_descr_lote  = 'Equivalencia Reflexa'
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

          IF gwa_zglt0104-moeda_funcional = 'OUTRAS' OR   gwa_zglt0104-moeda_funcional = 'BRL'.
            MOVE 'X'                      TO lwa_zglt035-st_lc_moeda.
          ELSE.
            MOVE ''                      TO lwa_zglt035-st_lc_moeda.
          ENDIF.

          IF lv_n_lanc EQ 2. "AND lv_tabix EQ 2.

            MOVE: 'X'    TO lwa_zglt035-st_lc_moeda,
                  'BRL'  TO lwa_zglt035-moeda_doc.

          ENDIF.

          CONCATENATE p_bukrs_invra '-' p_bukrs_invda INTO lwa_zglt035-xblnr.

* Criar item na ZGLT036
          SELECT *
            FROM zglt032 INTO TABLE lit_zglt032
          WHERE tp_lcto EQ  lwa_zglt035-tp_lcto.

          IF lit_zglt032 IS NOT INITIAL.

            IF ( lv_n_lanc EQ 2 AND lv_tabix EQ 1 ) OR lv_n_lanc EQ 1.

              CLEAR: git_zglt0108[].


*                  LOOP AT git_saida INTO gwa_saida. "RJF

              MOVE:
                    p_mes                     TO gwa_zglt0108-monat,
                    p_ano                     TO gwa_zglt0108-gjahr,
                    p_bukrs_invra             TO gwa_zglt0108-investidora,
                    p_bukrs_invda             TO gwa_zglt0108-investida,
                    lwa_zglt035-lote          TO gwa_zglt0108-lote,
                    lwa_zglt035-doc_lcto      TO gwa_zglt0108-doc_lcto,
                    ''                        TO gwa_zglt0108-doc_lcto_est,
                    sy-uname                  TO gwa_zglt0108-usnam,
                    sy-datum                  TO gwa_zglt0108-dt_atual,
                    sy-uzeit                  TO gwa_zglt0108-hr_atual.

              gwa_zglt0108-item_balanco  = gwa_saida-id_mov_pl    .
              gwa_zglt0108-desc_cta_equ  = gwa_saida-mov_pl       .
              gwa_zglt0108-moeda_per     = gwa_saida-moeda_per       .
              gwa_zglt0108-moeda_comp    = gwa_saida-moeda_comp      .
              gwa_zglt0108-vlr_brl       = gwa_saida-vlr_brl         .
              gwa_zglt0108-desvio_brl    = gwa_saida-desv_brl        .
              gwa_zglt0108-vlr_usd       = gwa_saida-vlr_usd         .
              gwa_zglt0108-desvio_usd    = gwa_saida-desv_usd        .
              gwa_zglt0108-taxa          = gwa_saida-taxa            .
              gwa_zglt0108-equiv_ref_brl = gwa_saida-equiv_ref_brl   .
              gwa_zglt0108-equiv_ref_usd = gwa_saida-equiv_ref_usd   .

              CONDENSE gwa_zglt0108-item_balanco NO-GAPS.

            ENDIF.

            IF gwa_saida-id_mov_pl = '99'.
              IF gwa_zglt0104-moeda_funcional = 'OUTRAS'.
                lva_equiv_brl = gwa_saida-equiv_ref_brl.
                lva_equiv_usd = gwa_saida-equiv_ref_brl.
              ELSE.
                IF gwa_zglt0104-moeda_funcional = 'BRL'.
                  lva_equiv_brl = gwa_saida-equiv_ref_usd.
                  lva_equiv_usd = gwa_saida-equiv_ref_usd.
                ENDIF.
              ENDIF.
            ELSE.
              IF gwa_zglt0104-moeda_funcional = 'USD' .
                lva_equiv_brl = gwa_saida-equiv_ref_brl.
                lva_equiv_usd = gwa_saida-equiv_ref_usd.
              ENDIF.
            ENDIF.

            APPEND gwa_zglt0108 TO git_zglt0108.
            CLEAR:
*                   gwa_saida,
                   gwa_zglt0108.

*                  ENDLOOP. "RJF

            LOOP AT lit_zglt032 INTO lwa_zglt032.

              lv_tabixi = lv_tabixi + 1.

              lwa_zglt036-tp_lcto = lwa_zglt035-tp_lcto.

              MOVE: lv_tabixi                   TO lwa_zglt036-seqitem,
                    lwa_zglt032-bschl           TO lwa_zglt036-bschl,
                    lwa_zglt032-hkont           TO lwa_zglt036-hkont,
                    lwa_zglt032-sgtxt           TO lwa_zglt036-sgtxt,
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

              IF lv_n_lanc EQ 2 AND lv_tabix EQ 2.

                IF gwa_zglt0104-moeda_funcional = 'OUTRAS'.
                  MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc,
                        abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int.

                ELSE.
                  IF gwa_zglt0104-moeda_funcional = 'BRL'.
                    MOVE: abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.

                  ELSE.
*                    MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc,
*                          abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int,
*                          abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.

*                    IF lv_tabix EQ 1.
*                      MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc.
**                            abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int.
*                    ELSE.

                    MOVE: abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_doc,
                          abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte .

                    FREE lwa_zglt036-vlr_moeda_int.

*                    ENDIF.

                  ENDIF.
                ENDIF.

                APPEND lwa_zglt036 TO lit_zglt036.
                CLEAR: lwa_zglt036, lwa_zglt032.

              ELSE.

                IF gwa_zglt0104-moeda_funcional = 'OUTRAS'.
                  MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc,
                        abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int.

                ELSE.
                  IF gwa_zglt0104-moeda_funcional = 'BRL'.
                    MOVE: abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.

                  ELSE.
                    MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_doc.

                    IF lv_tabix EQ 1.
                      MOVE: abs( lva_equiv_brl ) TO lwa_zglt036-vlr_moeda_int.
                    ELSE.
                      FREE: lwa_zglt036-vlr_moeda_int.
                    ENDIF.

                    IF lv_tabix EQ 1 AND lv_n_lanc EQ 2.
                      FREE: lwa_zglt036-vlr_moeda_forte.
                    ELSE.
                      MOVE: abs( lva_equiv_usd ) TO lwa_zglt036-vlr_moeda_forte.
                    ENDIF.
                  ENDIF.
                ENDIF.

                APPEND lwa_zglt036 TO lit_zglt036.
                CLEAR: lwa_zglt036, lwa_zglt032.

              ENDIF.

            ENDLOOP.
          ELSE.
            MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Tipo de lançamento não encontrados ZGLT032!'.
            lva_erro = abap_true.
            EXIT.
          ENDIF.

          CLEAR lwa_zglt036.
          LOOP AT lit_zglt036 INTO lwa_zglt036.

            wa_zglt036_flg-doc_lcto        = lwa_zglt036-doc_lcto.
            wa_zglt036_flg-seqitem         = lwa_zglt036-seqitem.
            wa_zglt036_flg-seqsub          = lwa_zglt036-seqsub.
            " wa_zglt036_flg-fl_cv_moeda_doc = abap_true.
            IF lv_n_lanc EQ 2 AND lv_tabix EQ 2.
              wa_zglt036_flg-fl_cv_moeda_int = abap_true.
            ENDIF.

            IF lv_n_lanc EQ 2 AND lv_tabix EQ 1.
              wa_zglt036_flg-fl_cv_moeda_for = abap_true.
            ENDIF.
            wa_zglt036_flg-fl_cv_moeda_gru = abap_true.
            APPEND  wa_zglt036_flg TO it_zglt036_flg.
          ENDLOOP.

* Contabilizar
          CALL METHOD zcl_gerar_lote=>contabilizar_lote(
            EXPORTING
              i_arredonda   = abap_true
              i_zglt036_flg = it_zglt036_flg
            CHANGING
              i_zglt036     = lit_zglt036
              i_zglt035     = lwa_zglt035 ).

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
          FREE: lit_zglt036, it_zglt036_flg, lv_tabixi. " RJF
          CONCATENATE 'ZGL17'  p_doc_lcto p_ano INTO lva_objkey.

          IF lv_n_lanc EQ 1.
            LOOP AT git_zglt0108 INTO gwa_zglt0108 WHERE item_balanco EQ gwa_saida-id_mov_pl.
              gwa_zglt0108-objkey   = lva_objkey.
              gwa_zglt0108-lote     = lva_num_lote.
              gwa_zglt0108-doc_lcto = p_doc_lcto.
              gwa_zglt0108-doc_lcto_est = ''.
              MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX sy-tabix TRANSPORTING objkey lote doc_lcto doc_lcto_est.
              CLEAR: gwa_zglt0108.
            ENDLOOP.
          ELSEIF lv_n_lanc EQ 2.

            IF lv_tabix EQ 1.

              LOOP AT git_zglt0108 INTO gwa_zglt0108 WHERE item_balanco EQ gwa_saida-id_mov_pl.
                gwa_zglt0108-objkey   = lva_objkey.
                gwa_zglt0108-lote     = lva_num_lote.
                gwa_zglt0108-doc_lcto = p_doc_lcto.
                gwa_zglt0108-doc_lcto_est = ''.
                MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX sy-tabix TRANSPORTING objkey lote doc_lcto doc_lcto_est.
                CLEAR: gwa_zglt0108.
              ENDLOOP.

            ELSEIF lv_tabix EQ 2.

              LOOP AT git_zglt0108 INTO gwa_zglt0108 WHERE item_balanco EQ gwa_saida-id_mov_pl.
                gwa_zglt0108-objkey_2   = lva_objkey.
                gwa_zglt0108-lote_2     = lva_num_lote.
                gwa_zglt0108-doc_lcto_2 = p_doc_lcto.
                gwa_zglt0108-doc_lcto_est_2 = ''.
                MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX sy-tabix TRANSPORTING objkey_2 lote_2 doc_lcto_2 doc_lcto_est_2.
                CLEAR: gwa_zglt0108.
              ENDLOOP.
            ENDIF.
          ENDIF.

          "LOOP AT git_saida INTO gwa_saida.
          IF lv_n_lanc EQ 2 AND lv_tabix EQ 2.
            MOVE ''  TO  gwa_saida-doc_lcto_est.
            MOVE ''  TO  gwa_saida-doc_lcto_est_2.

            MODIFY git_saida FROM gwa_saida TRANSPORTING doc_lcto_est doc_lcto_est_2.
          ELSEIF lv_n_lanc EQ 1.
            MOVE ''  TO  gwa_saida-doc_lcto_est.
            MOVE ''  TO  gwa_saida-doc_lcto_est_2.

            MODIFY git_saida FROM gwa_saida TRANSPORTING doc_lcto_est doc_lcto_est_2.
          ENDIF.
          "ENDLOOP.
          DELETE FROM zglt0108 WHERE monat       = gwa_zglt0108-monat
                                AND  gjahr       = gwa_zglt0108-gjahr
                                AND  investidora = gwa_zglt0108-investidora
                                AND  investida   = gwa_zglt0108-investida.
          COMMIT WORK.


          MODIFY zglt0108 FROM TABLE git_zglt0108.
          COMMIT WORK.

          MOVE: '' TO wg_estorno.

        ELSE.
          DATA(lva_errop) = abap_true.
          EXIT.
        ENDIF.
      ENDDO. " RJF

      CLEAR: gwa_saida.
      CLEAR lv_tabix.

    ENDLOOP. " RJF

    IF lva_erro IS INITIAL AND lva_errop IS INITIAL.
      MESSAGE s836(sd) WITH 'Documento(s) gerado(s) com sucesso!'.   "RJF Analisar controle
    ELSEIF lva_errop IS NOT INITIAL.
      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL079 não econtrados'
                                              'com parâmetros informados!'.
    ELSE.
      MESSAGE s836(sd) WITH 'Erro geração do(s) documento(s)!'.   "RJF Analisar controle
    ENDIF.

    CALL SCREEN 0100.

    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.

*    ELSE.
*      MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Dados ZGL079 não econtrados'
*                                              'com parâmetros informados!'.
*      EXIT.
*    ENDIF.
  ELSE.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Já existem documentos gerados!'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ESTORNAR_DOCUMENTO_U
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_estornar_documento_u .


  DATA: lva_budat(10) TYPE c,
        lva_stblg     TYPE bkpf-stblg,
        lv_tabixt     TYPE i.

  CLEAR: gwa_zglt0108.
*  READ TABLE git_zglt0108 INTO gwa_zglt0108 INDEX 1.

  LOOP AT git_zglt0108 INTO gwa_zglt0108.
    DATA(lv_tabix) = sy-tabix.

    IF gwa_zglt0108-belnr IS NOT INITIAL AND gwa_zglt0108-belnr_2 IS NOT INITIAL.
      DATA(lv_time) = 2.
    ELSEIF gwa_zglt0108-belnr IS NOT INITIAL.
      lv_time = 1.
    ENDIF.

    DO lv_time TIMES.
      lv_tabixt = lv_tabixt + 1.

      IF lv_tabixt EQ 1.
        DATA(lv_belnr) = gwa_zglt0108-belnr.
      ELSEIF lv_tabixt EQ 2.
        lv_belnr = gwa_zglt0108-belnr_2.
      ENDIF.



      IF lv_belnr IS NOT INITIAL.

        CONCATENATE gva_data_fim+6(2) gva_data_fim+4(2) gva_data_fim(4) INTO lva_budat.


        REFRESH: git_dta, git_msg.
        PERFORM zf_shdb USING: 'SAPMF05A' '0105' 'X'  ' '           ' ',
                               ' '        ' '    ' '  'BDC_CURSOR'  'UF05A-STGRD',
                               ' '        ' '    ' '  'BDC_OKCODE'   '=BU',
                               ' '        ' '    ' '  'RF05A-BELNS'  lv_belnr,
                               ' '        ' '    ' '  'BKPF-BUKRS'   gwa_zglt0108-investidora,
                               ' '        ' '    ' '  'RF05A-GJAHS'  gwa_zglt0108-gjahr,
                               ' '        ' '    ' '  'UF05A-STGRD'  '01',
                               ' '        ' '    ' '   'BSIS-BUDAT'  lva_budat.

        opt-dismode = 'N'.

        CALL TRANSACTION 'FB08' USING git_dta OPTIONS FROM opt
                MESSAGES INTO git_msg.

        READ TABLE git_msg ASSIGNING FIELD-SYMBOL(<fs_msg>)
                                          WITH KEY msgtyp = 'E'.

        IF sy-subrc IS INITIAL.

          DATA(lv_error) = abap_true.

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

          IF lv_tabixt EQ 1.
            SELECT SINGLE stblg
              FROM bkpf INTO lva_stblg
             WHERE bukrs = gwa_zglt0108-investidora
               AND belnr = gwa_zglt0108-belnr
               AND gjahr = gwa_zglt0108-gjahr.

            CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).
            CLEAR: gwa_zglt0108-belnr.
            gwa_zglt0108-doc_lcto_est = lva_stblg.
            MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX lv_tabix TRANSPORTING belnr doc_lcto_est.

          ELSEIF lv_tabixt EQ 2.
            SELECT SINGLE stblg
             FROM bkpf INTO lva_stblg
            WHERE bukrs = gwa_zglt0108-investidora
              AND belnr = gwa_zglt0108-belnr_2
              AND gjahr = gwa_zglt0108-gjahr.

            CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).
            CLEAR: gwa_zglt0108-belnr_2.
            gwa_zglt0108-doc_lcto_est_2 = lva_stblg.
            MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX lv_tabix  TRANSPORTING belnr_2 doc_lcto_est_2.

          ENDIF.
*          CHECK ( sy-subrc = 0 ) AND ( lva_stblg IS NOT INITIAL ).

*        LOOP AT git_zglt0108 INTO gwa_zglt0108.
*          CLEAR: gwa_zglt0108-belnr.
*          gwa_zglt0108-doc_lcto_est = lva_stblg.
*          MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX lv_tabix  TRANSPORTING belnr doc_lcto_est.
*        ENDLOOP.

          LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.
            MOVE: '@02@' TO gwa_saida-icons.
            IF lv_tabixt EQ 1.
              MOVE lva_stblg TO gwa_saida-doc_lcto_est.
              MOVE ' '  TO gwa_saida-belnr.

            ELSE.
              MOVE ' '  TO gwa_saida-belnr_2.
              MOVE lva_stblg TO gwa_saida-doc_lcto_est_2.
            ENDIF.

            MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons belnr belnr_2 doc_lcto_est doc_lcto_est_2 .
          ENDLOOP.

          MODIFY zglt0108  FROM TABLE git_zglt0108.
          COMMIT WORK.

        ENDIF.
      ELSE.
        DATA(lv_errbelnr) = abap_true.

      ENDIF.

    ENDDO.
    CLEAR lv_tabixt.

  ENDLOOP.

  IF lv_error IS INITIAL.
    MOVE: lva_stblg TO wg_estorno,
          ''        TO wg_doc_cont.
  ENDIF.
*      LOOP AT git_saida INTO gwa_saida.
*        MOVE: '@02@' TO gwa_saida-icons.
*        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons .
*      ENDLOOP.

  CALL SCREEN 0100.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = gwa_stable.

  IF lv_error IS INITIAL.
    MESSAGE s836(sd) WITH 'Documento(s) estornado(s) com sucesso!'.
  ENDIF.

  IF lv_errbelnr IS NOT INITIAL.
    MESSAGE s836(sd) DISPLAY LIKE 'E' WITH 'Não existe documento para estorno!'.
    EXIT.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_DOC_U
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_atualiza_doc_u .

  DATA: lwa_zibchv   TYPE zib_contabil_chv,
        lwa_ziberr   TYPE zib_contabil_err,
        lwa_zglt0108 TYPE zglt0108,
        lwa_zglt034  TYPE zglt034,
        lwa_saida    LIKE LINE OF git_saida.

  CLEAR: gwa_saida,
        gwa_zglt0108.
  FREE:  git_saida.

  LOOP AT git_zglt0108 INTO gwa_zglt0108.

    IF gwa_zglt0108-belnr IS NOT INITIAL.
      gwa_saida-icons = '@01@'.
    ELSE.
      gwa_saida-icons = '@02@'.
    ENDIF.

    CASE gwa_zglt0108-desc_cta_equ.
      WHEN 'ATIVO'.
        gwa_saida-id_ord = 1.
      WHEN 'PASSIVO'.
        gwa_saida-id_ord = 2.
      WHEN 'PATRIMÔNIO LÍQUIDO'.
        gwa_saida-id_ord = 3.
      WHEN 'RESULTADO DO PERIODO'.
        gwa_saida-id_ord = 4.
      WHEN 'PROVISOES P/IR E CSLL'.
        gwa_saida-id_ord = 5.
      WHEN 'PROVISOES P/PARTICIPAÇOES NOS RESULTADOS'.
        gwa_saida-id_ord = 6.
      WHEN 'CTA'.
        gwa_saida-id_ord = 7.
      WHEN OTHERS.
        gwa_saida-id_ord = 99.
    ENDCASE.

    gwa_saida-id_mov_pl        =    gwa_zglt0108-item_balanco  .
    gwa_saida-mov_pl           =    gwa_zglt0108-desc_cta_equ  .
    gwa_saida-moeda_per        =    gwa_zglt0108-moeda_per      .
    gwa_saida-moeda_comp       =    gwa_zglt0108-moeda_comp     .
    gwa_saida-vlr_brl          =    gwa_zglt0108-vlr_brl        .
    gwa_saida-desv_brl         =    gwa_zglt0108-desvio_brl     .
    gwa_saida-vlr_usd          =    gwa_zglt0108-vlr_usd        .
    gwa_saida-desv_usd         =    gwa_zglt0108-desvio_usd     .
    gwa_saida-taxa             =    gwa_zglt0108-taxa           .
    gwa_saida-equiv_ref_brl    =    gwa_zglt0108-equiv_ref_brl  .
    gwa_saida-equiv_ref_usd    =    gwa_zglt0108-equiv_ref_usd  .
    gwa_saida-belnr              =    gwa_zglt0108-belnr.
    gwa_saida-doc_lcto_est       =    gwa_zglt0108-doc_lcto_est.
    gwa_saida-belnr_2            =    gwa_zglt0108-belnr_2.
    gwa_saida-doc_lcto_est_2     =    gwa_zglt0108-doc_lcto_est_2.
    gwa_saida-doc_lcto           =    gwa_zglt0108-doc_lcto.
    gwa_saida-doc_lcto_2         =    gwa_zglt0108-doc_lcto_2.

    APPEND gwa_saida TO git_saida.
    CLEAR: gwa_saida,
          gwa_zglt0108.
  ENDLOOP.

  SORT  git_saida BY id_ord.

  LOOP AT git_zglt0108 INTO gwa_zglt0108.

    SELECT SINGLE *
    FROM zib_contabil_chv
    INTO @DATA(lwa_zibchv_aux)
    WHERE obj_key = @gwa_zglt0108-objkey.
    IF sy-subrc IS NOT INITIAL.
      FREE lwa_zibchv_aux-belnr.
    ENDIF.
    gwa_zglt0108-belnr =  lwa_zibchv_aux-belnr.
    MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX sy-tabix TRANSPORTING belnr.

  ENDLOOP.

  LOOP AT git_zglt0108 INTO gwa_zglt0108.
    DATA(lv_tabix) = sy-tabix.

    IF gwa_zglt0108-belnr IS INITIAL AND gwa_zglt0108-doc_lcto_est IS INITIAL.

      SELECT SINGLE *
        FROM zglt034
        INTO lwa_zglt034
       WHERE bukrs = gwa_zglt0108-investidora
         AND lote  = gwa_zglt0108-lote.

      CHECK sy-subrc IS INITIAL.

      SELECT SINGLE *
        FROM zib_contabil_chv
        INTO lwa_zibchv
      WHERE obj_key = gwa_zglt0108-objkey.

      IF ( sy-subrc IS NOT INITIAL ).

        SELECT SINGLE *
          FROM zib_contabil_err
          INTO lwa_ziberr
        WHERE obj_key = gwa_zglt0108-objkey.

      ENDIF.

      IF ( lwa_zibchv IS NOT INITIAL AND lwa_zibchv-belnr IS NOT INITIAL ).

        MOVE: lwa_zibchv-belnr   TO wg_doc_cont.

*        LOOP AT git_zglt0108 INTO gwa_zglt0108.
        gwa_zglt0108-belnr =  lwa_zibchv-belnr.
        MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX lv_tabix TRANSPORTING belnr.
*        ENDLOOP.

        MODIFY zglt0108  FROM TABLE git_zglt0108.
        COMMIT WORK.

        MOVE: gwa_zglt0108-belnr TO wg_doc_cont,
              gwa_zglt0108-belnr TO p_doc_cont.

        LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.

          IF gwa_zglt0108-belnr IS NOT INITIAL.
            MOVE: '@01@' TO gwa_saida-icons.
            MOVE gwa_zglt0108-belnr  TO  gwa_saida-belnr.
          ELSE.
            IF lwa_ziberr IS NOT INITIAL.
              MOVE: '@02@' TO gwa_saida-icons.
              MOVE gwa_zglt0108-belnr  TO  gwa_saida-belnr.
            ENDIF.
          ENDIF.

          MOVE: gwa_zglt0108-doc_lcto   TO gwa_saida-doc_lcto, " RJF
                gwa_zglt0108-doc_lcto_2 TO gwa_saida-doc_lcto_2.

          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons belnr doc_lcto doc_lcto_2.
        ENDLOOP.

      ELSEIF ( lwa_ziberr IS NOT INITIAL ).

        LOOP AT git_saida INTO gwa_saida  WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.

          IF gwa_zglt0108-belnr IS NOT INITIAL.
            MOVE: '@01@' TO gwa_saida-icons.
            MOVE gwa_zglt0108-belnr  TO  gwa_saida-belnr.
          ELSE.
            MOVE: '@02@' TO gwa_saida-icons.
            MOVE gwa_zglt0108-belnr  TO  gwa_saida-belnr.
          ENDIF.

          MOVE: gwa_zglt0108-doc_lcto   TO gwa_saida-doc_lcto, " RJF
                gwa_zglt0108-doc_lcto_2 TO gwa_saida-doc_lcto_2.

          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons belnr doc_lcto doc_lcto_2.
        ENDLOOP.

      ENDIF.

*    ELSEIF gwa_zglt0108-belnr IS NOT INITIAL .
*
*      LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.
*        IF gwa_zglt0108-belnr IS NOT INITIAL.
*          MOVE gwa_zglt0108-belnr TO gwa_saida-belnr.
*          MOVE ''  TO  gwa_saida-doc_lcto_est.
*        ENDIF.
*
*        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING doc_lcto_est belnr.
*      ENDLOOP.

      " ENDIF.



    ENDIF.

    IF gwa_zglt0108-belnr_2 IS  INITIAL AND gwa_zglt0108-doc_lcto_est_2 IS INITIAL.

      SELECT SINGLE *
        FROM zglt034
        INTO lwa_zglt034
       WHERE bukrs = gwa_zglt0108-investidora
         AND lote  = gwa_zglt0108-lote_2.

      CHECK sy-subrc IS INITIAL.

      SELECT SINGLE *
        FROM zib_contabil_chv
        INTO lwa_zibchv
      WHERE obj_key = gwa_zglt0108-objkey_2.

      IF ( sy-subrc IS NOT INITIAL ).

        SELECT SINGLE *
          FROM zib_contabil_err
          INTO lwa_ziberr
        WHERE obj_key = gwa_zglt0108-objkey_2.

      ENDIF.

      IF ( lwa_zibchv IS NOT INITIAL AND lwa_zibchv-belnr IS NOT INITIAL ).

        MOVE: lwa_zibchv-belnr   TO wg_doc_cont.

*        LOOP AT git_zglt0108 INTO gwa_zglt0108.
        gwa_zglt0108-belnr_2 =  lwa_zibchv-belnr.
        MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX lv_tabix TRANSPORTING belnr_2.
*        ENDLOOP.

        MODIFY zglt0108  FROM TABLE git_zglt0108.
        COMMIT WORK.

        MOVE: gwa_zglt0108-belnr_2 TO wg_doc_cont,
              gwa_zglt0108-belnr_2 TO p_doc_cont.

        LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.

          IF gwa_zglt0108-belnr_2 IS NOT INITIAL.
            MOVE gwa_zglt0108-belnr_2  TO  gwa_saida-belnr_2.
            MOVE: '@01@' TO gwa_saida-icons.
          ELSE.
            MOVE: '@02@' TO gwa_saida-icons.
            MOVE gwa_zglt0108-belnr_2  TO  gwa_saida-belnr_2.
          ENDIF.

          MOVE: gwa_zglt0108-doc_lcto   TO gwa_saida-doc_lcto, " RJF
                gwa_zglt0108-doc_lcto_2 TO gwa_saida-doc_lcto_2.

          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons belnr_2 doc_lcto doc_lcto_2.
        ENDLOOP.

      ELSEIF ( lwa_ziberr IS NOT INITIAL ).

        LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.

          IF gwa_zglt0108-belnr_2 IS NOT INITIAL.
            MOVE: '@01@' TO gwa_saida-icons.
            MOVE gwa_zglt0108-belnr_2  TO  gwa_saida-belnr_2.
          ELSE.
            MOVE: '@02@' TO gwa_saida-icons.
            MOVE gwa_zglt0108-belnr_2  TO  gwa_saida-belnr_2.
          ENDIF.

          MOVE: gwa_zglt0108-doc_lcto   TO gwa_saida-doc_lcto, " RJF
                gwa_zglt0108-doc_lcto_2 TO gwa_saida-doc_lcto_2.

          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons belnr_2 doc_lcto doc_lcto_2.
        ENDLOOP.

      ENDIF.

      IF gwa_zglt0108-belnr IS NOT INITIAL OR gwa_zglt0108-belnr_2 IS NOT INITIAL .
        LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.
          IF gwa_zglt0108-belnr IS NOT INITIAL.
            MOVE ''  TO  gwa_saida-doc_lcto_est.
          ENDIF.
          IF gwa_zglt0108-belnr_2 IS NOT INITIAL.
            MOVE ''  TO  gwa_saida-doc_lcto_est_2.
          ENDIF.


          MOVE: gwa_zglt0108-doc_lcto   TO gwa_saida-doc_lcto, " RJF
                gwa_zglt0108-doc_lcto_2 TO gwa_saida-doc_lcto_2.

          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING doc_lcto_est doc_lcto_est_2 doc_lcto doc_lcto_2.
        ENDLOOP.

      ENDIF.

*    ELSEIF gwa_zglt0108-belnr_2 IS NOT INITIAL .
*
*      LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.
*        IF gwa_zglt0108-belnr_2 IS NOT INITIAL.
*          MOVE gwa_zglt0108-belnr_2 TO gwa_saida-belnr_2.
*          MOVE ''  TO  gwa_saida-doc_lcto_est_2.
*        ENDIF.
*
*        MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING doc_lcto_est_2 belnr_2.
*      ENDLOOP.

    ENDIF.

  ENDLOOP.

*  LOOP AT git_zglt0108 INTO gwa_zglt0108.
*    DATA(lv_tabix) = sy-tabix.
*
*    IF gwa_zglt0108-belnr IS INITIAL AND gwa_zglt0108-doc_lcto_est IS INITIAL.
*
*      SELECT SINGLE *
*        FROM zglt034
*        INTO lwa_zglt034
*       WHERE bukrs = gwa_zglt0108-investidora
*         AND lote  = gwa_zglt0108-lote.
*
*      CHECK sy-subrc IS INITIAL.
*
*      SELECT SINGLE *
*        FROM zib_contabil_chv
*        INTO lwa_zibchv
*      WHERE obj_key = gwa_zglt0108-objkey.
*
*      IF ( sy-subrc IS NOT INITIAL ).
*
*        SELECT SINGLE *
*          FROM zib_contabil_err
*          INTO lwa_ziberr
*        WHERE obj_key = gwa_zglt0108-objkey.
*
*      ENDIF.
*
*      IF ( lwa_zibchv IS NOT INITIAL ).
*
*        MOVE: lwa_zibchv-belnr   TO wg_doc_cont.
*
**        LOOP AT git_zglt0108 INTO gwa_zglt0108.
*        gwa_zglt0108-belnr =  lwa_zibchv-belnr.
*        MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX lv_tabix TRANSPORTING belnr.
**        ENDLOOP.
*
*        MODIFY zglt0108  FROM TABLE git_zglt0108.
*        COMMIT WORK.
*
*        MOVE: gwa_zglt0108-belnr TO wg_doc_cont,
*              gwa_zglt0108-belnr TO p_doc_cont.
*
*        LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.
*          MOVE: '@01@' TO gwa_saida-icons.
*
*          IF gwa_zglt0108-belnr IS NOT INITIAL.
*            MOVE gwa_zglt0108-belnr  TO  gwa_saida-belnr.
*          ENDIF.
*
*          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
*        ENDLOOP.
*
*      ELSEIF ( lwa_ziberr IS NOT INITIAL ).
*
*        LOOP AT git_saida INTO gwa_saida.
*          MOVE: '@02@' TO gwa_saida-icons.
*
*          IF gwa_zglt0108-belnr IS NOT INITIAL.
*            MOVE gwa_zglt0108-belnr  TO  gwa_saida-belnr.
*          ENDIF.
*
*          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
*        ENDLOOP.
*
*      ENDIF.
*
*    ENDIF.
*
*
*    IF gwa_zglt0108-belnr_2 IS INITIAL AND gwa_zglt0108-doc_lcto_est_2 IS INITIAL.
*
*      SELECT SINGLE *
*        FROM zglt034
*        INTO lwa_zglt034
*       WHERE bukrs = gwa_zglt0108-investidora
*         AND lote  = gwa_zglt0108-lote_2.
*
*      CHECK sy-subrc IS INITIAL.
*
*      SELECT SINGLE *
*        FROM zib_contabil_chv
*        INTO lwa_zibchv
*      WHERE obj_key = gwa_zglt0108-objkey_2.
*
*      IF ( sy-subrc IS NOT INITIAL ).
*
*        SELECT SINGLE *
*          FROM zib_contabil_err
*          INTO lwa_ziberr
*        WHERE obj_key = gwa_zglt0108-objkey_2.
*
*      ENDIF.
*
*      IF ( lwa_zibchv IS NOT INITIAL ).
*
*        MOVE: lwa_zibchv-belnr   TO wg_doc_cont.
*
**        LOOP AT git_zglt0108 INTO gwa_zglt0108.
*        gwa_zglt0108-belnr_2 =  lwa_zibchv-belnr.
*        MODIFY git_zglt0108 FROM gwa_zglt0108 INDEX lv_tabix TRANSPORTING belnr_2.
**        ENDLOOP.
*
*        MODIFY zglt0108  FROM TABLE git_zglt0108.
*        COMMIT WORK.
*
*        MOVE: gwa_zglt0108-belnr_2 TO wg_doc_cont,
*              gwa_zglt0108-belnr_2 TO p_doc_cont.
*
*        LOOP AT git_saida INTO gwa_saida WHERE id_mov_pl EQ gwa_zglt0108-item_balanco.
*          MOVE: '@01@' TO gwa_saida-icons.
*
*          IF gwa_zglt0108-belnr_2 IS NOT INITIAL.
*            MOVE gwa_zglt0108-belnr_2  TO  gwa_saida-belnr_2.
*          ENDIF.
*
*          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
*        ENDLOOP.
*
*      ELSEIF ( lwa_ziberr IS NOT INITIAL ).
*
*        LOOP AT git_saida INTO gwa_saida.
*          MOVE: '@02@' TO gwa_saida-icons.
*
*          IF gwa_zglt0108-belnr_2 IS NOT INITIAL.
*            MOVE gwa_zglt0108-belnr_2  TO  gwa_saida-belnr_2.
*          ENDIF.
*
*          MODIFY git_saida FROM gwa_saida INDEX sy-tabix TRANSPORTING icons.
*        ENDLOOP.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDLOOP.

  CALL METHOD grid1->refresh_table_display
    EXPORTING
      is_stable = gwa_stable.

  IF git_saida[] IS NOT INITIAL.
    CALL SCREEN 0100.
    CALL METHOD grid1->refresh_table_display
      EXPORTING
        is_stable = gwa_stable.
  ENDIF.


ENDFORM.

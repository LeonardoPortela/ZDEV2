*/===========================================================================\*
*|      db      `7MMM.     ,MMF'      db       .g8"""bgd    .g8"""bgd `7MMF' |*
*|     ;MM:       MMMb    dPMM       ;MM:    .dP'     `M  .dP'     `M   MM   |*
*|    ,V^MM.      M YM   ,M MM      ,V^MM.   dM'       `  dM'       `   MM   |*
*|   ,M  `MM      M  Mb  M' MM     ,M  `MM   MM           MM            MM   |*
*|   AbmmmqMA     M  YM.P'  MM     AbmmmqMA  MM.    `7MMF'MM.    `7MMF' MM   |*
*|  A'     VML    M  `YM'   MM    A'     VML `Mb.     MM  `Mb.     MM   MM   |*
*| AMA.   .AMMA..JML. `'  .JMML..AMA.   .AMMA. `"bmmmdPY    `"bmmmdPY .JMML. |*
*/===========================================================================\*

*/===========================================================================\*
*|  Desenvolvedor:                                                           |*
*|    + Rodrigo Carvalho ( rodrigo.sa@amaggi.com.br)                         |*
*|                                                                           |*
*|  Tester:                                                                  |*
*|    + Carolini Santos ( carolini.santos@amaggi.com.br )                    |*
*|  Changelog:                                                               |*
*|                                                                           |*
*/===========================================================================\*

*/===========================================================================\*
*| Descrição:                                                                |*
*| Cockpit composição conta razão                                            |*
*/===========================================================================\*
REPORT zfir0101.

**********************************************************************
* tabelas
**********************************************************************
TABLES: t001, kna1, lfa1, acdoca, icon, zfit0208.

**********************************************************************
* includes
**********************************************************************
INCLUDE <icon>.
INCLUDE zfir0101_top.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS: s_bukrs  FOR t001-bukrs,
                  s_kunnr  FOR kna1-kunnr MODIF ID cli,
                  s_lifnr  FOR lfa1-lifnr MODIF ID for,
*                  s_saknr  FOR zfit0208-saknr MATCHCODE OBJECT zshfi_conta_razao,
                  s_saknr  FOR acdoca-racct, "MATCHCODE OBJECT zshfi_conta_razao,
                  s_dtp    FOR acdoca-augdt NO-EXTENSION NO INTERVALS.
SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE TEXT-002.
  PARAMETERS: p_cli RADIOBUTTON GROUP gr1 USER-COMMAND onn DEFAULT 'X',
              p_for RADIOBUTTON GROUP gr1.
SELECTION-SCREEN END OF BLOCK b0.

**********************************************************************
*SELECTION-SCREEN
**********************************************************************
AT SELECTION-SCREEN.

  FREE: l_leave.
  v_data_p = ( s_dtp-low ). "BUG - 178687 - CBRAND

  CASE sy-ucomm.
    WHEN 'BT001'.
      v_p_db_tab = 'ZFIT0208'.
      v_p_stcnam = 'ZFIT0208_OUT'.
      v_p_scmant = '0227'.
      v_p_title = 'Parâmetros de seleção cockpit composição conta razão'.

    WHEN 'BT002'.
      v_p_db_tab = 'ZFIT0220'.
      v_p_stcnam = 'ZFIT0220_OUT'.
      v_p_scmant = '0274'.
      v_p_title  = 'Parâmetros de seleção cockpit composição CODIGO OPERAÇÃO'.

    WHEN 'BT003'.
      v_p_db_tab = 'ZFIT0234'.
      v_p_stcnam = 'ZFIT0234_OUT'.
      v_p_scmant = '0319'.
      v_p_title  = 'Parâmetros de seleção cockpit composição CODIGO SUB-OPERAÇÃO'.

    WHEN 'BT004'.
      v_p_db_tab = 'ZFIT0235'.
      v_p_stcnam = 'ZFIT0235_OUT'.
      v_p_scmant = '0320'.
      v_p_title  = 'Parametrizar Sub-Operações'.

    WHEN 'ONLI'.
      "PERFORM f_start_selection.
    WHEN 'VARIANT'.
      PERFORM carrega_variantes.
    WHEN 'BACK' OR 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.
  ENDCASE.

  IF v_p_db_tab IS NOT INITIAL.
    l_opcao = '1'.
    SUBMIT zregister_data       WITH p_db_tab = v_p_db_tab
                                WITH p_stcnam = v_p_stcnam
                                WITH p_scmant = v_p_scmant
                                WITH p_title  = v_p_title
                                WITH p_maxdel = 10000
    AND RETURN.
    CLEAR: v_p_db_tab, v_p_stcnam, v_p_scmant, v_p_title.
  ENDIF.

**********************************************************************
*SELECTION-SCREEN output
**********************************************************************
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF p_cli EQ abap_true.
      CLEAR s_lifnr[].
      IF screen-group1 EQ 'CLI'.
        screen-active = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 EQ 'FOR'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_for EQ abap_true.
      CLEAR s_kunnr[].
      IF screen-group1 EQ 'FOR'.
        screen-active = 1.
        MODIFY SCREEN.
      ELSEIF screen-group1 EQ 'CLI'.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF l_opcao = 1.
    FREE MEMORY ID 'ZFIR0101'.
    l_leave = 'LEAVE'.
    EXPORT l_leave FROM l_leave TO MEMORY ID 'ZFIR0101'.
    l_opcao = '1'.
  ENDIF.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.

  SET PF-STATUS '1000'.
  FREE MEMORY ID 'ZFIR0101'.
  l_leave = 'LEAVE'.
  EXPORT l_leave FROM l_leave TO MEMORY ID 'ZFIR0101'.
  l_opcao = '1'.

**********************************************************************
* START
**********************************************************************
START-OF-SELECTION.
  PERFORM f_selecao_dados.
  PERFORM f_processa_dados.
  PERFORM f_exibir_dados.

**********************************************************************
* classes / implementacoes
**********************************************************************
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_column_id e_row_id es_row_no.

    CLASS-METHODS:
      on_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm ,

      data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

ENDCLASS.
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
**<<<------"189660 - NMS - INI------>>>
    ty_toolbar-icon      = icon_change.
    ty_toolbar-function  = 'EDITAR'.
    ty_toolbar-disabled  = ''.
    ty_toolbar-text      = TEXT-005. "Editar
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CHECK vg_edit IS NOT INITIAL.
**<<<------"189660 - NMS - FIM------>>>
    ty_toolbar-icon      = icon_copy_object.
    ty_toolbar-function  = 'TEXTO'.
    ty_toolbar-disabled  = ''.
    ty_toolbar-text      = TEXT-004.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

*    CALL REORGANIZE METHOD OF TOOLBAR MANAGER TO
*    DISPLAY THE TOOLBAR
*    CALL METHOD c_alv_toolbarmanager->reorganize
*      EXPORTING
*        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.
    DATA: wa_sel_rows TYPE lvc_s_row.

    FREE: t_rows[].

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = t_rows.

    CASE e_ucomm.

      WHEN 'TEXTO'.
        READ TABLE t_rows INTO wa_sel_rows INDEX 1.
        READ TABLE t_alv_saida INTO wa_alv_saida INDEX wa_sel_rows-index.
        IF NOT wa_alv_saida-historico IS INITIAL.
          DATA(v_historico) = wa_alv_saida-historico.
          LOOP AT t_alv_saida INTO wa_alv_saida WHERE historico IS INITIAL.
            wa_alv_saida-historico = v_historico.
            MODIFY t_alv_saida FROM wa_alv_saida INDEX sy-tabix TRANSPORTING historico.
          ENDLOOP.
        ENDIF.

        g_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).
        g_grid->register_edit_event( cl_gui_alv_grid=>mc_evt_modified ).
**<<<------"189660 - NMS - INI------>>>
      WHEN 'EDITAR'.
        vg_edit = COND #( WHEN vg_edit IS INITIAL THEN 1 ELSE 0 ).
*** Método de controle de Edição do ALV
        CALL METHOD g_grid->set_ready_for_input( EXPORTING i_ready_for_input = vg_edit ).
**<<<------"189660 - NMS - FIM------>>>
    ENDCASE.

*** Método de atualização de dados na Tela
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.

    CALL METHOD cl_gui_cfw=>flush.


*    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
*      EXPORTING
*        functioncode           = '=ENT'
*      EXCEPTIONS
*        function_not_supported = 1
*        OTHERS                 = 2.

  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_alv_toolbar IMPLEMENTATION


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_data_changed.

  ENDMETHOD.

  METHOD on_hotspot_click.

    READ TABLE t_alv_saida INTO wa_alv_saida INDEX e_row_id-index.
    IF sy-subrc = 0.
      CASE e_column_id .
        WHEN 'ANEXO'.
          PERFORM f_anexa_visualiza_arquivo.
        WHEN 'HISTORICO'.
          PERFORM f_anexa_visualiza_nota.
        WHEN 'BELNR'. "145997 -  DEVK9A20VP - FI - CS2022000723 - Ajuste - RSA
          SET PARAMETER ID 'BLN' FIELD wa_alv_saida-belnr.
          SET PARAMETER ID 'BUK' FIELD wa_alv_saida-rbukrs.
          SET PARAMETER ID 'GJR' FIELD wa_alv_saida-gjahr.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDCASE.
    ENDIF.


  ENDMETHOD.

  METHOD data_changed_finished.

    DATA: wa_good_cells TYPE lvc_s_modi.
    DATA: v_tabix TYPE sy-tabix.


    FIELD-SYMBOLS: <w_saida>  TYPE ty_alv_saida.

    "CLEAR: it_sel_rows[], wa_sel_rows.

*    CALL METHOD g_grid->get_selected_rows
*      IMPORTING
*        et_index_rows = it_sel_rows.


*    LOOP AT et_good_cells INTO wa_good_cells.
*
*      READ TABLE t_alv_saida ASSIGNING <w_saida> INDEX wa_good_cells-row_id.
*
*
*    ENDLOOP.
*
*    IF et_good_cells[] IS NOT INITIAL.
*
*      CALL METHOD g_grid->refresh_table_display
*        EXPORTING
*          is_stable = w_stable.
*    ENDIF.



  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*& Form f_exibir_dados
*&---------------------------------------------------------------------*
FORM f_exibir_dados .

  CALL SCREEN 100.

ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  SET PF-STATUS 'ZFIR0101'.
  SET TITLEBAR  'ZFIR0101'.

  PERFORM f_init_alv.

  CALL METHOD cl_gui_cfw=>flush.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_init_alv
*&---------------------------------------------------------------------*
FORM f_init_alv .

*  DATA: wl_layout TYPE slis_layout_alv.   "<<<------"189660 - NMS ------->>>
  DATA:
    p_text      TYPE sdydo_text_element,
    filtros	    TYPE zif_screen_linha_filtro,
    i_filtros	  TYPE zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) TYPE c,
    v_uzeit(10) TYPE c,
    v_dtpar(10) TYPE c.

  CREATE OBJECT obj_dyndoc_id
    EXPORTING
      no_margins = abap_true.

  PERFORM f_fieldcatalog.

  variante = VALUE #( report  = sy-repid                                                             "<<<------"189660 - NMS ------->>>
                      variant = COND #( WHEN abap_on EQ p_cli THEN '/CLIENTES' ELSE '/DIVERSOS' ) ). "<<<------"189660 - NMS ------->>>

  "PERFORM zf_preenche_cell_color.

  IF g_grid IS INITIAL.

    CLEAR: i_filtros.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04)    INTO v_datum.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02)    INTO v_uzeit.
    CONCATENATE s_dtp-low+06(02) '/' s_dtp-low+04(02) '/' s_dtp-low(04) INTO v_dtpar. ""BUG - 184687 - CBRAND
    DESCRIBE TABLE t_alv_saida LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'Data:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros.
    APPEND VALUE #( parametro = 'Data partida em aberto:' valor = v_dtpar ) TO i_filtros. "BUG - 184687 - CBRAND

    p_text = 'Relatório Composição Conta Razão'.    "<<<------"189660 - NMS ------->>>

  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
        i_titulo  = CONV #( p_text )
        i_filtros = i_filtros
      CHANGING
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.

    CREATE OBJECT obg_toolbar
      EXPORTING
        io_alv_grid = g_grid.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.

    SET HANDLER obg_toolbar->on_toolbar FOR g_grid.
    SET HANDLER obg_toolbar->handle_user_command FOR g_grid.

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
*    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.         "<<<------"189660 - NMS ------->>>
*    SET HANDLER lcl_event_handler=>data_changed_finished FOR g_grid.    "<<<------"189660 - NMS ------->>>

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
**<<<------"189660 - NMS - INI------>>>
*    variante = VALUE #( report = sy-repid ).
*
*    CLEAR: w_layout.
*    w_layout = VALUE #(
*                        zebra      = abap_true
*                        no_rowins  = abap_true
*                        info_fname = 'COLOR'
*                        sel_mode   = 'C'
*                       ).
    CLEAR w_layout.
    w_layout-sel_mode = 'A'.
    w_layout-zebra    = abap_true.
    w_layout-col_opt  = abap_true.
**<<<------"189660 - NMS - FIM------>>>


    LOOP AT t_alv_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = <fs_saida>-racct
        IMPORTING
          output = <fs_saida>-racct.

    ENDLOOP.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout
        i_save               = 'A'
*       i_default            = 'X'           "<<<------"189660 - NMS ------->>>
        it_toolbar_excluding = tl_function
        is_variant           = variante
      CHANGING
        it_outtab            = t_alv_saida[]
*       it_sort              = t_sort        "<<<------"189660 - NMS ------->>>
        it_fieldcatalog      = t_fieldcat.
**<<<------"189660 - NMS - INI------>>>
*** Método de controle de Edição do ALV
    CALL METHOD g_grid->set_ready_for_input( EXPORTING i_ready_for_input = vg_edit ).
**<<<------"189660 - NMS - FIM------>>>
    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.

  ELSE.
    CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).
  ENDIF.

*  wl_layout-colwidth_optimize = 'X'.    "<<<------"189660 - NMS ------->>>

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fieldcatalog
*&---------------------------------------------------------------------*
FORM f_fieldcatalog .

  FREE t_fieldcat[].

  CLEAR lv_cont.

  PERFORM f_estrutura_alv USING:

  01  ''   ''   'T_ALV_SAIDA'   'RBUKRS'                 'Empresa'                       '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  02  ''   ''   'T_ALV_SAIDA'   'NOME_EMPRESA'           'Nome Empresa'                  '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  03  ''   ''   'T_ALV_SAIDA'   'CLIENTE_FORNECEDOR'     'ID Conta Cliente/Fornecedor'   '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  04  ''   ''   'T_ALV_SAIDA'   'NOME_CLIENTE_FORNECEDOR' 'Nome Cliente/Fornecedor'      '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  04  ''   ''   'T_ALV_SAIDA'   'IDFIS'                  'CPF/CNPJ'                      '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',    "<<<------"189660 - NMS ------->>>
  05  ''   ''   'T_ALV_SAIDA'   'EBELN'                  'Doc. Compras'                  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  05  ''   ''   'T_ALV_SAIDA'   'BEDNR'                  'Nº acompanhamento'             '17'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',    "<<<------"189660 - NMS ------->>>
  06  ''   ''   'T_ALV_SAIDA'   'BSART'                  'Tp. Pedido'                    '10'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  07  ''   ''   'T_ALV_SAIDA'   'BATXT'                  'Desc. Tp. Pedido'              '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  08  ''   ''   'T_ALV_SAIDA'   'EKNAM'                  'Nome Comprador'                '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  09  ''   ''   'T_ALV_SAIDA'   'EINDT'                  'Data remessa pedido'           '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  10  ''   ''   'T_ALV_SAIDA'   'ZUONR'                  'Atribuição'                    '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  11  ''   ''   'T_ALV_SAIDA'   'RBUSA'                  'Divisão'                       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  12  ''   ''   'T_ALV_SAIDA'   'NOME_DIVISAO'           'Nome Divisao'                  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  13  ''   ''   'T_ALV_SAIDA'   'XBLNR'                  'Referência'                    '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  14  ''   ''   'T_ALV_SAIDA'   'BELNR'                  'Nº documento'                  '15'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  ' '  ' ',
  15  ''   ''   'T_ALV_SAIDA'   'BLDAT'                  'Data documento'                '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  16  ''   ''   'T_ALV_SAIDA'   'BUDAT'                  'Data lançamento'               '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  17  ''   ''   'T_ALV_SAIDA'   'NETDT'                  'Vencimento líquido'            '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  18  ''   ''   'T_ALV_SAIDA'   'GJAHR'                  'Exercício'                     '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  19  ''   ''   'T_ALV_SAIDA'   'ZTERM'                  'Cond. Pgto'                    '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  20  ''   ''   'T_ALV_SAIDA'   'WAERS'                  'Moeda do Documento'            '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  20  ''   ''   'T_ALV_SAIDA'   'RTCUR'                  'Moeda MI'                      '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  21  ''   ''   'T_ALV_SAIDA'   'HSL'                    'Montante em MI'                '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',"//wbarbosa 26/12/2024
  22  ''   ''   'T_ALV_SAIDA'   'RKCUR'                  'Moeda MI2'                     '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  23  ''   ''   'T_ALV_SAIDA'   'KSL'                    'Montante em MI2'               '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',"//wbarbosa 26/12/2024
  24  ''   ''   'T_ALV_SAIDA'   'KURSF'                  'Taxa Cambio Efetiva'           '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  25  ''   ''   'T_ALV_SAIDA'   'BLART'                  'Tipo de documento'             '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  26  ''   ''   'T_ALV_SAIDA'   'BSCHL'                  'Chave de lançamento'           '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  27  ''   ''   'T_ALV_SAIDA'   'UMSKZ'                  'Cód. Razão Especial'           '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  28  ''   ''   'T_ALV_SAIDA'   'SGTXT'                  'Texto'                         '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  29  ''   ''   'T_ALV_SAIDA'   'RACCT'                  'Conta do Razão'                '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  30  ''   ''   'T_ALV_SAIDA'   'TXT20'                  'Txt. descr. cta. razão'        '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  31  ''   ''   'T_ALV_SAIDA'   'AUGDT'                  'Data de compensação'           '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  32  ''   ''   'T_ALV_SAIDA'   'AUGBL'                  'Doc. compensação'              '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  33  ''   ''   'T_ALV_SAIDA'   'RCNTR'                  'Centro custo'                  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  34  ''   ''   'T_ALV_SAIDA'   'LTEXT'                  'Nome Centro de Custo'          '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  35  ''   ''   'T_ALV_SAIDA'   'PRCTR'                  'Centro de lucro'               '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  36  ''   ''   'T_ALV_SAIDA'   'ANLN1'                  'Imobilizado'                   '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  37  ''   ''   'T_ALV_SAIDA'   'KIDNO'                  'Referência de pagamento'       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  38  ''   ''   'T_ALV_SAIDA'   'XREF1'                  'Chave referência 1'            '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  39  ''   ''   'T_ALV_SAIDA'   'XREF2'                  'Chave referência 2'            '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ', "BUG - 181038 - CSB
  39  ''   ''   'T_ALV_SAIDA'   'XREF3'                  'Chave referência 3'            '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ', "BUG - 181038 - CSB
*  39  ''   ''   'T_ALV_SAIDA'   'XREF2'                  'Chave referência 3'            '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',"BUG - 181038 - CSB
  40 ''   ''   'T_ALV_SAIDA'   'ZLSPR'                  'Bloqueio pgto.'                '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  41  ''   ''   'T_ALV_SAIDA'   'VKBUR'                  'Esc Venda'                     '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  42  ''   ''   'T_ALV_SAIDA'   'BEZEI'                  'Nome Esc Venda'                '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  43  ''   ''   'T_ALV_SAIDA'   'KDAUF'                  'Ordem Venda'                   '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  44  ''   ''   'T_ALV_SAIDA'   'AUART'                  'TP. OV.'                       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  45  ''   ''   'T_ALV_SAIDA'   'DESC_OV'                'Descrição Tipo OV'             '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  46  ''   ''   'T_ALV_SAIDA'   'SAFRA'                  'Safra'                         '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  47  ''   ''   'T_ALV_SAIDA'   'CULTURA'                'Cultura'                       '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  48  ''   ''   'T_ALV_SAIDA'   'DT_FINAL_ENTREGA'       'Data final entrega'            '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  49  ''   ''   'T_ALV_SAIDA'   'RASSC'                  'Soc. parc. negócios'           '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  50  ''   ''   'T_ALV_SAIDA'   'BSTKD'                  'Texto Pedido (SD)'             '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  51  ''   ''   'T_ALV_SAIDA'   'MATNR'                  'Material'                      '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  52  ''   ''   'T_ALV_SAIDA'   'MAKTX'                  'Descrição Material'            '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  53  ''   ''   'T_ALV_SAIDA'   'WGBEZ'                  'Descrição Grp. Mercadoria'     '20'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  54  ''   ''   'T_ALV_SAIDA'   'HBKID'                  'Banco Empresa'                 '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  55  ''   ''   'T_ALV_SAIDA'   'BVTYP'                  'Tipo Banco Parceiro'           '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  56  ''   ''   'T_ALV_SAIDA'   'ZLSCH'                  'Forma Pagamento'               '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  57  ''   ''   'T_ALV_SAIDA'   'AWREF_REV'              'Estorno com'                   '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  58  ''   ''   'T_ALV_SAIDA'   'COD_DEB_CRED'           'Descrição Cód. débito/crédito' '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  59  ''   ''   'T_ALV_SAIDA'   'NATUREZA_CTO_RAZAO'     'Natureza Conta Razão'          '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  60  ''   ''   'T_ALV_SAIDA'   'AKONT'                  'Cadastro Cta.de reconciliação' '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  61  ''   ''   'T_ALV_SAIDA'   'PDD'                    'PDD'                           '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  62  ''   ''   'T_ALV_SAIDA'   'TP_OPERACAO'            'Tipo de Operação'              '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  62  ''   ''   'T_ALV_SAIDA'   'TP_SUBOP'               'Sub-Operação'                  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  63  ''   ''   'T_ALV_SAIDA'   'CLASSIFICACAO_ST'       'Classificação Status'          '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  64  ''   ''   'T_ALV_SAIDA'   'HISTORICO'              'Histórico'                     '50'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
  65  ''   ''   'T_ALV_SAIDA'   'ANEXO'                  'Anexo'                         '15'  ' '  ' '  ' '  'X'  ' '  ' '  ' '  ' '  'X'  ' ',
  66  ''   ''   'T_ALV_SAIDA'   'AGING_LIST'             'Aging List'                    '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_estrutura_alv
*&---------------------------------------------------------------------*
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "17

  ADD 1 TO lv_cont.

  CLEAR w_fieldcat.

  w_fieldcat-col_pos     = lv_cont.
*  w_fieldcat-row_pos     = lv_cont.     "<<<------"189660 - NMS ------->>>

  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = CONV #( p_scrtext_l ).
  w_fieldcat-scrtext_s   = CONV #( p_scrtext_l ).
  w_fieldcat-scrtext_m   = CONV #( p_scrtext_l ).
*  w_fieldcat-scrtext_l   = CONV #( p_scrtext_l ).    "<<<------"189660 - NMS ------->>>
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

*  FREE: t_rows[].
*
*  CALL METHOD g_grid->get_selected_rows
*    IMPORTING
*      et_index_rows = t_rows.

*  IF t_rows[] IS INITIAL.
*    MESSAGE s068(poc_main) DISPLAY LIKE 'E'.
*  ENDIF.

  CASE ok_code.
    WHEN '&F03' OR '&F15' OR '&F12'.
      LEAVE TO SCREEN 0.
    WHEN 'SAVE'.
      PERFORM grava_historico.
  ENDCASE.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

  FREE ok_code.


ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_selecao_dados
*&---------------------------------------------------------------------*
FORM f_selecao_dados .

*** BUG - 171955 - Inicio - CBRAND
  DATA: text_saknr TYPE TABLE OF string.

  " Selecionar Parâmetros de seleção cockpit composição conta razão
  "BUG 181038 - CBRAND
  "SELECT cod_operacao saknr bschl blart matkl bsart kostl lifnr drcrk gkont bukrs desc_cod_operacao desc_saknr atribuicao pdd
  SELECT cod_operacao saknr bschl blart matkl bsart lifnr gkont desc_cod_operacao desc_saknr atribuicao pdd anln1 sgtxt kidno xref1
         FROM zfit0208
         INTO TABLE t_zfit0208.

  FREE text_saknr.
  LOOP AT t_zfit0208 INTO DATA(lw_zfit0208) WHERE saknr IS NOT INITIAL.
    SPLIT lw_zfit0208-saknr  AT ',' INTO TABLE text_saknr.
    LOOP AT text_saknr INTO DATA(ls_saknr).
      lw_zfit0208-saknr = ls_saknr.
      CONDENSE lw_zfit0208-saknr NO-GAPS.
      APPEND lw_zfit0208 TO t_zfit0208_class.
      CLEAR: ls_saknr.
    ENDLOOP.
  ENDLOOP.

*** Bug - 181038 - CBRAND - Inicio
  SELECT cod_subop
         saknr
         blart
         des_subop
         desc_saknr
         sgtxt
         kidno
         xref1
         atribuicao
       FROM zfit0235
    INTO TABLE t_zfit0235.

  FREE text_saknr.
  LOOP AT t_zfit0235 INTO DATA(lw_zfit0235) WHERE saknr IS NOT INITIAL.
    SPLIT lw_zfit0235-saknr  AT ',' INTO TABLE text_saknr.
    LOOP AT text_saknr INTO ls_saknr.
      lw_zfit0235-saknr = ls_saknr.
      APPEND lw_zfit0235 TO t_zfit0235_class.
      CLEAR: ls_saknr.
    ENDLOOP.
  ENDLOOP.
*** Bug - 181038 - CBRAND - Fim
*** BUG - 171955 - Fim - CBRAND

  "//WSB RET
  IF s_bukrs[] IS INITIAL.
*    MESSAGE s131(zfi) WITH 'Preencher campo Obrigatório Empresa' DISPLAY LIKE 'E'.
*    STOP.
  ENDIF.
  "//WSB RET
  IF s_dtp[] IS INITIAL.
*    MESSAGE s131(zfi) WITH 'Preencher campo Obrigatório Data partida em aberto' DISPLAY LIKE 'E'.
*    STOP.
    s_dtp-low = sy-datum.             " 169346 - 19.03.2025
  ENDIF.

  IF s_saknr IS INITIAL.
    PERFORM f_seleciona_classificacao.
  ELSE.                               " 169346 - 19.03.2025
    rg_racct[] = s_saknr[].           " 169346 - 19.03.2025
  ENDIF.                              " 169346 - 19.03.2025

*  CHECK rg_racct IS NOT INITIAL.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF p_cli IS NOT INITIAL.
*** BUG - 178687 - CBRAND - Inicio
    FREE rg_fornecedor.
    LOOP AT s_kunnr.
      wa_fornecedor-sign   = 'I'.
      wa_fornecedor-option = 'EQ'.
      wa_fornecedor-low    =  s_kunnr-low.
      APPEND wa_fornecedor TO rg_fornecedor.
    ENDLOOP.
*** BUG - 178687 - CBRAND - Fim

*** BUG - 181038 - Inicio - CBRAND
    gva_koart = 'D'.
    gva_shkzg = 'H'.
*** BUG - 181038 - Fim - CBRAND

*"// INICIO WBARBOSA BUG-160850 13/12/2024 remover implementação da Partida MEMO
*    SELECT ad~bukrs ad~gjahr ad~belnr ad~kunnr ad~zuonr ad~gsber ad~bldat ad~infae ad~budat
*           ad~waers ad~blart ad~bschl ad~umskz
*           ad~sgtxt ad~augdt ad~augbl
*           ad~kostl ad~prctr ad~anln1
*           ad~vbel2 ad~saknr ad~shkzg
*           ad~hbkid ad~xstov ad~vbeln ad~zfbdt
*           ad~hkont ad~vbund
*      FROM bkpf AS bf
*      INNER JOIN bsad AS ad
*      ON bf~bukrs EQ ad~bukrs
*      AND bf~belnr EQ ad~belnr
*      AND bf~gjahr EQ ad~gjahr
*      INTO TABLE it_bsad_id
*      WHERE bf~bstat EQ 'S'
*      AND   ad~bukrs  IN s_bukrs
*      AND   ad~kunnr  IN s_kunnr
*      AND   ad~hkont  IN rg_racct
*      AND   ad~augdt  GT s_dtp-low.
*
*
*    SELECT id~bukrs id~gjahr id~belnr id~kunnr id~zuonr id~gsber id~bldat id~infae id~budat
*           id~waers id~blart id~bschl id~umskz
*           id~sgtxt id~augdt id~augbl
*           id~kostl id~prctr id~anln1
*           id~vbel2 id~saknr id~shkzg
*           id~hbkid id~xstov id~vbeln id~zfbdt
*           id~hkont id~vbund
*           id~dmbtr id~wrbtr id~fcsl id~rfccur
*      FROM bkpf AS bf
*      INNER JOIN bsid AS id
*      ON bf~bukrs EQ id~bukrs
*      AND bf~belnr EQ id~belnr
*      AND bf~gjahr EQ id~gjahr
*      APPENDING TABLE it_bsad_id
*      WHERE bf~bstat EQ 'S'
*      AND   id~bukrs  IN s_bukrs
*      AND   id~kunnr  IN s_kunnr
*      AND   id~hkont  IN rg_racct.
*
*    IF it_bsad_id IS NOT INITIAL.
*      SORT it_bsad_id BY bukrs gjahr belnr.
*    ENDIF.
*"// FIM WBARBOSA BUG-160850 13/12/2024 remover implementação da Partida MEMO

    " selecionar partidas de clientes na acdoca
    SELECT rbukrs gjahr belnr awref_rev rtcur rwcur rkcur racct rcntr prctr rbusa rassc hsl ksl drcrk budat bldat blart buzei zuonr
           bschl ebeln ebelp sgtxt kdauf matnr werks lifnr kunnr umskz hbkid augdt augbl anln1 gkont netdt
       FROM acdoca
       INTO TABLE t_acdoca
       WHERE rldnr  EQ '0L'
       AND   rbukrs IN s_bukrs
       AND   racct  IN rg_racct
       AND   rcntr  IN rg_rcntr
       AND   drcrk  IN rg_drcrk
       AND   budat  <= s_dtp-low   "CSSANTOS
       AND   blart  IN rg_blart
       AND   koart  EQ gva_koart    "<<<------"189660 - NMS ------->>>
       AND   zuonr  IN rg_zuonr
       AND   bschl  IN rg_bschl
       AND   kunnr  IN rg_fornecedor
       AND   gkont  IN rg_gkont
*       AND   augbl  EQ '00000000' "// WBARBOSA 18/12/2024 US-83808
       AND   augbl  EQ abap_false. "// WBARBOSA 18/12/2024 US-83808

    SELECT rbukrs gjahr belnr awref_rev rtcur rwcur rkcur racct rcntr prctr rbusa rassc hsl ksl drcrk budat bldat blart buzei zuonr
           bschl ebeln ebelp sgtxt kdauf matnr werks lifnr kunnr umskz hbkid augdt augbl anln1 gkont netdt
      FROM acdoca
      APPENDING TABLE t_acdoca
      WHERE rldnr  EQ '0L'
      AND   rbukrs IN s_bukrs
      AND   racct  IN rg_racct
      AND   rcntr  IN rg_rcntr
      AND   drcrk  IN rg_drcrk
      AND   budat  <= s_dtp-low   "CSSANTOS
      AND   blart  IN rg_blart
      AND   koart  EQ gva_koart    "<<<------"189660 - NMS ------->>>
      AND   zuonr  IN rg_zuonr
      AND   bschl  IN rg_bschl
      AND   kunnr  IN rg_fornecedor
      AND   gkont  IN rg_gkont
*      AND   augbl  <> '00000000' "// WBARBOSA 18/12/2024 US-83808
      AND   augbl  NE abap_false  "// WBARBOSA 18/12/2024 US-83808
      AND   augdt  > s_dtp-low .

  ENDIF.

  IF p_for IS NOT INITIAL.
*** BUG - 178687 - CBRAND - Inicio
    FREE rg_fornecedor.
    LOOP AT s_lifnr.
      wa_fornecedor-sign   = 'I'.
      wa_fornecedor-option = 'EQ'.
      wa_fornecedor-low    = s_lifnr-low.
      APPEND wa_fornecedor TO rg_fornecedor.
    ENDLOOP.
*** BUG - 178687 - CBRAND - Fim

*** BUG - 181038 - Inicio - CBRAND
    gva_koart = 'K'.
    gva_shkzg = 'S'.
*** BUG - 181038 - Fim - CBRAND

*"// INICIO WBARBOSA BUG-160850 13/12/2024 remover implementação da Partida MEMO
*    SELECT ak~bukrs ak~gjahr ak~belnr ak~lifnr ak~zuonr
*           ak~gsber ak~bldat ak~ebeln ak~ebelp ak~budat
*           ak~waers ak~blart ak~bschl ak~umskz
*           ak~sgtxt ak~augdt ak~augbl
*           ak~kostl ak~prctr ak~anln1
*           ak~saknr ak~shkzg
*           ak~hbkid ak~xstov ak~zfbdt
*           ak~hkont ak~vbund
*      FROM bkpf AS bf
*      INNER JOIN bsak AS ak
*      ON bf~bukrs EQ ak~bukrs
*      AND bf~belnr EQ ak~belnr
*      AND bf~gjahr EQ ak~gjahr
*      INTO TABLE it_bsak_ik
*      WHERE bf~bstat  EQ 'S'
*      AND   ak~bukrs  IN s_bukrs
*      AND   ak~lifnr  IN rg_fornecedor
*      AND   ak~hkont  IN rg_racct
*      AND   ak~augdt  GT s_dtp-low.
*
*    SELECT ak~bukrs ak~gjahr ak~belnr ak~lifnr ak~zuonr
*           ak~gsber ak~bldat ak~ebeln ak~ebelp ak~budat
*           ak~waers ak~blart ak~bschl ak~umskz
*           ak~sgtxt ak~augdt ak~augbl
*           ak~kostl ak~prctr ak~anln1
*           ak~saknr ak~shkzg
*           ak~hbkid ak~xstov ak~zfbdt
*           ak~hkont ak~vbund
*           ak~dmbtr ak~wrbtr ak~fcsl ak~rfccur
*      FROM bkpf AS bf
*      INNER JOIN bsik AS ak
*      ON bf~bukrs EQ ak~bukrs
*      AND bf~belnr EQ ak~belnr
*      AND bf~gjahr EQ ak~gjahr
*      APPENDING TABLE it_bsak_ik
*      WHERE bf~bstat  EQ 'S'
*      AND   ak~bukrs  IN s_bukrs
*      AND   ak~lifnr  IN rg_fornecedor
*      AND   ak~hkont  IN rg_racct.
*
*    IF it_bsak_ik IS NOT INITIAL.
*      SORT it_bsak_ik BY bukrs gjahr belnr.
*    ENDIF.
*"// FIM WBARBOSA BUG-160850 13/12/2024 remover implementação da Partida MEMO

*    " selecionar partidas de fornecedores na acdoca
    SELECT rbukrs gjahr belnr awref_rev rtcur rwcur rkcur racct rcntr prctr rbusa rassc hsl ksl drcrk budat bldat blart buzei zuonr
           bschl ebeln ebelp sgtxt kdauf matnr werks lifnr kunnr umskz hbkid augdt augbl anln1 gkont netdt
       FROM acdoca
    INTO TABLE t_acdoca
       WHERE rldnr  EQ '0L'
         AND rbukrs IN s_bukrs
         AND racct  IN rg_racct
         AND rcntr  IN rg_rcntr
         AND drcrk  IN rg_drcrk
         AND budat  <= s_dtp-low   "CSSANTOS
         AND blart  IN rg_blart
         AND koart  EQ gva_koart    "<<<------"189660 - NMS ------->>>
         AND zuonr  IN rg_zuonr
         AND bschl  IN rg_bschl
         AND lifnr  IN rg_fornecedor
         AND gkont  IN rg_gkont
*         AND ( augbl   = '00000000' ) "// WBARBOSA 18/12/2024 US-83808
         AND augbl  EQ abap_false.  "// WBARBOSA 18/12/2024 US-83808


    SELECT rbukrs gjahr belnr awref_rev rtcur rwcur rkcur racct rcntr prctr rbusa rassc hsl ksl drcrk budat bldat blart buzei zuonr
           bschl ebeln ebelp sgtxt kdauf matnr werks lifnr kunnr umskz hbkid augdt augbl anln1 gkont netdt
      FROM acdoca
    APPENDING TABLE t_acdoca
       WHERE rldnr  EQ '0L'
         AND rbukrs IN s_bukrs
         AND racct  IN rg_racct
         AND rcntr  IN rg_rcntr
         AND drcrk  IN rg_drcrk
         AND budat  <= s_dtp-low   "CSSANTOS
         AND blart  IN rg_blart
         AND koart  EQ gva_koart    "<<<------"189660 - NMS ------->>>
         AND zuonr  IN rg_zuonr
         AND bschl  IN rg_bschl
         AND lifnr  IN rg_fornecedor
         AND gkont  IN rg_gkont
*      AND   augbl  <> '00000000' "// WBARBOSA 18/12/2024 US-83808
         AND augbl  NE abap_false "// WBARBOSA 18/12/2024 US-83808
         AND augdt > s_dtp-low .

  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim

*    " Seleciona Universal Journal Entry Line Items
*    SELECT rbukrs gjahr belnr awref_rev rtcur rwcur rkcur racct rcntr prctr rbusa rassc tsl ksl drcrk budat bldat blart buzei zuonr
*           bschl ebeln ebelp sgtxt kdauf matnr werks lifnr kunnr umskz hbkid augdt augbl anln1 gkont netdt
*           FROM acdoca
*           INTO TABLE t_acdoca
*           WHERE rldnr  EQ '0L'
*           AND   rbukrs IN s_bukrs
*           AND   racct  IN rg_racct
*           AND   rcntr  IN rg_rcntr
*           AND   drcrk  IN rg_drcrk
*           AND   budat  <= s_dtp-low   "CSSANTOS
*           AND   blart  IN rg_blart
*           AND   zuonr  IN rg_zuonr
*           AND   bschl  IN rg_bschl
*           AND   lifnr  IN rg_fornecedor
*           AND   gkont  IN rg_gkont
*           AND   augbl   = '00000000'.
**         AND ( augdt  IN rg_augdt OR augdt IN rg_augdt_sp ). "-US 155522-23-10-2024-#155522-RJF retirado
**         AND ( augdt  IN rg_augdt OR augdt  IN rg_augdt_sp OR augdt EQ '00.00.0000' OR augdt IS NULL ). "-US 155522-23-10-2024-#155522-RJF

**-US 155522-23-10-2024-#155522-RJF-Inicio
*    " Seleciona Universal Journal Entry Line Items
*    SELECT rbukrs gjahr belnr awref_rev rtcur rwcur rkcur racct rcntr prctr rbusa rassc tsl ksl drcrk budat bldat blart buzei zuonr
*           bschl ebeln ebelp sgtxt kdauf matnr werks lifnr kunnr umskz hbkid augdt augbl anln1 gkont netdt
*           FROM acdoca
*           APPENDING TABLE t_acdoca
*           WHERE rldnr  EQ '0L'
*           AND   rbukrs IN s_bukrs
*           AND   racct  IN rg_racct
*           AND   rcntr  IN rg_rcntr
*           AND   drcrk  IN rg_drcrk
*           AND   budat  <= s_dtp-low    "CSSANTOS
*           AND   blart  IN rg_blart
*           AND   zuonr  IN rg_zuonr
*           AND   bschl  IN rg_bschl
*           AND   kunnr  IN rg_fornecedor
*           AND   gkont  IN rg_gkont
*           AND   augbl  <> '00000000'
*           AND augdt > s_dtp-low.
**         AND  ( augdt  IN rg_augdt OR augdt IN rg_augdt_sp OR augdt EQ '00.00.0000' OR augdt IS NULL ).  'CSSANTOS

** Não usa mais : it_bsad_id
*  IF t_acdoca[] IS INITIAL.
**"// INICIO WBARBOSA BUG-160850 13/12/2024 Retornando o Codigo Comentado
**  IF t_acdoca[] IS INITIAL AND it_bsad_id[] IS INITIAL AND it_bsak_ik IS INITIAL.  "// WBARBOSA BUG-160850 13/12/2024 Comentando a Condição de Partida MEMO
**-US 155522-23-10-2024-#155522-RJF-Fim
*
**    MESSAGE i005(zfi). "//WSB RET
**    STOP."//WSB RET
*  ENDIF.

  SORT t_acdoca BY rbukrs gjahr belnr.

  " Não usa mais it_bsad_id
*-US 155522-23-10-2024-#155522-RJF-Inicio
*  LOOP AT it_bsad_id INTO DATA(wa_bsad_id).
*    IF NOT wa_bsad_id-kunnr IS INITIAL.
*      wa_kunnr_c-kunnr = wa_bsad_id-kunnr.
*      APPEND wa_kunnr_c TO t_kunnr_c.
*    ENDIF.
*
*    IF NOT wa_bsad_id-kunnr IS INITIAL AND NOT wa_bsad_id-bukrs IS INITIAL.
*      wa_knb1_kunnr_bukrc-kunnr = wa_bsad_id-kunnr.
*      wa_knb1_kunnr_bukrc-bukrs = wa_bsad_id-bukrs.
*      APPEND wa_knb1_kunnr_bukrc TO t_knb1_kunnr_bukrc.
*    ENDIF.
*
**    IF NOT wa_bsad_id-lifnr IS INITIAL.
**      wa_lifnr-lifnr = wa_acdoca-lifnr.
**      APPEND wa_lifnr TO t_lifnr.
**    ENDIF.
*
**    IF NOT wa_acdoca-lifnr IS INITIAL AND NOT wa_acdoca-rbukrs IS INITIAL.
**      wa_lfb1_lifnr_bukrs-lifnr = wa_acdoca-lifnr.
**      wa_lfb1_lifnr_bukrs-bukrs = wa_acdoca-rbukrs.
**      APPEND wa_lfb1_lifnr_bukrs TO t_lfb1_lifnr_bukrs.
**    ENDIF.
*
**    IF NOT wa_acdoca-ebeln IS INITIAL.
**      wa_ekko_ebeln-ebeln = wa_acdoca-ebeln.
**      APPEND wa_ekko_ebeln TO t_ekko_ebeln.
**    ENDIF.
**
**    IF NOT wa_acdoca-ebeln IS INITIAL AND NOT wa_acdoca-ebelp IS INITIAL.
**      wa_ebeln_ebelp-ebeln = wa_acdoca-ebeln.
**      wa_ebeln_ebelp-ebelp = wa_acdoca-ebelp.
**      APPEND wa_ebeln_ebelp TO t_ebeln_ebelp.
**    ENDIF.
*
*    IF NOT wa_bsad_id-gsber IS INITIAL.
*      wa_t001w_werkc-werks = wa_bsad_id-gsber.
*      APPEND wa_t001w_werkc TO t_t001w_werkc.
*    ENDIF.
*    IF NOT wa_bsad_id-saknr IS INITIAL.
*      wa_ska1_saknc-saknr = wa_bsad_id-saknr.
*      APPEND wa_ska1_saknc TO t_ska1_saknc.
*    ENDIF.
*    IF NOT wa_bsad_id-kostl IS INITIAL.
*      wa_cskt_kostc-kostl = wa_bsad_id-kostl.
*      APPEND wa_cskt_kostc TO t_cskt_kostc.
*    ENDIF.
**    IF NOT wa_acdoca-kdauf IS INITIAL.
**      wa_vbak_vbeln-vbeln = wa_acdoca-kdauf.
**      APPEND wa_vbak_vbeln TO t_vbak_vbeln.
**    ENDIF.
**    IF NOT wa_acdoca-matnr IS INITIAL.
**      wa_mara_matnr-matnr = wa_acdoca-matnr.
**      APPEND wa_mara_matnr TO t_mara_matnr.
**    ENDIF.
*  ENDLOOP.

* ---------
  " Não usa mais it_bsak_ik
*  LOOP AT it_bsak_ik INTO DATA(wa_bsak_ik).
**    IF NOT wa_bsak_ik-kunnr IS INITIAL.
**      wa_kunnr_c-kunnr = wa_bsak_ik-kunnr.
**      APPEND wa_kunnr_c TO t_kunnr_c.
**    ENDIF.
*
**    IF NOT wa_bsad_id-kunnr IS INITIAL AND NOT wa_bsad_id-bukrs IS INITIAL.
**      wa_knb1_kunnr_bukrc-kunnr = wa_bsad_id-kunnr.
**      wa_knb1_kunnr_bukrc-bukrs = wa_bsad_id-bukrs.
**      APPEND wa_knb1_kunnr_bukrc TO t_knb1_kunnr_bukrc.
**    ENDIF.
*
*    IF NOT wa_bsak_ik-lifnr IS INITIAL.
*      wa_lifnr_f-lifnr = wa_bsak_ik-lifnr.
*      APPEND wa_lifnr_f TO t_lifnr_f.
*    ENDIF.
*
*    IF NOT wa_bsak_ik-lifnr IS INITIAL AND NOT wa_bsak_ik-bukrs IS INITIAL.
*      wa_lfb1_lifnr_bukrf-lifnr = wa_bsak_ik-lifnr.
*      wa_lfb1_lifnr_bukrf-bukrs = wa_bsak_ik-bukrs.
*      APPEND wa_lfb1_lifnr_bukrf TO t_lfb1_lifnr_bukrf.
*    ENDIF.
*
*    IF NOT wa_bsak_ik-ebeln IS INITIAL.
*      wa_ekko_ebelf-ebeln = wa_bsak_ik-ebeln.
*      APPEND wa_ekko_ebelf TO t_ekko_ebelf.
*    ENDIF.
*
*    IF NOT wa_bsak_ik-ebeln IS INITIAL AND NOT wa_bsak_ik-ebelp IS INITIAL.
*      wa_ebeln_ebelf-ebeln = wa_bsak_ik-ebeln.
*      wa_ebeln_ebelf-ebelp = wa_bsak_ik-ebelp.
*      APPEND wa_ebeln_ebelf TO t_ebeln_ebelf.
*    ENDIF.
*
*    IF NOT wa_bsak_ik-gsber IS INITIAL.
*      wa_t001w_werkc-werks = wa_bsak_ik-gsber.
*      APPEND wa_t001w_werkc TO t_t001w_werkc.
*    ENDIF.
*    IF NOT wa_bsak_ik-saknr IS INITIAL.
*      wa_ska1_saknc-saknr = wa_bsak_ik-saknr.
*      APPEND wa_ska1_saknc TO t_ska1_saknc.
*    ENDIF.
*    IF NOT wa_bsak_ik-kostl IS INITIAL.
*      wa_cskt_kostc-kostl = wa_bsak_ik-kostl.
*      APPEND wa_cskt_kostc TO t_cskt_kostc.
*    ENDIF.
**    IF NOT wa_acdoca-kdauf IS INITIAL.
**      wa_vbak_vbeln-vbeln = wa_acdoca-kdauf.
**      APPEND wa_vbak_vbeln TO t_vbak_vbeln.
**    ENDIF.
**    IF NOT wa_acdoca-matnr IS INITIAL.
**      wa_mara_matnr-matnr = wa_acdoca-matnr.
**      APPEND wa_mara_matnr TO t_mara_matnr.
**    ENDIF.
*  ENDLOOP.

  "// INICIO WBARBOSA 17/12/2024 US-83808
  IF t_acdoca[] IS NOT INITIAL.
    " Seleciona Segmento do documento contabilidade financeira
    SELECT bukrs belnr gjahr kunnr lifnr zterm zlsch zlspr bvtyp xref1 xref2 xref3 kidno matnr netdt ebeln ebelp anln1 "BUG - 181038 - ADD anln1 XREF3
           FROM bseg
           INTO CORRESPONDING FIELDS OF TABLE t_bseg
           FOR ALL ENTRIES IN t_acdoca
           WHERE bukrs EQ t_acdoca-rbukrs
             AND belnr EQ t_acdoca-belnr
             AND gjahr EQ t_acdoca-gjahr
             AND koart EQ gva_koart    "BUG - 181038 - CBRAND
             AND shkzg EQ gva_shkzg.   "BUG - 181038 - CBRAND
    "AND ebeln NE abap_false. "BUG - 181038 - CBRAND
  ENDIF.
  "// FIM WBARBOSA 17/12/2024 US-83808

*-US 155522-23-10-2024-#155522-RJF-Fim

  "// INICIO WBARBOSA 17/12/2024 US-83808
  LOOP AT t_bseg INTO wa_bseg.

    IF NOT wa_bseg-lifnr IS INITIAL.
      APPEND VALUE #( lifnr = wa_bseg-lifnr ) TO t_lifnr.
    ENDIF.

    IF NOT wa_bseg-kunnr IS INITIAL.
      APPEND VALUE #( kunnr = wa_acdoca-kunnr ) TO t_kunnr.
    ENDIF.

    IF NOT wa_bseg-ebeln IS INITIAL.
      APPEND VALUE #( ebeln = wa_bseg-ebeln ) TO t_ekko_ebeln.
    ENDIF.

    IF NOT wa_bseg-matnr IS INITIAL.
      APPEND VALUE #( matnr = wa_bseg-matnr ) TO t_mara_matnr.
    ENDIF.

    IF NOT wa_bseg-ebeln IS INITIAL AND NOT wa_bseg-ebelp IS INITIAL.
      APPEND VALUE #( ebeln = wa_bseg-ebeln ebelp = wa_bseg-ebelp ) TO t_ebeln_ebelp.
    ENDIF.

  ENDLOOP.
  "// FIM WBARBOSA 17/12/2024 US-83808

  LOOP AT t_acdoca INTO wa_acdoca.
    IF NOT wa_acdoca-kunnr IS INITIAL.
      wa_kunnr-kunnr = wa_acdoca-kunnr.
      APPEND wa_kunnr TO t_kunnr.
    ENDIF.

    IF NOT wa_acdoca-kunnr IS INITIAL AND NOT wa_acdoca-rbukrs IS INITIAL.
      wa_knb1_kunnr_bukrs-kunnr = wa_acdoca-kunnr.
      wa_knb1_kunnr_bukrs-bukrs = wa_acdoca-rbukrs.
      APPEND wa_knb1_kunnr_bukrs TO t_knb1_kunnr_bukrs.
    ENDIF.

    IF NOT wa_acdoca-lifnr IS INITIAL.
      wa_lifnr-lifnr = wa_acdoca-lifnr.
      APPEND wa_lifnr TO t_lifnr.
    ENDIF.

    IF NOT wa_acdoca-lifnr IS INITIAL AND NOT wa_acdoca-rbukrs IS INITIAL.
      wa_lfb1_lifnr_bukrs-lifnr = wa_acdoca-lifnr.
      wa_lfb1_lifnr_bukrs-bukrs = wa_acdoca-rbukrs.
      APPEND wa_lfb1_lifnr_bukrs TO t_lfb1_lifnr_bukrs.
    ENDIF.

    IF NOT wa_acdoca-ebeln IS INITIAL.
      wa_ekko_ebeln-ebeln = wa_acdoca-ebeln.
      APPEND wa_ekko_ebeln TO t_ekko_ebeln.
    ENDIF.

    IF NOT wa_acdoca-ebeln IS INITIAL AND NOT wa_acdoca-ebelp IS INITIAL.
      wa_ebeln_ebelp-ebeln = wa_acdoca-ebeln.
      wa_ebeln_ebelp-ebelp = wa_acdoca-ebelp.
      APPEND wa_ebeln_ebelp TO t_ebeln_ebelp.
    ENDIF.

    IF NOT wa_acdoca-rbusa IS INITIAL.
      wa_t001w_werks-werks = wa_acdoca-rbusa.
      APPEND wa_t001w_werks TO t_t001w_werks.
    ENDIF.
    IF NOT wa_acdoca-racct IS INITIAL.
      wa_ska1_saknr-saknr = wa_acdoca-racct.
      APPEND wa_ska1_saknr TO t_ska1_saknr.
    ENDIF.
    IF NOT wa_acdoca-rcntr IS INITIAL.
      wa_cskt_kostl-kostl = wa_acdoca-rcntr.
      APPEND wa_cskt_kostl TO t_cskt_kostl.
    ENDIF.
    IF NOT wa_acdoca-kdauf IS INITIAL.
      wa_vbak_vbeln-vbeln = wa_acdoca-kdauf.
      APPEND wa_vbak_vbeln TO t_vbak_vbeln.
    ENDIF.
    IF NOT wa_acdoca-matnr IS INITIAL.
      wa_mara_matnr-matnr = wa_acdoca-matnr.
      APPEND wa_mara_matnr TO t_mara_matnr.
    ENDIF.
  ENDLOOP.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF p_for IS NOT INITIAL OR p_cli IS NOT INITIAL.

    SORT t_kunnr_c BY kunnr.
    DELETE ADJACENT DUPLICATES FROM t_kunnr_c COMPARING kunnr.

    SORT t_knb1_kunnr_bukrc BY kunnr bukrs.
    DELETE ADJACENT DUPLICATES FROM t_knb1_kunnr_bukrc COMPARING kunnr bukrs.

    SORT t_t001w_werkc BY werks.
    DELETE ADJACENT DUPLICATES FROM t_t001w_werkc COMPARING werks.

    SORT t_ska1_saknc BY saknr.
    DELETE ADJACENT DUPLICATES FROM t_ska1_saknc COMPARING saknr.

    SORT t_cskt_kostc BY kostl.
    DELETE ADJACENT DUPLICATES FROM t_cskt_kostc COMPARING kostl.

    SORT t_lifnr_f BY lifnr.
    DELETE ADJACENT DUPLICATES FROM t_lifnr_f COMPARING lifnr.

    SORT t_lfb1_lifnr_bukrf BY lifnr bukrs.
    DELETE ADJACENT DUPLICATES FROM t_lfb1_lifnr_bukrf COMPARING lifnr bukrs.

    SORT t_ekko_ebelf BY ebeln.
    DELETE ADJACENT DUPLICATES FROM t_ekko_ebelf COMPARING ebeln.

    SORT t_ebeln_ebelf BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM t_ebeln_ebelf COMPARING ebeln ebelp.

  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim

  SORT t_kunnr BY kunnr.
  DELETE ADJACENT DUPLICATES FROM t_kunnr COMPARING kunnr.

  SORT t_knb1_kunnr_bukrs BY kunnr bukrs.
  DELETE ADJACENT DUPLICATES FROM t_knb1_kunnr_bukrs COMPARING kunnr bukrs.

  SORT t_lifnr BY lifnr.
  DELETE ADJACENT DUPLICATES FROM t_lifnr COMPARING lifnr.

  SORT t_lfb1_lifnr_bukrs BY lifnr bukrs.
  DELETE ADJACENT DUPLICATES FROM t_lfb1_lifnr_bukrs COMPARING lifnr bukrs.

  SORT t_t001w_werks BY werks.
  DELETE ADJACENT DUPLICATES FROM t_t001w_werks COMPARING werks.

  SORT t_ska1_saknr BY saknr.
  DELETE ADJACENT DUPLICATES FROM t_ska1_saknr COMPARING saknr.

  SORT t_cskt_kostl BY kostl.
  DELETE ADJACENT DUPLICATES FROM t_cskt_kostl COMPARING kostl.

  SORT t_vbak_vbeln BY vbeln.
  DELETE ADJACENT DUPLICATES FROM t_vbak_vbeln COMPARING vbeln.

  SORT t_mara_matnr BY matnr.
  DELETE ADJACENT DUPLICATES FROM t_mara_matnr COMPARING matnr.

  SORT t_ekko_ebeln BY ebeln.
  DELETE ADJACENT DUPLICATES FROM t_ekko_ebeln COMPARING ebeln.

  SORT t_ebeln_ebelp BY ebeln ebelp.
  DELETE ADJACENT DUPLICATES FROM t_ebeln_ebelp COMPARING ebeln ebelp.


*** BUG - 181038 - Inicio - CBRAND
  IF t_acdoca[] IS NOT INITIAL.

    SELECT doc_simulacao spart vbeln
       FROM zsdt0041
         INTO TABLE t_zsdt0041
      FOR ALL ENTRIES IN t_acdoca
         WHERE vbeln EQ t_acdoca-kdauf.

*    SORT t_zsdt0041 BY doc_simulacao. " Rubenilson Pereira - 25.07.25 #185551
    SORT t_zsdt0041 BY vbeln.          " Rubenilson Pereira - 25.07.25 #185551

    DELETE t_zsdt0041 WHERE vbeln IS INITIAL.
*** Inicio - Rubenilson Pereira - 25.07.25 #185551
    IF t_zsdt0041 IS NOT INITIAL.
      DATA(lt_0041) = t_zsdt0041.
      SORT lt_0041 BY doc_simulacao.
      DELETE ADJACENT DUPLICATES FROM lt_0041 COMPARING doc_simulacao.
*** Fim - Rubenilson Pereira - 25.07.25 #185551

      " Simulador de Vendas - dados de cabeçalho
**<<<------"189660 - NMS - INI------>>>
*      SELECT doc_simulacao cultura safra dt_entrega_sem dt_entrega_def dt_entrega_fet
      SELECT doc_simulacao cultura safra vkbur dt_entrega_sem dt_entrega_def dt_entrega_fet
**<<<------"189660 - NMS - FIM------>>>
          FROM zsdt0040
             INTO TABLE t_zsdt0040
          FOR ALL ENTRIES IN lt_0041
             WHERE doc_simulacao EQ lt_0041-doc_simulacao.

    ENDIF.

    SORT t_zsdt0040 BY doc_simulacao.

    SELECT doc_simulacao spart vbeln
      FROM zsdt0090
        INTO TABLE t_zsdt0090
     FOR ALL ENTRIES IN t_acdoca
        WHERE vbeln EQ t_acdoca-kdauf.

    SORT t_zsdt0090 BY doc_simulacao.
    DELETE t_zsdt0090 WHERE vbeln IS INITIAL.
**<<<------"189660 - NMS - INI------>>>
*    SELECT doc_simulacao cultura safra dt_entrega_sem dt_entrega_def dt_entrega_fet
    SELECT doc_simulacao cultura safra vkbur dt_entrega_sem dt_entrega_def dt_entrega_fet
**<<<------"189660 - NMS - FIM------>>>
        FROM zsdt0040
          APPENDING TABLE t_zsdt0040
        FOR ALL ENTRIES IN t_zsdt0090
           WHERE doc_simulacao EQ t_zsdt0090-doc_simulacao.

    SORT t_zsdt0040 BY doc_simulacao. " Rubenilson Pereira - 25.07.25 #185551
    SORT t_zsdt0090 BY vbeln.         "<<<------"189660 - NMS ------->>>
  ENDIF.

*  IF t_acdoca[] IS NOT INITIAL. "-US 155522-23-10-2024-#155522-RJF
*    " Seleciona Simulador de vendas – Pagamento Antecipado
*    SELECT adiant doc_simulacao "DEVK9A20VP - FI - CS2022000 - Composição de saldo cliente e fo #83808 RSA
*           FROM zsdt0159
*           INTO TABLE t_zsdt0159
*           FOR ALL ENTRIES IN t_acdoca
*           WHERE adiant EQ t_acdoca-belnr.
*
*    SORT t_zsdt0159 BY adiant doc_simulacao.
*
*
**-US 155522-23-10-2024-#155522-RJF-Inicio
*  ENDIF.

*  IF p_cli IS NOT INITIAL.
*
*    IF it_bsad_id[] IS NOT INITIAL.
*      " Seleciona Simulador de vendas – Pagamento Antecipado
*      SELECT adiant doc_simulacao
*             FROM zsdt0159
*             APPENDING TABLE t_zsdt0159
*             FOR ALL ENTRIES IN it_bsad_id
*             WHERE adiant EQ it_bsad_id-belnr.
*
*      SORT t_zsdt0159 BY adiant doc_simulacao.
*    ENDIF.
*  ELSEIF p_for IS NOT INITIAL.
*
*    IF it_bsak_ik[] IS NOT INITIAL.
*      " Seleciona Simulador de vendas – Pagamento Antecipado
*      SELECT adiant doc_simulacao
*             FROM zsdt0159
*             APPENDING TABLE t_zsdt0159
*             FOR ALL ENTRIES IN it_bsak_ik
*             WHERE adiant EQ it_bsak_ik-belnr.
*
*      SORT t_zsdt0159 BY adiant doc_simulacao.
*    ENDIF.
*  ENDIF.
**-US 155522-23-10-2024-#155522-RJF-Fim
*
*  IF NOT t_zsdt0159[] IS INITIAL.
*
*    " Simulador de Vendas - dados de itens
*    SELECT doc_simulacao spart
*           FROM zsdt0041
*           INTO TABLE t_zsdt0041
*           FOR ALL ENTRIES IN t_zsdt0159
*           WHERE doc_simulacao EQ t_zsdt0159-doc_simulacao.
*
*    SORT t_zsdt0041 BY doc_simulacao.
*
*    " Simulador de Vendas - dados de cabeçalho
*    SELECT doc_simulacao cultura safra dt_entrega_sem dt_entrega_def dt_entrega_fet
*           FROM zsdt0040
*           INTO TABLE t_zsdt0040
*           FOR ALL ENTRIES IN t_zsdt0159
*           WHERE doc_simulacao EQ t_zsdt0159-doc_simulacao.
*
*    SORT t_zsdt0040 BY doc_simulacao.
*
*
*  ENDIF. "DEVK9A20VP - FI - CS2022000 - Composição de saldo cliente e fo #83808 RSA
*** BUG - 181038 - IFim - CBRAND

  "

  IF t_acdoca[] IS NOT INITIAL. "-US 155522-23-10-2024-#155522-RJF
    " Seleciona dados de Empresas
    SELECT bukrs butxt
           FROM t001
           INTO TABLE t_t001
           FOR ALL ENTRIES IN t_acdoca
           WHERE bukrs EQ t_acdoca-rbukrs.
  ENDIF.
  " Não usa mais it_bsak_ik / it_bsad_id
**-US 155522-23-10-2024-#155522-RJF-Inicio
*  ENDIF.
*  IF p_cli IS NOT INITIAL.
*
*    IF it_bsad_id[] IS NOT INITIAL.
*      " Seleciona dados de Empresas
*      SELECT bukrs butxt
*             FROM t001
*             APPENDING TABLE t_t001
*             FOR ALL ENTRIES IN it_bsad_id
*             WHERE bukrs EQ it_bsad_id-bukrs.
*    ENDIF.
*  ELSEIF p_for IS NOT INITIAL.
*
*    IF it_bsak_ik[] IS NOT INITIAL.
*      " Seleciona dados de Empresas
*      SELECT bukrs butxt
*             FROM t001
*             APPENDING TABLE t_t001
*             FOR ALL ENTRIES IN it_bsak_ik
*             WHERE bukrs EQ it_bsak_ik-bukrs.
*    ENDIF.
*  ENDIF.
**-US 155522-23-10-2024-#155522-RJF-Fim

  SORT t_t001 BY bukrs.

  " Seleciona Mestre de clientes (parte geral)
  IF NOT t_kunnr[] IS INITIAL.
    SELECT kunnr name1 stkzn stcd1 stcd2    "<<<------"189660 - NMS ------->>>
           FROM kna1
           INTO TABLE t_kna1
           FOR ALL ENTRIES IN t_kunnr
           WHERE kunnr EQ t_kunnr-kunnr.
    SORT t_kna1 BY kunnr.
  ENDIF.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF NOT t_kunnr_c[] IS INITIAL.
    SELECT kunnr name1 stkzn stcd1 stcd2    "<<<------"189660 - NMS ------->>>
           FROM kna1
           APPENDING TABLE t_kna1
           FOR ALL ENTRIES IN t_kunnr_c
           WHERE kunnr EQ t_kunnr_c-kunnr.
    SORT t_kna1 BY kunnr.
  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim

  " Seleciona Mestre de clientes (parte geral)
  IF NOT t_lifnr[] IS INITIAL.
    SELECT lifnr name1 stkzn stcd1 stcd2    "<<<------"189660 - NMS ------->>>
           FROM lfa1
           INTO TABLE t_lfa1
           FOR ALL ENTRIES IN t_lifnr
           WHERE lifnr EQ t_lifnr-lifnr.
    SORT t_lfa1 BY lifnr.
  ENDIF.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF NOT t_lifnr_f[] IS INITIAL.
    SELECT lifnr name1 stkzn stcd1 stcd2    "<<<------"189660 - NMS ------->>>
           FROM lfa1
           APPENDING TABLE t_lfa1
           FOR ALL ENTRIES IN t_lifnr_f
           WHERE lifnr EQ t_lifnr_f-lifnr.
    SORT t_lfa1 BY lifnr.
  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim

*  "// INICIO WBARBOSA 17/12/2024 US-83808
  IF t_bseg IS NOT INITIAL.
    SELECT ebeln bsart ekgrp
             FROM ekko
             APPENDING TABLE t_ekko
             FOR ALL ENTRIES IN t_bseg
             WHERE ebeln EQ t_bseg-ebeln
             AND   bsart IN rg_bsart.

    SORT t_ekko BY ebeln bsart ekgrp.
  ENDIF.
*  "// FIM WBARBOSA 17/12/2024 US-83808

  " Seleciona dados Cabeçalho do documento de compra
  IF NOT t_ekko_ebeln[] IS INITIAL.
    SELECT ebeln bsart ekgrp
           FROM ekko
           APPENDING TABLE t_ekko
           FOR ALL ENTRIES IN t_ekko_ebeln
           WHERE ebeln EQ t_ekko_ebeln-ebeln
           AND   bsart IN rg_bsart.

    SORT t_ekko BY ebeln bsart ekgrp.
  ENDIF.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF NOT t_ekko_ebelf[] IS INITIAL.
    SELECT ebeln bsart ekgrp
           FROM ekko
           APPENDING TABLE t_ekko
           FOR ALL ENTRIES IN t_ekko_ebelf
           WHERE ebeln EQ t_ekko_ebelf-ebeln
           AND   bsart IN rg_bsart.

    SORT t_ekko BY ebeln bsart ekgrp.
  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim
**<<<------"189660 - NMS - INI------>>>
  IF NOT t_ekko[] IS INITIAL.
    SELECT DISTINCT ebeln bednr
      FROM ekpo
      INTO TABLE gt_ekpo
      FOR ALL ENTRIES IN t_ekko
    WHERE ebeln EQ t_ekko-ebeln.

    SORT gt_ekpo BY ebeln.

  ENDIF.
**<<<------"189660 - NMS - FIM------>>>
  IF NOT t_ebeln_ebelp[] IS INITIAL.
    SELECT ebeln ebelp eindt
            FROM eket
            INTO TABLE t_eket
            FOR ALL ENTRIES IN t_ebeln_ebelp
            WHERE ebeln EQ t_ebeln_ebelp-ebeln
            AND   ebelp EQ t_ebeln_ebelp-ebelp.

    SORT t_eket BY ebeln ebelp.
  ENDIF.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF NOT t_ebeln_ebelf[] IS INITIAL.
    SELECT ebeln ebelp eindt
            FROM eket
            APPENDING TABLE t_eket
            FOR ALL ENTRIES IN t_ebeln_ebelf
            WHERE ebeln EQ t_ebeln_ebelf-ebeln
            AND   ebelp EQ t_ebeln_ebelf-ebelp.

    SORT t_eket BY ebeln ebelp.
  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim

  IF NOT t_ekko[] IS INITIAL.
    " Seleciona Textos para os tipos de documentos de compra
    SELECT bsart batxt
           FROM t161t
           INTO TABLE t_t161t
           FOR ALL ENTRIES IN t_ekko
           WHERE spras EQ 'P'
           AND   bsart EQ t_ekko-bsart.
    SORT t_t161t BY bsart.

    " Seleciona Grupos de compra
    SELECT ekgrp eknam
           FROM t024
           INTO TABLE t_t024
           FOR ALL ENTRIES IN t_ekko
           WHERE ekgrp EQ t_ekko-ekgrp.
    SORT t_t024 BY ekgrp.
  ENDIF.

  " Seleciona Centros/filiais
  IF NOT t_t001w_werks[] IS INITIAL.
    SELECT werks name1
           FROM t001w
           INTO TABLE t_t001w
           FOR ALL ENTRIES IN t_t001w_werks
           WHERE werks EQ t_t001w_werks-werks.

    SORT t_t001w BY werks.
  ENDIF.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF NOT t_t001w_werkc[] IS INITIAL.
    SELECT werks name1
           FROM t001w
           APPENDING TABLE t_t001w
           FOR ALL ENTRIES IN t_t001w_werkc
           WHERE werks EQ t_t001w_werkc-werks.

    SORT t_t001w BY werks.
  ENDIF.
  IF t_acdoca[] IS NOT INITIAL.
*-US 155522-23-10-2024-#155522-RJF-Fim

    " Seleciona Cabeçalho do documento contábil
    SELECT bukrs belnr gjahr xblnr kursf bstat hwae2 kurs2 stblg waers
           FROM bkpf
           INTO CORRESPONDING FIELDS OF TABLE t_bkpf
           FOR ALL ENTRIES IN t_acdoca
           WHERE bukrs EQ t_acdoca-rbukrs
           AND   belnr EQ t_acdoca-belnr
           AND   gjahr EQ t_acdoca-gjahr.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  ENDIF.
*  Não usa mais it_bsad_id - it_bsak_ik
*  IF p_cli IS NOT INITIAL.
*
*    IF it_bsad_id[] IS NOT INITIAL.
*      " Seleciona Cabeçalho do documento contábil
*      SELECT bukrs belnr gjahr xblnr kursf bstat hwae2 kurs2 stblg waers
*             FROM bkpf
*             APPENDING CORRESPONDING FIELDS OF TABLE t_bkpf
*             FOR ALL ENTRIES IN it_bsad_id
*             WHERE bukrs EQ it_bsad_id-bukrs
*             AND   belnr EQ it_bsad_id-belnr
*             AND   gjahr EQ it_bsad_id-gjahr.
*    ENDIF.
*  ELSEIF p_for IS NOT INITIAL.
*
*    IF it_bsak_ik[] IS NOT INITIAL.
*      " Seleciona Cabeçalho do documento contábil
*      SELECT bukrs belnr gjahr xblnr kursf bstat hwae2 kurs2 stblg waers
*             FROM bkpf
*             APPENDING CORRESPONDING FIELDS OF TABLE t_bkpf
*             FOR ALL ENTRIES IN it_bsak_ik
*             WHERE bukrs EQ it_bsak_ik-bukrs
*             AND   belnr EQ it_bsak_ik-belnr
*             AND   gjahr EQ it_bsak_ik-gjahr.
*    ENDIF.
*  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim


  SORT t_bkpf BY bukrs belnr gjahr xblnr.

**** BUG - 181038 - CBRAND - Inicio
*==================================================================*
  "Seleciona dados da OV / Criadas na transação ZSDT0062. / AOENNING
*  IF t_bkpf[] IS NOT INITIAL.
*    FREE: it_ov_aux.
*    it_ov_aux = VALUE #( FOR i IN t_bkpf WHERE ( xblnr NE space )  ( vbeln = i-xblnr
*                                           xblnr = i-xblnr ) ).
*
*
*    SELECT * FROM zsdt0053 INTO TABLE it_zsdt0053
*      FOR ALL ENTRIES IN it_ov_aux
*      WHERE vbeln = it_ov_aux-vbeln.
*
*    IF sy-subrc EQ 0.
*      " Seleciona Tabela de Solicitação Ordem de Venda - Cabeçalho
*      SELECT *
*      FROM zsdt0051
*      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0051
*      FOR ALL ENTRIES IN it_zsdt0053
*      WHERE nro_sol_ov EQ it_zsdt0053-nro_sol_ov.
*      IF sy-subrc EQ 0.
*
*        FREE: it_zsdt0062_aux.
*        it_zsdt0062_aux = VALUE #( FOR f IN it_zsdt0051 ( nro_sol = f-nro_sol_ov ) ).
*
*        SELECT * FROM zsdt0062 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0062
*          FOR ALL ENTRIES IN it_zsdt0062_aux
*          WHERE nro_sol EQ it_zsdt0062_aux-nro_sol.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*==================================================================*
  IF t_acdoca IS NOT INITIAL.

    SELECT * FROM zsdt0053 INTO TABLE it_zsdt0053
       FOR ALL ENTRIES IN t_acdoca
     WHERE vbeln EQ t_acdoca-kdauf.

    SORT it_zsdt0053 BY nro_sol_ov.
    DELETE it_zsdt0053 WHERE vbeln IS INITIAL.

    IF sy-subrc EQ 0.
      " Seleciona Tabela de Solicitação Ordem de Venda - Cabeçalho
      SELECT *
        FROM zsdt0051
      INTO CORRESPONDING FIELDS OF TABLE it_zsdt0051
        FOR ALL ENTRIES IN it_zsdt0053
          WHERE nro_sol_ov EQ it_zsdt0053-nro_sol_ov.

      IF sy-subrc EQ 0.

        FREE: it_zsdt0062_aux.
        it_zsdt0062_aux = VALUE #( FOR f IN it_zsdt0051 ( nro_sol = f-nro_sol_ov ) ).

        SELECT * FROM zsdt0062 INTO CORRESPONDING FIELDS OF TABLE it_zsdt0062
          FOR ALL ENTRIES IN it_zsdt0062_aux
          WHERE nro_sol EQ it_zsdt0062_aux-nro_sol.
      ENDIF.
    ENDIF.
  ENDIF.
**** BUG - 181038 - CBRAND - Fim

  IF t_acdoca[] IS NOT INITIAL. "-US 155522-23-10-2024-#155522-RJF
    " Seleciona Segmento do documento contabilidade financeira
    SELECT bukrs belnr gjahr kunnr lifnr zterm zlsch zlspr bvtyp xref1 xref2 xref3 kidno matnr netdt ebeln ebelp anln1 "BUG - 181838 - CBRAND ADD XREF3 anln1
           FROM bseg
           INTO CORRESPONDING FIELDS OF TABLE t_bseg
           FOR ALL ENTRIES IN t_acdoca
           WHERE bukrs EQ t_acdoca-rbukrs
             AND belnr EQ t_acdoca-belnr
             AND gjahr EQ t_acdoca-gjahr
             AND koart EQ gva_koart    "BUG - 181038 - CBRAND
             AND shkzg EQ gva_shkzg.   "BUG - 181038 - CBRAND
    "AND ebeln NE abap_false. "BUG - 181038 - CBRAND

*-US 155522-23-10-2024-#155522-RJF-Inicio
  ENDIF.
* Não usa mais  it_bsad_id  it_bsak_ik
*  IF p_cli IS NOT INITIAL.
*
*    IF it_bsad_id[] IS NOT INITIAL.
*      " Seleciona Segmento do documento contabilidade financeira
*      SELECT bukrs belnr gjahr kunnr lifnr zterm zlsch zlspr bvtyp xref1 xref2 kidno matnr netdt ebeln ebelp anln1 "BUG - 181838 - CBRAND
*             FROM bseg
*             APPENDING CORRESPONDING FIELDS OF TABLE t_bseg
*             FOR ALL ENTRIES IN it_bsad_id
*             WHERE bukrs EQ it_bsad_id-bukrs
*               AND belnr EQ it_bsad_id-belnr
*               AND gjahr EQ it_bsad_id-gjahr
*               AND ebeln NE abap_false.
*    ENDIF.
*  ELSEIF p_for IS NOT INITIAL.
*
*    IF it_bsak_ik[] IS NOT INITIAL.
*      " Seleciona Segmento do documento contabilidade financeira
*      SELECT bukrs belnr gjahr kunnr lifnr zterm zlsch zlspr bvtyp xref1 xref2 kidno matnr netdt ebeln ebelp anln1 "BUG - 181838 - CBRAND
*             FROM bseg
*             APPENDING CORRESPONDING FIELDS OF TABLE t_bseg
*             FOR ALL ENTRIES IN it_bsak_ik
*             WHERE bukrs EQ it_bsak_ik-bukrs
*               AND belnr EQ it_bsak_ik-belnr
*               AND gjahr EQ it_bsak_ik-gjahr
*               AND ebeln NE abap_false.
*    ENDIF.
*  ENDIF.
**-US 155522-23-10-2024-#155522-RJF-Fim

  SORT t_bseg BY bukrs belnr gjahr kunnr lifnr.

  IF NOT t_bkpf[] IS INITIAL.

    " Seleciona Tabela de Solicitação de ordem de venda - MATERIAIS
    SELECT nro_sol_ov charg vbeln  "DEVK9A20VP - FI - CS2022000 - Composição de saldo cliente e fo #83808 RSA
           FROM zsdt0053
           INTO TABLE t_zsdt0053
           FOR ALL ENTRIES IN t_bkpf
           WHERE vbeln EQ t_bkpf-xblnr(10).

    SORT t_zsdt0053 BY nro_sol_ov. "DEVK9A20VP - FI - CS2022000 - Composição de saldo cliente e fo #83808 RSA
    IF NOT t_zsdt0053[] IS INITIAL.

      " Seleciona Tabela de Solicitação Ordem de Venda - Cabeçalho
      SELECT nro_sol_ov dtde_logist
             FROM zsdt0051
             INTO TABLE t_zsdt0051
             FOR ALL ENTRIES IN t_zsdt0053
             WHERE nro_sol_ov EQ t_zsdt0053-nro_sol_ov.

    ENDIF.

  ENDIF.


  " Seleciona Mestre de contas do Razão (plano de contas)
  IF NOT t_ska1_saknr[] IS INITIAL.
    SELECT ska1~saknr skat~txt20
           FROM ska1 INNER JOIN skat
           ON  ska1~ktopl = skat~ktopl
           AND ska1~saknr = skat~saknr
           INTO TABLE t_ska1
           FOR ALL ENTRIES IN t_ska1_saknr
           WHERE ska1~ktopl EQ '0050'
           AND   ska1~saknr EQ t_ska1_saknr-saknr
           AND   skat~spras EQ sy-langu.

    SORT t_ska1 BY saknr txt20.
  ENDIF.

  " Seleciona Textos de centros de custo
  IF NOT t_cskt_kostl[] IS INITIAL.
    SELECT kostl ltext
           FROM cskt
           INTO TABLE t_cskt
           FOR ALL ENTRIES IN t_cskt_kostl
           WHERE spras EQ 'P'
           AND   kostl EQ t_cskt_kostl-kostl.

    SORT t_cskt BY kostl.
  ENDIF.

  " Seleciona Documento de vendas: dados de cabeçalho
  IF NOT t_vbak_vbeln[] IS INITIAL.
    SELECT vbeln auart vkbur
           FROM vbak
           INTO TABLE t_vbak
           FOR ALL ENTRIES IN t_vbak_vbeln
           WHERE vbeln EQ t_vbak_vbeln-vbeln.

    SORT t_vbak BY vbeln auart vkbur.
  ENDIF.

  IF NOT t_vbak[] IS INITIAL.
    " Seleciona Unidade organizacional: escritórios de vendas: textos
    SELECT vkbur bezei
           FROM tvkbt
           INTO TABLE t_tvkbt
           FOR ALL ENTRIES IN t_vbak
           WHERE spras EQ 'P'
           AND   vkbur EQ t_vbak-vkbur.

    SORT t_tvkbt BY vkbur.

    " Seleciona Documentos de venda: tipos: textos
    SELECT auart bezei
           FROM tvakt
           INTO TABLE t_tvakt
           FOR ALL ENTRIES IN t_vbak
           WHERE spras EQ 'P'
           AND   auart EQ t_vbak-auart.

    SORT t_tvakt BY auart.

  ENDIF.

  " Seleciona Documento de vendas: dados comerciais
  IF NOT t_vbak_vbeln[] IS INITIAL.
    SELECT vbeln bstkd
           FROM vbkd
           INTO TABLE t_vbkd
           FOR ALL ENTRIES IN t_vbak_vbeln
           WHERE vbeln EQ t_vbak_vbeln-vbeln.

    SORT t_vbkd BY vbeln.
  ENDIF.

  " Seleciona Textos breves de material
  IF NOT t_mara_matnr[] IS INITIAL.
    SELECT matnr maktx
           FROM makt
           INTO TABLE t_makt
           FOR ALL ENTRIES IN t_mara_matnr
           WHERE matnr EQ t_mara_matnr-matnr
           AND   spras EQ 'P'.

    SORT t_makt BY matnr.

    " Seleciona Dados gerais de material
    SELECT matnr matkl
           FROM mara
           INTO TABLE t_mara
           FOR ALL ENTRIES IN t_mara_matnr
           WHERE matnr EQ t_mara_matnr-matnr
           AND   matkl IN rg_matkl.

    SORT t_mara BY matnr matkl.
  ENDIF.

  IF NOT t_mara[] IS INITIAL.

    " Seleciona Denominações para grupos de mercadoria
    SELECT matkl wgbez
           FROM t023t
           INTO TABLE t_t023t
           FOR ALL ENTRIES IN t_mara
           WHERE spras EQ 'P'
           AND   matkl EQ t_mara-matkl.

    SORT t_t023t BY matkl.

  ENDIF.

  IF NOT t_lfb1_lifnr_bukrs[] IS INITIAL.

    " Seleciona Mestre de fornecedores (empresa)
    SELECT lifnr bukrs akont
           FROM lfb1
           INTO TABLE t_lfb1
           FOR ALL ENTRIES IN t_lfb1_lifnr_bukrs
           WHERE lifnr EQ t_lfb1_lifnr_bukrs-lifnr
           AND   bukrs EQ t_lfb1_lifnr_bukrs-bukrs.

    SORT t_lfb1 BY lifnr bukrs.

  ENDIF.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF NOT t_lfb1_lifnr_bukrf[] IS INITIAL.

    " Seleciona Mestre de fornecedores (empresa)
    SELECT lifnr bukrs akont
           FROM lfb1
           APPENDING TABLE t_lfb1
           FOR ALL ENTRIES IN t_lfb1_lifnr_bukrf
           WHERE lifnr EQ t_lfb1_lifnr_bukrf-lifnr
           AND   bukrs EQ t_lfb1_lifnr_bukrf-bukrs.

    SORT t_lfb1 BY lifnr bukrs.

  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim

  IF NOT t_knb1_kunnr_bukrs[] IS INITIAL.

    " Seleciona Mestre de clientes (empresa)
    SELECT kunnr bukrs akont
           FROM knb1
           INTO TABLE t_knb1
           FOR ALL ENTRIES IN t_knb1_kunnr_bukrs
           WHERE kunnr EQ t_knb1_kunnr_bukrs-kunnr
           AND   bukrs EQ t_knb1_kunnr_bukrs-bukrs.

    SORT t_knb1 BY kunnr bukrs.

  ENDIF.

*-US 155522-23-10-2024-#155522-RJF-Inicio
  IF NOT t_knb1_kunnr_bukrc[] IS INITIAL.

    " Seleciona Mestre de clientes (empresa)
    SELECT kunnr bukrs akont
           FROM knb1
           APPENDING TABLE t_knb1
           FOR ALL ENTRIES IN t_knb1_kunnr_bukrc
           WHERE kunnr EQ t_knb1_kunnr_bukrc-kunnr
           AND   bukrs EQ t_knb1_kunnr_bukrc-bukrs.

    SORT t_knb1 BY kunnr bukrs.

  ENDIF.
*-US 155522-23-10-2024-#155522-RJF-Fim
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_dados
*&---------------------------------------------------------------------*
FORM f_processa_dados.

  DATA: ls_obj    TYPE sibflporb,
        ls_reltyp TYPE obl_s_relt,
        lt_reltyp TYPE obl_t_relt,
        lt_links  TYPE obl_t_link,
        ls_link   TYPE obl_s_link,
        lt_roles  TYPE obl_t_role,
        ls_role   TYPE obl_s_role.

  DATA: lv_docid      TYPE sofolenti1-doc_id,
        lt_objhead    TYPE TABLE OF solisti1,
        ls_objhead    TYPE solisti1,
        ls_foldinfo   TYPE sofolenti1,
        lt_objcontent TYPE TABLE OF solisti1,
        ls_objcontent TYPE solisti1,
        lt_content    TYPE TABLE OF solix.

  READ TABLE rg_augdt INTO wa_augdt INDEX 1.

  LOOP AT t_acdoca INTO wa_acdoca.

    CLEAR wa_alv_saida.

    IF wa_acdoca-belnr CA sy-abcde.
      CONTINUE.
    ENDIF.

    READ TABLE t_ekko INTO wa_ekko WITH KEY ebeln = wa_acdoca-ebeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-bsart = wa_ekko-bsart.                    "Tp. Pedido
**<<<------"189660 - NMS - INI------>>>
      CLEAR el_ekpo.
      READ TABLE gt_ekpo INTO el_ekpo WITH KEY ebeln = wa_ekko-ebeln BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_alv_saida-bednr = el_ekpo-bednr.

      ENDIF.
**<<<------"189660 - NMS - FIM------>>>
    ENDIF.

    READ TABLE t_eket INTO wa_eket WITH KEY ebeln = wa_acdoca-ebeln ebelp = wa_acdoca-ebelp BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-eindt = wa_eket-eindt.                    "DATA REMESSA DO PEDIDO
    ENDIF.

    READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_acdoca-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_alv_saida-wgbez = wa_t023t-wgbez.                   "Descrição Grupo Mercadoria
      ENDIF.
    ENDIF.

    wa_alv_saida-rbukrs         = wa_acdoca-rbukrs. "Empresa

    READ TABLE t_t001 INTO wa_t001 WITH KEY bukrs = wa_acdoca-rbukrs BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-nome_empresa = wa_t001-butxt. "Nome Empresa
    ENDIF.

    IF NOT wa_acdoca-kunnr IS INITIAL.
      wa_alv_saida-cliente_fornecedor = wa_acdoca-kunnr.      "ID Conta Cliente/ Fornecedor
      READ TABLE t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_acdoca-kunnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_alv_saida-nome_cliente_fornecedor = wa_kna1-name1. "Nome Cliente/Fornecedor
**<<<------"189660 - NMS - INI------>>>
* Verifica se é pessoa física.
        CASE wa_kna1-stkzn.
          WHEN abap_on. "Pessoa Física
            WRITE wa_kna1-stcd2 TO wa_alv_saida-idfis USING EDIT MASK 'RR___.___.___-__'.

          WHEN abap_off. "Pessoa Jurídica
            WRITE wa_kna1-stcd1 TO wa_alv_saida-idfis USING EDIT MASK 'RR__.___.___/____-__'.

          WHEN OTHERS.
*         Do nothing
        ENDCASE.
**<<<------"189660 - NMS - FIM------>>>
      ENDIF.
    ENDIF.

    IF NOT wa_acdoca-lifnr IS INITIAL.
      wa_alv_saida-cliente_fornecedor = wa_acdoca-lifnr. "ID Conta Cliente/ Fornecedor
      READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_acdoca-lifnr BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_alv_saida-nome_cliente_fornecedor = wa_lfa1-name1. "Nome Cliente/Fornecedor
**<<<------"189660 - NMS - INI------>>>
* Verifica se é pessoa física.
        CASE wa_lfa1-stkzn.
          WHEN abap_on. "Pessoa Física
            WRITE wa_lfa1-stcd2 TO wa_alv_saida-idfis USING EDIT MASK 'RR___.___.___-__'.

          WHEN abap_off. "Pessoa Jurídica
            WRITE wa_lfa1-stcd1 TO wa_alv_saida-idfis USING EDIT MASK 'RR__.___.___/____-__'.

          WHEN OTHERS.
*         Do nothing
        ENDCASE.
**<<<------"189660 - NMS - FIM------>>>
      ENDIF.
    ENDIF.

*    wa_alv_saida-ebeln                   = wa_acdoca-ebeln.  "Doc. Compras "// WBARBOSA BUG-160850 13/12/2024

    READ TABLE t_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-batxt = wa_t161t-batxt.                    "Desc. Tp. Pedido
    ENDIF.

    READ TABLE t_t024 INTO wa_t024 WITH KEY ekgrp = wa_ekko-ekgrp BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-eknam = wa_t024-eknam.                     "Nome Comprador
    ENDIF.

    wa_alv_saida-zuonr                   = wa_acdoca-zuonr.  "Atribuição
    wa_alv_saida-rbusa                   = wa_acdoca-rbusa.  "Divisão


    READ TABLE t_t001w INTO wa_t001w WITH KEY werks = wa_acdoca-rbusa BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-nome_divisao = wa_t001w-name1.            "Nome Divisao
    ENDIF.

    READ TABLE t_bkpf INTO wa_bkpf WITH KEY bukrs = wa_acdoca-rbukrs belnr = wa_acdoca-belnr gjahr = wa_acdoca-gjahr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-xblnr = wa_bkpf-xblnr.            "Referência
      wa_alv_saida-kursf = wa_bkpf-kursf.            "Taxa Cambio Efetiva
      wa_alv_saida-waers = wa_bkpf-waers .           "Moeda do Docuemnto "// WBARBOSA BUG-160850 13/12/2024
    ENDIF.

    wa_alv_saida-belnr                   = wa_acdoca-belnr.  "Nº documento
    wa_alv_saida-bldat                   = wa_acdoca-bldat.  "Data documento
    wa_alv_saida-budat                   = wa_acdoca-budat.  "Data lançamento
    wa_alv_saida-netdt                   = wa_acdoca-netdt.  "Vencimento líquido
    wa_alv_saida-gjahr                   = wa_acdoca-gjahr.  "Exercício


    READ TABLE t_bseg INTO wa_bseg WITH KEY bukrs = wa_acdoca-rbukrs
                                            belnr = wa_acdoca-belnr
                                            gjahr = wa_acdoca-gjahr
                                            kunnr = wa_acdoca-kunnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-zterm = wa_bseg-zterm.                      "Cond.Pgto
      wa_alv_saida-kidno = wa_bseg-kidno.                      "Referência de pagamento
      wa_alv_saida-xref1 = wa_bseg-xref1.                      "Chave referência 1
      wa_alv_saida-xref2 = wa_bseg-xref2.                      "Chave referência 2
      wa_alv_saida-xref3 = wa_bseg-xref3.                      "Chave referência 3 - BUG - 181038
      wa_alv_saida-zlspr = wa_bseg-zlspr.                      "Bloqueio pgto.
      wa_alv_saida-bvtyp = wa_bseg-bvtyp.                      "Tipo Banco Parceiro
      wa_alv_saida-zlsch = wa_bseg-zlsch.                      "Forma Pagamento
      wa_alv_saida-ebeln = wa_bseg-ebeln.                      "Documento de Compra  "// WBARBOSA BUG-160850 13/12/2024
      wa_alv_saida-anln1 = wa_bseg-anln1.  "Imobilizado BUG - 181038 - CBRAND
    ENDIF.

    READ TABLE t_bseg INTO wa_bseg WITH KEY bukrs = wa_acdoca-rbukrs
                                            belnr = wa_acdoca-belnr
                                            gjahr = wa_acdoca-gjahr
                                            lifnr = wa_acdoca-lifnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-zterm = wa_bseg-zterm.                      "Cond.Pgto
      wa_alv_saida-kidno = wa_bseg-kidno.                      "Referência de pagamento
      wa_alv_saida-xref1 = wa_bseg-xref1.                      "Chave referência 1
      wa_alv_saida-xref2 = wa_bseg-xref2.                      "Chave referência 2
      wa_alv_saida-xref3 = wa_bseg-xref3.                      "Chave referência 3 - BUG - 181038
      wa_alv_saida-zlspr = wa_bseg-zlspr.                      "Bloqueio pgto.
      wa_alv_saida-bvtyp = wa_bseg-bvtyp.                      "Tipo Banco Parceiro
      wa_alv_saida-zlsch = wa_bseg-zlsch.                      "Forma Pagamento
      wa_alv_saida-ebeln = wa_bseg-ebeln.                      "Documento de Compra  "// WBARBOSA BUG-160850 13/12/2024
      wa_alv_saida-anln1 = wa_bseg-anln1.  "Imobilizado BUG - 181038 - CBRAND
    ENDIF.


    wa_alv_saida-rtcur                   = wa_acdoca-rtcur.  "Moeda documento
    wa_alv_saida-hsl                     = wa_acdoca-hsl.    "Vlr. Moeda documento
    wa_alv_saida-rkcur                   = wa_acdoca-rkcur.  "Moeda Forte
    wa_alv_saida-ksl                     = wa_acdoca-ksl.    "Vlr. Moeda forte
    wa_alv_saida-blart                   = wa_acdoca-blart.  "Tipo de documento
    wa_alv_saida-bschl                   = wa_acdoca-bschl.  "Chave de lançamento
    wa_alv_saida-umskz                   = wa_acdoca-umskz.  "Cód.Razão Especial
    wa_alv_saida-sgtxt                   = wa_acdoca-sgtxt.  "Texto
    wa_alv_saida-racct                   = wa_acdoca-racct.  "Conta do Razão

    READ TABLE t_ekko INTO wa_ekko WITH KEY ebeln = wa_acdoca-ebeln BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-bsart = wa_ekko-bsart.                    "Tp. Pedido
    ENDIF.

    READ TABLE t_ska1 INTO wa_ska1 WITH KEY saknr = wa_acdoca-racct BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-txt20 = wa_ska1-txt20.                     "Txt.descr.cta.Razão
    ENDIF.

    wa_alv_saida-augdt                   = wa_acdoca-augdt.  "Data de compensação
    wa_alv_saida-augbl                   = wa_acdoca-augbl.  "Doc.compensação
    wa_alv_saida-rcntr                   = wa_acdoca-rcntr.  "Centro custo

    READ TABLE t_cskt INTO wa_cskt WITH KEY kostl = wa_acdoca-rcntr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-ltext = wa_cskt-ltext.                     "Nome Centro de Custo
    ENDIF.

    wa_alv_saida-prctr                   = wa_acdoca-prctr.  "Centro de lucro
*    wa_alv_saida-anln1                   = wa_acdoca-anln1.  "Imobilizado BUG - 181038 - CBRAND

    READ TABLE t_vbak INTO wa_vbak WITH KEY vbeln = wa_acdoca-kdauf BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-vkbur = wa_vbak-vkbur.                     "Esc Venda
      wa_alv_saida-auart = wa_vbak-auart.                     "TP.OV.


      READ TABLE t_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbak-vkbur BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_alv_saida-bezei = wa_tvkbt-bezei.                    "Nome Esc Venda
      ENDIF.

      READ TABLE t_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_alv_saida-desc_ov = wa_tvakt-bezei.                   "Descrição Tipo OV
      ENDIF.

    ENDIF.

    wa_alv_saida-kdauf                   = wa_acdoca-kdauf.  "Ordem Venda
    wa_alv_saida-rassc                   = wa_acdoca-rassc.  "Soc.parc.negócios

    READ TABLE t_vbkd INTO wa_vbkd WITH KEY vbeln = wa_acdoca-kdauf BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-bstkd = wa_vbkd-bstkd.                     "Texto Pedido (SD)
    ENDIF.

    wa_alv_saida-matnr                   = wa_acdoca-matnr.  "Material

    READ TABLE t_makt INTO wa_makt WITH KEY matnr = wa_acdoca-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-maktx  = wa_makt-maktx.                   "Descrição Material
    ENDIF.

    READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_acdoca-matnr BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-matkl  = wa_mara-matkl.                     "Grupo de Mercadoria
      READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_alv_saida-wgbez = wa_t023t-wgbez.                   "Descrição Grupo Mercadoria
      ENDIF.
    ENDIF.

    wa_alv_saida-hbkid                   = wa_acdoca-hbkid.     "Banco Empresa
    wa_alv_saida-awref_rev               = wa_acdoca-awref_rev. "Estorno com
    wa_alv_saida-drcrk = wa_acdoca-drcrk.

    IF     wa_acdoca-drcrk = 'S'.
      wa_alv_saida-cod_deb_cred = 'Débito'.                            "Descrição Cód.débito/crédito
    ELSEIF wa_acdoca-drcrk = 'H'.
      wa_alv_saida-cod_deb_cred = 'Crédito'.                           "Descrição Cód.débito/crédito
    ENDIF.

    PACK wa_acdoca-racct TO wa_acdoca-racct. CONDENSE wa_acdoca-racct.
    IF     wa_acdoca-racct(01) EQ '1'.
      wa_alv_saida-natureza_cto_razao = 'Ativo'.                        "Natureza Conta Razão
    ELSEIF wa_acdoca-racct(01) EQ '2'.
      wa_alv_saida-natureza_cto_razao = 'Passivo'.                      "Natureza Conta Razão
    ENDIF.


    IF NOT wa_acdoca-kunnr IS INITIAL.
      READ TABLE t_knb1 INTO wa_knb1 WITH KEY kunnr = wa_acdoca-kunnr bukrs = wa_acdoca-rbukrs BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_alv_saida-akont = wa_knb1-akont.                   "Cadastro Cta.de reconciliação
      ENDIF.
    ENDIF.

    IF NOT wa_acdoca-lifnr IS INITIAL.
      READ TABLE t_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_acdoca-lifnr bukrs = wa_acdoca-rbukrs BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_alv_saida-akont = wa_lfb1-akont.                   "Cadastro Cta.de reconciliação
      ENDIF.
    ENDIF.

    IF wa_acdoca-blart(02) EQ 'VC'.
      wa_alv_saida-classificacao_st = '1. PROVISAO'.          "Classificação Status
    ELSEIF NOT wa_acdoca-augbl IS INITIAL.
      wa_alv_saida-classificacao_st = '2. COMPENSADO'.        "Classificação Status
** BUG - 178687 - CBRAND - Inicio
*    ELSEIF wa_acdoca-netdt GT wa_augdt-low.
    ELSEIF wa_acdoca-netdt GE s_dtp-low.
** BUG - 178687 - CBRAND - Fim
      wa_alv_saida-classificacao_st = '3. EM ANDAMENTO'.      "Classificação Status
** BUG - 178687 - CBRAND - Inicio
*    ELSEIF wa_acdoca-netdt LT wa_augdt-low.
    ELSEIF wa_acdoca-netdt LT  s_dtp-low.
** BUG - 178687 - CBRAND - Fim
      wa_alv_saida-classificacao_st = '4. VENCIDO'.           "Classificação Status
    ENDIF.

** BUG - 178687 - CBRAND - Inicio
    IF wa_acdoca-netdt < v_data_p ." AND wa_acdoca-netdt IS NOT INITIAL.
** BUG - 178687 - CBRAND - Fim
      CLEAR v_day.
      CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
        EXPORTING
          date1            = wa_acdoca-netdt
          time1            = sy-uzeit
          date2            = v_data_p "wa_augdt-low BUG - 178687 - CBRAND
          time2            = sy-uzeit
        IMPORTING
          datediff         = v_day
        EXCEPTIONS
          invalid_datetime = 1
          OTHERS           = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

*** BUG - 171955 - Inicio - CBRAND
*    CLEAR: wa_zfit0208.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY blart = wa_acdoca-blart
*                                                    bschl = wa_acdoca-bschl.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-pdd = wa_zfit0208-pdd.                       "PDD
*      CONCATENATE wa_zfit0208-cod_operacao wa_zfit0208-desc_cod_operacao
*      INTO wa_alv_saida-tp_operacao SEPARATED BY space.         "Tipo de Operação
*
*    ELSE.
**      wa_alv_saida-tp_operacao = 'Sem parametro para esse documento'."// WBARBOSA 17/12/2024 - US-83808
*      wa_alv_saida-tp_operacao = 'DELETAR'."// WBARBOSA 17/12/2024 - US-83808
*    ENDIF.
*** BUG - 171955 - Fim - CBRAND
*** BUG - 184687 - CBRAND - Inicio
*      IF  v_day GT '365'.
*        wa_alv_saida-aging_list = 'Vencido +365 dias'.  "Aging List
*      ELSEIF v_day EQ '180'.
*        wa_alv_saida-aging_list = 'Vencidos até 180 dias'.  "Aging List
*        "ELSEIF v_day EQ '30'. BUG - 178687 - CBRAND
*      ELSEIF v_day LE '30'.
*        wa_alv_saida-aging_list = 'Vencidos até 30 dias'.  "Aging List
*      ELSEIF v_day EQ '365'.
*        wa_alv_saida-aging_list = 'Vencidos até 365 dias'.  "Aging List
*      ELSEIF v_day EQ '60'.
*        wa_alv_saida-aging_list = 'Vencidos até 60 dias'.  "Aging List
*      ELSEIF v_day EQ '90'.
*        wa_alv_saida-aging_list = 'Vencidos até 90 dias'.  "Aging List
*      ELSE.
*        wa_alv_saida-aging_list = 'A vencer'.  "Aging List
*      ENDIF.

      IF v_day LE '30'.
        wa_alv_saida-aging_list = 'Vencidos até 30 dias'.  "Aging List
      ELSEIF v_day >= '31' AND v_day <= '60'.
        wa_alv_saida-aging_list = 'Vencidos até 60 dias'.  "Aging List
      ELSEIF v_day >= '61' AND v_day <= '90'.
        wa_alv_saida-aging_list = 'Vencidos até 90 dias'.  "Aging List
      ELSEIF  v_day >= '91' AND  v_day <= '180'.
        wa_alv_saida-aging_list = 'Vencidos até 180 dias'.  "Aging List
      ELSEIF v_day >= '181' AND v_day < '365'.
        wa_alv_saida-aging_list = 'Vencidos até 365 dias'.  "Aging List
      ELSEIF  v_day GT '365'.
        wa_alv_saida-aging_list = 'Vencido +365 dias'.  "Aging List
      ENDIF.

**** BUG - 184687 - FIM - CBRAND
** BUG - 178687 - CBRAND - Inicio
    ELSE.
      IF wa_acdoca-netdt >= v_data_p.
        wa_alv_saida-aging_list = 'A vencer'.
      ENDIF.
    ENDIF.
** BUG - 178687 - CBRAND - Fim

*"// wbarbosa 17/12/2024 US-83808
    READ TABLE t_ekko INTO wa_ekko WITH KEY ebeln = wa_alv_saida-ebeln.
    IF sy-subrc IS INITIAL.
      wa_alv_saida-bsart = wa_ekko-bsart.                    "Tp. Pedido
    ENDIF.

    READ TABLE t_eket INTO wa_eket WITH KEY ebeln = wa_alv_saida-ebeln.
    IF sy-subrc IS INITIAL.
      wa_alv_saida-eindt = wa_eket-eindt.                    "DATA REMESSA DO PEDIDO
    ENDIF.

    READ TABLE t_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-batxt = wa_t161t-batxt.                    "Desc. Tp. Pedido
    ENDIF.

    READ TABLE t_t024 INTO wa_t024 WITH KEY ekgrp = wa_ekko-ekgrp BINARY SEARCH.
    IF sy-subrc EQ 0.
      wa_alv_saida-eknam = wa_t024-eknam.                     "Nome Comprador
    ENDIF.
*"// wbarbosa 17/12/2024 US-83808

    CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr INTO v_objkey.
    ls_obj-instid = v_objkey.
    ls_obj-typeid = 'BKPF'.
    ls_obj-catid  = 'BO'.

    CLEAR lt_reltyp[].
    ls_reltyp-sign   = 'I'.
    ls_reltyp-option = 'EQ'.
    ls_reltyp-low    = 'NOTE'.
    APPEND ls_reltyp TO lt_reltyp.

    TRY.
        CLEAR lt_links[].
        CALL METHOD cl_binary_relation=>read_links
          EXPORTING
            is_object           = ls_obj
            it_relation_options = lt_reltyp
          IMPORTING
            et_links            = lt_links.
      CATCH cx_obl_parameter_error .
      CATCH cx_obl_internal_error .
      CATCH cx_obl_model_error .
    ENDTRY.

    READ TABLE lt_links INTO ls_link INDEX 1.
    IF sy-subrc EQ 0.
      CLEAR lt_objcontent[].
      lv_docid = ls_link-instid_b.
      CALL FUNCTION 'SO_DOCUMENT_READ_API1'
        EXPORTING
          document_id                = lv_docid
          "filter                     = 'X '
        IMPORTING
          document_data              = ls_foldinfo
        TABLES
          object_content             = lt_objcontent
        EXCEPTIONS
          document_id_not_exist      = 1
          operation_no_authorization = 2
          x_error                    = 3
          OTHERS                     = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      READ TABLE lt_objcontent INTO ls_objcontent INDEX 1.
      IF sy-subrc EQ 0.
        wa_alv_saida-historico = ls_objcontent-line. "Histórico
      ENDIF.
    ENDIF.


    CLEAR v_objkey.
    v_classname = 'BKPF'.
    CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr INTO v_objkey.
    v_client    = sy-mandt.
    CLEAR t_gos_connections[].
    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
      EXPORTING
        classname          = v_classname
        objkey             = v_objkey
        client             = v_client
      TABLES
        gos_connections    = t_gos_connections
      EXCEPTIONS
        no_objects_found   = 1
        internal_error     = 2
        internal_gos_error = 3.

    IF lines( t_gos_connections ) > 0.
      wa_alv_saida-anexo = icon_attachment. "Anexo
    ELSE.
      wa_alv_saida-anexo = icon_aggregate. "Anexo
    ENDIF.

*** BUG - 181038 - CBRAND - Inicio
*    READ TABLE t_zsdt0159 INTO wa_zsdt0159 WITH KEY adiant = wa_acdoca-belnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*    READ TABLE t_zsdt0041 INTO wa_zsdt0041 WITH KEY doc_simulacao = wa_zsdt0159-doc_simulacao BINARY SEARCH.
*** BUG - 181038 - CBRAND - Fim
    READ TABLE t_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_acdoca-kdauf BINARY SEARCH.
    IF sy-subrc EQ 0.

      READ TABLE t_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao BINARY SEARCH. " wa_zsdt0159-doc_simulacao BINARY SEARCH. "BUG - 181038 - CBRAND
      IF sy-subrc EQ 0.
        wa_alv_saida-cultura = wa_zsdt0040-cultura.
        wa_alv_saida-safra   = wa_zsdt0040-safra.
        wa_alv_saida-vkbur   = wa_zsdt0040-vkbur.    "<<<------"189660 - NMS ------->>>

        IF wa_zsdt0041-spart = '02'.
          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_fet.
        ELSEIF wa_zsdt0041-spart = '03'.
          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_def.
        ELSEIF wa_zsdt0041-spart = '04'.
          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_sem.
        ENDIF.

      ENDIF.

      "ENDIF. BUG - 181038 - CBRND

    ELSE.
*==================================================================*
      "Seleciona dados da OV / Criadas na transação ZSDT0062. / AOENNING

      READ TABLE it_zsdt0053 INTO ws_zsdt0053 WITH KEY vbeln =  wa_acdoca-kdauf.  "wa_bkpf-xblnr. BUG - 181038
      IF sy-subrc EQ 0.
        wa_alv_saida-safra   = ws_zsdt0053-charg.

        READ TABLE it_zsdt0051 INTO ws_zsdt0051 WITH KEY nro_sol_ov = ws_zsdt0053-nro_sol_ov .
        IF sy-subrc EQ 0.
          wa_alv_saida-dt_final_entrega = ws_zsdt0051-dtde_logist.
          wa_alv_saida-vkbur            = ws_zsdt0051-vkbur.    "<<<------"189660 - NMS ------->>>
          READ TABLE it_zsdt0062 INTO ws_zsdt0062 WITH KEY nro_sol = ws_zsdt0051-nro_sol_ov.
          IF sy-subrc EQ 0.
*            wa_alv_saida-dt_final_entrega = ws_zsdt0062-dtde_logist.
          ENDIF.
        ENDIF.
** "BUG - 181038 - Inicio - CBRAND
      ELSE.

        READ TABLE t_zsdt0090 INTO wa_zsdt0090 WITH KEY vbeln = wa_acdoca-kdauf BINARY SEARCH.
        IF sy-subrc EQ 0.

          READ TABLE t_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0090-doc_simulacao BINARY SEARCH.
          IF sy-subrc EQ 0.
            wa_alv_saida-cultura = wa_zsdt0040-cultura.
            wa_alv_saida-safra   = wa_zsdt0040-safra.
            wa_alv_saida-vkbur   = wa_zsdt0040-vkbur.    "<<<------"189660 - NMS ------->>>
**<<<------"189660 - NMS - INI------>>>
*            IF wa_zsdt0041-spart = '02'.
            IF wa_zsdt0090-spart = '02'.
**<<<------"189660 - NMS - FIM------>>>
              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_fet.
**<<<------"189660 - NMS - INI------>>>
*            ELSEIF wa_zsdt0041-spart = '03'.
            ELSEIF wa_zsdt0090-spart = '03'.
**<<<------"189660 - NMS - FIM------>>>
              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_def.
**<<<------"189660 - NMS - INI------>>>
*            ELSEIF wa_zsdt0041-spart = '04'.
            ELSEIF wa_zsdt0090-spart = '04'.
**<<<------"189660 - NMS - FIM------>>>
              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_sem.
            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.
** "BUG - 181038 - Fim - CBRAND
    ENDIF.
*==================================================================*

    PERFORM f_classificacao CHANGING wa_alv_saida .

    APPEND wa_alv_saida TO t_alv_saida.
    CLEAR: wa_alv_saida.

  ENDLOOP.

*  DELETE t_alv_saida WHERE tp_operacao EQ 'DELETAR'. "// WBARBOSA 17/12/2024 - US-83808

*-US 155522-23-10-2024-#155522-RJF-Inicio
*  PERFORM f_part_memo. "// WBARBOSA BUG-160850 13/12/2024 remover implementação da Partida MEMO
*-US 155522-23-10-2024-#155522-RJF-Fim
  "//WSB RET
  IF t_alv_saida[] IS INITIAL.
*    MESSAGE i005(zfi).
*    STOP.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_anexa_visualiza_arquivo
*&---------------------------------------------------------------------*
FORM f_anexa_visualiza_arquivo .

  DATA: ls_object    TYPE sibflporb,
        save_request TYPE sgs_flag.

  CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr
  INTO ls_object-instid.
  ls_object-typeid = 'BKPF'.
  ls_object-catid = 'BO'.

  CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
    EXPORTING
      is_object       = ls_object
      ip_mode         = 'E' " Edit mode
      ip_notes        = ''
      ip_urls         = ''
    IMPORTING
      ep_save_request = save_request.
  IF save_request = 'X'.
    COMMIT WORK.
  ENDIF.

  "Atualiza Tela da ALV
  "obj_ret->refresh( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_anexa_visualiza_nota
*&---------------------------------------------------------------------*
FORM f_anexa_visualiza_nota.

  DATA: ls_object    TYPE sibflporb,
        save_request TYPE sgs_flag.

  CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr
  INTO ls_object-instid.
  ls_object-typeid = 'BKPF'.
  ls_object-catid  = 'BO'.

  CALL FUNCTION 'GOS_ATTACHMENT_LIST_POPUP'
    EXPORTING
      is_object       = ls_object
      ip_mode         = 'E' " Edit mode
      ip_attachments  = ''
      ip_urls         = ''
    IMPORTING
      ep_save_request = save_request.
  IF save_request = 'X'.
    COMMIT WORK.
  ENDIF.

  "Atualiza Tela da ALV
  "obj_ret->refresh( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form grava_historico
*&---------------------------------------------------------------------*
FORM grava_historico .

  DATA document_id       TYPE sofmk.
  DATA lt_objhead TYPE STANDARD TABLE OF soli.
  DATA lt_objcont TYPE STANDARD TABLE OF soli.
  DATA wa_objcont TYPE soli.
  DATA l_obj_id   TYPE soodk.
  DATA l_obj_data TYPE sood1.
  DATA is_object TYPE  borident.
  DATA rel_doc  TYPE borident.

  DATA: folder_id TYPE sofdk.

  CALL FUNCTION 'SO_FOLDER_ROOT_ID_GET'
    EXPORTING
      region    = 'B'
    IMPORTING
      folder_id = folder_id
    EXCEPTIONS
      OTHERS    = 0.


  READ TABLE rg_augdt INTO wa_augdt INDEX 1.

  LOOP AT t_alv_saida INTO wa_alv_saida WHERE historico NE ''.

    CLEAR: l_obj_data, lt_objcont[].

    CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr INTO is_object-objkey.

    SELECT instid_a UP TO 1 ROWS
           FROM srgbtbrel
           INTO @DATA(v_instid_a)
           WHERE reltype  EQ 'NOTE'
           AND   instid_a EQ @is_object-objkey.
    ENDSELECT.
    IF sy-subrc NE 0.

      CONCATENATE 'Fechamento Mês' ' ' wa_augdt-low+04(02) '/' wa_augdt-low(04) INTO l_obj_data-objdes.
      l_obj_data-file_ext = 'TXT'.
      l_obj_data-objla    = 'P'.
      l_obj_data-objdes   = l_obj_data-objdes.
      l_obj_data-file_ext = 'TXT'.

      wa_objcont-line = wa_alv_saida-historico.
      APPEND wa_objcont TO lt_objcont.

      CLEAR: document_id, is_object, l_obj_id.
      CALL FUNCTION 'SO_OBJECT_INSERT'
        EXPORTING
          folder_id             = folder_id
          object_type           = 'RAW'
          object_hd_change      = l_obj_data
        IMPORTING
          object_id             = l_obj_id
        TABLES
          objhead               = lt_objhead
          objcont               = lt_objcont
        EXCEPTIONS
          active_user_not_exist = 35
          folder_not_exist      = 6
          object_type_not_exist = 17
          owner_not_exist       = 22
          parameter_error       = 23
          OTHERS                = 1000.

      IF sy-subrc = 0.
        document_id-foltp = folder_id-foltp.
        document_id-folyr = folder_id-folyr.
        document_id-folno = folder_id-folno.
        document_id-doctp = l_obj_id-objtp.
        document_id-docyr = l_obj_id-objyr.
        document_id-docno = l_obj_id-objno.
      ENDIF.

      CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr INTO is_object-objkey.
      is_object-objtype = 'BKPF'.

      IF NOT is_object-objkey IS INITIAL.
        IF NOT document_id IS INITIAL.
          CLEAR rel_doc.
          rel_doc-objkey  = document_id.
          rel_doc-objtype = 'MESSAGE'.
          CALL FUNCTION 'BINARY_RELATION_CREATE'
            EXPORTING
              obj_rolea    = is_object
              obj_roleb    = rel_doc
              relationtype = 'NOTE'
            EXCEPTIONS
              OTHERS       = 1.
          IF sy-subrc = 0.
            COMMIT WORK.
            MESSAGE 'Histórico Gravado com sucesso !' TYPE 'S'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form carrega_variantes
*&---------------------------------------------------------------------*
FORM carrega_variantes .

  PERFORM escolher_variante CHANGING g_sel_var.

  IF g_sel_var NE space.
    CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
      EXPORTING
        report               = sy-repid
        variant              = g_sel_var
      EXCEPTIONS
        variant_not_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form escolher_variante
*&---------------------------------------------------------------------*
FORM escolher_variante CHANGING p_g_sel_var.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = sy-repid
      masked               = 'X'
    IMPORTING
      sel_variant          = p_g_sel_var
      sel_variant_text     = g_sel_vartxt
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.

ENDFORM.
*-US 155522-23-10-2024-#155522-RJF-Inicio
*&---------------------------------------------------------------------*
*& Form f_part_memo
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_part_memo.
** Não faz mais essa busca
*  DATA: ls_obj    TYPE sibflporb,
*        ls_reltyp TYPE obl_s_relt,
*        lt_reltyp TYPE obl_t_relt,
*        lt_links  TYPE obl_t_link,
*        ls_link   TYPE obl_s_link,
*        lt_roles  TYPE obl_t_role,
*        ls_role   TYPE obl_s_role.
*
*  DATA: lv_docid      TYPE sofolenti1-doc_id,
*        lt_objhead    TYPE TABLE OF solisti1,
*        ls_objhead    TYPE solisti1,
*        ls_foldinfo   TYPE sofolenti1,
*        lt_objcontent TYPE TABLE OF solisti1,
*        ls_objcontent TYPE solisti1,
*        lt_content    TYPE TABLE OF solix.
*
*  READ TABLE rg_augdt INTO wa_augdt INDEX 1.
*
*  LOOP AT it_bsad_id INTO DATA(wa_acdoca).
*
*    CLEAR wa_alv_saida.
*
*    IF wa_acdoca-belnr CA sy-abcde.
*      CONTINUE.
*    ENDIF.
*
**    READ TABLE t_ekko INTO wa_ekko WITH KEY ebeln = wa_acdoca-ebeln BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-bsart = wa_ekko-bsart.                    "Tp. Pedido
**    ENDIF.
*
**    READ TABLE t_eket INTO wa_eket WITH KEY ebeln = wa_acdoca-ebeln ebelp = wa_acdoca-ebelp BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-eindt = wa_eket-eindt.                    "DATA REMESSA DO PEDIDO
**    ENDIF.
*
**    READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_acdoca-matnr BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        wa_alv_saida-wgbez = wa_t023t-wgbez.                   "Descrição Grupo Mercadoria
**      ENDIF.
**    ENDIF.
*
*    wa_alv_saida-rbukrs         = wa_acdoca-bukrs. "Empresa
*
*    READ TABLE t_t001 INTO wa_t001 WITH KEY bukrs = wa_acdoca-bukrs BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-nome_empresa = wa_t001-butxt. "Nome Empresa
*    ENDIF.
*
*    IF NOT wa_acdoca-kunnr IS INITIAL.
*      wa_alv_saida-cliente_fornecedor = wa_acdoca-kunnr.      "ID Conta Cliente/ Fornecedor
*      READ TABLE t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_acdoca-kunnr BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-nome_cliente_fornecedor = wa_kna1-name1. "Nome Cliente/Fornecedor
*      ENDIF.
*    ENDIF.
*
**    IF NOT wa_acdoca-lifnr IS INITIAL.
**      wa_alv_saida-cliente_fornecedor = wa_acdoca-lifnr. "ID Conta Cliente/ Fornecedor
**      READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_acdoca-lifnr BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        wa_alv_saida-nome_cliente_fornecedor = wa_lfa1-name1. "Nome Cliente/Fornecedor
**      ENDIF.
**    ENDIF.
*
**    wa_alv_saida-ebeln                   = wa_acdoca-ebeln.  "Doc. Compras
*
**    READ TABLE t_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-batxt = wa_t161t-batxt.                    "Desc. Tp. Pedido
**    ENDIF.
*
*
**    READ TABLE t_t024 INTO wa_t024 WITH KEY ekgrp = wa_ekko-ekgrp BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-eknam = wa_t024-eknam.                     "Nome Comprador
**    ENDIF.
*
*    wa_alv_saida-zuonr                   = wa_acdoca-zuonr.  "Atribuição
*    wa_alv_saida-rbusa                   = wa_acdoca-gsber.  "Divisão
*
*
*    READ TABLE t_t001w INTO wa_t001w WITH KEY werks = wa_acdoca-gsber BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-nome_divisao = wa_t001w-name1.            "Nome Divisao
*    ENDIF.
*
*    READ TABLE t_bkpf INTO wa_bkpf WITH KEY bukrs = wa_acdoca-bukrs belnr = wa_acdoca-belnr gjahr = wa_acdoca-gjahr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*
*      IF wa_bkpf-bstat NE 'S'.
*        CONTINUE.
*      ENDIF.
*
*      wa_alv_saida-xblnr = wa_bkpf-xblnr.            "Referência
*      wa_alv_saida-kursf = wa_bkpf-kurs2.            "Taxa Cambio Efetiva
*      wa_alv_saida-waers = wa_bkpf-waers .           "Moeda do Docuemnto "// WBARBOSA BUG-160850 13/12/2024
*    ENDIF.
*
*    wa_alv_saida-belnr                   = wa_acdoca-belnr.  "Nº documento
*    wa_alv_saida-bldat                   = wa_acdoca-bldat.  "Data documento
*    wa_alv_saida-budat                   = wa_acdoca-budat.  "Data lançamento
**    wa_alv_saida-netdt                   = wa_acdoca-infae.  "Vencimento líquido
*    wa_alv_saida-gjahr                   = wa_acdoca-gjahr.  "Exercício
*
*
*    READ TABLE t_bseg INTO wa_bseg WITH KEY bukrs = wa_acdoca-bukrs
*                                            belnr = wa_acdoca-belnr
*                                            gjahr = wa_acdoca-gjahr
*                                            kunnr = wa_acdoca-kunnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-zterm = wa_bseg-zterm.                      "Cond.Pgto
*      wa_alv_saida-kidno = wa_bseg-kidno.                      "Referência de pagamento
*      wa_alv_saida-xref1 = wa_bseg-xref1.                      "Chave referência 1
*      wa_alv_saida-xref2 = wa_bseg-xref2.                      "Chave referência 3
*      wa_alv_saida-zlspr = wa_bseg-zlspr.                      "Bloqueio pgto.
*      wa_alv_saida-bvtyp = wa_bseg-bvtyp.                      "Tipo Banco Parceiro
*      wa_alv_saida-zlsch = wa_bseg-zlsch.                      "Forma Pagamento
*      wa_alv_saida-netdt = wa_bseg-netdt.                      "Vencimento líquido
*      wa_alv_saida-ebeln = wa_bseg-ebeln.                      "Documento de Compra  "// WBARBOSA BUG-160850 13/12/2024
*      wa_alv_saida-anln1 = wa_bseg-anln1.  "Imobilizado BUG - 181038 - CBRAND
*    ENDIF.
*
*    READ TABLE t_bseg INTO wa_bseg WITH KEY bukrs = wa_acdoca-bukrs
*                                            belnr = wa_acdoca-belnr
*                                            gjahr = wa_acdoca-gjahr
*                                            kunnr = wa_acdoca-kunnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-zterm = wa_bseg-zterm.                      "Cond.Pgto
*      wa_alv_saida-kidno = wa_bseg-kidno.                      "Referência de pagamento
*      wa_alv_saida-xref1 = wa_bseg-xref1.                      "Chave referência 1
*      wa_alv_saida-xref2 = wa_bseg-xref2.                      "Chave referência 3
*      wa_alv_saida-zlspr = wa_bseg-zlspr.                      "Bloqueio pgto.
*      wa_alv_saida-bvtyp = wa_bseg-bvtyp.                      "Tipo Banco Parceiro
*      wa_alv_saida-zlsch = wa_bseg-zlsch.                      "Forma Pagamento
*      wa_alv_saida-ebeln = wa_bseg-ebeln.                      "Documento de Compra  "// WBARBOSA BUG-160850 13/12/2024
*      wa_alv_saida-anln1 = wa_bseg-anln1.  "Imobilizado BUG - 181038 - CBRAND
*    ENDIF.
*
*
*    wa_alv_saida-rtcur                   = wa_acdoca-waers.  "Moeda documento
*    wa_alv_saida-hsl                     = wa_acdoca-wrbtr.    "Vlr. Moeda documento
*    wa_alv_saida-rkcur                   = wa_bkpf-hwae2.  "Moeda Forte
*    wa_alv_saida-ksl                     = wa_acdoca-dmbtr.    "Vlr. Moeda forte
*    wa_alv_saida-blart                   = wa_acdoca-blart.  "Tipo de documento
*    wa_alv_saida-bschl                   = wa_acdoca-bschl.  "Chave de lançamento
*    wa_alv_saida-umskz                   = wa_acdoca-umskz.  "Cód.Razão Especial
*    wa_alv_saida-sgtxt                   = wa_acdoca-sgtxt.  "Texto
*    wa_alv_saida-racct                   = wa_acdoca-hkont.  "Conta do Razão
*    wa_alv_saida-txt20 = wa_acdoca-hkont.
*
**    READ TABLE t_ska1 INTO wa_ska1 WITH KEY saknr = wa_acdoca-saknr BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-txt20 = wa_ska1-txt20.                     "Txt.descr.cta.Razão
**    ENDIF.
*
*    wa_alv_saida-augdt                   = wa_acdoca-augdt.  "Data de compensação
*    wa_alv_saida-augbl                   = wa_acdoca-augbl.  "Doc.compensação
**    wa_alv_saida-rcntr                   = wa_acdoca-rcntr.  "Centro custo
*    wa_alv_saida-rcntr                   = wa_acdoca-kostl.
*
*    READ TABLE t_cskt INTO wa_cskt WITH KEY kostl = wa_acdoca-kostl BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-ltext = wa_cskt-ltext.                     "Nome Centro de Custo
*    ENDIF.
*
*    wa_alv_saida-prctr                   = wa_acdoca-prctr.  "Centro de lucro
***   wa_alv_saida-anln1                   = wa_acdoca-anln1.  "Imobilizado BUG - 181038 - CBRAND
*
**    READ TABLE t_vbak INTO wa_vbak WITH KEY vbeln = wa_acdoca-vbel2 BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-vkbur = wa_vbak-vkbur.                     "Esc Venda
**      wa_alv_saida-auart = wa_vbak-auart.                     "TP.OV.
*
*
**      READ TABLE t_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbak-vkbur BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        wa_alv_saida-bezei = wa_tvkbt-bezei.                    "Nome Esc Venda
**      ENDIF.
**
**      READ TABLE t_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        wa_alv_saida-desc_ov = wa_tvakt-bezei.                   "Descrição Tipo OV
**      ENDIF.
**
**    ENDIF.
*
**    wa_alv_saida-kdauf                   = wa_acdoca-vbel2.  "Ordem Venda
*    wa_alv_saida-rassc                   = wa_acdoca-vbund.  "Soc.parc.negócios
*
**    READ TABLE t_vbkd INTO wa_vbkd WITH KEY vbeln = wa_acdoca-vbel2 BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-bstkd = wa_vbkd-bstkd.                     "Texto Pedido (SD)
**    ENDIF.
*
*    wa_alv_saida-matnr                   = wa_bseg-matnr.  "Material
*
*    READ TABLE t_makt INTO wa_makt WITH KEY matnr = wa_bseg-matnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-maktx  = wa_makt-maktx.                   "Descrição Material
*    ENDIF.
*
*    READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_bseg-matnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-wgbez = wa_t023t-wgbez.                   "Descrição Grupo Mercadoria
*      ENDIF.
*    ENDIF.
*
*    wa_alv_saida-hbkid                   = wa_acdoca-hbkid.     "Banco Empresa
*    wa_alv_saida-awref_rev               = wa_bkpf-stblg. "Estorno com
*
*    IF     wa_acdoca-shkzg = 'S'.
*      wa_alv_saida-cod_deb_cred = 'Débito'.                            "Descrição Cód.débito/crédito
*    ELSEIF wa_acdoca-shkzg = 'H'.
*      wa_alv_saida-cod_deb_cred = 'Crédito'.                           "Descrição Cód.débito/crédito
*    ENDIF.
*
*    PACK wa_acdoca-hkont TO wa_acdoca-hkont. CONDENSE wa_acdoca-hkont.
*    IF     wa_acdoca-hkont(01) EQ '1'.
*      wa_alv_saida-natureza_cto_razao = 'Ativo'.                        "Natureza Conta Razão
*    ELSEIF wa_acdoca-hkont(01) EQ '2'.
*      wa_alv_saida-natureza_cto_razao = 'Passivo'.                      "Natureza Conta Razão
*    ENDIF.
*
*
*    IF NOT wa_acdoca-kunnr IS INITIAL.
*      READ TABLE t_knb1 INTO wa_knb1 WITH KEY kunnr = wa_acdoca-kunnr bukrs = wa_acdoca-bukrs BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-akont = wa_knb1-akont.                   "Cadastro Cta.de reconciliação
*      ENDIF.
*    ENDIF.
*
**    IF NOT wa_acdoca-lifnr IS INITIAL.
**      READ TABLE t_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_acdoca-lifnr bukrs = wa_acdoca-bukrs BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        wa_alv_saida-akont = wa_lfb1-akont.                   "Cadastro Cta.de reconciliação
**      ENDIF.
**    ENDIF.
*
**** BUG - 171955 - Inicio - CBRAND
**    CLEAR: wa_zfit0208.
**    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY blart = wa_acdoca-blart
**                                                    bschl = wa_acdoca-bschl.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-pdd = wa_zfit0208-pdd.                       "PDD
**      CONCATENATE wa_zfit0208-cod_operacao wa_zfit0208-desc_cod_operacao
**      INTO wa_alv_saida-tp_operacao SEPARATED BY space.         "Tipo de Operação
**
**    ELSE.
***      wa_alv_saida-tp_operacao = 'Sem parametro para esse documento'."// WBARBOSA 17/12/2024 - US-83808
**      wa_alv_saida-tp_operacao = 'DELETAR'."// WBARBOSA 17/12/2024 - US-83808
**    ENDIF.
**** BUG - 171955 - Fim - CBRAND
*
*    IF wa_acdoca-blart(02) EQ 'VC'.
*      wa_alv_saida-classificacao_st = '1. PROVISAO'.          "Classificação Status
*    ELSEIF NOT wa_acdoca-augbl IS INITIAL.
*      wa_alv_saida-classificacao_st = '2. COMPENSADO'.        "Classificação Status
*** BUG - 178687 - CBRAND - Inicio
**    ELSEIF wa_acdoca-zfbdt GT wa_augdt-low.
*    ELSEIF wa_acdoca-zfbdt GE s_dtp-low.
*** BUG - 178687 - CBRAND - Fim
*      wa_alv_saida-classificacao_st = '3. EM ANDAMENTO'.      "Classificação Status
*** BUG - 178687 - CBRAND - Inicio
**    ELSEIF wa_acdoca-zfbdt LT wa_augdt-low.
*    ELSEIF wa_acdoca-zfbdt LT  s_dtp-low.
*** BUG - 178687 - CBRAND - Inicio
*      wa_alv_saida-classificacao_st = '4. VENCIDO'.           "Classificação Status
*    ENDIF.
*
*** BUG - 178687 - CBRAND - Inicio
*    IF wa_acdoca-zfbdt < v_data_p AND wa_acdoca-zfbdt  IS NOT INITIAL.
*** BUG - 178687 - CBRAND - Fim
*
*      CLEAR v_day.
*      CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
*        EXPORTING
*          date1            = wa_acdoca-zfbdt
*          time1            = sy-uzeit
*          date2            = v_data_p "wa_augdt-low BUG - 178687 - CBRAND
*          time2            = sy-uzeit
*        IMPORTING
*          datediff         = v_day
*        EXCEPTIONS
*          invalid_datetime = 1
*          OTHERS           = 2.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*
*      IF     v_day GT '365'.
*        wa_alv_saida-aging_list = 'Vencido +365 dias'.  "Aging List
*      ELSEIF v_day EQ '180'.
*        wa_alv_saida-aging_list = 'Vencidos até 180 dias'.  "Aging List
*        "ELSEIF v_day EQ '30'. BUG - 178687 - CBRAND
*      ELSEIF v_day LE '30'.
*        wa_alv_saida-aging_list = 'Vencidos até 30 dias'.  "Aging List
*      ELSEIF v_day EQ '365'.
*        wa_alv_saida-aging_list = 'Vencidos até 365 dias'.  "Aging List
*      ELSEIF v_day EQ '60'.
*        wa_alv_saida-aging_list = 'Vencidos até 60 dias'.  "Aging List
*      ELSEIF v_day EQ '90'.
*        wa_alv_saida-aging_list = 'Vencidos até 90 dias'.  "Aging List
*      ELSE.
*        wa_alv_saida-aging_list = 'A vencer'.  "Aging List
*      ENDIF.
*** BUG - 178687 - CBRAND - Inicio
*    ELSE.
*      IF wa_acdoca-zfbdt >= v_data_p.
*        wa_alv_saida-aging_list = 'A vencer'.
*      ENDIF.
*    ENDIF.
*** BUG - 178687 - CBRAND - Fim
*
*
*    CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr INTO v_objkey.
*    ls_obj-instid = v_objkey.
*    ls_obj-typeid = 'BKPF'.
*    ls_obj-catid  = 'BO'.
*
*    CLEAR lt_reltyp[].
*    ls_reltyp-sign   = 'I'.
*    ls_reltyp-option = 'EQ'.
*    ls_reltyp-low    = 'NOTE'.
*    APPEND ls_reltyp TO lt_reltyp.
*
*    TRY.
*        CLEAR lt_links[].
*        CALL METHOD cl_binary_relation=>read_links
*          EXPORTING
*            is_object           = ls_obj
*            it_relation_options = lt_reltyp
*          IMPORTING
*            et_links            = lt_links.
*      CATCH cx_obl_parameter_error .
*      CATCH cx_obl_internal_error .
*      CATCH cx_obl_model_error .
*    ENDTRY.
*
*    READ TABLE lt_links INTO ls_link INDEX 1.
*    IF sy-subrc EQ 0.
*      CLEAR lt_objcontent[].
*      lv_docid = ls_link-instid_b.
*      CALL FUNCTION 'SO_DOCUMENT_READ_API1'
*        EXPORTING
*          document_id                = lv_docid
*          "filter                     = 'X '
*        IMPORTING
*          document_data              = ls_foldinfo
*        TABLES
*          object_content             = lt_objcontent
*        EXCEPTIONS
*          document_id_not_exist      = 1
*          operation_no_authorization = 2
*          x_error                    = 3
*          OTHERS                     = 4.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*      READ TABLE lt_objcontent INTO ls_objcontent INDEX 1.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-historico = ls_objcontent-line. "Histórico
*      ENDIF.
*    ENDIF.
*
*    CLEAR v_objkey.
*    v_classname = 'BKPF'.
*    CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr INTO v_objkey.
*    v_client    = sy-mandt.
*    CLEAR t_gos_connections[].
*    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*      EXPORTING
*        classname          = v_classname
*        objkey             = v_objkey
*        client             = v_client
*      TABLES
*        gos_connections    = t_gos_connections
*      EXCEPTIONS
*        no_objects_found   = 1
*        internal_error     = 2
*        internal_gos_error = 3.
*
*    IF lines( t_gos_connections ) > 0.
*      wa_alv_saida-anexo = icon_attachment. "Anexo
*    ELSE.
*      wa_alv_saida-anexo = icon_aggregate. "Anexo
*    ENDIF.
*
**** BUG - 181038 - CBRAND - Inicio
**    READ TABLE t_zsdt0159 INTO wa_zsdt0159 WITH KEY adiant = wa_acdoca-belnr BINARY SEARCH.
**    IF sy-subrc EQ 0.
**
**      READ TABLE t_zsdt0041 INTO wa_zsdt0041 WITH KEY doc_simulacao = wa_zsdt0159-doc_simulacao BINARY SEARCH.
**      IF sy-subrc EQ 0.
*
*    READ TABLE t_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_acdoca-vbel2 BINARY SEARCH.
*    IF sy-subrc EQ 0.
**** BUG - 181038 - CBRAND - FIm
*      READ TABLE t_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao BINARY SEARCH. " wa_zsdt0159-doc_simulacao BINARY SEARCH. "BUG - 181038 - CBRAND
*
*      IF sy-subrc EQ 0.
*        wa_alv_saida-cultura = wa_zsdt0040-cultura.
*        wa_alv_saida-safra   = wa_zsdt0040-safra.
*
*        IF wa_zsdt0041-spart = '02'.
*          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_fet.
*        ELSEIF wa_zsdt0041-spart = '03'.
*          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_def.
*        ELSEIF wa_zsdt0041-spart = '04'.
*          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_sem.
*        ENDIF.
*
*      ENDIF.
*
*      "ENDIF. BUG - 181038 - CBRAND
*
*    ELSE.
*      "Seleciona dados da OV / Criadas na transação ZSDT0062. / AOENNING
*
*      READ TABLE it_zsdt0053 INTO ws_zsdt0053 WITH KEY vbeln = wa_acdoca-vbel2.  "wa_bkpf-xblnr. "BUG - 181038 - CBRAND
*      IF sy-subrc EQ 0.
*        wa_alv_saida-safra   = ws_zsdt0053-charg.
*
*        READ TABLE it_zsdt0051 INTO ws_zsdt0051 WITH KEY nro_sol_ov = ws_zsdt0053-nro_sol_ov .
*        IF sy-subrc EQ 0.
*          wa_alv_saida-dt_final_entrega = ws_zsdt0051-dtde_logist.
*          READ TABLE it_zsdt0062 INTO ws_zsdt0062 WITH KEY nro_sol = ws_zsdt0051-nro_sol_ov.
*          IF sy-subrc EQ 0.
**            wa_alv_saida-dt_final_entrega = ws_zsdt0062-dtde_logist.
*          ENDIF.
*        ENDIF.
*      ELSE.
*** "BUG - 181038 - Inicio - CBRAND
*
*        READ TABLE t_zsdt0090 INTO wa_zsdt0090 WITH KEY vbeln = wa_acdoca-vbel2 BINARY SEARCH.
*        IF sy-subrc EQ 0.
*
*          READ TABLE t_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0090-doc_simulacao BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            wa_alv_saida-cultura = wa_zsdt0040-cultura.
*            wa_alv_saida-safra   = wa_zsdt0040-safra.
*
*            IF wa_zsdt0041-spart = '02'.
*              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_fet.
*            ELSEIF wa_zsdt0041-spart = '03'.
*              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_def.
*            ELSEIF wa_zsdt0041-spart = '04'.
*              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_sem.
*            ENDIF.
*
*          ENDIF.
*        ENDIF.
*      ENDIF.
*** "BUG - 181038 - Fim - CBRAND
*    ENDIF.
*
*    APPEND wa_alv_saida TO t_alv_saidac.
*    CLEAR: wa_alv_saida.
*
*  ENDLOOP.


*-------------------------------------------------
* Não usa mais it_bsak_ik
*  LOOP AT it_bsak_ik INTO DATA(wa_bsak_ik).
*
*    CLEAR wa_alv_saida.
*
*    IF wa_bsak_ik-belnr CA sy-abcde.
*      CONTINUE.
*    ENDIF.
*
*    READ TABLE t_ekko INTO wa_ekko WITH KEY ebeln = wa_bsak_ik-ebeln BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-bsart = wa_ekko-bsart.                    "Tp. Pedido
*    ENDIF.
*
*    READ TABLE t_eket INTO wa_eket WITH KEY ebeln = wa_bsak_ik-ebeln ebelp = wa_bsak_ik-ebelp BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-eindt = wa_eket-eindt.                    "DATA REMESSA DO PEDIDO
*    ENDIF.
*
*    wa_alv_saida-rbukrs         = wa_bsak_ik-bukrs. "Empresa
*
*    READ TABLE t_t001 INTO wa_t001 WITH KEY bukrs = wa_bsak_ik-bukrs BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-nome_empresa = wa_t001-butxt. "Nome Empresa
*    ENDIF.
*
**    IF NOT wa_acdoca-kunnr IS INITIAL.
**      wa_alv_saida-cliente_fornecedor = wa_acdoca-kunnr.      "ID Conta Cliente/ Fornecedor
**      READ TABLE t_kna1 INTO wa_kna1 WITH KEY kunnr = wa_acdoca-kunnr BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        wa_alv_saida-nome_cliente_fornecedor = wa_kna1-name1. "Nome Cliente/Fornecedor
**      ENDIF.
**    ENDIF.
*
*    IF NOT wa_bsak_ik-lifnr IS INITIAL.
*      wa_alv_saida-cliente_fornecedor = wa_bsak_ik-lifnr. "ID Conta Cliente/ Fornecedor
*      READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_bsak_ik-lifnr BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-nome_cliente_fornecedor = wa_lfa1-name1. "Nome Cliente/Fornecedor
*      ENDIF.
*    ENDIF.
*
**    wa_alv_saida-ebeln                   = wa_acdoca-ebeln.  "Doc. Compras
*
*    READ TABLE t_t161t INTO wa_t161t WITH KEY bsart = wa_ekko-bsart BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-batxt = wa_t161t-batxt.                    "Desc. Tp. Pedido
*    ENDIF.
*
*
*    READ TABLE t_t024 INTO wa_t024 WITH KEY ekgrp = wa_ekko-ekgrp BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-eknam = wa_t024-eknam.                     "Nome Comprador
*    ENDIF.
*
*    wa_alv_saida-zuonr                   = wa_bsak_ik-zuonr.  "Atribuição
*    wa_alv_saida-rbusa                   = wa_bsak_ik-gsber.  "Divisão
*
*
*    READ TABLE t_t001w INTO wa_t001w WITH KEY werks = wa_bsak_ik-gsber BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-nome_divisao = wa_t001w-name1.            "Nome Divisao
*    ENDIF.
*
*    READ TABLE t_bkpf INTO wa_bkpf WITH KEY bukrs = wa_bsak_ik-bukrs belnr = wa_bsak_ik-belnr gjahr = wa_bsak_ik-gjahr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*
*      IF wa_bkpf-bstat NE 'S'.
*        CONTINUE.
*      ENDIF.
*
*      wa_alv_saida-xblnr = wa_bkpf-xblnr.            "Referência
*      wa_alv_saida-kursf = wa_bkpf-kurs2.            "Taxa Cambio Efetiva
*      wa_alv_saida-waers = wa_bkpf-waers .           "Moeda do Docuemnto "// WBARBOSA BUG-160850 13/12/2024
*    ENDIF.
*
*    wa_alv_saida-belnr                   = wa_bsak_ik-belnr.  "Nº documento
*    wa_alv_saida-bldat                   = wa_bsak_ik-bldat.  "Data documento
*    wa_alv_saida-budat                   = wa_bsak_ik-budat.  "Data lançamento
**    wa_alv_saida-netdt                   = wa_bsak_ik-infae.  "Vencimento líquido
*    wa_alv_saida-gjahr                   = wa_bsak_ik-gjahr.  "Exercício
*
*
*    READ TABLE t_bseg INTO wa_bseg WITH KEY bukrs = wa_bsak_ik-bukrs
*                                            belnr = wa_bsak_ik-belnr
*                                            gjahr = wa_bsak_ik-gjahr
*                                            lifnr = wa_bsak_ik-lifnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-zterm = wa_bseg-zterm.                      "Cond.Pgto
*      wa_alv_saida-kidno = wa_bseg-kidno.                      "Referência de pagamento
*      wa_alv_saida-xref1 = wa_bseg-xref1.                      "Chave referência 1
*      wa_alv_saida-xref2 = wa_bseg-xref2.                      "Chave referência 3
*      wa_alv_saida-zlspr = wa_bseg-zlspr.                      "Bloqueio pgto.
*      wa_alv_saida-bvtyp = wa_bseg-bvtyp.                      "Tipo Banco Parceiro
*      wa_alv_saida-zlsch = wa_bseg-zlsch.                      "Forma Pagamento
*      wa_alv_saida-ebeln = wa_bseg-ebeln.                      "Documento de Compra  "// WBARBOSA BUG-160850 13/12/2024
*      wa_alv_saida-anln1 = wa_bseg-anln1.  "Imobilizado BUG - 181038 - CBRAND
*    ENDIF.
*
*    READ TABLE t_bseg INTO wa_bseg WITH KEY bukrs = wa_bsak_ik-bukrs
*                                            belnr = wa_bsak_ik-belnr
*                                            gjahr = wa_bsak_ik-gjahr
*                                            lifnr = wa_bsak_ik-lifnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-zterm = wa_bseg-zterm.                      "Cond.Pgto
*      wa_alv_saida-kidno = wa_bseg-kidno.                      "Referência de pagamento
*      wa_alv_saida-xref1 = wa_bseg-xref1.                      "Chave referência 1
*      wa_alv_saida-xref2 = wa_bseg-xref2.                      "Chave referência 3
*      wa_alv_saida-zlspr = wa_bseg-zlspr.                      "Bloqueio pgto.
*      wa_alv_saida-bvtyp = wa_bseg-bvtyp.                      "Tipo Banco Parceiro
*      wa_alv_saida-zlsch = wa_bseg-zlsch.                      "Forma Pagamento
*      wa_alv_saida-netdt = wa_bseg-netdt.                      "Vencimento líquido
*      wa_alv_saida-ebeln = wa_bseg-ebeln.                      "Documento de Compra  "// WBARBOSA BUG-160850 13/12/2024
*      wa_alv_saida-anln1 = wa_bseg-anln1.  "Imobilizado BUG - 181038 - CBRA
*    ENDIF.
*
*    READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_bseg-matnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-wgbez = wa_t023t-wgbez.                   "Descrição Grupo Mercadoria
*      ENDIF.
*    ENDIF.
*
*
*    wa_alv_saida-rtcur                   = wa_bsak_ik-waers.  "Moeda documento
*    wa_alv_saida-hsl                     = wa_bsak_ik-wrbtr.   "Vlr. Moeda documento
*    wa_alv_saida-rkcur                   = wa_bkpf-hwae2.  "Moeda Forte
*    wa_alv_saida-ksl                     = wa_bsak_ik-dmbtr.    "Vlr. Moeda forte
*    wa_alv_saida-blart                   = wa_bsak_ik-blart.  "Tipo de documento
*    wa_alv_saida-bschl                   = wa_bsak_ik-bschl.  "Chave de lançamento
*    wa_alv_saida-umskz                   = wa_bsak_ik-umskz.  "Cód.Razão Especial
*    wa_alv_saida-sgtxt                   = wa_bsak_ik-sgtxt.  "Texto
*    wa_alv_saida-racct                   = wa_bsak_ik-hkont.  "Conta do Razão
*    wa_alv_saida-txt20 = wa_bsak_ik-hkont.
*
**    READ TABLE t_ska1 INTO wa_ska1 WITH KEY saknr = wa_bsak_ik-hkont BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-txt20 = wa_ska1-txt20.                     "Txt.descr.cta.Razão
**    ENDIF.
*
*    wa_alv_saida-augdt                   = wa_bsak_ik-augdt.  "Data de compensação
*    wa_alv_saida-augbl                   = wa_bsak_ik-augbl.  "Doc.compensação
**    wa_alv_saida-rcntr                   = wa_acdoca-rcntr.  "Centro custo
*    wa_alv_saida-rcntr                   = wa_bsak_ik-kostl.
*
*    READ TABLE t_cskt INTO wa_cskt WITH KEY kostl = wa_bsak_ik-kostl BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-ltext = wa_cskt-ltext.                     "Nome Centro de Custo
*    ENDIF.
*
*    wa_alv_saida-prctr                   = wa_bsak_ik-prctr.  "Centro de lucro
*    wa_alv_saida-anln1                   = wa_bsak_ik-anln1.  "Imobilizado
*
**    READ TABLE t_vbak INTO wa_vbak WITH KEY vbeln = wa_bsak_ik-vbel2 BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-vkbur = wa_vbak-vkbur.                     "Esc Venda
**      wa_alv_saida-auart = wa_vbak-auart.                     "TP.OV.
*
*
*    READ TABLE t_tvkbt INTO wa_tvkbt WITH KEY vkbur = wa_vbak-vkbur BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-bezei = wa_tvkbt-bezei.                    "Nome Esc Venda
*    ENDIF.
*
*    READ TABLE t_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-desc_ov = wa_tvakt-bezei.                   "Descrição Tipo OV
*    ENDIF.
*
**    ENDIF.
*
**    wa_alv_saida-kdauf                   = wa_bsak_ik-vbel2.  "Ordem Venda
*    wa_alv_saida-rassc                   = wa_bsak_ik-vbund.  "Soc.parc.negócios
*
**    READ TABLE t_vbkd INTO wa_vbkd WITH KEY vbeln = wa_acdoca-vbel2 BINARY SEARCH.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-bstkd = wa_vbkd-bstkd.                     "Texto Pedido (SD)
**    ENDIF.
*
**    wa_alv_saida-matnr                   = wa_acdoca-matnr.  "Material
*
*    READ TABLE t_makt INTO wa_makt WITH KEY matnr = wa_bseg-matnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      wa_alv_saida-maktx  = wa_makt-maktx.                   "Descrição Material
*    ENDIF.
*
*    READ TABLE t_mara INTO wa_mara WITH KEY matnr = wa_bseg-matnr BINARY SEARCH.
*    IF sy-subrc EQ 0.
*      READ TABLE t_t023t INTO wa_t023t WITH KEY matkl = wa_mara-matkl BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-wgbez = wa_t023t-wgbez.                   "Descrição Grupo Mercadoria
*      ENDIF.
*    ENDIF.
*
*    wa_alv_saida-hbkid                   = wa_bsak_ik-hbkid.     "Banco Empresa
*    wa_alv_saida-awref_rev               = wa_bkpf-stblg. "Estorno com
*
*    IF     wa_bsak_ik-shkzg = 'S'.
*      wa_alv_saida-cod_deb_cred = 'Débito'.                            "Descrição Cód.débito/crédito
*    ELSEIF wa_bsak_ik-shkzg = 'H'.
*      wa_alv_saida-cod_deb_cred = 'Crédito'.                           "Descrição Cód.débito/crédito
*    ENDIF.
*
*    PACK wa_bsak_ik-hkont TO wa_bsak_ik-hkont. CONDENSE wa_bsak_ik-hkont.
*    IF     wa_bsak_ik-hkont(01) EQ '1'.
*      wa_alv_saida-natureza_cto_razao = 'Ativo'.                        "Natureza Conta Razão
*    ELSEIF wa_bsak_ik-hkont(01) EQ '2'.
*      wa_alv_saida-natureza_cto_razao = 'Passivo'.                      "Natureza Conta Razão
*    ENDIF.
*
*
**    IF NOT wa_acdoca-kunnr IS INITIAL.
**      READ TABLE t_knb1 INTO wa_knb1 WITH KEY kunnr = wa_acdoca-kunnr bukrs = wa_acdoca-bukrs BINARY SEARCH.
**      IF sy-subrc EQ 0.
**        wa_alv_saida-akont = wa_knb1-akont.                   "Cadastro Cta.de reconciliação
**      ENDIF.
**    ENDIF.
*
*    IF NOT wa_bsak_ik-lifnr IS INITIAL.
*      READ TABLE t_lfb1 INTO wa_lfb1 WITH KEY lifnr = wa_bsak_ik-lifnr bukrs = wa_bsak_ik-bukrs BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-akont = wa_lfb1-akont.                   "Cadastro Cta.de reconciliação
*      ENDIF.
*    ENDIF.
*
**** BUG - 171955 - Inicio - CBRAND
**    CLEAR: wa_zfit0208.
**    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY blart = wa_bsak_ik-blart
**                                                    bschl = wa_bsak_ik-bschl.
**    IF sy-subrc EQ 0.
**      wa_alv_saida-pdd = wa_zfit0208-pdd.                       "PDD
**      CONCATENATE wa_zfit0208-cod_operacao wa_zfit0208-desc_cod_operacao
**      INTO wa_alv_saida-tp_operacao SEPARATED BY space.         "Tipo de Operação
**
**    ELSE.
***      wa_alv_saida-tp_operacao = 'Sem parametro para esse documento'."// WBARBOSA 17/12/2024 - US-83808
**      wa_alv_saida-tp_operacao = 'DELETAR'."// WBARBOSA 17/12/2024 - US-83808
**    ENDIF.
**** BUG - 171955 - Fim - CBRAND
*
*    IF wa_bsak_ik-blart(02) EQ 'VC'.
*      wa_alv_saida-classificacao_st = '1. PROVISAO'.          "Classificação Status
*    ELSEIF NOT wa_bsak_ik-augbl IS INITIAL.
*      wa_alv_saida-classificacao_st = '2. COMPENSADO'.        "Classificação Status
*** BUG - 178687 - CBRAND - Inicio
**    ELSEIF wa_bsak_ik-zfbdt GT wa_augdt-low.
*    ELSEIF wa_bsak_ik-zfbdt GE  s_dtp-low.
*** BUG - 178687 - CBRAND - Fim
*      wa_alv_saida-classificacao_st = '3. EM ANDAMENTO'.      "Classificação Status
*** BUG - 178687 - CBRAND - Inicio
**   ELSEIF wa_bsak_ik-zfbdt LT wa_augdt-low.
*    ELSEIF wa_bsak_ik-zfbdt LT  s_dtp-low.
*** BUG - 178687 - CBRAND - Inicio
*      wa_alv_saida-classificacao_st = '4. VENCIDO'.           "Classificação Status
*    ENDIF.
*
*** BUG - 178687 - CBRAND - Inicio
*    IF wa_bsak_ik-zfbdt < v_data_p AND wa_bsak_ik-zfbdt IS NOT INITIAL.
*** BUG - 178687 - CBRAND - Fim
*      CLEAR v_day.
*      CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
*        EXPORTING
*          date1            = wa_bsak_ik-zfbdt
*          time1            = sy-uzeit
*          date2            = v_data_p "wa_augdt-low BUG - 178687 - CBRAND
*          time2            = sy-uzeit
*        IMPORTING
*          datediff         = v_day
*        EXCEPTIONS
*          invalid_datetime = 1
*          OTHERS           = 2.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*      IF     v_day GT '365'.
*        wa_alv_saida-aging_list = 'Vencido +365 dias'.  "Aging List
*      ELSEIF v_day EQ '180'.
*        wa_alv_saida-aging_list = 'Vencidos até 180 dias'.  "Aging List
*        "ELSEIF v_day EQ '30'. BUG - 178687 - CBRAND
*      ELSEIF v_day LE '30'.
*        wa_alv_saida-aging_list = 'Vencidos até 30 dias'.  "Aging List
*      ELSEIF v_day EQ '365'.
*        wa_alv_saida-aging_list = 'Vencidos até 365 dias'.  "Aging List
*      ELSEIF v_day EQ '60'.
*        wa_alv_saida-aging_list = 'Vencidos até 60 dias'.  "Aging List
*      ELSEIF v_day EQ '90'.
*        wa_alv_saida-aging_list = 'Vencidos até 90 dias'.  "Aging List
*      ELSE.
*        wa_alv_saida-aging_list = 'A vencer'.  "Aging List
*      ENDIF.
*** BUG - 178687 - CBRAND - Inicio
*    ELSE.
*      IF wa_bsak_ik-zfbdt >= v_data_p.
*        wa_alv_saida-aging_list = 'A vencer'.
*      ENDIF.
*    ENDIF.
*** BUG - 178687 - CBRAND - Fim
*
*    CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr INTO v_objkey.
*    ls_obj-instid = v_objkey.
*    ls_obj-typeid = 'BKPF'.
*    ls_obj-catid  = 'BO'.
*
*    CLEAR lt_reltyp[].
*    ls_reltyp-sign   = 'I'.
*    ls_reltyp-option = 'EQ'.
*    ls_reltyp-low    = 'NOTE'.
*    APPEND ls_reltyp TO lt_reltyp.
*
*    TRY.
*        CLEAR lt_links[].
*        CALL METHOD cl_binary_relation=>read_links
*          EXPORTING
*            is_object           = ls_obj
*            it_relation_options = lt_reltyp
*          IMPORTING
*            et_links            = lt_links.
*      CATCH cx_obl_parameter_error .
*      CATCH cx_obl_internal_error .
*      CATCH cx_obl_model_error .
*    ENDTRY.
*
*    READ TABLE lt_links INTO ls_link INDEX 1.
*    IF sy-subrc EQ 0.
*      CLEAR lt_objcontent[].
*      lv_docid = ls_link-instid_b.
*      CALL FUNCTION 'SO_DOCUMENT_READ_API1'
*        EXPORTING
*          document_id                = lv_docid
*          "filter                     = 'X '
*        IMPORTING
*          document_data              = ls_foldinfo
*        TABLES
*          object_content             = lt_objcontent
*        EXCEPTIONS
*          document_id_not_exist      = 1
*          operation_no_authorization = 2
*          x_error                    = 3
*          OTHERS                     = 4.
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
*
*      READ TABLE lt_objcontent INTO ls_objcontent INDEX 1.
*      IF sy-subrc EQ 0.
*        wa_alv_saida-historico = ls_objcontent-line. "Histórico
*      ENDIF.
*    ENDIF.
*
*    CLEAR v_objkey.
*    v_classname = 'BKPF'.
*    CONCATENATE wa_alv_saida-rbukrs wa_alv_saida-belnr wa_alv_saida-gjahr INTO v_objkey.
*    v_client    = sy-mandt.
*    CLEAR t_gos_connections[].
*    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*      EXPORTING
*        classname          = v_classname
*        objkey             = v_objkey
*        client             = v_client
*      TABLES
*        gos_connections    = t_gos_connections
*      EXCEPTIONS
*        no_objects_found   = 1
*        internal_error     = 2
*        internal_gos_error = 3.
*
*    IF lines( t_gos_connections ) > 0.
*      wa_alv_saida-anexo = icon_attachment. "Anexo
*    ELSE.
*      wa_alv_saida-anexo = icon_aggregate. "Anexo
*    ENDIF.
*
**** BUG - 181038 - CBRAND - Inicio
**    READ TABLE t_zsdt0159 INTO wa_zsdt0159 WITH KEY adiant = wa_bsak_ik-belnr BINARY SEARCH.
**    IF sy-subrc EQ 0.
**
**      READ TABLE t_zsdt0041 INTO wa_zsdt0041 WITH KEY doc_simulacao = wa_zsdt0159-doc_simulacao BINARY SEARCH.
*
*    READ TABLE t_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_acdoca-vbel2 BINARY SEARCH.
*    IF sy-subrc EQ 0.
*
*      READ TABLE t_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao BINARY SEARCH.
**        READ TABLE t_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0159-doc_simulacao BINARY SEARCH.
**** BUG - 181038 - CBRAND - FIm
*      IF sy-subrc EQ 0.
*        wa_alv_saida-cultura = wa_zsdt0040-cultura.
*        wa_alv_saida-safra   = wa_zsdt0040-safra.
*
*        IF wa_zsdt0041-spart = '02'.
*          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_fet.
*        ELSEIF wa_zsdt0041-spart = '03'.
*          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_def.
*        ELSEIF wa_zsdt0041-spart = '04'.
*          wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_sem.
*        ENDIF.
*
*      ENDIF.
*
*      "ENDIF. "BUG - 181038 - CBRAND
*
*    ELSE.
*      "Seleciona dados da OV
*
*      READ TABLE it_zsdt0053 INTO ws_zsdt0053 WITH KEY vbeln = wa_acdoca-vbel2. "wa_bkpf-xblnr. "BUG - 181038 - CBRAND
*      IF sy-subrc EQ 0.
*        wa_alv_saida-safra   = ws_zsdt0053-charg.
*
*        READ TABLE it_zsdt0051 INTO ws_zsdt0051 WITH KEY nro_sol_ov = ws_zsdt0053-nro_sol_ov .
*        IF sy-subrc EQ 0.
*          wa_alv_saida-dt_final_entrega = ws_zsdt0051-dtde_logist.
*          READ TABLE it_zsdt0062 INTO ws_zsdt0062 WITH KEY nro_sol = ws_zsdt0051-nro_sol_ov.
*          IF sy-subrc EQ 0.
**            wa_alv_saida-dt_final_entrega = ws_zsdt0062-dtde_logist.
*          ENDIF.
*        ENDIF.
*** "BUG - 181038 - Inicio - CBRAND
*      ELSE.
*
*        READ TABLE t_zsdt0090 INTO wa_zsdt0090 WITH KEY vbeln = wa_acdoca-vbel2 BINARY SEARCH.
*        IF sy-subrc EQ 0.
*
*          READ TABLE t_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0090-doc_simulacao BINARY SEARCH.
*          IF sy-subrc EQ 0.
*            wa_alv_saida-cultura = wa_zsdt0040-cultura.
*            wa_alv_saida-safra   = wa_zsdt0040-safra.
*
*            IF wa_zsdt0041-spart = '02'.
*              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_fet.
*            ELSEIF wa_zsdt0041-spart = '03'.
*              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_def.
*            ELSEIF wa_zsdt0041-spart = '04'.
*              wa_alv_saida-dt_final_entrega = wa_zsdt0040-dt_entrega_sem.
*            ENDIF.
*
*          ENDIF.
*        ENDIF.
*      ENDIF.
*** "BUG - 181038 - Fim - CBRAND
*    ENDIF.
*
*    APPEND wa_alv_saida TO t_alv_saidac.
*    CLEAR: wa_alv_saida.
*
*  ENDLOOP.

*  DELETE t_alv_saidac WHERE tp_operacao EQ 'DELETAR'. "// WBARBOSA 17/12/2024 - US-83808

  IF t_alv_saidac[] IS NOT INITIAL AND t_alv_saida[] IS NOT INITIAL.
    LOOP AT t_alv_saidac ASSIGNING FIELD-SYMBOL(<fs_saidac>).
      APPEND INITIAL LINE TO t_alv_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
      <fs_saida> = <fs_saidac>.
    ENDLOOP.
  ELSEIF t_alv_saidac[] IS NOT INITIAL.
    t_alv_saida[] = t_alv_saidac[].
  ENDIF.

ENDFORM.
*-US 155522-23-10-2024-#155522-RJF-Fim

*&---------------------------------------------------------------------*
*& Form f_seleciona_classificacao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_seleciona_classificacao .

  DATA: text_tab TYPE TABLE OF string.
  DATA: lv_saknr(10) TYPE n.

  wa_augdt-sign   = 'I'.
  wa_augdt-option = 'GE'.

  wa_augdt_sp-sign   = 'I'.
  wa_augdt_sp-option = 'EQ'.
  APPEND wa_augdt_sp TO rg_augdt_sp.

  LOOP AT s_dtp.
    IF NOT s_dtp-low IS INITIAL.
      CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
        EXPORTING
          date      = s_dtp-low
          days      = 1
          months    = 0
          signum    = '+'
          years     = 0
        IMPORTING
          calc_date = wa_augdt-low.
      APPEND wa_augdt TO rg_augdt.
    ENDIF.
  ENDLOOP.

**** BUG - 171955 - CBRAND - Inicio
*  " Selecionar Parâmetros de seleção cockpit composição conta razão
*  SELECT cod_operacao saknr bschl blart matkl bsart kostl lifnr drcrk gkont bukrs desc_cod_operacao desc_saknr atribuicao pdd
*         FROM zfit0208
*         INTO TABLE t_zfit0208.
*  "//WSB RET
**** BUG - 171955 - CBRAND - Fim

  LOOP AT t_zfit0208 INTO wa_zfit0208.

    IF NOT wa_zfit0208-saknr IS INITIAL.
*      WA_RACCT-SIGN   = 'I'.
*      WA_RACCT-OPTION = 'EQ'.
*      WA_RACCT-LOW    = WA_ZFIT0208-SAKNR.
*      APPEND WA_RACCT TO RG_RACCT.
    ELSE.
      wa_racct-sign   = 'I'.
      wa_racct-option = 'CP'.
      wa_racct-low    = '*'.
      APPEND wa_racct TO rg_racct.
    ENDIF.

**** BUG - 171955 - Inicio - CBRAND
*    IF NOT wa_zfit0208-bschl IS INITIAL.
*      wa_bschl-sign   = 'I'.
*      wa_bschl-option = 'EQ'.
*      wa_bschl-low    = wa_zfit0208-bschl.
*      APPEND wa_bschl TO rg_bschl.
*    ELSE.
*      wa_bschl-sign   = 'I'.
*      wa_bschl-option = 'CP'.
*      wa_bschl-low    = '*'.
*      APPEND wa_bschl TO rg_bschl.
*    ENDIF.
*
*    IF NOT wa_zfit0208-blart IS INITIAL.
*      wa_blart-sign   = 'I'.
*      wa_blart-option = 'EQ'.
*      wa_blart-low    = wa_zfit0208-blart.
*      APPEND wa_blart TO rg_blart.
*    ELSE.
*      wa_blart-sign   = 'I'.
*      wa_blart-option = 'CP'.
*      wa_blart-low    = '*'.
*      APPEND wa_blart TO rg_blart.
*    ENDIF.
*
*    IF NOT wa_zfit0208-matkl IS INITIAL.
*      wa_matkl-sign   = 'I'.
*      wa_matkl-option = 'EQ'.
*      wa_matkl-low    = wa_zfit0208-matkl.
*      APPEND wa_matkl TO rg_matkl.
*    ELSE.
*      wa_matkl-sign   = 'I'.
*      wa_matkl-option = 'CP'.
*      wa_matkl-low    = '*'.
*      APPEND wa_matkl TO rg_matkl.
*    ENDIF.
**** BUG - 171955 - Fim - CBRAND
    FREE text_tab.
    IF NOT wa_zfit0208-saknr IS INITIAL.

      SPLIT wa_zfit0208-saknr  AT ',' INTO TABLE text_tab.

      LOOP AT text_tab INTO DATA(ls_saknr).
        lv_saknr = ls_saknr.   " 169346 - 19.03.2025
        ls_saknr = lv_saknr.   " 169346 - 19.03.2025
        APPEND
        VALUE #(
                  sign   = 'I'
                  option = 'EQ'
                  low    = ls_saknr
               ) TO rg_racct.
        CLEAR: ls_saknr.  "171955 - CBRAND
      ENDLOOP.

      DELETE rg_racct WHERE low NOT IN s_saknr.

    ELSE.
      APPEND
      VALUE #(
                sign   = 'I'
                option = 'CP'
                low    = '*'
             ) TO rg_racct.
    ENDIF.

**** BUG - 171955 - Inicio - CBRAND
*    FREE text_tab.
*    IF NOT wa_zfit0208-bsart IS INITIAL.
*
*      SPLIT wa_zfit0208-bsart  AT ',' INTO TABLE text_tab.
*
*      wa_bsart-sign   = 'I'.
*      wa_bsart-option = 'EQ'.
*
*      LOOP AT text_tab INTO DATA(ls_tab).
*        wa_bsart-low    = ls_tab.
*        APPEND wa_bsart TO rg_bsart.
*      ENDLOOP.
*
*    ELSE.
*      wa_bsart-sign   = 'I'.
*      wa_bsart-option = 'CP'.
*      wa_bsart-low    = '*'.
*      APPEND wa_bsart TO rg_bsart.
*    ENDIF.
*
*    IF NOT wa_zfit0208-kostl IS INITIAL.
*      wa_rcntr-sign   = 'I'.
*      wa_rcntr-option = 'EQ'.
*      wa_rcntr-low    = wa_zfit0208-kostl.
*      APPEND wa_rcntr TO rg_rcntr.
*    ELSE.
*      wa_rcntr-sign   = 'I'.
*      wa_rcntr-option = 'CP'.
*      wa_rcntr-low    = '*'.
*      APPEND wa_rcntr TO rg_rcntr.
*    ENDIF.
*
*    IF NOT wa_zfit0208-atribuicao IS INITIAL.
*      wa_zuonr-sign   = 'I'.
*      wa_zuonr-option = 'EQ'.
*      wa_zuonr-low    = wa_zfit0208-atribuicao.
*      APPEND wa_zuonr TO rg_zuonr.
*    ELSE.
*      wa_zuonr-sign   = 'I'.
*      wa_zuonr-option = 'CP'.
*      wa_zuonr-low    = '*'.
*      APPEND wa_zuonr TO rg_zuonr.
*    ENDIF.
*
*    IF s_lifnr[] IS INITIAL.
*      IF NOT wa_zfit0208-lifnr IS INITIAL.
*        wa_fornecedor-sign   = 'I'.
*        wa_fornecedor-option = 'EQ'.
*        wa_fornecedor-low    = wa_zfit0208-lifnr.
*        APPEND wa_fornecedor TO rg_fornecedor.
*      ELSE.
*        wa_fornecedor-sign   = 'I'.
*        wa_fornecedor-option = 'CP'.
*        wa_fornecedor-low    = '*'.
*        APPEND wa_fornecedor TO rg_fornecedor.
*      ENDIF.
*    ENDIF.
**** BUG - 171955 - Fim - CBRAND
*    if s_kunnr[] is initial.
*      if not wa_zfit0208-lifnr is initial.
*        wa_fornecedor-sign   = 'I'.
*        wa_fornecedor-option = 'EQ'.
*        wa_fornecedor-low    = wa_zfit0208-lifnr.
*        append wa_fornecedor to rg_fornecedor.
*      else.
*        wa_fornecedor-sign   = 'I'.
*        wa_fornecedor-option = 'CP'.
*        wa_fornecedor-low    = '*'.
*      endif.
*    endif.

**** BUG - 171955 - Inicio - CBRAND
*    IF NOT wa_zfit0208-drcrk IS INITIAL.
*      wa_drcrk-sign   = 'I'.
*      wa_drcrk-option = 'EQ'.
*      wa_drcrk-low    = wa_zfit0208-drcrk.
*      APPEND wa_drcrk TO rg_drcrk.
*    ELSE.
*      wa_drcrk-sign   = 'I'.
*      wa_drcrk-option = 'CP'.
*      wa_drcrk-low    = '*'.
*      APPEND wa_drcrk TO rg_drcrk.
*    ENDIF.
*
*    IF NOT wa_zfit0208-gkont IS INITIAL.
*      wa_gkont-sign   = 'I'.
*      wa_gkont-option = 'EQ'.
*      wa_gkont-low    = wa_zfit0208-gkont.
*      APPEND wa_gkont TO rg_gkont.
*    ELSE.
*      wa_gkont-sign   = 'I'.
*      wa_gkont-option = 'CP'.
*      wa_gkont-low    = '*'.
*      APPEND wa_gkont TO rg_gkont.
*    ENDIF.
**** BUG - 171955 - Fim - CBRAND
  ENDLOOP.
*** BUG - 178687 - CBRAND - Inicio
*  FREE rg_fornecedor.
*  LOOP AT s_lifnr."145997 -  DEVK9A20VP - FI - CS2022000723 - Ajuste - RSA
*    wa_fornecedor-sign   = 'I'.
*    wa_fornecedor-option = 'EQ'.
*    wa_fornecedor-low    = s_lifnr-low.
*    APPEND wa_fornecedor TO rg_fornecedor.
*  ENDLOOP.
*
*  LOOP AT s_kunnr."145997 -  DEVK9A20VP - FI - CS2022000723 - Ajuste - RSA
*    wa_fornecedor-sign   = 'I'.
*    wa_fornecedor-option = 'EQ'.
*    wa_fornecedor-low    =  s_kunnr-low.
*    APPEND wa_fornecedor TO rg_fornecedor.
*  ENDLOOP.
*** BUG - 178687 - CBRAND - Fim
  SORT rg_racct BY low.
  DELETE ADJACENT DUPLICATES FROM rg_racct COMPARING low.

  SORT rg_bschl BY low.
  DELETE ADJACENT DUPLICATES FROM rg_bschl COMPARING low.

  SORT rg_blart BY low.
  DELETE ADJACENT DUPLICATES FROM rg_blart COMPARING low.

  SORT rg_matkl BY low.
  DELETE ADJACENT DUPLICATES FROM rg_matkl COMPARING low.

  SORT rg_bsart BY low.
  DELETE ADJACENT DUPLICATES FROM rg_bsart COMPARING low.

  SORT rg_rcntr BY low.
  DELETE ADJACENT DUPLICATES FROM rg_rcntr COMPARING low.

  SORT rg_zuonr BY low.
  DELETE ADJACENT DUPLICATES FROM rg_zuonr COMPARING low.

  SORT rg_fornecedor BY low.
  DELETE ADJACENT DUPLICATES FROM rg_fornecedor COMPARING low.

  SORT rg_drcrk BY low.
  DELETE ADJACENT DUPLICATES FROM rg_drcrk COMPARING low.

  SORT rg_gkont BY low.
  DELETE ADJACENT DUPLICATES FROM rg_gkont COMPARING low.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_classificacao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_classificacao CHANGING p_alv_saida TYPE ty_alv_saida.

* BUG - 171955  - Inicio - CBRAND
  DATA: lv_saknr TYPE zfit0208-saknr,
        lv_bsart TYPE zfit0208-bsart,
        lv_lifnr TYPE zfit0208-lifnr. "BUG - 181038 - CSB
  DATA :BEGIN OF lwa_string,
          col1 TYPE char50,
        END     OF lwa_string.

  DATA: lit_string LIKE STANDARD TABLE OF lwa_string.
  DATA: lit_string_lif LIKE STANDARD TABLE OF lwa_string.
  DATA: lr_bsart TYPE RANGE OF zfit0208-bsart.
  DATA: lr_lifnr TYPE RANGE OF zfit0208-lifnr. "BUG - 181038 - CSB



  CLEAR: p_alv_saida-tp_operacao.

  lv_saknr = |{ p_alv_saida-racct ALPHA = OUT }|.
*  lv_bsart = p_alv_saida-bsart." Rubenilson Pereira - 25.07.25 #185551
  lv_bsart = p_alv_saida-auart. " Rubenilson Pereira - 25.07.25 #185551

  filter_saknr = VALUE #( ( lv_saknr  ) ).
  filter_bschl = VALUE #( ( p_alv_saida-bschl ) ).
  filter_blart = VALUE #( ( p_alv_saida-blart ) ).
  filter_matkl = VALUE #( ( p_alv_saida-matkl ) ).
  filter_bsart = VALUE #( ( lv_bsart ) ).
  filter_lifnr = VALUE #( ( lv_lifnr ) ). "BUG - 181038 - CSB

*** BUG - 181038 - Inicio - CBRAND
  filter_anln1 = VALUE #( ( p_alv_saida-anln1 ) ).
  filter_sgtxt = VALUE #( ( p_alv_saida-sgtxt ) ).
  filter_kidno = VALUE #( ( p_alv_saida-kidno ) ).
  filter_xref1 = VALUE #( ( p_alv_saida-xref1 ) ).
*** BUG - 181038 - Fim - CBRAND

  "filter_kostl = VALUE #( ( p_alv_saida-rcntr  ) ). BUG - 181038 - CSB
  "filter_lifnr = VALUE #( ( p_alv_saida-cliente_fornecedor ) ). BUG - 181038 - CSB
  "filter_drcrk = VALUE #( ( p_alv_saida-drcrk ) ). BUG - 181038 - CSB
  filter_gkont = VALUE #( ( p_alv_saida-gkont  ) ).
  "filter_bukrs = VALUE #( ( p_alv_saida-rbukrs ) ). BUG - 181038 - CSB

  IF p_alv_saida-racct IS NOT INITIAL.
    DATA(lit_saknr)   = FILTER #( t_zfit0208_class  IN filter_saknr  WHERE saknr = table_line ).
    LOOP AT lit_saknr INTO DATA(lwa_saknr).

      SPLIT lwa_saknr-bsart AT ',' INTO TABLE lit_string.
      SPLIT lwa_saknr-lifnr AT ',' INTO TABLE lit_string_lif.

      lr_bsart = VALUE #( FOR lwa IN lit_string ( sign = 'I' option = 'EQ' low = lwa-col1  ) ).
      lr_lifnr = VALUE #( FOR lwa_lif IN lit_string_lif ( sign = 'I' option = 'EQ' low = lwa_lif-col1  ) ).

* Inicio - FA - 22.09.2025
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = p_alv_saida-cliente_fornecedor
        IMPORTING
          output = p_alv_saida-cliente_fornecedor.
* Fim - FA - 22.09.2025

      IF  lwa_saknr-bschl IS INITIAL  OR lwa_saknr-bschl  = p_alv_saida-bschl.
        IF  lwa_saknr-blart IS INITIAL  OR lwa_saknr-blart  = p_alv_saida-blart.
          IF  lwa_saknr-matkl IS INITIAL  OR lwa_saknr-matkl  = p_alv_saida-matkl.

* Inicio - FA - 22.09.2025
            IF  lwa_saknr-bsart IS INITIAL OR  p_alv_saida-bsart IN lr_bsart[]." Rubenilson Pereira - 25.07.25 #185551
*            IF  lwa_saknr-bsart IS INITIAL OR  p_alv_saida-auart IN lr_bsart[]." Rubenilson Pereira - 25.07.25 #185551
              "IF  lwa_saknr-kostl IS INITIAL  OR lwa_saknr-kostl  = p_alv_saida-rcntr. BUG - 181038 - CSB
              IF  lwa_saknr-lifnr IS INITIAL  OR lwa_saknr-lifnr  =  p_alv_saida-cliente_fornecedor. "BUG - 181038 - CSB
                "IF  lwa_saknr-drcrk IS INITIAL  OR lwa_saknr-drcrk =  p_alv_saida-drcrk. BUG - 181038 - CSB
*              IF  lwa_saknr-lifnr IS INITIAL  OR lwa_saknr-lifnr  IN lr_lifnr[].

* Fim - FA - 22.09.2025
                IF  lwa_saknr-gkont IS INITIAL  OR lwa_saknr-gkont =  p_alv_saida-gkont.
                  "IF  lwa_saknr-bukrs IS INITIAL  OR lwa_saknr-bukrs =  p_alv_saida-rbukrs. BUG - 181038 - CSB
                  IF lwa_saknr-atribuicao IS INITIAL OR ( p_alv_saida-zuonr CP lwa_saknr-atribuicao  ) ."lwa_saknr-atribuicao = p_alv_saida-zuonr.
                    IF lwa_saknr-anln1 IS INITIAL OR lwa_saknr-anln1 = p_alv_saida-anln1 OR  ( lwa_saknr-anln1 = 'X' AND p_alv_saida-anln1 IS NOT INITIAL ). "BUG - 181038 / 184687 - CBRAND
                      IF lwa_saknr-sgtxt IS INITIAL OR ( p_alv_saida-sgtxt CP lwa_saknr-sgtxt ). "lwa_saknr-sgtxt = p_alv_saida-sgtxt. "BUG - 181038 - CBRAND
                        IF lwa_saknr-kidno IS INITIAL OR ( p_alv_saida-kidno CP lwa_saknr-kidno )."lwa_saknr-kidno = p_alv_saida-kidno. "BUG - 181038 - CBRAND
                          IF lwa_saknr-xref1 IS INITIAL OR  ( p_alv_saida-xref1 CP lwa_saknr-xref1 ). "lwa_saknr-xref1 = p_alv_saida-xref1. "BUG - 181038 - CBRAND
*** BUG - 181038 - CBRAND - Inicio
                            "p_alv_saida-tp_operacao = |{ lwa_saknr-cod_operacao } { lwa_saknr-desc_cod_operacao }|.
                            p_alv_saida-tp_operacao = lwa_saknr-desc_cod_operacao.
*** BUG - 181038 - CBRAND - Inicio
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                  "ENDIF.
                ENDIF.
                "ENDIF.
              ENDIF.
              "ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
      IF p_alv_saida-tp_operacao IS INITIAL.
        p_alv_saida-tp_operacao = 'DELETAR'.
      ENDIF.
    ENDLOOP.
  ENDIF.

*** BUG - 181038 - Inicio - CBRAND
  CLEAR: lit_string, lr_bsart[].

  DATA(lit_saknr_sub)   = FILTER #( t_zfit0235_class  IN filter_saknr  WHERE saknr = table_line ).

  LOOP AT lit_saknr_sub INTO DATA(lwa_saknr_sub).
    IF  lwa_saknr_sub-blart IS INITIAL OR  lwa_saknr_sub-blart = p_alv_saida-blart.
      IF lwa_saknr_sub-atribuicao IS INITIAL OR lwa_saknr_sub-atribuicao = p_alv_saida-zuonr.
        IF lwa_saknr_sub-sgtxt IS INITIAL OR lwa_saknr_sub-sgtxt = p_alv_saida-sgtxt.
          IF lwa_saknr_sub-kidno IS INITIAL OR lwa_saknr_sub-kidno = p_alv_saida-kidno.
            IF lwa_saknr_sub-xref1 IS INITIAL OR lwa_saknr_sub-xref1 = p_alv_saida-xref1.
              p_alv_saida-tp_subop = lwa_saknr_sub-des_subop.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
*** BUG - 181038 - Fim - CBRAND

  CLEAR: lv_saknr,
         lv_bsart,
         lwa_saknr,
         lwa_saknr_sub,
         lit_saknr,
         lit_saknr_sub,
         lit_string,
         lr_bsart[].
* BUG - 171955  - Fim - CBRAND

****************************************
* BUG - 171955  - Inicio - CBRAND

*  DATA: lv_condicao TYPE string.
*  IF p_alv_saida-racct IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY saknr = p_alv_saida-racct.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                          THEN |saknr eq '{ p_alv_saida-racct }'|
*                                          ELSE |{ lv_condicao } and saknr eq '{ p_alv_saida-racct }'| ).
*    ENDIF.
*  ENDIF.
*
*  IF p_alv_saida-bschl IS NOT INITIAL.
*
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY bschl = p_alv_saida-bschl.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                          THEN |bschl eq '{ p_alv_saida-bschl }'|
*                                          ELSE |{ lv_condicao } and bschl eq '{ p_alv_saida-bschl }'| ).
*    ENDIF.
*  ENDIF.
*
*  IF p_alv_saida-blart IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY blart = p_alv_saida-blart.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                        THEN |blart eq '{ p_alv_saida-blart }'|
*                                        ELSE |{ lv_condicao } and blart eq '{ p_alv_saida-blart }'| ).
*    ENDIF.
*  ENDIF.
*
*  IF p_alv_saida-matkl IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY matkl = p_alv_saida-matkl.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                        THEN | matkl eq '{ p_alv_saida-matkl }' |
*                                        ELSE | { lv_condicao } and matkl eq '{ p_alv_saida-matkl }' | ).
*    ENDIF.
*  ENDIF.
*
*  IF  p_alv_saida-bsart IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY bsart = p_alv_saida-bsart.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                        THEN | bsart eq '{ p_alv_saida-bsart }' |
*                                        ELSE | { lv_condicao } and bsart eq '{ p_alv_saida-bsart }' | ).
*    ENDIF.
*  ENDIF.
*
*  IF p_alv_saida-rcntr IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY kostl = p_alv_saida-rcntr.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                        THEN | kostl eq '{ p_alv_saida-rcntr }' |
*                                        ELSE | { lv_condicao } and kostl eq '{ p_alv_saida-rcntr }' | ).
*    ENDIF.
*  ENDIF.
*
*  IF  p_alv_saida-cliente_fornecedor IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY lifnr = p_alv_saida-cliente_fornecedor.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                        THEN | lifnr eq '{ p_alv_saida-cliente_fornecedor }' |
*                                        ELSE | { lv_condicao } and lifnr eq '{ p_alv_saida-cliente_fornecedor }' | ).
*    ENDIF.
*  ENDIF.
*
*  IF  p_alv_saida-drcrk IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY drcrk = p_alv_saida-drcrk.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                        THEN | drcrk eq '{ p_alv_saida-drcrk }' |
*                                        ELSE | { lv_condicao } and drcrk eq '{ p_alv_saida-drcrk }' | ).
*    ENDIF.
*  ENDIF.
*
*  IF  p_alv_saida-gkont IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY gkont = p_alv_saida-gkont.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                        THEN | gkont eq '{ p_alv_saida-gkont }' |
*                                        ELSE | { lv_condicao } and gkont eq '{ p_alv_saida-gkont }' | ).
*    ENDIF.
*  ENDIF.
*
*  IF  p_alv_saida-rbukrs IS NOT INITIAL.
*    READ TABLE t_zfit0208 INTO wa_zfit0208 WITH KEY bukrs = p_alv_saida-rbukrs.
*    IF sy-subrc IS INITIAL.
*      lv_condicao = COND #( WHEN lv_condicao IS INITIAL
*                                        THEN | bukrs eq '{ p_alv_saida-rbukrs }' |
*                                        ELSE | { lv_condicao } and bukrs eq '{ p_alv_saida-rbukrs }' | ).
*    ENDIF.
*  ENDIF.
*
*  LOOP AT t_zfit0208 INTO wa_zfit0208 WHERE (lv_condicao).
*    p_alv_saida-pdd = wa_zfit0208-pdd.                       "PDD
*    p_alv_saida-tp_operacao = |{ wa_zfit0208-cod_operacao } { wa_zfit0208-desc_cod_operacao }|.
*    EXIT.
*  ENDLOOP.
*
*  IF p_alv_saida-tp_operacao IS INITIAL.
*    p_alv_saida-tp_operacao = 'DELETAR'.
*  ENDIF.
* BUG - 171955  - Fim - CBRAND
ENDFORM.

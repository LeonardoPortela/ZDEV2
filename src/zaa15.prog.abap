*&---------------------------------------------------------------------
*
*& Report  ZAA15
*&---------------------------------------------------------------------*
*& Programa: Fluxo Baixa Imobilizado - Aprovação
*& Autor:    Jean Antunes
*& Data:     04.06.2018
*&---------------------------------------------------------------------*
REPORT zaa15.

TYPE-POOLS: vrm.

*=======================================================================
* TABLES
*=======================================================================
TABLES: anla, t001k, t001w, zaa004, zaa007.

*=======================================================================
* TYPES
*=======================================================================
TYPES: BEGIN OF ty_zaas0003,
         cellstyles TYPE lvc_t_styl.
         INCLUDE STRUCTURE zaas0003.
TYPES: END OF ty_zaas0003.

TYPES:BEGIN OF ty_solicitacoes.
        INCLUDE STRUCTURE zaas0001.
TYPES        vlr_aq_brl TYPE zaas0003-vlr_aq_brl.
TYPES         vlr_contabil_brl TYPE zaas0003-vlr_contabil_brl.
TYPES         vlr_aq_usd       TYPE zaas0003-vlr_aq_usd.
TYPES         vlr_contabil_usd TYPE zaas0003-vlr_aq_usd.
TYPES: END OF ty_solicitacoes.


TYPES:BEGIN OF y_index,
        ind             TYPE sy-tabix,
        imobilizado(10) TYPE c,
        aprovador(20)   TYPE c,
        nivel(2)        TYPE c,
      END OF y_index.


DATA: t_index TYPE TABLE OF y_index,
      w_index TYPE y_index.
*=======================================================================
* STRUCTURES E INTERNAL TABLES
*=======================================================================
DATA: tg_solicitacoes      TYPE STANDARD TABLE OF zaas0001,
      tg_estra             TYPE STANDARD TABLE OF zaas0002,
      wa_estra             TYPE zaas0002,
      tg_detalhes          TYPE STANDARD TABLE OF zaas0003,
      tg_saida_sol         TYPE STANDARD TABLE OF ty_solicitacoes, "zaas0001,
      tg_saida_aprov_massa TYPE STANDARD TABLE OF ty_solicitacoes, "zaas0001,
      tg_saida_estra       TYPE STANDARD TABLE OF zaas0002,
      tg_saida_detalhes    TYPE STANDARD TABLE OF ty_zaas0003,
      wg_sol_resumo        TYPE zaas0001,
      wg_zaa007            TYPE zaa007,
      lr_row               TYPE lvc_s_row,
      vclear(1)            TYPE c,
      linha_dclick         TYPE i.

FIELD-SYMBOLS: <wg_solicitacoes> LIKE LINE OF tg_solicitacoes[],
               <wg_estra>        LIKE LINE OF tg_estra[],
               <wg_detalhes>     LIKE LINE OF tg_saida_detalhes[].

*=======================================================================
* VARIABLES
*=======================================================================
*&SPWIZARD: FUNCTION CODES FOR TABSTRIP 'TAB_STRIP_NF'
CONSTANTS: BEGIN OF c_tab_strip_imp,
             tab1 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC1',
             tab2 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC2',
             tab3 LIKE sy-ucomm VALUE 'TAB_STRIP_IMP_FC3',
           END OF c_tab_strip_imp.

*&SPWIZARD: DATA FOR TABSTRIP 'TAB_STRIP_NF'
CONTROLS:  tab_strip_imp TYPE TABSTRIP.
DATA: BEGIN OF g_tab_strip_imp,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'ZAA15',
        pressed_tab LIKE sy-ucomm VALUE c_tab_strip_imp-tab1,
      END OF g_tab_strip_imp.

*=======================================================================
* ALV STRUCTURE
*=======================================================================
DATA: tg_fieldcat  TYPE lvc_t_fcat,
      gs_variant_c TYPE disvariant.

DATA: obg_container_sol       TYPE REF TO cl_gui_custom_container,
      obg_container_estra     TYPE REF TO cl_gui_custom_container,
      obg_container_docs      TYPE REF TO cl_gui_custom_container,
      g_cc_sol                TYPE scrfname VALUE 'CC_SOLICITACOES',
      g_cc_estra              TYPE scrfname VALUE 'CC_ESTRA',
      g_cc_docs               TYPE scrfname VALUE 'CC_DOCS',
      container_01            TYPE REF TO cl_gui_container,
      grid_sol                TYPE REF TO cl_gui_alv_grid,
      grid_estra              TYPE REF TO cl_gui_alv_grid,
      grid_docs               TYPE REF TO cl_gui_alv_grid,
      splitter                TYPE REF TO cl_gui_splitter_container,
      p_text                  TYPE sdydo_text_element,
      p_text_table            TYPE sdydo_text_table,
      wa_layout               TYPE lvc_s_layo,
      wa_stable               TYPE lvc_s_stbl,
      sdydo_text_element(255).

DATA: it_selected_rows TYPE lvc_t_row,                                  "Tabela de linhas selecionadas na alv de saída
      wa_selected_rows TYPE lvc_s_row.

DATA: wl_repid    TYPE sy-repid,
      tl_function TYPE ui_functions,
      wl_function LIKE tl_function WITH HEADER LINE.

DATA: tl_solicitacoes    TYPE STANDARD TABLE OF zaas0001,
      tl_solicitacoes_ap TYPE STANDARD TABLE OF zaas0001,
      wa_solicitacoes    TYPE zaas0001,
      tl_estra           TYPE STANDARD TABLE OF zaas0002,
      tl_estra_ap        TYPE STANDARD TABLE OF zaas0002,
      v_msg              TYPE char50,
      vl_zugdt           TYPE dzugdat,
      vl_motivo          TYPE char30,
      vl_obs             TYPE char72,
      tl_controller_obs  TYPE catsxt_longtext_itab.


CLASS cl_gui_cfw DEFINITION LOAD.


*&---------------------------------------------------------------------*
*&      Class (DEFINITION)  LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no sender.       "Método duplo click na ALV

    CLASS-METHODS:
      on_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

    CLASS-METHODS:
      handle_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.

ENDCLASS.


*&---------------------------------------------------------------------*
*&       Class (Implementation)  LCL_EVENT_HANDLER
*&---------------------------------------------------------------------*
CLASS lcl_event_handler IMPLEMENTATION.


  METHOD on_double_click.

    CONDENSE e_row NO-GAPS.
    linha_dclick = e_row.

    DATA: wl_saida_detalhes LIKE LINE OF tg_saida_detalhes[].

    IF linha_dclick > 0."( e_row GT 0 ).
      "tg_solicitacoes[] = tg_saida_sol[].
      "MOVE-CORRESPONDING tg_solicitacoes[] TO tg_saida_sol[].

      READ TABLE tg_saida_sol[] ASSIGNING FIELD-SYMBOL(<_get_row>) INDEX linha_dclick.
      LOOP AT tg_saida_sol[] ASSIGNING FIELD-SYMBOL(<field>) WHERE empresa = <_get_row>-empresa AND filial = <_get_row>-filial AND imobilizado = <_get_row>-imobilizado AND kostl = <_get_row>-kostl.
        LOOP AT tg_detalhes[] ASSIGNING FIELD-SYMBOL(<values>) WHERE bukrs = <field>-empresa AND werks = <field>-filial AND anln1 = <field>-imobilizado AND kostl = <field>-kostl.
          <field>-vlr_aq_brl        = <values>-vlr_aq_brl.
          <field>-vlr_contabil_brl  = <values>-vlr_contabil_brl.
          <field>-vlr_aq_usd        = <values>-vlr_aq_usd.
          <field>-vlr_contabil_usd  = <values>-vlr_contabil_usd.
        ENDLOOP.
      ENDLOOP.

      CLEAR: wg_sol_resumo, tg_saida_estra[], tg_saida_detalhes[].
      "READ TABLE tg_solicitacoes[] INTO DATA(wl_sol) INDEX 1.
      DATA: wl_sol TYPE ty_solicitacoes.
      CLEAR: wl_sol.
      LOOP AT tg_solicitacoes[] ASSIGNING FIELD-SYMBOL(<_get_tgsol>) WHERE empresa = <_get_row>-empresa AND filial = <_get_row>-filial AND imobilizado = <_get_row>-imobilizado AND kostl = <_get_row>-kostl.
        IF <_get_tgsol> IS NOT INITIAL.
          MOVE-CORRESPONDING <_get_tgsol> TO wl_sol.
        ENDIF.
      ENDLOOP.

      MOVE wl_sol-empresa     TO wg_sol_resumo-empresa.
      MOVE wl_sol-filial      TO wg_sol_resumo-filial.
      MOVE wl_sol-usuario     TO wg_sol_resumo-usuario.
      MOVE wl_sol-dt_solicita TO wg_sol_resumo-dt_solicita.

      CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
        EXPORTING
          functioncode           = '=ENT'
        EXCEPTIONS
          function_not_supported = 1
          OTHERS                 = 2.

      LOOP AT tg_estra[] INTO DATA(wl_estra) WHERE anln1 EQ wl_sol-imobilizado.
        IF wl_estra-estado <> '@01@' AND wl_estra-opcoes <> '@2W@'. "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
          APPEND wl_estra TO tg_saida_estra[].
        ENDIF.
      ENDLOOP.

      LOOP AT tg_detalhes[] INTO DATA(wl_detalhes) WHERE anln1 EQ wl_sol-imobilizado.
        MOVE-CORRESPONDING wl_detalhes TO wl_saida_detalhes.
        APPEND wl_saida_detalhes TO tg_saida_detalhes[].
      ENDLOOP.

      PERFORM busca_anexo.

      SORT tg_saida_estra[] BY nivel.

*      CALL METHOD grid_estra->refresh_table_display
*        EXPORTING
*          is_stable = wa_stable.

*      CALL METHOD grid_docs->refresh_table_display
*        EXPORTING
*          is_stable = wa_stable.

      vclear = 'X' .

    ENDIF.

  ENDMETHOD.            "METHOD ON_DOUBLE_CLICK

  METHOD on_click.

    CLEAR: lr_row.
    lr_row = e_row_id.
    "CLEAR: tl_solicitacoes,tl_estra,vl_zugdt ,vl_motivo,tl_controller_obs.

    IF ( e_row_id GT 0 ).

      READ TABLE tg_saida_estra[]   INTO DATA(wl_estra) INDEX e_row_id.
      READ TABLE tg_solicitacoes[]  INTO DATA(wl_sol)   WITH KEY imobilizado = wl_estra-anln1.

      APPEND wl_estra TO tl_estra[].
      APPEND wl_sol   TO tl_solicitacoes[].

      SELECT * FROM zaa004
        INTO TABLE @DATA(tl_estrat)
        WHERE bukrs EQ @wl_estra-bukrs
          AND gsber EQ @wl_estra-gsber
          AND kostl EQ @wl_estra-kostl
          AND fluxo_baixa EQ 'X'.

      SORT tl_estrat[] BY nivel_aa DESCENDING.
      READ TABLE tl_estrat[] INTO DATA(wl_estrat) INDEX 2.
      DATA(vl_nivel) = wl_estrat-nivel_aa.

      IF ( wl_estra-nivel EQ vl_nivel ) AND ( wl_estra-opcoes EQ icon_set_state OR wl_estra-opcoes EQ icon_reject  ).

*        PERFORM SET_VALUES_MOTIVO.

        CALL SCREEN 0140 STARTING AT 3 3.

        vl_zugdt  = wg_zaa007-zugdt.
        vl_motivo = wg_zaa007-motivo.
        TRANSLATE vl_motivo TO UPPER CASE.

      ENDIF.

      READ TABLE tl_estrat[] INTO wl_estrat INDEX 1.
      vl_nivel = wl_estrat-nivel_aa.

      "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
      " Condição desabilitada item 4 conforme especifcação
*      IF ( wl_estra-nivel EQ vl_nivel ) AND ( wl_estra-opcoes EQ icon_set_state OR wl_estra-opcoes EQ icon_reject ).
*
*        CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
*          EXPORTING
*            im_title = 'Observação Do Controller:'
**           IM_DISPLAY_MODE = ''
*          CHANGING
*            ch_text  = tl_controller_obs.
*
*        IF ( tl_controller_obs IS NOT INITIAL ).
*
*          READ TABLE tl_controller_obs[] INTO DATA(line_obs) INDEX 1.
*          vl_obs = line_obs.
*        ENDIF.
*
*      ENDIF.

      IF ( wl_estra-opcoes IS NOT INITIAL ).

        PERFORM aprova.
        PERFORM atualiza_solicitacoes.

*        DELETE ADJACENT DUPLICATES FROM tl_solicitacoes[].
*        DELETE tl_estra[] WHERE estado = '@01@' AND opcoes = '@2W@'.
*
*        CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
*          EXPORTING
*            v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
*            v_zugdt        = vl_zugdt
*            v_motivo       = vl_motivo
*            v_obs          = vl_obs
*          IMPORTING
*            msg            = v_msg      " Comentário
*          TABLES
*            t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
*            t_estra        = tl_estra.        " Baixa de Imobilizado - Estratégia de liberação
*
*        CLEAR: vl_zugdt, vl_motivo, vl_obs.
*
*        LOOP AT tl_estra[] INTO wl_estra.
*          MODIFY tg_saida_estra[] FROM wl_estra INDEX e_row_id.
*        ENDLOOP.
*
*        CALL METHOD grid_estra->refresh_table_display
*          EXPORTING
*            is_stable = wa_stable.
*
*        MESSAGE s836(sd) DISPLAY LIKE 'S' WITH v_msg.
*        CLEAR: wg_sol_resumo, wg_sol_resumo-empresa, wg_sol_resumo-filial, wg_sol_resumo-usuario, wg_sol_resumo-dt_solicita.
*
*        PERFORM atualiza_solicitacoes.

      ENDIF.

    ENDIF.

  ENDMETHOD.            "METHOD ON_CLICK.

  METHOD handle_button_click.
    DATA: anexo_obj     TYPE REF TO cl_gos_manager,
          vl_ip_service TYPE sgs_srvnam,
          wa_bor        TYPE borident,
          vl_obj_key    TYPE sibflporb-instid,
          tl_anexos     TYPE TABLE OF bdn_con,
          ip_mode       TYPE sgs_rwmod,
          vl_ano(4)     TYPE c.

    READ TABLE tg_saida_detalhes[] ASSIGNING <wg_detalhes> INDEX es_row_no-row_id.

    vl_ano = <wg_detalhes>-dt_solicitacao+0(4).

    CASE es_col_id.
      WHEN 'ANEXO'.

        CREATE OBJECT anexo_obj TYPE cl_gos_manager.

        IF ( <wg_detalhes>-anexo EQ '@1F@' ).        "Sem anexo

        ELSE.
          vl_ip_service = 'VIEW_ATTA'.
          ip_mode = 'R'.
        ENDIF.

        wa_bor-objkey   = |ZAA18{ <wg_detalhes>-anln1 }{ <wg_detalhes>-werks }{ vl_ano }|.
        wa_bor-objtype  = 'ZAA13'.

        anexo_obj->set_rw_mode( ip_mode = ip_mode ).
        anexo_obj->start_service_direct(
          EXPORTING
            ip_service         = vl_ip_service
            is_object          = wa_bor
          EXCEPTIONS
            no_object          = 1
            object_invalid     = 2
            execution_failed   = 3
            OTHERS             = 4 ).


    ENDCASE.

  ENDMETHOD.        "HANDLE_BUTTON_CLICK.

ENDCLASS.               "LCL_EVENT_HANDLER



*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0140  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0140 INPUT.

  CASE sy-ucomm.
    WHEN 'OK_140'.
      IF ( wg_zaa007-zugdt IS NOT INITIAL ) AND
         ( wg_zaa007-motivo IS NOT INITIAL ) .
        LEAVE TO SCREEN 0.
      ELSE.
        MESSAGE 'Preencher os campos obrigatórios!' TYPE 'I'.
        EXIT.
      ENDIF.
    WHEN 'CANC_140'.
      LEAVE TO CURRENT TRANSACTION.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  CARREGA_SOLICITACOES  OUTPUT
*&---------------------------------------------------------------------*
MODULE carrega_solicitacoes OUTPUT.

  IF ( obg_container_sol IS INITIAL ).
    PERFORM atualiza_solicitacoes.
  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_SOLICITACOES
*&---------------------------------------------------------------------*
FORM atualiza_solicitacoes .

  DATA: lt_celltab TYPE lvc_t_styl,
        ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4,
        vl_msg     TYPE char50.

  CLEAR: tg_solicitacoes[], tg_estra[], tg_detalhes[], wg_sol_resumo,
         tg_saida_sol[], tg_saida_estra[], tg_saida_detalhes[].

  CALL FUNCTION 'Z_SOL_BAIXA_IMOB_LISTA'
    EXPORTING
      v_usuario      = sy-uname
    IMPORTING
      msg            = vl_msg           " Comentário
    TABLES
      t_solicitacoes = tg_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
      t_estra        = tg_estra         " Baixa de Imobilizado - Estratégia de liberação
      t_detalhes     = tg_detalhes.     " Baixa de Imobilizado - Detalhes

  IF ( tg_solicitacoes[] IS NOT INITIAL ).
    "tg_saida_sol[] = tg_solicitacoes[].
    MOVE-CORRESPONDING tg_solicitacoes[] TO tg_saida_sol[].

    LOOP AT tg_saida_sol[] ASSIGNING FIELD-SYMBOL(<field>).

      LOOP AT tg_detalhes[] ASSIGNING FIELD-SYMBOL(<values>) WHERE bukrs = <field>-empresa AND anln1 = <field>-imobilizado.

        <field>-vlr_aq_brl        = <values>-vlr_aq_brl.
        <field>-vlr_contabil_brl  = <values>-vlr_contabil_brl.
        <field>-vlr_aq_usd        = <values>-vlr_aq_usd.
        <field>-vlr_contabil_usd  = <values>-vlr_contabil_usd.

      ENDLOOP.

    ENDLOOP.

  ENDIF.

  IF ( obg_container_sol IS NOT INITIAL ).
    CALL METHOD grid_sol->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
  IF ( obg_container_estra IS NOT INITIAL ).
    CALL METHOD grid_estra->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
  IF ( obg_container_docs IS NOT INITIAL ).
    CALL METHOD grid_docs->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.



ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST_001'.
  SET TITLEBAR  'T_001'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  CRIA_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
MODULE cria_objetos OUTPUT.

  DATA: wl_layout TYPE lvc_s_layo,
        wl_event  TYPE REF TO lcl_event_handler.

  wl_layout-zebra         = 'X'.
  wa_layout-box_fname     = 'FLAG'.
  wl_layout-no_rowmark    = 'X'.
  wl_layout-info_fname    = 'X'.
  wl_layout-no_toolbar    = 'X'.
  wl_layout-stylefname    = 'CELLSTYLES'.

  CLEAR: tl_function[].
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

*&---------------------------------------------------------------------*
*&      CRIANDO ALV SOLICITAÇÕES
*&---------------------------------------------------------------------*
  IF ( obg_container_sol IS INITIAL ).

    CREATE OBJECT obg_container_sol
      EXPORTING
        container_name = g_cc_sol.

    CREATE OBJECT splitter
      EXPORTING
        parent  = obg_container_sol
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_01.

    CREATE OBJECT grid_sol
      EXPORTING
        i_parent = container_01.


    PERFORM montar_layout_sol.
    wl_layout-no_toolbar    = ' '.
    CALL METHOD grid_sol->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wl_layout
      CHANGING
        it_fieldcatalog      = tg_fieldcat[]
        it_outtab            = tg_saida_sol[].

    CALL METHOD grid_sol->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid_sol->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_double_click FOR grid_sol.

    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.

  ELSE.

    CALL METHOD grid_sol->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

  wl_layout-no_toolbar    = 'X'.
*&---------------------------------------------------------------------*
*&      CRIANDO ALV ESTRATÉGIA
*&---------------------------------------------------------------------*
  IF ( obg_container_estra IS INITIAL ).

    CREATE OBJECT obg_container_estra
      EXPORTING
        container_name = g_cc_estra.

    CREATE OBJECT grid_estra
      EXPORTING
        i_parent = obg_container_estra.

    PERFORM montar_layout_estra.

    CALL METHOD grid_estra->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wl_layout
      CHANGING
        it_fieldcatalog      = tg_fieldcat[]
        it_outtab            = tg_saida_estra[].

    CALL METHOD grid_estra->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid_estra->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_click FOR grid_estra.

  ELSE.

    CALL METHOD grid_estra->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

*&---------------------------------------------------------------------*
*&      CRIANDO ALV DETALHES
*&---------------------------------------------------------------------*
  IF ( obg_container_docs IS INITIAL ).

    CREATE OBJECT obg_container_docs
      EXPORTING
        container_name = g_cc_docs.

    CREATE OBJECT grid_docs
      EXPORTING
        i_parent = obg_container_docs.

    PERFORM montar_layout_docs.

    wl_layout-stylefname = 'CELLSTYLES'.

    CALL METHOD grid_docs->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
        is_layout            = wl_layout
      CHANGING
        it_fieldcatalog      = tg_fieldcat[]
        it_outtab            = tg_saida_detalhes[].

    CALL METHOD grid_docs->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid_docs->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER:
        lcl_event_handler=>handle_button_click FOR grid_docs.

  ELSE.

    CALL METHOD grid_docs->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.


ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_SOL
*&---------------------------------------------------------------------*
FORM montar_layout_sol.

  CLEAR: tg_fieldcat[].
  PERFORM cria_estrutura USING:

        1  ''  '' 'TG_SOL'      'FLAG'              ' '                '01' 'C' 'X' '' '',
        1  ''  '' 'TG_SOL'      'EMPRESA'           'Empresa'          '08' 'C' '' '' '' ,
        1  ''  '' 'TG_SOL'      'KOSTL'             'C.Custo'          '10' 'C' '' '' '' ,
        2  ''  '' 'TG_SOL'      'FILIAL'            'Filial'           '06' 'C' '' '' '' ,
        3  ''  '' 'TG_SOL'      'IMOBILIZADO'       'N.Imobilizado'    '10' 'C' '' '' '' ,
        4  ''  '' 'TG_SOL'      'TXT50'             'Descricao'        '25' 'C' '' '' '' ,
        5  ''  '' 'TG_SOL'      'DT_SOLICITA'       'Dt Solicitacao'   '10' 'C' '' '' '' ,
        6  ''  '' 'TG_SOL'      'USUARIO'           'Solicitante'      '11' 'C' '' '' '' ,
        7  ''  '' 'TG_DETALHES' 'VLR_AQ_BRL'        'VLR.Aquis.BRL'    '10' 'C' '' '' '',
        9  ''  '' 'TG_DETALHES' 'VLR_CONTABIL_BRL'  'VLR.Contabil BRL' '10' 'C' '' '' '',
       10  ''  '' 'TG_DETALHES' 'VLR_AQ_USD'        'VLR.Aquis.USD'    '10' 'C' '' '' '',
       11  ''  '' 'TG_DETALHES' 'VLR_CONTABIL_USD'  'VLR.Contabil USD' '10' 'C' '' '' ''.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
FORM montar_layout_estra.

  CLEAR: tg_fieldcat[].
  PERFORM cria_estrutura USING:

*        1  ''  '' 'TG_ESTRA' 'BUKRS'      'Empresa'          '06' 'C' '' '' '',
*        2  ''  '' 'TG_ESTRA' 'GSBER'      'Filial'           '06' 'C' '' '' '',
*        3  ''  '' 'TG_ESTRA' 'KOSTL'      'C.Custo'          '12' 'C' '' '' '',
        4  ''  '' 'TG_ESTRA' 'APROVADOR'  'Aprovador'        '25' 'C' '' '' '',
        5  ''  '' 'TG_ESTRA' 'ESTADO'     'Status'           '08' 'C' '' '' '',
        6  ''  '' 'TG_ESTRA' 'OPCOES'     'Ação'             '08' 'C' '' '' ''.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT_ESTRA
*&---------------------------------------------------------------------*
FORM montar_layout_docs.

  CLEAR: tg_fieldcat[].
  PERFORM cria_estrutura USING:

        1  ''  '' 'TG_DETALHES' 'BUKRS'             'Empresa'                 '06' 'C' '' '' '',
        2  ''  '' 'TG_DETALHES' 'WERKS'             'Filial'                  '06' 'C' '' '' '',
        3  ''  '' 'TG_DETALHES' 'ANLN1'             'N.Imobilizado'           '12' 'C' '' '' '',
        4  ''  '' 'TG_DETALHES' 'ANLN2'             'N.Imob.2'                '12' 'C' '' '' '',
        5  ''  '' 'TG_DETALHES' 'TXT50'             'Denominacao'             '20' 'C' '' '' '',
        6  ''  '' 'TG_DETALHES' 'TXA50'             'Denomi.2'                '10' 'C' '' '' '',
        7  ''  '' 'TG_DETALHES' 'KOSTL'             'C.Custo'                 '15' 'C' '' '' '',
        8  ''  '' 'TG_DETALHES' 'ZUGDT'             'Dt.Referencia'           '10' 'C' '' '' '',
*** RMNI - CS1066933 - Alterar nomenclaturas - 24/03/2023 - Inicio
*        9  ''  '' 'TG_DETALHES' 'MOTIVO'            'Motivo'                '10' 'C' '' '' '',
        9  ''  '' 'TG_DETALHES' 'MOTIVO'            'Observação CSC Contábil' '10' 'C' '' '' '',
*** RMNI - CS1066933 - Alterar nomenclaturas - 24/03/2023 - Fim
        10 ''  '' 'TG_DETALHES' 'VLR_AQ_BRL'        'VLR.Aquis.BRL'           '10' 'C' '' '' '',
        11 ''  '' 'TG_DETALHES' 'DEPREC_BRL'        'Depreciacao Acum.BR'     '10' 'C' '' '' '',
        12 ''  '' 'TG_DETALHES' 'VLR_CONTABIL_BRL'  'VLR.Contabil BRL'        '10' 'C' '' '' '',
        13 ''  '' 'TG_DETALHES' 'VLR_AQ_USD'        'VLR.Aquis.USD'           '10' 'C' '' '' '',
        14 ''  '' 'TG_DETALHES' 'DEPREC_USD'        'Depreciacao Acum.USD'    '10' 'C' '' '' '',
        15 ''  '' 'TG_DETALHES' 'VLR_CONTABIL_USD'  'VLR.Contabil USD'        '10' 'C' '' '' '',
        16 ''  '' 'TG_DETALHES' 'ESTADO_BEM'        'Estado Bem'              '10' 'C' '' '' '',
        17 ''  '' 'TG_DETALHES' 'ANEXO'             'Anexo'                   '10' 'C' '' '' '',
        18 ''  '' 'TG_DETALHES' 'RESPONSAVEL'       'Responsável'             '10' 'C' '' '' '',
        19 ''  '' 'TG_DETALHES' 'DT_SOLICITACAO'    'DT.Solicitacao'          '10' 'C' '' '' '',
        20 ''  '' 'TG_DETALHES' 'SOLICITANTE'       'Solicitante'             '10' 'C' '' '' '',
        21 ''  '' 'TG_DETALHES' 'OBS_CONTROLLER'    'OBS.Controller'          '20' 'C' '' '' ''.


ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  CRIA_ESTRUTURA
*&---------------------------------------------------------------------*
FORM cria_estrutura USING VALUE(p_col_pos)       TYPE i
                            VALUE(p_ref_tabname)   LIKE dd02d-tabname
                            VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                            VALUE(p_tabname)       LIKE dd02d-tabname
                            VALUE(p_field)         LIKE dd03d-fieldname
                            p_scrtext_l
                            VALUE(p_outputlen)
                            VALUE(p_just)
                            VALUE(p_edit)
                            VALUE(p_sum)
                            VALUE(p_emphasize).


  DATA: wl_fieldcat TYPE lvc_s_fcat.

  CLEAR: wl_fieldcat.


  wl_fieldcat-fieldname     = p_field.
  wl_fieldcat-tabname       = p_tabname.
  wl_fieldcat-ref_table     = p_ref_tabname.
  wl_fieldcat-ref_field     = p_ref_fieldname.
  wl_fieldcat-key           = ' '.
  wl_fieldcat-edit          = p_edit.
  wl_fieldcat-just          = p_just.
  wl_fieldcat-do_sum        = p_sum.
  wl_fieldcat-col_pos       = p_col_pos.
  wl_fieldcat-outputlen     = p_outputlen.
  wl_fieldcat-reptext       = p_scrtext_l.
  wl_fieldcat-scrtext_s     = p_scrtext_l.
  wl_fieldcat-scrtext_m     = p_scrtext_l.
  wl_fieldcat-scrtext_l     = p_scrtext_l.
  wl_fieldcat-emphasize     = p_emphasize.


  IF wl_fieldcat-fieldname = 'FLAG'.

    wl_fieldcat-checkbox      = 'X'.

  ENDIF.
*  WL_FIELDCAT-COL_OPT       = 'X'.

  IF ( p_field EQ 'IMOBILIZADO' ) OR ( p_field EQ 'ANLN1' ).
    wl_fieldcat-no_zero = 'X'.
  ENDIF.

  IF ( p_field EQ 'OPCOES' ).
    wl_fieldcat-hotspot = 'X'.
  ENDIF.

  APPEND wl_fieldcat TO tg_fieldcat[].


ENDFORM.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  IF vclear <> 'X'.

    REFRESH:t_index.
    CLEAR:w_index.
    LOOP AT tg_estra INTO DATA(wl_index).
      w_index-ind  = w_index-ind + 1.
      w_index-imobilizado = wl_index-anln1.
      w_index-aprovador   = wl_index-aprovador.
      w_index-nivel       = wl_index-nivel.
      APPEND w_index TO t_index.

    ENDLOOP.

    DATA: t_zaa008 TYPE TABLE OF zaa008,
          w_zaa008 TYPE zaa008.

    SELECT * FROM zaa008
       INTO TABLE t_zaa008
      FOR ALL ENTRIES IN tg_saida_sol
       WHERE bukrs EQ tg_saida_sol-empresa(4)
         AND werks EQ tg_saida_sol-filial(4)
         AND anln1 EQ tg_saida_sol-imobilizado.



    CASE sy-ucomm.
      WHEN 'ALL'.
        LOOP AT tg_saida_sol[] INTO DATA(wl_all).

          wl_all-flag = 'X'.
          MODIFY tg_saida_sol[] FROM wl_all.

        ENDLOOP.
        CALL METHOD grid_estra->refresh_table_display.

      WHEN 'DESALL'.

        LOOP AT tg_saida_sol[] INTO DATA(wl_desall).

          wl_desall-flag = ' '.
          MODIFY tg_saida_sol[] FROM wl_desall.

        ENDLOOP.
        CALL METHOD grid_estra->refresh_table_display.

      WHEN 'APRO'." aprovar em massa

        APPEND LINES OF tg_saida_sol[] TO tg_saida_aprov_massa.
        SORT  tg_saida_aprov_massa BY flag ASCENDING.
        DELETE tg_saida_aprov_massa WHERE flag <> 'X'.

        LOOP AT tg_saida_aprov_massa ASSIGNING FIELD-SYMBOL(<_aprova_massa>).

          LOOP AT tg_estra[] ASSIGNING FIELD-SYMBOL(<_estra>) WHERE bukrs = <_aprova_massa>-empresa AND gsber = <_aprova_massa>-filial AND anln1 =  <_aprova_massa>-imobilizado AND aprovador = sy-uname.
            wa_estra-anln1 = <_estra>-anln1.
            wa_estra-anln2 = <_estra>-anln2.
            wa_estra-aprovador = <_estra>-aprovador.
            wa_estra-bukrs = <_estra>-bukrs.
            wa_estra-estado = <_estra>-estado.
            wa_estra-gsber = <_estra>-gsber.
            wa_estra-kostl = <_estra>-kostl.
            wa_estra-nivel = <_estra>-nivel.
            wa_estra-opcoes = <_estra>-opcoes.
            APPEND wa_estra TO tl_estra_ap[].
          ENDLOOP.

          CLEAR:wa_solicitacoes.
          wa_solicitacoes-empresa = <_aprova_massa>-empresa.
          wa_solicitacoes-imobilizado = <_aprova_massa>-imobilizado.
          wa_solicitacoes-filial = <_aprova_massa>-filial.
          wa_solicitacoes-kostl = <_aprova_massa>-kostl.
          wa_solicitacoes-usuario = <_aprova_massa>-usuario.
          wa_solicitacoes-txt50 = <_aprova_massa>-txt50.
          wa_solicitacoes-flag = <_aprova_massa>-flag.
          APPEND wa_solicitacoes TO tl_solicitacoes_ap[].
        ENDLOOP.

        APPEND LINES OF  tl_solicitacoes_ap[] TO tl_solicitacoes[].
        APPEND LINES OF tl_estra_ap[] TO tl_estra[].

        LOOP AT tg_saida_aprov_massa ASSIGNING FIELD-SYMBOL(<_aprova>).
          CLEAR: tl_solicitacoes,tl_solicitacoes[],tl_estra,tl_estra[].
          LOOP AT tl_estra_ap[] ASSIGNING FIELD-SYMBOL(<row_estra>) WHERE bukrs = <_aprova>-empresa AND gsber = <_aprova>-filial AND anln1 =  <_aprova>-imobilizado AND aprovador = sy-uname.
            APPEND <row_estra> TO tl_estra.
          ENDLOOP.

          LOOP AT tl_solicitacoes_ap[] ASSIGNING FIELD-SYMBOL(<row_solicitacaoes>) WHERE imobilizado =  <_aprova>-imobilizado AND filial = <_aprova>-filial AND empresa = <_aprova>-empresa AND usuario = <_aprova>-usuario.
            APPEND <row_solicitacaoes> TO tl_solicitacoes[].
          ENDLOOP.

          PERFORM aprova.

        ENDLOOP.

        CLEAR: tl_solicitacoes_ap,tl_solicitacoes_ap[],tl_estra_ap,tl_estra_ap[].
        PERFORM atualiza_solicitacoes.



*
*      "CALL METHOD grid_estra->refresh_table_display.
*      LOOP AT tg_saida_sol[] INTO DATA(wl_solicitacoe) WHERE flag = 'X'.
*
*
*
*
*        READ TABLE tg_estra INTO DATA(wl_aprovar) WITH KEY anln1 =  wl_solicitacoe-imobilizado
*                                                         aprovador = sy-uname
*                                                         nivel = '1'."nivel 1
*
*        IF sy-subrc = 0.
*
*          READ TABLE t_index INTO  w_index WITH KEY imobilizado = wl_solicitacoe-imobilizado
*                                                    aprovador = sy-uname
*                                                    nivel = '1'."nivel 1
*          wl_aprovar-opcoes = '@2W@'.
*          wl_aprovar-estado = '@01@'.
*          MODIFY tg_estra[] FROM wl_aprovar INDEX w_index-ind TRANSPORTING opcoes estado .
*
*          READ TABLE t_zaa008 INTO w_zaa008 WITH KEY bukrs = wl_solicitacoe-empresa
*                                                      werks = wl_solicitacoe-filial
*                                                      anln1 = wl_solicitacoe-imobilizado.
*
*          IF sy-subrc = 0.
*
*            w_zaa008-nivel = '1'.
*            w_zaa008-aprovador = sy-uname.
*            INSERT INTO zaa008 VALUES w_zaa008.
*
*          ENDIF.
*
*          CALL METHOD grid_estra->refresh_table_display.
*
*          READ TABLE tg_saida_estra[]   INTO DATA(wl_estratio) WITH KEY nivel = '1'.
*          READ TABLE tg_solicitacoes[]  INTO DATA(wl_sol1)   WITH KEY imobilizado = wl_solicitacoe-imobilizado.
*
*          APPEND wl_estratio TO tl_estra[].
*          APPEND wl_sol1   TO tl_solicitacoes[].
*
*          SELECT * FROM zaa004
*           INTO TABLE @DATA(tl_estrat)
*           WHERE bukrs EQ @wl_estratio-bukrs
*             AND gsber EQ @wl_estratio-gsber
*             AND kostl EQ @wl_estratio-kostl
*             AND fluxo_baixa EQ 'X'.
*
*          CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
*            EXPORTING
*              v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
*              v_zugdt        = vl_zugdt
*              v_motivo       = vl_motivo
*              v_obs          = vl_obs
*            IMPORTING
*              msg            = v_msg      " Comentário
*            TABLES
*              t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
*              t_estra        = tl_estra.
*
*
*          CLEAR:w_index.
*          READ TABLE tg_saida_estra INTO wl_estratio WITH KEY anln1 = wl_solicitacoe-imobilizado
*                                                           nivel = '1'.
*
*          wl_estratio-opcoes = '@2W@'.
*          wl_estratio-estado = '@01@'.
*
*
*          MODIFY tg_saida_estra[] FROM wl_estratio INDEX 1.
*
*
**          CALL FUNCTION 'Z_SOL_BAIXA_IMOB_LISTA'
**            EXPORTING
**              v_usuario      = sy-uname
**            IMPORTING
**              msg            = vl_msg           " Comentário
**            TABLES
**              t_solicitacoes = tg_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
**              t_estra        = tg_estra         " Baixa de Imobilizado - Estratégia de liberação
**              t_detalhes     = tg_detalhes.     " Baixa de Imobilizado - Detalhes
*
*
*          IF ( tg_solicitacoes[] IS NOT INITIAL ).
**            tg_saida_sol[] = tg_solicitacoes[].
*          ENDIF.
*
*          IF ( obg_container_sol IS NOT INITIAL ).
*            CALL METHOD grid_sol->refresh_table_display
*              EXPORTING
*                is_stable = wa_stable.
*          ENDIF.
*          IF ( obg_container_estra IS NOT INITIAL ).
*            CALL METHOD grid_estra->refresh_table_display
*              EXPORTING
*                is_stable = wa_stable.
*          ENDIF.
*          IF ( obg_container_docs IS NOT INITIAL ).
*            CALL METHOD grid_docs->refresh_table_display
*              EXPORTING
*                is_stable = wa_stable.
*          ENDIF.
*
*        ENDIF.
*
**     valida a opção 2
*
*        READ TABLE tg_estra INTO DATA(wl_aprov) WITH KEY anln1 =  wl_solicitacoe-imobilizado
**                                                         aprovador = sy-uname
*                                                         estado = '@01@'"sucesso
*                                                         nivel = '1'."nivel 1
*
*        IF sy-subrc = 0.
*
*          READ TABLE tg_estra INTO DATA(wl_aprovar2) WITH KEY anln1 =  wl_solicitacoe-imobilizado
*                                                       aprovador = sy-uname
*                                                       nivel = '2'."nivel 2
*
*          IF sy-subrc = 0.
*
*            READ TABLE t_index INTO  w_index WITH KEY imobilizado = wl_solicitacoe-imobilizado
*                                                      aprovador = sy-uname
*                                                      nivel = '2'."nivel 1
*
*            wl_aprovar2-opcoes = '@2W@'.
*            wl_aprovar2-estado = '@01@'.
*            MODIFY tg_estra[] FROM wl_aprovar2 INDEX w_index-ind TRANSPORTING opcoes estado .
*
*            READ TABLE t_zaa008 INTO w_zaa008 WITH KEY bukrs = wl_solicitacoe-empresa
*                                                       werks = wl_solicitacoe-filial
*                                                       anln1 = wl_solicitacoe-imobilizado.
*
*            IF sy-subrc = 0.
*
*              w_zaa008-aprovador  = sy-uname.
*              w_zaa008-nivel = '2'.
*              INSERT INTO zaa008 VALUES w_zaa008.
*
*            ENDIF.
*
*            CALL METHOD grid_estra->refresh_table_display.
*
*            CALL METHOD grid_estra->refresh_table_display.
*
*
*            READ TABLE tg_saida_estra[]   INTO DATA(wl_estrat) WITH KEY nivel = '2'.
*            READ TABLE tg_solicitacoes[]  INTO DATA(wl_sol)   WITH KEY imobilizado = wl_solicitacoe-imobilizado.
*
*            APPEND wl_estrat TO tl_estra[].
*            APPEND wl_sol   TO tl_solicitacoes[].
*
*            CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
*              EXPORTING
*                v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
*                v_zugdt        = vl_zugdt
*                v_motivo       = vl_motivo
*                v_obs          = vl_obs
*              IMPORTING
*                msg            = v_msg      " Comentário
*              TABLES
*                t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
*                t_estra        = tl_estra.
*
*
*            CLEAR:w_index.
*            READ TABLE tg_saida_estra INTO wl_estrat WITH KEY anln1 = wl_solicitacoe-imobilizado
*                                                             nivel = '2'.
*
*            wl_estrat-opcoes = '@2W@'.
*            wl_estrat-estado = '@01@'.
*
*
*            MODIFY tg_saida_estra[] FROM wl_estrat INDEX 2.
*
**          CALL FUNCTION 'Z_SOL_BAIXA_IMOB_LISTA'
**            EXPORTING
**              v_usuario      = sy-uname
**            IMPORTING
**              msg            = vl_msg           " Comentário
**            TABLES
**              t_solicitacoes = tg_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
**              t_estra        = tg_estra         " Baixa de Imobilizado - Estratégia de liberação
**              t_detalhes     = tg_detalhes.     " Baixa de Imobilizado - Detalhes
*
*            DELETE ADJACENT DUPLICATES FROM tg_solicitacoes COMPARING ALL FIELDS.
*            IF ( tg_solicitacoes[] IS NOT INITIAL ).
**            tg_saida_sol[] = tg_solicitacoes[].
*            ENDIF.
*
*            IF ( obg_container_sol IS NOT INITIAL ).
*              CALL METHOD grid_sol->refresh_table_display
*                EXPORTING
*                  is_stable = wa_stable.
*            ENDIF.
*            IF ( obg_container_estra IS NOT INITIAL ).
*              CALL METHOD grid_estra->refresh_table_display
*                EXPORTING
*                  is_stable = wa_stable.
*            ENDIF.
*            IF ( obg_container_docs IS NOT INITIAL ).
*              CALL METHOD grid_docs->refresh_table_display
*                EXPORTING
*                  is_stable = wa_stable.
*            ENDIF.
*
*          ENDIF.
*
*        ENDIF.
*
**  valida nivel 3
*
*        READ TABLE tg_estra INTO DATA(wl_aprovi) WITH KEY anln1 =  wl_solicitacoe-imobilizado
**                                                           aprovador = sy-uname
*                                                           estado = '@01@'"sucesso
*                                                           nivel = '2'."nivel 2
*
*        IF sy-subrc = 0.
*
*          READ TABLE tg_estra INTO DATA(wl_aprovar3) WITH KEY anln1 =  wl_solicitacoe-imobilizado
*                                                       aprovador = sy-uname
*                                                       nivel = '3'."nivel 2
*
*          IF sy-subrc = 0.
*
*            READ TABLE t_index INTO  w_index WITH KEY imobilizado = wl_solicitacoe-imobilizado
*                                                      aprovador = sy-uname
*                                                      nivel = '3'."nivel 1
*
*            wl_aprovar3-opcoes = '@2W@'.
*            wl_aprovar3-estado = '@01@'.
*
*            MODIFY  tg_estra FROM wl_aprovar3 INDEX w_index-ind TRANSPORTING opcoes estado .
*
*            READ TABLE t_zaa008 INTO w_zaa008 WITH KEY bukrs = wl_solicitacoe-empresa
*                                                       werks = wl_solicitacoe-filial
*                                                       anln1 = wl_solicitacoe-imobilizado.
*
*            IF sy-subrc = 0.
*
*              w_zaa008-aprovador = sy-uname.
*              w_zaa008-nivel = '3'.
*              INSERT INTO zaa008 VALUES w_zaa008.
*
*            ENDIF.
*            CALL METHOD grid_estra->refresh_table_display.
*
*
*            READ TABLE tg_saida_estra[]   INTO DATA(wl_estrati) WITH KEY nivel = '3'.
*            READ TABLE tg_solicitacoes[]  INTO DATA(wl_sol2)   WITH KEY imobilizado = wl_solicitacoe-imobilizado.
*
*            APPEND wl_estrati TO tl_estra[].
*            APPEND wl_sol2   TO tl_solicitacoes[].
*
*            CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
*              EXPORTING
*                v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
*                v_zugdt        = vl_zugdt
*                v_motivo       = vl_motivo
*                v_obs          = vl_obs
*              IMPORTING
*                msg            = v_msg      " Comentário
*              TABLES
*                t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
*                t_estra        = tl_estra.
*
*
*            CLEAR:w_index.
*            READ TABLE tg_saida_estra INTO wl_estrati WITH KEY anln1 = wl_solicitacoe-imobilizado
*                                                             nivel = '3'.
*
*            wl_estrati-opcoes = '@2W@'.
*            wl_estrati-estado = '@01@'.
*
*
*            MODIFY tg_saida_estra[] FROM wl_estrati INDEX 3.
*
**          CALL FUNCTION 'Z_SOL_BAIXA_IMOB_LISTA'
**            EXPORTING
**              v_usuario      = sy-uname
**            IMPORTING
**              msg            = vl_msg           " Comentário
**            TABLES
**              t_solicitacoes = tg_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
**              t_estra        = tg_estra         " Baixa de Imobilizado - Estratégia de liberação
**              t_detalhes     = tg_detalhes.     " Baixa de Imobilizado - Detalhes
*
*            DELETE ADJACENT DUPLICATES FROM tg_solicitacoes COMPARING ALL FIELDS.
*            IF ( tg_solicitacoes[] IS NOT INITIAL ).
**            tg_saida_sol[] = tg_solicitacoes[].
*            ENDIF.
*
*            IF ( obg_container_sol IS NOT INITIAL ).
*              CALL METHOD grid_sol->refresh_table_display
*                EXPORTING
*                  is_stable = wa_stable.
*            ENDIF.
*            IF ( obg_container_estra IS NOT INITIAL ).
*              CALL METHOD grid_estra->refresh_table_display
*                EXPORTING
*                  is_stable = wa_stable.
*            ENDIF.
*            IF ( obg_container_docs IS NOT INITIAL ).
*              CALL METHOD grid_docs->refresh_table_display
*                EXPORTING
*                  is_stable = wa_stable.
*            ENDIF.
*
*          ENDIF.
*        ENDIF.
*
**gravar na tabela
*
*
*      ENDLOOP.




*DELETE ADJACENT DUPLICATES FROM tg_estra COMPARING ALL FIELDS.
*        CALL METHOD grid_estra->refresh_table_display.
*      PERFORM atualiza_solicitacoes.
      WHEN 'DES'." Desaprovar em massa


        LOOP AT tg_saida_sol[] INTO DATA(wl_soli) WHERE flag = 'X'.



          READ TABLE tg_estra INTO DATA(wl_dessaso) WITH KEY anln1 =  wl_soli-imobilizado
                                                           aprovador = sy-uname
                                                           nivel = '3'."nivel 3

          IF sy-subrc = 0.

            READ TABLE t_index INTO  w_index WITH KEY imobilizado = wl_soli-imobilizado
                                                      aprovador = sy-uname
                                                      nivel = '3'."nivel 1

            wl_dessaso-opcoes = '@3J@'.
            wl_dessaso-estado = '@5D@'.
            MODIFY tg_estra FROM wl_dessaso INDEX w_index-ind TRANSPORTING opcoes estado .

            DELETE FROM  zaa008 WHERE bukrs = wl_soli-empresa
                                  AND werks = wl_soli-filial
                                  AND anln1 = wl_soli-imobilizado
                                  AND nivel = '3'
                                  AND aprovador = sy-uname.

            CALL METHOD grid_estra->refresh_table_display.


            READ TABLE tg_saida_estra[]   INTO DATA(wl_estratu) WITH KEY nivel = '3'.
            READ TABLE tg_solicitacoes[]  INTO DATA(wl_sol33)   WITH KEY imobilizado = wl_soli-imobilizado.

            APPEND wl_estratu TO tl_estra[].
            APPEND wl_sol33   TO tl_solicitacoes[].

            CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
              EXPORTING
                v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
                v_zugdt        = vl_zugdt
                v_motivo       = vl_motivo
                v_obs          = vl_obs
              IMPORTING
                msg            = v_msg      " Comentário
              TABLES
                t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
                t_estra        = tl_estra.


            CLEAR:w_index.
            READ TABLE tg_saida_estra INTO wl_estratu WITH KEY anln1 = wl_soli-imobilizado
                                                             nivel = '3'.

            wl_estratu-opcoes = '@3J@'.
            wl_estratu-estado = '@5D@'.


            MODIFY tg_saida_estra[] FROM wl_estratu INDEX 3.

*          CALL FUNCTION 'Z_SOL_BAIXA_IMOB_LISTA'
*            EXPORTING
*              v_usuario      = sy-uname
*            IMPORTING
*              msg            = vl_msg           " Comentário
*            TABLES
*              t_solicitacoes = tg_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
*              t_estra        = tg_estra         " Baixa de Imobilizado - Estratégia de liberação
*              t_detalhes     = tg_detalhes.     " Baixa de Imobilizado - Detalhes

            IF ( tg_solicitacoes[] IS NOT INITIAL ).
*            tg_saida_sol[] = tg_solicitacoes[].
            ENDIF.

            IF ( obg_container_sol IS NOT INITIAL ).
              CALL METHOD grid_sol->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.
            ENDIF.
            IF ( obg_container_estra IS NOT INITIAL ).
              CALL METHOD grid_estra->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.
            ENDIF.
            IF ( obg_container_docs IS NOT INITIAL ).
              CALL METHOD grid_docs->refresh_table_display
                EXPORTING
                  is_stable = wa_stable.
            ENDIF.

          ENDIF.

*verifica se tem nivel 2 para desasociar

          READ TABLE tg_estra INTO DATA(wl_dessa) WITH KEY anln1 =  wl_soli-imobilizado
*                                                                aprovador = sy-uname
                                                                  estado = '@5D@'
                                                                  nivel = '3'."nivel 3


          IF sy-subrc = 0.

            READ TABLE tg_estra INTO DATA(wl_dessas) WITH KEY       anln1 =  wl_soli-imobilizado
                                                                    aprovador = sy-uname
                                                                    nivel = '2'."nivel 2

            IF sy-subrc = 0.

              READ TABLE t_index INTO  w_index WITH KEY imobilizado = wl_soli-imobilizado
                                                        aprovador = sy-uname
                                                        nivel = '2'."nivel 1

              wl_dessas-opcoes = '@3J@'.
              wl_dessas-estado = '@5D@'.
              MODIFY tg_estra FROM wl_dessas INDEX w_index-ind TRANSPORTING opcoes estado .

              DELETE FROM  zaa008 WHERE bukrs = wl_soli-empresa
                                    AND werks = wl_soli-filial
                                    AND anln1 = wl_soli-imobilizado
                                    AND nivel = '2'
                                    AND aprovador = sy-uname.

              CALL METHOD grid_estra->refresh_table_display.

              READ TABLE tg_saida_estra[]   INTO DATA(wl_estratut) WITH KEY nivel = '2'.
              READ TABLE tg_solicitacoes[]  INTO DATA(wl_sol31)   WITH KEY imobilizado = wl_soli-imobilizado.

              APPEND wl_estratu TO tl_estra[].
              APPEND wl_sol31   TO tl_solicitacoes[].

              CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
                EXPORTING
                  v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
                  v_zugdt        = vl_zugdt
                  v_motivo       = vl_motivo
                  v_obs          = vl_obs
                IMPORTING
                  msg            = v_msg      " Comentário
                TABLES
                  t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
                  t_estra        = tl_estra.


              CLEAR:w_index.
              READ TABLE tg_saida_estra INTO wl_estratut WITH KEY anln1 = wl_soli-imobilizado
                                                               nivel = '2'.

              wl_estratut-opcoes = '@3J@'.
              wl_estratut-estado = '@5D@'.


              MODIFY tg_saida_estra[] FROM wl_estratut INDEX 2.
*
*          CALL FUNCTION 'Z_SOL_BAIXA_IMOB_LISTA'
*            EXPORTING
*              v_usuario      = sy-uname
*            IMPORTING
*              msg            = vl_msg           " Comentário
*            TABLES
*              t_solicitacoes = tg_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
*              t_estra        = tg_estra         " Baixa de Imobilizado - Estratégia de liberação
*              t_detalhes     = tg_detalhes.     " Baixa de Imobilizado - Detalhes

              IF ( tg_solicitacoes[] IS NOT INITIAL ).
*            tg_saida_sol[] = tg_solicitacoes[].
              ENDIF.

              IF ( obg_container_sol IS NOT INITIAL ).
                CALL METHOD grid_sol->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.
              ENDIF.
              IF ( obg_container_estra IS NOT INITIAL ).
                CALL METHOD grid_estra->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.
              ENDIF.
              IF ( obg_container_docs IS NOT INITIAL ).
                CALL METHOD grid_docs->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.
              ENDIF.


            ENDIF.
          ENDIF.


*    verifica nivel 1

          READ TABLE tg_estra INTO DATA(wl_dess) WITH KEY anln1 =  wl_soli-imobilizado
*                                                                    aprovador = sy-uname
                                                                      estado = '@5D@'
                                                                      nivel = '2'."nivel 2


          IF sy-subrc = 0.

            READ TABLE tg_estra INTO DATA(wl_des) WITH KEY       anln1 =  wl_soli-imobilizado
                                                                    aprovador = sy-uname
                                                                    nivel = '1'."nivel 2

            IF sy-subrc = 0.

              READ TABLE t_index INTO  w_index WITH KEY imobilizado = wl_soli-imobilizado
                                                        aprovador = sy-uname
                                                        nivel = '1'."nivel 1
              wl_des-opcoes = '@3J@'.
              wl_des-estado = '@5D@'.
              MODIFY tg_estra FROM wl_des INDEX w_index-ind TRANSPORTING opcoes estado .

              DELETE FROM  zaa008 WHERE bukrs = wl_soli-empresa
                                    AND werks = wl_soli-filial
                                    AND anln1 = wl_soli-imobilizado
                                    AND nivel = '1'
                                    AND aprovador = sy-uname.

              CALL METHOD grid_estra->refresh_table_display.

              READ TABLE tg_saida_estra[]   INTO DATA(wl_estratute) WITH KEY nivel = '1'.
              READ TABLE tg_solicitacoes[]  INTO DATA(wl_sol34)   WITH KEY imobilizado = wl_soli-imobilizado.

              APPEND wl_estratu TO tl_estra[].
              APPEND wl_sol34   TO tl_solicitacoes[].

              CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
                EXPORTING
                  v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
                  v_zugdt        = vl_zugdt
                  v_motivo       = vl_motivo
                  v_obs          = vl_obs
                IMPORTING
                  msg            = v_msg      " Comentário
                TABLES
                  t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
                  t_estra        = tl_estra.


              CLEAR:w_index.
              READ TABLE tg_saida_estra INTO wl_estratut WITH KEY anln1 = wl_soli-imobilizado
                                                               nivel = '1'.

              wl_estratute-opcoes = '@3J@'.
              wl_estratute-estado = '@5D@'.


              MODIFY tg_saida_estra[] FROM wl_estratute INDEX 1.

*          CALL FUNCTION 'Z_SOL_BAIXA_IMOB_LISTA'
*            EXPORTING
*              v_usuario      = sy-uname
*            IMPORTING
*              msg            = vl_msg           " Comentário
*            TABLES
*              t_solicitacoes = tg_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
*              t_estra        = tg_estra         " Baixa de Imobilizado - Estratégia de liberação
*              t_detalhes     = tg_detalhes.     " Baixa de Imobilizado - Detalhes

              IF ( tg_solicitacoes[] IS NOT INITIAL ).
*            tg_saida_sol[] = tg_solicitacoes[].
              ENDIF.

              IF ( obg_container_sol IS NOT INITIAL ).
                CALL METHOD grid_sol->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.
              ENDIF.
              IF ( obg_container_estra IS NOT INITIAL ).
                CALL METHOD grid_estra->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.
              ENDIF.
              IF ( obg_container_docs IS NOT INITIAL ).
                CALL METHOD grid_docs->refresh_table_display
                  EXPORTING
                    is_stable = wa_stable.
              ENDIF.


            ENDIF.
          ENDIF.

        ENDLOOP.
        CALL METHOD grid_estra->refresh_table_display.
*      PERFORM atualiza_solicitacoes.

      WHEN 'REFRESH'.
        PERFORM atualiza_solicitacoes.
      WHEN 'REJ'.
        READ TABLE tg_saida_estra[] INTO DATA(wl_estra) WITH KEY aprovador = sy-uname
                                                                 opcoes    = icon_set_state.
        IF ( sy-subrc EQ 0 ).

          IF ( wl_estra-opcoes EQ icon_reject ).
            wl_estra-opcoes = icon_set_state.
          ELSEIF ( wl_estra-opcoes EQ icon_set_state ).
            wl_estra-opcoes = icon_reject.

            "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
            " Condição desabilitada item 7 conforme especifcação
            CLEAR:tl_controller_obs.
            CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
              EXPORTING
                im_title = 'Observação Do Controller:'
*               IM_DISPLAY_MODE = ''
              CHANGING
                ch_text  = tl_controller_obs.

            IF tl_controller_obs IS NOT INITIAL.

              READ TABLE tl_controller_obs[] INTO DATA(line_obs) INDEX 1.
              vl_obs = line_obs.

            ENDIF.

          ENDIF.

          CLEAR: tl_estra,tl_estra[],tl_solicitacoes,tl_solicitacoes[].

          APPEND wl_estra TO tl_estra[].

          LOOP AT tl_estra[] ASSIGNING FIELD-SYMBOL(<get_values>).
            wa_solicitacoes-empresa = <get_values>-bukrs.
            wa_solicitacoes-imobilizado = <get_values>-anln1.
            wa_solicitacoes-filial = <get_values>-gsber.
            wa_solicitacoes-kostl = <get_values>-kostl.
            wa_solicitacoes-usuario = <get_values>-aprovador.
            APPEND wa_solicitacoes TO tl_solicitacoes[].
          ENDLOOP.


          DATA: dtnow(8) TYPE c.
          DATA: AUX_anln1 TYPE anla-anln1.

          CLEAR: dtnow, AUX_anln1.
          dtnow = sy-datum.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'  "Conversion exit ALPHA, internal->external
            EXPORTING
              input  = wl_estra-anln1
            IMPORTING
              output = AUX_anln1.

          CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
            EXPORTING
              v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
              v_zugdt        = vl_zugdt
              v_motivo       = vl_motivo
              v_obs          = vl_obs
            IMPORTING
              msg            = v_msg      " Comentário
            TABLES
              t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
              t_estra        = tl_estra.        " Baixa de Imobilizado - Estratégia de liberação

          CLEAR: vl_zugdt, vl_motivo, vl_obs.

          MODIFY tg_saida_estra[] FROM wl_estra INDEX sy-tabix TRANSPORTING opcoes.

          CLEAR: tl_estra,tl_estra[],tl_solicitacoes,tl_solicitacoes[].

          CALL METHOD grid_estra->refresh_table_display.

          PERFORM atualiza_solicitacoes.

        ENDIF.
      WHEN 'ENT'.
        "
    ENDCASE.

  ELSE.
    CLEAR vclear.

  ENDIF.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_EXIT  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_exit INPUT.

  LEAVE PROGRAM.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0140  OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0140 OUTPUT.
  SET PF-STATUS 'ST_140'.
  SET TITLEBAR 'T_140'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  SET_VALUES_MOTIVO
*&---------------------------------------------------------------------*
*       DEFINE OS VALORES PARA O MENU DROP DOWN 'MOTIVO DA BAIXA'
*----------------------------------------------------------------------*
*FORM SET_VALUES_MOTIVO.
*
*  DATA: LIST_MOTIVOS TYPE VRM_VALUES,
*        VALUE_MOTIVO LIKE LINE OF LIST_MOTIVOS.
*
*  DATA: VL_ID TYPE VRM_ID.
*  VL_ID = 'WG_ZAA007-MOTIVO'.
*
*  CLEAR: LIST_MOTIVOS[].
*
*  VALUE_MOTIVO-KEY  = 'Sucata'.
*  VALUE_MOTIVO-TEXT = 'Sucata'.
*  APPEND VALUE_MOTIVO TO LIST_MOTIVOS[].
*
*  VALUE_MOTIVO-KEY  = 'Queimado/Sucata'.
*  VALUE_MOTIVO-TEXT = 'Queimado/Sucata'.
*  APPEND VALUE_MOTIVO TO LIST_MOTIVOS[].
*
*  CALL FUNCTION 'VRM_SET_VALUES'
*    EXPORTING
*      ID     = 'WG_ZAA007-MOTIVO'
*      VALUES = LIST_MOTIVOS.
*
*ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  BUSCA_ANEXO
*&---------------------------------------------------------------------*
FORM busca_anexo .

  DATA: lt_celltab TYPE lvc_t_styl,
        vl_obj_key TYPE sibflporb-instid,
        tl_anexos  TYPE TABLE OF bdn_con,
        vl_ano(4)  TYPE c,
        ls_celltab TYPE lvc_s_styl,
        l_mode     TYPE raw4.

  LOOP AT tg_saida_detalhes ASSIGNING <wg_detalhes>.

    CLEAR: lt_celltab[], vl_ano.
    vl_ano = <wg_detalhes>-dt_solicitacao+0(4).

    vl_obj_key = |ZAA18{ <wg_detalhes>-anln1 }{ <wg_detalhes>-werks }{ vl_ano }|.

*    CALL FUNCTION 'BDS_GOS_CONNECTIONS_GET'
*      EXPORTING
**       LOGICAL_SYSTEM     =
*        classname          = 'ZAA13'
*        objkey             = vl_obj_key
*        client             = sy-mandt
*      TABLES
*        gos_connections    = tl_anexos
*      EXCEPTIONS
*        no_objects_found   = 1
*        internal_error     = 2
*        internal_gos_error = 3
*        OTHERS             = 4.
*
*    IF ( tl_anexos[] IS NOT INITIAL ).
    "128405 CS2023000909 Melhorias ZAA19 - Transação de baixa - PSA
    SELECT SINGLE * FROM srgbtbrel WHERE instid_a = @vl_obj_key AND typeid_a = 'ZAA13' AND reltype = 'ATTA' INTO @DATA(WA_srgbtbrel).

    IF sy-subrc = 0.

      <wg_detalhes>-anexo = '@1E@'.

      l_mode                = cl_gui_alv_grid=>mc_style_disabled.
      ls_celltab-style      = cl_gui_alv_grid=>mc_style_button.
      ls_celltab-fieldname  = 'ANEXO'.
      INSERT ls_celltab INTO TABLE lt_celltab.
      INSERT LINES OF lt_celltab INTO TABLE <wg_detalhes>-cellstyles.

    ELSE.
      <wg_detalhes>-anexo = '@1F@'.
    ENDIF.

  ENDLOOP.

ENDFORM.


FORM aprova.

  DELETE ADJACENT DUPLICATES FROM tl_solicitacoes[].
  DELETE tl_estra[] WHERE estado = '@01@' AND opcoes = '@2W@'.

  CALL FUNCTION 'Z_SOL_BAIXA_IMOB_EXECUTA'
    EXPORTING
      v_usuario      = sy-uname   " Campo do sistema ABAP: nome do usuário atual
      v_zugdt        = vl_zugdt
      v_motivo       = vl_motivo
      v_obs          = vl_obs
    IMPORTING
      msg            = v_msg      " Comentário
    TABLES
      t_solicitacoes = tl_solicitacoes  " Baixa de Imobilizado - Solicitações p/ liberação
      t_estra        = tl_estra.        " Baixa de Imobilizado - Estratégia de liberação

  CLEAR: vl_zugdt, vl_motivo, vl_obs.

  LOOP AT tl_estra[] INTO wl_estra FROM 1.
    MODIFY TABLE tg_saida_estra[] FROM wl_estra.
  ENDLOOP.

  CALL METHOD grid_estra->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

  MESSAGE s836(sd) DISPLAY LIKE 'S' WITH v_msg.
  CLEAR: wg_sol_resumo, wg_sol_resumo-empresa, wg_sol_resumo-filial, wg_sol_resumo-usuario, wg_sol_resumo-dt_solicita.

ENDFORM.

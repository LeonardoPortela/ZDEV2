*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_0300.
*----------------------------------------------------------------------*

CLASS lcl_alv_toolbar_0306 DEFINITION DEFERRED.
CLASS lcl_application_0300 DEFINITION DEFERRED.
CLASS lcl_event_handler_0306 DEFINITION DEFERRED.

TYPES: BEGIN OF ty_node_info_logs.
TYPES: node_key    TYPE tv_nodekey,
       item_name   TYPE tv_itmname,
       registro    TYPE zde_log_registro,
       dt_registro TYPE char10,
       hr_registro TYPE char08,
       us_registro TYPE char30.
TYPES: END OF ty_node_info_logs.

DATA: ck_alterado_carga TYPE c LENGTH 1,
      ck_alterado_ov    TYPE c LENGTH 1,
      ck_alterado_chave TYPE c LENGTH 1,
      ck_alterado_nota  TYPE c LENGTH 1,
      vg_tl_0310        TYPE sy-dynnr,
      vg_tl_0302        TYPE sy-dynnr,
      vg_tl_0305        TYPE sy-dynnr,
      vg_tl_0312        TYPE sy-dynnr,
      lbl_des_nota      TYPE string VALUE '--------------------------------------------------------------------------------------------------------'.

DATA: ctl_cccontainer_0306    TYPE REF TO cl_gui_custom_container,
      ctl_alv_0306            TYPE REF TO cl_gui_alv_grid,
      it_fieldcatalog_0306    TYPE lvc_t_fcat,
      gs_variant_0306         TYPE disvariant,
      gs_layout_0306          TYPE lvc_s_layo,
      event_handler_0306      TYPE REF TO lcl_event_handler_0306,
      obg_toolbar_0306        TYPE REF TO lcl_alv_toolbar_0306,
      obj_toolbarmanager_0306 TYPE REF TO cl_alv_grid_toolbar_manager,
      it_notas_sel            TYPE TABLE OF zde_zsdt0001nt_alv WITH HEADER LINE,
      gs_scroll_col_0306      TYPE lvc_s_col,
      gs_scroll_row_0306      TYPE lvc_s_roid,
      wa_stable_0306          TYPE lvc_s_stbl,
      nm_field_set_carga      TYPE c LENGTH 50,
      nm_field_set_nota       TYPE c LENGTH 50,
      pos                     TYPE i.

DATA: ctl_cccontainer_9012 TYPE REF TO cl_gui_custom_container,
      picture_9012         TYPE REF TO cl_gui_picture.

DATA: docking_0300       TYPE REF TO cl_gui_docking_container,
      docking_0300_se    TYPE REF TO cl_gui_docking_container,
      tree_0300          TYPE REF TO cl_gui_column_tree,
      g_application_0300 TYPE REF TO lcl_application_0300,
      events_0300        TYPE cntl_simple_events,
      node_table_0300    TYPE treev_ntab,
      item_table_0300    TYPE STANDARD TABLE OF mtreeitm,
      it_tree_info_log   TYPE TABLE OF ty_node_info_logs WITH HEADER LINE.

CONSTANTS:
  BEGIN OF c_tree_0300,
    column1 TYPE tv_itmname VALUE 'USUARIO',
    column2 TYPE tv_itmname VALUE 'DATA',
    column3 TYPE tv_itmname VALUE 'HORA',
  END OF c_tree_0300.

CLASS lcl_application_0300 DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_link_click  FOR EVENT link_click OF cl_gui_column_tree IMPORTING node_key item_name.
ENDCLASS.                    "LCL_APPLICATION DEFINITION

CLASS lcl_alv_toolbar_0306 DEFINITION.
  PUBLIC SECTION.
    METHODS: constructor   IMPORTING io_alv_grid TYPE REF TO cl_gui_alv_grid,
      on_toolbar           FOR EVENT toolbar OF cl_gui_alv_grid IMPORTING e_object,
      handle_user_command  FOR EVENT user_command OF cl_gui_alv_grid IMPORTING e_ucomm,
      double_click_command FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row e_column es_row_no.
ENDCLASS.

CLASS lcl_event_handler_0306 DEFINITION.
  PUBLIC SECTION.
    METHODS handle_double_click  FOR EVENT double_click  OF cl_gui_alv_grid IMPORTING e_row.
ENDCLASS.                    "lcl_event_handler DEFINITION

CLASS lcl_event_handler_0306 IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM handle_double_click_0306 USING e_row.
  ENDMETHOD.                    "HANDLE_DOUBLE_CLICK
ENDCLASS.                    "lcl_event_handler DEFINITION

*----------------------------------------------------------------------*
*       CLASS LCL_ALV_TOOLBAR IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_toolbar_0306 IMPLEMENTATION.

  METHOD constructor.
*   Create ALV toolbar manager instance
    CREATE OBJECT obj_toolbarmanager_0306
      EXPORTING
        io_alv_grid = io_alv_grid.

  ENDMETHOD.                    "constructor

  METHOD on_toolbar.

    DATA: ty_toolbar   TYPE stb_button.
*    "Separador
    ty_toolbar-butn_type = 3.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_change_text.
    ty_toolbar-function  = 'EDITAR'.
    ty_toolbar-quickinfo = TEXT-015.
    "TY_TOOLBAR-TEXT      = TEXT-015.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    "Marcar Todos os Documentos
    ty_toolbar-icon      = icon_delete.
    ty_toolbar-function  = 'ELIMINAR'.
    ty_toolbar-quickinfo = TEXT-014.
    "TY_TOOLBAR-TEXT      = TEXT-014.
    ty_toolbar-butn_type = 0.
    APPEND ty_toolbar TO e_object->mt_toolbar.
    CLEAR ty_toolbar.

    CALL METHOD obj_toolbarmanager_0306->reorganize
      EXPORTING
        io_alv_toolbar = e_object.

  ENDMETHOD.                    "on_toolbar

  METHOD handle_user_command.

    DATA: et_index_rows	TYPE lvc_t_row.

    CALL METHOD ctl_alv_0306->get_selected_rows
      IMPORTING
        et_index_rows = et_index_rows.

    CLEAR: it_notas_sel[].
    LOOP AT et_index_rows INTO DATA(wa_index_rows).
      READ TABLE it_notas INTO DATA(wa_notas_sel) INDEX wa_index_rows-index.
      APPEND wa_notas_sel TO it_notas_sel.
    ENDLOOP.

    CASE e_ucomm.
      WHEN 'ELIMINAR'.
        CLEAR: wa_nota_selecionada.

        CLEAR: event_handler_0312a.

        IF ctl_alv_0312a IS NOT INITIAL.
          ctl_alv_0312a->free( ).
        ENDIF.
        CLEAR: ctl_alv_0312a.

        CLEAR: obg_toolbar_0312a.

        IF ctl_cccontainer_0312a IS NOT INITIAL.
          ctl_cccontainer_0312a->free( ).
        ENDIF.
        CLEAR: ctl_cccontainer_0312a.

        CLEAR: it_takes_saldo[].

        LOOP AT it_notas_sel INTO wa_notas_sel.
          TRY .
              objeto->excluir_nota_fiscal( EXPORTING i_nota = wa_notas_sel IMPORTING e_notas = it_notas[] ).
            CATCH zcx_carga INTO ex_carga.
              ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          ENDTRY.
        ENDLOOP.
        LEAVE TO SCREEN 0300.

      WHEN 'EDITAR'.

        CLEAR: wa_nota_selecionada.

        CLEAR: event_handler_0312a.

        IF ctl_alv_0312a IS NOT INITIAL.
          ctl_alv_0312a->free( ).
        ENDIF.
        CLEAR: ctl_alv_0312a.

        CLEAR: obg_toolbar_0312a.

        IF ctl_cccontainer_0312a IS NOT INITIAL.
          ctl_cccontainer_0312a->free( ).
        ENDIF.
        CLEAR: ctl_cccontainer_0312a.

        CLEAR: it_takes_saldo[].

        READ TABLE it_notas_sel INDEX 1 INTO zde_zsdt0001nt_alv.
        CALL SCREEN 0307 STARTING AT 40 05.
        LEAVE TO SCREEN 0300.

    ENDCASE.
  ENDMETHOD. "zm_handle_user_command

  METHOD double_click_command.

    CLEAR: wa_nota_selecionada.


  ENDMETHOD.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS LCL_APPLICATION_0300 IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_application_0300 IMPLEMENTATION.

  METHOD  handle_link_click.
    PERFORM mostrar_node_log USING node_key item_name.
  ENDMETHOD.                    "HANDLE_LINK_CLICK

ENDCLASS.                    "LCL_APPLICATION IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  STATUS_0300  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0300 OUTPUT.

  DATA: it_tucomm                TYPE TABLE OF sy-ucomm,
        lc_hierarchy_header_0300 TYPE treev_hhdr,
        lc_event_0300            TYPE cntl_simple_event,
        html_pagina              TYPE string.

  CLEAR: it_tucomm.

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.

      TRY .
          objeto->get_ck_saida_automatica( ).
        CATCH zcx_carga.
          APPEND 'FATURAMENT' TO it_tucomm.
      ENDTRY.

      IF objeto->ck_alterou EQ abap_false.
        APPEND 'SAVE' TO it_tucomm.
      ENDIF.

      IF objeto->at_manutencao EQ abap_true.
        TRY .
            objeto->get_ck_sol_ajuste_nao_proc( ).
          CATCH zcx_carga.    "
            APPEND 'PROCESSSOL' TO it_tucomm.
        ENDTRY.
      ELSE.
        APPEND 'PROCESSSOL' TO it_tucomm.
      ENDIF.
      IF ( zde_zsdt0001cg_alv-id_carga IS INITIAL ) OR ( objeto->at_manutencao EQ abap_true ).
        APPEND 'SHOWLOGS' TO it_tucomm.
      ENDIF.

      objeto->get_tp_status( IMPORTING e_tp_status = DATA(e_tp_status) ).

      APPEND 'GMO01' TO it_tucomm.
      APPEND 'GMO02' TO it_tucomm.

      CASE e_tp_status.
        WHEN zif_carga=>st_status_aberto.
          APPEND 'ABRIR'      TO it_tucomm.
          APPEND 'CONFERIR'   TO it_tucomm.
          APPEND 'FATURAMENT' TO it_tucomm.
          APPEND 'NOVA_SOLIC' TO it_tucomm.
          IF zde_zsdt0001cg_alv-id_carga IS INITIAL.
            APPEND 'CANCELAR' TO it_tucomm.
          ENDIF.
        WHEN zif_carga=>st_status_fechado.
          APPEND 'ADD_NOTA'   TO it_tucomm.
          APPEND 'FECHAR'     TO it_tucomm.
          APPEND 'NOVA_SOLIC' TO it_tucomm.
        WHEN zif_carga=>st_status_cancelada.
          APPEND 'ADD_NOTA'   TO it_tucomm.
          APPEND 'ABRIR'      TO it_tucomm.
          APPEND 'FECHAR'     TO it_tucomm.
          APPEND 'CONFERIR'   TO it_tucomm.
          APPEND 'FATURAMENT' TO it_tucomm.
          APPEND 'CANCELAR'   TO it_tucomm.
          APPEND 'NOVA_SOLIC' TO it_tucomm.
        WHEN zif_carga=>st_status_conferido.
          APPEND 'ADD_NOTA' TO it_tucomm.
          APPEND 'ABRIR'    TO it_tucomm.
          APPEND 'CONFERIR' TO it_tucomm.
          IF objeto->at_manutencao EQ abap_true.
            APPEND 'NOVA_SOLIC' TO it_tucomm.
          ELSE.
            APPEND 'CANCELAR' TO it_tucomm.
          ENDIF.
      ENDCASE.

      IF ck_registro_log EQ abap_true.
        APPEND 'ADD_NOTA' TO it_tucomm.
        APPEND 'ABRIR'    TO it_tucomm.
        APPEND 'FECHAR'   TO it_tucomm.
        APPEND 'CONFERIR' TO it_tucomm.
        APPEND 'CANCELAR' TO it_tucomm.
        APPEND 'FATURAMENT' TO it_tucomm.
        "Registro Log: Data: &1 Hora: &2 Usuário: &3
        SET TITLEBAR 'TL0301' WITH it_tree_info_log-dt_registro it_tree_info_log-hr_registro it_tree_info_log-us_registro.
      ELSEIF pmanut EQ abap_true.
        SET TITLEBAR 'TL0302'.
      ELSE.
        SET TITLEBAR 'TL0300'.
      ENDIF.

    WHEN zif_carga=>st_tp_carga_saida_opus.
      APPEND 'SAVE' TO it_tucomm.
      APPEND 'PROCESSSOL' TO it_tucomm.
      APPEND 'SHOWLOGS'   TO it_tucomm.
      APPEND 'GMO01'      TO it_tucomm.
      APPEND 'GMO02'      TO it_tucomm.
      APPEND 'ABRIR'      TO it_tucomm.
      APPEND 'CONFERIR'   TO it_tucomm.
      APPEND 'NOVA_SOLIC' TO it_tucomm.
      APPEND 'CANCELAR'   TO it_tucomm.
      APPEND 'ADD_NOTA'   TO it_tucomm.
      APPEND 'FECHAR'     TO it_tucomm.
      APPEND 'FATURAMENT' TO it_tucomm.
      SET TITLEBAR 'TL0300'.
    WHEN zif_carga=>st_tp_carga_saida_ent_fob.

      APPEND 'SAVE'       TO it_tucomm.
      APPEND 'PROCESSSOL' TO it_tucomm.
      APPEND 'SHOWLOGS'   TO it_tucomm.
      APPEND 'GMO01'      TO it_tucomm.
      APPEND 'GMO02'      TO it_tucomm.
      APPEND 'ABRIR'      TO it_tucomm.
      APPEND 'CONFERIR'   TO it_tucomm.
      APPEND 'NOVA_SOLIC' TO it_tucomm.
      APPEND 'CANCELAR'   TO it_tucomm.
      APPEND 'ADD_NOTA'   TO it_tucomm.
      APPEND 'FECHAR'     TO it_tucomm.

      TRY .
          objeto->get_ck_saida_automatica( ).
        CATCH zcx_carga.
          APPEND 'FATURAMENT' TO it_tucomm.
      ENDTRY.

      SET TITLEBAR 'TL0300'.
  ENDCASE.

  SET PF-STATUS 'PF0300' EXCLUDING it_tucomm.

  IF zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_aberto OR
     zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_cancelada OR
     zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_aprovado OR
     zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_enviado OR
     zde_zsdt0001acb_alv-tp_solicitacao_status EQ zif_carga=>st_status_manut_recusada.

    PERFORM retornar_html_workflow USING zde_zsdt0001acb_alv CHANGING html_pagina.

    IF docking_0300_se IS INITIAL.
      CREATE OBJECT docking_0300_se
        EXPORTING
          repid     = sy-repid
          dynnr     = sy-dynnr
          side      = cl_gui_docking_container=>dock_at_right
          extension = 400.

      cl_abap_browser=>show_html(
       EXPORTING
         html_string = html_pagina
         modal       = abap_false
         format      = cl_abap_browser=>landscape
         size        = cl_abap_browser=>medium
         container   = docking_0300_se ).
    ELSE.
      cl_abap_browser=>close_browser( ).
      cl_abap_browser=>show_html(
       EXPORTING
         html_string = html_pagina
         modal       = abap_false
         format      = cl_abap_browser=>landscape
         size        = cl_abap_browser=>medium
         container   = docking_0300_se ).
    ENDIF.

  ELSEIF zde_zsdt0001cg_alv-id_carga IS NOT INITIAL AND ck_mostrar_logs EQ abap_false AND docking_0300 IS INITIAL
     AND zde_zsdt0001cg_alv-tp_status = zif_carga=>st_status_conferido.

    CREATE OBJECT docking_0300
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = docking_0300->dock_at_right
        extension = 400.

    PERFORM cria_alv_documentos USING 2 docking_0300->screen0 docking_0300.

    PERFORM atualiza_tree USING zde_zsdt0001cg_alv-id_carga.

  ELSEIF zde_zsdt0001cg_alv-id_carga IS NOT INITIAL AND ck_mostrar_logs EQ abap_true AND docking_0300 IS INITIAL.

    CREATE OBJECT docking_0300
      EXPORTING
        repid     = sy-repid
        dynnr     = sy-dynnr
        side      = docking_0300->dock_at_right
        extension = 400.

    lc_hierarchy_header_0300-heading = TEXT-016.
    lc_hierarchy_header_0300-width   = 31.

    CLEAR: events_0300.
    lc_event_0300-eventid    = cl_gui_column_tree=>eventid_link_click.
    lc_event_0300-appl_event = 'X'.
    APPEND lc_event_0300 TO events_0300.

    CREATE OBJECT g_application_0300.

    CREATE OBJECT tree_0300
      EXPORTING
        parent                = docking_0300
        node_selection_mode   = tree_0300->node_sel_mode_single
        item_selection        = 'X'
        hierarchy_column_name = c_tree_0300-column1
        hierarchy_header      = lc_hierarchy_header_0300.

    tree_0300->set_registered_events( EXPORTING events = events_0300
      EXCEPTIONS
        cntl_error                = 1
        cntl_system_error         = 2
        illegal_event_combination = 3
        OTHERS                    = 4 ).

    IF sy-subrc IS NOT INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    SET HANDLER g_application_0300->handle_link_click FOR tree_0300.

    CALL METHOD tree_0300->add_column
      EXPORTING
        name        = c_tree_0300-column2
        width       = 20
        header_text = TEXT-017
        alignment   = cl_gui_column_tree=>align_center.

    CALL METHOD tree_0300->add_column
      EXPORTING
        name        = c_tree_0300-column3
        width       = 20
        header_text = TEXT-018
        alignment   = cl_gui_column_tree=>align_center.

    PERFORM atualiza_tree_logs.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300_exit INPUT.

  DATA: answer TYPE c.

  IF ok_code EQ 'CHNFE'.
    PERFORM informar_chave_nfe.
  ELSE.
    answer = '1'.
    IF objeto->ck_alterou EQ abap_true.
      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = TEXT-001
          text_question         = TEXT-002
          text_button_1         = TEXT-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = TEXT-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF sy-subrc IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDIF.
    CHECK answer EQ '1'.
    PERFORM limpa_tela_0300.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0300  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0300 INPUT.

  DATA: lc_titlebar      TYPE string,
        lc_text_question TYPE string.

  CASE ok_code.
    WHEN 'FATURAMENT'.

      PERFORM faturamento_9011.

    WHEN 'PROCESSSOL'.

      TRY .
          objeto->set_aceite_soli_manutencao( EXPORTING i_tp_aprovacao = 'XX' i_tp_resposta = 'X' ).
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_ordem_venda INTO ex_ordem_venda.    "
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_ordem_carregamento INTO ex_ordem.    "
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_cadastro INTO ex_cadastro.    "
          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_pedido_compra_exception INTO ex_pedido.    "
          ex_pedido->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_job INTO ex_job.    "
          ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        CATCH zcx_miro_exception INTO ex_miro.    "
          ex_miro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      ENDTRY.

    WHEN 'SAVE'.
      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = TEXT-020.
          lc_text_question = TEXT-021.
        WHEN abap_true.
          lc_titlebar      = TEXT-026.
          lc_text_question = TEXT-027.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = TEXT-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = TEXT-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      TRY .
          objeto->gravar_registro(
               )->get_info_alv_apresentacao( IMPORTING e_apresentacao = DATA(e_apresentacao)
               ).

          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_carga->msgid ex_carga->msgno.
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_parceiros->msgid ex_parceiros->msgno.
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_ordem_venda->msgid ex_ordem_venda->msgno.
      ENDTRY.

    WHEN 'ABRIR'.
      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = TEXT-022.
          lc_text_question = TEXT-023.
        WHEN abap_true.
          lc_titlebar      = TEXT-028.
          lc_text_question = TEXT-029.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = TEXT-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = TEXT-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      TRY .
          objeto->set_abrir( IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv
               )->get_info_alv_apresentacao( IMPORTING e_apresentacao = e_apresentacao ).

          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].

          PERFORM limpa_tela_0300.
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_soft_expert_workflow INTO ex_soft_expert_workflow.
          ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_job INTO ex_job.
          ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

    WHEN 'CANCELAR'.

      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = TEXT-024.
          lc_text_question = TEXT-025.
        WHEN abap_true.
          lc_titlebar      = TEXT-030.
          lc_text_question = TEXT-031.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = TEXT-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = TEXT-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      TRY .
          objeto->set_cancelar(
               )->get_info_alv_apresentacao( IMPORTING e_apresentacao = e_apresentacao
               ).

          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].

          PERFORM limpa_tela_0300.
        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      ENDTRY.

    WHEN 'FECHAR'.

      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = TEXT-101.
          lc_text_question = TEXT-102.
        WHEN abap_true.
          lc_titlebar      = TEXT-032.
          lc_text_question = TEXT-033.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = TEXT-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = TEXT-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      TRY .

          objeto->verif_saldo_ordem_venda( ).

          objeto->verif_ticket_pesagem( ).

          objeto->verif_peso_notas( ).

          objeto->set_fechar( IMPORTING e_fechou = DATA(e_fechou)
               )->get_info_alv_apresentacao( IMPORTING e_apresentacao = e_apresentacao
               ).

          zde_zsdt0001cg_alv  = e_apresentacao-carga.
          zde_zsdt0001od_alv  = e_apresentacao-ordem_carrega.
          zde_zsdt0001acb_alv = e_apresentacao-manutencao.
          it_notas[]          = e_apresentacao-notas[].
          it_ordens_venda[]   = e_apresentacao-ordem_venda[].
          it_pedido_compra[]  = e_apresentacao-pedido_compra[].
          it_takes_vincu[]    = e_apresentacao-takeup[].

          IF NOT e_fechou EQ abap_true.
            IF sy-msgid EQ 'ZCARGA'.
              CASE sy-msgno.
                WHEN 039.
                  SET CURSOR FIELD 'ZDE_ZSDT0001CG_ALV-ID_ENTRADA'.
              ENDCASE.
            ENDIF.
            EXIT.
          ELSE.

            zde_zsdt0001cg_alv-id_carga = objeto->carga-id_carga.

            IF pmanut EQ abap_true.

              zde_zsdt0001acb_alv-id_solicitacao = objeto->solicitacao_manutencao-id_solicitacao.

              objeto->free(
                )->limpar_registro(
                )->set_registro_manutencao( i_id_solicitacao = zde_zsdt0001acb_alv-id_solicitacao
                )->get_info_alv_apresentacao( IMPORTING e_apresentacao = e_apresentacao
                ).

            ELSE.

              objeto->free(
                )->limpar_registro(
                )->set_registro( i_id_carga = zde_zsdt0001cg_alv-id_carga
                )->get_info_alv_apresentacao( IMPORTING e_apresentacao = e_apresentacao
                ).

            ENDIF.

          ENDIF.
          PERFORM limpa_tela_0300.

        CATCH zcx_carga INTO ex_carga.
          ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_carga->msgid ex_carga->msgno.
          EXIT.
        CATCH zcx_ordem_carregamento INTO ex_ordem.
          ex_ordem->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_ordem->msgid ex_ordem->msgno.
          EXIT.
        CATCH zcx_cadastro INTO ex_cadastro.
          ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_cadastro->msgid ex_cadastro->msgno.
          EXIT.
        CATCH zcx_parceiros INTO ex_parceiros.
          ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_parceiros->msgid ex_parceiros->msgno.
          EXIT.
        CATCH zcx_ordem_venda INTO ex_ordem_venda.
          ex_ordem_venda->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_ordem_venda->msgid ex_ordem_venda->msgno.
          EXIT.
        CATCH zcx_soft_expert_workflow INTO ex_soft_expert_workflow.
          ex_soft_expert_workflow->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          PERFORM seta_campo USING ex_soft_expert_workflow->msgid ex_soft_expert_workflow->msgno.
          EXIT.
        CATCH zcx_job INTO ex_job.
          ex_job->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          EXIT.
      ENDTRY.

    WHEN 'CONFERIR'.

      CLEAR: ok_code.

      CASE objeto->at_manutencao.
        WHEN abap_false.
          lc_titlebar      = TEXT-103.
          lc_text_question = TEXT-104.
        WHEN abap_true.
          lc_titlebar      = TEXT-034.
          lc_text_question = TEXT-035.
      ENDCASE.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = lc_titlebar
          text_question         = lc_text_question
          text_button_1         = TEXT-003
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = TEXT-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      PERFORM conferir_carga.

      IF ck_conferiu EQ abap_true.
        ck_confer_carga = abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'ADD_NOTA'.

      CLEAR: ok_code.
      CLEAR: zde_zsdt0001nt_alv.
      CALL SCREEN 0307 STARTING AT 40 05.

    WHEN 'SHOWLOGS'.
      CLEAR: ok_code.
      IF ck_mostrar_logs EQ abap_true.
        ck_mostrar_logs = abap_false.
      ELSE.
        ck_mostrar_logs = abap_true.
      ENDIF.
      PERFORM limpa_tela_0300.

    WHEN 'NOVA_SOLIC'.
      "Solicitar Manutenação
      CLEAR: ok_code.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = TEXT-105
          text_question         = TEXT-106
          text_button_1         = TEXT-007
          icon_button_1         = 'ICON_CHECKED'
          text_button_2         = TEXT-004
          icon_button_2         = 'ICON_INCOMPLETE'
          default_button        = '2'
          display_cancel_button = ' '
        IMPORTING
          answer                = answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      CHECK answer EQ '1'.

      SUBMIT zmmr126 WITH pck_cad  EQ abap_true
                     WITH pmanut   EQ abap_true
                     WITH psafra   EQ psafra
                     WITH pempre   EQ pempre
                     WITH pfilia   EQ pfilia
                     WITH pidcarga EQ objeto->carga-id_carga AND RETURN.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0302  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0302 OUTPUT.

* RJF - Ini - 2023.03
* CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT
  IF zde_zsdt0001ov_alv-nr_ordem_venda IS NOT INITIAL.
* Documento de vendas: dados comerciais - VBKD-INCO1
    SELECT vbeln, inco1
      FROM vbkd
      INTO TABLE @DATA(it_vbkd)
      WHERE vbeln EQ @zde_zsdt0001ov_alv-nr_ordem_venda.

    IF sy-subrc IS INITIAL.
      LOOP AT it_vbkd ASSIGNING FIELD-SYMBOL(<fs_vbkd>).
        IF <fs_vbkd>-inco1 EQ 'CPT'. " Modalidade CPT
          vg_ord_ext = abap_on.
          vg_text = 'Ord.Carreg.Exter'(042).
          EXIT.
        ELSE.
          FREE vg_ord_ext.
          vg_text = 'Ord.Carregamento'(043).
        ENDIF.
      ENDLOOP.
    ENDIF.
  ELSE.
    vg_text = 'Ord.Carregamento'(043).
  ENDIF.
* RJF - Fim - 2023.03

  LOOP AT SCREEN.
    SPLIT screen-name AT '-' INTO: DATA(str1a) DATA(str2a).
    IF str1a EQ 'ZDE_ZSDT0001CG_ALV' OR str1a EQ 'ZDE_ZSDT0001OD_ALV' OR str1a EQ 'ZDE_ZSDT0001OV_ALV'.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( EXPORTING i_campo = CONV #( str2a ) IMPORTING e_permitido = DATA(e_permitido_0302) ).
      IF e_permitido_0302 EQ abap_false.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

*  IF ZDE_ZSDT0001CG_ALV-ID_PRODUTO IS INITIAL.
*    VG_TL_0310 = '9012'.
*  ELSE.
*    SELECT SINGLE * INTO @WA_MARA
*      FROM MARA
*     WHERE MATNR EQ @ZDE_ZSDT0001CG_ALV-ID_PRODUTO.
*
*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*      EXPORTING
*        INPUT  = WA_MARA-MATKL
*      IMPORTING
*        OUTPUT = WA_MARA-MATKL.
*
*    IF WA_MARA-MATKL EQ ZIF_CARGA=>ST_GRUPO_ALGODAO_PLUMA. "Algodão
*      VG_TL_0310 = '0311'.
*    ELSE.
  vg_tl_0310 = '0310'.
*    ENDIF.
*  ENDIF.

  IF vg_tl_0302 IS INITIAL.
    vg_tl_0302 = '0309'.
  ENDIF.

  DESCRIBE TABLE it_notas LINES DATA(qtd_notas).
  IF qtd_notas EQ 1.
    IF zde_zsdt0001nt_alv IS INITIAL.
      READ TABLE it_notas INDEX 1.
      zde_zsdt0001nt_alv = it_notas.
    ENDIF.
    vg_tl_0305 = '0305'.
  ELSEIF qtd_notas EQ 0.
    vg_tl_0305 = '0305'.
  ELSE.
    vg_tl_0305 = '0306'.
  ENDIF.

  IF nm_field_set_carga IS NOT INITIAL.
    SET CURSOR FIELD nm_field_set_carga OFFSET pos.
    CLEAR: nm_field_set_carga.
  ENDIF.

  DESCRIBE TABLE it_ordens_venda LINES qtd_notas.
  IF qtd_notas EQ 1.
    READ TABLE it_ordens_venda INDEX 1.
    zde_zsdt0001ov_alv = it_ordens_venda.
  ENDIF.

  DESCRIBE TABLE it_pedido_compra LINES qtd_notas.
  IF qtd_notas EQ 1.
    READ TABLE it_pedido_compra INDEX 1.
    MOVE-CORRESPONDING it_pedido_compra TO zde_zsdt0001ov_alv.
    zde_zsdt0001ov_alv-nr_ordem_venda = it_pedido_compra-nr_pedido_compra.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALIDAR_NR_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE validar_nr_ordem INPUT.

  TRY .
      IF vg_ord_ext IS INITIAL. "RJF - CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT
        objeto->set_ordem_carregamento(
        EXPORTING
          i_nr_safra  = zde_zsdt0001cg_alv-nr_safra
          i_id_bukrs  = zde_zsdt0001cg_alv-id_bukrs
          i_id_branch = zde_zsdt0001cg_alv-id_branch
          i_nr_ordem  = zde_zsdt0001cg_alv-nr_ordem
        IMPORTING
          e_ordem_carrgamento = zde_zsdt0001od_alv
        CHANGING
          i_carga_alv = zde_zsdt0001cg_alv ).
* RJF - Ini - CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT
      ELSE.
        objeto->set_ordem_carregamento_ext(
        EXPORTING
          i_nr_safra  = zde_zsdt0001cg_alv-nr_safra
          i_id_bukrs  = zde_zsdt0001cg_alv-id_bukrs
          i_id_branch = zde_zsdt0001cg_alv-id_branch
          i_nr_ordem  = zde_zsdt0001cg_alv-nr_ordem
        CHANGING
          i_carga_alv = zde_zsdt0001cg_alv ).
      ENDIF.
* RJF - Fim - CS2021000978 Validação da ordem de carregamento de terceiros para embarques na modalidade CPT

    CATCH zcx_ordem_carregamento INTO DATA(ex_ordem_carregamento).
      ex_ordem_carregamento->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    CATCH zcx_parceiros INTO ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_CARGA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribui_info_carga INPUT.
  ck_alterado_carga = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0302  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0302 INPUT.

  IF ck_alterado_carga EQ abap_true.
    TRY.
        objeto->set_carga( EXPORTING i_carga = zde_zsdt0001cg_alv IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv ).
        CLEAR: ck_alterado_carga.
      CATCH zcx_carga INTO ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
        PERFORM seta_campo USING ex_carga->msgid ex_carga->msgno.
    ENDTRY.
  ENDIF.

  CASE ok_code.
    WHEN 'GMO01'.
      CLEAR: ok_code.
      CALL SCREEN 0303 STARTING AT 20 15.
    WHEN 'GMO02'.
      CLEAR: ok_code.
      CALL SCREEN 0304 STARTING AT 20 15.
    WHEN 'GMO'.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0303 INPUT.

  IF ck_alterado_carga EQ abap_true.
    TRY.
        objeto->set_carga( EXPORTING i_carga = zde_zsdt0001cg_alv IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv ).
      CATCH zcx_carga INTO ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDIF.

  CASE ok_code.
    WHEN 'CONFIRMAR'.
      CLEAR: ok_code.
      CHECK ck_alterado_carga EQ abap_false.
      LEAVE TO SCREEN 0.
    WHEN 'GMO'.
      CLEAR: ok_code.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0303  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0303 OUTPUT.
  SET PF-STATUS 'TL0303'.
  SET TITLEBAR 'TL0303'.

  LOOP AT SCREEN.
    SPLIT screen-name AT '-' INTO: str1a str2a.
    IF str1a EQ 'ZDE_ZSDT0001CG_ALV' OR str1a EQ 'ZDE_ZSDT0001OD_ALV'.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( EXPORTING i_campo = CONV #( str2a ) IMPORTING e_permitido = DATA(e_permitido_0303) ).
      IF e_permitido_0303 EQ abap_false.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: ck_alterado_carga.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0303_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0303_exit INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0304  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0304 OUTPUT.
  SET PF-STATUS 'TL0303'.
  SET TITLEBAR 'TL0304'.

  LOOP AT SCREEN.
    SPLIT screen-name AT '-' INTO: str1a str2a.
    IF str1a EQ 'ZDE_ZSDT0001CG_ALV' OR str1a EQ 'ZDE_ZSDT0001OD_ALV'.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( EXPORTING i_campo = CONV #( str2a ) IMPORTING e_permitido = DATA(e_permitido_0304) ).
      IF e_permitido_0304 EQ abap_false.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CLEAR: ck_alterado_carga.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0304  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0304 INPUT.

  IF ck_alterado_carga EQ abap_true.
    TRY.
        objeto->set_carga( EXPORTING i_carga = zde_zsdt0001cg_alv IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv ).
      CATCH zcx_carga INTO ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDIF.

  CASE ok_code.
    WHEN 'CONFIRMAR'.
      CLEAR: ok_code.
      CHECK ck_alterado_carga EQ abap_false.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_01  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE perc_01 INPUT.

  TRY.
      objeto->set_carga( EXPORTING i_carga = zde_zsdt0001cg_alv IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv ).
      CLEAR: ck_alterado_carga.
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

  PERFORM perc_resultado USING '1' zde_zsdt0001cg_alv-nr_perc_umi
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                      CHANGING zde_zsdt0001cg_alv-nr_qtde_umi.
  objeto->ck_digitado_umidade = abap_true.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_02  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE perc_02 INPUT.

  PERFORM perc_resultado USING '2' zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                      CHANGING zde_zsdt0001cg_alv-nr_qtde_imp.

  objeto->ck_digitado_impureza = abap_true.

  PERFORM perc_resultado USING '1' zde_zsdt0001cg_alv-nr_perc_umi
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                      CHANGING zde_zsdt0001cg_alv-nr_qtde_umi.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_03  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE perc_03 INPUT.

  DATA: lc_ck_avariado TYPE char01.

  IF zde_zsdt0001cg_alv-nr_perc_ava IS INITIAL.
    CLEAR: zde_zsdt0001cg_alv-nr_perc_ava_arq,
           zde_zsdt0001cg_alv-nr_perc_ava_que,
           zde_zsdt0001cg_alv-nr_perc_ava_mof,
           zde_zsdt0001cg_alv-nr_perc_ava_pic,
           zde_zsdt0001cg_alv-nr_perc_ava_fer,
           zde_zsdt0001cg_alv-nr_perc_ava_ger,
           zde_zsdt0001cg_alv-nr_perc_ava_ard,
           zde_zsdt0001cg_alv-nr_perc_ava_ges.
    lc_ck_avariado = abap_true.
  ELSE.
    PERFORM atribui_perc_sub_avariado USING zde_zsdt0001cg_alv-nr_perc_ava CHANGING lc_ck_avariado.
  ENDIF.

  IF lc_ck_avariado EQ abap_true.
    PERFORM perc_resultado USING '3' zde_zsdt0001cg_alv-nr_perc_ava
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            CHANGING zde_zsdt0001cg_alv-nr_qtde_ava.
  ELSE.
    MESSAGE e236.
  ENDIF.
  objeto->ck_digitado_avariado = abap_true.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_04  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE perc_04 INPUT.
  PERFORM perc_resultado USING '4' zde_zsdt0001cg_alv-nr_perc_ard
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                          CHANGING zde_zsdt0001cg_alv-nr_qtde_ard.
  objeto->ck_digitado_ardido = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_05  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE perc_05 INPUT.
  PERFORM perc_resultado USING '5' zde_zsdt0001cg_alv-nr_perc_que
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                          CHANGING zde_zsdt0001cg_alv-nr_qtde_que.
  objeto->ck_digitado_quebrado = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_06  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE perc_06 INPUT.
  PERFORM perc_resultado USING '6' zde_zsdt0001cg_alv-nr_perc_esv
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                          CHANGING zde_zsdt0001cg_alv-nr_qtde_esv.
  objeto->ck_digitado_esverdeado = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PERC_07  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE perc_07 INPUT.
  PERFORM perc_resultado USING '7' zde_zsdt0001cg_alv-nr_perc_car
                                   zde_zsdt0001cg_alv-nr_perc_imp
                                   zde_zsdt0001cg_alv-nm_peso_subtotal
                          CHANGING zde_zsdt0001cg_alv-nr_qtde_car.
  objeto->ck_digitado_carunchado = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  PERC_RESULTADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0501   text
*----------------------------------------------------------------------*
FORM perc_resultado  USING p_tipo       TYPE char01
                           p_percentual TYPE zde_nr_perc_class_com
                           p_perc_imp   TYPE zde_nr_perc_impureza
                           p_subtotal   TYPE zde_nm_peso_subtotal
                  CHANGING p_retorno TYPE zde_nr_qtde_umidade.

  DATA: obj_recebimento TYPE REF TO zif_carga.
  DATA: i_classificacao	TYPE zde_pes_resultado_class.
  i_classificacao-centro = zde_zsdt0001cg_alv-id_branch.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = zde_zsdt0001cg_alv-id_produto
    IMPORTING
      output = i_classificacao-material.

  i_classificacao-safra              = zde_zsdt0001cg_alv-nr_safra.
  i_classificacao-caracteristica     = p_tipo.

  p_retorno = 0.
  CHECK p_subtotal IS NOT INITIAL.
  CHECK zde_zsdt0001cg_alv-id_produto IS NOT INITIAL.

  WRITE p_percentual TO i_classificacao-percentual.
  WRITE p_perc_imp   TO i_classificacao-percentualimpureza.
  WRITE p_subtotal   TO i_classificacao-subtotal.
  CONDENSE i_classificacao-percentual         NO-GAPS.
  CONDENSE i_classificacao-percentualimpureza NO-GAPS.
  CONDENSE i_classificacao-subtotal           NO-GAPS.

  CASE ptipca.
    WHEN zif_carga=>st_tp_carga_entrada_fob.
      CREATE OBJECT obj_recebimento TYPE zcl_carga_recebimento.
    WHEN zif_carga=>st_tp_carga_saida_opus.
      CREATE OBJECT obj_recebimento TYPE zcl_carga_saida_opus.
    WHEN zif_carga=>st_tp_carga_saida_ent_fob.
      CREATE OBJECT obj_recebimento TYPE zcl_carga_saida.
  ENDCASE.

  TRY .
      obj_recebimento->get_result_desc_classificacao( EXPORTING i_classificacao = i_classificacao IMPORTING e_resultado = DATA(r_resultado) ).
      p_retorno = r_resultado-desconto-valordesconto.
      obj_recebimento->free( ).
    CATCH zcx_carga INTO ex_carga.
      TRY .
          obj_recebimento->free( ).
        CATCH zcx_carga.
      ENDTRY.
      CLEAR: obj_recebimento.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ALTERA_BRUTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE altera_bruto INPUT.
  PERFORM ajusta_descontos.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  AJUSTA_DESCONTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ajusta_descontos .

  IF NOT ( zde_zsdt0001cg_alv-nr_perc_umi NE 0 OR zde_zsdt0001cg_alv-nr_perc_imp NE 0 OR zde_zsdt0001cg_alv-nr_perc_ava NE 0
  OR zde_zsdt0001cg_alv-nr_perc_ard NE 0 OR zde_zsdt0001cg_alv-nr_perc_que NE 0 OR zde_zsdt0001cg_alv-nr_perc_esv NE 0 ).
    EXIT.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_umi NE 0.
    PERFORM perc_resultado USING '1' zde_zsdt0001cg_alv-nr_perc_umi
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                        CHANGING zde_zsdt0001cg_alv-nr_qtde_umi.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_imp NE 0.
    PERFORM perc_resultado USING '2' zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            CHANGING zde_zsdt0001cg_alv-nr_qtde_imp.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ava NE 0.
    PERFORM perc_resultado USING '3' zde_zsdt0001cg_alv-nr_perc_ava
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            CHANGING zde_zsdt0001cg_alv-nr_qtde_ava.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_ard NE 0.
    PERFORM perc_resultado USING '4' zde_zsdt0001cg_alv-nr_perc_ard
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            CHANGING zde_zsdt0001cg_alv-nr_qtde_ard.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_que NE 0.
    PERFORM perc_resultado USING '5' zde_zsdt0001cg_alv-nr_perc_que
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            CHANGING zde_zsdt0001cg_alv-nr_qtde_que.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_esv NE 0.
    PERFORM perc_resultado USING '6' zde_zsdt0001cg_alv-nr_perc_esv
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            CHANGING zde_zsdt0001cg_alv-nr_qtde_esv.
  ENDIF.

  IF zde_zsdt0001cg_alv-nr_perc_car NE 0.
    PERFORM perc_resultado USING '7' zde_zsdt0001cg_alv-nr_perc_car
                                     zde_zsdt0001cg_alv-nr_perc_imp
                                     zde_zsdt0001cg_alv-nm_peso_subtotal
                            CHANGING zde_zsdt0001cg_alv-nr_qtde_car.
  ENDIF.

  TRY.
      objeto->set_carga( EXPORTING i_carga = zde_zsdt0001cg_alv IMPORTING e_carga_recebimento = zde_zsdt0001cg_alv ).
      CLEAR: ck_alterado_carga.
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  ALTERA_TARA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE altera_tara INPUT.
  PERFORM ajusta_descontos.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_ORDEM  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_ordem INPUT.

  TRY .
      DATA(lc_id_produto) = zde_zsdt0001cg_alv-id_produto.

      objeto->set_ordem_venda( EXPORTING i_ordem_venda = zde_zsdt0001ov_alv-nr_ordem_venda
        IMPORTING
          e_carga = DATA(lc_saida_carga)
        CHANGING
          c_zde_zsdt0001ov_alv = zde_zsdt0001ov_alv
          ).

      zde_zsdt0001cg_alv-id_produto        = lc_saida_carga-id_produto.
      zde_zsdt0001cg_alv-ds_produto        = lc_saida_carga-ds_produto.
      zde_zsdt0001cg_alv-id_local_coleta   = lc_saida_carga-id_local_coleta.
      zde_zsdt0001cg_alv-id_local_descarga = lc_saida_carga-id_local_descarga.
      zde_zsdt0001cg_alv-id_local_destino  = lc_saida_carga-id_local_destino.
      zde_zsdt0001cg_alv-id_agent_frete    = lc_saida_carga-id_agent_frete.
      zde_zsdt0001cg_alv-ds_local_coleta   = lc_saida_carga-ds_local_coleta.
      zde_zsdt0001cg_alv-ds_local_descarga = lc_saida_carga-ds_local_descarga.
      zde_zsdt0001cg_alv-ds_local_destino  = lc_saida_carga-ds_local_destino.
      zde_zsdt0001cg_alv-ds_agent_frete    = lc_saida_carga-ds_agent_frete.
      zde_zsdt0001cg_alv-in_transferencia  = lc_saida_carga-in_transferencia.
      zde_zsdt0001cg_alv-ck_gera_aviso     = lc_saida_carga-ck_gera_aviso.
      zde_zsdt0001cg_alv-ck_frete_entrada  = lc_saida_carga-ck_frete_entrada.

      IF lc_id_produto NE zde_zsdt0001cg_alv-id_produto.
        PERFORM ajusta_descontos.
      ENDIF.

    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
    CATCH zcx_parceiros INTO ex_parceiros.
      ex_parceiros->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0305  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0305 OUTPUT.
  "BREAK-POINT.

  IF zde_zsdt0001nt_alv-id_mod_fiscal IS INITIAL.
    zde_zsdt0001nt_alv-id_mod_fiscal = zif_carga=>st_model_fiscal_papel.
  ENDIF.

  LOOP AT SCREEN.
    SPLIT screen-name AT '-' INTO: str1a str2a.
    IF str1a EQ 'ZDE_ZSDT0001NT_ALV'.

      objeto->valida_atributo_alteravel(
          EXPORTING
            i_campo         = CONV #( str2a )
            i_modelo_fiscal = zde_zsdt0001nt_alv-id_mod_fiscal
            i_id_entrada    = zde_zsdt0001nt_alv-id_entrada
            i_id_empresa    = zde_zsdt0001cg_alv-id_bukrs
          IMPORTING
            e_permitido = DATA(e_permitido_0305) ).

      IF e_permitido_0305 EQ abap_false.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF nm_field_set_nota IS NOT INITIAL.
    SET CURSOR FIELD nm_field_set_nota OFFSET pos.
    CLEAR nm_field_set_nota.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_NOTA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribui_info_nota INPUT.
  CLEAR: it_takes_saldo[].
  ck_alterado_nota = abap_true.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0305  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0305 INPUT.

  DATA: wa_zsdt0001pd TYPE zsdt0001pd.

  CHECK vg_tl_0305 EQ '0305'.

  IF ck_alterado_nota EQ abap_true.
    TRY .
        objeto->add_nota_fiscal( EXPORTING i_nota = zde_zsdt0001nt_alv IMPORTING e_nota = zde_zsdt0001nt_alv ).
        READ TABLE it_notas WITH KEY id_nota = zde_zsdt0001nt_alv-id_nota ASSIGNING FIELD-SYMBOL(<fs_nota>).
        IF sy-subrc IS INITIAL.
          <fs_nota> = zde_zsdt0001nt_alv.
        ELSE.
          APPEND zde_zsdt0001nt_alv TO it_notas.
        ENDIF.
        ck_alterado_nota = abap_false.

      CATCH zcx_parceiros INTO ex_parceiros.  "
        ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        PERFORM seta_campo USING ex_parceiros->msgid ex_parceiros->msgno.
      CATCH zcx_carga INTO ex_carga.  "

        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
        PERFORM seta_campo USING ex_carga->msgid ex_carga->msgno.

        IF ex_carga->msgid EQ zcx_carga=>zcx_forn_sem_parametro-msgid AND
           ex_carga->msgno EQ zcx_carga=>zcx_forn_sem_parametro-msgno AND
           zde_zsdt0001nt_alv-nr_fornecedor_ie IS NOT INITIAL.
          ex_carga->published_erro( EXPORTING i_msgty = 'W' i_msgty_display = 'W' ).
          wa_zsdt0001pd-id_branch    = objeto->carga-id_branch.
          wa_zsdt0001pd-id_bukrs     = objeto->carga-id_bukrs.
          wa_zsdt0001pd-nr_safra     = objeto->carga-nr_safra.

          TRY .
              zcl_fornecedores=>zif_parceiros~get_instance(
                )->set_parceiro_ie( i_insc_estatual = CONV #( zde_zsdt0001nt_alv-nr_fornecedor_ie )
                )->get_id_parceiro( IMPORTING e_parceiro = wa_zsdt0001pd-id_produtor
                )->ck_ativo(
                )->ck_ativo_empresa( i_empresa = objeto->carga-id_bukrs
                ).
              INSERT INTO zsdt0001pd VALUES wa_zsdt0001pd.
              COMMIT WORK.

            CATCH zcx_parceiros INTO ex_parceiros.
              ex_parceiros->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
          ENDTRY.

        ELSE.
          ex_carga->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
        ENDIF.
    ENDTRY.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  INFORMAR_CHAVE_NFE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM informar_chave_nfe .

  CHECK zde_zsdt0001cg_alv-tp_status EQ zif_carga=>st_status_aberto.

  IF ( zde_zsdt0001nt_alv-id_mod_fiscal NE '55' AND zde_zsdt0001nt_alv-id_mod_fiscal IS NOT INITIAL ).
    MESSAGE s033 DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  CLEAR: wa_add_nfe_9002.

  CALL SCREEN 9002 STARTING AT 40 10.

  IF wa_add_nfe_9002-ck_incluir EQ abap_true.
    "Entrar com a chave NF-e
    zde_zsdt0001nt_alv-id_mod_fiscal      = '55'.
    zde_zsdt0001nt_alv-dt_emissao         = wa_add_nfe_9002-dt_emissao.
    zde_zsdt0001nt_alv-ds_fornecedor      = wa_add_nfe_9002-name1.
    zde_zsdt0001nt_alv-id_fornecedor      = wa_add_nfe_9002-parid.
    zde_zsdt0001nt_alv-nr_fornecedor_ie   = wa_add_nfe_9002-parid_ie.
    zde_zsdt0001nt_alv-dt_vencimento_form = wa_add_nfe_9002-dt_emissao.
    zde_zsdt0001nt_alv-nr_nota            = wa_add_nfe_9002-numero.
    zde_zsdt0001nt_alv-nm_serie           = wa_add_nfe_9002-serie.
    zde_zsdt0001nt_alv-nr_valor           = wa_add_nfe_9002-nftot.
    zde_zsdt0001nt_alv-nr_quantidade      = wa_add_nfe_9002-ntgew.
    zde_zsdt0001nt_alv-nr_chave_nfe       = wa_add_nfe_9002-n55_chave_acesso.
    zde_zsdt0001nt_alv-cfop               = wa_add_nfe_9002-cfop.

    TRY .
        objeto->add_nota_fiscal( EXPORTING i_nota = zde_zsdt0001nt_alv IMPORTING e_nota = zde_zsdt0001nt_alv ).
        READ TABLE it_notas WITH KEY id_nota = zde_zsdt0001nt_alv-id_nota ASSIGNING FIELD-SYMBOL(<fs_nota>).
        IF sy-subrc IS INITIAL.
          <fs_nota> = zde_zsdt0001nt_alv.
        ELSE.
          APPEND zde_zsdt0001nt_alv TO it_notas.
        ENDIF.
        ck_alterado_nota = abap_false.
      CATCH zcx_parceiros INTO ex_parceiros.    "
        ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
      CATCH zcx_carga INTO ex_carga.    "
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'W' ).
    ENDTRY.

  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  VALIDA_SUBTOTAL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE valida_subtotal INPUT.
  "
  objeto->get_calcular_subtotal( EXPORTING i_peso_bruto = zde_zsdt0001cg_alv-nm_peso_bruto i_peso_tara = zde_zsdt0001cg_alv-nm_peso_tara
    IMPORTING
      e_peso_subtotal = DATA(e_peso_subtotal) ).

  IF zde_zsdt0001cg_alv-nm_peso_subtotal NE e_peso_subtotal.
    MESSAGE e063.
  ENDIF.

  PERFORM ajusta_descontos.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_MODEL  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_model INPUT.

  IF zde_zsdt0001nt_alv-id_mod_fiscal NE '55' AND zde_zsdt0001nt_alv-id_mod_fiscal NE '1'.
    MESSAGE e055.
  ENDIF.

  IF zde_zsdt0001nt_alv-id_mod_fiscal EQ '1'.
    CLEAR: zde_zsdt0001nt_alv-nr_chave_nfe.
  ENDIF.

ENDMODULE.

**&---------------------------------------------------------------------*
**&      Module  PSQ_PLACA  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PSQ_PLACA INPUT.
*
*  CHECK ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR IS NOT INITIAL.
*
*  TRY.
*      OBJETO->AT_CARGA_RECEBIMENTO->ZIF_CARGA~GET_INFO_PLACA( EXPORTING I_PLACA = ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR I_VALIDAR = ABAP_TRUE I_TRACAO = ABAP_TRUE IMPORTING E_ZLEST0002 = DATA(R_ZLEST0002) ).
*      ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO = R_ZLEST0002-PROPRIETARIO.
*    CATCH ZCX_CARGA INTO EX_CARGA.
*      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**&      Module  PSQ_REPO1  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PSQ_REPO1 INPUT.
*
*  CHECK ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1 IS NOT INITIAL.
*
*  TRY.
*      OBJETO->AT_CARGA_RECEBIMENTO->ZIF_CARGA~GET_INFO_PLACA( EXPORTING I_PLACA = ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1 I_VALIDAR = ABAP_TRUE IMPORTING E_ZLEST0002 = R_ZLEST0002 ).
*      IF R_ZLEST0002-PROPRIETARIO NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO.
*        MESSAGE W071 WITH ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1.
*      ENDIF.
*    CATCH ZCX_CARGA INTO EX_CARGA.
*      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**&      Module  PSQ_REPO2  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PSQ_REPO2 INPUT.
*
*  CHECK ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_2 IS NOT INITIAL.
*
*  TRY.
*      OBJETO->AT_CARGA_RECEBIMENTO->ZIF_CARGA~GET_INFO_PLACA( EXPORTING I_PLACA = ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_2 I_VALIDAR = ABAP_TRUE IMPORTING E_ZLEST0002 = R_ZLEST0002 ).
*      IF R_ZLEST0002-PROPRIETARIO NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO.
*        MESSAGE W071 WITH ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1.
*      ENDIF.
*    CATCH ZCX_CARGA INTO EX_CARGA.
*      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*ENDMODULE.
*
**&---------------------------------------------------------------------*
**&      Module  PSQ_REPO3  INPUT
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
*MODULE PSQ_REPO3 INPUT.
*
*  CHECK ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_3 IS NOT INITIAL.
*
*  TRY.
*      OBJETO->AT_CARGA_RECEBIMENTO->ZIF_CARGA~GET_INFO_PLACA( EXPORTING I_PLACA = ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_3 I_VALIDAR = ABAP_TRUE IMPORTING E_ZLEST0002 = R_ZLEST0002 ).
*      IF R_ZLEST0002-PROPRIETARIO NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO.
*        MESSAGE W071 WITH ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1.
*      ENDIF.
*    CATCH ZCX_CARGA INTO EX_CARGA.
*      EX_CARGA->PUBLISHED_ERRO( I_MSGTY = 'E' I_MSGTY_DISPLAY = 'E' ).
*  ENDTRY.
*
*ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0306  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0306 OUTPUT.

  IF ctl_alv_0306 IS INITIAL.

    CREATE OBJECT ctl_cccontainer_0306
      EXPORTING
        container_name = 'ALV_0306'.

    CREATE OBJECT ctl_alv_0306
      EXPORTING
        i_parent = ctl_cccontainer_0306.

    PERFORM fill_it_fieldcatalog_0306.

    PERFORM fill_gs_variant_0306.

    gs_layout_0306-sel_mode   = 'A'.
    gs_layout_0306-zebra      = abap_false.
    gs_layout_0306-cwidth_opt = abap_true.

    IF zde_zsdt0001cg_alv-tp_status NE zif_carga=>st_status_aberto.
      gs_layout_0306-no_toolbar = abap_true.
    ELSE.
      gs_layout_0306-no_toolbar = abap_false.
    ENDIF.

    CREATE OBJECT obg_toolbar_0306
      EXPORTING
        io_alv_grid = ctl_alv_0306.

    SET HANDLER obg_toolbar_0306->on_toolbar FOR ctl_alv_0306.
    SET HANDLER obg_toolbar_0306->handle_user_command FOR ctl_alv_0306.

    CALL METHOD ctl_alv_0306->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_0306
        is_variant      = gs_variant_0306
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_0306
        it_outtab       = it_notas[].

    CREATE OBJECT event_handler_0306.
    SET HANDLER event_handler_0306->handle_double_click  FOR ctl_alv_0306.

  ENDIF.

  wa_stable_0306-row = abap_true.
  wa_stable_0306-col = abap_true.

  CALL METHOD ctl_alv_0306->refresh_table_display
    EXPORTING
      is_stable      = wa_stable_0306
      i_soft_refresh = abap_true.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  LIMPA_TELA_0300
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM limpa_tela_0300 .

  IF tree IS NOT INITIAL.
    tree->free( ).
  ENDIF.
  CLEAR: tree.

  CLEAR: obg_toolbar_0306,
         obj_toolbarmanager_0306,
         obg_toolbar_0313a,
         obj_toolbarmanager_0313a,
         obg_toolbar_0312a,
         obj_toolbarmanager_0312a,
         obg_toolbar_0312b,
         obj_toolbarmanager_0312b,
         event_handler_0312a.

  "Alv de Take UP's Vinculados
  IF ctl_alv_0313a IS NOT INITIAL.
    ctl_alv_0313a->free( ).
  ENDIF.
  CLEAR: ctl_alv_0313a.

  IF ctl_cccontainer_0313a IS NOT INITIAL.
    ctl_cccontainer_0313a->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_0313a.

  "Alv de Take UP's Livres
  IF ctl_alv_0312a IS NOT INITIAL.
    ctl_alv_0312a->free( ).
  ENDIF.
  CLEAR: ctl_alv_0312a.

  IF ctl_cccontainer_0312a IS NOT INITIAL.
    ctl_cccontainer_0312a->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_0312a.

  "Alv de Take UP's Vinculados
  IF ctl_alv_0312b IS NOT INITIAL.
    ctl_alv_0312b->free( ).
  ENDIF.
  CLEAR: ctl_alv_0312b.

  IF ctl_cccontainer_0312b IS NOT INITIAL.
    ctl_cccontainer_0312b->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_0312b.

  IF ctl_alv_0306 IS NOT INITIAL.
    ctl_alv_0306->free( ).
  ENDIF.
  CLEAR: ctl_alv_0306.

  IF ctl_cccontainer_0306 IS NOT INITIAL.
    ctl_cccontainer_0306->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_0306.

  IF picture_9012 IS NOT INITIAL.
    picture_9012->free( ).
  ENDIF.
  CLEAR: picture_9012.

  IF ctl_cccontainer_9012 IS NOT INITIAL.
    ctl_cccontainer_9012->free( ).
  ENDIF.
  CLEAR: ctl_cccontainer_9012.

  IF tree_0300 IS NOT INITIAL.
    tree_0300->free( ).
  ENDIF.
  CLEAR: tree_0300.

  CLEAR: g_application_0300.

  IF docking_0300_se IS NOT INITIAL.
    cl_abap_browser=>close_browser( ).
    docking_0300_se->free( ).
  ENDIF.
  CLEAR: docking_0300_se.

  IF docking_0300 IS NOT INITIAL.
    docking_0300->free( ).
  ENDIF.
  CLEAR: docking_0300.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_it_fieldcatalog_0306 .

  DATA: lc_col_pos  TYPE lvc_colpos.

  FIELD-SYMBOLS: <fs_cat> TYPE lvc_s_fcat.

  CLEAR: it_fieldcatalog_0306[].

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZDE_ZSDT0001NT_ALV'
    CHANGING
      ct_fieldcat      = it_fieldcatalog_0306.

  LOOP AT it_fieldcatalog_0306 ASSIGNING <fs_cat>.

    <fs_cat>-tabname = 'ZDE_ZSDT0001NT_ALV'.

    CASE <fs_cat>-fieldname.
      WHEN 'ID_CARGA'.
        <fs_cat>-no_out = abap_true.
      WHEN 'DT_EMISSAO' OR 'DT_VENCIMENTO_FORM'.
        <fs_cat>-just    = 'C'.
      WHEN 'NR_QUANTIDADE' OR 'NR_VALOR'    OR
           'NR_QTDE_UMI'   OR 'NR_QTDE_IMP' OR
           'NR_QTDE_AVA'   OR 'NR_QTDE_ARD' OR
           'NR_QTDE_QUE'   OR 'NR_QTDE_ESV'.
        <fs_cat>-do_sum = abap_true.
    ENDCASE.

    IF <fs_cat>-fieldname <> 'ST_MENSAGEM'.
      <fs_cat>-col_pos = lc_col_pos.
      ADD 1 TO lc_col_pos.
    ENDIF.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_GS_VARIANT_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fill_gs_variant_0306.

  gs_variant_0306-report      = sy-repid.
  gs_variant_0306-handle      = '0306'.
  gs_variant_0306-log_group   = abap_false.
  gs_variant_0306-username    = abap_false.
  gs_variant_0306-variant     = abap_false.
  gs_variant_0306-text        = abap_false.
  gs_variant_0306-dependvars  = abap_false.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0307  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0307 OUTPUT.
  SET PF-STATUS 'PF0307'.
  SET TITLEBAR 'TL0307'.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0307_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0307_exit INPUT.

  IF ok_code EQ 'CHNFE'.
    PERFORM informar_chave_nfe.
  ELSE.
    CLEAR zde_zsdt0001nt_alv.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0307  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0307 INPUT.

  CASE ok_code.
    WHEN 'ENTER'.

      IF ck_alterado_nota EQ abap_true.

        TRY .
            objeto->add_nota_fiscal( EXPORTING i_nota = zde_zsdt0001nt_alv IMPORTING e_nota = zde_zsdt0001nt_alv ).
            READ TABLE it_notas WITH KEY id_nota = zde_zsdt0001nt_alv-id_nota ASSIGNING <fs_nota>.
            IF sy-subrc IS INITIAL.
              <fs_nota> = zde_zsdt0001nt_alv.
            ELSE.
              APPEND zde_zsdt0001nt_alv TO it_notas.
            ENDIF.
            ck_alterado_nota = abap_false.
          CATCH zcx_parceiros INTO ex_parceiros.  "
            ex_parceiros->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            PERFORM seta_campo USING ex_parceiros->msgid ex_parceiros->msgno.
            EXIT.
          CATCH zcx_carga INTO ex_carga.
            ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
            PERFORM seta_campo USING ex_carga->msgid ex_carga->msgno.
            EXIT.
        ENDTRY.

      ENDIF.

      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SCROLL_INFO_0306  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_scroll_info_0306 INPUT.

  IF ctl_alv_0306 IS NOT INITIAL.
    CALL METHOD ctl_alv_0306->get_scroll_info_via_id
      IMPORTING
        es_col_info = gs_scroll_col_0306
        es_row_no   = gs_scroll_row_0306.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_SELECTED_ROWS_0306  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_selected_rows_0306 INPUT.

  IF ctl_alv_0306 IS NOT INITIAL.
    CLEAR it_selected_rows.
    CALL METHOD ctl_alv_0306->get_selected_rows
      IMPORTING
        et_index_rows = it_selected_rows.

    CLEAR: it_notas_sel[], it_notas_sel.

    LOOP AT it_selected_rows INTO wa_selected_rows.
      READ TABLE it_notas INTO DATA(wa_nota) INDEX wa_selected_rows-index.
      APPEND wa_nota TO it_notas_sel.
    ENDLOOP.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_FORNE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_forne INPUT.

  CHECK zde_zsdt0001nt_alv-nr_fornecedor_ie IS NOT INITIAL.

  TRY .
      objeto->get_nota_fornecedor_ie( EXPORTING i_stcd3 = CONV #( zde_zsdt0001nt_alv-nr_fornecedor_ie ) IMPORTING e_nota = DATA(lc_nota_ie) ).

      "Verificar Restrição de Embargos """""""""""""""""""""""""""""""""""""
      zcl_fornecedores=>zif_parceiros~get_instance(
        )->set_parceiro( i_parceiro = lc_nota_ie-id_fornecedor
        )->ck_restricao_embargo( EXPORTING i_gera_erro = abap_false IMPORTING e_resultado   = DATA(e_resultado_restri)
        ).

      IF e_resultado_restri-bloqueado EQ abap_true.
        PERFORM apresenta_restricao USING e_resultado_restri.
        MESSAGE e_resultado_restri-motivo TYPE 'E'.
      ENDIF.
      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

      zde_zsdt0001nt_alv-id_fornecedor = lc_nota_ie-id_fornecedor.
      zde_zsdt0001nt_alv-ds_fornecedor = lc_nota_ie-ds_fornecedor.


    CATCH zcx_carga INTO ex_carga.

      IF ex_carga->msgid EQ zcx_carga=>zcx_forn_sem_parametro-msgid.

        ex_carga->published_erro( EXPORTING i_msgty = 'W' i_msgty_display = 'W' ).
        wa_zsdt0001pd-id_branch    = objeto->carga-id_branch.
        wa_zsdt0001pd-id_bukrs     = objeto->carga-id_bukrs.
        wa_zsdt0001pd-nr_safra     = objeto->carga-nr_safra.

        TRY .

            zcl_fornecedores=>zif_parceiros~get_instance(
              )->set_parceiro_ie( i_insc_estatual = CONV #( zde_zsdt0001nt_alv-nr_fornecedor_ie )
              )->get_id_parceiro( IMPORTING e_parceiro = wa_zsdt0001pd-id_produtor
              )->ck_ativo(
              )->ck_ativo_empresa( i_empresa = objeto->carga-id_bukrs
              ).

            INSERT INTO zsdt0001pd VALUES wa_zsdt0001pd.
            COMMIT WORK.

          CATCH zcx_parceiros INTO ex_parceiros.
            ex_parceiros->published_erro( EXPORTING i_msgty = 'E' i_msgty_display = 'E' ).
        ENDTRY.

      ELSE.
        ex_carga->published_erro( EXPORTING i_msgty = 'E' i_msgty_display = 'E' ).
      ENDIF.
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  CONFERIR_CARGA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM conferir_carga .

  DATA: lc_peso_liquido TYPE zde_nm_peso_liquido.

  CLEAR: ck_conferiu.

*  LC_PESO_LIQUIDO = ZDE_ZSDT0001CG_ALV-NM_PESO_LIQUIDO.

*  DESCRIBE TABLE IT_NOTAS LINES DATA(QT_NOTAS).

*  IF QT_NOTAS GT 1.
*    "Sujerir o Peso Fiscal no Peso SubTotal
*    LOOP AT IT_NOTAS ASSIGNING FIELD-SYMBOL(<FS_NOTA>) WHERE NM_PESO_SUBTOTAL IS INITIAL.
*      <FS_NOTA>-NM_PESO_SUBTOTAL = <FS_NOTA>-NR_QUANTIDADE.
*
*      IF LC_PESO_LIQUIDO LE <FS_NOTA>-NM_PESO_SUBTOTAL.
*        <FS_NOTA>-NM_PESO_LIQUIDO = LC_PESO_LIQUIDO.
*        LC_PESO_LIQUIDO = 0.
*      ELSE.
*        <FS_NOTA>-NM_PESO_LIQUIDO = <FS_NOTA>-NM_PESO_SUBTOTAL.
*        IF ( LC_PESO_LIQUIDO - <FS_NOTA>-NM_PESO_SUBTOTAL ) LE 0.
*          LC_PESO_LIQUIDO = 0.
*        ELSE.
*          LC_PESO_LIQUIDO = LC_PESO_LIQUIDO - <FS_NOTA>-NM_PESO_SUBTOTAL.
*        ENDIF.
*      ENDIF.
*
*      TRY .
*          OBJETO->SET_PESOS_NOTAS(
*            EXPORTING
*              I_ID_CARGA      = <FS_NOTA>-ID_CARGA
*              I_ID_NOTA       = <FS_NOTA>-ID_NOTA
*              I_PESO_SUBTOTAL = <FS_NOTA>-NM_PESO_SUBTOTAL
*              I_PESO_LIQUIDO  = <FS_NOTA>-NM_PESO_LIQUIDO
*            IMPORTING
*              E_NOTA          = DATA(INFO_NOTA) ).
*          MOVE-CORRESPONDING INFO_NOTA TO <FS_NOTA>.
*        CATCH ZCX_CARGA.    "
*      ENDTRY.
*
*    ENDLOOP.
*  ELSE.
*    LOOP AT IT_NOTAS ASSIGNING <FS_NOTA>.
*
*      <FS_NOTA>-NM_PESO_SUBTOTAL = ZDE_ZSDT0001CG_ALV-NM_PESO_SUBTOTAL.
*      <FS_NOTA>-NM_PESO_LIQUIDO  = ZDE_ZSDT0001CG_ALV-NM_PESO_LIQUIDO.
*
*      TRY .
*          OBJETO->SET_PESOS_NOTAS(
*            EXPORTING
*              I_ID_CARGA      = <FS_NOTA>-ID_CARGA
*              I_ID_NOTA       = <FS_NOTA>-ID_NOTA
*              I_PESO_SUBTOTAL = <FS_NOTA>-NM_PESO_SUBTOTAL
*              I_PESO_LIQUIDO  = <FS_NOTA>-NM_PESO_LIQUIDO
*            IMPORTING
*              E_NOTA          = INFO_NOTA ).
*          MOVE-CORRESPONDING INFO_NOTA TO <FS_NOTA>.
*        CATCH ZCX_CARGA.    "
*      ENDTRY.
*
*    ENDLOOP.
*
*  ENDIF.

  CALL SCREEN 9004 STARTING AT 40 05.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRAR_NODE_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_NODE_KEY  text
*      -->P_ITEM_NAME  text
*----------------------------------------------------------------------*
FORM mostrar_node_log
  USING  p_node_key  TYPE  tv_nodekey
         p_item_name TYPE  tv_itmname.

  READ TABLE it_tree_info_log WITH KEY node_key = p_node_key item_name = p_item_name BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    TRY .
        objeto->get_info_alv_apresentacao_log(
          EXPORTING
            i_dt_registro  = it_tree_info_log-registro-dt_registro
            i_hr_registro  = it_tree_info_log-registro-hr_registro
            i_us_registro  = it_tree_info_log-registro-us_registro
          IMPORTING
            e_apresentacao = DATA(r_apresentacao) ).

        ck_registro_log    = abap_true.
        zde_zsdt0001cg_alv = r_apresentacao-carga.
        zde_zsdt0001od_alv = r_apresentacao-ordem_carrega.
        it_notas[] = r_apresentacao-notas[].
        it_takes_vincu[] = e_apresentacao-takeup[].

        LEAVE TO SCREEN 0300.
      CATCH zcx_carga INTO ex_carga.
        ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      CATCH zcx_ordem_carregamento INTO ex_ordem_carregamento.
        ex_ordem_carregamento->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    ENDTRY.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  ATUALIZA_TREE_LOGS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM atualiza_tree_logs .

  DATA: qtd_itens TYPE i,
        node      TYPE treev_node,
        item      TYPE mtreeitm.

  CHECK tree_0300 IS NOT INITIAL.

  tree_0300->delete_all_nodes( ).

  CLEAR: node_table_0300[], item_table_0300[], it_tree_info_log[], it_tree_info_log.

  TRY .
      objeto->get_logs_historico( IMPORTING e_logs = DATA(it_logs) ).
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

  CHECK it_logs[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_usuarios)
    FROM user_addr
     FOR ALL ENTRIES IN @it_logs
   WHERE bname EQ @it_logs-us_registro.

  SORT it_usuarios BY bname.

  DESCRIBE TABLE it_logs LINES DATA(qtd_linhas).

  LOOP AT it_logs INTO DATA(wa_logs).

    CLEAR: node.
    IF sy-tabix EQ qtd_linhas.
      node-n_image    = icon_customer.
      node-exp_image  = icon_customer.
    ELSE.
      node-n_image    = icon_hr_position.
      node-exp_image  = icon_hr_position.
    ENDIF.

    ADD 1 TO qtd_itens.
    node-node_key   = qtd_itens.
    CONDENSE node-node_key NO-GAPS.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = node-node_key
      IMPORTING
        output = node-node_key.

    node-hidden     = abap_false. " The node is visible,
    node-disabled   = abap_false. " selectable,
    node-isfolder   = abap_true. " a folder.
    node-expander   = abap_false.
    APPEND node TO node_table_0300.

    it_tree_info_log-node_key    = node-node_key.
    it_tree_info_log-item_name   = c_tree_0300-column1.
    it_tree_info_log-registro    = wa_logs.
    CONCATENATE wa_logs-dt_registro+6(2) '.' wa_logs-dt_registro+4(2) '.' wa_logs-dt_registro(4) INTO it_tree_info_log-dt_registro.
    CONCATENATE wa_logs-hr_registro(2) ':' wa_logs-hr_registro+2(2) ':' wa_logs-hr_registro+4(2) INTO it_tree_info_log-hr_registro.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0300-column1.
    item-class     = cl_gui_list_tree=>item_class_link. " Text Item
    item-alignment = cl_gui_list_tree=>align_auto.
    item-style     = cl_gui_list_tree=>style_intensifd_critical.
    item-font      = cl_gui_list_tree=>item_font_prop.
    READ TABLE it_usuarios INTO DATA(wa_usuarios) WITH KEY bname = wa_logs-us_registro.
    IF sy-subrc IS INITIAL.
      item-text = wa_usuarios-name_textc.
      it_tree_info_log-us_registro = wa_usuarios-name_textc.
    ELSE.
      item-text = wa_logs-us_registro.
      it_tree_info_log-us_registro = wa_logs-us_registro.
    ENDIF.
    APPEND item TO item_table_0300.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0300-column2.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_center.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_default.
    item-text      = it_tree_info_log-dt_registro.
    APPEND item TO item_table_0300.

    CLEAR item.
    item-node_key  = node-node_key.
    item-item_name = c_tree_0300-column3.
    item-class     = cl_gui_list_tree=>item_font_default. " Text Item
    item-alignment = cl_gui_list_tree=>align_center.
    item-style     = cl_gui_list_tree=>style_default.
    item-font      = cl_gui_list_tree=>item_font_default.
    item-text      = it_tree_info_log-hr_registro.
    APPEND item TO item_table_0300.

    APPEND it_tree_info_log.
  ENDLOOP.

  CALL METHOD tree_0300->add_nodes_and_items
    EXPORTING
      node_table                     = node_table_0300
      item_table                     = item_table_0300
      item_table_structure_name      = 'MTREEITM'
    EXCEPTIONS
      failed                         = 1
      cntl_system_error              = 3
      error_in_tables                = 4
      dp_error                       = 5
      table_structure_name_not_found = 6.

*    TREE_0300->EXPAND_NODES( EXPORTING NODE_KEY_TABLE = IT_NODE
*      EXCEPTIONS
*        FAILED                  = 1
*        CNTL_SYSTEM_ERROR       = 2
*        ERROR_IN_NODE_KEY_TABLE = 3
*        DP_ERROR                = 4
*        OTHERS                  = 5 ).

  SORT it_tree_info_log BY node_key item_name.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  GET_ENTRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE get_entra INPUT.

  IF zde_zsdt0001nt_alv-id_entrada IS NOT INITIAL.
    SELECT SINGLE ds_entrada INTO zde_zsdt0001nt_alv-ds_entrada
      FROM zsdt0001tetx
     WHERE id_entrada EQ zde_zsdt0001nt_alv-id_entrada.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  PRC_IN_GMO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE prc_in_gmo INPUT.

  TRY .

      objeto->set_transgenia(
         CHANGING
           i_in_gmo                = zde_zsdt0001cg_alv-in_gmo
           i_nr_resultado_01       = zde_zsdt0001cg_alv-nr_resultado_01
           i_nr_resultado_02       = zde_zsdt0001cg_alv-nr_resultado_02
           i_nr_res_rr1_rr2        = zde_zsdt0001cg_alv-nr_res_rr1_rr2
           i_in_gmo_03             = zde_zsdt0001cg_alv-in_gmo_03
           i_in_srr_origem_partic  = zde_zsdt0001cg_alv-in_srr_origem_partic
           i_id_outro_partic       = zde_zsdt0001cg_alv-id_outro_partic
           i_in_srr_declarado      = zde_zsdt0001cg_alv-in_srr_declarado
           i_in_teste_srr          = zde_zsdt0001cg_alv-in_teste_srr
           i_in_srr_declarado_2    = zde_zsdt0001cg_alv-in_srr_declarado_2
           i_in_teste_srr_2        = zde_zsdt0001cg_alv-in_teste_srr_2
           i_id_classificadora     = zde_zsdt0001cg_alv-id_classificadora
           i_tp_transgenia         = zde_zsdt0001cg_alv-tp_transgenia   ).
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( i_msgty = 'E' i_msgty_display = 'E' ).
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALU_REQUEST_ID_ENTRADA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_id_entrada INPUT.

  DATA: lc_tp_entrada TYPE  zsdt0001tetx.

  CALL FUNCTION 'Z_PSQ_TIPO_ENTRADA'
    EXPORTING
      id_bukrs   = zde_zsdt0001cg_alv-id_bukrs
      id_branch  = zde_zsdt0001cg_alv-id_branch
      nr_safra   = zde_zsdt0001cg_alv-nr_safra
    IMPORTING
      tp_entrada = lc_tp_entrada
    EXCEPTIONS
      erro       = 1
      OTHERS     = 2.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    zde_zsdt0001nt_alv-id_entrada = lc_tp_entrada-id_entrada.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  SETA_CAMPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_EX_ORDEM_VENDA_>MSGID  text
*      -->P_EX_ORDEM_VENDA_>MSGNO  text
*----------------------------------------------------------------------*
FORM seta_campo  USING    p_msgid TYPE syst_msgid
                          p_msgno TYPE syst_msgno.

  CLEAR: nm_field_set_nota,
         nm_field_set_carga.

  CASE p_msgid.
    WHEN 'ZODVENDA'. "Ordem de Venda

      nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM_VENDA'.

    WHEN 'ZORDEMCA'. "Ordem de Carregamento

      "Local Entrega
      nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM'.

    WHEN 'ZCARGA'.
      CASE p_msgno.

          """" Cabeçalho """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        WHEN zcx_carga=>zcx_obg_inf_ordem_venda-msgno OR
             zcx_carga=>zcx_obg_inf_produto-msgno.

          "Ordem de Venda
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM_VENDA'.

        WHEN zcx_carga=>zcx_ordem_empresaag-msgno.

          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE'.

        WHEN zcx_carga=>zcx_obg_inf_lc_entrega-msgno OR
             zcx_carga=>zcx_le_sem_param_lc_negocio-msgno OR
             zcx_carga=>zcx_le_sem_param_material-msgno.

          "Local Entrega
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_ENTREGA'.

        WHEN zcx_carga=>zcx_obg_inf_lc_coleta-msgno.

          "Local de Coleta
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_COLETA'.

        WHEN zcx_carga=>zcx_obg_inf_lc_destino-msgno.

          "Destino
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESTINO'.

        WHEN zcx_carga=>zcx_obg_inf_lc_descarga-msgno.

          "Local de Descarga
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESCARGA'.

        WHEN zcx_carga=>zcx_ordem_vencida-msgno OR
             zcx_carga=>zcx_ordem_produto-msgno OR
             zcx_carga=>zcx_placa_trator-msgno OR
             zcx_carga=>zcx_placa_reboque1-msgno OR
             zcx_carga=>zcx_placa_reboque2-msgno OR
             zcx_carga=>zcx_placa_reboque3-msgno OR
             zcx_carga=>zcx_ordem_motorista-msgno OR
             zcx_carga=>zcx_ordem_proprietario-msgno OR
             zcx_carga=>zcx_ordem_destino-msgno OR
             zcx_carga=>zcx_ordem_descarga-msgno.

          "Ordem de Carregamento
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_ORDEM'.

        WHEN zcx_carga=>zcx_obg_inf_motorista-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_MOTORISTA'.

        WHEN zcx_carga=>zcx_obg_inf_proprietario-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO'.

        WHEN zcx_carga=>zcx_obg_inf_trator-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR'.

        WHEN zcx_carga=>zcx_obg_inf_ticket-msgno OR
             zcx_carga=>zcx_erro_ticket_utilizado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_TICKET'.

        WHEN zcx_carga=>zcx_obg_inf_ps_bruto-msgno.

        WHEN zcx_carga=>zcx_obg_inf_ps_bruto-msgno OR
             zcx_carga=>zcx_tara_maior_bruto-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_BRUTO'.

        WHEN zcx_carga=>zcx_obg_inf_ps_tara-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_TARA'.

        WHEN zcx_carga=>zcx_obg_inf_ps_subtotal-msgno OR
             zcx_carga=>zcx_errp_ps_subtotal-msgno OR
             zcx_carga=>zcx_peso_liq_subtotal-msgno OR
             zcx_carga=>zcx_obg_inf_ps_liquido-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NM_PESO_SUBTOTAL'.

        WHEN zcx_carga=>zcx_obg_emp_classificadora-msgno OR
             zcx_carga=>zcx_fornecedor_nao_classifica-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_CLASSIFICADORA'.

        WHEN zcx_carga=>zcx_obg_class_umidade-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_UMI'.
        WHEN zcx_carga=>zcx_obg_class_impureza-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_IMP'.
        WHEN zcx_carga=>zcx_obg_class_avariado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_AVA'.
        WHEN zcx_carga=>zcx_obg_class_ardido-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_ARD'.
        WHEN zcx_carga=>zcx_obg_class_quebrado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_QUE'.
        WHEN zcx_carga=>zcx_obg_class_esverdeado-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-NR_PERC_ESV'.

        WHEN zcx_carga=>zcx_nao_teste_amaggi_positivo-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-IN_GMO'.

        WHEN zcx_carga=>zcx_obg_inf_outro_part-msgno OR
             zcx_carga=>zcx_nao_inf_outro_part-msgno.
          nm_field_set_carga = 'ZDE_ZSDT0001CG_ALV-ID_OUTRO_PARTIC'.

          """" Nota Fiscal """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
          "Tipo de Entrada
        WHEN zcx_carga=>zcx_obg_inf_tp_entrada-msgno OR
             zcx_carga=>zcx_tp_entrada_nao_permitido-msgno OR
             zcx_carga=>zcx_te_md_fiscal_sem_param-msgno OR
             zcx_carga=>zcx_te_sem_ct_fiscal-msgno OR
             zcx_carga=>zcx_te_sem_tp_mov_merc-msgno OR
             zcx_carga=>zcx_te_sem_iva-msgno OR
             zcx_carga=>zcx_te_sem_form_pagamento-msgno OR
             zcx_carga=>zcx_te_sem_ch_bloqueio-msgno OR
             zcx_carga=>zcx_te_sem_bnc_empresa-msgno OR
             zcx_carga=>zcx_te_somente_fisica-msgno OR
             zcx_carga=>zcx_te_somente_juridica-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-ID_ENTRADA'.

          "Modelo de Documento Fiscal
        WHEN zcx_carga=>zcx_nao_permitido_md_fiscal-msgno OR
             zcx_carga=>zcx_pessoa_fis_nfe-msgno OR
             zcx_carga=>zcx_pessoa_jus_papel-msgno OR
             zcx_carga=>zcx_nf_propria_nfe-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-ID_MOD_FISCAL'.

          "Chave NF-e
        WHEN zcx_carga=>zcx_obg_inf_chave_nfe-msgno OR
             zcx_carga=>zcx_nfe_nao_distribuida-msgno OR
             zcx_carga=>zcx_nfe_item_nao_distribuido-msgno OR
             zcx_carga=>zcx_nfe_item_unidade-msgno OR
             zcx_carga=>zcx_xml_nfe_nao_recebido-msgno.

          nm_field_set_nota = 'BTN_CHAVE'.

          "IE do Fornecedor
        WHEN zcx_carga=>zcx_obg_inf_ie_prod-msgno OR
             zcx_carga=>zcx_obg_inf_fornecedor-msgno OR
             zcx_carga=>zcx_forn_sem_parametro-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_FORNECEDOR_IE'.

          "Numero Documento
        WHEN zcx_carga=>zcx_obg_inf_nf_numero-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_NOTA'.

          "Série do Documento
        WHEN zcx_carga=>zcx_obg_inf_nf_serie-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NM_SERIE'.

          "Data de Emissão
        WHEN zcx_carga=>zcx_obg_inf_nf_dt_emissao-msgno OR
             zcx_carga=>zcx_data_emissao_nf-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-DT_EMISSAO'.

          "Quantidade
        WHEN zcx_carga=>zcx_obg_inf_nf_quantidade-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_QUANTIDADE'.

          "Valor
        WHEN zcx_carga=>zcx_obg_inf_nf_valor_total-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-NR_VALOR'.

          "Data de Vencimento
        WHEN zcx_carga=>zcx_obg_inf_dt_venc_form-msgno OR
             zcx_carga=>zcx_data_formulario_venc-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-DT_VENCIMENTO_FORM'.

          "CFOP
        WHEN zcx_carga=>zcx_obg_inf_cfop-msgno OR
             zcx_carga=>zcx_cfop_nao_permitido_te-msgno OR
             zcx_carga=>zcx_cfop_nao_permitido_forn-msgno OR
             zcx_carga=>zcx_xml_nfe_cfop_invalido-msgno.

          nm_field_set_nota = 'ZDE_ZSDT0001NT_ALV-CFOP'.

      ENDCASE.
  ENDCASE.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_OV  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_ov INPUT.

  DATA: e_vbeln TYPE  zde_ordem_venda_psq.

  CHECK objeto->carga-tp_status EQ zif_carga=>st_status_aberto.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Tipo de Pesquisaq'
      text_question         = 'Pesquisar Ordem de Venda?'
      text_button_1         = TEXT-003
      icon_button_1         = 'ICON_CHECKED'
      text_button_2         = TEXT-004
      icon_button_2         = 'ICON_INCOMPLETE'
      default_button        = '1'
      display_cancel_button = ' '
    IMPORTING
      answer                = answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CASE answer.
    WHEN '1'.

      CALL FUNCTION 'Z_PSQ_ORDEM_VENDA'
        EXPORTING
          i_id_bukrs  = zde_zsdt0001cg_alv-id_bukrs
          i_id_branch = zde_zsdt0001cg_alv-id_branch
          i_nr_safra  = zde_zsdt0001cg_alv-nr_safra
        IMPORTING
          e_vbeln     = e_vbeln
        EXCEPTIONS
          erro        = 1
          OTHERS      = 2.

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        zde_zsdt0001ov_alv-nr_ordem_venda = e_vbeln-vbeln.
      ENDIF.

    WHEN '2'.

      "MILHO EM GRAOS ADQ TERCEIROS
      "SOJA EM GRAOS ADQ TERCEIROS

      zcl_pedido_compra=>get_pedido_compra_chave_e(
        EXPORTING
          i_lifnr               = CONV #( zcl_string=>lpad( i_str = CONV #( zde_zsdt0001cg_alv-id_branch ) i_qtd = 10 i_char = '0' ) )
          i_bukrs               = zde_zsdt0001cg_alv-id_bukrs    " Empresa
          i_charg               = CONV #( zde_zsdt0001cg_alv-nr_safra )
          i_bstyp               = 'F'    " Categoria do documento de compras
          i_bsart               = 'ZUB'    " Tipo de documento de compras
          i_matnr_t             = VALUE #( ( matnr = '000000000000119892' ) ( matnr = '000000000000119895' ) (  ) )
       RECEIVING
         r_ekpo                = DATA(r_ekpo)    " Item do documento de compras
       EXCEPTIONS
         nao_encontrado_pedido = 1
         OTHERS                = 2
      ).

      IF sy-subrc IS NOT INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
        zde_zsdt0001ov_alv-nr_ordem_venda = r_ekpo-ebeln.
      ENDIF.

  ENDCASE.


ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  RETORNAR_HTML_WORKFLOW
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ZDE_ZSDT0001ACB_ALV  text
*      <--P_HTML_PAGINA  text
*----------------------------------------------------------------------*
FORM retornar_html_workflow  USING    p_zsdt0001acb_alv TYPE zde_zsdt0001acb_alv
                             CHANGING p_html_pagina TYPE string.

  DATA: tx_status_filial    TYPE string,
        tx_status_fiscal    TYPE string,
        tx_status_comercial TYPE string.

  DATA: mt_status_filial    TYPE string,
        mt_status_fiscal    TYPE string,
        mt_status_comercial TYPE string.

  DATA: cl_status_filial    TYPE string,
        cl_status_fiscal    TYPE string,
        cl_status_comercial TYPE string.

  p_html_pagina = '<!DOCTYPE html>' && '<html>' && '<head>' && '<style>'.

*A  Solicitação Aprovada
*R  Solicitação Recusada
*W  Solicitação Em Espera de Aprovação
*S  Solicitação não gera Aprovação

  CASE p_zsdt0001acb_alv-tp_solicitacao_status.
    WHEN zif_carga=>st_status_manut_aberto.
      cl_status_filial    = 'inicial'.
      cl_status_fiscal    = 'inicial'.
      cl_status_comercial = 'inicial'.
      tx_status_filial    = 'Solicitação de Manutenção não enviada'.
      tx_status_fiscal    = 'Solicitação de Manutenção não enviada'.
      tx_status_comercial = 'Solicitação de Manutenção não enviada'.
    WHEN OTHERS.
      CASE p_zsdt0001acb_alv-rs_aceite_filial.
        WHEN zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_filial = 'sem_aprovacao'.
          tx_status_filial = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> da Filial'.
        WHEN zif_carga=>st_rs_aceite_manut_espera.
          cl_status_filial = 'enviado'.
          tx_status_filial = 'Solicitação de Manutenção <b>enviada para aprovação</b> pela Filial'.
        WHEN zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_filial = 'aprovado'.
          tx_status_filial = 'Solicitação de Manutenção <b>aprovada</b> pela Filial'.
        WHEN zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_filial = 'recusado'.
          tx_status_filial = 'Solicitação de Manutenção <b>recusada</b> pela Filial'.
          mt_status_filial = '<b>' && p_zsdt0001acb_alv-ds_aceite_filial && '</b>'.
      ENDCASE.

      CASE p_zsdt0001acb_alv-rs_aceite_fiscal.
        WHEN zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_fiscal = 'sem_aprovacao'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> do CSC Fiscal'.
        WHEN zif_carga=>st_rs_aceite_manut_espera.
          cl_status_fiscal = 'enviado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>enviada para aprovação</b> do CSC Fiscal'.
        WHEN zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_fiscal = 'aprovado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>aprovada</b> pelo CSC Fiscal'.
        WHEN zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_fiscal = 'recusado'.
          tx_status_fiscal = 'Solicitação de Manutenção <b>recusada</b> pelo CSC Fiscal'.
          mt_status_fiscal = '<b>' && p_zsdt0001acb_alv-ds_aceite_fiscal && '</b>'.
      ENDCASE.

      CASE p_zsdt0001acb_alv-rs_aceite_comercial.
        WHEN zif_carga=>st_rs_aceite_manut_nao_gera.
          cl_status_comercial = 'sem_aprovacao'.
          tx_status_comercial = 'Solicitação de Manutenção <b>sem necessidade de aprovação</b> do CSC Financeiro'.
        WHEN zif_carga=>st_rs_aceite_manut_espera.
          cl_status_comercial = 'enviado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>enviada para aprovação</b> do CSC Financeiro'.
        WHEN zif_carga=>st_rs_aceite_manut_aprovada.
          cl_status_comercial = 'aprovado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>aprovada</b> pelo do CSC Financeiro'.
        WHEN zif_carga=>st_rs_aceite_manut_recusada.
          cl_status_comercial = 'recusado'.
          tx_status_comercial = 'Solicitação de Manutenção <b>recusada</b> pelo do CSC Financeiro'.
          mt_status_comercial = '<b>' && p_zsdt0001acb_alv-ds_aceite_comercial && '</b>'.
      ENDCASE.
  ENDCASE.

  p_html_pagina = p_html_pagina &&
 ' .inicial {' && ' background-color:LightGray;' && ' color:black;' &&' margin:10px;' && ' padding:10px;' && '}' &&
 ' .enviado {' && ' background-color:Orange;' &&' color:black;' &&' margin:10px;' && ' padding:10px;' && '}' &&
 ' .sem_aprovacao {' &&' background-color:DodgerBlue;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}' &&
 ' .aprovado {' &&' background-color:MediumSeaGreen;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}' &&
 ' .recusado {' &&' background-color:Tomato;' && ' color:black;' && ' margin:10px;' && ' padding:10px;' && '}'.

  p_html_pagina = p_html_pagina && '</style>' && '</head>' && '<body>'. " && '<table style="width:100%">' && '<tr>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_filial && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação Filial</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_filial && '</p>'.
  IF mt_status_filial IS NOT INITIAL.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_filial && '</b></p>'.
  ENDIF.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_fiscal && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação CSC Fiscal</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_fiscal && '</p>'.
  IF mt_status_fiscal IS NOT INITIAL.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_fiscal && '</b></p>'.
  ENDIF.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '<div class="' && cl_status_comercial && '">'.
  p_html_pagina = p_html_pagina && '<h3>Ação CSC Financeira</h3>'.
  p_html_pagina = p_html_pagina && '<p>' && tx_status_comercial && '</p>'.
  IF mt_status_comercial IS NOT INITIAL.
    p_html_pagina = p_html_pagina && '<p><b>' && mt_status_comercial && '</b></p>'.
  ENDIF.
  p_html_pagina = p_html_pagina && '</div>'.

  p_html_pagina = p_html_pagina && '</body>' && '</html>'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  VALUE_HELP_CLASSIFICA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_help_classifica INPUT.

  html_pagina =
'<!DOCTYPE html>' &&
'<html>' &&
'<style>' &&
' .teste_amaggi { background-color:White; color:black; margin:5px; padding:5px; }' &&
' .teste_declarado { background-color:Lavender; color:black; margin:5px; padding:5px; }' &&
' .teste_monsanto { background-color:Beige; color:black; margin:5px; padding:5px; }' &&
' .teste_participante { background-color:SeaShell; color:black; margin:5px; padding:5px; }' &&
' .outros { background-color:WhiteSmoke; color:black; margin:5px; padding:5px; }' &&
'</style>' &&
'<body>' &&
'<div align="center"><p><h3>Ajuda preenchimento de transgenia</h3></p></div>' &&
'<div class="teste_amaggi">' &&
'<p>O campo <b>“Teste Amaggi”</b> deve ser utilizado/preenchido <b>apenas</b> para cargas de <b>Soja Convencional</b>. Nesses casos, o ticket/laudo de ' &&
'classificação que acompanha a carga deve estar assinalado pelo classificador com a informação de que o resultado do teste foi negativo, e portanto, o ' &&
'produto é convencional. No lançamento deve ser selecionada a opção <b>“Negativo”</b>. Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
'</div>' &&

'<div class="outros">' &&
'<div class="outros">' &&
'<p>Para os casos de recebimento de soja transgênico, é necessário identificar se é um produto que possui a tecnologia Intacta. ' &&
'Essa definição pode ocorrer da seguinte forma:</p>' &&
'</div>' &&
'<div class="teste_declarado">' &&
'<p><b>Caso tenha sido descrito (Declarado) pelo produtor/fornecedor na Nota Fiscal da carga que o produto é Intacta (RR2)</b>, ' &&
'não deve ser realizado nenhum teste do produto, e o lançamento, a única marcação que deve ser feita é assinalar “Sim” no campo <b>“Intac. ' &&
'Declarado (RR2)”</b>. Nenhum dos demais campos deve ser marcado pelo usuário. <b>Essa opção deve ser selecionada apenas se estiver ' &&
'explicitamente escrito na Nota Fiscal que o produto é Intacta/RR2</b>.</p>' &&
'</div>' &&
'<div class="teste_monsanto">' &&
'<p>Em caso de <b>embarque de soja depositado em um armazém de empresa que é Participante do Programa Monsanto, devidamente comprovado ' &&
'através de consulta no site da Monsanto</b>, também não é realizado teste do produto. No lançamento da carga o usuário deve selecionar ' &&
'a opção <b>“Sim” no campo “Outro Participante?”, vinculando no campo “Outro Participante” o nome/endereço do Participante</b>. Nenhum dos ' &&
'demais campos deve ser marcado pelo usuário. <b>Deve estar descrito na Nota Fiscal da carga que o produto foi embarcado em Armazém Participante</b>.</p>' &&
'</div>' &&
'<div class="teste_participante">' &&
'<p>Caso o produto <b>não seja embarcado em um armazém Participante do Programa Monsanto</b> e a na <b>Nota Fiscal da carga não esteja Declarado ' &&
'que o produto é Intacta (RR2), deve ser realizado o teste</b> para identificar se o produto possui ou não a tecnologia Intacta, e o resultado ' &&
'desse teste deve ser informado pelo classificador no ticket/laudo de classificação.</p>' &&
'<p><b>Se o resultado do teste for negativo</b>, o usuário deve selecionar a opção <b>“Negativo”</b> no campo <b>“Monsanto Intac.(RR2)”</b>. ' &&
'Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
'<p><b>Se o resultado do teste for positivo</b>, o usuário deve selecionar a opção <b>“Positivo”</b> no campo <b>“Monsanto Intac.(RR2)”</b>. ' &&
'Nenhum dos demais campos deve ser marcado pelo usuário.</p>' &&
'</div>' &&
'</div>' &&
'</body>' &&
'</html>'.

  CALL SCREEN 9006 STARTING AT 05 05.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  SET_ORDEM_CARREG  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_ordem_carreg INPUT.

  CHECK zde_zsdt0001cg_alv-nr_ordem       IS NOT INITIAL.
  CHECK zde_zsdt0001ov_alv-nr_ordem_venda IS NOT INITIAL.



  TRY .

      objeto->verif_ordem_carregamento( ).

*      CATCH ZCX_CARGA.    "
*      CATCH ZCX_ORDEM_CARREGAMENTO.    "

*      DATA(R_ORDEM) =
*        ZCL_ORDEM_CARREGAMENTO=>BUSCA_ORDEM_CARREGAMENTO_NR(
*          EXPORTING
*            I_NR_SAFRA             = ZDE_ZSDT0001CG_ALV-NR_SAFRA
*            I_ID_BUKRS             = ZDE_ZSDT0001CG_ALV-ID_BUKRS
*            I_ID_BRANCH            = ZDE_ZSDT0001CG_ALV-ID_BRANCH
*            I_NR_ORDEM             = ZDE_ZSDT0001CG_ALV-NR_ORDEM ).
*
*
*      IF R_ORDEM-NR_SAFRA NE ZDE_ZSDT0001CG_ALV-NR_SAFRA.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_CARGA_SAFRA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_CARGA_SAFRA-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_CARGA_SAFRA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_CARGA_SAFRA-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_BUKRS NE ZDE_ZSDT0001CG_ALV-ID_BUKRS.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_CARGA_EMPRESA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_CARGA_EMPRESA-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_CARGA_EMPRESA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_CARGA_EMPRESA-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_BRANCH NE ZDE_ZSDT0001CG_ALV-ID_BRANCH.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_CARGA_FILIAL-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_CARGA_FILIAL-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_CARGA_FILIAL-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_CARGA_FILIAL-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DT_VALIDADE LT SY-DATLO AND R_ORDEM-DT_VALIDADE LT SY-DATUM
*          AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE
*          AND OBJETO->CK_EXECUTAR_MANUTENCAO_ENTRADA EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_VENCIDA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_VENCIDA-MSGNO ATTR1 = CONV #( R_ORDEM-DT_VALIDADE ) )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_VENCIDA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_VENCIDA-MSGNO
*            MSGV1  = CONV #( R_ORDEM-DT_VALIDADE ).
*      ENDIF.
*
*      DATA: LC_ID_AGENT_FRETE TYPE ZDE_ID_AGENT_FRETE.
*
*      LC_ID_AGENT_FRETE = R_ORDEM-ID_BRANCH_AG.
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*        EXPORTING
*          INPUT  = LC_ID_AGENT_FRETE
*        IMPORTING
*          OUTPUT = LC_ID_AGENT_FRETE.
*
*      IF LC_ID_AGENT_FRETE                 NE ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE  AND
*         ZDE_ZSDT0001CG_ALV-ID_AGENT_FRETE NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO AND
*         OBJETO->AT_MANUTENCAO  EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_EMPRESAAG-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_EMPRESAAG-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_EMPRESAAG-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_EMPRESAAG-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_PRODUTO NE ZDE_ZSDT0001CG_ALV-ID_PRODUTO.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_PRODUTO-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_PRODUTO-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_PRODUTO-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_PRODUTO-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DS_PLACA_TRATOR NE ZDE_ZSDT0001CG_ALV-DS_PLACA_TRATOR AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PLACA_TRATOR-MSGID MSGNO = ZCX_CARGA=>ZCX_PLACA_TRATOR-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_PLACA_TRATOR-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_PLACA_TRATOR-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DS_PLACA_REBOQ_1 NE ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_1 AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PLACA_REBOQUE1-MSGID MSGNO = ZCX_CARGA=>ZCX_PLACA_REBOQUE1-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_PLACA_REBOQUE1-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_PLACA_REBOQUE1-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DS_PLACA_REBOQ_2 NE ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_2 AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PLACA_REBOQUE2-MSGID MSGNO = ZCX_CARGA=>ZCX_PLACA_REBOQUE2-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_PLACA_REBOQUE2-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_PLACA_REBOQUE2-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-DS_PLACA_REBOQ_3 NE ZDE_ZSDT0001CG_ALV-DS_PLACA_REBOQ_3 AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_PLACA_REBOQUE3-MSGID MSGNO = ZCX_CARGA=>ZCX_PLACA_REBOQUE3-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_PLACA_REBOQUE3-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_PLACA_REBOQUE3-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_MOTORISTA NE ZDE_ZSDT0001CG_ALV-ID_MOTORISTA AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_MOTORISTA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_MOTORISTA-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_MOTORISTA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_MOTORISTA-MSGNO.
*      ENDIF.
*
*      IF R_ORDEM-ID_PROPRIETARIO NE ZDE_ZSDT0001CG_ALV-ID_PROPRIETARIO AND OBJETO->AT_MANUTENCAO EQ ABAP_FALSE.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_PROPRIETARIO-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_PROPRIETARIO-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_PROPRIETARIO-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_PROPRIETARIO-MSGNO.
*      ENDIF.
*
*      "Fornecedor
*      IF R_ORDEM-ID_LOCAL_DESTINO NE ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESTINO.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_DESTINO-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_DESTINO-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_DESTINO-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_DESTINO-MSGNO.
*      ENDIF.
*
*      "Destino
*      IF R_ORDEM-ID_LOCAL_DESCARGA NE ZDE_ZSDT0001CG_ALV-ID_LOCAL_DESCARGA.
*        RAISE EXCEPTION TYPE ZCX_CARGA
*          EXPORTING
*            TEXTID = VALUE #( MSGID = ZCX_CARGA=>ZCX_ORDEM_DESCARGA-MSGID MSGNO = ZCX_CARGA=>ZCX_ORDEM_DESCARGA-MSGNO )
*            MSGTY  = 'E'
*            MSGID  = ZCX_CARGA=>ZCX_ORDEM_DESCARGA-MSGID
*            MSGNO  = ZCX_CARGA=>ZCX_ORDEM_DESCARGA-MSGNO.
*      ENDIF.
    CATCH zcx_carga INTO ex_carga.
      ex_carga->published_erro( EXPORTING i_msgty = 'E' i_msgty_display = 'E' ).
*    CATCH ZCX_ORDEM_CARREGAMENTO.
  ENDTRY.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  APRESENTA_RESTRICAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_RESULTADO_RESTRI  text
*----------------------------------------------------------------------*
FORM apresenta_restricao  USING e_restri TYPE zde_pes_resultado_restricao.

  html_pagina =
'<!DOCTYPE html>' &&
'<html lang="en">' &&
'<head>' &&
'<title>CSS Template</title>' &&
'<meta charset="utf-8">' &&
'<meta name="viewport" content="width=device-width, initial-scale=1">' &&
'<style>' &&
'* {' &&
'    box-sizing: border-box;' &&
'}' &&
'' &&
'body {' &&
'    font-family: Arial, Helvetica, sans-serif;' &&
'}' &&
'' &&
'/* Style the header */' &&
'header {' &&
'    background-color: #666;' &&
'    padding: 30px;' &&
'    text-align: center;' &&
'    color: white;' &&
'}' &&
'' &&
'/* Create two columns/boxes that floats next to each other */' &&
'nav {' &&
'    float: left;' &&
'    width: 30%;' &&
'    height: 300px; /* only for demonstration, should be removed */' &&
'    background: #ccc;' &&
'    padding: 20px;' &&
'}' &&

'/* Style the list inside the menu */' &&
'nav ul {' &&
'    list-style-type: none;' &&
'    padding: 0;' &&
'}' &&

'article {' &&
'    float: left;' &&
'    padding: 20px;' &&
'    width: 100%;' &&
'    background-color: #f1f1f1;' &&
'    height: 300px; /* only for demonstration, should be removed */' &&
'}' &&

'/* Clear floats after the columns */' &&
'section:after {' &&
'    content: "";' &&
'    display: table;' &&
'    clear: both;' &&
'}' &&

'/* Style the footer */' &&
'footer {' &&
'    background-color: #777;' &&
'    padding: 10px;' &&
'    text-align: center;' &&
'    color: white;' &&
'}' &&

'/* Responsive layout - makes the two columns/boxes stack on top of each other instead of next to each other, on small screens */' &&
'@media (max-width: 600px) {' &&
'    nav, article {' &&
'        width: 100%;' &&
'        height: auto;' &&
'    }' &&
'}' &&
'</style>' &&
'</head>' &&
'<body>' &&

'<h2 style="text-align:center;color:Tomato;">Restrição de Embargo</h2>' &&

'<header>' &&
'<h4>' && e_restri-nome && '</h4>' &&
'</header>' &&

'<section>' &&
'  <article>' &&
    '<h1>' && e_restri-tipo && '</h1>' &&
    '<p>' && e_restri-motivo && '</p>' &&
'  </article>' &&
'</section>' &&

'<footer>' &&
'  <p>OPUS Crédito</p>' &&
'</footer>' &&

'</body>' &&
'</html>'.

  CALL SCREEN 9006 STARTING AT 05 05 ENDING AT 85 28.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0309  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0309 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  LOOP AT SCREEN.
    SPLIT screen-name AT '-' INTO: DATA(str10309) DATA(str20309).
    IF str10309 EQ 'ZDE_ZSDT0001CG_ALV' OR str10309 EQ 'ZDE_ZSDT0001OD_ALV' OR str10309 EQ 'ZDE_ZSDT0001OV_ALV'.
      IF str20309 EQ 'NM_PESO_SUBTOTAL'.
        objeto->valida_atributo_alteravel( EXPORTING i_campo = 'NM_PESO_BRUTO' IMPORTING e_permitido = DATA(e_permitido_0309) ).
        IF e_permitido_0309 EQ abap_false.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
        CONTINUE.
      ENDIF.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( EXPORTING i_campo = CONV #( str20309 ) IMPORTING e_permitido = e_permitido_0309 ).
      IF e_permitido_0309 EQ abap_false.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0311  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0311 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF objeto->carga-tp_status EQ zif_carga=>st_status_aberto.
    vg_tl_0312 = '0312'.
  ELSE.
    vg_tl_0312 = '0313'.
  ENDIF.

  LOOP AT SCREEN.
    SPLIT screen-name AT '-' INTO: DATA(str10311) DATA(str20311).
    IF str10311 EQ 'ZDE_ZSDT0001CG_ALV' OR str10311 EQ 'ZDE_ZSDT0001OD_ALV' OR str10311 EQ 'ZDE_ZSDT0001OV_ALV'.
      IF str20311 EQ 'NM_PESO_SUBTOTAL'.
        objeto->valida_atributo_alteravel( EXPORTING i_campo = 'NM_PESO_BRUTO' IMPORTING e_permitido = DATA(e_permitido_0311) ).
        IF e_permitido_0311 EQ abap_false.
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
        CONTINUE.
      ENDIF.
      "Campos não Alterável
      objeto->valida_atributo_alteravel( EXPORTING i_campo = CONV #( str20311 ) IMPORTING e_permitido = e_permitido_0311 ).
      IF e_permitido_0311 EQ abap_false.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.

    ENDIF.
  ENDLOOP.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  STATUS_9012  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9012 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.

  IF ctl_cccontainer_9012 IS INITIAL.

    CREATE OBJECT ctl_cccontainer_9012
      EXPORTING
        container_name = 'IMAGEM'.

    CREATE OBJECT picture_9012
      EXPORTING
        parent = ctl_cccontainer_9012
      EXCEPTIONS
        error  = 1.

    CALL METHOD picture_9012->set_display_mode
      EXPORTING
        display_mode = picture_9012->display_mode_stretch
      EXCEPTIONS
        error        = 1.

    PERFORM load_pic_from_db USING picture_9012.

  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK_0306
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW  text
*----------------------------------------------------------------------*
FORM handle_double_click_0306  USING p_row TYPE lvc_s_row.

  DATA: lc_row TYPE lvc_t_row.

  CHECK p_row-index IS NOT INITIAL.

  IF p_row-rowtype IS INITIAL.

    APPEND p_row TO lc_row.

    CALL METHOD ctl_alv_0306->set_selected_rows
      EXPORTING
        it_index_rows = lc_row.

    READ TABLE it_notas INDEX p_row-index INTO wa_nota_selecionada.

    CLEAR: event_handler_0312a.

    IF ctl_alv_0312a IS NOT INITIAL.
      ctl_alv_0312a->free( ).
    ENDIF.
    CLEAR: ctl_alv_0312a.

    CLEAR: obg_toolbar_0312a.

    IF ctl_cccontainer_0312a IS NOT INITIAL.
      ctl_cccontainer_0312a->free( ).
    ENDIF.
    CLEAR: ctl_cccontainer_0312a.

    CLEAR: it_takes_saldo[].

*    DATA: P_TITLE TYPE LVC_TITLE.
*    PERFORM TEXTO_NOTA_SELECIONADA CHANGING P_TITLE.
*
*    IF CTL_ALV_0312A IS NOT INITIAL.
*      CTL_ALV_0312A->SET_GRIDTITLE( I_GRIDTITLE = P_TITLE ).
*    ENDIF.

    LEAVE TO SCREEN 0300.

  ENDIF.

ENDFORM.

*** Inicio - Rubenilson Pereira - 22.04.25 #169642
*----------------------------------------------------------------------*
***INCLUDE ZSDR0060_5900.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_5900 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*

  DATA: lo_custom_bordero TYPE REF TO cl_gui_custom_container,
        lo_splitter_1     TYPE REF TO cl_gui_splitter_container,
        lo_splitter_2     TYPE REF TO cl_gui_splitter_container,
        lo_parent_1       TYPE REF TO cl_gui_container,
        lo_parent_2       TYPE REF TO cl_gui_container,
        lo_bordero_header TYPE REF TO cl_gui_alv_grid,
        lo_bordero_itens  TYPE REF TO cl_gui_alv_grid.


  DATA: ls_layout_bordero TYPE lvc_s_layo.

  DATA: lt_0206 TYPE TABLE OF zmmt0206,
        lt_0205 TYPE TABLE OF zmmt0205.

  CLASS handle_event DEFINITION.
    PUBLIC SECTION.
      CLASS-METHODS : handle_double_click
                FOR EVENT double_click OF cl_gui_alv_grid IMPORTING e_row .

  ENDCLASS.

  CLASS handle_event IMPLEMENTATION.
    METHOD handle_double_click.
      READ TABLE lt_0205 ASSIGNING FIELD-SYMBOL(<fs_0205>) INDEX e_row-index.
      IF sy-subrc IS INITIAL.
        SELECT *
          FROM zmmt0206
          INTO TABLE lt_0206
          WHERE id_chegada_cd_luft = <fs_0205>-id_chegada_cd_luft.
        IF sy-subrc IS INITIAL.
          lo_bordero_itens->refresh_table_display( ).
        ENDIF.
      ENDIF.
    ENDMETHOD.
  ENDCLASS.

  MODULE status_5900 OUTPUT.
    DATA: lt_fcode TYPE TABLE OF sy-ucomm,
          lv_lifnr TYPE lifnr.

    FREE: lt_fcode.

    IF sy-ucomm EQ 'CRIA_CARGA' OR gv_ucomm EQ 'CRIA_CARGA'.
      APPEND 'EDIT_ITENS' TO lt_fcode.
      APPEND 'AUTOR_EMB' TO lt_fcode.
      APPEND 'EDIT_LOG' TO lt_fcode.
      APPEND 'INF_CHAVE' TO lt_fcode.
      APPEND 'CONF_CARGA' TO lt_fcode.
      APPEND 'CANC_CARGA' TO lt_fcode.
      APPEND 'REJ_CARGA' TO lt_fcode.
    ELSEIF  gv_ucomm EQ 'EDIT_ITENS'.
      IF g_grid IS BOUND.
        IF g_grid->is_ready_for_input( ) EQ 1  .
          APPEND 'AUTOR_EMB' TO lt_fcode.
          APPEND 'EDIT_LOG' TO lt_fcode.
          APPEND 'INF_CHAVE' TO lt_fcode.
          APPEND 'CONF_CARGA' TO lt_fcode.
          APPEND 'CANC_CARGA' TO lt_fcode.
          APPEND 'REJ_CARGA' TO lt_fcode.
        ENDIF.
      ENDIF.
    ELSEIF gv_ucomm EQ 'EDIT_LOG' AND gv_edit_log IS NOT INITIAL.
      APPEND 'EDIT_ITENS' TO lt_fcode.
      APPEND 'AUTOR_EMB' TO lt_fcode.
      APPEND 'INF_CHAVE' TO lt_fcode.
      APPEND 'CONF_CARGA' TO lt_fcode.
      APPEND 'CANC_CARGA' TO lt_fcode.
      APPEND 'REJ_CARGA' TO lt_fcode.
    ELSEIF gv_ucomm EQ 'INF_CHAVE'.
      IF g_grid_chaves IS BOUND.
        IF g_grid_chaves->is_ready_for_input( ) EQ 1.
          APPEND 'EDIT_ITENS' TO lt_fcode.
          APPEND 'AUTOR_EMB' TO lt_fcode.
          APPEND 'EDIT_LOG' TO lt_fcode.
          APPEND 'CONF_CARGA' TO lt_fcode.
          APPEND 'CANC_CARGA' TO lt_fcode.
          APPEND 'REJ_CARGA' TO lt_fcode.
        ENDIF.
      ENDIF.
    ENDIF.

    IF wa_tela_monta_carga-viagem_id IS NOT INITIAL.
      APPEND 'EDIT_LOG' TO lt_fcode.
    ENDIF.

    SET TITLEBAR 'T5900' WITH wa_tela_monta_carga-nro_cg .
    SET PF-STATUS 'STATUS_GUI_5900' EXCLUDING lt_fcode.

    IF wa_tela_monta_carga-motorista IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_tela_monta_carga-motorista
        IMPORTING
          output = lv_lifnr.

      SELECT SINGLE name1
        FROM lfa1
        INTO wa_tela_monta_carga-desc_motorista
        WHERE lifnr = lv_lifnr.
    ENDIF.

    IF wa_tela_monta_carga-transportadora IS NOT INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_tela_monta_carga-transportadora
        IMPORTING
          output = lv_lifnr.

      SELECT SINGLE name1
        FROM lfa1
        INTO wa_tela_monta_carga-desc_transp
        WHERE lifnr = lv_lifnr.
    ENDIF.

    CASE sy-ucomm.
      WHEN 'PUSH1' OR 'PUSH2' OR 'PUSH3'.
        tabstrip2-activetab = sy-ucomm.
      WHEN 'INF_CHAVE'.
        tabstrip2-activetab = 'PUSH2'.
      WHEN 'EDIT_ITENS'.
        tabstrip2-activetab = 'PUSH1'.
      WHEN OTHERS.
    ENDCASE.

    IF sy-ucomm IS INITIAL OR gv_ucomm EQ 'CRIA_CARGA'.

      LOOP AT SCREEN.

        IF screen-name CS 'TAB'.
          screen-invisible = 1.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    ENDIF.

    IF gv_ucomm <> 'CRIA_CARGA'.

*    IF sy-ucomm EQ 'LIST_CARGA' OR
*       sy-ucomm EQ 'TAB1' OR
*       sy-ucomm EQ 'TAB2' OR
*       sy-ucomm EQ 'TAB3' OR
*       sy-ucomm EQ 'PUSH1' OR
*       sy-ucomm EQ 'PUSH2' OR
*       sy-ucomm EQ 'PUSH3' OR
*       sy-ucomm EQ 'EDIT_ITENS' OR
*       sy-ucomm EQ 'AUTOR_EMB' OR
*       sy-ucomm EQ 'EDIT_LOG' OR
*       sy-ucomm EQ 'INF_CHAVE' OR
*       sy-ucomm EQ 'CONF_CARGA' OR
*       sy-ucomm EQ 'CANC_CARGA' OR
*       sy-ucomm EQ 'REJ_CARGA' OR
*       sy-ucomm EQ 'SAVE'.

      "D -  Modo de exibição
      "E -  Modo de modificação

      ip_mode = 'E'.

      CLEAR obj.
      obj-objtype = 'ZSDT0112'.
      obj-objkey  = wa_tela_monta_carga-nro_cg.

      CREATE OBJECT manager
        EXPORTING
          is_object        = obj
          ip_no_commit     = 'R'
          ip_mode          = ip_mode
        EXCEPTIONS
          object_invalid   = 1
          callback_invalid = 2
          OTHERS           = 3.

      LOOP AT SCREEN.
        IF screen-name EQ 'TAB1' OR
           screen-name EQ 'TAB2' OR
           screen-name EQ 'TAB3' .
          CONTINUE.
        ENDIF.

        IF screen-name CS 'TAB'.
          screen-invisible = 0.
          screen-active = 1.
        ENDIF.

        IF gv_edit_log IS INITIAL.
          screen-input = 0.
        ENDIF.

        MODIFY SCREEN.

      ENDLOOP.

*    ENDIF.

    ENDIF.
  ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  MODULE user_command_5900 INPUT.

    CASE sy-ucomm.
      WHEN 'SAVE'.

        PERFORM f_grava_carga.

      WHEN 'BACK' OR 'LEAVE' OR 'EXIT'.

        IF manager IS NOT INITIAL.
          CALL METHOD manager->unpublish.
          FREE manager.
        ENDIF.

        gv_ucomm = sy-ucomm.
        g_grid_chaves->free( ).
        g_grid_notas->free( ).
        g_grid->free( ).
        g_custom_container->free( ).
        g_custom_container_chaves->free( ).
        g_custom_container_notas->free( ).

        FREE: g_custom_container,
              g_custom_container_chaves,
              g_custom_container_notas,
              g_grid_chaves,
              g_grid_notas,
              g_grid.

        CLEAR: gv_ucomm,
               gv_edit_log.

        zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
        EXPORTING
          i_nro_cg = wa_tela_monta_carga-nro_cg
          bloqueio = abap_false
          IMPORTING
            msg = DATA(lv_msg) ).

        SET SCREEN 0.
        LEAVE SCREEN.

      WHEN 'EDIT_ITENS'.
        gv_ucomm = sy-ucomm.
        PERFORM f_edita_itens.

      WHEN 'EDIT_LOG'.
        gv_ucomm = sy-ucomm.
        PERFORM f_edit_log.

      WHEN 'AUTOR_EMB'.
        gv_ucomm = sy-ucomm.
        PERFORM f_autoriza_embarque.

      WHEN 'CANC_CARGA'.
        gv_ucomm = sy-ucomm.
        PERFORM f_cancela_carga.

      WHEN 'INF_CHAVE'.
        gv_ucomm = sy-ucomm.
        PERFORM f_informar_chave.
      WHEN 'CONF_CARGA'.
        PERFORM f_conferir_carga.
      WHEN OTHERS.
    ENDCASE.
  ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_grava_carga
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_grava_carga .

    DATA: lo_cargueiro TYPE REF TO zcl_integracao_lote_frete,
          lo_retorno   TYPE REF TO zif_integracao_lote_frete,
          lt_0201      TYPE TABLE OF zmmt0201,
          lt_0204      TYPE TABLE OF zsdt0391,
          lt_0346      TYPE TABLE OF zsdt0346,
          lt_0202      TYPE TABLE OF zmmt0202,
          lv_msg       TYPE string,
          lt_0203_save TYPE TABLE OF zmmt0203,
          ls_dados     TYPE zsde_grava_carga.

    IF wa_tela_monta_carga-nro_cg IS INITIAL.

      MOVE-CORRESPONDING wa_tela_monta_carga TO ls_dados.

      zcl_carga_entrada_insumos=>grava_carga(
      IMPORTING
        e_msg_erro = DATA(lv_msg_erro)
        e_carga = DATA(lv_carga)
        CHANGING
        i_dados = ls_dados
        ).
      IF lv_carga IS NOT INITIAL.
        CONCATENATE 'Carga' lv_carga 'criada com sucesso' INTO lv_msg SEPARATED BY space.
        MESSAGE lv_msg TYPE 'S'.
      ELSE.
        MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR: wa_tela_monta_carga-desc_motorista,
             wa_tela_monta_carga-desc_transp,
             wa_tela_monta_carga-motorista,
             wa_tela_monta_carga-transportadora,
             wa_tela_monta_carga-placa_carreta1,
             wa_tela_monta_carga-placa_carreta2,
             wa_tela_monta_carga-placa_cavalo,
             wa_tela_monta_carga-placa_dolly,
             wa_tela_monta_carga-qtd_prevista,
             wa_tela_monta_carga-valor_frete,
             wa_tela_monta_carga-tipo_frete.

    ELSE.


      IF t_chaves_del IS NOT INITIAL.

        zcl_carga_entrada_insumos=>elimina_notas_carga(
         EXPORTING
           i_nro_carga     = wa_tela_monta_carga-nro_cg
           i_chaves        = t_chaves_del
           i_transferencia = abap_false
         IMPORTING
           e_msg_erro = lv_msg_erro ).

      ENDIF.

      IF t_chaves_transf_del IS NOT INITIAL.

        zcl_carga_entrada_insumos=>elimina_notas_carga(
         EXPORTING
           i_nro_carga     = wa_tela_monta_carga-nro_cg
           i_chaves        = t_chaves_transf_del
           i_transferencia = abap_true
         IMPORTING
           e_msg_erro = lv_msg_erro ).

      ENDIF.

      IF wa_tela_monta_carga <> wa_tela_monta_carga_aux.

        MOVE-CORRESPONDING wa_tela_monta_carga TO ls_dados.

      ENDIF.

      IF t_sub_itens_del IS NOT INITIAL.
        APPEND LINES OF t_sub_itens_del TO t_sub_itens.
      ENDIF.

      zcl_carga_entrada_insumos=>atualiza_dados_carga(
      EXPORTING
        i_nro_carga           = wa_tela_monta_carga-nro_cg
        i_header              = ls_dados
        i_itens               = t_sub_itens
        i_notas_venda         = t_chaves_nf
        i_notas_transferencia = t_sub_notas
      IMPORTING
      e_msg_erro = lv_msg_erro ).
      IF lv_msg_erro IS INITIAL.
        MESSAGE 'Dados salvos com sucesso!' TYPE 'S'.
        IF g_grid IS BOUND.
* lock edit enabled cells against input
          CALL METHOD g_grid->set_ready_for_input
            EXPORTING
              i_ready_for_input = 0.

          CALL METHOD g_grid_chaves->set_ready_for_input
            EXPORTING
              i_ready_for_input = 0.

          CALL METHOD g_grid_notas->set_ready_for_input
            EXPORTING
              i_ready_for_input = 0.
        ENDIF.
        CLEAR: gv_ucomm,
               gv_edit_log.
      ELSE.
        DELETE t_sub_itens WHERE cancel = abap_true.
        MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      g_grid_chaves->free( ).
      g_grid_notas->free( ).
      g_grid->free( ).
      g_custom_container->free( ).
      g_custom_container_chaves->free( ).
      g_custom_container_notas->free( ).

      FREE: g_custom_container,
            g_custom_container_chaves,
            g_custom_container_notas,
            g_grid_chaves,
            g_grid_notas,
            g_grid,
            t_lista_cargas,
            t_alv_lista_cargas,
            t_0203,
            t_chaves_nf,
            t_sub_itens,
            t_sub_notas,
            t_sub_itens_del.

      zcl_carga_entrada_insumos=>busca_dados_carga(
         EXPORTING
           i_carga = e_carga[]
           i_empresa = e_bukrs[]
           i_segmento = gv_spart
           i_id_viagem = e_idvgm[]
           i_dt_carga  = e_dtcar[]
          IMPORTING
            e_cds_carga = t_lista_cargas
            e_cargas = t_alv_lista_cargas
            e_notas  = t_0203
            e_msg_erro = lv_msg_erro ).

    ENDIF.

  ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_exibe_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_exibe_alv .

    DATA: lw_layout   TYPE lvc_s_layo,
          lt_fieldcat TYPE lvc_t_fcat,
          lt_events   TYPE slis_t_event,
          lv_title    TYPE lvc_title.

    CHECK t_monta_carga IS NOT INITIAL.

    PERFORM f_monta_fieldcat USING 'ZSDE_ALV_MONT_CARGA'
                          CHANGING lt_fieldcat.

    lw_layout-zebra = abap_true.
    lw_layout-box_fname = 'SEL'.

    APPEND INITIAL LINE TO lt_events ASSIGNING FIELD-SYMBOL(<fs_events>).

    <fs_events>-form = 'XPF_STATUS_SET'.
    <fs_events>-name = slis_ev_pf_status_set.

    APPEND INITIAL LINE TO lt_events ASSIGNING <fs_events>.

    <fs_events>-form = 'XUSER_COMMAND'.
    <fs_events>-name = slis_ev_user_command.

    CASE abap_true.
      WHEN p_spart2.
        lv_title = 'Fertilizantes'.
      WHEN p_spart3.
        lv_title = 'Defensivos'.
      WHEN p_spart4.
        lv_title = 'Sementes'.
      WHEN OTHERS.
    ENDCASE.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program = sy-repid
        is_layout_lvc      = lw_layout
        i_grid_title       = lv_title
        it_fieldcat_lvc    = lt_fieldcat
        it_events          = lt_events
      TABLES
        t_outtab           = t_monta_carga
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      <-- LT_FIELDCAT
*&---------------------------------------------------------------------*
  FORM f_monta_fieldcat  USING  p_estrutura   TYPE dd02l-tabname
                         CHANGING ch_fieldcat TYPE lvc_t_fcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = p_estrutura
      CHANGING
        ct_fieldcat            = ch_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.

      LOOP AT ch_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

        CASE <fs_fieldcat>-fieldname.
          WHEN 'SEL'.
            <fs_fieldcat>-no_out = abap_true.
          WHEN 'ITEM_SOLIC'.
            PERFORM f_edit_fieldcat USING 'Item Solicitação' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN 'DATA_SOLIC'.
            PERFORM f_edit_fieldcat USING 'Data Solicitação' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN 'SEGMENTO'.
            PERFORM f_edit_fieldcat USING 'Segmento' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN 'SALDO_A_SOLIC'.
            PERFORM f_edit_fieldcat USING 'Saldo à Solicitar' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN 'DATA_RECEB'.
            PERFORM f_edit_fieldcat USING 'Data Prevista Recebimento' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN 'PONTO_COLETA'.
            PERFORM f_edit_fieldcat USING 'Ponto Coleta' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN 'OBS_ROTEIRO_PC'.
            PERFORM f_edit_fieldcat USING 'Obs Roteiro PC' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN 'LOCAL_ENTREGA'.
            PERFORM f_edit_fieldcat USING 'Local Entrega' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN 'OBS_ROTEIRO_LE'.
            PERFORM f_edit_fieldcat USING 'Obs Roteiro LE' '' ''
                                    CHANGING <fs_fieldcat>.
          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.
    ENDIF.
  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_edit_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&---------------------------------------------------------------------*
  FORM f_edit_fieldcat  USING    VALUE(p_descricao)
                                 VALUE(p_tamanho)
                                 VALUE(p_edit)
                        CHANGING ch_fieldcat TYPE lvc_s_fcat.

    DESCRIBE FIELD p_descricao LENGTH DATA(lv_length) IN CHARACTER MODE.

    ch_fieldcat-scrtext_l = p_descricao.
    ch_fieldcat-scrtext_m = p_descricao.

    IF lv_length > 10 .
      ch_fieldcat-scrtext_s = p_descricao(10).
    ELSE.
      ch_fieldcat-scrtext_s = p_descricao.
    ENDIF.

    ch_fieldcat-outputlen = p_tamanho.
    ch_fieldcat-edit = p_edit.

  ENDFORM.


  FORM xpf_status_set USING ucomm TYPE kkblo_t_extab.       "#EC CALLED
    DATA: tl_fcode TYPE TABLE OF sy-ucomm,
          wl_fcode TYPE sy-ucomm.

    DATA: gt_f4 TYPE lvc_t_f4.
    DATA: gs_f4 TYPE lvc_s_f4.

    DATA: gr_events       TYPE REF TO lcl_event_receiver,
          ls_sel_hide     TYPE slis_sel_hide_alv,
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
      SET HANDLER:
                   gr_events->handle_on_button_click FOR ref1.
      init  = 'X'.
    ENDIF.
    SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING tl_fcode.
  ENDFORM. "XPF_STATUS_SET

  FORM xpf_status_set2 USING ucomm TYPE kkblo_t_extab.      "#EC CALLED
    DATA: tl_fcode TYPE TABLE OF sy-ucomm,
          wl_fcode TYPE sy-ucomm.

    DATA: gt_f4 TYPE lvc_t_f4.
    DATA: gs_f4 TYPE lvc_s_f4.

    DATA: gr_events       TYPE REF TO lcl_event_receiver,
          ls_sel_hide     TYPE slis_sel_hide_alv,
          it_fieldcatalog TYPE lvc_t_fcat,
          wa_fieldcatalog TYPE lvc_s_fcat,
          is_table        TYPE lvc_s_stbl.

    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = ls_sel_hide
        e_grid      = ref2.

    CALL METHOD ref2->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = it_fieldcatalog.

    is_table-row = 'X'.
    is_table-col = 'X'.

    CALL METHOD ref2->refresh_table_display
      EXPORTING
        is_stable      = is_table
        i_soft_refresh = 'X'.

    IF init2 IS INITIAL.
      CALL METHOD ref2->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD ref2->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      CREATE OBJECT gr_events.
      SET HANDLER:
                   gr_events->handle_on_button_click FOR ref2.
      init  = 'X'.
    ENDIF.

    SET PF-STATUS 'STANDARD_FULLSCREEN2' EXCLUDING tl_fcode.

  ENDFORM. "XPF_STATUS_SET

  FORM xuser_command USING ucomm    LIKE sy-ucomm
                           selfield TYPE kkblo_selfield.

    DATA: tl_0082        TYPE TABLE OF zsdt0082,
          wl_0082        TYPE zsdt0082,
          p_number       TYPE zde_nro_sol,
          wl_header      TYPE thead,
          wl_index(3)    TYPE n,
          tl_tlines      LIKE tline OCCURS 0 WITH HEADER LINE,
          wg_index,
          xsdo           TYPE zsdt0082-qte_sol,
          xqtsol         TYPE zsdt0082-qte_sol,
          tqtsol(20)     TYPE c,
          xqlib          TYPE zsdt0082-qte_lib,
          tqlib(20)      TYPE c,
          nrosol         TYPE zsdt0082-nro_sol,
          lv_num_criadas TYPE c LENGTH 255,
          ls_sel_hide    TYPE slis_sel_hide_alv,
          is_table       TYPE lvc_s_stbl,
          msg_error(255),
          wa_zsdt0132    TYPE zsdt0132.

    DATA: tl_vbep   TYPE TABLE OF vbep WITH HEADER LINE,
          valida    TYPE char1,
          f_headinx LIKE bapisdh1x,
          tl_return TYPE TABLE OF bapiret2   WITH HEADER LINE.

    DATA: BEGIN OF i_order_item_in OCCURS 0.
            INCLUDE STRUCTURE bapisditm.
    DATA: END   OF i_order_item_in.

    DATA: BEGIN OF i_order_item_inx OCCURS 0.
            INCLUDE STRUCTURE bapisditmx.
    DATA: END   OF i_order_item_inx.

    DATA: BEGIN OF i_sched OCCURS 10.
            INCLUDE STRUCTURE bapischdl.
    DATA: END OF i_sched.

    DATA: BEGIN OF i_schedx OCCURS 10.
            INCLUDE STRUCTURE bapischdlx.
    DATA: END OF i_schedx.

    DATA: r_werks TYPE RANGE OF werks_d.

    DATA: it_values  TYPE TABLE OF rgsb4.

    DATA: wa_values TYPE rgsb4,
          wa_werks  LIKE LINE OF r_werks.

    DATA: lw_sel_hide TYPE slis_sel_hide_alv,
          lt_sel_rows TYPE lvc_t_row,
          lw_sel_rows TYPE lvc_s_row,
          lt_zmmt0196 TYPE TABLE OF zmmt0196,
          lv_seq      TYPE zmmt0196-seq,
          lv_nro_sol  TYPE zmmt0196-nro_sol,
          lv_msg      TYPE string.

    REFRESH it_values.

    gv_ucomm = sy-ucomm.

    CASE ucomm.

      WHEN 'CRIA_CARGA'.

        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            es_sel_hide = lw_sel_hide
            e_grid      = ref1.

        CALL METHOD ref1->get_selected_rows
          IMPORTING
            et_index_rows = lt_sel_rows.

        CLEAR lv_seq.

        IF lt_sel_rows IS INITIAL.
          MESSAGE 'Favor selecionar ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ELSE.
          DESCRIBE TABLE lt_sel_rows LINES DATA(lv_lines).
          IF lv_lines > 1.
            MESSAGE 'Favor selecionar apenas uma linha' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDIF.

        READ TABLE lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_rows>) INDEX 1.
        IF sy-subrc IS INITIAL.
          READ TABLE t_monta_carga ASSIGNING FIELD-SYMBOL(<fs_monta_carga>) INDEX <fs_rows>-index.
          IF sy-subrc IS INITIAL.

            zcl_solicitacao_entrada_insumo=>bloqueia_solicitacao(
            EXPORTING
              nro_solicitacao = <fs_monta_carga>-nro_solic
            IMPORTING
              bloqueado     = DATA(lv_bloq)
              mensagem_erro = lv_msg ).
            IF lv_bloq IS NOT INITIAL.

              wa_tela_monta_carga-nro_sol = <fs_monta_carga>-nro_solic.
              wa_tela_monta_carga-data_embarque = <fs_monta_carga>-data_receb.

*            wa_tela_monta_carga-seq = <fs_monta_carga>-item_solic.

              SELECT *
                FROM zmmt0196
                INTO @DATA(ls_0196)
                UP TO 1 ROWS
                WHERE nro_sol = @<fs_monta_carga>-nro_solic.
              ENDSELECT.
              IF sy-subrc IS INITIAL.
                wa_tela_monta_carga-ponto_coleta = ls_0196-parceiro_pc.

                SELECT SINGLE name1
                  FROM lfa1
                  INTO wa_tela_monta_carga-descricao_pc
                  WHERE lifnr = wa_tela_monta_carga-ponto_coleta.

                wa_tela_monta_carga-local_entrega = ls_0196-parceiro_le.

                SELECT SINGLE name1
                  FROM lfa1
                  INTO wa_tela_monta_carga-descricao_le
                  WHERE lifnr = wa_tela_monta_carga-local_entrega.

              ENDIF.

              CALL SCREEN '5900'.

            ELSE.

              MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.

            ENDIF.

          ENDIF.

        ENDIF.

    ENDCASE.

  ENDFORM.

  FORM xuser_command2 USING ucomm    LIKE sy-ucomm
                           selfield TYPE kkblo_selfield.

    DATA: tl_0082        TYPE TABLE OF zsdt0082,
          wl_0082        TYPE zsdt0082,
          p_number       TYPE zde_nro_sol,
          wl_header      TYPE thead,
          wl_index(3)    TYPE n,
          tl_tlines      LIKE tline OCCURS 0 WITH HEADER LINE,
          wg_index,
          xsdo           TYPE zsdt0082-qte_sol,
          xqtsol         TYPE zsdt0082-qte_sol,
          tqtsol(20)     TYPE c,
          xqlib          TYPE zsdt0082-qte_lib,
          tqlib(20)      TYPE c,
          nrosol         TYPE zsdt0082-nro_sol,
          lv_num_criadas TYPE c LENGTH 255,
          ls_sel_hide    TYPE slis_sel_hide_alv,
          is_table       TYPE lvc_s_stbl,
          msg_error(255),
          wa_zsdt0132    TYPE zsdt0132.

    DATA: tl_vbep   TYPE TABLE OF vbep WITH HEADER LINE,
          valida    TYPE char1,
          f_headinx LIKE bapisdh1x,
          tl_return TYPE TABLE OF bapiret2   WITH HEADER LINE.

    DATA: BEGIN OF i_order_item_in OCCURS 0.
            INCLUDE STRUCTURE bapisditm.
    DATA: END   OF i_order_item_in.

    DATA: BEGIN OF i_order_item_inx OCCURS 0.
            INCLUDE STRUCTURE bapisditmx.
    DATA: END   OF i_order_item_inx.

    DATA: BEGIN OF i_sched OCCURS 10.
            INCLUDE STRUCTURE bapischdl.
    DATA: END OF i_sched.

    DATA: BEGIN OF i_schedx OCCURS 10.
            INCLUDE STRUCTURE bapischdlx.
    DATA: END OF i_schedx.

    DATA: r_werks TYPE RANGE OF werks_d.

    DATA: it_values  TYPE TABLE OF rgsb4.

    DATA: wa_values TYPE rgsb4,
          wa_werks  LIKE LINE OF r_werks.

    DATA: lw_sel_hide TYPE slis_sel_hide_alv,
          lt_sel_rows TYPE lvc_t_row,
          lw_sel_rows TYPE lvc_s_row,
          lt_zmmt0196 TYPE TABLE OF zmmt0196,
          lv_seq      TYPE zmmt0196-seq,
          lv_nro_sol  TYPE zmmt0196-nro_sol,
          lv_msg      TYPE string.

    REFRESH it_values.

    CASE ucomm.

      WHEN 'LIST_CARGA'.

        FREE: t_sub_itens,
              t_sub_notas,
              t_chaves_nf,
              t_chaves_del,
              t_chaves_transf_del.

        zcl_carga_entrada_insumos=>busca_dados_carga(
             EXPORTING
               i_carga = e_carga[]
               i_empresa = e_bukrs[]
               i_segmento = gv_spart
               i_id_viagem = e_idvgm[]
               i_dt_carga  = e_dtcar[]
              IMPORTING
                e_cds_carga = t_lista_cargas
                e_cargas = DATA(lt_cargas)
                e_notas  = t_0203
                e_msg_erro = lv_msg_erro ).

        SORT t_lista_cargas BY nrocg.

        CLEAR: gv_ucomm.

        CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
          IMPORTING
            es_sel_hide = lw_sel_hide
            e_grid      = ref2.

        CALL METHOD ref2->get_selected_rows
          IMPORTING
            et_index_rows = lt_sel_rows.

        CLEAR lv_seq.

        IF lt_sel_rows IS INITIAL.
          MESSAGE 'Favor selecionar ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ELSE.
          DESCRIBE TABLE lt_sel_rows LINES DATA(lv_lines).
          IF lv_lines > 1.
            MESSAGE 'Favor selecionar apenas uma linha' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDIF.

        READ TABLE lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_rows>) INDEX 1.
        IF sy-subrc IS INITIAL.
          READ TABLE t_alv_lista_cargas ASSIGNING FIELD-SYMBOL(<fs_alv_lista_carga>) INDEX <fs_rows>-index.
          IF sy-subrc IS INITIAL.

            READ TABLE t_lista_cargas ASSIGNING FIELD-SYMBOL(<fs_lista_cargas>)
            WITH KEY nrocg = <fs_alv_lista_carga>-nro_carga
            BINARY SEARCH.

            IF sy-subrc IS INITIAL.
              wa_tela_monta_carga-nro_cg = <fs_lista_cargas>-nrocg.
              wa_tela_monta_carga-nro_sol = <fs_lista_cargas>-nrosol.
              wa_tela_monta_carga-ponto_coleta = <fs_lista_cargas>-pontocoleta.
              wa_tela_monta_carga-descricao_pc = <fs_lista_cargas>-desc_pc.
              wa_tela_monta_carga-local_entrega = <fs_lista_cargas>-localentrega.
              wa_tela_monta_carga-descricao_le = <fs_lista_cargas>-desc_le.
              wa_tela_monta_carga-transportadora = <fs_lista_cargas>-codtransportadora.
              wa_tela_monta_carga-desc_transp    = <fs_lista_cargas>-desc_transp.
              wa_tela_monta_carga-motorista      = <fs_lista_cargas>-codmotorista.
              wa_tela_monta_carga-desc_motorista = <fs_lista_cargas>-nomemotorista.
              wa_tela_monta_carga-tipo_frete     = <fs_lista_cargas>-inco1 .
              wa_tela_monta_carga-data_embarque  = <fs_lista_cargas>-dtprevistaembarque.
              wa_tela_monta_carga-viagem_id = <fs_lista_cargas>-viagemid.

              IF <fs_lista_cargas>-modalidade_frete EQ '1'.
                wa_tela_monta_carga-frete_por_t = abap_true.
              ELSEIF <fs_lista_cargas>-modalidade_frete EQ '2'.
                wa_tela_monta_carga-frete_por_v = abap_true.
              ENDIF.

              wa_tela_monta_carga-valor_frete = <fs_lista_cargas>-preco_frete.
              wa_tela_monta_carga-placa_cavalo = <fs_lista_cargas>-placacav.
              wa_tela_monta_carga-placa_carreta1 = <fs_lista_cargas>-placacar1.
              wa_tela_monta_carga-placa_carreta2 = <fs_lista_cargas>-placacar2.
              wa_tela_monta_carga-placa_dolly = <fs_lista_cargas>-placacar3.
              wa_tela_monta_carga-qtd_prevista = <fs_lista_cargas>-qtdtotalkg.
              wa_tela_monta_carga-status = <fs_lista_cargas>-status.

            ENDIF.

            tabstrip2-activetab = 'PUSH1'.

            wa_tela_monta_carga_aux = wa_tela_monta_carga.

            CALL SCREEN '5900'.

            IF ( gv_ucomm EQ 'CANC_CARGA' OR gv_ucomm EQ 'REJ_CARGA' ) AND
                 t_alv_lista_cargas IS INITIAL.
              CLEAR gv_ucomm.
              SET SCREEN 0.
              LEAVE SCREEN.
            ENDIF.

            selfield-refresh = 'X'.

          ELSE.


            MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.

          ENDIF.

        ENDIF.

      WHEN 'REFRESH'.

        PERFORM f_refresh_listar_carga.
        selfield-refresh = 'X'.

    ENDCASE.

  ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_exibe_alv_lista
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_exibe_alv_lista .
    DATA: lw_layout   TYPE lvc_s_layo,
          lt_fieldcat TYPE lvc_t_fcat,
          lt_events   TYPE slis_t_event.

    CHECK t_alv_lista_cargas IS NOT INITIAL.

    PERFORM f_monta_fieldcat USING 'ZSDE_ALV_LISTA_CARGA'
                          CHANGING lt_fieldcat.

    lw_layout-zebra = abap_true.
    lw_layout-box_fname = 'SEL'.

    APPEND INITIAL LINE TO lt_events ASSIGNING FIELD-SYMBOL(<fs_events>).

    <fs_events>-form = 'XPF_STATUS_SET2'.
    <fs_events>-name = slis_ev_pf_status_set.

    APPEND INITIAL LINE TO lt_events ASSIGNING <fs_events>.

    <fs_events>-form = 'XUSER_COMMAND2'.
    <fs_events>-name = slis_ev_user_command.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
      EXPORTING
        i_callback_program = sy-repid
        is_layout_lvc      = lw_layout
        it_fieldcat_lvc    = lt_fieldcat
        it_events          = lt_events
      TABLES
        t_outtab           = t_alv_lista_cargas
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDFORM.

*&---------------------------------------------------------------------*
*& Module MONTA_SUB_ITENS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  MODULE monta_sub_itens OUTPUT.
    DATA: ls_layout   TYPE lvc_s_layo,
          lt_fieldcat TYPE lvc_t_fcat,
          lw_celltab  TYPE lvc_s_styl,
          lt_f4       TYPE lvc_t_f4,
          ls_f4       TYPE lvc_s_f4,
          lt_color    TYPE lvc_t_scol,
          ls_color    TYPE lvc_s_scol.

    CHECK gv_ucomm NE 'EDIT_ITENS'.

    FREE:
          lt_fieldcat,
          lt_f4,
          ls_layout.

    DATA(lt_lista_cargas) = t_lista_cargas.
    DELETE lt_lista_cargas WHERE nrocg <> wa_tela_monta_carga-nro_cg OR seq EQ space OR itemcancel = abap_true.

    IF lt_lista_cargas IS NOT INITIAL.

      FREE t_sub_itens.

      SELECT nro_sol,seq,solicitacao_qte
        FROM zmmt0196
        INTO TABLE @DATA(lt_0196)
        FOR ALL ENTRIES IN @lt_lista_cargas
        WHERE nro_sol = @lt_lista_cargas-nrosol
         AND seq      = @lt_lista_cargas-seq.
      IF sy-subrc IS INITIAL.
        SORT lt_0196 BY nro_sol seq.
      ENDIF.

      SELECT nro_sol, seq, qtd_vinc_carga
        FROM zmmt0202
        INTO TABLE @DATA(lt_0202)
        FOR ALL ENTRIES IN @lt_lista_cargas
        WHERE nro_sol = @lt_lista_cargas-nrosol
         AND  seq = @lt_lista_cargas-seq.
      IF sy-subrc IS INITIAL.
        SORT lt_0202 BY nro_sol seq.
      ENDIF.

    ENDIF.

    zcl_solicitacao_entrada_insumo=>saldo_solicitacao(
       EXPORTING
         i_nro_cg            = wa_tela_monta_carga-nro_cg
         i_nro_solicitacao   = wa_tela_monta_carga-nro_sol
         i_itens  = abap_true
         i_tp_saldo = 'G'
       IMPORTING
         e_saldo = DATA(lt_saldo) ).

    SORT lt_saldo BY nro_solic seq tp_saldo.

    LOOP AT lt_lista_cargas ASSIGNING FIELD-SYMBOL(<fs_lista_carga>).

      APPEND INITIAL LINE TO t_sub_itens ASSIGNING FIELD-SYMBOL(<fs_sub_itens>).

      <fs_sub_itens>-item_carga    = <fs_lista_carga>-itemcarga.
      <fs_sub_itens>-nro_sol       = <fs_lista_carga>-nrosol_item.
      <fs_sub_itens>-item_solic    = <fs_lista_carga>-seq.
      <fs_sub_itens>-bukrs         = <fs_lista_carga>-bukrs.
      <fs_sub_itens>-ebeln         = <fs_lista_carga>-ebeln.
      <fs_sub_itens>-ebelp         = <fs_lista_carga>-ebelp.
      <fs_sub_itens>-esart         = <fs_lista_carga>-tipo_pedido.
      <fs_sub_itens>-ponto_coleta  = <fs_lista_carga>-pontocoleta.
      <fs_sub_itens>-desc_pc       = <fs_lista_carga>-desc_pc.
      <fs_sub_itens>-local_entrega = <fs_lista_carga>-localentrega.
      <fs_sub_itens>-desc_le       = <fs_lista_carga>-desc_le.
      <fs_sub_itens>-matnr         = <fs_lista_carga>-matnr.
      <fs_sub_itens>-maktx         = <fs_lista_carga>-desc_material.
      <fs_sub_itens>-qtd_carga     = <fs_lista_carga>-qtdvinccarga.
      <fs_sub_itens>-unidade       = <fs_lista_carga>-unidade.
      <fs_sub_itens>-tp_saldo_vinc = <fs_lista_carga>-tp_saldo_vinc.
      <fs_sub_itens>-qtd_carga_kg  = <fs_lista_carga>-qtdvinccargakg.

      READ TABLE lt_saldo ASSIGNING FIELD-SYMBOL(<fs_saldo>)
      WITH KEY nro_solic  = <fs_lista_carga>-nrosol_item
               seq        = <fs_lista_carga>-seq
               tp_saldo   = <fs_lista_carga>-tp_saldo_vinc
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_sub_itens>-saldo_a_formar = <fs_saldo>-saldo.
      ENDIF.

      <fs_sub_itens>-celltab = lt_celltab.

    ENDLOOP.

    ls_layout-stylefname = 'CELLTAB'.
    ls_layout-info_fname = 'COLOR'.
    ls_layout-cwidth_opt = 'X'.

    IF g_custom_container IS INITIAL.

      CREATE OBJECT g_custom_container
        EXPORTING
          container_name = 'CONTAINER_ITENS'.

      CREATE OBJECT g_grid
        EXPORTING
          i_parent = g_custom_container.

      CALL METHOD g_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD g_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      SET HANDLER:
      lcl_carga=>set_toolbar     FOR g_grid,
      lcl_carga=>get_ucomm       FOR g_grid,
      lcl_carga=>on_data_changed FOR g_grid,
      lcl_carga=>on_f4           FOR g_grid.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZSDE_ALV_ITENS_CARGA'
        CHANGING
          ct_fieldcat            = lt_fieldcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      IF sy-subrc = 0.

        READ TABLE lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>)
        WITH KEY fieldname = 'NRO_SOL'.
        IF sy-subrc IS INITIAL.
          <fs_fieldcat>-f4availabl = abap_true.
          <fs_fieldcat>-emphasize = 'C310'.
        ENDIF.

        READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
        WITH KEY fieldname = 'QTD_CARGA'.
        IF sy-subrc IS INITIAL.
          <fs_fieldcat>-edit = abap_true.
        ENDIF.

        READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
        WITH KEY fieldname = 'QTD_CARGA_KG'.
        IF sy-subrc IS INITIAL.
          <fs_fieldcat>-do_sum = abap_true.
        ENDIF.

        READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
        WITH KEY fieldname = 'CANCEL'.
        IF sy-subrc IS INITIAL.
          <fs_fieldcat>-no_out = abap_true.
        ENDIF.

        IF g_grid->is_ready_for_input( ) EQ 0.
          READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
          WITH KEY fieldname = 'SALDO_A_FORMAR'.
          IF sy-subrc IS INITIAL.
            <fs_fieldcat>-no_out = abap_true.
          ENDIF.
        ENDIF.

      ENDIF.

      ls_f4-fieldname  = 'NRO_SOL'.
      ls_f4-register   = 'X'.

      APPEND ls_f4 TO lt_f4.

      CALL METHOD g_grid->register_f4_for_fields
        EXPORTING
          it_f4 = lt_f4.

      CALL METHOD g_grid->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.

      CALL METHOD g_grid->set_table_for_first_display
        EXPORTING
*         i_structure_name = 'ZSDE_ALV_ITENS_CARGA'
          is_layout       = ls_layout
        CHANGING
          it_fieldcatalog = lt_fieldcat
          it_outtab       = t_sub_itens.

    ELSE.

      g_grid->refresh_table_display( ).

    ENDIF.


  ENDMODULE.

*&---------------------------------------------------------------------*
*& Module MONTA_SUB_NOTAS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  MODULE monta_sub_notas OUTPUT.
    DATA: lt_excluded TYPE ui_functions.

    CHECK gv_ucomm NE 'INF_CHAVE'.

    FREE: t_sub_notas,
          t_chaves_nf,
          lt_fieldcat,
          lt_celltab.

    lw_celltab-fieldname = 'CHAVE_NOTA'.
    lw_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
    INSERT lw_celltab INTO lt_celltab INDEX 1.

    lt_lista_cargas = t_lista_cargas.
    DELETE lt_lista_cargas WHERE nrocg <> wa_tela_monta_carga-nro_cg.

    READ TABLE lt_lista_cargas ASSIGNING <fs_lista_carga> INDEX 1.
    IF sy-subrc IS INITIAL.
      vg_transf_fornec = <fs_lista_carga>-transfnofornecedor.
    ENDIF.

*  LOOP AT lt_lista_cargas ASSIGNING <fs_lista_carga>.

    READ TABLE t_0203 TRANSPORTING NO FIELDS
    WITH KEY nro_cg = wa_tela_monta_carga-nro_cg
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT t_0203 ASSIGNING FIELD-SYMBOL(<fs_0203>) FROM sy-tabix.
        IF <fs_lista_carga>-nrocg <> <fs_0203>-nro_cg.
          EXIT.
        ENDIF.

        CASE <fs_0203>-processo.
          WHEN '1'.
            APPEND INITIAL LINE TO t_sub_notas ASSIGNING FIELD-SYMBOL(<fs_sub_notas>).

            <fs_sub_notas>-data_nfe = <fs_0203>-data_nfe.
            <fs_sub_notas>-chave_cte = <fs_0203>-chave_cte.
            <fs_sub_notas>-chave_nota = <fs_0203>-chave_nfe.
            <fs_sub_notas>-celltab = lt_celltab.

          WHEN '2'.
            APPEND INITIAL LINE TO t_chaves_nf ASSIGNING FIELD-SYMBOL(<fs_chaves>).
            <fs_chaves>-chave_nota = <fs_0203>-chave_nfe.
            <fs_chaves>-celltab = lt_celltab.
          WHEN OTHERS.
        ENDCASE.

      ENDLOOP.

    ENDIF.

*  ENDLOOP.

    APPEND cl_gui_alv_grid=>mc_fc_sum TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_find TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_graph TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_help TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_info TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_filter TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_maximum TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_minimum TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_sort TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_refresh TO  lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_detail TO  lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_print TO lt_excluded.
    APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO lt_excluded.

    IF g_custom_container_chaves IS INITIAL.

      CREATE OBJECT g_custom_container_chaves
        EXPORTING
          container_name = 'CONTAINER_NOTAS1'.

      CREATE OBJECT g_grid_chaves
        EXPORTING
          i_parent = g_custom_container_chaves.

      CALL METHOD g_grid_chaves->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD g_grid_chaves->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      SET HANDLER:
      lcl_carga_notas=>set_toolbar     FOR g_grid_chaves,
      lcl_carga_notas=>get_ucomm       FOR g_grid_chaves,
      lcl_carga_notas=>on_data_changed FOR g_grid_chaves.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZSDE_ALV_SUB_CHAVES_NF'
        CHANGING
          ct_fieldcat            = lt_fieldcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      IF sy-subrc = 0.

        READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
        WITH KEY fieldname = 'CHAVE_NOTA'.
        IF sy-subrc IS INITIAL.
          <fs_fieldcat>-edit = abap_true.
        ENDIF.

      ENDIF.

      CALL METHOD g_grid_chaves->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD g_grid_chaves->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      CALL METHOD g_grid_chaves->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.

      ls_layout-stylefname = 'CELLTAB'.
      ls_layout-grid_title = 'Notas de venda do Fornecedor'.

      CALL METHOD g_grid_chaves->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding = lt_excluded
          is_layout            = ls_layout
        CHANGING
          it_fieldcatalog      = lt_fieldcat
          it_outtab            = t_chaves_nf.

    ELSE.

      g_grid_chaves->refresh_table_display( ).

    ENDIF.

    IF g_custom_container_notas IS INITIAL.

      CREATE OBJECT g_custom_container_notas
        EXPORTING
          container_name = 'CONTAINER_NOTAS2'.

      CREATE OBJECT g_grid_notas
        EXPORTING
          i_parent = g_custom_container_notas.

      FREE: lt_fieldcat.

      CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
        EXPORTING
          i_structure_name       = 'ZSDE_ALV_SUB_NOTAS'
        CHANGING
          ct_fieldcat            = lt_fieldcat
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
      IF sy-subrc = 0.

        READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
        WITH KEY fieldname = 'CHAVE_NOTA'.
        IF sy-subrc IS INITIAL.
          <fs_fieldcat>-edit = abap_true.
        ENDIF.

        READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
        WITH KEY fieldname = 'DATA_NFE'.
        IF sy-subrc IS INITIAL.
          <fs_fieldcat>-edit = abap_true.
        ENDIF.

        READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
        WITH KEY fieldname = 'CHAVE_CTE'.
        IF sy-subrc IS INITIAL.
          <fs_fieldcat>-edit = abap_true.
        ENDIF.

      ENDIF.

      SET HANDLER:
      lcl_carga_notas2=>set_toolbar     FOR g_grid_notas,
      lcl_carga_notas2=>get_ucomm       FOR g_grid_notas,
      lcl_carga_notas2=>on_data_changed FOR g_grid_notas.

      CALL METHOD g_grid_notas->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD g_grid_notas->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      CALL METHOD g_grid_notas->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.

      ls_layout-grid_title = 'Notas de transferência entre o Fornecedor'.

      CALL METHOD g_grid_notas->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding = lt_excluded
          is_layout            = ls_layout
        CHANGING
          it_fieldcatalog      = lt_fieldcat
          it_outtab            = t_sub_notas.

    ELSE.

      g_grid_notas->refresh_table_display( ).

    ENDIF.
  ENDMODULE.

*&---------------------------------------------------------------------*
*& Form f_edita_itens
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_edita_itens .

    DATA: ls_celltab TYPE lvc_s_styl,
          lv_mkl     TYPE mara-matkl,
          lv_trf     TYPE c,
          lv_msg2    TYPE string,
          lv_text    TYPE string,
          lv_answer  TYPE c.

    gv_ucomm = sy-ucomm.

    DATA(r_msg_error) = zcl_carga_entrada_insumos=>habilitar_edicao_itens( EXPORTING i_nro_carga = wa_tela_monta_carga-nro_cg ).

    IF r_msg_error IS NOT INITIAL.
      MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    FREE: lt_celltab.

    ls_celltab-fieldname = 'QTD_CARGA'.
    ls_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    INSERT ls_celltab INTO lt_celltab INDEX 3.

    LOOP AT t_sub_itens ASSIGNING FIELD-SYMBOL(<fs_sub_itens>).
      <fs_sub_itens>-celltab = lt_celltab.
    ENDLOOP.

    CALL METHOD g_grid->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = lt_fieldcat.

    IF g_grid->is_ready_for_input( ) EQ 0.

      zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
        EXPORTING
          i_nro_cg = wa_tela_monta_carga-nro_cg
          bloqueio = abap_true
        IMPORTING
          msg = DATA(lv_msg) ).
      IF lv_msg IS NOT INITIAL.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

* set edit enabled cells ready for input
      CALL METHOD g_grid->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.

      SELECT transf_no_fornecedor,ebeln,ebelp
        FROM zmmt0196
        INTO @DATA(ls_0196)
        UP TO 1 ROWS
        WHERE nro_sol = @wa_tela_monta_carga-nro_sol.
      ENDSELECT.
      IF sy-subrc IS INITIAL.
        lv_trf = ls_0196-transf_no_fornecedor.

        SET PARAMETER ID 'TRF' FIELD lv_trf.

        SELECT SINGLE matkl
          FROM ekpo
          INTO lv_mkl
          WHERE ebeln = ls_0196-ebeln
            AND ebelp = ls_0196-ebelp.
        IF sy-subrc IS INITIAL.
          SET PARAMETER ID 'MKL' FIELD lv_mkl.
        ENDIF.
      ENDIF.

      READ TABLE lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>)
      WITH KEY fieldname = 'SALDO_A_FORMAR'.
      IF sy-subrc IS INITIAL.
        <fs_fieldcat>-no_out = abap_false.
      ENDIF.

    ELSE.

      zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
      EXPORTING
        i_nro_cg = wa_tela_monta_carga-nro_cg
        bloqueio = abap_false
        IMPORTING
          msg = lv_msg ).

      SET PARAMETER ID 'MKL' FIELD lv_mkl.
      SET PARAMETER ID 'TRF' FIELD lv_trf.

* lock edit enabled cells against input
      CALL METHOD g_grid->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.

      CLEAR gv_ucomm.
      FREE: t_sub_itens.

      READ TABLE lt_fieldcat ASSIGNING <fs_fieldcat>
      WITH KEY fieldname = 'SALDO_A_FORMAR'.
      IF sy-subrc IS INITIAL.
        <fs_fieldcat>-no_out = abap_true.
      ENDIF.


    ENDIF.

    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = lt_fieldcat.

    CALL METHOD g_grid->refresh_table_display.

  ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_insere_novos_itens
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_insere_novos_itens .
    DATA: lt_celltab TYPE lvc_t_styl,
          lw_celltab TYPE lvc_s_styl,
          lt_color   TYPE lvc_t_scol,
          ls_color   TYPE lvc_s_scol.

    SELECT MAX( item_carga )
      FROM zmmt0202 INTO @DATA(lv_item_carga)
     WHERE nro_cg = @wa_tela_monta_carga-nro_cg.

    APPEND INITIAL LINE TO t_sub_itens ASSIGNING FIELD-SYMBOL(<fs_sub_itens>).

    ADD 1 TO lv_item_carga.
    <fs_sub_itens>-item_carga = lv_item_carga.

*  <fs_sub_itens>-color = 'C300'.

    lw_celltab-fieldname = 'QTD_CARGA'.
    lw_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
    INSERT lw_celltab INTO lt_celltab INDEX 1.

    <fs_sub_itens>-celltab = lt_celltab.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wl_stable.
  ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_edit_log
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_edit_log .

    DATA: lv_mode TYPE c VALUE 'E',
          lv_data TYPE datum,
          lv_msg2 TYPE string.

    DATA(lt_lista_carga) = t_lista_cargas.

    DATA(r_msg_error) = zcl_carga_entrada_insumos=>habilitar_edicao_dados_logist( EXPORTING i_nro_carga = wa_tela_monta_carga-nro_cg ).
    IF r_msg_error IS NOT INITIAL.
      MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF gv_edit_log IS INITIAL.
      gv_edit_log = abap_true.
      zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
        EXPORTING
          i_nro_cg = wa_tela_monta_carga-nro_cg
          bloqueio = abap_true
        IMPORTING
          msg = DATA(lv_msg) ).
      IF lv_msg IS NOT INITIAL.
        MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.
    ELSE.
      CLEAR gv_edit_log.

      wa_tela_monta_carga = wa_tela_monta_carga_aux.

      zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
        EXPORTING
          i_nro_cg = wa_tela_monta_carga-nro_cg
          bloqueio = abap_false
        IMPORTING
          msg = lv_msg ).
    ENDIF.

  ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_autoriza_embarque
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_autoriza_embarque .

    gv_ucomm = sy-ucomm.

    zcl_carga_entrada_insumos=>gerar_autorizacao_embarque(
      EXPORTING
        i_nro_cg      = wa_tela_monta_carga-nro_cg
      IMPORTING
        e_msg_error   = DATA(lva_msg_error)
      RECEIVING
        r_sucesso     = DATA(lva_sucesso) ).

    IF lva_msg_error IS NOT INITIAL.
      MESSAGE lva_msg_error TYPE 'I'.
    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_cancela_carga
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_cancela_carga .
    DATA: lv_answer          TYPE c,
          tl_texto           TYPE catsxt_longtext_itab,
          lo_lote_embarcador TYPE REF TO zcl_integracao_lote_frete,
          lo_lote_frete      TYPE REF TO zif_integracao_lote_frete,
          lv_motivo          TYPE string,
          lv_id_integracao   TYPE zde_id_integracao,
          lv_msg2            TYPE string,
          lv_erro            TYPE c.

    gv_ucomm = sy-ucomm.

    zcl_carga_entrada_insumos=>cancela_carga(
    EXPORTING
      i_nro_carga = wa_tela_monta_carga-nro_cg
      i_motivo    = lv_motivo
    IMPORTING
      e_msg_erro = DATA(lv_msg) ).

    IF lv_msg IS NOT INITIAL.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ELSE.
      MESSAGE 'Carga cancelada com sucesso' TYPE 'S'.
    ENDIF.

    DELETE t_alv_lista_cargas WHERE nro_carga = wa_tela_monta_carga-nro_cg.

    IF t_alv_lista_cargas IS INITIAL.
      PERFORM f_limpa_variaveis.
    ENDIF.

    SET SCREEN 0.
    LEAVE SCREEN.


  ENDFORM.

  FORM f_conferir_carga.

    zcl_carga_entrada_insumos=>conferir_carga_v2(
      EXPORTING
        i_nro_carga = wa_tela_monta_carga-nro_cg
      IMPORTING
        e_msg_erro = DATA(lva_msg_error)
    ).

    IF lva_msg_error IS NOT INITIAL.
      MESSAGE lva_msg_error TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_informar_chave
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_informar_chave .

    DATA: lv_msg2 TYPE string.

    DATA(r_msg_error) = zcl_carga_entrada_insumos=>habilitar_edicao_chaves( EXPORTING i_nro_carga = wa_tela_monta_carga-nro_cg ).

    IF r_msg_error IS NOT INITIAL.
      MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
    EXPORTING
      i_nro_cg = wa_tela_monta_carga-nro_cg
      bloqueio = abap_true
      IMPORTING
        msg = DATA(lv_msg) ).
    IF lv_msg IS NOT INITIAL.
      MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF g_grid_chaves->is_ready_for_input( ) EQ 0.
* set edit enabled cells ready for input
      CALL METHOD g_grid_chaves->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.
    ELSE.

      CLEAR gv_ucomm.
      FREE: t_chaves_nf.

* set edit enabled cells ready for input
      CALL METHOD g_grid_chaves->set_ready_for_input
        EXPORTING
          i_ready_for_input = 0.

    ENDIF.

    IF vg_transf_fornec IS NOT INITIAL.

      IF g_grid_notas->is_ready_for_input( ) EQ 0.
* set edit enabled cells ready for input
        CALL METHOD g_grid_notas->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.
      ELSE.

        CLEAR gv_ucomm.
        FREE: t_sub_notas.

* set edit enabled cells ready for input
        CALL METHOD g_grid_notas->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.

      ENDIF.

    ENDIF.

    zcl_carga_entrada_insumos=>bloqueio_desbloqueio_carga(
    EXPORTING
      i_nro_cg = wa_tela_monta_carga-nro_cg
      bloqueio = abap_true
      IMPORTING
        msg = lv_msg ).

    g_grid_chaves->refresh_table_display( ).
    g_grid_notas->refresh_table_display( ).

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_elimina_linhas_nota
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_elimina_linhas_nota .

    DATA: lw_sel_hide   TYPE slis_sel_hide_alv,
          lt_sel_rows   TYPE lvc_t_row,
          lw_sel_rows   TYPE lvc_s_row,
          lt_zmmt0196   TYPE TABLE OF zmmt0196,
          lv_seq        TYPE zmmt0196-seq,
          lv_nro_sol    TYPE zmmt0196-nro_sol,
          lv_msg        TYPE string,
          lr_chaves_del TYPE RANGE OF zmmt0203-chave_nfe,
          lt_chaves     TYPE zmmt_chaves_nota.

    REFRESH it_values.

    CALL METHOD g_grid_chaves->get_selected_rows
      IMPORTING
        et_index_rows = lt_sel_rows.

    IF lt_sel_rows IS NOT INITIAL.

      LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>).
        READ TABLE t_chaves_nf ASSIGNING FIELD-SYMBOL(<fs_chaves_nf>) INDEX <fs_sel_rows>-index.
        IF sy-subrc IS INITIAL.

          APPEND INITIAL LINE TO t_chaves_del ASSIGNING FIELD-SYMBOL(<fs_chaves>).
          <fs_chaves>-chave = <fs_chaves_nf>-chave_nota.

        ENDIF.
      ENDLOOP.

*    zcl_carga_entrada_insumos=>elimina_notas_carga(
*       EXPORTING
*         i_nro_carga     = wa_tela_monta_carga-nro_cg
*         i_chaves        = lt_chaves
*         i_transferencia = abap_false
*         IMPORTING
*           e_msg_erro = DATA(lv_msg_erro) ).
*    IF lv_msg_erro IS NOT INITIAL.
*      MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
*    ELSE.
      IF t_chaves_del IS NOT INITIAL.

*      MESSAGE 'Notas eliminadas com sucesso' TYPE 'S'.

        lr_chaves_del = VALUE #( FOR ls_chaves_del IN t_chaves_del
                                   ( sign = 'I'
                                    option = 'EQ'
                                    low = ls_chaves_del-chave
                                    ) ).

        DELETE t_chaves_nf WHERE chave_nota IN lr_chaves_del.
      ENDIF.

    ELSE.

      MESSAGE 'Favor selecionar pelo menos uma linha!' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

    CALL METHOD g_grid_chaves->refresh_table_display( ).

  ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_elimina_linhas_nota
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_elimina_linhas_nota2 .

    DATA: lw_sel_hide   TYPE slis_sel_hide_alv,
          lt_sel_rows   TYPE lvc_t_row,
          lw_sel_rows   TYPE lvc_s_row,
          lt_zmmt0196   TYPE TABLE OF zmmt0196,
          lv_seq        TYPE zmmt0196-seq,
          lv_nro_sol    TYPE zmmt0196-nro_sol,
          lv_msg        TYPE string,
          lr_chaves_del TYPE RANGE OF zmmt0203-chave_nfe,
          lt_chaves     TYPE zmmt_chaves_nota.

    REFRESH it_values.

    CALL METHOD g_grid_notas->get_selected_rows
      IMPORTING
        et_index_rows = lt_sel_rows.

    IF lt_sel_rows IS NOT INITIAL.

      LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>).
        READ TABLE t_sub_notas ASSIGNING FIELD-SYMBOL(<fs_chaves_nf>) INDEX <fs_sel_rows>-index.
        IF sy-subrc IS INITIAL.

          APPEND INITIAL LINE TO t_chaves_transf_del ASSIGNING FIELD-SYMBOL(<fs_chaves>).

          <fs_chaves>-chave = <fs_chaves_nf>-chave_nota.

          DELETE t_sub_notas WHERE chave_nota = <fs_chaves_nf>-chave_nota.

        ENDIF.
      ENDLOOP.

*    zcl_carga_entrada_insumos=>elimina_notas_carga(
*    EXPORTING
*      i_nro_carga = wa_tela_monta_carga-nro_cg
*      i_chaves = lt_chaves
*      i_transferencia = abap_true
*      IMPORTING
*        e_msg_erro = DATA(lv_msg_erro) ).
*    IF lv_msg_erro IS NOT INITIAL.
*      MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
*    ELSE.
*      MESSAGE 'Notas eliminadas com sucesso' TYPE 'S'.
*    ENDIF.

    ELSE.

      MESSAGE 'Favor selecionar pelo menos uma linha!' TYPE 'S' DISPLAY LIKE 'E'.

    ENDIF.

    CALL METHOD g_grid_notas->refresh_table_display( ).

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_limpa_variaveis
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_limpa_variaveis .

    FREE: t_0196,
          t_0202,
          t_ekpo,
          t_0200,
          t_lfa1,
          t_0132,
          t_monta_carga,
          t_lista_cargas    ,
          t_alv_lista_cargas       ,
          t_0203                    ,
          t_dist_ter                ,
          t_nfe_itm                 ,
          t_doc                     ,
          t_sub_itens          ,
          t_bdcdata,
          t_sub_notas               ,
          t_chaves_nf               .

    CLEAR:
          ref1  ,
          ref2   ,
          init    ,
          init2    ,
          wa_tela_monta_carga,
          g_grid              ,
          g_grid_chaves        ,
          g_grid_notas          ,
          g_custom_container     ,
          g_custom_container_chaves,
          g_custom_container_notas  ,
          manager                   ,
          obj                       ,
          ip_mode                   ,
          objtype                   ,
          wl_stable                 ,
          vg_transf_fornec          ,
          gv_spart                  ,
          gv_edit_log,
          wa_tela_monta_carga,
          gv_ucomm.


  ENDFORM.

*** Fim - Rubenilson Pereira - 22.04.25 #169642

  FORM f_dispara_email_aut_forn  USING  p_nro_cg      TYPE zmmt0201-nro_cg
                                        p_pdf_xstring TYPE xstring.

    DATA: lo_send_request TYPE REF TO cl_bcs.
    DATA: lo_document TYPE REF TO cl_document_bcs.
    DATA: lo_sender TYPE REF TO if_sender_bcs.
    DATA: lo_recipient   TYPE REF TO if_recipient_bcs,
          lt_solix       TYPE solix_tab,
          lv_sent_to_all TYPE os_boolean,
          lv_pdf_size    TYPE so_obj_len,
          lv_titulo      TYPE so_obj_des.

    DATA: it_text TYPE bcsy_text. "Internal table for email body

    DATA: lv_ponto_coleta TYPE lifnr.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_tela_monta_carga-ponto_coleta
      IMPORTING
        output = lv_ponto_coleta.

    IF sy-sysid = 'PRD'.

      SELECT SINGLE b~smtp_addr
        FROM but020 AS a
        INNER JOIN adr6 AS b
        ON b~addrnumber = a~addrnumber
        INTO @DATA(lv_email)
        WHERE partner = @lv_ponto_coleta.

    ELSE.

      lv_email = 'suporte.sap@amaggi.com.br'.

    ENDIF.

    " ... (Code to populate sender, recipient, subject, and email body in it_text)

    TRY.
        lo_send_request = cl_bcs=>create_persistent( ).

        lv_pdf_size = xstrlen( p_pdf_xstring ).

        lt_solix = cl_bcs_convert=>xstring_to_solix( iv_xstring = p_pdf_xstring ).

        CONCATENATE 'Autorização de embarque da carga' p_nro_cg INTO lv_titulo SEPARATED BY space.

        " Add the attachment (replace with your actual attachment logic)
        lo_document = cl_document_bcs=>create_document(
                  i_type    = 'PDF'
                  i_hex     = lt_solix
                  i_length  = lv_pdf_size
                  i_subject = lv_titulo ).                  "#EC NOTEXT


        lo_send_request->set_document( lo_document ).

        lo_recipient = cl_cam_address_bcs=>create_internet_address(
        i_address_string = lv_email ).

        lo_send_request->add_recipient( i_recipient = lo_recipient ).

        lv_sent_to_all = lo_send_request->send(
            i_with_error_screen = 'X' ).

        COMMIT WORK.

      CATCH cx_bcs INTO DATA(lo_bcs_exception).

        MESSAGE lo_bcs_exception->get_text( ) TYPE 'S' DISPLAY LIKE 'E'.

    ENDTRY.
  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_define_itens_auto
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_define_itens_auto .

    DATA: lva_item_carga TYPE zmmt0202-item_carga.

    zcl_carga_entrada_insumos=>definir_itens_carga(
    EXPORTING i_nro_carga       = wa_tela_monta_carga-nro_cg
              i_nro_solicitacao = wa_tela_monta_carga-nro_sol
              i_tp_saldo        = 'G' "Carga
    IMPORTING
      i_itens_carga = DATA(lt_itens) ).

    CHECK lt_itens IS NOT INITIAL.

    DATA(lt_itens_aux) = lt_itens.
    SORT lt_itens_aux BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_itens_aux COMPARING matnr.

    SELECT *
      FROM makt
      INTO TABLE @DATA(lt_makt)
      FOR ALL ENTRIES IN @lt_itens_aux
      WHERE matnr = @lt_itens_aux-matnr
        AND spras = @sy-langu.
    IF sy-subrc IS INITIAL.
      SORT lt_makt BY matnr.
    ENDIF.

    lt_itens_aux = lt_itens.
    SORT lt_itens_aux BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM lt_itens_aux COMPARING ebeln ebelp.

    SELECT ebeln,ebelp,meins
      FROM ekpo
      INTO TABLE @DATA(lt_ekpo)
      FOR ALL ENTRIES IN @lt_itens_aux
      WHERE ebeln = @lt_itens_aux-ebeln
        AND ebelp = @lt_itens_aux-ebelp.
    IF sy-subrc IS INITIAL.
      SORT lt_ekpo BY ebeln ebelp.
    ENDIF.

    SELECT nro_sol,seq,parceiro_pc, parceiro_le,solicitacao_qte
      FROM zmmt0196
      INTO TABLE @DATA(lt_0196)
      FOR ALL ENTRIES IN @lt_itens_aux
      WHERE nro_sol = @lt_itens_aux-nro_sol.
    IF sy-subrc IS INITIAL.

      SORT lt_0196 BY nro_sol seq.

      SELECT lifnr,name1
        FROM lfa1
        INTO TABLE @DATA(lt_lfa1)
        FOR ALL ENTRIES IN @lt_0196
        WHERE lifnr = @lt_0196-parceiro_pc
          OR  lifnr = @lt_0196-parceiro_le.
      IF sy-subrc IS INITIAL.
        SORT lt_lfa1 BY lifnr.
      ENDIF.

      SELECT nro_sol, seq, qtd_vinc_carga
        FROM zmmt0202
        INTO TABLE @DATA(lt_0202)
        FOR ALL ENTRIES IN @lt_0196
        WHERE nro_sol = @lt_0196-nro_sol
         AND  seq = @lt_0196-seq.
      IF sy-subrc IS INITIAL.
        SORT lt_0202 BY nro_sol seq.
      ENDIF.

    ENDIF.

    SELECT MAX( item_carga )
      FROM zmmt0202 INTO @lva_item_carga
     WHERE nro_cg = @wa_tela_monta_carga-nro_cg.

    DATA(t_sub_itens_aux)  = t_sub_itens[].
    SORT t_sub_itens_aux BY item_carga DESCENDING.
    LOOP AT t_sub_itens_aux INTO DATA(lwa_sub_item) WHERE item_carga > lva_item_carga.
      IF lwa_sub_item-item_carga > lva_item_carga.
        lva_item_carga = lwa_sub_item-item_carga.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_itens ASSIGNING FIELD-SYMBOL(<fs_itens>).

      APPEND INITIAL LINE TO t_sub_itens ASSIGNING FIELD-SYMBOL(<fs_sub_itens>).

      ADD 1 TO lva_item_carga.

      <fs_sub_itens>-item_carga     = lva_item_carga.
      <fs_sub_itens>-nro_sol        = <fs_itens>-nro_sol.
      <fs_sub_itens>-item_solic     = <fs_itens>-seq.
      <fs_sub_itens>-ebeln          = <fs_itens>-ebeln.
      <fs_sub_itens>-ebelp          = <fs_itens>-ebelp.
      <fs_sub_itens>-tp_saldo_vinc  = <fs_itens>-tp_saldo_vinc.

      READ TABLE t_lista_cargas ASSIGNING FIELD-SYMBOL(<fs_carga>)
      WITH KEY nrocg = wa_tela_monta_carga-nro_cg
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_sub_itens>-bukrs = <fs_carga>-bukrs.
      ENDIF.

      READ TABLE lt_0196 ASSIGNING FIELD-SYMBOL(<fs_0196>)
      WITH KEY nro_sol = <fs_itens>-nro_sol
               seq     = <fs_itens>-seq
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
        WITH KEY lifnr = <fs_0196>-parceiro_pc
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_sub_itens>-ponto_coleta  = <fs_0196>-parceiro_pc.
          <fs_sub_itens>-desc_pc       = <fs_lfa1>-name1.
        ENDIF.

        READ TABLE lt_lfa1 ASSIGNING <fs_lfa1>
        WITH KEY lifnr = <fs_0196>-parceiro_le
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          <fs_sub_itens>-local_entrega  = <fs_0196>-parceiro_le.
          <fs_sub_itens>-desc_le        = <fs_lfa1>-name1.
        ENDIF.

        <fs_sub_itens>-saldo_a_formar = <fs_itens>-qtd_vinc_carga.

*      READ TABLE lt_0202 ASSIGNING FIELD-SYMBOL(<fs_0202>)
*      WITH KEY nro_sol = <fs_0196>-nro_sol
*               seq     = <fs_0196>-seq
*      BINARY SEARCH.
*      IF sy-subrc IS INITIAL.
*        <fs_sub_itens>-saldo_a_formar = <fs_0196>-solicitacao_qte - <fs_0202>-qtd_vinc_carga.
*      ELSE.
*        <fs_sub_itens>-saldo_a_formar = <fs_0196>-solicitacao_qte.
*      ENDIF.

      ENDIF.

      <fs_sub_itens>-matnr = <fs_itens>-matnr.

      READ TABLE lt_makt ASSIGNING FIELD-SYMBOL(<fs_makt>)
      WITH KEY matnr = <fs_itens>-matnr
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_sub_itens>-maktx = <fs_makt>-maktx.
      ENDIF.

      <fs_sub_itens>-qtd_carga    = <fs_itens>-qtd_vinc_carga.
*      <fs_sub_itens>-qtd_carga_kg = <fs_itens>-qtd_vinc_carga_kg. " Rubenilson - 03.07.25 #184395

      READ TABLE lt_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>)
      WITH KEY ebeln = <fs_itens>-ebeln
               ebelp = <fs_itens>-ebelp
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <fs_sub_itens>-unidade = <fs_ekpo>-meins.
      ENDIF.

    ENDLOOP.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wl_stable.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_deleta_linhas
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_deleta_linhas .

    DATA: lt_sel_rows TYPE lvc_t_row,
          lw_sel_rows TYPE lvc_s_row.

    CALL METHOD g_grid->get_selected_rows
      IMPORTING
        et_index_rows = lt_sel_rows.

    DELETE lt_sel_rows WHERE index = '0000000000'.
    IF lt_sel_rows IS NOT INITIAL.

      SORT lt_sel_rows BY index DESCENDING.

      LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>).
        READ TABLE t_sub_itens ASSIGNING FIELD-SYMBOL(<fs_itens>) INDEX <fs_sel_rows>-index.
        IF sy-subrc IS INITIAL.
          APPEND INITIAL LINE TO t_sub_itens_del ASSIGNING FIELD-SYMBOL(<fs_itens_del>).
          <fs_itens_del> = <fs_itens>.
          <fs_itens_del>-cancel = abap_true.
          DELETE t_sub_itens INDEX <fs_sel_rows>-index.
        ENDIF.
      ENDLOOP.

      g_grid->refresh_table_display( ).


    ENDIF.
  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_listar_carga
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_refresh_listar_carga .

    PERFORM f_limpa_variaveis.

    zcl_carga_entrada_insumos=>busca_dados_carga(
          EXPORTING
            i_carga = e_carga[]
            i_empresa = e_bukrs[]
            i_segmento = gv_spart
            i_id_viagem = e_idvgm[]
            i_dt_carga  = e_dtcar[]
           IMPORTING
             e_cds_carga = t_lista_cargas
             e_cargas = t_alv_lista_cargas
             e_notas  = t_0203
             e_msg_erro = lv_msg_erro ).

    IF lv_msg_erro IS NOT INITIAL.
      MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Module MONTA_DADOS_BORDERO OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
  MODULE monta_dados_bordero OUTPUT.

    FREE: lt_fieldcat,
          lt_0205,
          lt_0206.

    IF t_chaves_nf IS NOT INITIAL.

      SELECT *
        FROM zmmt0205
        INTO TABLE lt_0205
        FOR ALL ENTRIES IN t_chaves_nf
        WHERE chave_nfe = t_chaves_nf-chave_nota
        AND cancel = space.

    ENDIF.

    IF lo_custom_bordero IS INITIAL.

      PERFORM f_monta_objetos_alv.
      PERFORM f_monta_dados_header_bordero.
      PERFORM f_monta_alv_itens_bordero.

    ELSE.
      lo_bordero_header->refresh_table_display( ).
      lo_bordero_itens->refresh_table_display( ).
    ENDIF.

  ENDMODULE.
*&---------------------------------------------------------------------*
*& Form f_monta_dados_header_bordero
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_monta_dados_header_bordero .

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZMMT0205'
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.
      LOOP AT lt_fieldcat ASSIGNING <fs_fieldcat>.
        IF <fs_fieldcat>-fieldname <> 'CHAVE_NFE' AND
           <fs_fieldcat>-fieldname <> 'TIPO_INSUMO'  AND
           <fs_fieldcat>-fieldname <> 'DATA_CHEGADA' AND
           <fs_fieldcat>-fieldname <> 'DATA_DESCARGA' AND
           <fs_fieldcat>-fieldname <> 'CD_DESCARGA' AND
           <fs_fieldcat>-fieldname <> 'TRANSPORTADORA' AND
           <fs_fieldcat>-fieldname <> 'PLACA' AND
           <fs_fieldcat>-fieldname <> 'RECEB_MIGO_GERADA'.
          <fs_fieldcat>-no_out = abap_true.
        ENDIF.
      ENDLOOP.

      ls_layout_bordero-cwidth_opt = 'X'.
      ls_layout_bordero-grid_title = 'Dados Gerais'.
      ls_layout_bordero-no_rowmark = abap_true.

      CREATE OBJECT lo_bordero_header
        EXPORTING
          i_parent = lo_parent_1.           "ALV borderô

      SET HANDLER handle_event=>handle_double_click FOR lo_bordero_header.

      CALL METHOD lo_bordero_header->set_table_for_first_display
        EXPORTING
          is_layout       = ls_layout_bordero
          i_save          = 'A'
        CHANGING
          it_fieldcatalog = lt_fieldcat
          it_outtab       = lt_0205.
    ENDIF.


  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_objetos_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_monta_objetos_alv .

    CREATE OBJECT lo_custom_bordero
      EXPORTING
        container_name              = 'CONTAINER_BORDERO'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT lo_splitter_1
      EXPORTING
        parent  = lo_custom_bordero
        rows    = 2
        columns = 1.

    CALL METHOD lo_splitter_1->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = lo_parent_1.

    CALL METHOD lo_splitter_1->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = lo_parent_2.

    CALL METHOD lo_splitter_1->set_row_mode
      EXPORTING
        mode = lo_splitter_1->mode_relative.

    CALL METHOD lo_splitter_1->set_row_height
      EXPORTING
        id     = 1
        height = 10.

    CALL METHOD lo_splitter_1->set_row_height
      EXPORTING
        id     = 2
        height = 10.

  ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_dados_itens_bordero
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
  FORM f_monta_alv_itens_bordero .

    FREE: lt_fieldcat.

    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = 'ZMMT0206'
      CHANGING
        ct_fieldcat            = lt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc = 0.
      LOOP AT lt_fieldcat ASSIGNING <fs_fieldcat>.
        IF <fs_fieldcat>-fieldname = 'ID_CHEGADA_CD_LUFT' OR
           <fs_fieldcat>-fieldname = 'ITEM_NFE'  OR
           <fs_fieldcat>-fieldname = 'USER_CREATE' OR
           <fs_fieldcat>-fieldname = 'DATE_CREATE' OR
           <fs_fieldcat>-fieldname = 'TIME_CREATE'.
          <fs_fieldcat>-no_out = abap_true.
        ENDIF.
      ENDLOOP.

      ls_layout_bordero-cwidth_opt = 'X'.
      ls_layout_bordero-grid_title = 'Itens'.
      ls_layout_bordero-no_rowmark = abap_true.

      CREATE OBJECT lo_bordero_itens
        EXPORTING
          i_parent = lo_parent_2.           "ALV borderô

      CALL METHOD lo_bordero_itens->set_table_for_first_display
        EXPORTING
          is_layout       = ls_layout_bordero
          i_save          = 'A'
        CHANGING
          it_fieldcatalog = lt_fieldcat
          it_outtab       = lt_0206.
    ENDIF.

  ENDFORM.

  FORM f_pdf_nfe_venda_5900 .

    DATA: lw_sel_hide TYPE slis_sel_hide_alv,
          lt_sel_rows TYPE lvc_t_row,
          lw_sel_rows TYPE lvc_s_row.

    CALL METHOD g_grid_chaves->get_selected_rows
      IMPORTING
        et_index_rows = lt_sel_rows.

    CHECK lt_sel_rows[] IS NOT INITIAL.

    IF lines( lt_sel_rows[] ) > 1 .
      MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
      RETURN.
    ENDIF.

    READ TABLE lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>) INDEX 1.
    CHECK sy-subrc EQ 0.

    READ TABLE t_chaves_nf ASSIGNING FIELD-SYMBOL(<fs_chaves_nf>) INDEX <fs_sel_rows>-index.
    CHECK sy-subrc EQ 0.

    IF wa_tela_monta_carga-viagem_id IS INITIAL.
      MESSAGE 'Carga não integrada ao Carguero/Strada!' TYPE 'S'.
      RETURN.
    ENDIF.

    PERFORM f_imprimir_nfe_viagem USING <fs_chaves_nf>-chave_nota wa_tela_monta_carga-viagem_id.

  ENDFORM.

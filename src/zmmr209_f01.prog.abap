*&---------------------------------------------------------------------*
*& Include          ZMMR209_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA: lw_0201_sum TYPE ty_0201_sum.

  LOOP AT s_bukrs.
    AUTHORITY-CHECK OBJECT 'ZMM0235'
    ID 'EKORG' FIELD s_bukrs-low.
    IF sy-subrc IS NOT INITIAL.
      MESSAGE 'Usuário sem permissão para empresa: ' && 'S_BUKRS-LOW' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.
  ENDLOOP.

  CASE abap_true.
    WHEN p_asol. "A solicitar
      PERFORM f_seleciona_dados_a_solicitar.
    WHEN p_jsol. "Solicitados
      PERFORM f_seleciona_dados_solicitadas.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_dados .

  DATA: lw_celltab TYPE lvc_s_styl,
        lt_celltab TYPE lvc_t_styl,
        lw_saida   TYPE zmme_alv_solic_receb_compras,
        lv_qtd_sol TYPE zmmt0196-solicitacao_qte,
        lv_tot_201 TYPE zmmt0201-qtd_total_kg.

  CASE abap_true.
    WHEN p_asol. "A solicitar
      PERFORM f_monta_dados_a_solicitar.
    WHEN p_jsol. "Solicitados
      PERFORM f_monta_dados_solicitadas.
  ENDCASE.

  IF t_saida IS INITIAL.
    MESSAGE 'Não há dados para serem exibidos!' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_dados .

  DATA: lw_layout   TYPE lvc_s_layo,
        lt_fieldcat TYPE lvc_t_fcat,
        lt_events   TYPE slis_t_event,
        lv_alv      TYPE c,
        ls_title    TYPE lvc_title.

  IF p_jsol IS INITIAL.
    ls_title = 'Solicitações de Recebimento Pedido Compras - Solicitar'.
  ELSE.
    ls_title = 'Solicitações de Recebimento Pedido Compras - Já Solicitados'.
  ENDIF.

  lv_alv = '1'.
  lv_direcao = '1'.

  PERFORM f_monta_fieldcat USING lv_alv
                        CHANGING lt_fieldcat.

  lw_layout-stylefname = 'CELLTAB'.

  APPEND INITIAL LINE TO lt_events ASSIGNING FIELD-SYMBOL(<fs_events>).

  <fs_events>-form = 'XPF_STATUS_SET'.
  <fs_events>-name = slis_ev_pf_status_set.

  APPEND INITIAL LINE TO lt_events ASSIGNING <fs_events>.

  <fs_events>-form = 'XUSER_COMMAND'.
  <fs_events>-name = slis_ev_user_command.

  lw_layout-cwidth_opt = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = lw_layout
      it_fieldcat_lvc    = lt_fieldcat
      it_events          = lt_events
      i_grid_title       = ls_title
    TABLES
      t_outtab           = t_saida
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

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

  FREE: t_saida,
        t_dados,
        "t_zmmt0196,
        t_ekpa,
        t_observacoes,
        lv_direcao.

ENDFORM.


FORM xpf_status_set USING ucomm TYPE kkblo_t_extab.         "#EC CALLED
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
                 gr_events->handle_on_button_click FOR ref1,
                 gr_events->on_data_changed FOR ref1,
                 gr_events->on_f4 FOR ref1.

    gs_f4-fieldname = 'ROTA_LE'.
    gs_f4-register = 'X'.
    gs_f4-getbefore = 'X'.
    gs_f4-chngeafter = 'X'.
    INSERT gs_f4 INTO TABLE gt_f4.

    gs_f4-fieldname = 'ROTA_PC'.
    gs_f4-register = 'X'.
    gs_f4-getbefore = 'X'.
    gs_f4-chngeafter = 'X'.
    INSERT gs_f4 INTO TABLE gt_f4.

    CALL METHOD ref1->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4.

    init  = 'X'.
  ENDIF.

  IF p_jsol IS NOT INITIAL.

    APPEND INITIAL LINE TO tl_fcode ASSIGNING FIELD-SYMBOL(<fs_fcode>).
    <fs_fcode> = '&DATA_SAVE'.
    APPEND INITIAL LINE TO tl_fcode ASSIGNING <fs_fcode>.
    <fs_fcode> = '&REFRESH'.

  ENDIF.

  SET PF-STATUS 'STANDARD_FULLSCREEN' EXCLUDING tl_fcode.
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

  CASE ucomm.
    WHEN 'BACK1' OR 'EXIT' OR 'CANCEL'.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.

      PERFORM f_refresh.
      selfield-refresh = 'X'.

    WHEN '&DATA_SAVE'.

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
      ENDIF.

      DATA(_sucesso) = abap_false.
      PERFORM f_criar_solicitacao USING lt_sel_rows
                                        t_saida
                                        t_dados
                               CHANGING lt_zmmt0196
                                        lv_nro_sol
                                        _sucesso.

      IF _sucesso EQ abap_true.
        PERFORM f_refresh.
        selfield-refresh = 'X'.
      ENDIF.

  ENDCASE.

  CASE selfield-fieldname.
    WHEN 'NRO_SOL'.
      CHECK p_jsol IS NOT INITIAL.
      PERFORM f_alv_edita_solicitacao USING selfield.

    WHEN 'STATUS_CARGUERO'.

      CHECK p_jsol IS NOT INITIAL.

      CHECK selfield-refresh IS INITIAL.

      CASE selfield-value(4).
        WHEN '@09@' OR '@0A@'.
        WHEN OTHERS.
          RETURN.
      ENDCASE.

      READ TABLE t_saida INTO DATA(wa_saida) INDEX selfield-tabindex.
      CHECK sy-subrc IS INITIAL.

      zcl_solicitacao_entrada_insumo=>integrar_carguero( i_nro_sol = wa_saida-nro_sol ).

      PERFORM f_refresh.
      selfield-refresh = 'X'.

      CLEAR selfield.

    WHEN OTHERS.
  ENDCASE.

  FREE: selfield, ucomm.

ENDFORM. "XUSER_COMMAND


FORM xpf_status_set2 USING ucomm TYPE kkblo_t_extab.        "#EC CALLED
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
                 gr_events->handle_on_button_click2 FOR ref2,
                 gr_events->on_data_changed2 FOR ref2,
                 gr_events->on_f4_2 FOR ref2.

    gs_f4-fieldname = 'ROTA_LE'.
    gs_f4-register = 'X'.
    gs_f4-getbefore = 'X'.
    gs_f4-chngeafter = 'X'.
    INSERT gs_f4 INTO TABLE gt_f4.

    gs_f4-fieldname = 'ROTA_PC'.
    gs_f4-register = 'X'.
    gs_f4-getbefore = 'X'.
    gs_f4-chngeafter = 'X'.
    INSERT gs_f4 INTO TABLE gt_f4.

    CALL METHOD ref2->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4.

    init2  = 'X'.

  ENDIF.

  SET PF-STATUS 'STANDARD_FULLSCREEN2' EXCLUDING tl_fcode.
ENDFORM.


FORM xuser_command2 USING ucomm    LIKE sy-ucomm
                         selfield TYPE kkblo_selfield.

  DATA: tl_0082        TYPE TABLE OF zsdt0082,
        wl_0082        TYPE zsdt0082,
        lv_number      TYPE zde_nro_sol,
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
        wa_zsdt0132    TYPE zsdt0132,
        lw_celltab     TYPE lvc_s_styl,
        lt_celltab     TYPE lvc_t_styl.

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
        lv_nro_sol  TYPE zmmt0196-nro_sol.

  REFRESH it_values.

  CASE ucomm.
    WHEN 'BACK1' OR 'EXIT' OR 'CANCEL'.

      READ TABLE t_saida_edit_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saida3>) INDEX 1.
      IF sy-subrc IS INITIAL.
        CALL METHOD zcl_solicitacao_entrada_insumo=>desbloqueia_solicitacao
          EXPORTING
            nro_solicitacao = <fs_saida3>-nro_sol
          IMPORTING
            desbloqueado    = DATA(lv_desbloqueado).
      ENDIF.

      LEAVE TO SCREEN 0.
    WHEN '&REFRESH'.

    WHEN 'VINC_PED'.

      PERFORM f_alv_pedidos.
      selfield-refresh = 'X'.

    WHEN '&DATA_SAVE'.
      DATA(lt_0196) = t_zmmt0196.
      SORT lt_0196 BY ebeln ebelp.

      DATA(_editou) = abap_false.
      PERFORM f_editar_solicitacao USING t_saida_edit_solicitacao
                                CHANGING _editou.
      IF _editou EQ abap_true.
        DELETE t_saida_edit_solicitacao WHERE solicitacao_qte <= 0.

        PERFORM f_refresh.
        selfield-refresh = 'X'.
      ENDIF.

    WHEN 'CANCELAR'.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = lw_sel_hide
          e_grid      = ref2.

      DATA(_cancelou) = abap_false.
      PERFORM f_cancelar_solicitacao CHANGING _cancelou.

      IF _cancelou EQ abap_true.
        PERFORM f_refresh.
        selfield-refresh = 'X'.
      ENDIF.

    WHEN '&F03' OR '&F15' OR '&F12'.

      READ TABLE t_saida_edit_solicitacao ASSIGNING <fs_saida3> INDEX 1.
      IF sy-subrc IS INITIAL.
        CALL METHOD zcl_solicitacao_entrada_insumo=>desbloqueia_solicitacao
          EXPORTING
            nro_solicitacao = <fs_saida3>-nro_sol
          IMPORTING
            desbloqueado    = lv_desbloqueado.
      ENDIF.

  ENDCASE.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_refresh .
  PERFORM f_limpa_variaveis.
  PERFORM f_seleciona_dados.
  PERFORM f_monta_dados.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_trata_exibicao_campos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_trata_exibicao_campos .

  IF p_asol IS NOT INITIAL.
    PERFORM f_limpa_variaveis.

    LOOP AT SCREEN.
      IF screen-name CS 'NRSOL' OR
         screen-name CS 'DTSOL'.
        screen-invisible = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ELSEIF p_jsol IS NOT INITIAL.

    PERFORM f_limpa_variaveis.

    LOOP AT SCREEN.
      IF screen-name NS 'NRSOL' AND
         screen-name NS 'DTSOL' AND
         screen-name NS 'BUKRS' AND
         screen-name NS 'WERKS' AND
         screen-name NS 'EBELN' AND
         screen-name NS 'EBELP' AND
         screen-name NS 'ASOL' AND
         screen-name NS 'JSOL' AND
         screen-name NS 'BLOCK' AND
         screen-name NS 'BSART'.
        screen-invisible = '1'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_valida_obrigatoriedade
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_valida_obrigatoriedade .

  IF s_bukrs IS INITIAL.
    MESSAGE 'Empresa é um campo obrigatório' TYPE 'S' DISPLAY LIKE 'E'.
    gv_obrig = abap_true.
  ENDIF.

  IF p_asol IS NOT INITIAL AND
     ( s_bukrs IS INITIAL OR
       s_werks IS INITIAL OR
       s_bsart IS INITIAL OR
       s_erdat IS INITIAL OR
       s_inco1 IS INITIAL ) AND s_ebeln IS INITIAL.
    MESSAGE 'Campos: Empresa, Centro, Tipo Compra, Data e Incoterms são obrigatórios' TYPE 'S' DISPLAY LIKE 'E'.
    gv_obrig = abap_true.
  ENDIF.

  IF p_jsol IS NOT INITIAL AND
     ( s_bukrs IS INITIAL OR
       s_werks IS INITIAL OR
       s_dtsol IS INITIAL ) AND s_ebeln IS INITIAL AND s_nrsol IS INITIAL.
    MESSAGE 'Campos: Empresa, Centro, Data são obrigatórios' TYPE 'S' DISPLAY LIKE 'E'.
    gv_obrig = abap_true.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_items_solicitacao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_alv_edita_solicitacao USING selfield TYPE kkblo_selfield.

  DATA: lw_layout     TYPE lvc_s_layo,
        lt_fieldcat   TYPE lvc_t_fcat,
        lt_events     TYPE slis_t_event,
        lv_alv        TYPE c,
        lo_solic      TYPE REF TO zcl_solicitacao_entrada_insumo,
        lv_bloq       TYPE c,
        lv_msg        TYPE string,
        ls_event_exit TYPE slis_event_exit,
        lt_event_exit TYPE slis_t_event_exit,
        ls_title      TYPE lvc_title.

  CLEAR: init2.

  lv_direcao = '2'.

  READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX selfield-tabindex.
  CHECK sy-subrc IS INITIAL.

  ls_title = |Edição Solicitação { <fs_saida>-nro_sol } |.

  CALL METHOD zcl_solicitacao_entrada_insumo=>bloqueia_solicitacao(
    EXPORTING
      nro_solicitacao = <fs_saida>-nro_sol
    IMPORTING
      bloqueado       = lv_bloq
      mensagem_erro   = lv_msg ).

  IF lv_msg IS NOT INITIAL.

    MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

  t_saida_edit_solicitacao = t_saida_sol_item.
  DELETE t_saida_edit_solicitacao WHERE nro_sol <> <fs_saida>-nro_sol.

  LOOP AT t_saida_edit_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saida_edit_sol>).

    SELECT SUM( solicitacao_qte )
      FROM zmmt0196 INTO @DATA(lv_solicitacao_qte)
     WHERE ebeln   EQ @<fs_saida_edit_sol>-ebeln
       AND ebelp   EQ @<fs_saida_edit_sol>-ebelp
       AND NOT ( nro_sol EQ @<fs_saida_edit_sol>-nro_sol AND
                 seq     EQ @<fs_saida_edit_sol>-seq )
       AND cancel  EQ @abap_false.

    <fs_saida_edit_sol>-qtd_solicitada    = lv_solicitacao_qte.
    <fs_saida_edit_sol>-saldo_a_solicitar = <fs_saida_edit_sol>-menge - <fs_saida_edit_sol>-qtd_solicitada.

    LOOP AT <fs_saida_edit_sol>-celltab ASSIGNING FIELD-SYMBOL(<fs_celltab>) WHERE fieldname EQ 'ENTREGA_DT'.
      <fs_celltab>-style = '00080000'.
    ENDLOOP.

    LOOP AT <fs_saida_edit_sol>-celltab ASSIGNING <fs_celltab> WHERE fieldname EQ 'SOLICITACAO_QTE' OR
                                                                     fieldname EQ 'SOLICITACAO_QTE_MANUAL'.
      <fs_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
    ENDLOOP.

  ENDLOOP.

  lv_alv = '2'.

  PERFORM f_monta_fieldcat USING lv_alv
                        CHANGING lt_fieldcat.

  lw_layout-stylefname = 'CELLTAB'.

  APPEND INITIAL LINE TO lt_events ASSIGNING FIELD-SYMBOL(<fs_events>).

  <fs_events>-form = 'XPF_STATUS_SET2'.
  <fs_events>-name = slis_ev_pf_status_set.

  APPEND INITIAL LINE TO lt_events ASSIGNING <fs_events>.

  <fs_events>-form = 'XUSER_COMMAND2'.
  <fs_events>-name = slis_ev_user_command.

  APPEND INITIAL LINE TO lt_event_exit ASSIGNING FIELD-SYMBOL(<fs_event_exit>).

  <fs_event_exit>-ucomm = '&F03'.
  <fs_event_exit>-after = abap_true.


  APPEND INITIAL LINE TO lt_event_exit ASSIGNING <fs_event_exit>.

  <fs_event_exit>-ucomm = '&F12'.
  <fs_event_exit>-after = abap_true.


  APPEND INITIAL LINE TO lt_event_exit ASSIGNING <fs_event_exit>.

  <fs_event_exit>-ucomm = '&F15'.
  <fs_event_exit>-after = abap_true.

  lw_layout-cwidth_opt = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = lw_layout
      it_fieldcat_lvc    = lt_fieldcat
      it_events          = lt_events
      it_event_exit      = lt_event_exit
      i_grid_title       = ls_title
    TABLES
      t_outtab           = t_saida_edit_solicitacao
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
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat USING p_alv TYPE c
                   CHANGING ch_fieldcat TYPE lvc_t_fcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMME_ALV_SOLIC_RECEB_COMPRAS'
    CHANGING
      ct_fieldcat            = ch_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc = 0.

    IF p_alv = '1'.

      IF p_asol IS NOT INITIAL.

        LOOP AT ch_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

          CASE <fs_fieldcat>-fieldname.
            WHEN 'SOLICITACAO_QTE'        OR
                 'ENTREGA_DT'             OR
                 'SOLICITACAO_QTE_MANUAL' OR
                 'PRIORIDADE_ITEM'.

              <fs_fieldcat>-edit = abap_true.

            WHEN 'PARCEIRO_LE'     OR
                 'PARCEIRO_PC'     .

              <fs_fieldcat>-edit = abap_true.
              <fs_fieldcat>-outputlen = 20.

            WHEN 'ROTA_LE'     OR
                 'ROTA_PC'     .

              <fs_fieldcat>-outputlen = 20.
              <fs_fieldcat>-edit = abap_true.

            WHEN 'OBSERVACAO'.
              <fs_fieldcat>-style = cl_gui_alv_grid=>mc_style_button.
              <fs_fieldcat>-edit = abap_true.

            WHEN 'NRO_SOL' OR 'PRIORIDADE_SOL' OR 'STATUS_CARGUERO'
              OR 'MATKL' OR 'WAERS' OR 'QTD_FATURADO' OR 'OBSERVACAO'
              OR 'SALDO_FAT_SOLIC' OR 'SALDO_ENTREGUE_SOLIC'
              OR 'PRIORIDADE_ITEM' OR 'CELLTAB' OR 'MARK' OR 'DATA' OR 'USUARIO'.

              <fs_fieldcat>-no_out = abap_true.

            WHEN 'TRANSF_NO_FORNECEDOR'.
              <fs_fieldcat>-edit = abap_true.
              <fs_fieldcat>-checkbox = abap_true.
              <fs_fieldcat>-outputlen = 15.
            WHEN OTHERS.

          ENDCASE.

        ENDLOOP.

        PERFORM f_ordena_grid USING 1 CHANGING ch_fieldcat.

      ELSE.

        LOOP AT ch_fieldcat ASSIGNING <fs_fieldcat>.

          CASE <fs_fieldcat>-fieldname.
            WHEN 'UNSEZ' OR 'MENGE' OR
                 'MEINS' OR 'WAERS' OR 'QTD_FATURADO' OR 'SALDO_FAT_PED' OR
                 'SALDO_FAT_SOLIC' OR 'PRIORIDADE_ITEM' OR 'MARK' OR 'SALDO_A_SOLICITAR' OR 'QTD_SOLICITADA'.

              <fs_fieldcat>-no_out = abap_true.

            WHEN 'NRO_SOL'.

              <fs_fieldcat>-hotspot = abap_true.

            WHEN 'STATUS_CARGUERO'.
              <fs_fieldcat>-icon = abap_true.
              <fs_fieldcat>-hotspot = abap_true.

            WHEN 'TRANSF_NO_FORNECEDOR'.

              <fs_fieldcat>-checkbox = abap_true.
              <fs_fieldcat>-outputlen = 15.

            WHEN 'OBSERVACAO'.
              <fs_fieldcat>-style = cl_gui_alv_grid=>mc_style_button.
            WHEN OTHERS.

          ENDCASE.

        ENDLOOP.

        PERFORM f_ordena_grid USING 2 CHANGING ch_fieldcat.

      ENDIF.

    ELSEIF p_alv = '2'.

      LOOP AT ch_fieldcat ASSIGNING <fs_fieldcat>.

        CASE <fs_fieldcat>-fieldname.
*          WHEN 'UNSEZ' OR 'MENGE' OR
*               'MEINS' OR
*            'WAERS' OR 'QTD_FATURADO' OR 'SALDO_FAT_PED' OR
*               'SALDO_FAT_SOLIC' OR 'PERCENTUAL' OR 'PRIORIDADE_SOL' OR 'QTD_SOLICITADA' OR
*               'STATUS_CARGUERO'  OR 'MARK' OR 'SALDO_A_SOLICITAR'.

*            <FS_FIELDCAT>-NO_OUT = ABAP_TRUE.

          WHEN 'PRIORIDADE_ITEM' OR
               'SOLICITACAO_QTE' OR
               'SOLICITACAO_QTE_MANUAL' OR
               'ENTREGA_DT'.

            <fs_fieldcat>-edit = abap_true.

          WHEN
             'OBSERVACAO'.

            <fs_fieldcat>-edit = abap_true.
            <fs_fieldcat>-style = cl_gui_alv_grid=>mc_style_button.

          WHEN 'TRANSF_NO_FORNECEDOR'.

            <fs_fieldcat>-edit = abap_true.
            <fs_fieldcat>-checkbox = abap_true.
            <fs_fieldcat>-outputlen = 15.

          WHEN OTHERS.

        ENDCASE.

      ENDLOOP.

      PERFORM f_ordena_grid USING 3 CHANGING ch_fieldcat.

    ENDIF.

  ENDIF.

  DELETE ch_fieldcat WHERE no_out EQ abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_pedidos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_alv_pedidos .

  DATA: ls_title    TYPE lvc_title.
  TYPES: BEGIN OF ty_saida_ped,
           sel   TYPE flag,
           ebeln TYPE ekpo-ebeln,
           ebelp TYPE ekpo-ebelp,
           lifnr TYPE lfa1-lifnr,
           name1 TYPE lfa1-name1,
           matnr TYPE mara-matnr,
           maktx TYPE makt-maktx,
         END OF ty_saida_ped.

  DATA: lw_layout   TYPE lvc_s_layo,
        lt_fieldcat TYPE lvc_t_fcat,
        lt_events   TYPE slis_t_event,
        lv_alv      TYPE c,
        lo_solic    TYPE REF TO zcl_solicitacao_entrada_insumo,
        lv_bloq     TYPE c,
        lv_msg      TYPE string,
        ls_selfield TYPE slis_selfield,
        t_saida_ped TYPE TABLE OF ty_saida_ped,
        lv_item     TYPE zmmt0196-prioridade_item,
        lt_celltab  TYPE lvc_t_styl,
        lw_celltab  TYPE lvc_s_styl.

  READ TABLE t_saida_edit_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saida2>) INDEX 1.
  IF sy-subrc IS INITIAL.

    SELECT MAX( prioridade_item )
      FROM zmmt0196
      INTO @DATA(lv_max_prioridade)
      WHERE nro_sol = @<fs_saida2>-nro_sol.

    READ TABLE t_dados ASSIGNING FIELD-SYMBOL(<fs_dados>)
    WITH KEY ebeln = <fs_saida2>-ebeln
             ebelp = <fs_saida2>-ebelp
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      SELECT *
        FROM zcds_solic_receb_pedidos_comp
        INTO TABLE @t_dados2
        WHERE bukrs = @<fs_dados>-bukrs
          AND werks = @<fs_dados>-werks
          AND matkl = @<fs_dados>-matkl
          AND inco1 = @<fs_dados>-inco1
          AND NOT ( ebeln EQ @<fs_dados>-ebeln AND ebelp EQ @<fs_dados>-ebelp  ).

      IF sy-subrc IS INITIAL.
        PERFORM f_atrib_saldos_pedido TABLES t_dados2.

        DELETE t_dados2 WHERE saldo_a_solicitar <= 0.
        DELETE t_dados2 WHERE netwr = '0.01'.

        LOOP AT t_dados2 ASSIGNING FIELD-SYMBOL(<fs_dados2>).
          IF <fs_dados2>-menge EQ <fs_dados2>-qtde_fat_pedido.
            CLEAR <fs_dados2>-ebeln.
          ENDIF.
        ENDLOOP.

        DELETE t_dados2 WHERE ebeln IS INITIAL.

        MOVE-CORRESPONDING t_dados2 TO t_saida_ped.

      ENDIF.

    ENDIF.

  ENDIF.

  ls_title = 'Solicitações de Recebimento Pedido Compras - Já Solicitados Edicao'.

  DATA(tl_fieldcat) =
  VALUE slis_t_fieldcat_alv(
    ( fieldname = 'EBELN' seltext_m = 'Pedido'             just = 'X' )
    ( fieldname = 'EBELP' seltext_m = 'Item do Pedido'     just = 'X' )
    ( fieldname = 'LIFNR' seltext_m = 'Fornecedor'         just = 'X' )
    ( fieldname = 'NAME1' seltext_m = 'Nome Fornecedor'    just = 'X' )
    ( fieldname = 'MATNR' seltext_m = 'Material'           just = 'X' )
    ( fieldname = 'MAKTX' seltext_m = 'Descrição Material' just = 'X' )
     ).

  CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
    EXPORTING
      i_title               = ls_title
      i_selection           = 'X'
      i_zebra               = 'X'
      i_tabname             = 'T_SAIDA_PED'
      it_fieldcat           = tl_fieldcat
*     I_GRID_TITLE          = LS_TITLE
      i_checkbox_fieldname  = 'SEL'
      i_screen_start_column = 1
      i_screen_end_column   = 80
      i_screen_start_line   = 1
      i_screen_end_line     = 15
    IMPORTING
      es_selfield           = ls_selfield
    TABLES
      t_outtab              = t_saida_ped
    EXCEPTIONS
      program_error         = 1
      OTHERS                = 2.
  IF sy-subrc = 0.

    SORT t_dados2 BY ebeln ebelp.

    DATA(lt_saida_ped) = t_saida_ped.
    DELETE lt_saida_ped WHERE sel IS INITIAL.

    LOOP AT lt_saida_ped ASSIGNING FIELD-SYMBOL(<fs_saida_ped>).

      APPEND INITIAL LINE TO t_saida_edit_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saida_edit_sol>).

      READ TABLE t_dados2 ASSIGNING <fs_dados>
        WITH KEY ebeln = <fs_saida_ped>-ebeln
                 ebelp = <fs_saida_ped>-ebelp BINARY SEARCH.

      CHECK sy-subrc IS INITIAL.

      MOVE-CORRESPONDING <fs_dados> TO <fs_saida_edit_sol>.

      ADD 1 TO lv_max_prioridade.

      <fs_saida_edit_sol>-prioridade_item = lv_max_prioridade.
      <fs_saida_edit_sol>-observacao      = icon_enter_more.

      READ TABLE t_saida_edit_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saida_edit_sol_index1>) INDEX 1.
      IF sy-subrc IS INITIAL.
        <fs_saida_edit_sol>-rota_le       = <fs_saida_edit_sol_index1>-rota_le.
        <fs_saida_edit_sol>-rota_pc       = <fs_saida_edit_sol_index1>-rota_pc.
        <fs_saida_edit_sol>-parceiro_le   = <fs_saida_edit_sol_index1>-parceiro_le.
        <fs_saida_edit_sol>-parceiro_pc   = <fs_saida_edit_sol_index1>-parceiro_pc.
        <fs_saida_edit_sol>-nro_sol       = <fs_saida_edit_sol_index1>-nro_sol.
        <fs_saida_edit_sol>-entrega_dt    = <fs_saida_edit_sol_index1>-entrega_dt.
        <fs_saida_edit_sol>-inco1         = <fs_saida_edit_sol_index1>-inco1.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_zmmt0196
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_SEL_ROWS
*&      --> T_SAIDA
*&      <-- LT_ZMMT0196
*&---------------------------------------------------------------------*


FORM f_editar_solicitacao USING  p_t_saida TYPE tb_saida
                        CHANGING p_editou TYPE c.

  DATA: lva_qtde_sol TYPE zmmt0196-solicitacao_qte.

  DATA: wl_header   TYPE thead,
        wl_index(3) TYPE n,
        tl_tlines   LIKE tline OCCURS 0 WITH HEADER LINE,
        lv_seq      TYPE zmmt0196-seq,
        lw_celltab  TYPE lvc_s_styl,
        lt_celltab  TYPE lvc_t_styl.

  DATA: lit_zmmt0196 TYPE TABLE OF zmmt0196.
  DATA: lit_zmmt0196_carguero TYPE TABLE OF zmmt0196.

  SORT p_t_saida BY lifnr matnr werks rota_pc rota_le.

  CLEAR: lit_zmmt0196[], p_editou.

  READ TABLE p_t_saida INTO DATA(lwa_saida_tmp) INDEX 1.
  CHECK sy-subrc EQ 0 AND p_t_saida[] IS NOT INITIAL.

  SELECT MAX( seq )
    FROM zmmt0196 INTO lv_seq
   WHERE nro_sol = lwa_saida_tmp-nro_sol.

  ADD 1 TO lv_seq.

  LOOP AT p_t_saida ASSIGNING FIELD-SYMBOL(<fs_saida_edicao>).

    DATA(lv_tabix) = sy-tabix.

    CLEAR: lva_qtde_sol.

    SELECT SUM( solicitacao_qte )
      FROM zmmt0196 INTO lva_qtde_sol
     WHERE ebeln         EQ <fs_saida_edicao>-ebeln
       AND ebelp         EQ <fs_saida_edicao>-ebelp
       AND NOT ( nro_sol EQ <fs_saida_edicao>-nro_sol AND
                 seq     EQ <fs_saida_edicao>-seq )
       AND cancel  EQ abap_false.

    DATA(_qtde_sol) = lva_qtde_sol + <fs_saida_edicao>-solicitacao_qte.

    IF _qtde_sol > <fs_saida_edicao>-menge.
      MESSAGE 'Quantidade total solicitada é maior que a quantidade do pedido' TYPE 'S' DISPLAY LIKE 'E'.
      FREE: lit_zmmt0196 .
      RETURN.
    ENDIF.

    IF <fs_saida_edicao>-prioridade_item > '5'.
      MESSAGE 'Prioridade Item não pode ser maior que 5' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF <fs_saida_edicao>-solicitacao_qte IS INITIAL OR
       <fs_saida_edicao>-solicitacao_qte IS INITIAL OR
       <fs_saida_edicao>-entrega_dt      IS INITIAL OR
       <fs_saida_edicao>-rota_pc         IS INITIAL OR
       <fs_saida_edicao>-rota_le         IS INITIAL OR
       <fs_saida_edicao>-parceiro_pc     IS INITIAL OR
       <fs_saida_edicao>-parceiro_le     IS INITIAL.
      MESSAGE 'Todos campos editáveis são obrigatórios favor preencher!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO lit_zmmt0196 ASSIGNING FIELD-SYMBOL(<fs_zmmt0196>).

    IF <fs_saida_edicao>-seq IS NOT INITIAL. "Solicitação já existe
      SELECT SINGLE  *
        FROM zmmt0196 INTO @DATA(lwa_zmmt0196_exists)
       WHERE nro_sol = @<fs_saida_edicao>-nro_sol
         AND seq     = @<fs_saida_edicao>-seq.

      CHECK sy-subrc EQ 0.

      <fs_zmmt0196> = lwa_zmmt0196_exists.
    ENDIF.

    MOVE-CORRESPONDING <fs_saida_edicao> TO <fs_zmmt0196>.

    <fs_zmmt0196>-solicitacao_qte         = <fs_saida_edicao>-solicitacao_qte.
    <fs_zmmt0196>-entrega_dt              = <fs_saida_edicao>-entrega_dt.
    <fs_zmmt0196>-nro_sol                 = <fs_saida_edicao>-nro_sol.
    <fs_zmmt0196>-inco1                   = <fs_saida_edicao>-inco1.
    <fs_zmmt0196>-rota_pc                 = <fs_saida_edicao>-rota_pc    .
    <fs_zmmt0196>-rota_le                 = <fs_saida_edicao>-rota_le    .
    <fs_zmmt0196>-parceiro_pc             = <fs_saida_edicao>-parceiro_pc.
    <fs_zmmt0196>-parceiro_le             = <fs_saida_edicao>-parceiro_le.
    <fs_zmmt0196>-solicitacao_qte         = <fs_saida_edicao>-solicitacao_qte.
    <fs_zmmt0196>-solicitacao_qte_manual  = <fs_saida_edicao>-solicitacao_qte_manual.
    <fs_zmmt0196>-prioridade_sol          = '03'.
    <fs_zmmt0196>-transf_no_fornecedor    = <fs_zmmt0196>-transf_no_fornecedor.
    <fs_zmmt0196>-prioridade_item         = <fs_saida_edicao>-prioridade_item.
    <fs_zmmt0196>-date_create             = sy-datum.
    <fs_zmmt0196>-time_create             = sy-uzeit.
    <fs_zmmt0196>-user_create             = sy-uname.
    <fs_zmmt0196>-date_change             = sy-datum.
    <fs_zmmt0196>-time_change             = sy-uzeit.
    <fs_zmmt0196>-user_change             = sy-uname.

    IF <fs_zmmt0196>-seq IS INITIAL.
      <fs_zmmt0196>-seq = lv_seq.
      ADD 1 TO lv_seq.
    ENDIF.

  ENDLOOP.

  DATA(_msg_error) = zcl_solicitacao_entrada_insumo=>gravar( EXPORTING i_zmmt0196_t = lit_zmmt0196 ).

  IF _msg_error IS INITIAL.
    p_editou = abap_true.
  ENDIF.

ENDFORM.

FORM f_criar_solicitacao  USING  p_lt_sel_rows TYPE lvc_t_row
                                 p_t_saida TYPE tb_saida
                                 p_dados   TYPE tb_dados
                        CHANGING p_lt_zmmt0196 TYPE tb_zmmt0196
                                 p_number TYPE zde_nro_sol
                                 p_salvou TYPE c.

  DATA: wl_header   TYPE thead,
        wl_index(3) TYPE n,
        tl_tlines   LIKE tline OCCURS 0 WITH HEADER LINE,
        lv_seq      TYPE zmmt0196-seq,
        lw_celltab  TYPE lvc_s_styl,
        lt_celltab  TYPE lvc_t_styl.

  DATA: lt_saida_sel TYPE tb_saida.

  CLEAR: lt_saida_sel[], p_lt_zmmt0196[], p_salvou.

  LOOP AT p_lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>).
    READ TABLE p_t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_sel_rows>-index.
    IF sy-subrc IS INITIAL.
      APPEND <fs_saida> TO lt_saida_sel.
    ENDIF.
  ENDLOOP.

  SORT lt_saida_sel BY matkl werks rota_pc rota_le entrega_dt.

  DATA(lt_saida_group) = lt_saida_sel.
  DELETE ADJACENT DUPLICATES FROM lt_saida_group COMPARING matkl werks rota_pc rota_le entrega_dt.

  LOOP AT lt_saida_group ASSIGNING FIELD-SYMBOL(<fs_saida_group>).

    READ TABLE lt_saida_sel TRANSPORTING NO FIELDS
      WITH KEY matkl      = <fs_saida_group>-matkl
               werks      = <fs_saida_group>-werks
               rota_pc    = <fs_saida_group>-rota_pc
               rota_le    = <fs_saida_group>-rota_le
               entrega_dt = <fs_saida_group>-entrega_dt BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    DATA(lv_tabix) = sy-tabix.
    lv_seq = 0.

    CALL FUNCTION 'NUMBER_GET_NEXT'
      EXPORTING
        nr_range_nr             = '01'
        object                  = 'Z_NUM_SOL'
      IMPORTING
        number                  = p_number
      EXCEPTIONS
        interval_not_found      = 1
        number_range_not_intern = 2
        object_not_found        = 3
        quantity_is_0           = 4
        quantity_is_not_1       = 5
        interval_overflow       = 6
        buffer_overflow         = 7
        OTHERS                  = 8.

    LOOP AT  lt_saida_sel ASSIGNING <fs_saida> FROM lv_tabix.

      IF <fs_saida_group>-matkl      <> <fs_saida>-matkl   OR
         <fs_saida_group>-werks      <> <fs_saida>-werks   OR
         <fs_saida_group>-rota_pc    <> <fs_saida>-rota_pc OR
         <fs_saida_group>-rota_le    <> <fs_saida>-rota_le OR
         <fs_saida_group>-entrega_dt <> <fs_saida>-entrega_dt.
        EXIT.
      ENDIF.

      IF <fs_saida>-prioridade_item > '5'.
        MESSAGE 'Prioridade Item não pode ser maior que 5' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF <fs_saida>-solicitacao_qte = 0 OR
         <fs_saida>-solicitacao_qte IS INITIAL OR
         <fs_saida>-entrega_dt IS INITIAL OR
         <fs_saida>-rota_pc IS INITIAL OR
         <fs_saida>-rota_le IS INITIAL OR
         <fs_saida>-parceiro_pc IS INITIAL OR
        <fs_saida>-parceiro_le IS INITIAL.
        MESSAGE 'Todos campos editáveis são obrigatórios favor preencher!' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF <fs_saida>-qtd_solicitada > <fs_saida>-menge.
        <fs_saida>-qtd_solicitada = <fs_saida>-qtd_solicitada - <fs_saida>-solicitacao_qte.
        MESSAGE 'Quantidade solicitada é maior que a quantidade do pedido!' TYPE 'S' DISPLAY LIKE 'E'.
        FREE: p_lt_zmmt0196.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO p_lt_zmmt0196 ASSIGNING FIELD-SYMBOL(<fs_zmmt0196>).

      READ TABLE p_dados ASSIGNING FIELD-SYMBOL(<fs_dados>)
      WITH KEY ebeln = <fs_saida>-ebeln
               ebelp = <fs_saida>-ebelp
      BINARY SEARCH.
      CHECK sy-subrc IS INITIAL.

      MOVE-CORRESPONDING <fs_dados> TO <fs_zmmt0196>.

      <fs_zmmt0196>-nro_sol                 = p_number.
      <fs_zmmt0196>-inco1                   = <fs_saida>-inco1.
      <fs_zmmt0196>-entrega_dt              = <fs_saida>-entrega_dt.
      <fs_zmmt0196>-rota_pc                 = <fs_saida>-rota_pc    .
      <fs_zmmt0196>-rota_le                 = <fs_saida>-rota_le    .
      <fs_zmmt0196>-parceiro_pc             = <fs_saida>-parceiro_pc.
      <fs_zmmt0196>-parceiro_le             = <fs_saida>-parceiro_le.
      <fs_zmmt0196>-solicitacao_qte         = <fs_saida>-solicitacao_qte.
      <fs_zmmt0196>-solicitacao_qte_manual  = <fs_saida>-solicitacao_qte_manual.
      <fs_zmmt0196>-date_create             = sy-datum.
      <fs_zmmt0196>-time_create             = sy-uzeit.
      <fs_zmmt0196>-user_create             = sy-uname.
      <fs_zmmt0196>-prioridade_sol          = '03'.
      <fs_zmmt0196>-transf_no_fornecedor    = <fs_saida>-transf_no_fornecedor.
      <fs_zmmt0196>-prioridade_item         = <fs_saida>-prioridade_item.

      ADD 1 TO lv_seq.
      <fs_zmmt0196>-seq = lv_seq.

      "Checa se tem Observações para o Pedido Item..
      READ TABLE t_observacoes TRANSPORTING NO FIELDS
      WITH KEY   ebeln = <fs_saida>-ebeln
                 ebelp = <fs_saida>-ebelp
      BINARY SEARCH.
      CHECK sy-subrc IS INITIAL.


      LOOP AT t_observacoes ASSIGNING FIELD-SYMBOL(<fs_aux>) FROM sy-tabix.

        IF <fs_saida>-ebeln <> <fs_aux>-ebeln OR
           <fs_saida>-ebelp <> <fs_aux>-ebelp .
          EXIT.
        ENDIF.

        MOVE: '*'           TO tl_tlines-tdformat,
              <fs_aux>-tdline TO tl_tlines-tdline.

        APPEND tl_tlines.
        CLEAR tl_tlines.

        wl_header-tdname = |SOL_EMBARQUE_{ <fs_zmmt0196>-nro_sol }|.

        wl_header-tdobject = 'ZTEXTO'.
        wl_header-tdid     = 'OBSE'.
        wl_header-tdspras  = sy-langu.

        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
            header          = wl_header
            savemode_direct = 'X'
          TABLES
            lines           = tl_tlines
          EXCEPTIONS
            id              = 1
            language        = 2
            name            = 3
            object          = 4
            OTHERS          = 5.

      ENDLOOP.

    ENDLOOP.

  ENDLOOP.

  DATA(_msg_error) = zcl_solicitacao_entrada_insumo=>gravar( EXPORTING i_zmmt0196_t = p_lt_zmmt0196 ).
  IF _msg_error IS INITIAL.
    p_salvou = abap_true.
  ELSE.
    MESSAGE _msg_error  TYPE 'S'.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_monta_observacao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- T_SAIDA
*&---------------------------------------------------------------------*
FORM f_monta_observacao USING p_es_row_no TYPE lvc_s_roid
                        CHANGING p_t_saida TYPE tb_saida.

  DATA: tl_texto       TYPE catsxt_longtext_itab,
        wl_texto       TYPE LINE OF catsxt_longtext_itab,
        wl_field       TYPE lvc_s_col,
        v_cont         TYPE i,
        c_x            TYPE c,
        wa_observacoes TYPE ty_observacoes,
        wl_name        TYPE thead-tdname,
        lt_lines       TYPE TABLE OF tline.

  DATA: ls_sel_hide TYPE slis_sel_hide_alv,
        is_table    TYPE lvc_s_stbl.

  REFRESH tl_texto.
  CLEAR:wl_texto.

  READ TABLE p_t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX p_es_row_no-row_id.
  LOOP AT t_observacoes INTO wa_observacoes WHERE ebeln EQ <fs_saida>-ebeln
                             AND  ebelp EQ <fs_saida>-ebelp.

    MOVE: wa_observacoes-tdline   TO wl_texto.

    APPEND wl_texto TO tl_texto.
    CLEAR: wl_texto.
  ENDLOOP.

  IF <fs_saida>-nro_sol IS NOT INITIAL.

    wl_name = |SOL_EMBARQUE_{ <fs_saida>-nro_sol }|.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        id                      = 'OBSE'
        language                = sy-langu
        name                    = wl_name
        object                  = 'ZTEXTO'
      TABLES
        lines                   = lt_lines
      EXCEPTIONS
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        OTHERS                  = 8.

    IF lt_lines IS NOT INITIAL.
      LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).
        APPEND INITIAL LINE TO tl_texto ASSIGNING FIELD-SYMBOL(<fs_texto>).

        <fs_texto> = <fs_lines>-tdline.
      ENDLOOP.
    ENDIF.

    c_x = abap_true.

  ENDIF.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title        = 'Observação'           "" Título
      im_display_mode = c_x
    CHANGING
      ch_text         = tl_texto.

  IF sy-ucomm EQ 'CX_CONT'.
    IF tl_texto[] IS NOT INITIAL.
      READ TABLE p_t_saida ASSIGNING <fs_saida> INDEX p_es_row_no-row_id.
*          IF <fs_saida>-checkbox = ''.
      MOVE icon_display_more TO <fs_saida>-observacao.
      MODIFY  t_saida FROM <fs_saida> INDEX p_es_row_no-row_id TRANSPORTING observacao.

      READ TABLE p_t_saida ASSIGNING <fs_saida> INDEX p_es_row_no-row_id.
      DELETE t_observacoes WHERE ebeln EQ <fs_saida>-ebeln
                    AND  ebelp EQ <fs_saida>-ebelp.

      LOOP AT tl_texto INTO wl_texto.
        CLEAR:wa_observacoes.

        MOVE: <fs_saida>-ebeln    TO wa_observacoes-ebeln,
              <fs_saida>-ebelp    TO wa_observacoes-ebelp,
              '*'                 TO wa_observacoes-tdformat,
              wl_texto            TO wa_observacoes-tdline.


        APPEND wa_observacoes TO t_observacoes.
      ENDLOOP.

    ELSE.
      MOVE icon_enter_more TO <fs_saida>-observacao.
      MODIFY  p_t_saida FROM <fs_saida> INDEX p_es_row_no-row_id TRANSPORTING observacao.

    ENDIF.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_monta_observacao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- T_SAIDA
*&---------------------------------------------------------------------*
FORM f_check_observacao USING nro_sol CHANGING obs.

  DATA: wl_name  TYPE thead-tdname,
        lt_lines TYPE TABLE OF tline.

  obs = icon_enter_more.

  CHECK nro_sol IS NOT INITIAL.

  wl_name = |SOL_EMBARQUE_{ nro_sol }|.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id                      = 'OBSE'
      language                = sy-langu
      name                    = wl_name
      object                  = 'ZTEXTO'
    TABLES
      lines                   = lt_lines
    EXCEPTIONS
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      OTHERS                  = 8.

  obs = COND #( WHEN lt_lines[] IS NOT INITIAL THEN icon_display_more ELSE icon_enter_more ).

ENDFORM.

*-CS2025000249-26.03.2025-#170726-JT-inicio
********************************************************************************
* envio carguero
********************************************************************************
*FORM f_envia_carguero TABLES pt_zmmt0196 TYPE tb_zmmt0196
*                    CHANGING c_error.
*
*  DATA: t_0196         TYPE TABLE OF zmmt0196,
*        t_0196_itens   TYPE TABLE OF zmmt0196,
*        w_0196         TYPE zmmt0196,
*        lv_mesg        TYPE char255,
*        lc_solicitacao TYPE zde_cargueiro_sl.
*
*  CLEAR: c_error.
*
*  IF pt_zmmt0196[] IS INITIAL.
*    c_error = abap_true.
*    RETURN.
*  ENDIF.
*
*  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
*    EXPORTING
*      percentage = 50
*      text       = 'Aguarde...Enviando Carguero...'.
*
*  t_0196[] = pt_zmmt0196[].
*
*  SORT t_0196 BY nro_sol.
*  DELETE ADJACENT DUPLICATES FROM t_0196 COMPARING nro_sol.
*
*  LOOP AT t_0196 INTO w_0196.
*
*    FREE: lc_solicitacao, t_0196_itens[].
*
*    SELECT *
*      FROM zmmt0196 INTO TABLE t_0196_itens
*     WHERE nro_sol EQ w_0196-nro_sol.
*
*    IF t_0196_itens[] IS INITIAL.
*      c_error = abap_true.
*      RETURN.
*    ENDIF.
*
*    READ TABLE t_0196_itens INTO DATA(lwa_solicitacao) INDEX 1.
*
*    IF sy-subrc NE 0.
*      c_error = abap_true.
*      RETURN.
*    ENDIF.
*
*    CHECK lwa_solicitacao-inco1 EQ zcl_solicitacao_entrada_insumo=>lc_incoterms-fob.
*
*    MOVE-CORRESPONDING lwa_solicitacao TO lc_solicitacao.
*
*    LOOP AT t_0196_itens INTO DATA(w_zmmt0196).
*      APPEND w_zmmt0196 TO lc_solicitacao-item.
*    ENDLOOP.
*
*    TRY .
*        zcl_integracao_lote_frete=>zif_integracao_lote_frete~set_gerencia_lote(
*          EXPORTING
*            i_solicitacao   = lc_solicitacao
*            i_sincronia     = zif_integracao=>at_tp_sincronia_sincrona
*          IMPORTING
*            e_id_lote_frete = DATA(lv_id_lote_frete) ).
*
*      CATCH zcx_integracao INTO DATA(ex_integracao).
*        lv_mesg = ex_integracao->get_text( ).
*        REPLACE ALL OCCURRENCES OF REGEX '&' IN lv_mesg WITH ''.
*
*        CALL FUNCTION 'S_AUT_POPUP_TO_DISPLAY_TEXT_LO'
*          EXPORTING
*            iv_titel        = 'Erro de Integração com Carguero'
*            iv_textline1    = lv_mesg(65)
*            iv_textline2    = lv_mesg+65(65)
*            iv_textline3    = lv_mesg+130(65)
*            iv_start_column = 20
*            iv_start_row    = 10.
*
*        c_error = abap_true.
*        RETURN.
*
*      CATCH zcx_error INTO DATA(ex_error).
*        lv_mesg = ex_error->get_text( ).
*        REPLACE ALL OCCURRENCES OF REGEX '&' IN lv_mesg WITH ''.
*
*        CALL FUNCTION 'S_AUT_POPUP_TO_DISPLAY_TEXT_LO'
*          EXPORTING
*            iv_titel        = 'Erro de Integração com Carguero'
*            iv_textline1    = lv_mesg(65)
*            iv_textline2    = lv_mesg+65(65)
*            iv_textline3    = lv_mesg+130(65)
*            iv_start_column = 20
*            iv_start_row    = 10.
*
*        c_error = abap_true.
*        RETURN.
*    ENDTRY.
*
*  ENDLOOP.
*
*ENDFORM.
*-CS2025000249-26.03.2025-#170726-JT-fim

FORM f_check_agente_frete TABLES   t_fields STRUCTURE sval
                          CHANGING error  STRUCTURE svale.

  READ TABLE t_fields INTO DATA(w_fields) INDEX 1.
  IF sy-subrc IS INITIAL AND w_fields-value IS NOT INITIAL.

    SELECT COUNT(*)
      FROM lfa1
      WHERE lifnr EQ w_fields-value
      AND ktokk EQ 'ZFIC'.

    IF sy-subrc IS NOT INITIAL.
      CLEAR error.
      error =
      VALUE #(
                errortab   = 'LFA1'
                errorfield = 'LIFNR'
                msgty      = 'I'
                msgid      = 'Z_MM'
                msgno      = '015'
                msgv1      = w_fields-value

             ).
    ENDIF.
  ELSE.
    CLEAR error.
    error =
    VALUE #(
              errortab   = 'LFA1'
              errorfield = 'LIFNR'
              msgty      = 'I'
              msgid      = 'Z_MM'
              msgno      = '016'
           ).
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form F_ORDENA_GRID
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- CH_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_ordena_grid USING alv CHANGING p_ch_fieldcat TYPE lvc_t_fcat.

  LOOP AT p_ch_fieldcat ASSIGNING FIELD-SYMBOL(<f_fieldcat>).
    IF alv EQ '1'. "A solicitar
      CASE <f_fieldcat>-fieldname.
        WHEN 'BUKRS'. "Empresa
          <f_fieldcat>-col_pos = 1.
        WHEN 'LIFNR'. "Fornecedor
          <f_fieldcat>-col_pos = 2.
        WHEN 'NAME1'. "Nome Fornecedor
          <f_fieldcat>-col_pos = 3.
        WHEN 'EBELN'. " Nº do documento de compras
          <f_fieldcat>-col_pos = 4.
        WHEN 'EBELP'. " Nº item do documento de compra
          <f_fieldcat>-col_pos = 5.
        WHEN 'INCO1'. " Nº do documento de compras
          <f_fieldcat>-col_pos = 6.
        WHEN 'UNSEZ'. " Safra Referencia
          <f_fieldcat>-col_pos = 7.
        WHEN 'WERKS'. " Centro
          <f_fieldcat>-col_pos = 7.
        WHEN 'MATNR'. " Nº do material
          <f_fieldcat>-col_pos = 8.
        WHEN 'MAKTX'. " Texto breve de material
          <f_fieldcat>-col_pos = 9.
        WHEN 'MEINS'. "Unidade de medida básica
          <f_fieldcat>-col_pos = 10.
        WHEN 'PARCEIRO_PC'. " Parceiro Ponto de Coleta
          <f_fieldcat>-col_pos = 11.
        WHEN 'ROTA_PC'. " Rota Ponto de Coleta
          <f_fieldcat>-col_pos = 12.
        WHEN 'ROTA_PC_DESC'. " Descrição Rota SD
          <f_fieldcat>-col_pos = 13.
        WHEN 'PARCEIRO_LE'. " Parceiro Local de Entrega
          <f_fieldcat>-col_pos = 14.
        WHEN 'ROTA_LE'. " Rota Local de entrega
          <f_fieldcat>-col_pos = 15.
        WHEN 'ROTA_LE_DESC'. " Descrição Rota SD
          <f_fieldcat>-col_pos = 16.
        WHEN 'MENGE'. "Quantidade Pedido
          <f_fieldcat>-col_pos = 17.
        WHEN 'QTD_SOLICITADA'. "Quantidade Solicitada
          <f_fieldcat>-col_pos = 18.
        WHEN 'SALDO_A_SOLICITAR'. "Saldo à Solicitar
          <f_fieldcat>-col_pos = 19.
        WHEN 'SOLICITACAO_QTE'. " Quantidade da solicitação
          <f_fieldcat>-col_pos = 20.
        WHEN 'SOLICITACAO_QTE_MANUAL'.
          <f_fieldcat>-col_pos = 20.
        WHEN 'SALDO_FAT_PED'. "Saldo à Faturar Pedido
          <f_fieldcat>-col_pos = 21.
        WHEN 'ENTREGA_DT'. "Data requerida para entrega
          <f_fieldcat>-col_pos = 22.
        WHEN 'OBSERVACAO'. "Observação
          <f_fieldcat>-col_pos = 23.
*          WHEN 'SALDO_A_FORMAR_CARGA'. "Saldo a Formar Carga
*            <F_FIELDCAT>-COL_POS = CONT.
        WHEN 'PRIORIDADE_ITEM'. "Prioridade Item
          <f_fieldcat>-col_pos = 24.
        WHEN 'TRANSF_NO_FORNECEDOR'. "Transferencia no Fornecedor
          <f_fieldcat>-col_pos = 25.
        WHEN OTHERS.
          <f_fieldcat>-no_out = abap_true.
      ENDCASE.
    ENDIF.

    IF alv EQ '2'. "Solicitados
      CASE <f_fieldcat>-fieldname.
        WHEN 'BUKRS'.
          <f_fieldcat>-col_pos = 1.
        WHEN 'NRO_SOL'.
          <f_fieldcat>-col_pos = 2.
        WHEN 'SEQ'.
          <f_fieldcat>-col_pos = 2.
        WHEN 'EBELN'. " Nº do documento de compras
          <f_fieldcat>-col_pos = 3.
        WHEN 'EBELP'. " Nº item do documento de compra
          <f_fieldcat>-col_pos = 4.
        WHEN 'INCO1'. " Nº do documento de compras
          <f_fieldcat>-col_pos = 4.
        WHEN 'LIFNR'.
          <f_fieldcat>-col_pos = 5.
        WHEN 'NAME1'.
          <f_fieldcat>-col_pos = 6.
        WHEN 'MATKL'.
          <f_fieldcat>-col_pos = 7.
        WHEN 'MATNR'.
          <f_fieldcat>-col_pos = 8.
        WHEN 'MAKTX'.
          <f_fieldcat>-col_pos = 9.
        WHEN 'WERKS'.
          <f_fieldcat>-col_pos = 10.
        WHEN 'PARCEIRO_PC'.
          <f_fieldcat>-col_pos = 11.
        WHEN 'ROTA_PC'.
          <f_fieldcat>-col_pos = 12.
        WHEN 'ROTA_PC_DESC'.
          <f_fieldcat>-col_pos = 13.
        WHEN 'PARCEIRO_LE'.
          <f_fieldcat>-col_pos = 14.
        WHEN 'ROTA_LE'.
          <f_fieldcat>-col_pos = 15.
        WHEN 'ROTA_LE_DESC'.
          <f_fieldcat>-col_pos = 16.
        WHEN 'SOLICITACAO_QTE'.
          <f_fieldcat>-col_pos = 17.
        WHEN 'SOLICITACAO_QTE_MANUAL'.
          <f_fieldcat>-col_pos = 17.
        WHEN 'SOLICITACAO_QTE_CARGUERO'.
          <f_fieldcat>-col_pos = 17.
        WHEN 'SALDO_ENTREGUE_SOLIC'.
          <f_fieldcat>-col_pos = 18.
        WHEN 'SALDO_A_FORMAR_CARGA'.
          <f_fieldcat>-col_pos = 19.
        WHEN 'ENTREGA_DT'.
          <f_fieldcat>-col_pos = 20.
        WHEN 'OBSERVACAO'.
          <f_fieldcat>-col_pos = 21.
        WHEN 'PRIORIDADE_SOL'.
          <f_fieldcat>-col_pos = 22.
        WHEN 'TRANSF_NO_FORNECEDOR'.
          <f_fieldcat>-col_pos = 23.
        WHEN 'STATUS_CARGUERO'.
          <f_fieldcat>-col_pos = 24.
        WHEN 'DATA'.
          <f_fieldcat>-col_pos = 25.
        WHEN 'USUARIO'.
          <f_fieldcat>-col_pos = 26.
        WHEN OTHERS.
          <f_fieldcat>-no_out = abap_true.
      ENDCASE.
    ENDIF.

    IF alv EQ '3'.
      CASE <f_fieldcat>-fieldname.
        WHEN 'BUKRS'. "Empresa
          <f_fieldcat>-col_pos = 1.
        WHEN 'NRO_SOL'.
          <f_fieldcat>-col_pos = 2.
        WHEN 'LIFNR'. "Fornecedor
          <f_fieldcat>-col_pos = 3.
        WHEN 'NAME1'. "Nome Fornecedor
          <f_fieldcat>-col_pos = 4.
        WHEN 'EBELN'. " Nº do documento de compras
          <f_fieldcat>-col_pos = 5.
        WHEN 'EBELP'. " Nº item do documento de compra
          <f_fieldcat>-col_pos = 6.
        WHEN 'UNSEZ'. " Safra Referencia
          <f_fieldcat>-col_pos = 7.
        WHEN 'WERKS'. " Centro
          <f_fieldcat>-col_pos = 8.
        WHEN 'MATNR'. " Nº do material
          <f_fieldcat>-col_pos = 9.
        WHEN 'MAKTX'. " Texto breve de material
          <f_fieldcat>-col_pos = 10.
        WHEN 'MEINS'. "Unidade de medida básica
          <f_fieldcat>-col_pos = 11.
        WHEN 'PARCEIRO_PC'. " Parceiro Ponto de Coleta
          <f_fieldcat>-col_pos = 12.
        WHEN 'ROTA_PC'. " Rota Ponto de Coleta
          <f_fieldcat>-col_pos = 13.
        WHEN 'ROTA_PC_DESC'. " Descrição Rota SD
          <f_fieldcat>-col_pos = 14.
        WHEN 'PARCEIRO_LE'. " Parceiro Local de Entrega
          <f_fieldcat>-col_pos = 15.
        WHEN 'ROTA_LE'. " Rota Local de entrega
          <f_fieldcat>-col_pos = 16.
        WHEN 'ROTA_LE_DESC'. " Descrição Rota SD
          <f_fieldcat>-col_pos = 17.

        WHEN 'MENGE'. "Quantidade Pedido
          <f_fieldcat>-col_pos = 18.
        WHEN 'QTD_SOLICITADA'. "Quantidade Solicitada
          <f_fieldcat>-col_pos = 18.
        WHEN 'SALDO_A_SOLICITAR'. " Saldo à Solicitar
          <f_fieldcat>-col_pos = 20.
        WHEN 'SALDO_ENTREGUE_SOLIC'. "Quantidade em Carga
          <f_fieldcat>-col_pos = 21.
        WHEN 'SOLICITACAO_QTE'. " Quantidade da solicitação
          <f_fieldcat>-col_pos = 22.
        WHEN 'SOLICITACAO_QTE_MANUAL'. " Quantidade da solicitação
          <f_fieldcat>-col_pos = 22.

*        WHEN 'SALDO_A_SOLICITAR'. "Saldo à Solicitar
*          <F_FIELDCAT>-COL_POS = 19.
*        WHEN 'SALDO_FAT_PED'. "Saldo à Faturar Pedido
*          <F_FIELDCAT>-COL_POS = 21.

        WHEN 'ENTREGA_DT'. "Data requerida para entrega
          <f_fieldcat>-col_pos = 23.
*        WHEN 'OBSERVACAO'. "Observação
*          <F_FIELDCAT>-COL_POS = 23.
*          WHEN 'SALDO_A_FORMAR_CARGA'. "Saldo a Formar Carga
*            <F_FIELDCAT>-COL_POS = CONT.
        WHEN 'PRIORIDADE_ITEM'. "Prioridade Item
          <f_fieldcat>-col_pos = 24.
        WHEN 'TRANSF_NO_FORNECEDOR'. "Transferencia no Fornecedor
          <f_fieldcat>-col_pos = 25.
        WHEN 'DATA'.
          <f_fieldcat>-col_pos = 26.

          <f_fieldcat>-reptext = 'Data Criação'.
          <f_fieldcat>-scrtext_l = 'Data Criação'.
          <f_fieldcat>-scrtext_m = 'Data Criação'.
          <f_fieldcat>-scrtext_s = 'Data Criação'.

        WHEN OTHERS.
          <f_fieldcat>-no_out = abap_true.
      ENDCASE.
    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_MONTA_DADOS_solicitados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_dados_solicitadas .

  DATA: lw_celltab TYPE lvc_s_styl,
        lt_celltab TYPE lvc_t_styl,
        lw_saida   TYPE zmme_alv_solic_receb_compras,
        lv_qtd_sol TYPE zmmt0196-solicitacao_qte,
        lv_tot_201 TYPE zmmt0201-qtd_total_kg.

  lw_celltab-fieldname = 'ENTREGA_DT'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 1.

  lw_celltab-fieldname = 'NUMERO_ROTA'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 2.

  lw_celltab-fieldname = 'OBSERVACAO'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 3.

  lw_celltab-fieldname = 'PARCEIRO_LE'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 4.

  lw_celltab-fieldname = 'PARCEIRO_PC'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 5.

  lw_celltab-fieldname = 'PRIORIDADE_ITEM'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 6.

  lw_celltab-fieldname = 'ROTA_LE'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 7.

  lw_celltab-fieldname = 'ROTA_PC'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 8.

  lw_celltab-fieldname = 'SOLICITACAO_QTE'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 9.

  lw_celltab-fieldname = 'SOLICITACAO_QTE_MANUAL'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 10.

  lw_celltab-fieldname = 'TRANSF_NO_FORNECEDOR'.
  lw_celltab-style     = cl_gui_alv_grid=>mc_style_disabled.
  INSERT lw_celltab INTO lt_celltab INDEX 11.

  PERFORM f_atrib_saldos_pedido TABLES t_dados.

  LOOP AT t_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).

    READ TABLE t_t001w INTO DATA(ls_t001w) WITH KEY werks = <fs_dados>-werks.
    IF sy-subrc IS INITIAL.
      lw_saida-bukrs = ls_t001w-vkorg.
    ENDIF.

    READ TABLE t_zmmt0196 TRANSPORTING NO FIELDS
    WITH KEY ebeln = <fs_dados>-ebeln
             ebelp = <fs_dados>-ebelp
    BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    LOOP AT t_zmmt0196 ASSIGNING FIELD-SYMBOL(<fs_zmmt0196>) FROM sy-tabix.
      IF <fs_dados>-ebeln <> <fs_zmmt0196>-ebeln OR
         <fs_dados>-ebelp <> <fs_zmmt0196>-ebelp .
        EXIT.
      ENDIF.

      MOVE-CORRESPONDING <fs_dados> TO lw_saida.

      lw_saida-qtd_solicitada = <fs_zmmt0196>-solicitacao_qte.

      lw_saida-nro_sol                   = <fs_zmmt0196>-nro_sol.
      lw_saida-seq                       = <fs_zmmt0196>-seq.
      lw_saida-inco1                     = <fs_zmmt0196>-inco1.
      lw_saida-usuario                   = <fs_zmmt0196>-user_create.
      lw_saida-data                      = <fs_zmmt0196>-date_create.
      lw_saida-rota_pc                   = <fs_zmmt0196>-rota_pc.
      lw_saida-rota_le                   = <fs_zmmt0196>-rota_le.
      lw_saida-parceiro_pc               = <fs_zmmt0196>-parceiro_pc.
      lw_saida-parceiro_le               = <fs_zmmt0196>-parceiro_le.
      lw_saida-solicitacao_qte           = <fs_zmmt0196>-solicitacao_qte.
      lw_saida-solicitacao_qte_carguero  = <fs_zmmt0196>-solicitacao_qte_carguero.
      lw_saida-solicitacao_qte_manual    = <fs_zmmt0196>-solicitacao_qte_manual.
      lw_saida-entrega_dt                = <fs_zmmt0196>-entrega_dt.
      lw_saida-qtd_faturado              = <fs_dados>-qtde_fat_pedido - <fs_zmmt0196>-solicitacao_qte.
      lw_saida-transf_no_fornecedor      = <fs_zmmt0196>-transf_no_fornecedor.
      lw_saida-prioridade_item           = <fs_zmmt0196>-prioridade_item.
      lw_saida-prioridade_sol            = <fs_zmmt0196>-prioridade_sol.

      READ TABLE lt_rota INTO DATA(ls_rota) WITH KEY nr_rot = lw_saida-rota_le.
      IF sy-subrc IS INITIAL.
        lw_saida-rota_le_desc = ls_rota-rot_desc.
      ENDIF.

      READ TABLE lt_rota INTO ls_rota WITH KEY nr_rot = lw_saida-rota_pc.
      IF sy-subrc IS INITIAL.
        lw_saida-rota_pc_desc = ls_rota-rot_desc.
      ENDIF.

      IF <fs_dados>-inco1 EQ 'FOB'.

*        LW_SAIDA-SALDO_ENTREGUE_SOLIC = <FS_DADOS>-QTDE_VINC_CARGA.
        PERFORM f_saldo_carga_solicitacao USING lw_saida-nro_sol lw_saida-seq CHANGING lw_saida-saldo_entregue_solic.

      ELSEIF <fs_dados>-inco1 EQ 'CIF'.

        lw_saida-saldo_entregue_solic = <fs_dados>-qtde_fat_pedido.

      ENDIF.

      lw_saida-saldo_fat_solic = <fs_dados>-qtde_solicitacao - lw_saida-saldo_entregue_solic.

      lw_saida-saldo_fat_ped = <fs_dados>-menge - <fs_dados>-qtde_fat_pedido.

      lw_saida-status_carguero = COND #( WHEN <fs_zmmt0196>-id_lote_frete IS NOT INITIAL
                                                THEN '@09@ - Aguardado Envio'
                                                ELSE '@0A@ - Não enviado' ).
      READ TABLE t_zlest0181 INTO DATA(wa_zlest0181) WITH KEY id_lote_frete = <fs_zmmt0196>-id_lote_frete.
      IF wa_zlest0181-id_carguero IS NOT INITIAL.
        lw_saida-status_carguero = '@08@ - Integrado'.
      ENDIF.

      lw_saida-saldo_a_formar_carga = lw_saida-qtd_solicitada - lw_saida-saldo_entregue_solic.

      PERFORM f_check_observacao USING lw_saida-nro_sol CHANGING lw_saida-observacao.
      lw_saida-celltab = lt_celltab.

      APPEND lw_saida TO t_saida.
      CLEAR lw_saida.

    ENDLOOP.


  ENDLOOP.

*--------------------------------------------------------------------------------------*
* Montar saida a nivel de Solicitação
*--------------------------------------------------------------------------------------*
  t_saida_sol_item = t_saida.

*  FREE: t_saida.
*  SORT t_saida_sol_item BY nro_sol.
*
*  LOOP AT t_saida_sol_item ASSIGNING FIELD-SYMBOL(<fs_saida2>) .
*
*    AT NEW nro_sol.
*      APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
*      <fs_saida> = <fs_saida2>.
*      CLEAR <fs_saida>-solicitacao_qte.
*    ENDAT.
*
*    <fs_saida>-solicitacao_qte = <fs_saida>-solicitacao_qte + <fs_saida2>-solicitacao_qte.
*
*    READ TABLE <fs_saida2>-celltab ASSIGNING FIELD-SYMBOL(<fs_celltab>)
*    WITH KEY fieldname = 'SOLICITACAO_QTE'.
*    IF sy-subrc IS INITIAL.
*      <fs_celltab>-style = cl_gui_alv_grid=>mc_style_enabled.
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.

FORM f_monta_dados_a_solicitar .

  DATA: lw_celltab TYPE lvc_s_styl,
        lt_celltab TYPE lvc_t_styl,
        lw_saida   TYPE zmme_alv_solic_receb_compras,
        lv_qtd_sol TYPE zmmt0196-solicitacao_qte,
        lv_tot_201 TYPE zmmt0201-qtd_total_kg.

  PERFORM f_atrib_saldos_pedido TABLES t_dados.

  LOOP AT t_dados ASSIGNING FIELD-SYMBOL(<fs_dados>).
    CLEAR: lv_qtd_sol , lv_tot_201.

    MOVE-CORRESPONDING <fs_dados> TO lw_saida.

    READ TABLE t_t001w INTO DATA(ls_t001w) WITH KEY werks = <fs_dados>-werks.
    IF sy-subrc IS INITIAL.
      lw_saida-bukrs = ls_t001w-vkorg.
    ENDIF.

    lw_saida-inco1             = <fs_dados>-inco1.
    lw_saida-qtd_solicitada    = <fs_dados>-qtde_solicitacao.
    lw_saida-saldo_a_solicitar = <fs_dados>-saldo_a_solicitar.
    lw_saida-saldo_fat_solic   = lw_saida-qtd_solicitada - <fs_dados>-qtde_vinc_carga.

    IF lw_saida-qtd_solicitada >= <fs_dados>-menge.
      CONTINUE.
    ENDIF.

*    READ TABLE t_zmmt0196 TRANSPORTING NO FIELDS
*    WITH KEY ebeln = <fs_dados>-ebeln
*             ebelp = <fs_dados>-ebelp
*    BINARY SEARCH.
*    IF sy-subrc IS INITIAL.
*
*      LOOP AT t_zmmt0196 ASSIGNING FIELD-SYMBOL(<fs_zmmt0196>).
*        IF <fs_dados>-ebeln <> <fs_zmmt0196>-ebeln OR
*           <fs_dados>-ebelp <> <fs_zmmt0196>-ebelp .
*          CONTINUE.
*        ENDIF.
*
*        lv_qtd_sol = lv_qtd_sol +  <fs_zmmt0196>-solicitacao_qte.
*        lw_saida-qtd_solicitada = lv_qtd_sol.
*
*
*
*      ENDLOOP.
*
*      IF lv_qtd_sol >= <fs_dados>-menge.
*        CONTINUE.
*      ENDIF.
*    ENDIF.

    IF <fs_dados>-inco1 EQ 'CIF'.
      lw_saida-saldo_entregue_solic = <fs_dados>-qtde_fat_pedido.
    ELSEIF <fs_dados>-inco1 EQ 'FOB'.
*      LW_SAIDA-SALDO_ENTREGUE_SOLIC = <FS_DADOS>-QTDE_VINC_CARGA.
      PERFORM f_saldo_carga_solicitacao USING lw_saida-nro_sol lw_saida-seq CHANGING lw_saida-saldo_entregue_solic.
    ENDIF.

    lw_saida-qtd_faturado  = <fs_dados>-qtde_fat_pedido.
    lw_saida-saldo_fat_ped = <fs_dados>-menge - <fs_dados>-qtde_fat_pedido.

    IF lw_saida-saldo_fat_ped IS INITIAL.
      CONTINUE.
    ENDIF.

    PERFORM f_check_observacao USING lw_saida-nro_sol CHANGING lw_saida-observacao.

    APPEND lw_saida TO t_saida.
    CLEAR lw_saida.

  ENDLOOP.




ENDFORM.


FORM f_seleciona_dados_a_solicitar .


  SELECT *
     FROM zcds_solic_receb_pedidos_comp
     INTO TABLE @t_dados
     WHERE ebeln IN @s_ebeln
       AND ebelp IN @s_ebelp
       AND lifnr IN @s_lifnr
       AND aedat IN @s_erdat
       AND bsart IN @s_bsart
       "AND procstat = '05'  "MM - Ajuste Listagem Pedidos US 170726 - WPP
       AND bukrs IN @s_bukrs
       AND werks IN @s_werks
       AND matnr IN @s_matnr
       AND inco1 IN @s_inco1 "wbarbosa 29/04/2025
       AND ( menge > 1 OR netwr > '0.01' ) .

  "MM - Ajuste Listagem Pedidos US 170726 - WPP - Ini
  LOOP AT t_dados ASSIGNING FIELD-SYMBOL(<fs_pedido>).

    CASE <fs_pedido>-bsart.
      WHEN 'ZSON'.
        IF <fs_pedido>-procstat NE '02'.
          CLEAR <fs_pedido>-ebeln.
        ENDIF.
      WHEN OTHERS.
        IF <fs_pedido>-procstat NE '05'.
          CLEAR <fs_pedido>-ebeln.
        ENDIF.
    ENDCASE.
  ENDLOOP.

  DELETE t_dados WHERE ebeln IS INITIAL.
  "MM - Ajuste Listagem Pedidos US 170726 - WPP - Fim

  CHECK t_dados[] IS NOT INITIAL.

  SELECT *
    FROM t001w
    INTO TABLE t_t001w
    FOR ALL ENTRIES IN t_dados
    WHERE werks EQ t_dados-werks.

  SORT t_dados BY ebeln ebelp.

*  DATA(lt_dados) = t_dados.
*  SORT lt_dados BY ebeln ebelp.
*  DELETE ADJACENT DUPLICATES FROM lt_dados COMPARING ebeln ebelp.

*  SELECT *
*    FROM zmmt0196
*    INTO TABLE t_zmmt0196
*    FOR ALL ENTRIES IN lt_dados
*    WHERE ebeln = lt_dados-ebeln
*      AND ebelp = lt_dados-ebelp
*      AND cancel EQ abap_false.
*
*  IF sy-subrc IS INITIAL.
*    SORT t_zmmt0196 BY ebeln ebelp.
*
*    DATA(lt_0196) = t_zmmt0196.
*    SORT lt_0196 BY nro_sol.
*    DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING nro_sol.
*
*
*
*  ENDIF.


ENDFORM.

FORM f_seleciona_dados_solicitadas .

  SELECT *
    FROM zmmt0196
    INTO TABLE t_zmmt0196
    WHERE nro_sol IN s_nrsol
      AND ebeln IN s_ebeln
      AND ebelp IN s_ebelp
      AND date_create IN s_dtsol
      AND werks IN s_werks
      AND bsart IN s_bsart
      AND cancel EQ abap_false
      AND solicitacao_qte > 0.
  IF sy-subrc IS INITIAL.

    SELECT *
      FROM t001w
      INTO TABLE t_t001w
      FOR ALL ENTRIES IN t_dados
      WHERE werks EQ t_dados-werks.

    SELECT lifnr, nr_rot, rot_desc
        FROM zsdt0132
        INTO CORRESPONDING FIELDS OF TABLE @lt_rota
      FOR ALL ENTRIES IN @t_zmmt0196
        WHERE lifnr EQ @t_zmmt0196-parceiro_pc
          AND nr_rot EQ @t_zmmt0196-rota_pc.

    SELECT lifnr, nr_rot, rot_desc
        FROM zsdt0132
        APPENDING CORRESPONDING FIELDS OF TABLE @lt_rota
      FOR ALL ENTRIES IN @t_zmmt0196
        WHERE lifnr EQ @t_zmmt0196-parceiro_le
          AND nr_rot EQ @t_zmmt0196-rota_le.

    SELECT *
      FROM zlest0181
      INTO TABLE @t_zlest0181
      FOR ALL ENTRIES IN @t_zmmt0196
      WHERE id_lote_frete EQ @t_zmmt0196-id_lote_frete.

    SORT t_zmmt0196 BY ebeln ebelp.

    DATA(lt_zmmt0196) = t_zmmt0196.
    SORT lt_zmmt0196 BY ebeln ebelp.
    DELETE ADJACENT DUPLICATES FROM lt_zmmt0196 COMPARING ebeln ebelp.

*    SELECT *
*     FROM zmmt0196
*     INTO TABLE t_zmmt0196_2
*      FOR ALL ENTRIES IN lt_zmmt0196
*     WHERE ebeln = lt_zmmt0196-ebeln
*       AND ebelp = lt_zmmt0196-ebelp
*       AND cancel EQ abap_false.
*    IF sy-subrc IS INITIAL.
*      SORT t_zmmt0196_2 BY ebeln ebelp.
*    ENDIF.

    SELECT *
      FROM zcds_solic_receb_pedidos_comp
      INTO TABLE @t_dados
      FOR ALL ENTRIES IN @lt_zmmt0196
      WHERE ebeln = @lt_zmmt0196-ebeln
        AND ebelp = @lt_zmmt0196-ebelp.
    IF sy-subrc IS INITIAL.
      SORT t_dados BY ebeln ebelp.
    ENDIF.

*    lt_0196 = t_zmmt0196.
*    SORT lt_0196 BY nro_sol.
*    DELETE ADJACENT DUPLICATES FROM lt_0196 COMPARING nro_sol.

*    SELECT *
*      FROM zmmt0201
*      INTO TABLE t_0201
*      FOR ALL ENTRIES IN lt_0196
*      WHERE nro_sol = lt_0196-nro_sol.
*    IF sy-subrc IS INITIAL.
*      SORT t_0201 BY nro_sol.
*
*      LOOP AT t_0201 ASSIGNING <fs_0201>.
*        lw_0201_sum-nro_sol = <fs_0201>-nro_sol.
*        lw_0201_sum-qtd     = <fs_0201>-qtd_total_kg.
*
*        COLLECT lw_0201_sum INTO t_0201_sum.
*
*      ENDLOOP.
*    ENDIF.

  ENDIF.

ENDFORM.


FORM f_atrib_saldos_pedido TABLES p_pedidos LIKE t_dados.

  DATA: lv_qtd_sol TYPE zmmt0196-solicitacao_qte,
        lv_tot_201 TYPE zmmt0201-qtd_total_kg.

  CHECK p_pedidos[] IS NOT INITIAL.

  SELECT *
    FROM zmmt0196 INTO TABLE @DATA(lit_zmmt0196)
     FOR ALL ENTRIES IN @p_pedidos
   WHERE ebeln EQ @p_pedidos-ebeln
     AND ebelp EQ @p_pedidos-ebelp.

  DELETE lit_zmmt0196 WHERE cancel EQ abap_true.

  IF lit_zmmt0196[] IS NOT INITIAL.
    SELECT *
      FROM zmmt0202 AS i
      INTO TABLE @DATA(lit_zmmt0202)
      FOR ALL ENTRIES IN @lit_zmmt0196
      WHERE nro_sol EQ @lit_zmmt0196-nro_sol
        AND seq     EQ @lit_zmmt0196-seq
        AND NOT EXISTS (

         SELECT nro_cg
          FROM zmmt0201  AS c
         WHERE c~nro_cg = i~nro_cg
          AND c~cancel = @abap_true
       ).

    DELETE lit_zmmt0202 WHERE cancel = abap_true.
  ENDIF.


  LOOP AT p_pedidos ASSIGNING FIELD-SYMBOL(<fs_pedido>).

    CLEAR: <fs_pedido>-qtde_solicitacao, <fs_pedido>-saldo_a_solicitar, <fs_pedido>-qtde_vinc_carga.

    "Solicitação
    LOOP AT lit_zmmt0196 ASSIGNING FIELD-SYMBOL(<fs_zmmt0196>) WHERE ebeln = <fs_pedido>-ebeln
                                                               AND ebelp = <fs_pedido>-ebelp.
      ADD <fs_zmmt0196>-solicitacao_qte TO <fs_pedido>-qtde_solicitacao.

      "Utilização em Carga
      LOOP AT lit_zmmt0202 ASSIGNING FIELD-SYMBOL(<fs_zmmt0202>) WHERE nro_sol = <fs_zmmt0196>-nro_sol
                                                                   AND seq     = <fs_zmmt0196>-seq.
        ADD <fs_zmmt0202>-qtd_vinc_carga TO <fs_pedido>-qtde_vinc_carga.

      ENDLOOP.

    ENDLOOP.

    <fs_pedido>-saldo_a_solicitar = <fs_pedido>-menge - <fs_pedido>-qtde_solicitacao.

  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_SALDO_CARGA_SOLICITACAO
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LW_SAIDA_NRO_SOL
*&      --> LW_SAIDA_SEQ
*&      <-- LW_SAIDA_SALDO_ENTREGUE_SOLIC
*&---------------------------------------------------------------------*
FORM f_saldo_carga_solicitacao  USING    lv_nro_sol
                                         lv_seq
                                CHANGING lv_saldo_entregue_solic.
  CLEAR lv_saldo_entregue_solic.

  SELECT *
    FROM zmmt0202 AS i
    INTO TABLE @DATA(lit_zmmt0202)
    WHERE nro_sol EQ @lv_nro_sol
      AND seq     EQ @lv_seq
      AND NOT EXISTS (

       SELECT nro_cg
        FROM zmmt0201  AS c
       WHERE c~nro_cg = i~nro_cg
         AND c~cancel = @abap_true

     ).

  DELETE lit_zmmt0202 WHERE cancel = abap_true.

  LOOP AT lit_zmmt0202 INTO DATA(ls_zmmt0202).
    ADD ls_zmmt0202-qtd_vinc_carga TO lv_saldo_entregue_solic.
  ENDLOOP.

ENDFORM.

FORM f_cancelar_solicitacao CHANGING p_cancelou.

  DATA: lit_zmmt0196_cancel TYPE TABLE OF zmmt0196.

  DATA: lw_sel_hide TYPE slis_sel_hide_alv,
        lt_sel_rows TYPE lvc_t_row,
        lw_sel_rows TYPE lvc_s_row,
        lt_zmmt0196 TYPE TABLE OF zmmt0196,
        lv_seq      TYPE zmmt0196-seq,
        lv_nro_sol  TYPE zmmt0196-nro_sol.

  CLEAR: p_cancelou.

  CALL METHOD ref2->get_selected_rows
    IMPORTING
      et_index_rows = lt_sel_rows.

  IF lt_sel_rows[] IS INITIAL .
    MESSAGE 'Selecione um item!' TYPE 'E'.
    EXIT.
  ENDIF.

  IF lines( lt_sel_rows[] ) NE 1.
    MESSAGE 'Selecione apenas um item!' TYPE 'E'.
    EXIT.
  ENDIF.

  LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>).

    READ TABLE t_saida_edit_solicitacao ASSIGNING FIELD-SYMBOL(<fs_saida_edit_sol>) INDEX <fs_sel_rows>-index.
    CHECK sy-subrc IS INITIAL.

    IF <fs_saida_edit_sol>-saldo_entregue_solic IS NOT INITIAL.
      MESSAGE 'Existe Carga Gerada para essa Solicitação!' TYPE 'E'.
      RETURN.
    ENDIF.

    IF <fs_saida_edit_sol>-seq IS INITIAL OR <fs_saida_edit_sol>-nro_sol IS INITIAL.
      MESSAGE 'Item Solicitação não foi gravado!' TYPE 'E'.
      RETURN.
    ENDIF.

    SELECT SINGLE *
      FROM zmmt0196 INTO @DATA(lwa_zmmt0196_cancel)
      WHERE nro_sol EQ @<fs_saida_edit_sol>-nro_sol
        AND seq     EQ @<fs_saida_edit_sol>-seq.

    IF sy-subrc NE 0.
      MESSAGE 'Item Solicitação não foi gravado!' TYPE 'E'.
      RETURN.
    ENDIF.

    lwa_zmmt0196_cancel-date_cancel     = sy-datum.
    lwa_zmmt0196_cancel-time_cancel     = sy-uzeit.
    lwa_zmmt0196_cancel-user_cancel     = sy-uname.
    lwa_zmmt0196_cancel-cancel          = abap_true.

    CLEAR: lit_zmmt0196_cancel[].
    APPEND INITIAL LINE TO lit_zmmt0196_cancel ASSIGNING FIELD-SYMBOL(<fs_zmmt0196_cancel>).
    MOVE-CORRESPONDING lwa_zmmt0196_cancel TO <fs_zmmt0196_cancel>.

    DATA(_msg_error) = zcl_solicitacao_entrada_insumo=>gravar( EXPORTING i_zmmt0196_t = lit_zmmt0196_cancel ).

    IF _msg_error IS INITIAL.
      DELETE t_saida_edit_solicitacao INDEX <fs_sel_rows>-index.
      p_cancelou = abap_true.
    ENDIF.

  ENDLOOP.

ENDFORM.

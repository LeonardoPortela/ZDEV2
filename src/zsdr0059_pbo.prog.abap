*&---------------------------------------------------------------------*
*&  Include           ZSDR0059_PBO
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.

MODULE pbo_0100 OUTPUT.
  CLEAR: gs_layout.
  FREE: it_fcat[].

  IF obj_container_0100 IS INITIAL.
    FREE: it_fcat.
    PERFORM f_refresh_objetos.
*    PERFORM f_criar_field_catalog USING '0100'.
    PERFORM f_criar_catalog USING '0100'.


    IF NOT  cl_gui_alv_grid=>offline( ) .
      CREATE OBJECT obj_container_0100
        EXPORTING
          container_name = 'CC_ALV_0100'.
    ENDIF.

    CREATE OBJECT obj_alv_0100
      EXPORTING
        i_parent = obj_container_0100.

    CREATE OBJECT obj_toolbar_0100
      EXPORTING
        io_alv_grid = obj_alv_0100.

    gs_layout-info_fname = 'LINE_COLOR'.
    gs_layout-cwidth_opt = 'X'.
    gs_layout-col_opt = 'X'.
*    gs_layout-cwidth_opt = 'X'.
    gs_variant_c-report    = sy-repid.

    SET HANDLER: obj_toolbar_0100->on_toolbar          FOR obj_alv_0100,
                 obj_toolbar_0100->handle_user_command FOR obj_alv_0100.

    PERFORM f_exclude_code USING '0100'.

    CALL METHOD obj_alv_0100->set_table_for_first_display
      EXPORTING
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant_c
        is_layout            = gs_layout
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0100.

    SET HANDLER lcl_event_handler_0100->handle_hotspot_click FOR obj_alv_0100.

    CALL METHOD obj_alv_0100->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD obj_alv_0100->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
    CALL METHOD obj_alv_0100->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.

MODULE status_0105 OUTPUT.

*#127333 - 04.12.2023 - JT - inicio
  DATA: zcl_manifesto_dest TYPE REF TO zcl_manifesto_dest,
        t_ucomm            TYPE TABLE OF syst_ucomm.

  FREE: t_ucomm.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  READ TABLE it_sel_rows   INTO wa_sel_rows   INDEX 1.
  READ TABLE it_saida_0100 INTO wa_saida_0100 INDEX wa_sel_rows-index.

  SELECT SINGLE *
    FROM zsdt0127
    INTO @DATA(w_0127)
   WHERE chave = @wa_saida_0100-chave.

  IF sy-subrc <> 0.
    CLEAR w_0127.
  ENDIF.

  CREATE OBJECT zcl_manifesto_dest
    EXPORTING
      i_chave         = w_0127-chave
      i_doc_manifesto = w_0127-doc_manifesto.

  DATA(l_anula_rejeicao) = zcl_manifesto_dest->set_valida_anulacao_rejeicao( w_0127 ).

  IF l_anula_rejeicao = abap_off.
    APPEND 'ANULAR_REJECT'  TO t_ucomm.
  ENDIF.

  DATA(l_anula_conf_operacao) = zcl_manifesto_dest->set_valida_anulacao_conf_oper( w_0127 ).

  IF l_anula_conf_operacao = abap_off.
    APPEND 'ANULAR_CONF_OP'  TO t_ucomm.
  ENDIF.
*#127333 - 04.12.2023 - JT - fim

  SET PF-STATUS 'PF0105' EXCLUDING t_ucomm.  "*#127333 - 04.12.2023 - JT - inicio
  SET TITLEBAR 'T0105'.

  LOOP AT SCREEN.
    CASE screen-name.
      WHEN 'ZSDT0127-JUSTIFICATIVA' OR 'TXT_JUST'.
        IF zsdt0127-cd_operacao   EQ '210240'.  "Justificativa é obrigatário para a evento 210240 ( Operação não Realizada ).
          screen-active = 1.
        ELSE.
          screen-active = 0.
        ENDIF.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.


ENDMODULE.

MODULE pbo_0105 OUTPUT.

*---> CS0993761 / IR096524
  DATA: v_restr(1) TYPE c.

  IMPORT w_restr TO v_restr FROM MEMORY ID 'Restric'.
*  3 = Migo/Miro Branco
*  1 = MIGO Branco
*  2 = MIRO Branco
*  USER_command_0100
*<--- CS0993761 / IR096524

*-CS2022000243-#76365-26.04.2022-JT-inicio
  CLEAR: it_value[].

  IF p_modelo = '57'.
    wa_value-key  = '610110'.
    wa_value-text = 'Desacordo de Entrega de Serviços (CT-e)'.
    APPEND wa_value TO it_value.
  ELSE.
    IF v_restr EQ 3.
      wa_value-key  = '000000'.
      wa_value-text = 'Sem Manifesto'.
      APPEND wa_value TO it_value.
    ENDIF.

    IF v_restr EQ 3 OR v_restr EQ 1 OR v_restr EQ 2 OR v_restr EQ 0. "CS0993761 / IR096524 / CS1090972 IR136911
      wa_value-key  = '210210'.
      wa_value-text = 'Ciência da Operação'.
      APPEND wa_value TO it_value.
    ENDIF.

    IF v_restr EQ 3 OR v_restr EQ 1 OR v_restr EQ 2 OR v_restr EQ 0. "CS0993761 / IR096524 / CS1090972 IR136911
      wa_value-key  = '210200'.
      wa_value-text = 'Confirmação da Operação'.
      APPEND wa_value TO it_value.
    ENDIF.

    IF v_restr EQ 3.
      wa_value-key  = '210240'.
      wa_value-text = 'Operação não Realizada'.
      APPEND wa_value TO it_value.
    ENDIF.

    IF v_restr EQ 3.
      wa_value-key  = '210220'.
      wa_value-text = 'Desconhecimento da Operação'.
      APPEND wa_value TO it_value.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'P_ST_MNF'
      values = it_value.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'ZSDT0127-CD_OPERACAO'
      values = it_value.
*-CS2022000243-#76365-26.04.2022-JT-fim

  IF obj_container_0105 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_field_catalog USING '0105'.

    CREATE OBJECT obj_container_0105
      EXPORTING
        container_name = 'CC_ALV_0105'.

    CREATE OBJECT obj_alv_0105
      EXPORTING
        i_parent = obj_container_0105.

    "Configurar Timer
    CREATE OBJECT ob_timer
      EXPORTING
        parent = obj_container_0105.

    CREATE OBJECT ob_recev.

    SET HANDLER ob_recev->handle_finished FOR ob_timer.

    ob_timer->interval = 10.
    "Fim Config. Timer


    CREATE OBJECT obj_toolbar_0105
      EXPORTING
        io_alv_grid = obj_alv_0105.

    gs_layout-zebra    = 'X'.
    gs_variant-report  = sy-repid.

    SET HANDLER: obj_toolbar_0105->on_toolbar          FOR obj_alv_0105,
                 obj_toolbar_0105->handle_user_command FOR obj_alv_0105.

    PERFORM f_exclude_code USING '0105'.

    CALL METHOD obj_alv_0105->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant_c
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0105.

    SET HANDLER lcl_event_handler_0105->handle_hotspot_click FOR obj_alv_0105.

    CALL METHOD obj_alv_0105->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_0105->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF ( vg_row IS NOT INITIAL ) AND ( vg_col IS NOT INITIAL ).
    CALL METHOD obj_alv_0105->set_current_cell_via_id
      EXPORTING
        is_row_id    = vg_row
        is_column_id = vg_col.

    READ TABLE it_saida_0105 INTO wa_saida_0105 INDEX vg_row-index.
    IF sy-subrc = 0.
      zsdt0127-msg_retorno = wa_saida_0105-msg_retorno.
      zsdt0127-authcode    = wa_saida_0105-authcode.
      zsdt0127-dt_authcod  = wa_saida_0105-dt_authcod.
      zsdt0127-hr_authcod  = wa_saida_0105-hr_authcod.
      zsdt0127-code        = wa_saida_0105-code.
      zsdt0127-autorizado  = wa_saida_0105-autorizado.
    ENDIF.

  ELSE.
    CLEAR: zsdt0127-msg_retorno, zsdt0127-authcode, zsdt0127-dt_authcod,
           zsdt0127-hr_authcod,  zsdt0127-code ,zsdt0127-autorizado.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           ZSDR0117_PBO
*&---------------------------------------------------------------------*


MODULE pbo_0100 OUTPUT.

  IF obj_alv_0100_01 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0100_01'.



    IF obj_container_0100_01 IS INITIAL.
      CREATE OBJECT obj_container_0100_01
        EXPORTING
          container_name = 'CC_ALV_0100_01'.
    ENDIF.

    CREATE OBJECT obj_alv_0100_01
      EXPORTING
        i_parent = obj_container_0100_01.

    CREATE OBJECT obj_toolbar_0100_01
      EXPORTING
        io_alv_grid = obj_alv_0100_01.


    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.


    SET HANDLER: obj_toolbar_0100_01->on_toolbar                      FOR obj_alv_0100_01,
                 obj_toolbar_0100_01->handle_user_command             FOR obj_alv_0100_01,
                 lcl_event_handler_0100_01=>catch_hotspot             FOR obj_alv_0100_01,
                 lcl_event_handler_0100_01=>on_data_changed_finished  FOR obj_alv_0100_01.

    PERFORM f_exclude_fcode USING '0100_01'.

    CALL METHOD obj_alv_0100_01->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0100_01.

    CALL METHOD obj_alv_0100_01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0100_01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.

    CALL METHOD obj_alv_0100_01->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

  IF obj_alv_0100_02 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0100_02'.



    IF obj_container_0100_02 IS INITIAL.
      CREATE OBJECT obj_container_0100_02
        EXPORTING
          container_name = 'CC_ALV_0100_02'.
    ENDIF.

    CREATE OBJECT obj_alv_0100_02
      EXPORTING
        i_parent = obj_container_0100_02.

    CREATE OBJECT obj_toolbar_0100_02
      EXPORTING
        io_alv_grid = obj_alv_0100_02.

    gs_layout-sel_mode    = 'A'.
    gs_layout-stylefname  = 'CELLTAB'. "Adicionado CS2020001194
    gs_variant-report     = sy-repid.

    SET HANDLER: obj_toolbar_0100_02->on_toolbar                     FOR obj_alv_0100_02,
                 obj_toolbar_0100_02->handle_user_command            FOR obj_alv_0100_02,
                 lcl_event_handler_0100_02=>catch_hotspot            FOR obj_alv_0100_02,
                 lcl_event_handler_0100_02=>on_data_changed_finished FOR obj_alv_0100_02,
                 lcl_event_handler_0100_02=>handle_data_changed      FOR obj_alv_0100_02."Adicionado CS2020001194

    PERFORM f_exclude_fcode USING '0100_02'.

    CALL METHOD obj_alv_0100_02->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0100_02.


    CALL METHOD obj_alv_0100_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0100_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.

    PERFORM f_mostrar_cpos_mercado_interno  USING '0100_02'.

    CALL METHOD obj_alv_0100_02->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.

ENDMODULE.


MODULE pbo_0120 OUTPUT.

  IF obj_container_0120 IS INITIAL.

    CREATE OBJECT obj_container_0120
      EXPORTING
        container_name = 'CC_ALV_0120'.

    CREATE OBJECT obj_splitter_0120
      EXPORTING
        parent  = obj_container_0120
        rows    = 1
        columns = 2.

    "Set Column Width
    wg_refresh_splitter = abap_true.

    CALL METHOD obj_splitter_0120->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = obj_container_0120_01.

    CALL METHOD obj_splitter_0120->get_container
      EXPORTING
        row       = 1
        column    = 2
      RECEIVING
        container = obj_container_0120_02.
  ENDIF.

  IF ( obj_splitter_0120 IS NOT INITIAL ) AND ( wg_refresh_splitter EQ abap_true ).

    IF ( wg_cab_boletim_prod-id_boletim IS NOT INITIAL ) AND ( wg_cab_boletim_prod-com_nf EQ abap_true ).

      CALL METHOD obj_splitter_0120->set_column_width
        EXPORTING
          id    = 1
          width = 50.

      CALL METHOD obj_splitter_0120->set_column_width
        EXPORTING
          id    = 2
          width = 50.

    ELSE.

      CALL METHOD obj_splitter_0120->set_column_width
        EXPORTING
          id    = 1
          width = 0.

      CALL METHOD obj_splitter_0120->set_column_width
        EXPORTING
          id    = 2
          width = 100.

    ENDIF.

    wg_refresh_splitter = abap_false.

  ENDIF.

  IF ( obj_alv_0120_01 IS INITIAL ) AND ( wg_cab_boletim_prod-id_boletim IS NOT INITIAL ) AND ( wg_cab_boletim_prod-com_nf EQ abap_true ).

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0120_01'.

    "CREATE OBJECT OBJ_CONTAINER_0120_01
    "  EXPORTING
    "    CONTAINER_NAME = 'CC_ALV_0120_01'.

    CREATE OBJECT obj_alv_0120_01
      EXPORTING
        i_parent = obj_container_0120_01.

    CREATE OBJECT obj_toolbar_0120_01
      EXPORTING
        io_alv_grid = obj_alv_0120_01.

    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar_0120_01->on_toolbar             FOR obj_alv_0120_01,
                 obj_toolbar_0120_01->handle_user_command    FOR obj_alv_0120_01,
                 lcl_event_handler_0120_01=>catch_hotspot    FOR obj_alv_0120_01.

    PERFORM f_exclude_fcode USING '0120_01'.

    CALL METHOD obj_alv_0120_01->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0120_01.


    CALL METHOD obj_alv_0120_01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0120_01->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSEIF obj_alv_0120_01 IS NOT INITIAL.
    CALL METHOD obj_alv_0120_01->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

  IF obj_alv_0120_02 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0120_02'.

    "CREATE OBJECT OBJ_CONTAINER_0120_02
    "  EXPORTING
    "    CONTAINER_NAME = 'CC_ALV_0120_02'.

    CREATE OBJECT obj_alv_0120_02
      EXPORTING
        i_parent = obj_container_0120_02.

    CREATE OBJECT obj_toolbar_0120_02
      EXPORTING
        io_alv_grid = obj_alv_0120_02.

    gs_layout-sel_mode   = 'A'.
    gs_layout-ctab_fname = 'COLOR'.

    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.

    SET HANDLER: obj_toolbar_0120_02->on_toolbar             FOR obj_alv_0120_02,
                 obj_toolbar_0120_02->handle_user_command    FOR obj_alv_0120_02,
                 lcl_event_handler_0120_02=>catch_hotspot    FOR obj_alv_0120_02.

    PERFORM f_exclude_fcode USING '0120_02'.

    CALL METHOD obj_alv_0120_02->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0120_02.


    CALL METHOD obj_alv_0120_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0120_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_0120_02->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.

MODULE status_0120 OUTPUT.

  PERFORM f_config_exc_tcode_0120.

  SET PF-STATUS 'PF0120' EXCLUDING tg_exc_tcode_0120.

  PERFORM f_set_titlebar.

ENDMODULE.

MODULE status_0100 OUTPUT.

  PERFORM f_config_exc_tcode_0100.

  SET PF-STATUS 'PF0100' EXCLUDING tg_exc_tcode_0100.

  PERFORM f_set_titlebar.

ENDMODULE.

MODULE config_fields_0100 OUTPUT.

*--------------------------------------------------------------------------*
* Tratativas por Operação do Boletim
*--------------------------------------------------------------------------*
  LOOP AT SCREEN.

    CASE vg_operacao_bol.
      WHEN c_new_bol OR c_edit_bol.
        CASE screen-group1.
          WHEN 'CB'.
            screen-input = 1.
            MODIFY SCREEN.
        ENDCASE.
      WHEN OTHERS.

        IF screen-name EQ 'WG_CAB_BOLETIM_PROD-ID_BOLETIM'.

          IF wg_cab_boletim_prod-id_boletim IS INITIAL.
            screen-input = 1.
          ELSE.
            screen-input = 0.
          ENDIF.

          MODIFY SCREEN.

        ENDIF.

        CASE screen-group1.
          WHEN 'CB'.
            screen-input = 0.
            MODIFY SCREEN.
        ENDCASE.
    ENDCASE.

    IF vg_operacao_bol EQ c_edit_bol.
      IF screen-name EQ 'WG_CAB_BOLETIM_PROD-BRANCH'.
        screen-input  = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.

*-CS2021000386 - 28.04.2021 - JT - inicio
    IF vg_operacao_bol EQ c_edit_bol OR
       vg_operacao_bol EQ c_new_bol.
      IF screen-name EQ 'WG_CAB_BOLETIM_PROD-CATEG_SOJA'.
        IF wg_cab_boletim_prod-branch IS INITIAL.
          screen-input  = 0.
          MODIFY SCREEN.
        ELSE.
          SELECT SINGLE emissao_nf
                   INTO @DATA(l_emissao_nf)
                   FROM zsdt0253
                  WHERE branch = @wg_cab_boletim_prod-branch.
          IF l_emissao_nf = abap_true.
            screen-input  = 1.
            MODIFY SCREEN.
          ELSE.
            IF tp_boletim EQ space.
              screen-input  = 0.
              wg_cab_boletim_prod-categ_soja = 'RR'.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
*-CS2021000386 - 28.04.2021 - JT - inicio

    IF tp_boletim NE space.
      IF screen-name EQ 'WG_CAB_BOLETIM_PROD-CHARG' OR screen-name EQ 'WG_CAB_BOLETIM_PROD-QTDE_SI' AND tp_boletim NE '01'.
        screen-input  = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.


    "Se for boletim de produção farelo e óleo, mostrar os campos de saldo a vincular, se for diferente, esconder.
    IF b_farelo IS INITIAL.
      CASE screen-group1.
        WHEN 'GR5'.
          screen-active = 0.
          MODIFY SCREEN.
      ENDCASE.
    ENDIF.
  ENDLOOP.

  CASE vg_operacao_bol.
    WHEN c_new_bol OR c_edit_bol.

    WHEN OTHERS.

      LOOP AT SCREEN.
        IF screen-name EQ 'BTN_MIN_DETAIL_BOL'.

          IF wg_cab_boletim_prod-id_boletim IS NOT INITIAL.
            screen-active  = '1'.
          ELSE.
            screen-active  = '0'.
          ENDIF.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.
  ENDCASE.

*  CASE tp_boletim.
*    WHEN '02' OR '03'.
*      LOOP AT SCREEN.
*        IF screen-group1 EQ 'GR3'.
*          screen-active  = '0'.
*        ENDIF.
*        MODIFY SCREEN.
*      ENDLOOP.
*
*    WHEN OTHERS.
*      LOOP AT SCREEN.
*        IF screen-group1 EQ 'GR4'.
*          screen-active  = '0'.
*        ENDIF.
*        MODIFY SCREEN.
*      ENDLOOP.
*  ENDCASE.


ENDMODULE.

MODULE pbo_0121 OUTPUT.

  SET PF-STATUS 'PF0121'.
  SET TITLEBAR 'T0121'.

  IF obj_alv_0121 IS INITIAL.

    PERFORM f_refresh_objetos.
    PERFORM f_criar_catalog USING '0121'.

    IF obj_container_0121 IS INITIAL.
      CREATE OBJECT obj_container_0121
        EXPORTING
          container_name = 'CC_ALV_0121'.
    ENDIF.

    CREATE OBJECT obj_alv_0121
      EXPORTING
        i_parent = obj_container_0121.

    CREATE OBJECT obj_toolbar_0121
      EXPORTING
        io_alv_grid = obj_alv_0121.


    gs_layout-sel_mode   = 'A'.
    gs_variant-report  = sy-repid.
    wa_stable-row         = 'X'.
    wa_stable-col         = 'X'.


    SET HANDLER: obj_toolbar_0121->on_toolbar                      FOR obj_alv_0121,
                 obj_toolbar_0121->handle_user_command             FOR obj_alv_0121,
                 lcl_event_handler_0121=>catch_hotspot             FOR obj_alv_0121,
                 lcl_event_handler_0121=>on_data_changed_finished  FOR obj_alv_0121.

    PERFORM f_exclude_fcode USING '0121'.

    CALL METHOD obj_alv_0121->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_0121.

    CALL METHOD obj_alv_0121->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD obj_alv_0121->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_0121->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0124  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0124 OUTPUT.

  SET PF-STATUS 'PF0124'.
  SET TITLEBAR 'T0124'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CONFIG_FIELDS_0124  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE config_fields_0124 OUTPUT.

*--------------------------------------------------------------------------*
* Tratativas por Operação do Boletim
*--------------------------------------------------------------------------*
  LOOP AT SCREEN.
    CASE vg_operacao_bol.
      WHEN c_new_bol OR c_edit_bol.
        CASE screen-group1.
          WHEN 'CB'.
            screen-input = 1.
            MODIFY SCREEN.
        ENDCASE.
      WHEN OTHERS.

        IF screen-name EQ 'WG_CAB_BOLETIM_PROD-ID_BOLETIM'.

          IF wg_cab_boletim_prod-id_boletim IS INITIAL.
            screen-input = 1.
          ELSE.
            screen-input = 0.
          ENDIF.

          MODIFY SCREEN.

        ENDIF.

        CASE screen-group1.
          WHEN 'CB'.
            screen-input = 0.
            MODIFY SCREEN.
        ENDCASE.
    ENDCASE.

*-CS2021000386 - 28.04.2021 - JT - inicio
    IF vg_operacao_bol EQ c_edit_bol OR
       vg_operacao_bol EQ c_new_bol.
      IF screen-name EQ 'WG_CAB_BOLETIM_PROD-CATEG_SOJA'.
        IF wg_cab_boletim_prod-branch IS INITIAL.
          screen-input  = 0.
          MODIFY SCREEN.
        ELSE.
          SELECT SINGLE emissao_nf
          INTO l_emissao_nf
          FROM zsdt0253
          WHERE branch = wg_cab_boletim_prod-branch.
          IF l_emissao_nf = abap_true.
            screen-input  = 1.
            MODIFY SCREEN.
          ELSE.
            IF tp_boletim EQ '01'.
              screen-input  = 0.
              wg_cab_boletim_prod-categ_soja = 'RR'.
              MODIFY SCREEN.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDMODULE.

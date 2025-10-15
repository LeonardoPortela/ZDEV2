*&---------------------------------------------------------------------*
*&  Include           ZIM12_O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'Z001'.
  SET TITLEBAR 'Z001'.
ENDMODULE.

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
        wl_function LIKE tl_function  WITH HEADER LINE.

  IF grid1 IS NOT INITIAL.
    CALL METHOD grid1->free.
  ENDIF.

  IF splitter IS NOT INITIAL.
    CALL METHOD splitter->free.
  ENDIF.
  IF g_custom_container IS NOT INITIAL.
    CALL METHOD g_custom_container->free.
  ENDIF.

  FREE: grid1,obg_toolbar.
  FREE: g_custom_container,splitter.

  IF g_custom_container IS INITIAL.
    wa_layout-cwidth_opt  = c_x.
    wa_layout-zebra       = c_x.
    wa_layout-no_rowmark  = space.
*   wa_layout-col_opt     = c_x.
    wa_stable-row         = c_x.
    wa_layout-sel_mode    = 'A'.
    wa_layout-box_fname   = 'MARK'.
    wa_layout-stylefname  = 'STYLE'.
*   wa_layout-cwidth_opt  = 'X'.     "  Otimizar colunas na tela
    wa_layout-ctab_fname  = 'CELLCOLOR'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.

    CALL METHOD splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = container_1.

    CREATE OBJECT grid1
      EXPORTING
        i_parent = container_1.

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

    CLEAR wa_layout-cwidth_opt.
    gs_variant_c-report = sy-repid. "Enable users save own LAYOUTs

    if r_mapa  = 'X'.
       wa_layout-grid_title = 'Exportar MAPA'.
      PERFORM f_montar_layout_mapa USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_conf2[].
    elseIF r_zim01 = 'X'.
      wa_layout-grid_title = 'Exportar ZIM01'.
      PERFORM f_montar_layout_zim01 USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_zim01[].
    ELSEIF r_zim02 = 'X'.
      wa_layout-grid_title = 'Atualizar ZIM02'.
      PERFORM f_montar_layout_zim02 USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_zim02[].
    ELSEIF r_dre = 'X'.
      wa_layout-grid_title = 'Exportar DRE'.
      PERFORM f_montar_layout_dre USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_dre[].
    ELSEIF r_ksb1 = 'X'.
      wa_layout-grid_title = 'Exportar KSB1'.
      PERFORM f_montar_layout_ksb1 USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_ksb1[].
    ELSEIF r_inves = 'X'.
      wa_layout-grid_title = 'Exportar Investimentos'.
      PERFORM f_cores_colunas.
      PERFORM f_montar_layout_inves USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_inves[].
    ELSEIF r_hcm = 'X'.
      wa_layout-grid_title = 'Exportar Funções folha'.
      PERFORM f_montar_layout_hcm USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_hcm[].
    ELSEIF r_hcm2 = 'X'.
      wa_layout-grid_title = 'Exportar Funcionários folha'.
      PERFORM f_montar_layout_hcm2 USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_hcm2[].
    ELSEIF r_kp06 = 'X'.
      wa_layout-grid_title = 'Atualiza KP06'.
      PERFORM f_montar_layout_kp06 USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_kp06[].

*-CS2022000122 - 07.03.2022 - JT - inicio
    ELSEIF r_gl056 = 'X'.
      wa_layout-grid_title = 'Exportar ZGL056'.
      wa_layout-cwidth_opt = abap_true.

      PERFORM f_montar_layout_zgl056 USING c_x.

      CALL METHOD grid1->set_table_for_first_display
        EXPORTING
          is_variant           = gs_variant_c
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
          i_save               = 'A'
        CHANGING
          it_fieldcatalog      = t_fieldcatalog[]
          it_outtab            = it_zgl056[].
*-CS2022000122 - 07.03.2022 - JT - fim
    ENDIF.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD grid1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    SET HANDLER:
      lcl_event_handler=>on_double_click          FOR grid1,
      lcl_event_handler=>on_data_changed_finished FOR grid1,
      lcl_event_handler=>on_data_changed          FOR grid1,
      lcl_event_handler=>on_onf4                  FOR grid1.


*    posiciona spliter na altura x
    CALL METHOD splitter->set_row_height
      EXPORTING
        id     = 1
        height = 100.

  ENDIF.


ENDMODULE.

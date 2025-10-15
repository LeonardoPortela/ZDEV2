*----------------------------------------------------------------------*
***INCLUDE ZFIR0067_PBO.
*----------------------------------------------------------------------*

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.
ENDMODULE.

MODULE criar_objetos OUTPUT.

  PERFORM: create_container_alv_tree,
           iniciar_tree.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0101 OUTPUT.

  IF obj_alv_mov_flx IS INITIAL.

    PERFORM refresh_objetos.

    PERFORM f_get_dias_mov USING ''. "Carrega todos os dias Movimento
    PERFORM f_get_dias_mov USING 'X'."Carrega somente dias de Movimento para o Registro Avulso

    PERFORM criar_field_catalog_0101.

    IF obj_container_mov_flx IS INITIAL.
      CREATE OBJECT obj_container_mov_flx
        EXPORTING
          container_name = 'CC_MOV_FLX'.
    ENDIF.

    CREATE OBJECT obj_alv_mov_flx
      EXPORTING
        i_parent = obj_container_mov_flx.

    gs_layout-zebra      = 'X'.
    gs_variant-report  = sy-repid.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    IF it_fcat[] IS NOT INITIAL.

      CALL METHOD obj_alv_mov_flx->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
          is_variant           = gs_variant
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_saida_mov_flx.

      CALL METHOD obj_alv_mov_flx->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      SET HANDLER: lcl_event_handler_0101=>on_f4                    FOR obj_alv_mov_flx,
                   lcl_event_handler_0101=>on_data_changed_finished FOR obj_alv_mov_flx,
                   lcl_event_handler_0101=>handle_hotspot_click     FOR obj_alv_mov_flx.

      CALL METHOD obj_alv_mov_flx->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.

    ENDIF.

  ELSE.
    CALL METHOD obj_alv_mov_flx->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0101  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  SET PF-STATUS 'PF0101'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0102  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0102 OUTPUT.
  SET PF-STATUS 'PF0102'.
  SET TITLEBAR 'T0102'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0103 OUTPUT.
  SET PF-STATUS 'PF0103'.
  SET TITLEBAR 'T0103'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS_0103  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0103 OUTPUT.

  IF obj_alv_var_v1 IS INITIAL.

    PERFORM refresh_objetos.

    PERFORM criar_field_catalog_0103 USING '1'.

    IF obj_container_var_v1 IS INITIAL.
      CREATE OBJECT obj_container_var_v1
        EXPORTING
          container_name = 'CC_VERSAO_1'.
    ENDIF.

    CREATE OBJECT obj_alv_var_v1
      EXPORTING
        i_parent = obj_container_var_v1.

    SET HANDLER: lcl_event_handler_0103v1=>handle_hotspot_click  FOR obj_alv_var_v1.

    gs_layout-zebra      = 'X'.
    gs_variant-report  = sy-repid.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD obj_alv_var_v1->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_var1.

    CALL METHOD obj_alv_var_v1->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_var_v1->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


  IF obj_alv_var_v2 IS INITIAL.

    PERFORM refresh_objetos.

    PERFORM criar_field_catalog_0103 USING '2'.

    IF obj_container_var_v2 IS INITIAL.
      CREATE OBJECT obj_container_var_v2
        EXPORTING
          container_name = 'CC_VERSAO_2'.
    ENDIF.

    CREATE OBJECT obj_alv_var_v2
      EXPORTING
        i_parent = obj_container_var_v2.

    SET HANDLER: lcl_event_handler_0103v2=>handle_hotspot_click  FOR obj_alv_var_v2.

    gs_layout-zebra      = 'X'.
    gs_variant-report  = sy-repid.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD obj_alv_var_v2->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_var2.

    CALL METHOD obj_alv_var_v2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_var_v2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.

MODULE status_0104 OUTPUT.
  SET PF-STATUS 'PF0104'.
  SET TITLEBAR 'T0104'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS_0104  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0104 OUTPUT.

  IF obj_alv_vproc IS INITIAL.

    PERFORM refresh_objetos.
    PERFORM criar_field_catalog_0104.

    IF obj_container_vproc IS INITIAL.
      CREATE OBJECT obj_container_vproc
        EXPORTING
          container_name = 'CC_VERSAO_PROC'.
    ENDIF.

    CREATE OBJECT obj_alv_vproc
      EXPORTING
        i_parent = obj_container_vproc.

    gs_layout-zebra      = 'X'.
    gs_variant-report  = sy-repid.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD obj_alv_vproc->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_vproc.

    CALL METHOD obj_alv_vproc->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD obj_alv_vproc->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_SCREEN  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_screen OUTPUT.

  qd_versao_1 = wa_saida_mov_flx-txt_vrs1.
  qd_versao_2 = wa_saida_mov_flx-txt_vrs2.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0105 OUTPUT.

  IF obj_alv_bmov IS INITIAL.

    PERFORM refresh_objetos.
    PERFORM criar_field_catalog_0105.

    IF obj_container_bmov IS INITIAL.
      CREATE OBJECT obj_container_bmov
        EXPORTING
          container_name = 'CC_BLOQ_MOV'.
    ENDIF.

    CREATE OBJECT obj_alv_bmov
      EXPORTING
        i_parent = obj_container_bmov.

    gs_layout-zebra      = 'X'.
    gs_variant-report  = sy-repid.

    CREATE OBJECT obj_toolbar_bmov
      EXPORTING
        io_alv_grid = obj_alv_bmov.

    SET HANDLER: obj_toolbar_bmov->on_toolbar          FOR obj_alv_bmov,
                 obj_toolbar_bmov->handle_user_command FOR obj_alv_bmov.

    "WA_EXCLUDE_FCODE = CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW.
    "APPEND WA_EXCLUDE_FCODE TO IT_EXCLUDE_FCODE.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    CALL METHOD obj_alv_bmov->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida_bmov.

    CALL METHOD obj_alv_bmov->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD obj_alv_bmov->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.
    CALL METHOD obj_alv_bmov->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0105  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0105 OUTPUT.
  SET PF-STATUS 'PF0105'.
  SET TITLEBAR 'T0105'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0106  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0106 OUTPUT.
  SET PF-STATUS 'PF0106'.
  SET TITLEBAR 'T0106'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0107  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0107 OUTPUT.
  SET PF-STATUS 'PF0107'.
  SET TITLEBAR 'T0107'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS_0107  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos_0107 OUTPUT.

  IF obj_alv_ajuste IS INITIAL.

    PERFORM refresh_objetos.
    PERFORM criar_field_catalog_0107.

    IF obj_container_ajuste IS INITIAL.
      CREATE OBJECT obj_container_ajuste
        EXPORTING
          container_name = 'CC_MOV_AJUSTE'.
    ENDIF.

    CREATE OBJECT obj_alv_ajuste
      EXPORTING
        i_parent = obj_container_ajuste.

    gs_layout-zebra      = 'X'.
    gs_layout-ctab_fname = 'COLOR'.
    gs_variant-report  = sy-repid.

    CREATE OBJECT obj_toolbar_ajuste
      EXPORTING
        io_alv_grid = obj_alv_ajuste.

    SET HANDLER: obj_toolbar_ajuste->on_toolbar          FOR obj_alv_ajuste,
                 obj_toolbar_ajuste->handle_user_command FOR obj_alv_ajuste.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_move_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_undo.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_append_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_copy_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_cut.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_refresh.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    IF it_fcat[] IS NOT INITIAL.

      CALL METHOD obj_alv_ajuste->set_table_for_first_display
        EXPORTING
          is_layout            = gs_layout
          i_save               = 'A'
          it_toolbar_excluding = it_exclude_fcode
          is_variant           = gs_variant
        CHANGING
          it_fieldcatalog      = it_fcat
          it_outtab            = it_sai_ajuste.

      CALL METHOD obj_alv_ajuste->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      SET HANDLER: lcl_event_handler_0107=>on_data_changed_finished FOR obj_alv_ajuste,
                   lcl_event_handler_0107=>on_data_changed          FOR obj_alv_ajuste.

      CALL METHOD obj_alv_ajuste->set_ready_for_input
        EXPORTING
          i_ready_for_input = 1.

    ENDIF.

  ELSE.
    CALL METHOD obj_alv_ajuste->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.


ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0108  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0108 OUTPUT.
  SET PF-STATUS 'PF0108'.
*  SET TITLEBAR 'xxx'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0110  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0110 OUTPUT.

  SET PF-STATUS 'PF0110'.
  SET TITLEBAR 'T0110'.

  PERFORM zf_preparar_alv_list_vsr.

ENDMODULE.

*&---------------------------------------------------------------------*
*&  Include           ZFIR0065_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'T0100'.

ENDMODULE.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  CRIAR_OBJETOS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE criar_objetos OUTPUT.

  DATA: lt_sort TYPE lvc_t_sort.

  CLEAR gv_erro.

  IF obj_container IS INITIAL.

    PERFORM selecionar_dados.
    PERFORM refresh_objetos_alv.
    PERFORM criar_field_catalog.

    CREATE OBJECT obj_container
      EXPORTING
        container_name = 'CC_PARAMETROS'.

    CREATE OBJECT obj_alv
      EXPORTING
        i_parent = obj_container.

    CREATE OBJECT obj_toolbar
      EXPORTING
        io_alv_grid = obj_alv.

    gs_layout-zebra      = 'X'.
    gs_layout-stylefname = 'CELLTAB'.
    gs_variant-report  = sy-repid.

    SET HANDLER: obj_toolbar->on_toolbar          FOR obj_alv,
                 obj_toolbar->handle_user_command FOR obj_alv,
                 obj_toolbar->handle_data_changed FOR obj_alv.

    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = cl_gui_alv_grid=>mc_fc_loc_insert_row.
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

    CALL METHOD obj_alv->set_table_for_first_display
      EXPORTING
        is_layout            = gs_layout
        i_save               = 'A'
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = gs_variant
      CHANGING
        it_fieldcatalog      = it_fcat
        it_outtab            = it_saida.

    CALL METHOD obj_alv->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    REFRESH gt_f4.

    gt_f4-fieldname  = 'FKART'.
    gt_f4-register   = 'X'.
    gt_f4-getbefore  = 'X'.
    gt_f4-chngeafter = 'X'.
    APPEND gt_f4.

    CALL METHOD obj_alv->register_f4_for_fields
      EXPORTING
        it_f4 = gt_f4[].

    SET HANDLER: lcl_event_handler=>on_f4 FOR obj_alv.

    CALL METHOD obj_alv->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD obj_alv->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.

ENDMODULE.                 " CRIAR_OBJETOS  OUTPUT

FORM refresh_objetos_alv .
  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.                    " REFRESH_OBJETOS

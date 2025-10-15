*----------------------------------------------------------------------*
***INCLUDE ZFIS43_MONTA_ALVO01.
*----------------------------------------------------------------------*

module monta_alv_conf output.
  data: lt_fieldcat     type slis_t_fieldcat_alv,
        lt_fieldcat_aux type lvc_t_fcat.

  data: event       type cntl_simple_event,
        events      type cntl_simple_events,
        tl_filter   type lvc_t_filt,
        wl_filter   type lvc_s_filt,
        tl_function type ui_functions,
        wl_function like tl_function  with header line,
        lt_f4       type lvc_t_f4     with header line.

  if t_saida_conf is not initial.

    if g_custom_container is initial.

      create object g_custom_container
        exporting
          container_name = 'CC_CONF'.

      create object g_grid
        exporting
          i_parent = g_custom_container.
*§1.Set status of all cells to editable using the layout structure.
      gs_layout-edit = 'X'.
      gs_layout-stylefname = 'CELLTAB'.
*      gs_layout-box_fname  = 'MARK'.
      gs_layout-sel_mode   = 'A'.

      wa_stable-row         = 'X'.
      wa_stable-col         = 'X'.



      call function 'REUSE_ALV_FIELDCATALOG_MERGE'
        exporting
          i_program_name         = sy-repid
          i_structure_name       = 'ZFIS_FILIAL_CONF'
        changing
          ct_fieldcat            = lt_fieldcat
        exceptions
          inconsistent_interface = 1
          program_error          = 2
          others                 = 3.
      if sy-subrc = 0.

        loop at lt_fieldcat assigning field-symbol(<fs_fieldcat>).

          case <fs_fieldcat>-fieldname.
            when 'FILIAL'.
              <fs_fieldcat>-outputlen = '15'.
            when 'TIPO_NOTA'.
              <fs_fieldcat>-outputlen = '12'.
            when 'DIAS_CORTE'.
              <fs_fieldcat>-outputlen = '12'.
            when 'HORA_FIX_CONF'.
              <fs_fieldcat>-outputlen = '12'.
            when 'FLAG_APOS_REGISTRO'.
              <fs_fieldcat>-outputlen = '25'.
              <fs_fieldcat>-checkbox = abap_true.
            when others.
          endcase.

          append initial line to lt_fieldcat_aux assigning field-symbol(<fs_fieldcat_aux>).
          move-corresponding <fs_fieldcat> to <fs_fieldcat_aux>.

        endloop.

      endif.

      create object obg_toolbar
        exporting
          io_alv_grid = g_grid.


      set handler obg_toolbar->on_toolbar for g_grid.
      set handler obg_toolbar->handle_user_command for g_grid.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      append wl_function to tl_function.
*      wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_check.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_refresh.
      append wl_function to tl_function.

      call method g_grid->set_table_for_first_display
        exporting
          it_toolbar_excluding = tl_function
          i_structure_name     = 'ZFIS_FILIAL_CONF'
          is_layout            = gs_layout
        changing
          it_outtab            = t_saida_conf
          it_fieldcatalog      = lt_fieldcat_aux.

*      call method g_grid->set_ready_for_input
*        exporting
*          i_ready_for_input = 1.

      call method g_grid->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      call method g_grid->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    endif.

  endif.

endmodule.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
module status_0100 output.
  set pf-status 'STATUS_PADRAO'.

  if rb_conf is not initial.
    set titlebar 'FILIA_CONF'.
  elseif rb_desc is not initial.
    set titlebar 'FILIA_DESC'.
  endif.

endmodule.

module monta_alv_desc output.

  free: lt_fieldcat,
        lt_fieldcat_aux.

  if t_saida_desc is not initial.

    if g_custom_container is initial.

      create object g_custom_container
        exporting
          container_name = 'CC_CONF'.

      create object g_grid
        exporting
          i_parent = g_custom_container.
*§1.Set status of all cells to editable using the layout structure.
      gs_layout-edit = 'X'.
      gs_layout-stylefname = 'CELLTAB'.

      wa_stable-row         = 'X'.
      wa_stable-col         = 'X'.

      call function 'REUSE_ALV_FIELDCATALOG_MERGE'
        exporting
          i_program_name         = sy-repid
          i_structure_name       = 'ZFIS_FILIAL_DESC'
        changing
          ct_fieldcat            = lt_fieldcat
        exceptions
          inconsistent_interface = 1
          program_error          = 2
          others                 = 3.
      if sy-subrc = 0.

        loop at lt_fieldcat assigning <fs_fieldcat>.

          case <fs_fieldcat>-fieldname.
            when 'BRANCH'.
              <fs_fieldcat>-outputlen = '20'.
            when 'DIAS_DESCON'.
              <fs_fieldcat>-outputlen = '20'.
            when 'DATA_FIXA'.
              <fs_fieldcat>-outputlen = '20'.
            when 'DIAS_ENVIO_EMAIL'.
              <fs_fieldcat>-outputlen = '20'.
            when others.
          endcase.

          append initial line to lt_fieldcat_aux assigning <fs_fieldcat_aux>.
          move-corresponding <fs_fieldcat> to <fs_fieldcat_aux>.

        endloop.

      endif.


      create object obg_toolbar
        exporting
          io_alv_grid = g_grid.

      set handler obg_toolbar->on_toolbar for g_grid.
      set handler obg_toolbar->handle_user_command2 for g_grid.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      append wl_function to tl_function.
*      wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
*      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_undo.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_loc_cut.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_check.
      append wl_function to tl_function.
      wl_function = cl_gui_alv_grid=>mc_fc_refresh.
      append wl_function to tl_function.

      call method g_grid->set_table_for_first_display
        exporting
          it_toolbar_excluding = tl_function
          i_structure_name = 'ZFIS_FILIAL_DESC'
          is_layout        = gs_layout
        changing
          it_outtab        = t_saida_desc
          it_fieldcatalog  = lt_fieldcat_aux.


*      call method g_grid->set_ready_for_input
*        exporting
*          i_ready_for_input = 1.

      call method g_grid->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      call method g_grid->register_edit_event
        exporting
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    endif.

  endif.

endmodule.

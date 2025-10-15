*&---------------------------------------------------------------------*
*&  Include           ZLESR0102_PBO
*&---------------------------------------------------------------------*

MODULE pbo_0100 OUTPUT.

  IF sy-tcode = 'ZLES0136' OR sy-tcode = 'ZMM0127'.

    IF cl_container_95 IS INITIAL.
      CREATE OBJECT cl_container_95
        EXPORTING
          side  = '4'
          ratio = '80'.
    ENDIF.

    IF cl_grid IS INITIAL.

      PERFORM f_config_layout USING '0100'.

      CREATE OBJECT obj_dyndoc_id
        EXPORTING
          no_margins = 'X'.

      PERFORM f_alv_header .

      IF editcontainer IS INITIAL .
        CREATE OBJECT editcontainer
          EXPORTING
            container_name = 'HEADER'.
      ENDIF .

      CALL METHOD obj_dyndoc_id->merge_document.

      CALL METHOD obj_dyndoc_id->display_document
        EXPORTING
          reuse_control      = 'X'
          parent             = editcontainer
        EXCEPTIONS
          html_display_error = 1.

      "Grafico 1
      CALL METHOD cl_gui_cfw=>flush.

      CREATE OBJECT: container
         EXPORTING
           container_name = 'CC_IMG',
           picture
         EXPORTING
           parent = container.

      PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

      CALL METHOD picture->load_picture_from_url
        EXPORTING
          url = url.

      CALL METHOD picture->set_display_mode
        EXPORTING
          display_mode = picture->display_mode_fit_center.

      CREATE OBJECT cl_grid
        EXPORTING
          i_parent = cl_container_95.

      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      SET HANDLER:
          lcl_event_handler=>catch_hotspot            FOR cl_grid,
          lcl_event_handler=>on_data_changed_finished FOR cl_grid,
          lcl_event_handler=>on_f4                    FOR cl_grid,
          lcl_event_handler=>set_toolbar              FOR cl_grid,
          lcl_event_handler=>handle_user_command      FOR cl_grid,
          lcl_event_handler=>on_data_changed          FOR cl_grid.

      PERFORM f_exclude_toolbar USING '0100'.

      gs_variant_c-report = sy-repid.

      CALL METHOD cl_grid->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding = tl_function
          is_layout            = wa_layout
        CHANGING
          it_fieldcatalog      = it_fieldcat[]
          it_sort              = i_sort[]
          it_outtab            = it_saida[].

      REFRESH gt_f4.
      gt_f4-fieldname = 'EBELN'.
      gt_f4-register = 'X'.
      gt_f4-getbefore = 'X'.
      gt_f4-chngeafter ='X'.
      APPEND gt_f4.

      gt_f4-fieldname = 'REGION'.
      gt_f4-register = 'X'.
      gt_f4-getbefore = 'X'.
      gt_f4-chngeafter ='X'.
      APPEND gt_f4.

      CALL METHOD cl_grid->register_f4_for_fields
        EXPORTING
          it_f4 = gt_f4[].

      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    ELSE.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    ENDIF.
  ELSE.                                                     "ZLES0137
    IF cl_container_95 IS INITIAL.
      CREATE OBJECT cl_container_95
        EXPORTING
          side  = '4'
          ratio = '80'.
    ENDIF.

    IF cl_grid IS INITIAL.

      PERFORM f_config_layout USING '0100'.

      CREATE OBJECT obj_dyndoc_id
        EXPORTING
          no_margins = 'X'.

*      PERFORM f_alv_header.

      IF editcontainer IS INITIAL .
        CREATE OBJECT editcontainer
          EXPORTING
            container_name = 'HEADER'.
      ENDIF .

      CALL METHOD obj_dyndoc_id->merge_document.

      CALL METHOD obj_dyndoc_id->display_document
        EXPORTING
          reuse_control      = 'X'
          parent             = editcontainer
        EXCEPTIONS
          html_display_error = 1.

      "Grafico 1
      CALL METHOD cl_gui_cfw=>flush.
      CREATE OBJECT: container
         EXPORTING
           container_name = 'CC_IMG',
           picture
         EXPORTING
           parent = container.

      PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

      CALL METHOD picture->load_picture_from_url
        EXPORTING
          url = url.

      CALL METHOD picture->set_display_mode
        EXPORTING
          display_mode = picture->display_mode_fit_center.

      CREATE OBJECT cl_grid
        EXPORTING
          i_parent = cl_container_95.

      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      PERFORM f_exclude_toolbar USING '0100'.

      gs_variant_c-report = sy-repid.
      CALL METHOD cl_grid->set_table_for_first_display
        EXPORTING
          it_toolbar_excluding = tl_function
          is_variant           = gs_variant_c          "Modificar Layout
          is_layout            = wa_layout
          i_save               = wg_save
        CHANGING
          it_fieldcatalog      = it_fieldcat[]        "i_default = 'X'
          it_sort              = i_sort[]
          it_outtab            = it_saida[].

      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_modified.

      REFRESH gt_f4.
      gt_f4-fieldname = 'REGION'.
      gt_f4-register = 'X'.
      gt_f4-getbefore = 'X'.
      gt_f4-chngeafter ='X'.
      APPEND gt_f4.

      CALL METHOD cl_grid->register_f4_for_fields
        EXPORTING
          it_f4 = gt_f4[].

      CALL METHOD cl_grid->register_edit_event
        EXPORTING
          i_event_id = cl_gui_alv_grid=>mc_evt_enter.

      SET HANDLER:
          lcl_event_handler=>catch_hotspot            FOR cl_grid,
          lcl_event_handler=>on_data_changed_finished FOR cl_grid,
          lcl_event_handler=>on_f4                    FOR cl_grid,
          lcl_event_handler=>set_toolbar              FOR cl_grid,
          lcl_event_handler=>handle_user_command      FOR cl_grid,
          lcl_event_handler=>on_data_changed          FOR cl_grid.
    ELSE.
      CALL METHOD cl_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
      wg_save = 'X'.
    ENDIF.
  ENDIF.                                                    "zles0106
ENDMODULE.                 " STATUS_0100  OUTPUT

MODULE status_0200 OUTPUT.
  SET PF-STATUS '0200'.
  SET TITLEBAR  '0200'.
  IF wa_saida-transp NE icon_execute_object.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'A1'.
        screen-active    = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

  wa_layout-zebra      = c_x.
  wa_layout-no_rowmark = c_x.
  wa_stable-row        = c_x.
  wa_layout-no_toolbar = c_x.

  wa_layout-grid_title = ' '.

  "GRID2
  IF obg_conteiner_veic IS INITIAL.
    CREATE OBJECT obg_conteiner_veic
      EXPORTING
        container_name = 'CC_VEIC'.


    CREATE OBJECT grid2
      EXPORTING
        i_parent = obg_conteiner_veic.


    PERFORM f_montar_layout_veic.

    REFRESH: tl_function.
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

*    wa_layout-cwidth_opt = c_x.
    wa_layout-no_toolbar = space.
*     WA_LAYOUT-COL_OPT    = C_X.
    wa_layout-grid_title = ''.
    wa_layout-no_toolbar = ''.


    CALL METHOD grid2->set_table_for_first_display
      EXPORTING
        is_layout            = wa_layout
        it_toolbar_excluding = tl_function
      CHANGING
        it_fieldcatalog      = it_fieldcat[]
        it_outtab            = it_veic[].

    CALL METHOD grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.
  ELSE.
    CALL METHOD grid2->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDIF.
ENDMODULE.                 " STATUS_0200  OUTPUT

MODULE cria_objetos OUTPUT.
  IF g_custom_cont_desc IS INITIAL.

    CREATE OBJECT g_custom_cont_desc
      EXPORTING
        container_name = g_descbox.


    CREATE OBJECT obg_descbox
      EXPORTING
        parent            = g_custom_cont_desc
        wordwrap_mode     = cl_gui_textedit=>wordwrap_at_fixed_position
        wordwrap_position = 72
        max_number_chars  = 1000.

    CALL METHOD obg_descbox->set_toolbar_mode
      EXPORTING
        toolbar_mode = '0'.
    CALL METHOD obg_descbox->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
  ELSE.
    CALL METHOD obg_descbox->set_text_as_r3table
      EXPORTING
        table = tg_editor.
    CALL METHOD obg_descbox->set_readonly_mode
      EXPORTING
        readonly_mode = 1.
  ENDIF.
ENDMODULE.                 " CRIA_OBJETOS  OUTPUT

MODULE status_0300 OUTPUT.
  SET PF-STATUS '0300'.
  SET TITLEBAR '0300'.
ENDMODULE.                 " STATUS_0300  OUTPUT

MODULE status_0100 OUTPUT.

  DATA: fcode  TYPE TABLE OF sy-ucomm.

  READ TABLE t_usermd WITH KEY from = sy-uname.
  IF sy-subrc NE 0.
    APPEND 'RECU' TO fcode.
  ENDIF.

  IF ( vg_cockpit NE '01' ) AND "Permissão Opção Recuperar Documentos
     ( vg_cockpit NE '04' ) AND
     ( vg_cockpit NE '05' ) AND
     ( vg_cockpit NE '06' ) AND
     ( vg_cockpit NE '07' ) AND
     ( vg_cockpit NE '09' ).

    APPEND 'CARTA' TO fcode.

  ENDIF.

*-CS2021000218-08.12.2022-#99735-JT-inicio
  IF vg_cockpit <> '06'.  "Insumos
    APPEND 'RA_ASSINA' TO fcode.
  ENDIF.
*-CS2021000218-08.12.2022-#99735-JT-fim

*-CS2023000189-26.05.2023-#108752-JT-inicio
  IF vg_cockpit <> '01'.  "Pesagem OPus-Saida
    APPEND 'ROM_ALGD'  TO fcode.
  ENDIF.
*-CS2023000189-26.05.2023-#108752-JT-fim

  "Faturamento Contingencia ECC - WPP
  SELECT SINGLE *
    FROM TVARVC INTO @DATA(LWA_TVARVC_FAT_CONTINGENCIA)
   WHERE NAME = 'FAT_CONTINGENCIA_GOLIVE_US'
     AND LOW = @SY-UNAME.
  IF SY-SUBRC NE 0.
    APPEND 'CK_FAT_ECC'  TO fcode.
  ENDIF.
  "Faturamento Contingencia ECC - WPP

  SET PF-STATUS 'F_SET_PF' EXCLUDING fcode.

  IF sy-tcode = 'ZLES0136' OR sy-tcode = 'ZMM0127'.

    CASE vg_cockpit. "Set Title Bar
      WHEN '01'.
        SET TITLEBAR  '0100_01'.
      WHEN '02'.
        SET TITLEBAR  '0100_02'.
      WHEN '03'.
        SET TITLEBAR  '0100_03'.
      WHEN '04'.
        SET TITLEBAR  '0100_04'.
      WHEN '05'.
        SET TITLEBAR  '0100_05'.
      WHEN '06'.
        SET TITLEBAR  '0100_06'.
      WHEN '07'.
        SET TITLEBAR  '0100_07'.
      WHEN '09'.
        SET TITLEBAR  '0100_09'.
      WHEN '10'.
        SET TITLEBAR  '0100_10'.
    ENDCASE.

  ELSE. " Consulta / ZLES0137

    CASE vg_cockpit." Set Title Bar Consulta
      WHEN '01'.
        SET TITLEBAR  '0100C_01'.
      WHEN '02'.
        SET TITLEBAR  '0100C_02'.
      WHEN '03'.
        SET TITLEBAR  '0100C_03'.
      WHEN '04'.
        SET TITLEBAR  '0100C_04'.
      WHEN '05'.
        SET TITLEBAR  '0100C_05'.
      WHEN '06'.
        SET TITLEBAR  '0100C_06'.
      WHEN '07'.
        SET TITLEBAR  '0100C_07'.
    ENDCASE.

    wg_save = 'X'.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0400  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0400 OUTPUT.
  SET PF-STATUS '0400'.
  SET TITLEBAR '0400'.
ENDMODULE.

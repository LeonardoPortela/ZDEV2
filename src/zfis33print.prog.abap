*&---------------------------------------------------------------------*
*&  Include           ZFIS33PRINT
*&---------------------------------------------------------------------*

class lcl_event_handler definition.

  public section.
    class-methods:
      on_double_click for event double_click of cl_gui_alv_grid
        importing e_row e_column.

    class-methods:
      on_data_changed for event data_changed of cl_gui_alv_grid
        importing er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    class-methods:
      on_data_changed_finished for event data_changed_finished of cl_gui_alv_grid
        importing e_modified et_good_cells.

    class-methods:
      on_button_click for event button_click of cl_gui_alv_grid
        importing es_col_id es_row_no.

    class-methods:
      on_onf4 for event onf4 of cl_gui_alv_grid
        importing e_fieldname e_fieldvalue es_row_no er_event_data
                  et_bad_cells e_display.

endclass.

class lcl_event_handler implementation.

  method on_double_click.
* IMPLEMENTAR METHOD DOUBEL_CLICK.
  endmethod.

  method on_data_changed.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value.

  endmethod.

  method on_data_changed_finished.

    data: ls_good  type lvc_s_modi,
          lv_value type lvc_value,
          vl_value type lvc_value.

    loop at et_good_cells into ls_good
      where tabix gt 0.

      if rd_cfop is not initial.
        read table it_saida_136 into wa_saida_136 index ls_good-row_id.
* TRATA CFOP PARA OBTER O CFOP LEGAL PSA
        call function 'CONVERSION_EXIT_CFOBR_OUTPUT'
          exporting
            input  = wa_saida_136-cfop
          importing
            output = cfop_aux.

        replace '/' with ' ' into cfop_aux.
        if  sy-subrc = 0.
          condense cfop_aux no-gaps.
        endif.
        "cfop_aux = wa_saida_136-cfop.

*        PERFORM VERIFICA_ERROS.

      elseif rd_filial_cc is not initial.
        read table it_saida_137 into wa_saida_137 index ls_good-row_id.
        werks_aux = wa_saida_137-werks.
        kostl_aux = wa_saida_137-kostl.

*        PERFORM VERIFICA_ERROS.

      elseif rd_filial_ctb is not initial.
        read table it_saida_151 into wa_saida_151 index ls_good-row_id.
        werks_aux = wa_saida_151-werks.
        saknr_aux = wa_saida_151-saknr.

*        PERFORM VERIFICA_ERROS.

      elseif rd_grupo_merc is not initial."Equalização ECC X HANA #108307 inicio  - SMC
        read table it_saida_195 into wa_saida_195 index ls_good-row_id.
        matkl_aux = wa_saida_195-matkl.
        "wgbez_AUX = WA_SAIDA_195-wgbez.

*        PERFORM VERIFICA_ERROS."Equalização ECC X HANA #108307 fim  - SMC
      endif.


    endloop.

  endmethod.

  method on_button_click.

    perform f_refresh_alv.

  endmethod.

  method on_onf4.
* Set code for help
  endmethod.

endclass.


class lcl_alv_toolbar definition.
  public section.
*CONSTRUCTOR
    methods: constructor
      importing io_alv_grid type ref to cl_gui_alv_grid,
*EVENT FOR TOOLBAR
      on_toolbar for event toolbar of cl_gui_alv_grid
        importing e_object,

      handle_user_command for event user_command of cl_gui_alv_grid
        importing e_ucomm.

endclass.

class lcl_alv_toolbar implementation.
  method constructor.

    create object c_alv_toolbarmanager
      exporting
        io_alv_grid = io_alv_grid.
  endmethod.

  method on_toolbar.
*    DATA: WL_DESACTIVE.

    clear ty_toolbar.
    ty_toolbar-icon       = icon_insert_row.
    ty_toolbar-function   = c_add.
    ty_toolbar-text       = 'Inserir Linha'.
    ty_toolbar-butn_type  = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

    ty_toolbar-icon       = icon_delete_row.
    ty_toolbar-function   = c_del.
    ty_toolbar-text       = 'Deletar'.
    ty_toolbar-butn_type  = 0.
    append ty_toolbar to e_object->mt_toolbar.
    clear ty_toolbar.

*    TY_TOOLBAR-ICON       = ICON_CHANGE.
*    TY_TOOLBAR-FUNCTION   = C_CHANGE.
*    TY_TOOLBAR-TEXT       = 'Modificar'.
*    TY_TOOLBAR-BUTN_TYPE  = 0.
*    APPEND TY_TOOLBAR TO E_OBJECT->MT_TOOLBAR.
*    CLEAR TY_TOOLBAR.

    call method c_alv_toolbarmanager->reorganize
      exporting
        io_alv_toolbar = e_object.
  endmethod.

  method handle_user_command.

    case e_ucomm.
*********************Add Line*********************************
      when c_add.
        data: wl_insert_136 like line of it_saida_136.
        data: wl_insert_137 like line of it_saida_137.
        data: wl_insert_151 like line of it_saida_151.
        data: wl_insert_195 like line of it_saida_195."Equalização ECC X HANA #108307   - SMC

        if rd_cfop is not initial.

          move:
           p_bukrs   to wl_insert_136-bukrs,"Equalização ECC X HANA #108307   - SMC
           sy-uname to wl_insert_136-usnam,
           sy-datum to wl_insert_136-data_atual,
           sy-uzeit to wl_insert_136-hora_atual.

          append wl_insert_136 to it_saida_136.

        elseif rd_filial_cc is not initial.

          move:
          p_bukrs   to wl_insert_137-bukrs,"Equalização ECC X HANA #108307   - SMC
           sy-uname to wl_insert_137-usnam,
           sy-datum to wl_insert_137-data_atual,
           sy-uzeit to wl_insert_137-hora_atual.

          append wl_insert_137 to it_saida_137.

        elseif rd_filial_ctb is not initial.

          move:
          p_bukrs   to wl_insert_151-bukrs,"Equalização ECC X HANA #108307   - SMC
          sy-uname to wl_insert_151-usnam,
          sy-datum to wl_insert_151-data_atual,
          sy-uzeit to wl_insert_151-hora_atual.

          append wl_insert_151 to it_saida_151.
          "Equalização ECC X HANA #108307 inicio  - SMC
        elseif rd_grupo_merc is not initial.

          move:
          p_bukrs   to wl_insert_195-bukrs,
          sy-uname to wl_insert_195-usnam,
          sy-datum to wl_insert_195-data_atual,
          sy-uzeit to wl_insert_195-hora_atual.

          append wl_insert_195 to it_saida_195.
          "Equalização ECC X HANA #108307 fim  - SMC
        endif.

*********************Delete Line*********************************
      when c_del.
        data: ans       type c.

        if rd_cfop is not initial.
          call method g_grid->get_selected_rows
            importing
              et_index_rows = tg_selectedrow.
        elseif rd_filial_cc is not initial.
          call method g_grid2->get_selected_rows
            importing
              et_index_rows = tg_selectedrow.
        elseif rd_filial_ctb is not initial.
          call method g_grid3->get_selected_rows
            importing
              et_index_rows = tg_selectedrow.
        elseif rd_grupo_merc is not initial."Equalização ECC X HANA #108307 inicio  - SMC
          call method g_grid4->get_selected_rows
            importing
              et_index_rows = tg_selectedrow. "Equalização ECC X HANA #108307 fim  - SMC
        endif.


        call function 'POPUP_TO_CONFIRM'
          exporting
            titlebar              = 'Confirmação'
            text_question         = 'Deseja eliminar a(s) linha(s) selecionada(s)?'
            text_button_1         = 'Sim'
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'Não'
            icon_button_2         = 'ICON_CANCEL'
            popup_type            = 'ICON_MESSAGE_ERROR'
            display_cancel_button = ''
          importing
            answer                = ans.

        case ans.
          when 2 or 'A'.
            leave to current transaction.
          when 1.
            perform deleta_dados.
        endcase.

    endcase.

    perform f_refresh_alv.

  endmethod.
endclass.               " LCL_ALV_TOOLBAR IMPLEMENTATION.

*=======================================================================
* FORM BUILD_FIELDCAT
*=======================================================================

form build_fieldcat changing pt_fieldcat type lvc_t_fcat .

  data ls_fcat type lvc_s_fcat.
  clear pt_fieldcat.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name = 'ZFIT0136'
*     I_INTERNAL_TABNAME = 'IT_ZFIT0136_ALV'
    changing
      ct_fieldcat      = pt_fieldcat.

  clear ls_fcat."Equalização ECC X HANA #108307   - SMC
  loop at pt_fieldcat into ls_fcat.

    if  ls_fcat-fieldname eq 'CFOP'.

      ls_fcat-edit = 'X'.
      "ls_fcat-edit_mask = '_______/__'."Equalização ECC X HANA #108307   - SMC
      ls_fcat-just = 'C'.
      "ls_fcat-no_convext = 'X'.
      ls_fcat-f4availabl = 'X'."Equalização ECC X HANA #108307   - SMC
      ls_fcat-checktable = '!'."Equalização ECC X HANA #108307   - SMC
      ls_fcat-outputlen = 10."Equalização ECC X HANA #108307   - SMC
      modify pt_fieldcat from ls_fcat.

    elseif ls_fcat-fieldname eq 'TP_MERCADO'.

      ls_fcat-edit = 'X'.
      ls_fcat-drdn_hndl = '1'.
      ls_fcat-outputlen = 12.
      "comentado PSA - Equalização ECC X HANA #108307   - SMC
*      IF  1 EQ 2.
*        ls_fcat-drdn_alias = abap_true.
*      ENDIF.

      ls_fcat-checktable = '!'.

      modify pt_fieldcat from ls_fcat.
    elseif ls_fcat-fieldname eq 'DATA_ATUAL'.

      ls_fcat-coltext = 'Data Mod.'.

      modify pt_fieldcat from ls_fcat.
    elseif ls_fcat-fieldname eq 'HORA_ATUAL'.

      ls_fcat-coltext = 'Hora Mod.'.

      modify pt_fieldcat from ls_fcat.
    endif.
  endloop.

endform.

form build_fieldcat2 changing pt_fieldcat2 type lvc_t_fcat .

  data ls_fcat type lvc_s_fcat.
  clear pt_fieldcat2.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name = 'ZFIT0137'
    changing
      ct_fieldcat      = pt_fieldcat2.

  clear ls_fcat.

  loop at pt_fieldcat2 into ls_fcat.

    if  ls_fcat-fieldname eq 'WERKS'.
      ls_fcat-edit = 'X'.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 10.
      modify pt_fieldcat2 from ls_fcat.
    elseif ls_fcat-fieldname eq 'KOSTL'.
      ls_fcat-edit = 'X'.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 10.
      modify pt_fieldcat2 from ls_fcat.
    elseif ls_fcat-fieldname eq 'DATA_ATUAL'.

      ls_fcat-coltext = 'Data Mod.'.

      modify pt_fieldcat2 from ls_fcat.
    elseif ls_fcat-fieldname eq 'HORA_ATUAL'.

      ls_fcat-coltext = 'Hora Mod.'.

      modify pt_fieldcat2 from ls_fcat.
    endif.
  endloop.


endform. "BUILD_FIELDCAT

form build_fieldcat3 changing pt_fieldcat3 type lvc_t_fcat .

  data ls_fcat type lvc_s_fcat.
  clear pt_fieldcat3.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name = 'ZFIT0151'
    changing
      ct_fieldcat      = pt_fieldcat3.

  clear ls_fcat.

  loop at pt_fieldcat3 into ls_fcat.

    if  ls_fcat-fieldname eq 'WERKS'.
      ls_fcat-edit = 'X'.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 10.
      modify pt_fieldcat3 from ls_fcat.
    elseif ls_fcat-fieldname eq 'SAKNR'.
      ls_fcat-edit = 'X'.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 10.
      modify pt_fieldcat3 from ls_fcat.
    elseif ls_fcat-fieldname eq 'DATA_ATUAL'.

      ls_fcat-coltext = 'Data Mod.'.

      modify pt_fieldcat3 from ls_fcat.
    elseif ls_fcat-fieldname eq 'HORA_ATUAL'.

      ls_fcat-coltext = 'Hora Mod.'.

      modify pt_fieldcat3 from ls_fcat.
    endif.

  endloop.

endform. "BUILD_FIELDCAT

"Equalização ECC X HANA #108307 INICIO  - SMC
form build_fieldcat4 changing pt_fieldcat4 type lvc_t_fcat .

  data ls_fcat type lvc_s_fcat.
  clear pt_fieldcat4.

  call function 'LVC_FIELDCATALOG_MERGE'
    exporting
      i_structure_name = 'ZFIT0195'
    changing
      ct_fieldcat      = pt_fieldcat4.

  clear ls_fcat.

  loop at pt_fieldcat4 into ls_fcat.

    if  ls_fcat-fieldname eq 'MATKL'.
      ls_fcat-edit = abap_true.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 10.
      modify pt_fieldcat4 from ls_fcat.

*    ELSEIF LS_FCAT-FIELDNAME EQ 'WGBEZ'.
*      LS_FCAT-EDIT = abap_false.
*      LS_FCAT-CHECKTABLE = '!'.
*      LS_FCAT-OUTPUTLEN = 10.
*      MODIFY PT_FIELDCAT4 FROM LS_FCAT.

    elseif ls_fcat-fieldname eq 'DATA_ATUAL'.

      ls_fcat-coltext = 'Data Mod.'.

      modify pt_fieldcat4 from ls_fcat.
    elseif ls_fcat-fieldname eq 'HORA_ATUAL'.

      ls_fcat-coltext = 'Hora Mod.'.

      modify pt_fieldcat4 from ls_fcat.
    endif.

  endloop.


endform. "BUILD_FIELDCAT
"Equalização ECC X HANA #108307  FIM - SMC

*=======================================================================
* FORM CREATE_AND_INIT_ALV
*=======================================================================

form create_and_init_alv changing pt_fieldcat  type lvc_t_fcat.
*

  data: lt_f4    type lvc_t_f4 with header line,
        l_tab(8) type c.



  if g_container is initial.

    wa_stable   = c_x.
    g_container =  'CC_101'.

    create object g_custom_container
      exporting
        container_name = g_container.

    create object g_grid
      exporting
        i_parent = g_custom_container.

    create object obg_toolbar
      exporting
        io_alv_grid = g_grid.


    set handler obg_toolbar->on_toolbar for g_grid.
    set handler obg_toolbar->handle_user_command for g_grid.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
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

    perform seleciona_dados.

* CONSTRÓI FIELDCAT E DEFINE AS COLUNAS EDITÁVEIS.
    perform build_fieldcat changing pt_fieldcat.

    if rd_cfop is not initial.
*     DEFINE OS VALORES PARA O MENU DROP DOWN
      perform set_drdwn_table.
    endif.

    call method cl_gui_cfw=>flush.

***************************************
    call method g_grid->set_table_for_first_display
      exporting
        it_toolbar_excluding = tl_function
      changing
        it_fieldcatalog      = pt_fieldcat
        it_outtab            = it_saida_136.

    set handler:
      lcl_event_handler=>on_button_click for g_grid,
      lcl_event_handler=>on_data_changed for g_grid,
      lcl_event_handler=>on_data_changed_finished for g_grid.

* * *
    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    call method g_grid->set_ready_for_input
      exporting
        i_ready_for_input = 1.
    "Comentado PSA - Equalização ECC X HANA #108307 INICIO  - SMC
*    IF rd_cfop IS NOT INITIAL.
*      CLEAR lt_f4.
*      lt_f4-fieldname = 'CFOP'.
*      lt_f4-register  = 'X'.
*      APPEND lt_f4.
*    ELSE.
*      CLEAR lt_f4.
*      lt_f4-fieldname = 'WERKS'.
*      lt_f4-register  = 'X'.
*      APPEND lt_f4.
*    ENDIF.
    "Comentado PSA - Equalização ECC X HANA #108307 FIM  - SMC
  else.

    call method g_grid->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.


endform.              "CREATE_AND_INIT_ALV

form create_and_init_alv2 changing pt_fieldcat2 type lvc_t_fcat.
*

  data: lt_f4    type lvc_t_f4 with header line,
        l_tab(8) type c.

  if g_container2 is initial.

    g_container2 = 'CC_102'.

    create object g_custom_container2
      exporting
        container_name = g_container2.


    create object g_grid2
      exporting
        i_parent = g_custom_container.

    create object g_grid2
      exporting
        i_parent = g_custom_container2.

    create object obg_toolbar
      exporting
        io_alv_grid = g_grid2.

    create object obg_toolbar
      exporting
        io_alv_grid = g_grid2.

    set handler obg_toolbar->on_toolbar for g_grid2.
    set handler obg_toolbar->handle_user_command for g_grid2.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
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

    perform seleciona_dados.

* CONSTRÓI FIELDCAT E DEFINE AS COLUNAS EDITÁVEIS.
    perform build_fieldcat2 changing pt_fieldcat2.


    call method g_grid2->set_table_for_first_display
      exporting
        it_toolbar_excluding = tl_function
      changing
        it_fieldcatalog      = pt_fieldcat2
        it_outtab            = it_saida_137.


    set handler:
      lcl_event_handler=>on_button_click for g_grid2,
      lcl_event_handler=>on_data_changed_finished for g_grid2.

* * *
    call method g_grid2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid2->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    call method g_grid2->set_ready_for_input
      exporting
        i_ready_for_input = 1.


  else.

    call method g_grid2->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.


endform.              "CREATE_AND_INIT_ALV2


form create_and_init_alv3 changing pt_fieldcat3 type lvc_t_fcat.
*

  data: lt_f4    type lvc_t_f4 with header line,
        l_tab(8) type c.

  if g_container3 is initial.

    g_container3 = 'CC_103'.

    create object g_custom_container3
      exporting
        container_name = g_container3.


    create object g_grid3
      exporting
        i_parent = g_custom_container.

    create object g_grid3
      exporting
        i_parent = g_custom_container3.

    create object obg_toolbar
      exporting
        io_alv_grid = g_grid3.

    create object obg_toolbar
      exporting
        io_alv_grid = g_grid3.

    set handler obg_toolbar->on_toolbar for g_grid3.
    set handler obg_toolbar->handle_user_command for g_grid3.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
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

    perform seleciona_dados.

* CONSTRÓI FIELDCAT E DEFINE AS COLUNAS EDITÁVEIS.
    perform build_fieldcat3 changing pt_fieldcat3.


    call method g_grid3->set_table_for_first_display
      exporting
        it_toolbar_excluding = tl_function
      changing
        it_fieldcatalog      = pt_fieldcat3
        it_outtab            = it_saida_151.


    set handler:
      lcl_event_handler=>on_button_click for g_grid3,
      lcl_event_handler=>on_data_changed_finished for g_grid3.

* * *
    call method g_grid3->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid3->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    call method g_grid3->set_ready_for_input
      exporting
        i_ready_for_input = 1.


  else.

    call method g_grid3->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.

endform.              "CREATE_AND_INIT_ALV3

"Equalização ECC X HANA #108307  INICIO - SMC
form create_and_init_alv4 changing pt_fieldcat4 type lvc_t_fcat.
*

  data: lt_f4    type lvc_t_f4 with header line,
        l_tab(8) type c.

  if g_container4 is initial.

    g_container4 = 'CC_104'.

    create object g_custom_container4
      exporting
        container_name = g_container4.


    create object g_grid4
      exporting
        i_parent = g_custom_container.

    create object g_grid4
      exporting
        i_parent = g_custom_container4.

    create object obg_toolbar
      exporting
        io_alv_grid = g_grid4.

    create object obg_toolbar
      exporting
        io_alv_grid = g_grid4.

    set handler obg_toolbar->on_toolbar for g_grid4.
    set handler obg_toolbar->handle_user_command for g_grid4.

    wl_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
    append wl_function to tl_function.
    wl_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
    append wl_function to tl_function.
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

    perform seleciona_dados.

* CONSTRÓI FIELDCAT E DEFINE AS COLUNAS EDITÁVEIS.
    perform build_fieldcat4 changing pt_fieldcat4.


    call method g_grid4->set_table_for_first_display
      exporting
        it_toolbar_excluding = tl_function
      changing
        it_fieldcatalog      = pt_fieldcat4
        it_outtab            = it_saida_195.


    set handler:
      lcl_event_handler=>on_button_click for g_grid4,
      lcl_event_handler=>on_data_changed_finished for g_grid4.

* * *
    call method g_grid4->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    call method g_grid4->register_edit_event
      exporting
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    call method g_grid4->set_ready_for_input
      exporting
        i_ready_for_input = 1.


  else.

    call method g_grid4->refresh_table_display
      exporting
        is_stable = wa_stable.

  endif.


endform.              "CREATE_AND_INIT_ALV4
"Equalização ECC X HANA #108307  FIM  - SMC

*=======================================================================
* FORM SET_DRDWN_TABLE
*=======================================================================
form set_drdwn_table.
  data: lt_dropdown type lvc_t_drop,
        ls_dropdown type lvc_s_drop.

  data: lt_dral type lvc_t_dral,
        ls_dral type lvc_s_dral.

* LISTBOX
  ls_dropdown-handle = '1'.
  ls_dropdown-value  = 'MI'.
  append ls_dropdown to lt_dropdown.

  ls_dropdown-handle = '1'.
  ls_dropdown-value  = 'ME'.
  append ls_dropdown to lt_dropdown.

  ls_dropdown-handle = '1'.
  ls_dropdown-value  = 'EN'.
  append ls_dropdown to lt_dropdown.

  g_grid->set_drop_down_table(
    it_drop_down = lt_dropdown ).

  "Comentado PSA - Equalização ECC X HANA #108307   - SMC
*  IF 1 EQ 2.
*    ls_dral-handle = '1'.
*    ls_dral-int_value  = 'MI'.
*    ls_dral-value  = 'Mercado Interno'.
*    APPEND ls_dral TO lt_dral.
*
*    ls_dral-handle = '1'.
*    ls_dral-int_value  = 'ME'.
*    ls_dral-value  = 'Mercado Externo'.
*    APPEND ls_dral TO lt_dral.
*
*    ls_dral-handle = '1'.
*    ls_dral-int_value  = 'EN'.
*    ls_dral-value  = 'Energia'.
*    APPEND ls_dral TO lt_dral.
*
*    g_grid->set_drop_down_table(
*      it_drop_down_alias = lt_dral ).
*  ENDIF.
  "Comentado PSA - Equalização ECC X HANA #108307   - SMC

endform.              "FORM SET_DRDWN_TABLE

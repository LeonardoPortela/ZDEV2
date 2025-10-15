*&---------------------------------------------------------------------*
*& Report ZMAIL_BULK_INSERT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZMAIL_BULK_INSERT.




*&---------------------------------------------------------------------*
*&  TABELAS INTERNAS E WORKAREAS
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_dados,
         mandt       TYPE zmail-mandt,
         bukrs       TYPE zmail-bukrs,
         werks       TYPE zmail-werks,
         dep_resp    TYPE zmail-dep_resp,
         tcode       TYPE zmail-tcode,
         email       TYPE zmail-email,
         param_espec TYPE zmail-param_espec,
         vkbur       TYPE zmail-vkbur,
         id_processo TYPE zmail-id_processo,
         usuario     TYPE zmail-usuario,
         zdt_atual   TYPE zmail-zdt_atual,
         zhr_atual   TYPE zmail-zhr_atual,
       END OF ty_dados.

DATA: it_saida TYPE TABLE OF ty_dados.
DATA: new_line TYPE ty_dados.


*&---------------------------------------------------------------------*
*&  ALV
*&---------------------------------------------------------------------*
TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

DATA: dg_splitter_1        TYPE REF TO cl_gui_splitter_container,
      g_grid               TYPE REF TO cl_gui_alv_grid,
      g_custom_container   TYPE REF TO cl_gui_custom_container,
      c_alv_toolbarmanager TYPE REF TO cl_alv_grid_toolbar_manager,
      container_1          TYPE REF TO cl_gui_container,
      cl_container_95      TYPE REF TO cl_gui_docking_container,
      obj_dyndoc_id        TYPE REF TO cl_dd_document,
      tl_function          TYPE ui_functions,
      wl_function          TYPE ui_func,
*
      t_fieldcat           TYPE lvc_t_fcat,
      w_fieldcat           TYPE lvc_s_fcat,
      t_colorcell          TYPE TABLE OF lvc_s_scol,
      w_colorcell          TYPE lvc_s_scol,
      t_exctab             TYPE slis_t_extab,
      w_exctab             TYPE slis_extab,
      w_layout             TYPE lvc_s_layo,
      w_stable             TYPE lvc_s_stbl,
      t_style              TYPE lvc_t_styl,
      w_style              TYPE lvc_s_styl,
      t_rows               TYPE lvc_t_row,
      w_rows               TYPE lvc_s_row,
      ok_code              TYPE sy-ucomm,
*
      zcl_util             TYPE REF TO zcl_util.

DATA: it_del_row       TYPE TABLE OF lvc_s_row-index,
      wl_del_row       TYPE lvc_s_row-index,
      gt_selected_rows TYPE lvc_t_row.

DATA: variante     LIKE disvariant.
DATA: gs_variant_c TYPE disvariant.

DATA: it_return TYPE TABLE OF ddshretval,
      it_T028G  TYPE TABLE OF t028g.


*&---------------------------------------------------------------------*
*&  CLASSES
*&---------------------------------------------------------------------*
CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.


    CLASS-methods:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

*    CLASS-methods:
*      on_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
*        IMPORTING e_row_id e_column_id.

    CLASS-METHODS:
      set_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object.

    CLASS-METHODS:
      get_ucomm FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm.

    CLASS-METHODS:
      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING
          er_data_changed
          e_onf4
          e_onf4_before
          e_onf4_after
          e_ucomm.

ENDCLASS.                    "LCL_EVENT_HANDLER DEFINITION


CLASS lcl_event_handler implementation.
  METHOD on_double_click.

  ENDMETHOD.

  METHOD set_toolbar.

    DATA wa_tool TYPE stb_button.

    MOVE 3 TO wa_tool-butn_type.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function     = 'BTN_INSERT_ROW'.
    wa_tool-icon         =  icon_insert_row.
    wa_tool-quickinfo    = 'Inserir linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.

    wa_tool-function     = 'BTN_DELETE_ROW'.
    wa_tool-icon         =  icon_delete_row.
    wa_tool-quickinfo    = 'Deletar linha'.
    APPEND wa_tool TO e_object->mt_toolbar.
    CLEAR wa_tool.


  ENDMETHOD.


  METHOD get_ucomm.
    case e_ucomm.
      when 'BTN_INSERT_ROW'.

        PERFORM f_insert_row.

      when 'BTN_DELETE_ROW'.

        PERFORM f_delete_row.

    ENDCASE.
  ENDMETHOD.

  METHOD handle_data_changed.

    DATA wa_paste TYPE ty_dados.
    DATA it_paste TYPE TABLE OF ty_dados.

    loop at er_data_changed->mt_good_cells into DATA(_item_changed).
      read table it_saida assigning field-symbol(<fs_get_index>) index _item_changed-row_id.

      IF sy-subrc <> 0.
        APPEND new_line TO it_saida.
      ENDIF.

      read table it_saida assigning field-symbol(<fs_set_info>) index _item_changed-row_id.

      CASE _item_changed-fieldname.
        WHEN 'BUKRS'.
          <fs_set_info>-bukrs = _item_changed-value.
        WHEN 'WERKS'.
          <fs_set_info>-werks = _item_changed-value.
        WHEN 'DEP_RESP'.
          <fs_set_info>-dep_resp = _item_changed-value.
        WHEN 'TCODE'.
          <fs_set_info>-tcode = _item_changed-value.
        WHEN 'EMAIL'.
          <fs_set_info>-email = _item_changed-value.
        WHEN 'PARAM_ESPEC'.
          <fs_set_info>-param_espec = _item_changed-value.
        WHEN 'VKBUR'.
          <fs_set_info>-vkbur = _item_changed-value.
        WHEN 'ID_PROCESSO'.
          <fs_set_info>-id_processo = _item_changed-value.
        WHEN 'USUARIO'.
          <fs_set_info>-usuario = sy-uname.
        WHEN 'ZDT_ATUAL'.
          <fs_set_info>-zdt_atual = sy-datum.
        WHEN 'ZHR_ATUAL'.
          <fs_set_info>-zhr_atual = sy-uzeit.
      ENDCASE.

    ENDLOOP.

    IF er_data_changed->mt_good_cells IS INITIAL.
      CASE e_ucomm.

        WHEN 'BTN_INSERT_ROW'.

          PERFORM f_insert_row.

      ENDCASE.
    ELSE.
      LEAVE TO SCREEN 0100.
    ENDIF.



  ENDMETHOD.
ENDCLASS.


*&---------------------------------------------------------------------*
*&  INICIO
*&---------------------------------------------------------------------*
initialization.

  PERFORM f_exibir_dados.


*&---------------------------------------------------------------------*
*&  EXIBIR DADOS
*&---------------------------------------------------------------------*
FORM f_exibir_dados.
  CALL SCREEN 0100.
ENDFORM.


*&---------------------------------------------------------------------*
*&  IMPRIMIR DADOS
*&---------------------------------------------------------------------*
FORM f_init_alv .

  DATA: wl_layout TYPE slis_layout_alv.
  DATA:
    p_text      TYPE sdydo_text_element,
    filtros	    TYPE zif_screen_linha_filtro,
    i_filtros	  TYPE zif_screen_linha_filtro_t,
    v_valor(60),
    v_datum(10) TYPE c,
    v_uzeit(10) TYPE c.


  PERFORM f_fieldcatalog.

  variante = VALUE #( report = sy-repid ).



  IF g_grid is initial.

    CLEAR: i_filtros.
    CONCATENATE sy-datum+06(02) '/' sy-datum+04(02) '/' sy-datum(04) INTO v_datum.
    CONCATENATE sy-uzeit(02) ':' sy-uzeit+02(02) ':' sy-uzeit+04(02) INTO v_uzeit.
    DESCRIBE TABLE it_saida LINES DATA(v_lines).
    APPEND VALUE #( parametro = 'DATA:' valor = v_datum ) TO i_filtros.
    APPEND VALUE #( parametro = 'Hora:' valor = v_uzeit ) TO i_filtros.
    APPEND VALUE #( parametro = 'Registros:' valor = v_lines ) TO i_filtros.

    p_text = 'Inserir Dados em Massa - ZMAIL'.
  ENDIF.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(
      EXPORTING
        i_titulo  = CONV #( p_text )
        i_filtros = i_filtros
      CHANGING
        split     = dg_splitter_1
        alv       = g_grid ) = abap_true.


    w_layout-sel_mode = 'A'.
    w_layout-col_opt  = abap_true.
    w_layout-no_rowins = ''.

    w_stable-row          = abap_true.
    w_stable-col          = abap_true.

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

    SET HANDLER: lcl_event_handler=>get_ucomm FOR g_grid,
                lcl_event_handler=>set_toolbar FOR g_grid,
                lcl_event_handler=>handle_data_changed FOR g_grid.


    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
        it_toolbar_excluding          = tl_function
        is_variant                    = variante
      CHANGING
        it_outtab                     = it_saida[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.


    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.


    IF lines( t_rows ) > 0.
      CALL METHOD g_grid->set_selected_rows
        EXPORTING
          it_index_rows = t_rows.
    ENDIF.



  ELSE.
    CALL METHOD g_grid->refresh_table_display( is_stable = w_stable ).
  ENDIF.

  wl_layout-colwidth_optimize = 'X'.

  IF it_saida IS INITIAL.
    PERFORM f_insert_row.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& FORM montar_layout
*&---------------------------------------------------------------------*
FORM f_fieldcatalog .

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
 01  ''   ''   'IT_SAIDA'   'MANDT        '     'Mandante             '  '08'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  'X'  ' '  ' ',
 01  ''   ''   'IT_SAIDA'   'BUKRS        '     'Empresa              '  '08'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 02  ''   ''   'IT_SAIDA'   'WERKS        '     'Centro               '  '20'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 03  ''   ''   'IT_SAIDA'   'DEP_RESP     '     'Departamento         '  '10'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 04  ''   ''   'IT_SAIDA'   'TCODE        '     'Código trans.        '  '15'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 05  ''   ''   'IT_SAIDA'   'EMAIL        '     'Email                '  '60'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 06  ''   ''   'IT_SAIDA'   'PARAM_ESPEC  '     'Param. Espec.        '  '15'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 07  ''   ''   'IT_SAIDA'   'VKBUR        '     'Esc. Venda           '  '15'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 08  ''   ''   'IT_SAIDA'   'ID_PROCESSO  '     'ID Processo          '  '15'  'X'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 09  ''   ''   'IT_SAIDA'   'USUARIO      '     'Nome do Usuário      '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 10  ''   ''   'IT_SAIDA'   'ZDT_ATUAL    '     'DATA Atualização     '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' ',
 11  ''   ''   'IT_SAIDA'   'ZHR_ATUAL    '     'Hora Atualização     '  '15'  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '  ' '.

ENDFORM.


*&---------------------------------------------------------------------*
*& FORM  f_estrutura_alv
*&---------------------------------------------------------------------*
FORM f_estrutura_alv  USING VALUE(p_col_pos)       TYPE i                    "1
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname        "2
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname      "3
                           VALUE(p_tabname)       LIKE dd02d-tabname        "4
                           VALUE(p_field)         LIKE dd03d-fieldname      "5
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l      "6
                           VALUE(p_outputlen)                               "7
                           VALUE(p_edit)                                    "8
                           VALUE(p_sum)                                     "9
                           VALUE(p_just)                                    "10
                           VALUE(p_hotspot)                                 "11
                           VALUE(p_f4)                                      "12
                           VALUE(p_checkbox)                                "13
                           VALUE(p_style)                                   "14
                           VALUE(p_no_out)                                  "15
                           VALUE(p_icon)                                    "16
                           VALUE(p_fix).                                    "17

  CLEAR w_fieldcat.
  w_fieldcat-fieldname   = p_field.
  w_fieldcat-tabname     = p_tabname.
  w_fieldcat-ref_table   = p_ref_tabname.
  w_fieldcat-ref_field   = p_ref_fieldname.
  w_fieldcat-key         = ' '.
  w_fieldcat-edit        = p_edit.
  w_fieldcat-col_pos     = p_col_pos.
  w_fieldcat-outputlen   = p_outputlen.
  w_fieldcat-no_out      = p_no_out.
  w_fieldcat-do_sum      = p_sum.
  w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-scrtext_s   = p_scrtext_l.
  w_fieldcat-scrtext_m   = p_scrtext_l.
  w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'L'.
  w_fieldcat-selddictxt  = 'L'.
  w_fieldcat-tipddictxt  = 'L'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.


*&---------------------------------------------------------------------*
*& FORM  f_save_dados
*&---------------------------------------------------------------------*
FORM f_save_dados.

  DATA: resposta TYPE c.

  CLEAR resposta.

  DATA: lt_tab TYPE esp1_message_tab_type.
  DATA: ls_tab TYPE esp1_message_wa_type.
  CLEAR: lt_tab,ls_tab.

  ls_tab-msgid  = 'E4'.
  ls_tab-msgno  = '000'.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      TITLEBAR              = 'Confirmar'
      TEXT_QUESTION         = 'Deseja incluir dados em massa?'
      TEXT_BUTTON_1         = 'Sim'
      TEXT_BUTTON_2         = 'Não'
      DEFAULT_BUTTON        = '2'
      DISPLAY_CANCEL_BUTTON = ''
    IMPORTING
      ANSWER                = RESPOSTA
    EXCEPTIONS
      TEXT_NOT_FOUND        = 1
      OTHERS                = 2.

  IF RESPOSTA = '1'.

    IF it_saida IS NOT INITIAL.

      DATA(lr_index) = 1.

      LOOP AT it_saida INTO DATA(wa_saida).

        READ TABLE it_saida ASSIGNING FIELD-SYMBOL(<fs_get_info>) INDEX lr_index.

        IF sy-subrc = 0.

          SELECT SINGLE bukrs, werks, dep_resp, tcode, email, param_espec, vkbur, id_processo FROM zmail WHERE bukrs       = @wa_saida-bukrs
                                                                                                         AND   werks       = @wa_saida-werks
                                                                                                         AND   dep_resp    = @wa_saida-dep_resp
                                                                                                         AND   tcode       = @wa_saida-tcode
                                                                                                         AND   email       = @wa_saida-email
                                                                                                         AND   param_espec = @wa_saida-param_espec
                                                                                                         AND   vkbur       = @wa_saida-vkbur
                                                                                                         AND   id_processo = @wa_saida-id_processo INTO @DATA(lr_zmail).

          IF sy-subrc <> 0.

            MODIFY zmail FROM wa_saida.

            COMMIT WORK.

            IF sy-subrc = 0.

              ls_tab-msgty  = 'S'.
              ls_tab-msgv1  = |Linha: { lr_index } - Registro gravado com sucesso!|.
              ls_tab-lineno = 1.
              APPEND ls_tab TO lt_tab.

            ELSE.

              ls_tab-msgty  = 'E'.
              ls_tab-msgv1  = |Linha: { lr_index } - Houve um erro ao gravar o registro!|.
              ls_tab-lineno = 1.
              APPEND ls_tab TO lt_tab.

            ENDIF.

          ELSE.

            ls_tab-msgty  = 'E'.
            ls_tab-msgv1  = |Linha: { lr_index } - Registro já cadastrado!|.
            ls_tab-lineno = 1.
            APPEND ls_tab TO lt_tab.

          ENDIF.

        ENDIF.

        lr_index = lr_index + 1.

      ENDLOOP.

      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = lt_tab.



    ELSE.

      MESSAGE 'Não há dados para incluir.' TYPE 'E'.

    ENDIF.

  ELSE.

    EXIT.

  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& FORM  f_insert_row
*&---------------------------------------------------------------------*
FORM f_insert_row.

  CLEAR: new_line.

  new_line-usuario = sy-uname.
  new_line-zdt_atual = sy-datum.
  new_line-zhr_atual = sy-uzeit.

  APPEND new_line TO it_saida.

  PERFORM f_init_alv.

  g_grid->refresh_table_display( ).

ENDFORM.


*&---------------------------------------------------------------------*
*& FORM  f_delete_row
*&---------------------------------------------------------------------*
FORM f_delete_row.

  CLEAR: gt_selected_rows, gt_selected_rows[], it_del_row[], wl_del_row.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = gt_selected_rows.

  LOOP AT gt_selected_rows INTO DATA(wl_selected_rows).

    READ TABLE it_saida INTO DATA(wa_saida) INDEX w_rows.

    wl_del_row = wl_selected_rows-index.
    APPEND wl_del_row TO it_del_row.

  ENDLOOP.

  IF it_del_row[] IS NOT INITIAL.
    SORT it_del_row DESCENDING.
    LOOP AT it_del_row INTO wl_del_row.

      DELETE it_saida INDEX wl_del_row.
    ENDLOOP.
  ENDIF.

  g_grid->refresh_table_display( ).

ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'PF0100'.
  SET TITLEBAR 'TB0100'.

  PERFORM f_init_alv.
ENDMODULE.


*&---------------------------------------------------------------------*
*&  Module  USER_COMMAND_100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'BACK'.
     LEAVE TO TRANSACTION 'ZMAIL'.

    WHEN 'EXIT' OR 'CANCEL'.
      LEAVE PROGRAM.

    WHEN 'SAVE'.
      PERFORM f_save_dados.
  ENDCASE.
ENDMODULE.

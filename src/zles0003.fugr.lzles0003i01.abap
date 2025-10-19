*----------------------------------------------------------------------*
***INCLUDE LZLES0003I01 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0002 OUTPUT.
  SET PF-STATUS 'PF0002'.
  SET TITLEBAR 'TL0002'.

  PERFORM griar_alv_ajustes.

ENDMODULE.                 " STATUS_0002  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002 INPUT.

  CASE ok_code_0002.
    WHEN ok_ajustar.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_0002  INPUT

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0002_exit INPUT.
  vg_cancelado_0002 = c_x.
  LEAVE TO SCREEN 0.
ENDMODULE.                 " USER_COMMAND_0002_EXIT  INPUT

*&---------------------------------------------------------------------*
*&      Form  GRIAR_ALV_AJUSTES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM griar_alv_ajustes .

  DATA: it_exclude_fcode TYPE ui_functions,
        wa_exclude_fcode LIKE LINE OF it_exclude_fcode.

  CONSTANTS: tabela_item       TYPE string VALUE 'IT_ITENS_ALV',
             c_grid_color_c200 TYPE c LENGTH 4 VALUE 'C200'.

  IF vg_alv_0002 IS INITIAL.

    CLEAR: it_exclude_fcode.

    wa_exclude_fcode = '&PRINT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&AVERAGE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_VIEW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&INFO'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

*   Create object for container
    CREATE OBJECT cockpit_container_alv
      EXPORTING
        container_name = 'QTD_ALV'.

    CREATE OBJECT cockpit_alv
      EXPORTING
        i_parent = cockpit_container_alv.

    CLEAR: it_cockpit_catalog.

    PERFORM z_estrutura_fieldcat TABLES it_cockpit_catalog USING:
        tabela_item 'CK_CONFERIDO'   text-l16 'X' 01 04 'X'   space   space space space c_grid_color_c200 'X'   'X',
        tabela_item 'NM_LOTE_ITEM'   text-d02 ' ' 02 05 'X'   'ALPHA' space space space c_grid_color_c200 space space,
        tabela_item 'CHVID'          text-d03 ' ' 03 02 'X'   space   space space space c_grid_color_c200 space space,
        tabela_item 'DESCHVID'       text-d04 ' ' 04 25 'X'   space   space space space c_grid_color_c200 space space,
        tabela_item 'CD_CIOT'        text-d05 'X' 05 12 space space   space space space c_grid_color_c200 space space,
        tabela_item 'NR_CIOT'        text-d06 ' ' 06 12 space space   space space space c_grid_color_c200 space space,
        tabela_item 'NUCONTRATO'     text-d12 ' ' 07 12 space space   space space space c_grid_color_c200 space space,
        tabela_item 'TKNUM'          text-d20 'X' 08 12 space space   space space space c_grid_color_c200 space space,
        tabela_item 'DOCNUM'         text-d07 'X' 09 10 space space   space space space c_grid_color_c200 space space,
        tabela_item 'CTENUM'         text-d13 ' ' 10 09 space space   space space space c_grid_color_c200 space space,
        tabela_item 'CTESERIE'       text-d14 ' ' 11 03 space space   space space space c_grid_color_c200 space space,
        tabela_item 'VL_TRANSACAO'   text-d09 ' ' 12 15 space space   space space space c_grid_color_c200 space space,
        tabela_item 'VL_PAGO_LOTE'   text-d19 ' ' 13 15 space space   space space space c_grid_color_c200 space space,
        tabela_item 'VL_CONFERIDO'   text-d15 ' ' 14 15 space space   space space space c_grid_color_c200 space space,
        tabela_item 'VL_DIFERENCA'   text-d16 ' ' 15 15 space space   space space space c_grid_color_c200 space space,
        tabela_item 'BELNR'          text-l13 'X' 16 10 space space   space space space c_grid_color_c200 space space,
        tabela_item 'GJAHR'          text-l14 ' ' 17 06 space space   space space space c_grid_color_c200 space space.

    CLEAR: cockpit_gs_layout.
    cockpit_gs_layout-zebra      = c_x.
    cockpit_gs_layout-info_fname = 'ROWCOLOR'.

    wa_exclude_fcode = '&LOCAL&CUT'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&INSERT_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&DELETE_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&MOVE_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&PASTE'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&PASTE_NEW_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&UNDO'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&VARI_ADMIN'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&APPEND'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&COPY'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&LOCAL&COPY_ROW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&VLOTUS'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&AQW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&MB_VIEW'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&REFRESH'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&REPREP'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.
    wa_exclude_fcode = '&CHECK'.
    APPEND wa_exclude_fcode TO it_exclude_fcode.

    is_variant-report = sy-repid.
    is_variant-handle = '0002'.

    CALL METHOD cockpit_alv->set_table_for_first_display
      EXPORTING
        i_default            = space
        is_layout            = cockpit_gs_layout
        it_toolbar_excluding = it_exclude_fcode
        is_variant           = is_variant
        i_save               = 'A'
      CHANGING
        it_fieldcatalog      = it_cockpit_catalog
        it_outtab            = it_itens_alv[].

    CREATE OBJECT cockpit_event.
    SET HANDLER cockpit_event->cockpit_hotspot FOR cockpit_alv.

    vg_alv_0002 = c_x.

  ENDIF.

  CALL METHOD cockpit_alv->set_scroll_info_via_id
    EXPORTING
      is_row_info = es_row_info
      is_col_info = es_col_info
      is_row_no   = es_row_no.

ENDFORM.                    " GRIAR_ALV_AJUSTES

*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM z_estrutura_fieldcat TABLES it_catalogo TYPE lvc_t_fcat
                           USING p_tab_name
                                 p_fieldname
                                 p_texto_grande
                                 p_hot
                                 p_posicao
                                 p_outputlen
                                 p_fix_column
                                 p_convexit
                                 p_do_sum
                                 p_icon
                                 p_just
                                 p_emphasize
                                 p_edit
                                 p_checkbox.

  DATA: wa_catalog TYPE lvc_s_fcat.

  CLEAR wa_catalog.
  wa_catalog-tabname     = p_tab_name.
  wa_catalog-fieldname   = p_fieldname.
  wa_catalog-scrtext_l   = p_texto_grande.
  wa_catalog-scrtext_m   = p_texto_grande.
  wa_catalog-scrtext_s   = p_texto_grande.
  wa_catalog-hotspot     = p_hot.
  wa_catalog-col_pos     = p_posicao.
  wa_catalog-outputlen   = p_outputlen.
  wa_catalog-fix_column  = p_fix_column.
  wa_catalog-convexit    = p_convexit.
  wa_catalog-do_sum      = p_do_sum.
  wa_catalog-icon        = p_icon.
  wa_catalog-just        = p_just.
  wa_catalog-emphasize   = p_emphasize.
  wa_catalog-edit        = p_edit.
  wa_catalog-checkbox    = p_checkbox.
  APPEND wa_catalog TO it_catalogo.
ENDFORM.                    " Z_ESTRUTURA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0020  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0020 INPUT.
  DATA: tl_file_table TYPE TABLE OF  sdokpath WITH HEADER LINE,
        tl_dir_table  TYPE TABLE OF  sdokpath WITH HEADER LINE,
        wl_lines TYPE sy-tabix,
        wl_lines_aux(6),
        wl_msg(255),
        wl_answer(5).

*  call method grid1->check_changed_data.
*  BREAK-POINT.
  CASE sy-ucomm.
    WHEN 'DOWNLOAD'.
      REFRESH: tg_saida_arq.
      CALL FUNCTION 'TMP_GUI_DIRECTORY_LIST_FILES'
        EXPORTING
          directory  = wg_caminho
        TABLES
          file_table = tl_file_table
          dir_table  = tl_dir_table.

      LOOP AT tl_file_table.
        tg_saida_arq-filename = tl_file_table-pathname.

        APPEND tg_saida_arq.
        CLEAR: tg_saida_arq.
      ENDLOOP.
      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN 'OK'.
      CALL METHOD grid1->get_selected_rows
        IMPORTING
          et_index_rows = tg_selected_rows.
*
      LOOP AT tg_selected_rows INTO wg_selected_rows.
        READ TABLE tg_saida_arq INDEX wg_selected_rows-index.
        MOVE: tg_saida_arq-filename TO tg_filename-pathname.
        APPEND tg_filename.
        CLEAR: tg_filename.
      ENDLOOP.

      CALL METHOD grid1->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

      DESCRIBE TABLE tg_filename LINES wl_lines.
      wl_lines_aux = wl_lines.

      IF wl_lines IS NOT INITIAL.
        SHIFT wl_lines_aux LEFT DELETING LEADING '0'.
        CONDENSE wl_lines_aux NO-GAPS.

        CONCATENATE 'tem certeza que deseja importar' wl_lines_aux 'arquivo(s)?'
          INTO wl_msg SEPARATED BY space.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
           titlebar                    = 'Confirmar importação'
            text_question               = wl_msg
           text_button_1               = 'Sim'
*                                ICON_BUTTON_1               = ' '
           text_button_2               = 'Não'
*                                ICON_BUTTON_2               = ' '
           display_cancel_button       = ' '
           iv_quickinfo_button_1       = 'Sim'
           iv_quickinfo_button_2       = 'Não'
         IMPORTING
           answer                      = wl_answer.

        IF wl_answer EQ '2'.
          REFRESH: tg_filename.

        ELSEIF wl_answer EQ '1'.
          LEAVE TO SCREEN 0.

        ENDIF.

      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'CANC'.

      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_0020  INPUT

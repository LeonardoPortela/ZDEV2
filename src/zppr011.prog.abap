*&---------------------------------------------------------------------*
*& Report  ZPPR011
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zppr011.

TABLES zppt0017.

DATA: it_saida     TYPE TABLE OF zppt0017,
      wa_saida     TYPE zppt0017,

      it_saida_aux TYPE TABLE OF zppt0017,
      wa_saida_aux TYPE zppt0017,

      it_saida_mod TYPE TABLE OF zppt0017,
      wa_saida_mod TYPE zppt0017.

DATA: wa_prod TYPE zppt0017,
      wa_mara TYPE mara.


DATA: g_container        TYPE scrfname,
      g_grid             TYPE REF TO cl_gui_alv_grid,
      g_custom_container TYPE REF TO cl_gui_custom_container,

      gt_fieldcat        TYPE lvc_t_fcat,
      gs_layout          TYPE lvc_s_layo,
      i_selected_rows    TYPE lvc_t_row,

      tg_selectedcell    TYPE lvc_t_cell,
      wg_selectedcell    TYPE lvc_s_cell,
      tg_selectedrow     TYPE lvc_t_row,
      wg_selectedrow     TYPE lvc_s_row,
      ty_toolbar         TYPE stb_button,
      wa_stable          TYPE lvc_s_stbl,
      tg_msg_ret         TYPE TABLE OF zfiwrs0002 WITH HEADER LINE.


DATA: wl_repid    TYPE sy-repid,
      tl_function TYPE ui_functions,
      wl_function LIKE tl_function WITH HEADER LINE.

DATA: lt_celltab TYPE lvc_t_styl.

DATA: abap    TYPE c,
      abap01  TYPE c,
      usnam   TYPE sy-uname,
      usnam01 TYPE sy-uname.

INITIALIZATION.

START-OF-SELECTION.

  CALL SCREEN 0100.

CLASS lcl_event_handler DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS:
      on_double_click FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column.

    CLASS-METHODS:
      on_data_changed  FOR EVENT data_changed OF cl_gui_alv_grid
        IMPORTING er_data_changed e_onf4 e_onf4_before e_onf4_after e_ucomm.

    CLASS-METHODS:
      on_data_changed_finished FOR EVENT data_changed_finished OF cl_gui_alv_grid
        IMPORTING e_modified et_good_cells.

    CLASS-METHODS:
      on_button_click FOR EVENT button_click OF cl_gui_alv_grid
        IMPORTING es_col_id es_row_no.
ENDCLASS.

CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_double_click.

  ENDMETHOD.

  METHOD on_data_changed.

    DATA: v_matnr_ac TYPE matnr,
          v_matnr_rv TYPE matnr,
          v_werks    TYPE werks,
          wa_prod    TYPE zppt0017,
          wa_mara    TYPE mara,
          wa_t001w   TYPE t001w,
          lv_matnr   TYPE matnr18.


    LOOP AT er_data_changed->mt_good_cells INTO DATA(wa_data_changed)
      WHERE fieldname EQ 'MATNR_AC' OR fieldname EQ 'MATNR_RV' OR fieldname EQ 'WERKS'.

      LOOP AT it_saida INTO wa_saida.

        CHECK wa_data_changed-row_id EQ sy-tabix.

        CASE wa_data_changed-fieldname.

          WHEN 'WERKS'.

            CLEAR: wa_prod, wa_mara, v_matnr_ac, v_matnr_rv, v_werks.

            v_werks    = wa_data_changed-value.
            v_matnr_ac = |{ wa_saida-matnr_ac ALPHA = IN }|.
            lv_matnr = |{ wa_saida-matnr_ac ALPHA = IN }|.


            SELECT SINGLE * FROM  zppt0017 INTO wa_prod
             WHERE ( matnr_ac EQ v_matnr_ac OR
                     matnr_ac EQ lv_matnr )
             AND   werks    EQ  v_werks.

            IF wa_prod IS NOT INITIAL.
              MESSAGE 'Produto e Centro já cadastrados neste De/Para. Favor informar outro código do tipo ( ZFER - Produto Acabado ).' TYPE 'I'.
              EXIT.
            ENDIF.

            SELECT SINGLE * FROM t001w INTO wa_t001w
             WHERE werks EQ v_werks.

            IF sy-subrc IS NOT INITIAL.
              MESSAGE 'Centro informado não existe!' TYPE 'I'.
              EXIT.
            ELSE.

              wa_saida_mod-situacao     = wa_saida-situacao.
              wa_saida_mod-werks        = wa_saida-werks.
              wa_saida_mod-matnr_ac     = wa_mara-matnr.
              wa_saida_mod-usuario_c    = wa_saida-usuario_c.
              wa_saida_mod-data_atual_c = wa_saida-data_atual_c.
              wa_saida_mod-hora_atual_c = wa_saida-hora_atual_c.

              MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
            ENDIF.

          WHEN 'MATNR_AC'.

            CLEAR: wa_prod, wa_mara, v_matnr_ac, v_matnr_rv, v_werks.

            v_matnr_ac = |{ wa_data_changed-value ALPHA = IN }|.
            lv_matnr = |{ wa_data_changed-value ALPHA = IN }|.
            v_werks    =  wa_saida-werks.

            SELECT SINGLE * FROM  zppt0017 INTO wa_prod
             WHERE ( matnr_ac EQ  v_matnr_ac OR
                     matnr_ac EQ lv_matnr )
             AND   werks    EQ  v_werks.

            IF wa_prod IS NOT INITIAL.
              MESSAGE 'Produto e Centro já cadastrados neste De/Para. Favor informar outro código do tipo ( ZFER - Produto Acabado ).' TYPE 'I'.
              EXIT.
            ENDIF.

            SELECT SINGLE * FROM makt INTO @DATA(wa_makt)
             WHERE matnr EQ @v_matnr_ac OR
                   matnr EQ @lv_matnr.

            IF sy-subrc IS NOT INITIAL.
              MESSAGE 'Produto informado não existe!' TYPE 'I'.
              EXIT.
            ENDIF.

            SELECT SINGLE * FROM mara INTO wa_mara
             WHERE matnr EQ v_matnr_ac or
                   matnr eq lv_matnr.

            IF wa_mara-mtart <> 'ZFER'.
              MESSAGE 'Favor selecionar produto do tipo "ZFER - Produto Acabado"' TYPE 'I'.
              EXIT.
            ELSE.
              wa_saida-matnr_ac = wa_mara-matnr.
              wa_saida-maktx_ac = wa_makt-maktx.

              wa_saida_mod-situacao     = wa_saida-situacao.
              wa_saida_mod-werks        = wa_saida-werks.
              wa_saida_mod-matnr_ac     = wa_mara-matnr.
              wa_saida_mod-maktx_ac     = wa_makt-maktx.
              wa_saida_mod-usuario_c    = wa_saida-usuario_c.
              wa_saida_mod-data_atual_c = wa_saida-data_atual_c.
              wa_saida_mod-hora_atual_c = wa_saida-hora_atual_c.

              MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
            ENDIF.

          WHEN 'MATNR_RV'.
            CLEAR wa_mara.

            v_matnr_rv = |{ wa_data_changed-value ALPHA = IN }|.
            lv_matnr   = |{ wa_data_changed-value ALPHA = IN }|.

            SELECT SINGLE * FROM makt INTO wa_makt
             WHERE matnr EQ v_matnr_rv.

            IF sy-subrc IS NOT INITIAL.
              SELECT SINGLE * FROM makt INTO wa_makt
               WHERE matnr EQ lv_matnr.
              IF sy-subrc IS NOT INITIAL.
                MESSAGE 'Produto informado não existe!' TYPE 'I'.
                EXIT.
              ENDIF.
            ENDIF.

            SELECT SINGLE * FROM mara INTO wa_mara
            WHERE matnr EQ wa_makt-matnr.

            IF wa_mara-mtart <> 'ZHAW'.
              MESSAGE 'Favor selecionar produto do tipo "ZHAW - Produto de Revenda"' TYPE 'I'.
              EXIT.
            ELSE.
              wa_saida-matnr_rv = wa_mara-matnr.
              wa_saida-maktx_rv = wa_makt-maktx.

              wa_saida_mod-matnr_rv = wa_mara-matnr.
              wa_saida_mod-maktx_rv = wa_makt-maktx.
              MODIFY it_saida FROM wa_saida INDEX wa_data_changed-row_id.
            ENDIF.
        ENDCASE.
      ENDLOOP.
    ENDLOOP.


    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.

  ENDMETHOD.

  METHOD on_data_changed_finished.

    IF usnam = 'EDIT'.
      CLEAR it_saida_mod[].

      LOOP AT et_good_cells  INTO DATA(wa_good_cells).
        READ TABLE it_saida INTO wa_saida INDEX wa_good_cells-row_id.
        IF sy-subrc = 0.
          wa_saida_mod-situacao     = wa_saida-situacao.
          wa_saida_mod-werks        = wa_saida-werks.
          wa_saida_mod-matnr_ac     = wa_saida-matnr_ac.
          wa_saida_mod-maktx_ac     = wa_saida-maktx_ac.
          wa_saida_mod-usuario_c    = wa_saida-usuario_c.
          wa_saida_mod-data_atual_c = wa_saida-data_atual_c.
          wa_saida_mod-hora_atual_c = wa_saida-hora_atual_c.
          wa_saida_mod-matnr_rv     = wa_saida-matnr_rv.
          wa_saida_mod-maktx_rv     = wa_saida-maktx_rv.

          APPEND wa_saida_mod TO it_saida_mod.
        ENDIF.
      ENDLOOP.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    ENDIF.

    IF usnam = 'INS'.
      LOOP AT et_good_cells  INTO wa_good_cells.
        IF wa_saida_mod-matnr_ac IS NOT INITIAL AND
           wa_saida_mod-matnr_rv IS NOT INITIAL.
          APPEND wa_saida_mod TO it_saida_mod.
          CLEAR wa_saida_mod.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD on_button_click.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDMETHOD.
ENDCLASS.

FORM seleciona_dados.

  SELECT * FROM zppt0017 INTO TABLE it_saida.

  SORT it_saida BY  data_atual_c hora_atual_c  ASCENDING.

ENDFORM.

FORM excluir_dados.

  LOOP AT tg_selectedrow INTO wg_selectedrow.
    READ TABLE it_saida INTO wa_saida INDEX wg_selectedrow-index.
    DELETE zppt0017 FROM wa_saida.
  ENDLOOP.

  CLEAR wa_saida.

  REFRESH it_saida.

  PERFORM seleciona_dados.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = wa_stable.

ENDFORM.


FORM salvar_dados.
  CLEAR wa_prod.

  LOOP AT it_saida_mod INTO wa_saida_mod.
    IF usnam  = 'EDIT'.
      IF wa_saida_mod-matnr_rv IS INITIAL.
        MESSAGE ' Favor informar um codigo Produto Revenda do  tipo ( ZHAW - Produto Revenda )' TYPE 'I'.
        EXIT.
      ENDIF.
      APPEND wa_saida_mod TO it_saida_aux.

    ELSE.

      SELECT SINGLE * FROM  zppt0017 INTO wa_prod
        WHERE matnr_ac EQ wa_saida_mod-matnr_ac
        AND   werks    EQ wa_saida_mod-werks.

      IF wa_prod-matnr_ac = wa_saida_mod-matnr_ac.
        MESSAGE 'Produto e Centro já cadastrado. Favor informar outro código do tipo ( ZFER - Produto Acabado ).' TYPE 'I'.
        EXIT.
      ENDIF.
      IF wa_saida_mod-matnr_ac IS INITIAL.
        MESSAGE ' Favor informar um codigo Produto Acabado do  tipo ( ZFER - Produto Acabado )' TYPE 'I'.
        EXIT.
      ENDIF.
      IF wa_saida_mod-matnr_rv IS INITIAL.
        MESSAGE ' Favor informar um codigo Produto Revenda do  tipo ( ZHAW - Produto Revenda )' TYPE 'I'.
        EXIT.
      ENDIF.
      IF wa_saida_mod-matnr_ac = wa_saida_mod-matnr_rv.
        MESSAGE 'Os Produtos não podem ser iguais. Favor informar um do tipo ( ZFER - Produto Acabado ) e ( ZHAW - Produto Revenda )' TYPE 'I'.
        EXIT.
      ENDIF.
      IF wa_saida_mod IS INITIAL.
        MESSAGE 'Favor informar um Centro.' TYPE 'I'.
        EXIT.
      ELSE.
        APPEND wa_saida_mod TO it_saida_aux.
      ENDIF.
    ENDIF.
    CLEAR: wa_saida_mod.
  ENDLOOP.

  IF it_saida_aux[] IS NOT INITIAL.
    MODIFY zppt0017 FROM TABLE it_saida_aux.
    MESSAGE TEXT-001 TYPE 'S'.
    FREE it_saida_aux.
  ENDIF.
  PERFORM seleciona_dados.

  abap   = ''.
  abap01 = ''.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: ls_fcat     TYPE lvc_s_fcat,
        pt_fieldcat TYPE lvc_t_fcat..

  SET PF-STATUS 'STATUS'.
  SET TITLEBAR 'TITULO'.

  CLEAR pt_fieldcat.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name = 'ZPPT0017'
    CHANGING
      ct_fieldcat      = pt_fieldcat.

  CLEAR ls_fcat.

  LOOP AT pt_fieldcat INTO ls_fcat.
    IF ls_fcat-fieldname EQ 'SITUACAO'.
      ls_fcat-col_pos = 1.
      ls_fcat-edit = abap.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 08 .
      ls_fcat-coltext = 'Situação'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'WERKS'.
      ls_fcat-col_pos = 2.
      ls_fcat-edit = abap.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 06 .
      ls_fcat-coltext = 'Centro'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'MATNR_AC'.
      ls_fcat-col_pos = 3.
      ls_fcat-edit = abap01.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 15.
      ls_fcat-coltext = 'Produto Acabado'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'MAKTX_AC'.
      ls_fcat-col_pos = 4.
      ls_fcat-outputlen = 40.
      ls_fcat-coltext = 'Desc. Produto Acabado'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'MATNR_RV'.
      ls_fcat-col_pos = 5.
      ls_fcat-edit = abap.
      ls_fcat-checktable = '!'.
      ls_fcat-outputlen = 15.
      ls_fcat-coltext = 'Produto Revenda'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'MAKTX_RV'.
      ls_fcat-col_pos = 6.
      ls_fcat-outputlen = 40.
      ls_fcat-coltext = 'Desc. Produto Revenda'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'USUARIO_C'.
      ls_fcat-col_pos = 16.
      ls_fcat-coltext = 'Criado por'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'DATA_ATUAL_C'.
      ls_fcat-col_pos = 12.
      ls_fcat-coltext = 'Data Criação'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'HORA_ATUAL_C'.
      ls_fcat-col_pos = 16.
      ls_fcat-coltext = 'Hora Criação'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'USUARIO_M'.
      ls_fcat-col_pos = 20.
      ls_fcat-coltext = 'Modificado por'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'DATA_ATUAL_M'.
      ls_fcat-col_pos = 13.
      ls_fcat-coltext = 'Data Modif.'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ELSEIF ls_fcat-fieldname EQ 'HORA_ATUAL_M'.
      ls_fcat-col_pos = 14.
      ls_fcat-coltext = 'Hora  Modif.'.
      MODIFY pt_fieldcat FROM ls_fcat.
    ENDIF.
  ENDLOOP.

  IF g_container IS INITIAL.

    wa_stable = 'X'.
    g_container = 'CONTAINER'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = g_custom_container.

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

    PERFORM seleciona_dados.

    CALL METHOD cl_gui_cfw=>flush.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        it_toolbar_excluding = tl_function
      CHANGING
        it_fieldcatalog      = pt_fieldcat
        it_outtab            = it_saida.

    SET HANDLER:
      lcl_event_handler=>on_button_click FOR g_grid,
      lcl_event_handler=>on_data_changed FOR g_grid,
      lcl_event_handler=>on_data_changed_finished FOR g_grid.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    CALL METHOD g_grid->set_ready_for_input
      EXPORTING
        i_ready_for_input = 1.

  ELSE.

    CALL METHOD g_grid->set_frontend_fieldcatalog
      EXPORTING
        it_fieldcatalog = pt_fieldcat.

    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  DATA: wl_insert LIKE LINE OF it_saida,
        wl_edit   LIKE LINE OF it_saida,
        ans       TYPE c.

  CASE sy-ucomm.
    WHEN 'SALVAR'.

      PERFORM salvar_dados.

      CALL METHOD g_grid->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = pt_fieldcat.

      CALL METHOD g_grid->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN 'INS'.
      abap = 'X'.
      abap01 = 'X'.
      usnam = sy-ucomm.
      MOVE:
          sy-uname TO wl_insert-usuario_c,
          sy-datum TO wl_insert-data_atual_c,
          sy-uzeit TO wl_insert-hora_atual_c,
          'A'      TO wl_insert-situacao.

      APPEND wl_insert TO it_saida.

    WHEN 'EDIT'.
      abap = 'X'.
      abap01 = ''.
      usnam  = sy-ucomm.
      MOVE:
          sy-uname TO wa_saida_mod-usuario_m,
          sy-datum TO wa_saida_mod-data_atual_m,
          sy-uzeit TO wa_saida_mod-hora_atual_m.

    WHEN 'ELI'.
      CLEAR ans.

      CALL METHOD g_grid->get_selected_rows
        IMPORTING
          et_index_rows = tg_selectedrow.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar      = 'Confirmação'
          text_question = 'Deseja eliminar a(s) linha(s) selecionada(s)?'
          text_button_1 = 'Sim'
          icon_button_1 = 'ICON_CHECKED'
          text_button_2 = 'Não'
          icon_button_2 = 'ICON_CANCEL'
          popup_type    = 'ICON_MESSAGE_ERROR'
        IMPORTING
          answer        = ans.

      CASE ans.
        WHEN 2 OR 'A'.
          LEAVE TO CURRENT TRANSACTION.
        WHEN 1.
          PERFORM excluir_dados.
      ENDCASE.

    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

  IF g_grid IS NOT INITIAL.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = wa_stable.
  ENDIF.
ENDMODULE.

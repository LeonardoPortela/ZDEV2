*----------------------------------------------------------------------*
***INCLUDE LZGFSD006F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_user_command_popup
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_user_command
*&---------------------------------------------------------------------*
FORM f_user_command_popup USING p_ucomm TYPE salv_de_function.

  DATA lv_erro.

  CASE p_ucomm.

    WHEN 'ENTER'.

      PERFORM f_fill_selected_rows_popup CHANGING lv_erro .

      CHECK lv_erro IS INITIAL.

      gv_popup_processa = 'X'.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.

      gv_popup_processa = space.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_fill_selected_rows
*&---------------------------------------------------------------------*
FORM f_fill_selected_rows_popup CHANGING cv_erro.

  DATA: lo_selections TYPE REF TO cl_salv_selections.

  DATA lt_rows TYPE salv_t_row.

  DATA ls_row TYPE i.

  CLEAR gt_rows.

  lo_selections = go_alv->get_selections( ).

  gt_rows = lo_selections->get_selected_rows( ).

  LOOP AT gt_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).

    READ TABLE gt_0378 ASSIGNING FIELD-SYMBOL(<fs_0378>)
      INDEX <fs_rows>.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_0378_r ASSIGNING FIELD-SYMBOL(<fs_0378_r>)
      WITH KEY checkid_r = <fs_0378>-checkid.

    IF sy-subrc EQ 0.

      READ TABLE gt_0378 ASSIGNING FIELD-SYMBOL(<fs_mae>)
        WITH KEY checkid = <fs_0378_r>-checkid.

      IF sy-subrc EQ 0.
        DATA(lv_tabix) = sy-tabix.

        READ TABLE gt_rows TRANSPORTING NO FIELDS
          WITH KEY table_line = lv_tabix.

        IF sy-subrc NE 0.
          MESSAGE s016(ds) WITH 'Selecionar a pergunta superior' <fs_mae>-checkid DISPLAY LIKE 'E'.
          cv_erro = abap_true.
          EXIT.
        ENDIF.


      ENDIF.

    ENDIF.


    APPEND INITIAL LINE TO gt_perguntas ASSIGNING FIELD-SYMBOL(<fs_perguntas>).

    MOVE-CORRESPONDING <fs_0378> TO <fs_perguntas>.

    <fs_perguntas>-changed = abap_true.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_LEAVE_TO_SCREEN
*&---------------------------------------------------------------------*
FORM f_leave_to_screen .

*  DATA lv_answer TYPE c VALUE '1'.
*
*  IF gv_changed = abap_true.
*    PERFORM f_popup_to_confirm USING 'Dados não foram gravados, sair mesmo assim?' CHANGING lv_answer.
*  ENDIF.
*
*  CHECK lv_answer EQ '1'.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_CREATE_ALV_7001 .
*&---------------------------------------------------------------------*
FORM f_create_alv_7001 .

  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler_7001.

  PERFORM f_get_alv_layout_7001 CHANGING lw_layout.

  IF go_container_7001 IS INITIAL.

    " Create a custom container control for our ALV Control
    CREATE OBJECT go_container_7001
      EXPORTING
        container_name              = g_container_7001
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      MESSAGE i000(zpp) WITH 'The custom control could not be created'.
*      RETURN.
    ENDIF.

  ENDIF.

  IF go_cc_alv_02 IS INITIAL.

    CREATE OBJECT go_cc_alv_02
      EXPORTING
        i_parent = go_container_7001.

    PERFORM f_monta_fieldcat_7001.

*  CALL METHOD go_005_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CREATE OBJECT lo_handle.

    SET HANDLER lo_handle->handle_double_click FOR go_cc_alv_02.
*    SET HANDLER lo_handle->handle_user_command FOR go_cc_alv_02.
    "SET HANDLER lo_handle->handle_hotspot_click FOR go_cc_alv_02.

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.

    " Configuration for first display.
    CALL METHOD go_cc_alv_02->set_table_for_first_display
      EXPORTING
        is_layout       = lw_layout
        is_variant      = lw_variant
        i_save          = 'A'
      CHANGING
        it_filter       = gt_filter_7001
        it_outtab       = gt_alv_7001
        it_fieldcatalog = gt_fcat_7001.

  ELSE.

    CALL METHOD go_cc_alv_02->set_frontend_layout
      EXPORTING
        is_layout = lw_layout.

    CALL METHOD go_cc_alv_02->refresh_table_display( ).


  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_create_alv
*&---------------------------------------------------------------------*
FORM f_create_alv .

  DATA lw_settings TYPE lvc_s_glay.
  DATA lw_layout TYPE lvc_s_layo.
  DATA lw_variant TYPE disvariant.

  DATA lo_events TYPE REF TO cl_salv_events_table.
  DATA lo_handle TYPE REF TO lcl_event_handler.

  PERFORM f_get_alv_layout CHANGING lw_layout.

  IF go_container_8001 IS INITIAL.

    " Create a custom container control for our ALV Control
    CREATE OBJECT go_container_8001
      EXPORTING
        container_name              = g_container_8001
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    IF sy-subrc NE 0.
*      MESSAGE i000(zpp) WITH 'The custom control could not be created'.
*      RETURN.
    ENDIF.

  ENDIF.

  IF go_cc_alv_01 IS INITIAL.

    CREATE OBJECT go_cc_alv_01
      EXPORTING
        i_parent = go_container_8001.

    PERFORM f_monta_fieldcat_8001.

*  CALL METHOD go_005_alv->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CREATE OBJECT lo_handle.

    SET HANDLER lo_handle->handle_toolbar FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_user_command FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_hotspot_click FOR go_cc_alv_01.
    SET HANDLER lo_handle->handle_double_click FOR go_cc_alv_01.

    "SET HANDLER lo_handle->handle_top_of_page FOR go_005_alv.

    "PERFORM f_filtro_lote_alv.

    "go_cc_alv_01->SET_

    lw_variant-report       = sy-repid.
    lw_variant-username     = sy-uname.

    " Configuration for first display.
    CALL METHOD go_cc_alv_01->set_table_for_first_display
      EXPORTING
        is_layout       = lw_layout
        is_variant      = lw_variant
        i_save          = 'A'
      CHANGING
        it_outtab       = gt_alv_8001
        it_fieldcatalog = gt_fcat_8001.

  ELSE.

    CALL METHOD go_cc_alv_01->set_frontend_layout
      EXPORTING
        is_layout = lw_layout.

    CALL METHOD go_cc_alv_01->refresh_table_display( ).


  ENDIF.




ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_fieldcat
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat_8001.

  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZSDS378'
      i_internal_tabname     = 'GT_ALV_8001'
    CHANGING
      ct_fieldcat            = gt_fcat_8001
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE gt_fcat_8001 WHERE fieldname = 'SELEC'.
  DELETE gt_fcat_8001 WHERE fieldname = 'ICON'.
  DELETE gt_fcat_8001 WHERE fieldname = 'MSGTX'.
  DELETE gt_fcat_8001 WHERE fieldname = 'MANDT'.
  DELETE gt_fcat_8001 WHERE fieldname = 'INATIVAR'.
  DELETE gt_fcat_8001 WHERE fieldname = 'USER_CREATE'.
  DELETE gt_fcat_8001 WHERE fieldname = 'DATE_CREATE'.
  DELETE gt_fcat_8001 WHERE fieldname = 'TIME_CREATE'.
  DELETE gt_fcat_8001 WHERE fieldname = 'USER_CHANGE'.
  DELETE gt_fcat_8001 WHERE fieldname = 'DATE_CHANGE'.
  DELETE gt_fcat_8001 WHERE fieldname = 'TIME_CHANGE'.
  DELETE gt_fcat_8001 WHERE fieldname = 'DELETED'.
  DELETE gt_fcat_8001 WHERE fieldname = 'TPCHECK_TXT'.
  DELETE gt_fcat_8001 WHERE fieldname = 'TPCOND_TXT'.
  DELETE gt_fcat_8001 WHERE fieldname = 'TPINCONF_TXT'.
  DELETE gt_fcat_8001 WHERE fieldname = 'FLAG_VINCULO'.
  DELETE gt_fcat_8001 WHERE fieldname = 'FLAG_CHANGED'.
  DELETE gt_fcat_8001 WHERE fieldname = 'CELLTAB'.
  DELETE gt_fcat_8001 WHERE fieldname = 'COLOR'.

  READ TABLE gt_fcat_8001 ASSIGNING FIELD-SYMBOL(<fs_fcat>)
     WITH KEY fieldname = 'ICON'.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext = 'Status'.
    <fs_fcat>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fcat_8001 ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'VINCULO'.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext = 'Vínculo'.
    <fs_fcat>-dd_outlen = 000010.
    <fs_fcat>-hotspot = abap_true.
  ENDIF.

  READ TABLE gt_fcat_8001 ASSIGNING <fs_fcat>
    WITH KEY fieldname = 'PERGUNTA'.

  IF sy-subrc EQ 0.
    <fs_fcat>-reptext = 'Pergunta'.
    <fs_fcat>-dd_outlen = 000010.
  ENDIF.

  PERFORM f_fieldcat_modi USING 'CHECKID' 'JUST' 'C' CHANGING gt_fcat_8001.

  "PERFORM f_coluna_descr USING 'MSGTX' 'Status'.

  "PERFORM f_coluna_descr USING 'COUNT_SAP' 'Notas SAP'.

  "PERFORM f_set_edit USING 'WERKS'.

  "PERFORM f_coluna_edita2 USING 'FFIN_NOTIF' 'Data' 'Data'.

  "<fs_fcat>-edit = p_editavel.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_REFRESH_8001
*&---------------------------------------------------------------------*
FORM f_refresh_8001 .

  CLEAR: "go_container_8001,
         "go_cc_alv_01,
         gt_fcat_8001,
         gt_alv_8001,
         gv_ucomm_8001,
         gt_zsdt0380,
         gt_zsdt0380_r.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_handle_toolbar
*&---------------------------------------------------------------------*
FORM f_handle_toolbar USING e_object TYPE REF TO  cl_alv_event_toolbar_set
                            e_interactive	TYPE char01.

  DATA: ls_toolbar TYPE stb_button.

  IF zsds379-desativar IS NOT INITIAL.

    "--- Append a separator to normal toolbar
    CLEAR ls_toolbar.
    MOVE 3 TO ls_toolbar-butn_type.
    APPEND ls_toolbar TO e_object->mt_toolbar.

    CLEAR ls_toolbar.
    MOVE 'ADD' TO ls_toolbar-function.
    MOVE icon_insert_row TO ls_toolbar-icon.
    MOVE 'Perguntas' TO ls_toolbar-quickinfo.
    MOVE 'Perguntas' TO ls_toolbar-text.
    MOVE ' ' TO ls_toolbar-disabled.
    APPEND ls_toolbar TO e_object->mt_toolbar.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_user_command
*&---------------------------------------------------------------------*
FORM handle_user_command USING uv_ucomm.

  CASE uv_ucomm.
    WHEN 'ADD'.
      PERFORM f_add_perguntas.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_add_perguntas
*&---------------------------------------------------------------------*
FORM f_add_perguntas .

  DATA lt_relacao TYPE zsdc378_r.
  DATA et_perguntas TYPE zsdc380.
  DATA lv_canc TYPE c.

  CALL FUNCTION 'ZSDMF_SEL_PERGUNTAS_POPUP'
    EXPORTING
      it_0380      = gt_zsdt0380
      iv_so_relac  = abap_false
    IMPORTING
      ev_canc      = lv_canc
      et_perguntas = et_perguntas.

  CHECK lv_canc IS INITIAL.

  IF gt_zsdt0380_r IS NOT INITIAL.

    SELECT * FROM zsdt0378_r
     INTO TABLE lt_relacao
       FOR ALL ENTRIES IN gt_zsdt0380_r
         WHERE checkid = gt_zsdt0380_r-checkid.

  ELSE.

    SELECT * FROM zsdt0378_r
     INTO TABLE lt_relacao
         WHERE deleted = abap_false.

  ENDIF.

  LOOP AT et_perguntas ASSIGNING FIELD-SYMBOL(<fs_perguntas>).

    APPEND INITIAL LINE TO gt_alv_8001 ASSIGNING FIELD-SYMBOL(<fs_8001>).

    MOVE-CORRESPONDING <fs_perguntas> TO <fs_8001>.

    <fs_8001>-flag_changed = <fs_perguntas>-changed.

    APPEND INITIAL LINE TO gt_zsdt0380 ASSIGNING FIELD-SYMBOL(<fs_0380>).

    MOVE-CORRESPONDING <fs_perguntas> TO <fs_0380>.

    LOOP AT lt_relacao ASSIGNING FIELD-SYMBOL(<fs_relacao>) WHERE checkid = <fs_8001>-checkid.

      READ TABLE gt_zsdt0380_r TRANSPORTING NO FIELDS
        WITH KEY checkid = <fs_8001>-checkid
                 checkid_r = <fs_relacao>-checkid_r.

      IF sy-subrc NE 0.

        APPEND INITIAL LINE TO gt_zsdt0380_r ASSIGNING FIELD-SYMBOL(<fs_zsdt0380_r>).

        <fs_zsdt0380_r>-checklistid = zsds379-checklistid.
        <fs_zsdt0380_r>-checkid = <fs_8001>-checkid.
        <fs_zsdt0380_r>-checkid_r = <fs_relacao>-checkid_r.

      ENDIF.

    ENDLOOP.

    PERFORM f_alv_status_change CHANGING <fs_8001>.

  ENDLOOP.

  SORT gt_alv_8001 BY checkid ASCENDING.
  SORT gt_zsdt0380 BY checklistid checkid ASCENDING.
  SORT gt_zsdt0380_r BY checklistid checkid checkid_r ASCENDING.

  DELETE ADJACENT DUPLICATES FROM gt_alv_8001 COMPARING checkid.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0380 COMPARING checklistid checkid.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0380_r COMPARING checklistid checkid checkid_r.


  PERFORM f_refresh_alv USING go_cc_alv_01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_alv_status_change
*&---------------------------------------------------------------------*
FORM f_alv_status_change CHANGING cs_alv TYPE zsds378.

  cs_alv-icon = icon_checked.
  cs_alv-msgtx = 'Ativo'.

  PERFORM f_check_vinculo CHANGING cs_alv.

  IF cs_alv-flag_vinculo = abap_true.
    cs_alv-vinculo = icon_display_more.
  ELSE.
    cs_alv-vinculo = icon_enter_more.
  ENDIF.

  IF cs_alv-inativar = abap_true.

    cs_alv-icon = icon_dummy.
    cs_alv-msgtx = 'Inativo'.

  ENDIF.

  IF cs_alv-deleted = abap_true.
    cs_alv-icon = icon_delete.
    cs_alv-msgtx = 'Excluído'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_alv
*&---------------------------------------------------------------------*
FORM f_refresh_alv USING uo_alv TYPE REF TO cl_gui_alv_grid.

  DATA ls_layout TYPE lvc_s_layo.
  DATA ls_stable TYPE	lvc_s_stbl.

  ls_stable-col = abap_true.
  ls_stable-row = abap_true.

  PERFORM f_get_alv_layout CHANGING ls_layout.

  CALL METHOD uo_alv->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.

  uo_alv->refresh_table_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_alv
*&---------------------------------------------------------------------*
FORM f_refresh_alv_7001 USING uo_alv TYPE REF TO cl_gui_alv_grid.

  DATA ls_layout TYPE lvc_s_layo.
  DATA ls_stable TYPE	lvc_s_stbl.

  ls_stable-col = abap_true.
  ls_stable-row = abap_true.

  PERFORM f_get_alv_layout_7001 CHANGING ls_layout.

  CALL METHOD uo_alv->set_frontend_layout
    EXPORTING
      is_layout = ls_layout.

  uo_alv->refresh_table_display( ).

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_FIELDCAT_MODI
*&---------------------------------------------------------------------*
FORM f_fieldcat_modi USING p_fieldname TYPE slis_fieldname
                           p_column TYPE c
                           p_value TYPE any
                  CHANGING p_field_cat TYPE lvc_t_fcat.

  READ TABLE p_field_cat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  DATA(lv_name) = '<FS_FCAT>-' && p_column.

  ASSIGN (lv_name) TO FIELD-SYMBOL(<fs_colum>).

  CHECK sy-subrc EQ 0.

  <fs_colum> = p_value.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_alv_layout
*&---------------------------------------------------------------------*
FORM f_get_alv_layout CHANGING cs_layout TYPE lvc_s_layo.

  cs_layout-sel_mode = 'A'.
  cs_layout-col_opt = 'X'.
  cs_layout-cwidth_opt = 'X'.

  cs_layout-box_fname = 'SELEC'.
  cs_layout-zebra = 'X'.
  cs_layout-stylefname = 'CELLTAB'.
  cs_layout-info_fname = 'COLOR'.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_alv_layout
*&---------------------------------------------------------------------*
FORM f_get_alv_layout_7001 CHANGING cs_layout TYPE lvc_s_layo.

  cs_layout-sel_mode = 'A'.
  "cs_layout-col_opt = 'X'.
  "cs_layout-cwidth_opt = 'X'.

  "cs_layout-box_fname = 'SELEC'.
  cs_layout-zebra = 'X'.
  cs_layout-stylefname = 'CELLTAB'.
  cs_layout-info_fname = 'COLOR'.

  cs_layout-no_toolbar = abap_true.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_save_8001
*&---------------------------------------------------------------------*
FORM f_save_8001 .

  DATA lt_0380 TYPE TABLE OF zsdt0380.
  DATA lt_0380_r TYPE TABLE OF zsdt0380_r.
  DATA lv_change.

  LOOP AT gt_alv_8001 ASSIGNING FIELD-SYMBOL(<fs_alv>) WHERE flag_changed = abap_true.

    READ TABLE gt_zsdt0380 TRANSPORTING NO FIELDS
      WITH KEY checklistid = zsds379-checklistid
               checkid = <fs_alv>-checkid.

    IF sy-subrc EQ 0.
      lv_change = abap_true.
    ELSE.
      lv_change = abap_false.
    ENDIF.

    APPEND INITIAL LINE TO lt_0380 ASSIGNING FIELD-SYMBOL(<fs_0380>).

    MOVE-CORRESPONDING <fs_alv> TO <fs_0380>.

    <fs_0380>-checklistid = zsds379-checklistid.

    IF lv_change = abap_true.

      <fs_0380>-date_change = sy-datum.
      <fs_0380>-time_change = sy-uzeit.
      <fs_0380>-user_change = sy-uname.

    ELSE.

      <fs_0380>-date_create = sy-datum.
      <fs_0380>-time_create = sy-uzeit.
      <fs_0380>-user_create = sy-uname.

    ENDIF.

    LOOP AT gt_zsdt0380_r ASSIGNING FIELD-SYMBOL(<fs_0380_r>) WHERE checkid = <fs_0380>-checkid.
      APPEND <fs_0380_r> TO lt_0380_r.
    ENDLOOP.

  ENDLOOP.

  IF lt_0380[] IS NOT INITIAL.
    MODIFY zsdt0380 FROM TABLE lt_0380.
  ENDIF.

  IF lt_0380_r[] IS NOT INITIAL.
    MODIFY zsdt0380_r FROM TABLE lt_0380_r.
  ENDIF.

  COMMIT WORK AND WAIT.

  MESSAGE s016(ds) WITH 'Dados gravados'.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_vinculo
*&---------------------------------------------------------------------*
FORM f_check_vinculo CHANGING cs_alv TYPE zsds378.

  READ TABLE gt_zsdt0380_r TRANSPORTING NO FIELDS
    WITH KEY checkid = cs_alv-checkid.

  IF sy-subrc EQ 0.
    cs_alv-flag_vinculo = abap_true.
  ELSE.
    cs_alv-flag_vinculo = abap_false.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_hotspot_click
*&---------------------------------------------------------------------*
FORM handle_double_click_7001 USING uv_row_id TYPE lvc_s_row
                           uv_column_id TYPE lvc_s_col
                           uv_row_no TYPE lvc_s_roid.

  READ TABLE gt_alv_7001 ASSIGNING FIELD-SYMBOL(<fs_alv>)
    INDEX uv_row_id.

  CHECK sy-subrc EQ 0.

  IF <fs_alv>-tpcheck = 'C'.

    PERFORM f_get_text_7001 USING <fs_alv>-pergunta CHANGING <fs_alv>-texto.

  ELSE.

    CHECK zsds0006-edit = abap_true.

    IF uv_column_id = 'RADIO_SIM'.
      <fs_alv>-flag_sim = abap_true.
      <fs_alv>-flag_nao = abap_false.

      IF <fs_alv>-tpinconf = 'S'.
        <fs_alv>-flag_incoformidade = abap_true.
      ELSE.
        <fs_alv>-flag_incoformidade = abap_false.
      ENDIF.

    ENDIF.

    IF uv_column_id = 'RADIO_NAO'.

      <fs_alv>-flag_sim = abap_false.
      <fs_alv>-flag_nao = abap_true.

      IF <fs_alv>-tpinconf = 'N'.
        <fs_alv>-flag_incoformidade = abap_true.
      ELSE.
        <fs_alv>-flag_incoformidade = abap_false.
      ENDIF.

    ENDIF.

    PERFORM f_check_hide USING <fs_alv>.

    CLEAR <fs_alv>-color.

    PERFORM f_check_inconformidade.

  ENDIF.

  PERFORM f_alv_2_icons CHANGING <fs_alv>.

  PERFORM f_refresh_alv_7001 USING go_cc_alv_02.

  PERFORM f_dispach.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_hotspot_click
*&---------------------------------------------------------------------*
FORM f_hotspot_click USING uv_row_id TYPE lvc_s_row
                           uv_column_id TYPE lvc_s_col
                           uv_row_no TYPE lvc_s_roid.

  CASE uv_column_id.
    WHEN 'VINCULO'.

      READ TABLE gt_alv_8001 ASSIGNING FIELD-SYMBOL(<fs_alv>)
        INDEX uv_row_id.

      IF sy-subrc EQ 0.
        PERFORM f_marcar_vinculo CHANGING <fs_alv>.
      ENDIF.

    WHEN OTHERS.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_marcar_vinculo
*&---------------------------------------------------------------------*
FORM f_marcar_vinculo CHANGING us_alv TYPE zsds378.

  DATA lv_canc.
  DATA lt_relacao TYPE TABLE OF zsdt0378_r.
  DATA lt_0380 TYPE TABLE OF zsdt0380.

  us_alv-color = 'C500'.

  PERFORM f_refresh_alv USING go_cc_alv_01.

  LOOP AT gt_zsdt0380_r ASSIGNING FIELD-SYMBOL(<fs_0380_r>) WHERE checkid = us_alv-checkid.

    APPEND INITIAL LINE TO lt_relacao ASSIGNING FIELD-SYMBOL(<fs_relacao>).

    MOVE-CORRESPONDING <fs_0380_r> TO <fs_relacao>.

  ENDLOOP.

  CHECK lt_relacao IS NOT INITIAL.

  SELECT * FROM zsdt0378
    INTO CORRESPONDING FIELDS OF TABLE lt_0380
    FOR ALL ENTRIES IN lt_relacao
      WHERE checkid = lt_relacao-checkid_r.

  CALL FUNCTION 'ZSDMF_SEL_PERGUNTAS_POPUP'
    EXPORTING
      iv_checkid  = us_alv-checkid
      it_0380     = lt_0380
      iv_so_relac = abap_true
    IMPORTING
      ev_canc     = lv_canc
    CHANGING
      ct_relacao  = lt_relacao.

  CLEAR us_alv-color.

  PERFORM f_refresh_alv USING go_cc_alv_01.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh_7001
*&---------------------------------------------------------------------*
FORM f_refresh_7001 .

  CLEAR: zsds0006,
         gt_fcat_7001,
         gt_alv_7001,
         gv_ucomm_7001,
         gt_filter_7001,

         gt_zsdt0380_r.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_fieldcat_7001
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat_7001 .

  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
*     I_BUFFER_ACTIVE        =
      i_structure_name       = 'ZSDS0006'
*     I_CLIENT_NEVER_DISPLAY = 'X'
*     I_BYPASSING_BUFFER     =
*     i_internal_tabname     = 'GT_FIELDCAT2'
    CHANGING
      ct_fieldcat            = gt_fcat_7001
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc <> 0.
    "PERFORM f_mensagem_sistema.
  ENDIF.

  DELETE gt_fcat_7001 WHERE fieldname = 'CHECKID'.
  DELETE gt_fcat_7001 WHERE fieldname = 'ICON'.
  DELETE gt_fcat_7001 WHERE fieldname = 'SELEC'.
  DELETE gt_fcat_7001 WHERE fieldname = 'ATIVO'.
  DELETE gt_fcat_7001 WHERE fieldname = 'DOC_SIMULACAO'.
  DELETE gt_fcat_7001 WHERE fieldname = 'CHECKLISTID'.
  DELETE gt_fcat_7001 WHERE fieldname = 'SIM_NAO_INCONFOR'.
  DELETE gt_fcat_7001 WHERE fieldname = 'FLAG_SIM'.
  DELETE gt_fcat_7001 WHERE fieldname = 'FLAG_NAO'.

  DELETE gt_fcat_7001 WHERE fieldname = 'BUKRS'.
  DELETE gt_fcat_7001 WHERE fieldname = 'VKBUR'.


  DELETE gt_fcat_7001 WHERE fieldname = 'TPCHECK'.
  DELETE gt_fcat_7001 WHERE fieldname = 'TPCOND'.
  DELETE gt_fcat_7001 WHERE fieldname = 'TPINCONF'.

  DELETE gt_fcat_7001 WHERE fieldname = 'ATIVO'.

  DELETE gt_fcat_7001 WHERE fieldname = 'EDIT'.
  DELETE gt_fcat_7001 WHERE fieldname = 'HIDE'.

  DELETE gt_fcat_7001 WHERE fieldname = 'FLAG_INCOFORMIDADE'.
  DELETE gt_fcat_7001 WHERE fieldname = 'ICON_INCOFORMIDADE'.
  DELETE gt_fcat_7001 WHERE fieldname = 'TXT_INCOFORMIDADE'.
  DELETE gt_fcat_7001 WHERE fieldname = 'COLOR'.

  READ TABLE gt_fcat_7001 ASSIGNING FIELD-SYMBOL(<fs_field>)
    WITH KEY fieldname = 'CHECKID'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    "<fs_field>-reptext = 'Ativo'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fcat_7001 ASSIGNING <fs_field>
    WITH KEY fieldname = 'PERGUNTA'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = 'Item a ser Avaliado/Apontado'.
    <fs_field>-col_opt = abap_true.
  ENDIF.

  READ TABLE gt_fcat_7001 ASSIGNING <fs_field>
    WITH KEY fieldname = 'TEXTO'.

  IF sy-subrc EQ 0.
    <fs_field>-reptext = 'Valores'.
    <fs_field>-col_opt = abap_true.
  ENDIF.

  READ TABLE gt_fcat_7001 ASSIGNING <fs_field>
    WITH KEY fieldname = 'RADIO_SIM'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Sim'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fcat_7001 ASSIGNING <fs_field>
    WITH KEY fieldname = 'RADIO_NAO'.

  IF sy-subrc EQ 0.
    <fs_field>-just = 'C'.
    <fs_field>-reptext = 'Não'.
    <fs_field>-dd_outlen = 000010.
  ENDIF.

  READ TABLE gt_fcat_7001 ASSIGNING <fs_field>
    WITH KEY fieldname = 'PERGUNTA'.

  IF sy-subrc EQ 0.
    <fs_field>-dd_outlen = 000020.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_ALV_2_ICONS
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <FS_ALV2>
*&---------------------------------------------------------------------*
FORM f_alv_2_icons CHANGING cs_alv2 TYPE zsds0006.

  IF cs_alv2-ativo IS INITIAL.
    cs_alv2-icon = '@02@'.
  ELSE.
    cs_alv2-icon = '@01@'.
  ENDIF.

  IF cs_alv2-tpcheck = 'C'.
    cs_alv2-radio_sim = '@0O@'.
  ELSE.

    IF cs_alv2-flag_sim IS INITIAL.
      cs_alv2-radio_sim = '@SR@'.
    ELSE.
      cs_alv2-radio_sim = '@R6@'.
    ENDIF.

    IF cs_alv2-flag_nao IS INITIAL.
      cs_alv2-radio_nao = '@SR@'.
    ELSE.
      cs_alv2-radio_nao = '@R6@'.
    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_text_init_7001
*&---------------------------------------------------------------------*
FORM f_text_init_7001 .

*  DATA lt_text TYPE TABLE OF txw_note.
*  DATA lt_trtexts TYPE trtexts.
  DATA lv_texto TYPE string.

  IF gr_custom_cont IS NOT BOUND.

    CREATE OBJECT gr_custom_cont
      EXPORTING
        container_name = 'CC_HEADER_TEXT'
        repid          = sy-repid
        dynnr          = sy-dynnr.
  ENDIF.

  IF gr_text_edit IS NOT BOUND.

    CREATE OBJECT gr_text_edit
      EXPORTING
        wordwrap_mode     = 1 " 0: OFF; 1: wrap a window border; 2: wrap at fixed pos
        wordwrap_position = 254   " pos of wordwrap, only makes sense with wordwrap_mode=2
        parent            = gr_custom_cont.

  ENDIF.

  gr_text_edit->set_toolbar_mode( 0 ).
  gr_text_edit->set_statusbar_mode( gr_text_edit->false ).
  gr_text_edit->set_enable( abap_false ).

  IF gt_alv_7001_full IS NOT INITIAL.

    DATA: lt_text_stream TYPE STANDARD TABLE OF zsde_questio,
          lv_question    TYPE zsde_questio,
          lv_string      TYPE string.

    LOOP AT gt_alv_7001_full ASSIGNING FIELD-SYMBOL(<fs_7001>) WHERE tpcheck = 'T'.

      lv_question = <fs_7001>-pergunta.
      REPLACE ALL OCCURRENCES OF REGEX `[\t\v\n\r]` IN lv_question WITH space.
      CONDENSE lv_question.

      APPEND lv_question TO lt_text_stream.
      APPEND cl_abap_char_utilities=>cr_lf TO lt_text_stream.

    ENDLOOP.

    CONCATENATE LINES OF lt_text_stream INTO lv_texto.

    "REPLACE ALL OCCURRENCES OF REGEX `[\t\v\n\r]` IN lv_texto WITH space.

    "REPLACE ALL OCCURRENCES OF `. ` IN lv_texto WITH `\n`.

*    SPLIT lv_texto AT `.` INTO TABLE DATA(lt_texto).
*
*    LOOP AT lt_texto INTO data(ls_line).
*      lv_texto = lv_texto && `\n\r` && ls_line-
*    ENDLOOP.

    CALL METHOD gr_text_edit->set_textstream
      EXPORTING
        text = lv_texto.

  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_filtro_7001
*&---------------------------------------------------------------------*
FORM f_filtro_7001 USING uv_hide TYPE c.



  APPEND INITIAL LINE TO gt_filter_7001 ASSIGNING FIELD-SYMBOL(<fs_filter>).

  <fs_filter>-fieldname = 'HIDE'.
  <fs_filter>-sign = 'I'.
  <fs_filter>-option = 'EQ'.
  <fs_filter>-low = space.
  <fs_filter>-order = 1.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_dependencia
*&---------------------------------------------------------------------*
FORM f_check_dependencia USING uv_checklistid TYPE zsdchecklistid
                               uv_checkid TYPE zsdcheckid
                      CHANGING cv_hide TYPE c.

  READ TABLE gt_zsdt0380_r TRANSPORTING NO FIELDS
    WITH KEY checklistid = uv_checklistid
             checkid_r = uv_checkid.

  CHECK sy-subrc EQ 0.

  cv_hide = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_text_7001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- <FS_ALV>_TEXTO
*&---------------------------------------------------------------------*
FORM f_get_text_7001 USING uv_title CHANGING cv_texto TYPE c.

  DATA lv_code TYPE c.

  DATA lt_fields  TYPE TABLE OF sval.

  lt_fields =
    VALUE #( ( tabname = 'ZSDS0006' fieldname = 'TEXTO' field_obl = 'X' fieldtext = uv_title value =  cv_texto )  ).

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
      popup_title     = sy-title
    IMPORTING
      returncode      = lv_code
    TABLES
      fields          = lt_fields
    EXCEPTIONS
      error_in_fields = 1
      OTHERS          = 2.

  CHECK lv_code IS INITIAL AND lt_fields[] IS NOT INITIAL.

  CHECK zsds0006-edit = abap_true.

  LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<fs_field>) WHERE value IS NOT INITIAL.

    CASE <fs_field>-fieldname.
      WHEN 'TEXTO'.
        cv_texto = <fs_field>-value.
    ENDCASE.

  ENDLOOP.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_save_7001
*&---------------------------------------------------------------------*
FORM f_save_7001 .

  DATA ls_0381 TYPE zsdt0381.
  DATA lt_0382 TYPE TABLE OF zsdt0382.
  DATA lv_inconfor TYPE c.
  DATA lv_checklistid TYPE zsdchecklistid.
  DATA lv_erro.

  PERFORM f_verifica_erros CHANGING lv_erro.

  CHECK lv_erro IS INITIAL.

  LOOP AT gt_alv_7001 ASSIGNING FIELD-SYMBOL(<fs_alv2>).

    READ TABLE gt_alv_7001_full ASSIGNING FIELD-SYMBOL(<fs_alv>)
      WITH KEY doc_simulacao = <fs_alv2>-doc_simulacao
               checklistid = <fs_alv2>-checklistid
               checkid = <fs_alv2>-checkid.

    CHECK sy-subrc EQ 0.

    MOVE-CORRESPONDING <fs_alv2> TO <fs_alv>.

  ENDLOOP.

  LOOP AT gt_alv_7001_full ASSIGNING <fs_alv2>.

    APPEND INITIAL LINE TO lt_0382 ASSIGNING FIELD-SYMBOL(<fs_0382>).

    <fs_0382>-doc_simulacao = <fs_alv2>-doc_simulacao.

    <fs_0382>-checklistid = <fs_alv2>-checklistid.
    <fs_0382>-checkid = <fs_alv2>-checkid.
    <fs_0382>-pergunta = <fs_alv2>-pergunta.
    <fs_0382>-tpcheck = <fs_alv2>-tpcheck.
    <fs_0382>-tpcond = <fs_alv2>-tpcond.
    <fs_0382>-tpinconf = <fs_alv2>-tpinconf.
    " = <fs_alv2>-inconformidade.
    <fs_0382>-flag_sim = <fs_alv2>-flag_sim.
    <fs_0382>-flag_nao = <fs_alv2>-flag_nao.
    <fs_0382>-texto = <fs_alv2>-texto.
    <fs_0382>-user_create = sy-uname.
    <fs_0382>-date_create = sy-datum.
    <fs_0382>-time_create = sy-uzeit.

    lv_checklistid = <fs_0382>-checklistid = <fs_alv2>-checklistid.
    <fs_0382>-checkid = <fs_alv2>-checkid.
    <fs_0382>-pergunta = <fs_alv2>-pergunta.

    <fs_0382>-flag_sim = <fs_alv2>-flag_sim.
    <fs_0382>-flag_nao = <fs_alv2>-flag_nao.
    <fs_0382>-user_create = sy-uname.
    <fs_0382>-date_create = sy-datum.
    <fs_0382>-time_create = sy-uzeit.

    IF <fs_alv2>-tpinconf = 'S' AND <fs_0382>-flag_sim = abap_true.
      <fs_0382>-inconformidade = lv_inconfor = abap_true.
    ENDIF.

    IF <fs_alv2>-tpinconf = 'N' AND <fs_0382>-flag_nao = abap_true.
      <fs_0382>-inconformidade = lv_inconfor = abap_true.
    ENDIF.

  ENDLOOP.

  IF lt_0382 IS NOT INITIAL.

    ls_0381-doc_simulacao = zsds0006-doc_simulacao.
    ls_0381-checklistid = lv_checklistid.

    IF lv_inconfor = abap_true.
      ls_0381-status = '01'. "<--- PENDENTE DE APROVAÇÃO
    ELSE.
      ls_0381-status = '04'. "<--- APROVADO
    ENDIF.

    ls_0381-flag_incoformidade = lv_inconfor.

    ls_0381-bukrs = zsds0006-bukrs.
    ls_0381-vkbur = zsds0006-vkbur.

    ls_0381-user_create = sy-uname.
    ls_0381-date_create = sy-datum.
    ls_0381-time_create = sy-uzeit.

    MODIFY zsdt0381 FROM ls_0381.
    MODIFY zsdt0382 FROM TABLE lt_0382.

  ENDIF.

  COMMIT WORK AND WAIT.

  MESSAGE s016(ds) WITH 'Gravado com sucesso'.

  LEAVE TO SCREEN 0.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_verifica_erros
*&---------------------------------------------------------------------*
FORM f_verifica_erros CHANGING cv_erro TYPE c.

  DATA lv_checkid TYPE c LENGTH 30.

  LOOP AT gt_alv_7001 ASSIGNING FIELD-SYMBOL(<fs_alv2>) WHERE color IS NOT INITIAL.
    CLEAR <fs_alv2>-color.
  ENDLOOP.

  LOOP AT gt_alv_7001 ASSIGNING <fs_alv2>.

    DATA(lv_index) = sy-tabix.

    IF <fs_alv2>-hide = abap_false.

      IF <fs_alv2>-flag_sim = abap_false AND <fs_alv2>-flag_nao = abap_false.

        IF <fs_alv2>-tpcheck = 'C' AND <fs_alv2>-texto IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        lv_checkid = <fs_alv2>-checkid.

        SHIFT lv_checkid LEFT DELETING LEADING '0'.

        CONDENSE lv_checkid.

        MESSAGE s016(ds) WITH 'Responder perguntas' 'marcardas' DISPLAY LIKE 'E'.
        cv_erro = abap_true.

        <fs_alv2>-color = 'C600'.

      ENDIF.

    ENDIF.

  ENDLOOP.

  PERFORM f_refresh_alv_7001 USING go_cc_alv_02.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_dispach
*&---------------------------------------------------------------------*
FORM f_dispach .

  cl_gui_cfw=>set_new_ok_code( new_code = 'ZXY' ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_inconformidade
*&---------------------------------------------------------------------*
FORM f_check_inconformidade.

  DATA lv_incoformidade.

  zsds0006-icon_incoformidade = '@08@'.
  zsds0006-txt_incoformidade = 'Sem Não conformidade'.

  READ TABLE gt_alv_7001 ASSIGNING FIELD-SYMBOL(<fs_alv>)
    WITH KEY tpinconf = 'S'
             flag_sim = abap_true
             hide     = abap_false. " 09.09.2025 --

  IF sy-subrc EQ 0.
    lv_incoformidade = abap_true.
  ENDIF.

  READ TABLE gt_alv_7001 ASSIGNING <fs_alv>
    WITH KEY tpinconf = 'N'
             flag_nao = abap_true
             hide     = abap_false. " 09.09.2025 --

  IF sy-subrc EQ 0.
    lv_incoformidade = abap_true.
  ENDIF.

  IF lv_incoformidade = abap_true.
    zsds0006-icon_incoformidade = '@0A@'.
    zsds0006-txt_incoformidade = 'Com Não conformidade'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_double_click_8001
*&---------------------------------------------------------------------*
FORM handle_double_click_8001 USING uv_row TYPE lvc_s_row
                                    uv_column TYPE lvc_s_col
                                    uv_row_no TYPE lvc_s_roid.

  DATA lv_canc.
  DATA lt_relacao TYPE TABLE OF zsdt0378_r.

  DATA lt_text_aux TYPE TABLE OF txw_note.
  DATA lv_edit TYPE c.
  DATA tl_texto TYPE catsxt_longtext_itab.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).


  CASE uv_column.

    WHEN 'PERGUNTA'.

      READ TABLE gt_alv_8001 ASSIGNING FIELD-SYMBOL(<fs_alv>) INDEX uv_row.

      CHECK sy-subrc EQ 0.

      <fs_alv>-color = 'C500'.

      PERFORM f_refresh_alv USING go_cc_alv_01.

      lv_texto = <fs_alv>-pergunta.

      CALL FUNCTION 'TR_SPLIT_TEXT'
        EXPORTING
          iv_text  = lv_texto
          iv_len   = 60
        IMPORTING
          et_lines = lt_trtexts.

      LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

        APPEND INITIAL LINE TO lt_text_aux ASSIGNING FIELD-SYMBOL(<fs_aux>).

        <fs_aux>-line = <fs_line>.

      ENDLOOP.

      lv_edit = abap_false.

      CALL FUNCTION 'TXW_TEXTNOTE_EDIT'
        EXPORTING
          edit_mode = lv_edit
        TABLES
          t_txwnote = lt_text_aux[].

      CLEAR <fs_alv>-color.

      PERFORM f_refresh_alv USING go_cc_alv_01.

  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_hide
*&---------------------------------------------------------------------*
FORM f_check_hide USING us_alv TYPE zsds0006.

  LOOP AT gt_zsdt0380_r ASSIGNING FIELD-SYMBOL(<fs_0380_r>)
    WHERE checklistid = us_alv-checklistid
      AND checkid = us_alv-checkid.

    READ TABLE gt_alv_7001 ASSIGNING FIELD-SYMBOL(<fs_alv_r>)
      WITH KEY checkid = <fs_0380_r>-checkid_r.

    CHECK sy-subrc EQ 0.

    IF us_alv-tpcond = 'S' AND us_alv-flag_sim = abap_true.

      <fs_alv_r>-hide = abap_false.

    ELSEIF us_alv-tpcond = 'N' AND us_alv-flag_nao = abap_true.
      <fs_alv_r>-hide = abap_false.
    ELSE.
      <fs_alv_r>-hide = abap_true.
    ENDIF.

  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_inconformidade_item
*&---------------------------------------------------------------------*
FORM f_check_inconformidade_item .

  LOOP AT gt_alv_7001 ASSIGNING FIELD-SYMBOL(<fs_alv>)
      WHERE ( tpinconf = 'S' AND flag_sim = abap_true )
         OR ( tpinconf = 'N' AND flag_nao = abap_true ).

    <fs_alv>-color = 'C600'.

  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&  Include           ZSDR0060_FORMS
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  SORT
*&---------------------------------------------------------------------*
FORM sort USING p_fieldname
          CHANGING p_it_sort TYPE lvc_t_sort.

  DATA wa_sort TYPE lvc_s_sort.

  wa_sort-spos = '1' .
  wa_sort-fieldname = p_fieldname.
*-CS2021000218-22.12.2022-#91290-JT-inicio
  wa_sort-up   = abap_off.  "'X' . "A to Z
  wa_sort-down = abap_true. "space .
*-CS2021000218-22.12.2022-#91290-JT-fim
  APPEND wa_sort TO p_it_sort.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog  TABLES p_it_fieldacatalog STRUCTURE lvc_s_fcat
                           USING VALUE(p_colnum)
                                 VALUE(p_fieldname)
                                 VALUE(p_tabname)
                                 VALUE(p_emphasize)
                                 VALUE(p_edit)
                                 VALUE(p_icon)
                                 VALUE(p_hotspot)
                                 VALUE(p_opt)
                                 VALUE(p_checkbox)
                                 VALUE(p_dosum)
                                 VALUE(p_f4)
                                 VALUE(p_header).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-f4availabl = p_f4.

*** Inicio - Rubenilson - 05.09.24 - #140377
  IF wa_fieldcatalog-fieldname EQ 'DADOS_ADICIONAIS'.
    wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
  ENDIF.
*** Fim - Rubenilson - 05.09.24 - #140377

  IF wa_fieldcatalog-fieldname(3) = 'QTD'.
    wa_fieldcatalog-outputlen = 12.
  ENDIF.

  IF wa_fieldcatalog-fieldname = 'NAME1' OR
     wa_fieldcatalog-fieldname = 'MAKTX'.
    wa_fieldcatalog-outputlen = 20.
  ELSEIF wa_fieldcatalog-fieldname = 'MOEDA'.
    wa_fieldcatalog-outputlen = 5.
  ENDIF.

*-CS2019001896 - 12.08.2021 - JT - inicio
  IF wa_fieldcatalog-fieldname = 'EDITAR'.
    wa_fieldcatalog-outputlen = 06.
    wa_fieldcatalog-dd_outlen = 06.
    wa_fieldcatalog-just      = abap_true.
  ENDIF.
*-CS2019001896 - 12.08.2021 - JT - fim

*User Story 153510 - MMSILVA
  IF wa_fieldcatalog-fieldname = 'ICONE' OR
     wa_fieldcatalog-fieldname = 'ICONE2'.
    wa_fieldcatalog-ref_table  = 'ICON'.
    wa_fieldcatalog-ref_field  = 'NAME'.
    wa_fieldcatalog-just      = abap_true.
  ENDIF.
*User Story 153510 - MMSILVA

*  IF P_FIELDNAME EQ 'LIFNR' AND P_ATIVE IS INITIAL.
*    WA_FIELDCATALOG-NO_OUT = ABAP_TRUE.
*  ENDIF.

  APPEND wa_fieldcatalog TO p_it_fieldacatalog.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
FORM excluir_botoes CHANGING p_it_exclude TYPE ui_functions.

  DATA: ls_exclude     TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_print.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_check.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.
  ls_exclude = cl_gui_alv_grid=>mc_fc_info.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EXCLUIR_BOTOES
*&---------------------------------------------------------------------*
FORM excluir_botoes_7103 CHANGING p_it_exclude TYPE ui_functions.

  DATA: ls_exclude     TYPE ui_func.

  ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND ls_exclude TO p_it_exclude.
  CLEAR ls_exclude.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMA_TEXTO_OBS
*&---------------------------------------------------------------------*
FORM chama_texto_obs USING wl_name TYPE thead-tdname.

  DATA: it_texto TYPE STANDARD TABLE OF tline,
        wa_texto TYPE tline,
        tl_texto TYPE catsxt_longtext_itab,
        wl_texto TYPE LINE OF catsxt_longtext_itab.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id        = 'OBSE'
      language  = sy-langu
      name      = wl_name
      object    = 'ZTEXTO'
    TABLES
      lines     = it_texto
    EXCEPTIONS
      id        = 1
      language  = 2
      name      = 3
      not_found = 4
      OTHERS    = 5.

  IF sy-subrc IS INITIAL.
    LOOP AT it_texto INTO wa_texto.
      MOVE: wa_texto-tdline TO wl_texto.
      APPEND wl_texto TO tl_texto.
      CLEAR: wl_texto.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title        = 'Observação'
      im_display_mode = 'X'
    CHANGING
      ch_text         = tl_texto.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMA_TEXTO_ROT
*&---------------------------------------------------------------------*
FORM chama_texto_rot USING wl_name TYPE thead-tdname.

  DATA: it_texto TYPE STANDARD TABLE OF tline,
        wa_texto TYPE tline,
        tl_texto TYPE catsxt_longtext_itab,
        wl_texto TYPE LINE OF catsxt_longtext_itab.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      id        = 'ZROT'
      language  = sy-langu
      name      = wl_name
      object    = 'ZSDROTEIRO'
    TABLES
      lines     = it_texto
    EXCEPTIONS
      id        = 1
      language  = 2
      name      = 3
      not_found = 4
      OTHERS    = 5.

  IF sy-subrc IS INITIAL.
    LOOP AT it_texto INTO wa_texto.
      MOVE: wa_texto-tdline TO wl_texto.
      APPEND wl_texto TO tl_texto.
      CLEAR: wl_texto.
    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'CATSXT_SIMPLE_TEXT_EDITOR'
    EXPORTING
      im_title        = 'Roteiro'
      im_display_mode = 'X'
    CHANGING
      ch_text         = tl_texto.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHAMA_ANEXO
*&---------------------------------------------------------------------*
FORM chama_anexo USING wa_bor TYPE borident.

  DATA: anexo_obj  TYPE REF TO cl_gos_manager.

  CREATE OBJECT anexo_obj TYPE cl_gos_manager.

  anexo_obj->set_rw_mode( ip_mode = 'R' ).

  anexo_obj->start_service_direct(
    EXPORTING
      ip_service       = 'VIEW_ATTA'
      is_object        = wa_bor
    EXCEPTIONS
      no_object        = 1
      object_invalid   = 2
      execution_failed = 3
      OTHERS           = 4 ).

  COMMIT WORK.

  CLEAR: wa_bor.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  COMPLETA_MOTORISTA
*&---------------------------------------------------------------------*
FORM completa_motorista CHANGING wa_header_lote TYPE ty_header_lote.

  DATA: wa_lfa1 TYPE lfa1.

  SELECT SINGLE *
    FROM lfa1
    INTO  wa_lfa1
    WHERE lifnr EQ wa_header_lote-motorista.

  wa_header_lote-telf1 = wa_lfa1-telf1.
  wa_header_lote-stcd2 = wa_lfa1-stcd2.
  wa_header_lote-mot_desc = wa_lfa1-name1.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F4_LOCAL_EMBARQUE
*&---------------------------------------------------------------------*
FORM f4_local_embarque USING: er_event_data TYPE REF TO cl_alv_event_data
                              es_row_no TYPE lvc_s_roid.

  DATA: it_ret TYPE STANDARD TABLE OF ddshretval,
        it_f4  TYPE STANDARD TABLE OF ty_f4_loc_emb.

  DATA: it_zsdt0132_f4 TYPE STANDARD TABLE OF zsdt0132,
        wa_zsdt0132_f4 TYPE zsdt0132.

  DATA: wa_f4   TYPE ty_f4_loc_emb,
        wa_ret  TYPE ddshretval,
        wa_modi TYPE lvc_s_modi.

  FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

  DATA : it_fmap TYPE STANDARD TABLE OF dselc,
         wa_fmap TYPE dselc.

  it_fmap =
  VALUE #(
           ( fldname = 'F0001' dyfldname = 'LIFNR' )
           ( fldname = 'F0002' dyfldname = 'COD_LOC_EMB' )
           ( fldname = 'F0003' dyfldname = 'LOCAL_EMBARQ' )
           ( fldname = 'F0004' dyfldname = 'ARMAZEM' )
           ( fldname = 'F0005' dyfldname = 'TRANSPORTADORA' )
           ( fldname = 'F0006' dyfldname = 'TRANSP_RESP' )
         ).

  SELECT *
    FROM zsdt0132
    INTO TABLE it_zsdt0132_f4
    WHERE marca  EQ wa_header_lote-wrkst
      AND status NE 'I'.

  it_f4 = VALUE #( FOR ls IN it_zsdt0132_f4
  (
      lifnr          = ls-lifnr
      cod_loc_emb    = ls-nr_rot
      local_embarq   = ls-rot_desc
      armazem        = ls-armazem
      transportadora = ls-transportadora
      transp_resp    = ls-transp_resp
   ) ).

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LIFNR'
      window_title    = 'Lista de Locais'(002)
      value_org       = 'S'
      dynprofield     = 'LIFNR'
    TABLES
      value_tab       = it_f4
      return_tab      = it_ret
      dynpfld_mapping = it_fmap
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.

  IF sy-subrc = 0.
    ASSIGN er_event_data->m_data->* TO <itab>.
    READ TABLE it_ret INTO wa_ret INDEX 1.
    wa_modi-row_id   = es_row_no-row_id.
    wa_modi-fieldname = 'LIFNR'.
    wa_modi-value     = wa_ret-fieldval.
    APPEND wa_modi TO <itab>.
    READ TABLE it_ret INTO wa_ret INDEX 2.
    wa_modi-fieldname = 'COD_LOC_EMB'.
    wa_modi-value     = wa_ret-fieldval.
    APPEND wa_modi TO <itab>.
    READ TABLE it_ret INTO wa_ret INDEX 3.
    wa_modi-fieldname = 'LOCAL_EMBARQ'.
    wa_modi-value     = wa_ret-fieldval.
    APPEND wa_modi TO <itab>.
    READ TABLE it_ret INTO wa_ret INDEX 4.
    wa_modi-fieldname = 'ARMAZEM'.
    wa_modi-value     = wa_ret-fieldval.
    APPEND wa_modi TO <itab>.
    READ TABLE it_ret INTO wa_ret INDEX 5.
    wa_modi-fieldname = 'TRANSPORTADORA'.
    wa_modi-value     = wa_ret-fieldval.
    APPEND wa_modi TO <itab>.
    READ TABLE it_ret INTO wa_ret INDEX 6.
    wa_modi-fieldname = 'TRANSP_RESP'.
    wa_modi-value     = wa_ret-fieldval.
    APPEND wa_modi TO <itab>.
  ENDIF.

  er_event_data->m_event_handled = 'X'. "(to inform grid that f4 was handled manually)

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_PLACA
*&---------------------------------------------------------------------*
FORM valida_placa_sementes USING VALUE(p_placa)
                                 VALUE(p_tpveiculo)
                           CHANGING vl_check TYPE char1.

  DATA: wa_zlest0002 TYPE zlest0002,
        vl_renavam   TYPE zlest0002-cd_renavam.

  SELECT SINGLE *
        FROM zlest0002
        INTO wa_zlest0002
        WHERE pc_veiculo EQ p_placa
          AND tp_veiculo EQ p_tpveiculo.

  IF wa_zlest0002 IS NOT INITIAL.
    IF wa_zlest0002-qt_eixo = 0.
      MESSAGE TEXT-080 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
    ELSE.
      vl_renavam = strlen( wa_zlest0002-cd_renavam ).
      IF vl_renavam NOT BETWEEN 9 AND 11.
        MESSAGE TEXT-081 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE TEXT-082 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  VALIDA_PLACA
*&---------------------------------------------------------------------*
FORM valida_placa_sementes_2 USING VALUE(p_tpveiculo)
                             CHANGING wa_carga-placa TYPE zlest0002-pc_veiculo
                                      vl_check TYPE char1.

  DATA: wa_zlest0002 TYPE zlest0002,
        vl_renavam   TYPE zlest0002-cd_renavam.

  CLEAR: vl_check.

  SELECT SINGLE *
        FROM zlest0002
        INTO wa_zlest0002
        WHERE pc_veiculo EQ wa_carga-placa
          AND tp_veiculo EQ p_tpveiculo.

  IF wa_zlest0002 IS NOT INITIAL.
    IF wa_zlest0002-qt_eixo = 0.
      MESSAGE TEXT-080 TYPE 'S' DISPLAY LIKE 'E'.
      vl_check = abap_true.
      CLEAR: wa_carga-placa.
    ELSE.
      vl_renavam = strlen( wa_zlest0002-cd_renavam ).
      IF vl_renavam NOT BETWEEN 9 AND 11.
        MESSAGE TEXT-081 TYPE 'S' DISPLAY LIKE 'E'.
        vl_check = abap_true.
        CLEAR: wa_carga-placa.
      ENDIF.
    ENDIF.
  ELSE.
    MESSAGE TEXT-082 TYPE 'S' DISPLAY LIKE 'E'.
    vl_check = abap_true.
    CLEAR: wa_carga-placa.
  ENDIF.

ENDFORM.

*----------------------------------------------------------------------*
***INCLUDE LZGFS_DEFENSIVOSF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SELECAO_ROMANEIOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_selecao_romaneios USING p_nro_cg.

  DATA: it_rom_5820 TYPE STANDARD TABLE OF ty_ordem_5820,
        it_zsdt0134 TYPE STANDARD TABLE OF zsdt0134.

  DATA: wa_zsdt0001 TYPE ty_romaneios_5821.

  FREE: it_rom_5820, it_zsdt0001_5821, it_zsdt0134_5821, it_zsdt0001_5821,
        t_zsdt0302,  t_zsdt0298.

  SELECT *
    FROM zsdt0140
   INNER JOIN zsdt0082 ON zsdt0140~nro_sol = zsdt0082~nro_sol
                      AND zsdt0140~seq     = zsdt0082~seq
   INTO CORRESPONDING FIELDS OF TABLE it_rom_5820
  WHERE zsdt0140~nro_cgd EQ p_nro_cg
    AND zsdt0140~status  NE 'X'.

  CHECK it_rom_5820[] IS NOT INITIAL.

  SORT it_rom_5820 BY vbeln seq.
  DELETE ADJACENT DUPLICATES FROM it_rom_5820 COMPARING vbeln seq.

  SELECT *
    FROM zsdt0134
    INTO TABLE it_zsdt0134_5821
     FOR ALL ENTRIES IN it_rom_5820
   WHERE vbeln  EQ it_rom_5820-vbeln
     AND nro_cg EQ p_nro_cg
     AND status NE 'X'.

  SELECT *
    FROM zsdt0001
    INTO CORRESPONDING FIELDS OF TABLE it_zsdt0001_5821
     FOR ALL ENTRIES IN it_rom_5820
   WHERE nro_cg EQ p_nro_cg
     AND vbeln  EQ it_rom_5820-vbeln.

  IF it_zsdt0001_5821[] IS NOT INITIAL.
    SELECT *
      FROM zsdt0302
      INTO TABLE t_zsdt0302
       FOR ALL ENTRIES IN it_zsdt0001_5821
     WHERE nro_cgd       EQ p_nro_cg
       AND ch_referencia EQ it_zsdt0001_5821-ch_referencia.
  ENDIF.

  LOOP AT it_zsdt0001_5821 INTO wa_zsdt0001.
    l_tabix = sy-tabix.

    CLEAR w_zsdt0302.
    READ TABLE t_zsdt0302 INTO w_zsdt0302 WITH KEY nro_cgd       = wa_zsdt0001-nro_cg
                                                   ch_referencia = wa_zsdt0001-ch_referencia.

    IF sy-subrc = 0. " AND w_zsdt0302-qtd_solicitacao_ra <> 0.
      wa_zsdt0001-qtd_solicitacao_ra = w_zsdt0302-qtd_solicitacao_ra.
      wa_zsdt0001-log_solrec         = '@96@'.
    ELSE.
      CLEAR: wa_zsdt0001-qtd_solicitacao_ra,
             wa_zsdt0001-log_solrec.
    ENDIF.

    IF wa_zsdt0001-st_proc EQ '99'.
      wa_zsdt0001-icone = '@01@'.
    ENDIF.

    MODIFY it_zsdt0001_5821 FROM wa_zsdt0001 INDEX l_tabix.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&  listar receituario
*&---------------------------------------------------------------------*
FORM f_lista_receitas USING p_nro_cg
                            p_ch_referencia.

  SELECT *
    FROM zsdt0298
    INTO TABLE t_zsdt0298
   WHERE nro_cgd       EQ p_nro_cg
     AND ch_referencia EQ p_ch_referencia
     AND cancelado     EQ abap_off.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  MOSTRA_POP_5821
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM mostra_pop_5821 .

  FREE: it_fieldcatalog_pop_5821,
        t_fieldcatalog2.

  IF g_custom_container_pop_5821 IS INITIAL.

    CREATE OBJECT g_custom_container_pop_5821
      EXPORTING
        container_name              = 'CONTAINER5821'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    PERFORM fill_it_fieldcatalog_5821 TABLES it_fieldcatalog_pop_5821 USING:
          01 'NRO_CG'              ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Carga',
          02 'CH_REFERENCIA'       ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ref.',
          03 'NR_ROMANEIO'         ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Nr. Romaneio',
          04 'VBELN'               ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Ordem Venda',
          05 'ICONE'               ''         ' '  ' ' 'X'  ' '   'X'   ' '   ' '   ' '   'Finalizado',
          06 'QTD_SOLICITACAO_RA'  ''         ' '  ' ' ' '  ' '   'X'   ' '   ' '   ' '   'Sol.Receitas Calc.',
          07 'LOG_SOLREC'          ''         ' '  ' ' 'X'  'X'   'X'   ' '   ' '   ' '   'Log.Sol.Receita'.

    gs_layout_pop_5821-sel_mode   = 'A'.
    gs_layout_pop_5821-cwidth_opt = 'X'.
    gs_layout_pop_5821-info_fname = 'COLOR'.

    CREATE OBJECT ctl_alv1_pop_5821
      EXPORTING
        i_parent = g_custom_container_pop_5821.           "ALV Lote

    SET HANDLER:
      lcl_event_handler_5821=>toolbar_5821      FOR ctl_alv1_pop_5821,
      lcl_event_handler_5821=>user_command_5821 FOR ctl_alv1_pop_5821,
      lcl_event_handler_5821=>on_double_click   FOR ctl_alv1_pop_5821,
      lcl_event_handler_5821=>on_hotspot        FOR ctl_alv1_pop_5821.

    CALL METHOD ctl_alv1_pop_5821->set_table_for_first_display
      EXPORTING
        is_layout       = gs_layout_pop_5821
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = it_fieldcatalog_pop_5821
        it_outtab       = it_zsdt0001_5821.

  ELSE.
    CALL METHOD ctl_alv1_pop_5821->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF g_custom_container2 IS INITIAL.
    CREATE OBJECT g_custom_container2 EXPORTING container_name = g_container2.
    CREATE OBJECT g_grid2 EXPORTING i_parent = g_custom_container2.

    PERFORM fill_it_fieldcatalog2 USING:
            01 'CH_REFERENCIA' 'ZSDT0298'     ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Ref.'                 '20',
            01 'ID'            'ZSDT0298'     ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Id.Sol.Receita'       '15',
            01 'STATUS'        'ZSDT0298'     ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Status'               '06',
            01 'DATA_ATUAL'    'ZSDT0298'     ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Data Criação'         '10',
            01 'HORA_ATUAL'    'ZSDT0298'     ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Hora Criação'         '10',
            01 'USNAM'         'ZSDT0298'     ' '  ' '  ' '  ' '  ' '   ' '   ' '   ' '   'Usuário'              '12'.

    w_layout-sel_mode   = 'A'.
*   w_layout-cwidth_opt = 'X'.

    PERFORM toolbar_alv2.

    IF m_event_handler2 IS INITIAL.
      CREATE OBJECT m_event_handler2.
*     SET HANDLER : m_event_handler2->toolbar FOR g_grid2.
      SET HANDLER : m_event_handler2->user_command FOR g_grid2.
    ENDIF.

    CALL METHOD g_grid2->set_table_for_first_display
      EXPORTING
        is_layout            = w_layout
        it_toolbar_excluding = pt_exclude2
        i_save               = 'U' "abap_true
      CHANGING
*       it_sort              = lt_sort2
        it_fieldcatalog      = t_fieldcatalog2
        it_outtab            = t_zsdt0298.

    CALL METHOD g_grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid2->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER : lcl_event_handler2=>on_data_changed4 FOR g_grid2.

  ELSE.
    CALL METHOD g_grid2->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FILL_IT_FIELDCATALOG
*&---------------------------------------------------------------------*
FORM fill_it_fieldcatalog_5821  TABLES p_it_fieldacatalog STRUCTURE lvc_s_fcat
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

  IF wa_fieldcatalog-fieldname(3) = 'QTD'.
    wa_fieldcatalog-outputlen = 12.
  ENDIF.

*-CS2019001896 - 12.08.2021 - JT - inicio
  IF wa_fieldcatalog-fieldname = 'EDITAR'.
    wa_fieldcatalog-outputlen = 06.
    wa_fieldcatalog-dd_outlen = 06.
    wa_fieldcatalog-just      = abap_true.
  ENDIF.
*-CS2019001896 - 12.08.2021 - JT - fim

*  IF P_FIELDNAME EQ 'LIFNR' AND P_ATIVE IS INITIAL.
*    WA_FIELDCATALOG-NO_OUT = ABAP_TRUE.
*  ENDIF.

  APPEND wa_fieldcatalog TO p_it_fieldacatalog.

ENDFORM.

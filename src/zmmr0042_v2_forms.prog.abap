*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados.

  CLEAR: git_zppt0002[],  git_zppt0002_tp_01[],git_zppt0002_tp_02[], git_zppt0002_tp_03[].

  SELECT a~id_referencia,
         a~id_sessao,
         a~lgort,
         a~werks,
         a~status_registro,
         a~status_processamento,
         a~status_ret_sistema_origem,
         a~motivo_reprocessamento,
         a~erro_ret_sistema_origem,
         a~qtd_tentativa_ret_sis_orig,
         a~cd_mensagem,
         b~name1
    FROM zppt0002 AS a
    INNER JOIN t001w AS b ON b~werks = a~werks
    INTO CORRESPONDING FIELDS OF TABLE @git_zppt0002
   WHERE a~werks IN @s_werks
     AND a~laeda IN @s_data.

  SELECT a~id_referencia,
         a~id_sessao,
         a~lgort,
         a~werks,
         a~status_registro,
         a~status_processamento,
         a~status_ret_sistema_origem,
         a~cd_mensagem,
         b~name1
    FROM zppt0002_wait AS a
    INNER JOIN t001w AS b ON b~werks = a~werks
    APPENDING CORRESPONDING FIELDS OF TABLE @git_zppt0002
   WHERE a~werks                  IN @s_werks
     AND a~laeda                  IN @s_data
     AND a~liberado_processamento EQ @abap_false.

  DELETE git_zppt0002 WHERE status_processamento IS INITIAL.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados.

  DATA: wl_color  TYPE kkblo_specialcol.

  FREE: t_alv.

  LOOP AT git_zppt0002 ASSIGNING FIELD-SYMBOL(<fs_zppt0002>).

    CASE <fs_zppt0002>-status_registro.
      WHEN '01' OR '02' OR 'E2'.
        <fs_zppt0002>-status_group = '01'.
        <fs_zppt0002>-ds_status    = 'Produção'.
      WHEN '03' OR '04' OR 'E4'.
        <fs_zppt0002>-status_group = '03'.
        <fs_zppt0002>-ds_status    = 'Classificação'.
      WHEN '07' OR '08' OR 'E8'.
        <fs_zppt0002>-status_group = '07'.
        <fs_zppt0002>-ds_status    = 'Transferência'.
      WHEN '99' OR '06' OR 'E6'.
        <fs_zppt0002>-status_group = '99'.
        <fs_zppt0002>-ds_status    = 'Estorno'.
      WHEN OTHERS.
        <fs_zppt0002>-status_group = <fs_zppt0002>-status_registro.
        <fs_zppt0002>-ds_status    = 'Outros'.
    ENDCASE.

  ENDLOOP.

  DATA(lit_zppt0002_grp) = git_zppt0002[].

  SORT lit_zppt0002_grp BY werks status_group.
  DELETE ADJACENT DUPLICATES FROM lit_zppt0002_grp COMPARING werks status_group.


  LOOP AT lit_zppt0002_grp INTO DATA(lwa_group).

    CLEAR:  w_alv, wl_color.

    w_alv-status_registro = lwa_group-status_group.
    w_alv-status          = lwa_group-ds_status.
    w_alv-werks           = lwa_group-werks.
    w_alv-name1           = lwa_group-name1.

    DATA(_erro_step_proc_pendente)      = abap_false.
    DATA(_alert_step_proc_pendente)     = abap_false.

    DATA(_erro_step_proc_andamento)     = abap_false.
    DATA(_alert_step_proc_andamento)    = abap_false.

    DATA(_erro_step_ret_sis_orig_pend)  = abap_false.
    DATA(_alert_step_ret_sis_orig_pend) = abap_false.

    LOOP AT git_zppt0002 INTO DATA(lwa_zppt0002) WHERE  werks        = lwa_group-werks
                                                    AND status_group = lwa_group-status_group.

      CASE  lwa_zppt0002-status_processamento.
        WHEN 'P'. "Processamento Pendente

          ADD 1 TO w_alv-qtd_penden.
          ADD 1 TO w_alv-total.

          IF lwa_zppt0002-motivo_reprocessamento = '01'. "Erro Estorno Fardinho
            _erro_step_proc_pendente = abap_true.
          ELSE.

            IF lwa_zppt0002-cd_mensagem IS INITIAL.
              lwa_zppt0002-cd_mensagem = 'Processamento Pendente...'.
            ENDIF.

            IF lwa_zppt0002-motivo_reprocessamento = '02'. "Erro Lock Usuario
              _alert_step_proc_pendente = abap_true.
            ENDIF.

          ENDIF.

          APPEND lwa_zppt0002 TO git_zppt0002_tp_01.

        WHEN 'A'. "Processamento em Andamento

          ADD 1 TO w_alv-qtd_emproc.
          ADD 1 TO w_alv-total.

          IF lwa_zppt0002-motivo_reprocessamento = '01'. "Erro Estorno Fardinho
            _erro_step_proc_andamento = abap_true.
          ELSE.
            IF lwa_zppt0002-cd_mensagem IS INITIAL.
              lwa_zppt0002-cd_mensagem = 'Processamento em Andamento...'.
            ENDIF.

            IF lwa_zppt0002-motivo_reprocessamento = '02'. "Erro Lock Usuario
              _alert_step_proc_andamento = abap_true.
            ENDIF.
          ENDIF.

          APPEND lwa_zppt0002 TO git_zppt0002_tp_02.

        WHEN 'C'. "Processamento Concluido

          CASE lwa_zppt0002-status_ret_sistema_origem.
            WHEN 'P'. "Retorno Pendente.
              ADD 1 TO w_alv-qtd_ret_pend.
              ADD 1 TO w_alv-total.

              CASE lwa_zppt0002-erro_ret_sistema_origem .
                WHEN abap_true.
                  IF lwa_zppt0002-qtd_tentativa_ret_sis_orig > 15.
                    _erro_step_ret_sis_orig_pend  = abap_true.
                    lwa_zppt0002-cd_mensagem = |Verificar Log Transação ZWS0004 - Id Interface: 034 e Id. Referencia: { lwa_zppt0002-id_referencia } |.
                  ELSE.
                    _alert_step_ret_sis_orig_pend = abap_true.
                    lwa_zppt0002-cd_mensagem = |Tentativas de entrega para Trace Cotton sendo realizadas...! Numero Tentativas já realizadas: { lwa_zppt0002-qtd_tentativa_ret_sis_orig } |.
                  ENDIF.
                WHEN abap_false.
                  lwa_zppt0002-cd_mensagem = 'Pendente Retorno para Trace Cotton...'.
              ENDCASE.

              APPEND lwa_zppt0002 TO git_zppt0002_tp_03.

            WHEN 'C'. "Retorno Concluido.
              ADD 1 TO w_alv-qtd_ret_proc.
              ADD 1 TO w_alv-total.
          ENDCASE.

      ENDCASE.

    ENDLOOP.

    "Step Processamento Pendentes
    IF _erro_step_proc_pendente IS NOT INITIAL.
      CLEAR: wl_color.
      wl_color-fieldname = 'QTD_PENDEN'.
      wl_color-color-col = 6.
      wl_color-color-inv = 6.
      APPEND wl_color TO w_alv-color.
    ELSEIF _alert_step_proc_pendente IS NOT INITIAL.
      CLEAR: wl_color.
      wl_color-fieldname = 'QTD_PENDEN'.
      wl_color-color-col = 3.
      wl_color-color-int = 1.
      wl_color-color-inv = 1.
      APPEND wl_color TO w_alv-color.
    ENDIF.

    "Step Processamento Em andamento
    IF _erro_step_proc_andamento IS NOT INITIAL.
      CLEAR: wl_color.
      wl_color-fieldname = 'QTD_EMPROC'.
      wl_color-color-col = 6.
      wl_color-color-inv = 6.
      APPEND wl_color TO w_alv-color.
    ELSEIF _ALERT_step_proc_andamento IS NOT INITIAL.
      CLEAR: wl_color.
      wl_color-fieldname = 'QTD_EMPROC'.
      wl_color-color-col = 3.
      wl_color-color-int = 1.
      wl_color-color-inv = 1.
      APPEND wl_color TO w_alv-color.
    ENDIF.

    "Step Pendentes Retorno Sistema Origem
    IF _erro_step_ret_sis_orig_pend = abap_true.

      CLEAR: wl_color.
      wl_color-fieldname = 'QTD_RET_PEND'.
      wl_color-color-col = 6.
      wl_color-color-inv = 0.
      APPEND wl_color TO w_alv-color.

    ELSEIF _alert_step_ret_sis_orig_pend = abap_true.

      CLEAR: wl_color.
      wl_color-fieldname = 'QTD_RET_PEND'.
      wl_color-color-col = 3.
      wl_color-color-int = 1.
      wl_color-color-inv = 1.
      APPEND wl_color TO w_alv-color.

    ENDIF.

    APPEND w_alv TO t_alv.

  ENDLOOP.

ENDFORM.


**********************************************************************
* alv saida
**********************************************************************
FORM f_alv_saida.

  CALL SCREEN 100.


ENDFORM.

**********************************************************************
* INICIA ALV
**********************************************************************
FORM f_init_alv.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog.
  PERFORM f_sort.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
  w_layout-info_fname   = 'LINE_COLOR'.
  w_layout-ctab_fname   = 'COLOR'.
  w_layout-zebra        = abap_false.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
  w_layout-stylefname   = 'CELLSTYLES'.

  IF cl_container_95 IS INITIAL.
    CREATE OBJECT cl_container_95
      EXPORTING
        side  = '4'
        ratio = '80'.
  ENDIF.

  IF g_grid IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CREATE OBJECT obj_dyndoc_id
      EXPORTING
        no_margins = 'X'.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'HEADER'.

    PERFORM f_alv_header .

    CALL METHOD obj_dyndoc_id->merge_document.

    CALL METHOD obj_dyndoc_id->display_document
      EXPORTING
        reuse_control      = 'X'
        parent             = g_custom_container
      EXCEPTIONS
        html_display_error = 1.

    "Grafico 1
    CALL METHOD cl_gui_cfw=>flush.

    CREATE OBJECT: g_custom_container
       EXPORTING
         container_name = 'CC_IMG',
         picture
       EXPORTING
         parent = g_custom_container.

    PERFORM f_pega_imagem USING 'LOGO_NOVO' CHANGING url.

    CALL METHOD picture->load_picture_from_url
      EXPORTING
        url = url.

    CALL METHOD picture->set_display_mode
      EXPORTING
        display_mode = picture->display_mode_fit_center.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent = cl_container_95.
*    ENDIF.

    SET HANDLER lcl_event_handler=>on_hotspot_click  FOR g_grid.
    SET HANDLER lcl_event_handler=>on_data_changed  FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_alv[]
        it_sort                       = t_sort[]
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

  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid->set_selected_rows
      EXPORTING
        it_index_rows = t_rows.
  ENDIF.

ENDFORM.

**********************************************************************
*  cabecalho
**********************************************************************
FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        wl_text TYPE sdydo_text_element.

  DATA: wa_t001       TYPE t001,
        wa_j_1bbranch TYPE j_1bbranch.

  wl_linha = 'Painel de Processamento Beneficiamento Algodão'.

  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>extra_large
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.

  IF s_werks[] IS NOT INITIAL.
    CONCATENATE  'Centro....................:' s_werks-low
            INTO wl_linha SEPARATED BY space.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

* CALL METHOD obj_dyndoc_id->new_line.

  IF s_data[] IS NOT INITIAL.
    READ TABLE s_data INDEX 1.

    wl_data1 = s_data-low+6(2)  && '.' && s_data-low+4(2) && '.'  && s_data-low(4).
    wl_data2 = s_data-high+6(2) && '.' && s_data-high+4(2) && '.' && s_data-high(4).

    IF s_data-high IS NOT INITIAL.
      CONCATENATE  'Data Processamento:' wl_data1 'a' wl_data2
             INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE  'Data Processamento:' wl_data1
             INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

ENDFORM.                    " ZF_ALV_HEADER

**********************************************************************
*  imagen
**********************************************************************
FORM f_pega_imagem  USING    nome_logo
                    CHANGING url.

  REFRESH graphic_table.
  CALL METHOD cl_ssf_xsf_utilities=>get_bds_graphic_as_bmp
    EXPORTING
      p_object = 'GRAPHICS'
      p_name   = nome_logo
      p_id     = 'BMAP'
      p_btype  = 'BCOL'
    RECEIVING
      p_bmp    = l_graphic_xstr.

  graphic_size = xstrlen( l_graphic_xstr ).
  l_graphic_conv = graphic_size.
  l_graphic_offs = 0.
  WHILE l_graphic_conv > 255.
    graphic_table-line = l_graphic_xstr+l_graphic_offs(255).
    APPEND graphic_table.
    l_graphic_offs = l_graphic_offs + 255.
    l_graphic_conv = l_graphic_conv - 255.
  ENDWHILE.
  graphic_table-line = l_graphic_xstr+l_graphic_offs(l_graphic_conv).
  APPEND graphic_table.
  CALL FUNCTION 'DP_CREATE_URL'
    EXPORTING
      type     = 'IMAGE'
      subtype  = 'X-UNKNOWN'
      size     = graphic_size
      lifetime = 'T'
    TABLES
      data     = graphic_table
    CHANGING
      url      = url.
ENDFORM.                    " F_PEGA_IMAGEM

**********************************************************************
*  barra tarefas
**********************************************************************
FORM f_funcoes.

  FREE: t_function.

  w_function = cl_gui_alv_grid=>mc_fc_loc_delete_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_insert_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_move_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_undo.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_append_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_copy_row.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_loc_cut.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_check.
  APPEND w_function TO t_function.
  w_function = cl_gui_alv_grid=>mc_fc_refresh.
  APPEND w_function TO t_function.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_sort.

  FREE: t_sort.

  w_sort-fieldname = 'WERKS'.
  w_sort-subtot    = 'X'.
* w_SORT-SPOS      = 1.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''      ''       'T_ALV' 'WERKS'         'Centro'                   '07'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    02  ''      ''       'T_ALV' 'NAME1'         'Nome'                     '30'  ' '    ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    03  ''      ''       'T_ALV' 'STATUS'        'Fase Processamento'       '25'  ' '    'X' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    04  ''      ''       'T_ALV' 'QTD_PENDEN'    'Aguardando Process.'      '20'  ' '    'X' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
    05  ''      ''       'T_ALV' 'QTD_EMPROC'    'Em Processamento'         '20'  ' '    'X' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
    07  ''      ''       'T_ALV' 'QTD_RET_PEND'  'Pendente Retorno Trace'   '22'  ' '    'X' 'C' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
    08  ''      ''       'T_ALV' 'QTD_RET_PROC'  'Retornado Trace Cotton'   '22'  ' '    'X' 'C' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''      ''       'T_ALV' 'TOTAL'         'Total'                    '20'  ' '    'X' 'C' ' '  ' ' ' ' ' ' ' ' ' ' ' '.


ENDFORM.

**********************************************************************
* estrutura alv
**********************************************************************
FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i                    "1
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
                           VALUE(p_fix).                                    "16

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
*  w_fieldcat-colddictxt  = 'M'.
*  w_fieldcat-selddictxt  = 'M'.
*  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV


FORM f_montar_estrutura USING  VALUE(p_col_pos)       TYPE i
                               VALUE(p_ref_tabname)   LIKE dd02d-tabname
                               VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                               VALUE(p_tabname)       LIKE dd02d-tabname
                               VALUE(p_field)         LIKE dd03d-fieldname
                               VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                               VALUE(p_outputlen)
                               VALUE(p_hotspot).

  CLEAR gwa_estrutura.

  gwa_estrutura-fieldname     = p_field.
  gwa_estrutura-tabname       = p_tabname.
  gwa_estrutura-ref_tabname   = p_ref_tabname.
  gwa_estrutura-ref_fieldname = p_ref_fieldname.
  gwa_estrutura-key           = ' '.
  gwa_estrutura-key_sel       = 'X'.
  gwa_estrutura-col_pos       = p_col_pos.
  gwa_estrutura-no_out        = ' '.
  gwa_estrutura-seltext_s     = p_scrtext_l.
  gwa_estrutura-seltext_m     = p_scrtext_l.
  gwa_estrutura-seltext_l     = p_scrtext_l.
  gwa_estrutura-hotspot       = p_hotspot.

  IF p_scrtext_l IS NOT INITIAL.
    gwa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  TRANSLATE  gwa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  gwa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  gwa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  gwa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND gwa_estrutura TO git_estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM f_show_registros_det USING p_tipo
                                p_saida TYPE ty_alv.

  DATA: lwa_layout  TYPE slis_layout_alv.

  CLEAR: git_zppt0002_det[].
  CASE p_tipo.
    WHEN '01'.
      git_zppt0002_det[] = git_zppt0002_tp_01.
    WHEN '02'.
      git_zppt0002_det[] = git_zppt0002_tp_02.
    WHEN '03'.
      git_zppt0002_det[] = git_zppt0002_tp_03.



    WHEN OTHERS.
      EXIT.
  ENDCASE.

  DELETE git_zppt0002_det WHERE NOT ( werks EQ p_saida-werks AND status_group = p_saida-status_registro ).

  CHECK git_zppt0002_det IS NOT INITIAL.

  CLEAR: git_estrutura[].

  lwa_layout-colwidth_optimize = abap_true.

  PERFORM f_montar_estrutura USING:

     01   ''  ''              'GIT_ZPPT0002_DET' 'ID_REFERENCIA'                   'Protocolo'          '25' '',
     02   ''  ''              'GIT_ZPPT0002_DET' 'ID_SESSAO'                       'Sessao'             '36' '',
     03   ''  ''              'GIT_ZPPT0002_DET' 'WERKS'                           'Filial'             '06' '',
     04   ''  ''              'GIT_ZPPT0002_DET' 'LGORT'                           'Bloco'              '05' '',
     05   ''  ''              'GIT_ZPPT0002_DET' 'CD_MENSAGEM'                     'Mensagem'           '30' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_grid_title = 'Detalhamento'
      is_layout    = lwa_layout
      it_fieldcat  = git_estrutura
    TABLES
      t_outtab     = git_zppt0002_det.

ENDFORM.

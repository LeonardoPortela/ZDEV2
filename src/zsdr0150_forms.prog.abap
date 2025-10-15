*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MAD |09/06/2025 |Inclusão do campo de Check-List&*
*&                                    |de Documentação Jurídica.      &*
*&                                    |Chamado: 174343.               &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações p/ geração Contrato &*
*&                                    |Compra.                        &*
*&                                    |Chamado: 168919 2ª Parte.      &*
*&--------------------------------------------------------------------&*
**********************************************************************
* alv saida
**********************************************************************
FORM f_alv_saida.

  IF g_popup = abap_false.
    CASE abap_true.
      WHEN p_insumo.
        PERFORM f_selecao_dados USING 0.
        CASE abap_true.
          WHEN p_sintet.
            PERFORM f_processa_dados_sintetico.
          WHEN p_analit.
            PERFORM f_processa_dados_analitico.
        ENDCASE.
    ENDCASE.

    IF t_sinte[] IS INITIAL AND
       t_anali[] IS INITIAL.
      MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    CALL SCREEN 300.
  ELSE.
    PERFORM f_selecao_dados            USING g_doc_simulacao.
    PERFORM f_processa_dados_analitico.

*   CHECK t_anali[] IS NOT INITIAL.

    CALL SCREEN 301 STARTING AT 20   2
                      ENDING AT 170  21.
  ENDIF.

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
* w_layout-no_rowmark   = abap_true.
  w_layout-zebra        = abap_true.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
* w_layout-ctab_fname   = 'CELLCOLOR'.
  w_layout-info_fname   = 'LINE_COLOR'.

  CASE abap_true.
    WHEN p_sintet.
      w_layout-no_rowmark = abap_true.
    WHEN p_analit.
      w_layout-no_rowmark = abap_false.
  ENDCASE.

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

    CASE abap_true.
      WHEN p_sintet.

        SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid,
                     lcl_event_handler=>on_data_changed  FOR g_grid,
                     lcl_event_handler=>user_command     FOR g_grid,
                     lcl_event_handler=>toolbar          FOR g_grid.

        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_sinte[]
            it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

      WHEN p_analit.
        SET HANDLER: lcl_event_handler=>on_hotspot_click_analit FOR g_grid,
                     lcl_event_handler=>on_data_changed         FOR g_grid,
                     lcl_event_handler=>user_command            FOR g_grid,
                     lcl_event_handler=>toolbar                 FOR g_grid.

        CALL METHOD g_grid->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_anali[]
            it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
    ENDCASE.

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
* INICIA ALV
**********************************************************************
FORM f_init_alv_pop.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog.
  PERFORM f_sort.

  w_stable-row          = abap_true.
  w_stable-col          = abap_true.
*
* w_layout-no_rowmark   = abap_true.
  w_layout-zebra        = abap_true.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
  w_layout-ctab_fname   = 'CELLCOLOR'.
  w_layout-info_fname   = 'LINE_COLOR'.

  CASE abap_true.
    WHEN p_sintet.
      w_layout-no_rowmark = abap_true.
    WHEN p_analit.
      w_layout-no_rowmark = abap_false.
  ENDCASE.

  IF g_grid_pop IS INITIAL. " AND  g_custom_container IS NOT INITIAL.
    CREATE OBJECT g_custom_container_pop
      EXPORTING
        container_name              = 'CONTAINER_POP'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    CREATE OBJECT g_grid_pop
      EXPORTING
        i_parent          = g_custom_container_pop
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CASE abap_true.
      WHEN p_sintet.

        SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid_pop,
                     lcl_event_handler=>on_data_changed  FOR g_grid_pop,
                     lcl_event_handler=>user_command     FOR g_grid_pop,
                     lcl_event_handler=>toolbar          FOR g_grid_pop.

        CALL METHOD g_grid_pop->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_sinte[]
            it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.

      WHEN p_analit.
        SET HANDLER: lcl_event_handler=>on_hotspot_click_analit FOR g_grid_pop,
                     lcl_event_handler=>on_data_changed         FOR g_grid_pop,
                     lcl_event_handler=>user_command            FOR g_grid_pop,
                     lcl_event_handler=>toolbar                 FOR g_grid_pop.

        CALL METHOD g_grid_pop->set_table_for_first_display
          EXPORTING
            is_layout                     = w_layout
            i_save                        = 'A'
*           it_toolbar_excluding          = t_function
          CHANGING
            it_outtab                     = t_anali[]
            it_sort                       = t_sort[]
            it_fieldcatalog               = t_fieldcat
          EXCEPTIONS
            invalid_parameter_combination = 1
            program_error                 = 2
            too_many_lines                = 3
            OTHERS                        = 4.
    ENDCASE.

    CALL METHOD g_grid_pop->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid_pop->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  ELSE.
    CALL METHOD g_grid_pop->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

  IF lines( t_rows ) > 0.
    CALL METHOD g_grid_pop->set_selected_rows
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

*  wl_linha = 'Cockpit de Documentos'.
*  wl_text = wl_linha.
*
*  CALL METHOD obj_dyndoc_id->initialize_document.
*
*  CALL METHOD obj_dyndoc_id->add_text
*    EXPORTING
*      text         = wl_text
*      sap_style    = cl_dd_area=>heading
*      sap_fontsize = cl_dd_area=>large
*      sap_color    = cl_dd_area=>list_heading_int.
**<<<------"168919 - NMS - INI------>>>
*  wl_linha = COND #( WHEN p_insumo = abap_true THEN 'Area : Insumos'
  IF NOT p_insumo IS INITIAL      AND
         p_tpinsm EQ sy-abcde+2(1). "C - Compra
    wl_linha = 'Area : Insumos/Compra'.

  ELSE.
    wl_linha = 'Area : Insumos/Venda'.

  ENDIF.

  wl_linha = COND #( WHEN p_insumo = abap_true THEN wl_linha
**<<<------"168919 - NMS - FIM------>>>
                                               ELSE 'Area : Mercado Interno' ).
  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->initialize_document.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>large
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.

  wl_linha = COND #( WHEN p_sintet = abap_true THEN 'Visão: Sintética'
                                               ELSE 'Visão: Analítica' ).
  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>list_normal
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.

  wl_linha = COND #( WHEN p_pend   = abap_true THEN 'Status: Pendente'
                     WHEN p_conclu = abap_true THEN 'Status: Concluído'
                     WHEN p_todos  = abap_true THEN 'Status: Todos' ).
  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>list_normal
      sap_color    = cl_dd_area=>list_heading_int.

ENDFORM.

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

  w_sort-fieldname = 'DOC_SIMULACAO'.
* w_sort-subtot    = 'X'.
  w_sort-spos      = 1.
  w_sort-up        = 'X'.
  APPEND w_sort   TO t_sort.
*
  IF p_analit = abap_true.
*   w_sort-fieldname = 'TIPO_DOC'.
**  w_sort-subtot    = 'X'.
*   w_sort-spos      = 2.
*   w_sort-up        = 'X'.
*   APPEND w_sort   TO t_sort.

*   w_sort-fieldname = 'ID_DOCUMENTO'.
**  w_sort-subtot    = 'X'.
*   w_sort-spos      = 3.
*   w_sort-up        = 'X'.
*   APPEND w_sort   TO t_sort.

    w_sort-fieldname = 'ORDEM_SAIDA'.
*   w_sort-subtot    = 'X'.
    w_sort-spos      = 2.
    w_sort-up        = 'X'.
    APPEND w_sort   TO t_sort.
  ENDIF.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

**<<<------"168919 - NMS - INI------>>>
  DATA: x TYPE c,
        vl_lbl_doc TYPE scrtext_l,
        vl_lbl_par TYPE scrtext_l,
        vl_lbl_saf TYPE scrtext_l,
        vl_namfeld TYPE name_feld.

  IF NOT p_insumo IS INITIAL      AND
         p_tpinsm EQ sy-abcde+2(1). "C - Compra
    x          = abap_on.
    vl_lbl_doc = 'Nro.sol.Ordem'.
    vl_lbl_par = 'Fornecedor'.
    vl_namfeld = 'SAFR2'.
    vl_lbl_saf = 'Safra'.

  ELSE.
    x          = abap_off.
    vl_lbl_doc = 'Doc.Simulação'.
    vl_lbl_par = 'Cliente'.
    vl_namfeld = 'SAFRA'.
    vl_lbl_saf = 'Safra Pagamento'.

  ENDIF.
**<<<------"168919 - NMS - FIM------>>>
  FREE t_fieldcat[].

  CASE abap_true.

    WHEN p_sintet.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_SINTE' 'VKBUR'               'Escr.Vendas'              '11'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        02  ''      ''       'T_SINTE' 'BEZEI'               'Descr.Escr.Vendas'        '17'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''      ''       'T_SINTE' 'NAME1'               'Cliente'                  '30'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        04  ''      ''       'T_SINTE' 'REGIONAL'            'Regional'                 '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        05  ''      ''       'T_SINTE' 'DOC_SIMULACAO'       'Doc.Simulação'            '13'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' 'X',
        06  ''      ''       'T_SINTE' 'CONTRATO'            'Contrato'                 '08'  ' ' ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        07  ''      ''       'T_SINTE' 'PED_VENDA'           'Pedido Venda'             '12'  ' ' ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        08  ''      ''       'T_SINTE' 'DISTRATO'            'Distrato'                 '08'  ' ' ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        09  ''      ''       'T_SINTE' 'ADITIVOS'            'Aditivos'                 '08'  ' ' ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
        10  ''      ''       'T_SINTE' 'DECLAR_REC'          'Declaração Recbmto'       '18'  ' ' ' ' 'C' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
**<<<------"174343 - NMS - INI------>>>
        10  ''      ''       'T_SINTE' 'CHKLSTJD'            'Check-List Jurídico'      '27'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
**<<<------"174343 - NMS - FIM------>>>
        11  ''      ''       'T_SINTE' 'TIPO_VENDA'          'Tipo da Venda'            '13'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        12  ''      ''       'T_SINTE' 'WAERK'               'Moeda'                    '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        13  ''      ''       'T_SINTE' 'ERDAT'               'Data Venda'               '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        14  ''      ''       'T_SINTE' 'SAFRA'               'Safra Pagamento'          '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        15  ''      ''       'T_SINTE' 'CULTURA'             'Cultura Pagamento'        '17'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN p_analit.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_ANALI' 'DOC_SIMULACAO'       vl_lbl_doc                 '13'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' 'X', "<<<------"168919 - NMS ------>>>
        02  ''      ''       'T_ANALI' 'TIPO_DOC'            'Tipo Documento'           '18'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  'VBAK'  'VBELN'  'T_ANALI' 'VBELN'               'OV'                       '11'  ' ' ' ' ' ' 'X'  ' ' ' ' ' '  x  ' ' 'X', "<<<------"168919 - NMS ------>>>
        04  ''      ''       'T_ANALI' 'ORDEM_SAIDA'         'Orde Saida'               '18'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' 'X' ' ' 'X',
        05  ''      ''       'T_ANALI' 'ID_DOCUMENTO'        'ID Documento'             '14'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        06  ''      ''       'T_ANALI' 'DOCUMENTO'           'Documento'                '10'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        07  ''      ''       'T_ANALI' 'ASSINATURA'          'Assinatura'               '10'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' 'X',
        08  ''      ''       'T_ANALI' 'TIPO_ASSINA'         'Tipo Assinatura'          '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        09  ''      ''       'T_ANALI' 'STATUS'              'Status do Documento'      '40'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        10  ''      ''       'T_ANALI' 'TIPO_VENDA'          'Tipo da Venda'            '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' '  x  ' ' ' ', "<<<------"168919 - NMS ------>>>
        11  ''      ''       'T_ANALI' 'VKBUR'               'Esc.Vendas'               '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' '  x  ' ' ' ', "<<<------"168919 - NMS ------>>>
        12  ''      ''       'T_ANALI' 'BEZEI'               'Desc.Esc.Vendas'          '20'  ' ' ' ' ' ' ' '  ' ' ' ' ' '  x  ' ' ' ', "<<<------"168919 - NMS ------>>>
        13  ''      ''       'T_ANALI' 'REGIONAL'            'Regional'                 '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' '  x  ' ' ' ', "<<<------"168919 - NMS ------>>>
        14  ''      ''       'T_ANALI' 'NAME1'               vl_lbl_par                 '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "<<<------"168919 - NMS ------>>>
        15  ''      ''       'T_ANALI' vl_namfeld            vl_lbl_saf                 '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ', "<<<------"168919 - NMS ------>>>
        16  ''      ''       'T_ANALI' 'CULTURA'             'Cultura Pagamento'        '17'  ' ' ' ' ' ' ' '  ' ' ' ' ' '  x  ' ' ' ', "<<<------"168919 - NMS ------>>>
        17  ''      ''       'T_ANALI' 'CATEG_ADIT'          'Categoria Aditivo'        '17'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        18  ''      ''       'T_ANALI' 'SEQ_ADIT'            'Seq. Aditivo'             '17'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' ',
*       19  'VBAK'  'VBELN'  'T_ANALI' 'VBELN'               'OV'                       '11'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' ' ' ' ',
        20  'VBAP'  'POSNR'  'T_ANALI' 'POSNR'               'Item OV'                  '08'  ' ' ' ' ' ' 'X'  ' ' ' ' ' '  x  ' ' ' ', "<<<------"168919 - NMS ------>>>
        21  ''      ''       'T_ANALI' 'NR_DOC_GERADO'       'Nro.SIGAM'                '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        22  ''      ''       'T_ANALI' 'DATA'                'Data Registro'            '13'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        23  ''      ''       'T_ANALI' 'HORA'                'Hora Registro'            '13'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        24  ''      ''       'T_ANALI' 'USNAME'              'Usuário Registro'         '15'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        25  ''      ''       'T_ANALI' 'LOG'                 'Log Processo'             '12'  ' ' ' ' ' ' 'X'  ' ' ' ' ' ' ' ' 'X' ' '.

  ENDCASE.

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
                           VALUE(p_fix).

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
* w_fieldcat-reptext     = p_scrtext_l.
  w_fieldcat-coltext     = p_scrtext_l.
* w_fieldcat-scrtext_s   = p_scrtext_l.
* w_fieldcat-scrtext_m   = p_scrtext_l.
* w_fieldcat-scrtext_l   = p_scrtext_l.
  w_fieldcat-style       = p_style.
  w_fieldcat-just        = p_just.
  w_fieldcat-hotspot     = p_hotspot.
  w_fieldcat-f4availabl  = p_f4.
  w_fieldcat-checkbox    = p_checkbox.
  w_fieldcat-icon        = p_icon.
  w_fieldcat-colddictxt  = 'M'.
  w_fieldcat-selddictxt  = 'M'.
  w_fieldcat-tipddictxt  = 'M'.
  w_fieldcat-fix_column  = p_fix.
  w_fieldcat-no_out      = p_no_out.
* w_fieldcat-col_opt     = 'X'.

  APPEND w_fieldcat TO t_fieldcat.

ENDFORM.                    " ESTRUTURA_ALV

**********************************************************************
* salvar variante
**********************************************************************
FORM f_save_variant.

  DATA: ft_params    TYPE TABLE OF rsparams,
        ft_sscr      TYPE TABLE OF rsscr,
        fs_sscr      TYPE rsscr,
        fr_selopt    TYPE RANGE OF rsscr-name,
        fs_selopt    LIKE LINE OF fr_selopt,
        f_vari_desc  TYPE varid,
        ft_vari_text TYPE TABLE OF varit,
        fs_vari_text TYPE varit,
        ft_vscreens  TYPE TABLE OF rsdynnr,
        fs_vscreens  TYPE rsdynnr,
        f_retcode    TYPE c,
        l_dynnr      TYPE sy-dynnr,
        l_varname    TYPE rsvar-variant,
        l_vartext    TYPE varit-vtext.

  CALL SCREEN 0200 STARTING AT 10 02
                     ENDING AT 57 04.

  CHECK rsvar-variant IS NOT INITIAL.
  l_dynnr   = '0100'.
  l_varname = rsvar-variant.
  l_vartext = rsvar-vtext.

*get all selection screen parameters and values
  CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
    EXPORTING
      curr_report     = sy-repid
* IMPORTING
*     SP              = SP
    TABLES
      selection_table = ft_params
    EXCEPTIONS
      not_found       = 1
      no_report       = 2
      OTHERS          = 3.
  IF sy-subrc EQ 0.
  ENDIF.

*get fields of our subscreen
  CALL FUNCTION 'RS_ISOLATE_1_SELSCREEN'
    EXPORTING
      program     = sy-repid
      dynnr       = '0101' "l_dynnr
*     TABIX       = 0
*   IMPORTING
*     LAST_TABIX  = LAST_TABIX
    TABLES
      screen_sscr = ft_sscr
*     GLOBAL_SSCR = GLOBAL_SSCR
    EXCEPTIONS
      no_objects  = 1
      OTHERS      = 2.
  IF sy-subrc EQ 0.
    LOOP AT ft_sscr INTO fs_sscr.
      fs_selopt-sign = 'I'.
      fs_selopt-option = 'EQ'.
      fs_selopt-low = fs_sscr-name.
      APPEND fs_selopt TO fr_selopt.
    ENDLOOP.
*delete parameters and values not used on our subscreen
    DELETE ft_params WHERE selname NOT IN fr_selopt.
  ENDIF.

*set variant global data
  MOVE sy-mandt             TO f_vari_desc-mandt.
  MOVE sy-repid             TO f_vari_desc-report.
  MOVE l_varname            TO f_vari_desc-variant.
  MOVE sy-uname             TO f_vari_desc-ename.
  MOVE sy-datum             TO f_vari_desc-edat .
  MOVE sy-uzeit             TO f_vari_desc-etime.
  MOVE 'X'                  TO f_vari_desc-environmnt.

*set description of variant
  MOVE sy-mandt             TO fs_vari_text-mandt.
  MOVE sy-langu             TO fs_vari_text-langu.
  MOVE sy-repid             TO fs_vari_text-report .
  MOVE l_varname            TO fs_vari_text-variant.
  MOVE l_vartext            TO fs_vari_text-vtext.
  APPEND  fs_vari_text TO ft_vari_text.

*set subscreen number
  fs_vscreens-dynnr = l_dynnr.
  fs_vscreens-kind = ''.
  APPEND fs_vscreens TO ft_vscreens.
*create variant
  CALL FUNCTION 'RS_CREATE_VARIANT'
    EXPORTING
      curr_report               = sy-repid
      curr_variant              = l_varname
      vari_desc                 = f_vari_desc
    TABLES
      vari_contents             = ft_params
      vari_text                 = ft_vari_text
      vscreens                  = ft_vscreens
    EXCEPTIONS
      illegal_report_or_variant = 1
      illegal_variantname       = 2
      not_authorized            = 3
      not_executed              = 4
      report_not_existent       = 5
      report_not_supplied       = 6
      variant_exists            = 7
      variant_locked            = 8
      OTHERS                    = 9.
  IF sy-subrc EQ 7.
*variant already exists so ask if user wants to overwrite it
    CALL FUNCTION 'POPUP_TO_CONFIRM_WITH_MESSAGE'
      EXPORTING
        diagnosetext1  = 'Variant exists, do you want to overwrite it?'
        textline1      = ' '
        titel          = 'Variant exists, do you want to overwrite it?'
        cancel_display = space
      IMPORTING
        answer         = f_retcode
      EXCEPTIONS
        OTHERS         = 1.
    IF sy-subrc <> 0.
      EXIT.
    ENDIF.
    IF f_retcode EQ 'J'.
*user agreed so change existing variant
      CALL FUNCTION 'RS_CHANGE_CREATED_VARIANT'
        EXPORTING
          curr_report               = sy-repid
          curr_variant              = l_varname
          vari_desc                 = f_vari_desc
*         ONLY_CONTENTS             = ONLY_CONTENTS
        TABLES
          vari_contents             = ft_params
          vari_text                 = ft_vari_text
*         VARI_SEL_DESC             = VARI_SEL_DESC
*         OBJECTS                   = OBJECTS
        EXCEPTIONS
          illegal_report_or_variant = 1
          illegal_variantname       = 2
          not_authorized            = 3
          not_executed              = 4
          report_not_existent       = 5
          report_not_supplied       = 6
          variant_doesnt_exist      = 7
          variant_locked            = 8
          selections_no_match       = 9
          OTHERS                    = 10.
      IF sy-subrc <> 0.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* ler variante
**********************************************************************
FORM f_get_variant.

  DATA: ft_params    TYPE TABLE OF rsparams,
        f_variant    TYPE rsvar-variant,
        f_text       TYPE rsvar-vtext,
        fs_params    TYPE rsparams,
        fs_paramsscr TYPE rsparams,
        f_fieldname  TYPE fieldname,
        ft_sscr      TYPE TABLE OF rsscr,
        l_dynnr      TYPE sy-dynnr.

  FIELD-SYMBOLS <any_selopt_itab> TYPE STANDARD TABLE.
  FIELD-SYMBOLS <any_selopt> TYPE any.
  FIELD-SYMBOLS <any_field> TYPE any.

  l_dynnr = '0100'.

  "now I will display pop-up with variants for one subscreen
  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report               = sy-repid
*     NEW_TITLE            = ' '
      dynnr                = l_dynnr
*     INTERNAL_CALL        = ' '
*     MASKED               = 'X'
*     VARIANT              = ' '
      pop_up               = 'X'
    IMPORTING
      sel_variant          = f_variant
      sel_variant_text     = f_text
*   TABLES
*     BELONGING_DYNNR      = BELONGING_DYNNR
    EXCEPTIONS
      no_report            = 1
      report_not_existent  = 2
      report_not_supplied  = 3
      no_variants          = 4
      no_variant_selected  = 5
      variant_not_existent = 6
      OTHERS               = 7.

  IF sy-subrc EQ 0 .
    rsvar-variant = f_variant.
    rsvar-vtext   = f_text.

    "if variant was supplied then I read its content
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = sy-repid
        variant              = f_variant
        move_or_write        = 'M'
*       NO_IMPORT            = ' '
*       EXECUTE_DIRECT       = ' '
*   IMPORTING
*       SP                   = SP
      TABLES
*       L_PARAMS             = L_PARAMS
*       L_PARAMS_NONV        = L_PARAMS_NONV
*       L_SELOP              = L_SELOP
*       L_SELOP_NONV         = L_SELOP_NONV
        valutab              = ft_params
*       OBJECTS              = OBJECTS
*       FREE_SELECTIONS_DESC = FREE_SELECTIONS_DESC
*       FREE_SELECTIONS_VALUE       = FREE_SELECTIONS_VALUE
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.
    IF sy-subrc EQ 0.
      "let's see what fields are on this subscreen
      CALL FUNCTION 'RS_ISOLATE_1_SELSCREEN'
        EXPORTING
          program     = sy-repid
          dynnr       = '0101'  "l_dynnr
*         TABIX       = 0
*   IMPORTING
*         LAST_TABIX  = LAST_TABIX
        TABLES
          screen_sscr = ft_sscr
*         GLOBAL_SSCR = GLOBAL_SSCR
        EXCEPTIONS
          no_objects  = 1
          OTHERS      = 2.
      IF sy-subrc EQ 0.
        SORT ft_sscr BY name ASCENDING.

        " clear current content of selection fields
        LOOP AT ft_params INTO fs_params.
          READ TABLE ft_sscr WITH KEY name = fs_params-selname BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CONCATENATE '(' sy-repid ')' fs_params-selname INTO f_fieldname.
          CASE fs_params-kind.
            WHEN 'S'.
              CONCATENATE f_fieldname '[]' INTO f_fieldname.
              ASSIGN (f_fieldname) TO <any_selopt_itab>.
              IF sy-subrc EQ 0.
                REFRESH <any_selopt_itab>.
              ENDIF.
            WHEN 'P'.
              ASSIGN (f_fieldname) TO <any_field>.
              IF sy-subrc EQ 0.
                CLEAR <any_field>.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
        ENDLOOP.

        "add values from saved variant to selection fields
        LOOP AT ft_params INTO fs_params.
          READ TABLE ft_sscr WITH KEY name = fs_params-selname BINARY SEARCH TRANSPORTING NO FIELDS.
          IF sy-subrc NE 0.
            CONTINUE.
          ENDIF.
          CONCATENATE '(' sy-repid ')' fs_params-selname INTO f_fieldname.
          CASE fs_params-kind.
            WHEN 'S'. "select-options
              CONCATENATE f_fieldname '[]' INTO f_fieldname.
              ASSIGN (f_fieldname) TO <any_selopt_itab>.
              IF sy-subrc EQ 0.
                "firstly append initial line to be able to assign components
                APPEND INITIAL LINE TO <any_selopt_itab> ASSIGNING <any_selopt>.
                "now fill each component separately
                ASSIGN COMPONENT 'SIGN' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-sign.
                ENDIF.

                ASSIGN COMPONENT 'OPTION' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-option.
                ENDIF.

                ASSIGN COMPONENT 'LOW' OF STRUCTURE <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-low.
                ENDIF.

                ASSIGN COMPONENT 'HIGH' OF STRUCTURE <any_selopt> TO <any_field>.
                IF sy-subrc EQ 0.
                  <any_field> = fs_params-high.
                ENDIF.

                "just to be sure that select options are filled
                ASSIGN COMPONENT 'SIGN' OF STRUCTURE  <any_selopt> TO <any_field>.
                IF sy-subrc NE 0.
                  DELETE TABLE <any_selopt_itab> FROM <any_selopt>.
                ELSEIF <any_field> IS INITIAL.
                  DELETE TABLE <any_selopt_itab> FROM <any_selopt>.
                ENDIF.

              ENDIF.
            WHEN 'P'. "parameters
              ASSIGN (f_fieldname) TO <any_field>.
              IF sy-subrc EQ 0.
                CLEAR <any_field>.
                <any_field> = fs_params-low.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
*  Form  Z_PREENCHER_DYNPRO
**********************************************************************
FORM f_preencher_dynpro    USING l_start TYPE c
                                 l_name  TYPE c
                                 l_value.

  MOVE l_start TO w_bdc-dynbegin.

  IF l_start = 'X'.
    MOVE: l_name  TO w_bdc-program,
          l_value TO w_bdc-dynpro.
  ELSE.
    MOVE: l_name  TO w_bdc-fnam,
          l_value TO w_bdc-fval.
  ENDIF.

  APPEND w_bdc TO t_bdc.
  CLEAR: w_bdc.

ENDFORM.

**********************************************************************
**********************************************************************

************************************************************************
*&                        AMAGGI                                      &*
************************************************************************
*& Projeto..: AMaggi                                                  &*
*& Autor....: Jaime Tassoni                                           &*
*& Data.....: 07.07.2025                                              &*
*& Descrição: Reenvio de Materiais / Contatos para Api SAFRA          &*
************************************************************************

************************************************************************
* includes
************************************************************************
INCLUDE zsdr0230_top.
INCLUDE zsdr0230_classe.
INCLUDE zsdr0230_status_0100o01.

************************************************************************
* SCREEN OUTPUT
************************************************************************
AT SELECTION-SCREEN OUTPUT.

************************************************************************
* start
************************************************************************
START-OF-SELECTION.

*-----------------------
* processamento
*-----------------------
  PERFORM f_selecao_dados.

  IF t_product[] IS INITIAL.
    MESSAGE s024(sd) WITH TEXT-002 DISPLAY LIKE 'E'.
    STOP.
  ENDIF.

  PERFORM f_processa_dados.

  CALL SCREEN 0100.

************************************************************************
* selecao dados
************************************************************************
FORM f_selecao_dados.

  lv_data = sy-datum - 10.

*-------------------------------------------------
*-selecao dados
*-------------------------------------------------
  SELECT mara~matnr, makt~maktx, mara~laeda,
         mara~meins, mara~brgew
    FROM mara
   INNER JOIN makt ON makt~matnr = mara~matnr
                  AND makt~spras = @sy-langu
    INTO TABLE @t_product
   WHERE mara~matnr IN @s_matnr
     AND mara~laeda IN @s_laeda.

  CHECK t_product[] IS NOT INITIAL.

  LOOP AT t_product  INTO w_product.
    w_product-matnr2    = |{ w_product-matnr ALPHA = OUT } |.
    MODIFY t_product FROM w_product INDEX sy-tabix.
  ENDLOOP.

  SELECT id_integracao id_interface  dt_registro
         tp_referencia id_referencia ck_integrado ds_metodo ds_data_retorno
    INTO CORRESPONDING FIELDS OF TABLE t_integra
    FROM zintegracao
     FOR ALL ENTRIES IN t_product
   WHERE id_interface  = '291'
     AND dt_registro  >= lv_data
     AND tp_referencia = 'OB_SAFRA_PRODUCT'
     AND id_referencia = t_product-matnr2
     AND ds_metodo    IN ('POST','PUT').

  SORT t_integra BY id_referencia
                    dt_registro DESCENDING
                    hr_registro DESCENDING.

  DELETE ADJACENT DUPLICATES FROM t_integra COMPARING id_referencia.

ENDFORM.

************************************************************************
* processa dados
************************************************************************
FORM f_processa_dados.

  FREE: t_saida_prod.

  LOOP AT t_product             INTO w_product.
    MOVE-CORRESPONDING w_product  TO w_saida_prod.

    READ TABLE t_integra        INTO w_integra WITH KEY id_referencia = w_saida_prod-matnr2.

    IF sy-subrc <> 0.
      w_saida_prod-status   = icon_dummy.
    ELSE.
      IF w_integra-ck_integrado = abap_true.
        w_saida_prod-status = icon_led_green.
      ELSE.
        w_saida_prod-status = icon_led_red.
      ENDIF.
    ENDIF.

    /ui2/cl_json=>deserialize( EXPORTING json = w_integra-ds_data_retorno CHANGING data = w_error ).

    w_saida_prod-mensagem   = w_error-message.

    APPEND w_saida_prod           TO t_saida_prod.
  ENDLOOP.

  SORT t_saida_prod BY matnr.

ENDFORM.

************************************************************************
* REENCIAR
************************************************************************
FORM f_reenviar_produto.

  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.

  IF t_rows[] IS INITIAL.
    MESSAGE s024(sd) WITH 'Selecionar pelo menos uma linha!' DISPLAY LIKE 'W'.
    RETURN.
  ENDIF.

  LOOP AT t_rows INTO w_rows.
    READ TABLE t_saida_prod INTO w_saida_prod INDEX w_rows-index.
    w_saida_prod-status        = icon_dummy.
    MODIFY t_saida_prod     FROM w_saida_prod INDEX sy-tabix.

    DATA(l_task) = 'INTEGRAR_MATERIAL' && w_saida_prod-matnr.

    CALL FUNCTION 'ZSD_INT_OB_INTEGRA_MATERIAL' STARTING NEW TASK l_task
      EXPORTING
        i_matnr = w_saida_prod-matnr.
  ENDLOOP.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.

ENDFORM.

************************************************************************
* ALV
************************************************************************
FORM f_init_alv.

* PERFORM f_funcoes.
  PERFORM f_fieldcatalog USING 'MATERIAL'.
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
  w_layout-no_rowmark   = abap_false.

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

    SET HANDLER: lcl_event_handler=>on_hotspot_click   FOR g_grid,
                 lcl_event_handler=>on_data_changed    FOR g_grid,
                 lcl_event_handler=>user_command       FOR g_grid,
                 lcl_event_handler=>toolbar            FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida_prod[]
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

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_sort.

  FREE: t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog USING p_tipo.

  FREE t_fieldcat[].

  CASE p_tipo.

    WHEN 'MATERIAL'.
      PERFORM f_estrutura_alv USING:
        01  ''      ''       'T_SAIDA_PROD' 'STATUS'            'Status'                 '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' 'X',
        02  'MARA'  'MATNR'  'T_SAIDA_PROD' 'MATNR'             'Cod.Produto'            '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
        03  ''      ''       'T_SAIDA_PROD' 'MAKTX'             'Descrição'              '40'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        04  ''      ''       'T_SAIDA_PROD' 'LAEDA'             'Dt.Modif.'              '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        05  ''      ''       'T_SAIDA_PROD' 'MEINS'             'Unid.Medida'            '03'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        06  ''      ''       'T_SAIDA_PROD' 'BRGEW'             'Peso Bruto'             '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
        07  ''      ''       'T_SAIDA_PROD' 'MENSAGEM'          'Mensagem'               '300' ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

    WHEN 'CONTATO'.

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

FORM f_alv_header .

  DATA: wl_data1(10),
        wl_data2(10),
        wl_hora(8),
        wl_linha(60),
        lv_matnr_low  TYPE mara-matnr,
        lv_matnr_high TYPE mara-matnr,
        wl_text       TYPE sdydo_text_element.

  CALL METHOD obj_dyndoc_id->initialize_document.

  wl_linha = 'Reenvio Materiais e Contatos para Api SAFRA'.
  wl_text = wl_linha.

  CALL METHOD obj_dyndoc_id->add_text
    EXPORTING
      text         = wl_text
      sap_style    = cl_dd_area=>heading
      sap_fontsize = cl_dd_area=>large
      sap_color    = cl_dd_area=>list_heading_int.

  CALL METHOD obj_dyndoc_id->new_line.


  IF s_matnr[] IS NOT INITIAL.
    READ TABLE s_matnr INDEX 1.

    lv_matnr_low  = |{ s_matnr-low  ALPHA = OUT } |.
    lv_matnr_high = |{ s_matnr-high ALPHA = OUT } |.

    IF s_matnr-high IS NOT INITIAL.
      CONCATENATE  'Material...............:' lv_matnr_low 'a' lv_matnr_high
             INTO wl_linha SEPARATED BY space.
    ELSE.
      CONCATENATE  'Material...............:' lv_matnr_low
             INTO wl_linha SEPARATED BY space.
    ENDIF.

    wl_text = wl_linha.
    CALL METHOD obj_dyndoc_id->new_line.

    CALL METHOD obj_dyndoc_id->add_text
      EXPORTING
        text         = wl_text
        sap_fontsize = cl_dd_area=>list_normal.
  ENDIF.

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

************************************************************************
************************************************************************

*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_FORMS
*&---------------------------------------------------------------------*

**********************************************************************
* selecao de dados
**********************************************************************
FORM f_selecao_dados.

  FREE: t_campos_nfe, t_chave_cte, t_bsak, t_bkpf.

  CREATE OBJECT zcl_util.

  IF s_cte[] IS NOT INITIAL.
    SELECT *
      INTO TABLE t_zib_cte
      FROM zib_cte_dist_ter
     WHERE cd_chave_cte IN s_cte
       AND dt_emissao   IN s_dtemi.

    CHECK sy-subrc = 0.

    LOOP AT t_zib_cte   INTO w_zib_cte.
      w_campos_nfe         = zcl_util->get_atributos_nfe( w_zib_cte-cd_chave_cte ).
      APPEND w_campos_nfe TO t_campos_nfe.
    ENDLOOP.

    SELECT *
      INTO TABLE t_active
      FROM j_1bnfe_active
       FOR ALL ENTRIES IN t_campos_nfe
     WHERE regio   = t_campos_nfe-regio
       AND nfyear  = t_campos_nfe-nfyear
       AND nfmonth = t_campos_nfe-nfmonth
       AND stcd1   = t_campos_nfe-stcd1
       AND model   = t_campos_nfe-model
       AND serie   = t_campos_nfe-serie
       AND nfnum9  = t_campos_nfe-nfnum9
       AND docnum9 = t_campos_nfe-docnum9
       AND cdv     = t_campos_nfe-cdv.

    CHECK sy-subrc = 0.

    SELECT zlest0019~*
      INTO TABLE @t_zlest0019
      FROM zlest0019
     INNER JOIN lfa1 ON lfa1~stcd1 = zlest0019~cnpjferro
     INNER JOIN mara ON mara~matnr = zlest0019~matnr
      FOR ALL ENTRIES IN @t_active
     WHERE zlest0019~docnum   = @t_active-docnum
       AND lfa1~lifnr        IN @s_lifnr
       AND lfa1~ort01        IN @s_ort01
       AND mara~matnr        IN @s_matnr
       AND zlest0019~bukrs   IN @s_bukrs
       AND zlest0019~branch  IN @s_branch
       AND zlest0019~idvagao IN @s_idvaga.
  ELSE.
    SELECT zlest0019~*
      INTO TABLE @t_zlest0019
      FROM zlest0019
     INNER JOIN lfa1 ON lfa1~stcd1 = zlest0019~cnpjferro
     INNER JOIN mara ON mara~matnr = zlest0019~matnr
     WHERE lfa1~lifnr        IN @s_lifnr
       AND lfa1~ort01        IN @s_ort01
       AND mara~matnr        IN @s_matnr
       AND zlest0019~bukrs   IN @s_bukrs
       AND zlest0019~branch  IN @s_branch
       AND zlest0019~idvagao IN @s_idvaga.

    IF t_zlest0019[] IS NOT INITIAL.
      SELECT *
        INTO TABLE t_active
        FROM j_1bnfe_active
         FOR ALL ENTRIES IN t_zlest0019
       WHERE docnum  = t_zlest0019-docnum.

      LOOP AT t_active    INTO w_active.
        w_chave_cte-chave    = zcl_util->get_chave_nfe( w_active-docnum ).
        APPEND w_chave_cte  TO t_chave_cte.
      ENDLOOP.

      SELECT *
        INTO TABLE t_zib_cte
        FROM zib_cte_dist_ter
         FOR ALL ENTRIES IN t_chave_cte
       WHERE cd_chave_cte  = t_chave_cte-chave
         AND dt_emissao   IN s_dtemi.
    ENDIF.
  ENDIF.

  CHECK t_zlest0019[] IS NOT INITIAL AND
        t_zib_cte[]   IS NOT INITIAL.

  SELECT *
    FROM makt
    INTO TABLE t_makt
     FOR ALL ENTRIES IN t_zlest0019
   WHERE matnr = t_zlest0019-matnr
     AND spras = sy-langu.

  SELECT *
    FROM lfa1
    INTO TABLE t_lfa1
     FOR ALL ENTRIES IN t_zlest0019
   WHERE stcd1 = t_zlest0019-cnpjferro.

  SELECT *
    FROM j_1bnfdoc
    INTO TABLE t_jdoc
     FOR ALL ENTRIES IN t_zlest0019
   WHERE docnum = t_zlest0019-docnum.

  IF t_zib_cte[] IS NOT INITIAL.
    LOOP AT t_zib_cte  INTO w_zib_cte.
      w_zib_cte-awkey     = w_zib_cte-belnr && w_zib_cte-gjahr.
      MODIFY t_zib_cte FROM w_zib_cte INDEX sy-tabix.
    ENDLOOP.

    SELECT *
      FROM bkpf
      INTO TABLE t_bkpf
       FOR ALL ENTRIES IN t_zib_cte
     WHERE awkey = t_zib_cte-awkey.

    IF t_bkpf[] IS NOT INITIAL.
      SELECT *
        FROM bsak
        INTO TABLE t_bsak
         FOR ALL ENTRIES IN t_bkpf
       WHERE bukrs  = t_bkpf-bukrs
         AND belnr  = t_bkpf-belnr
         AND gjahr  = t_bkpf-gjahr
         AND budat IN s_dtcomp.
    .ENDIF.
  ENDIF.

ENDFORM.

**********************************************************************
* processa dados
**********************************************************************
FORM f_processa_dados.

  FREE: t_alv.

  LOOP AT t_zlest0019  INTO w_zlest0019.

    CLEAR: w_alv, w_lfa1, w_makt, w_jdoc, w_active, w_zib_cte, w_bkpf, w_bsak.

    w_chave_cte-chave = zcl_util->get_chave_nfe( w_zlest0019-docnum ).

    READ TABLE t_lfa1    INTO w_lfa1     WITH KEY stcd1        = w_zlest0019-cnpjferro.
    READ TABLE t_makt    INTO w_makt     WITH KEY matnr        = w_zlest0019-matnr.
    READ TABLE t_jdoc    INTO w_jdoc     WITH KEY docnum       = w_zlest0019-docnum.
    READ TABLE t_zib_cte INTO w_zib_cte  WITH KEY cd_chave_cte = w_chave_cte-chave.
    CHECK sy-subrc = 0.

    READ TABLE t_bkpf    INTO w_bkpf     WITH KEY awkey        = w_zib_cte-awkey.
    READ TABLE t_bsak    INTO w_bsak     WITH KEY bukrs        = w_bkpf-bukrs
                                                  belnr        = w_bkpf-belnr
                                                  gjahr        = w_bkpf-gjahr.
    CHECK sy-subrc = 0.

    SHIFT w_zlest0019-dcl LEFT DELETING LEADING '0'.

    w_alv-filial          = w_zlest0019-branch.
    w_alv-transbordo      = w_lfa1-ort01.
    w_alv-vagao           = w_zlest0019-idvagao.
    w_alv-cidade_origem   = w_jdoc-ort01.
    w_alv-data_saida      = w_zlest0019-dtadecarga.
    w_alv-peso_saida      = w_zlest0019-pesodvagao.
    w_alv-dcl             = w_zlest0019-dcl.
    w_alv-produto         = w_makt-maktx.
    w_alv-valor_pago      = w_bsak-dmbtr.
    w_alv-data_pagto      = w_bsak-budat.

    APPEND w_alv         TO t_alv.
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
  w_layout-zebra        = abap_false.
  w_layout-sel_mode     = 'A'.
* w_layout-edit         = abap_true. " Makes all Grid editable
  w_layout-no_totarr    = abap_false.
  w_layout-no_totexp    = abap_false.
  w_layout-no_totline   = abap_false.
  w_layout-no_toolbar   = abap_false.
* w_layout-stylefname   = 'CELLSTYLES'.
  w_layout-ctab_fname   = 'CELLCOLOR'.

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


*  wl_linha = 'Retroativo Ferroviário'.
*  wl_text = wl_linha.
*
*  CALL METHOD obj_dyndoc_id->add_text
*    EXPORTING
*      text         = wl_text
*      sap_style    = cl_dd_area=>heading
*      sap_fontsize = cl_dd_area=>extra_large
*      sap_color    = cl_dd_area=>list_heading_int.
*
*  CALL METHOD obj_dyndoc_id->new_line.
*  CALL METHOD obj_dyndoc_id->new_line.
*  CALL METHOD obj_dyndoc_id->new_line.
*
*  wl_text = wl_linha.
*
*  CALL METHOD obj_dyndoc_id->add_text
*    EXPORTING
*      text = wl_text.
*
*  CALL METHOD obj_dyndoc_id->new_line.

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

*  w_sort-fieldname = 'ETAPA_HEAD'.
** w_sort-subtot    = 'X'.
*  w_sort-spos      = 1.
*  w_sort-up        = 'X'.
*  APPEND w_sort   TO t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''          ''       'T_ALV' 'FILIAL'              'Filial'                   '07'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    02  ''          ''       'T_ALV' 'TRANSBORDO'          'Transbordo'               '30'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    03  ''          ''       'T_ALV' 'VAGAO'               'Vagão'                    '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    04  ''          ''       'T_ALV' 'CIDADE_ORIGEM'       'Cidade Origem NF'         '30'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    05  ''          ''       'T_ALV' 'DATA_SAIDA'          'Data Saída'               '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    06  ''          ''       'T_ALV' 'PESO_SAIDA'          'Peso Saída'               '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    07  'ZLEST0019' 'DCL'    'T_ALV' 'DCL'                 'DCL'                      '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    08  ''          ''       'T_ALV' 'PRODUTO'             'Produto'                  '35'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''          ''       'T_ALV' 'VALOR_PAGO'          'Valor Pago'               '25'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''          ''       'T_ALV' 'DATA_PAGTO'          'Data Pagamento'           '16'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

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
**********************************************************************

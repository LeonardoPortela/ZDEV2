*----------------------------------------------------------------------*
***INCLUDE LZGFS_DISTRIB_INSUMOSO02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.

  DATA: t_fcode2      TYPE slis_t_extab.

  FREE: t_fcode2, ok_code.

  SET PF-STATUS 'PF_0200'           EXCLUDING t_fcode2.
  SET TITLEBAR 'PT_0200'.

  PERFORM f_init_alv2.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0200 INPUT.

  CASE ok_code.

    WHEN 'VOLTAR'.
      CALL METHOD g_custom_container->free.
      FREE: g_grid, g_custom_container.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
      PERFORM f_selecao_dados_estoque.
      PERFORM f_exibir_dados_estoque.

    WHEN 'BACK' OR 'EXIT' OR 'CANCEL' OR 'VOLTAR'.
      CALL METHOD g_custom_container->free.
      FREE: g_grid, g_custom_container.
      LEAVE TO SCREEN 0.

  ENDCASE.

  FREE ok_code.

ENDMODULE.

************************************************************************
* SELECAO DADOS
************************************************************************
FORM f_selecao_dados_estoque.

  t_zsdt0415 = lc_distribuicao_insumos->get_dados_zsdt0415( i_zsds093 = lc_zsds093 i_zsds094 = wl_zsds094 ).

ENDFORM.

************************************************************************
* EXIBIR DADOS
************************************************************************
FORM f_exibir_dados_estoque.

  FREE: t_saida_estoque, lv_total_consum.

  LOOP AT t_zsdt0415 INTO DATA(_zsdt0415).
    CLEAR w_saida_estoque.

    CHECK _zsdt0415-mesma_solic   = abap_false.

    w_saida_estoque-nro_sol       = _zsdt0415-nro_sol.
    w_saida_estoque-seq           = _zsdt0415-seq.
    w_saida_estoque-vbeln         = _zsdt0415-vbeln.
    w_saida_estoque-posnr         = _zsdt0415-posnr.

    SELECT SINGLE *
      INTO @DATA(_0415_libe)
      FROM zsdt0415
     WHERE ch_referencia = @_zsdt0415-ch_referencia
       AND item_distrib  = @_zsdt0415-item_distrib
       AND etapa         = 'LIBE'
       AND cancelado     = @abap_off.

    IF sy-subrc = 0.
      w_saida_estoque-nro_sol_ref = _0415_libe-nro_sol_ref.
      w_saida_estoque-seq_ref     = _0415_libe-seq_ref.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(_0415_oven)
      FROM zsdt0415
     WHERE ch_referencia = @_zsdt0415-ch_referencia
       AND item_distrib  = @_zsdt0415-item_distrib
       AND etapa         = 'OVEN'
       AND cancelado     = @abap_off.

    IF sy-subrc = 0.
      w_saida_estoque-vbeln_new = _0415_oven-vbeln_new.
      w_saida_estoque-posnr_new = _0415_oven-posnr_new.
    ENDIF.

    SELECT SINGLE *
      INTO @DATA(_0415_soli)
      FROM zsdt0415
     WHERE ch_referencia = @_zsdt0415-ch_referencia
       AND item_distrib  = @_zsdt0415-item_distrib
       AND etapa         = 'SOLI'
       AND cancelado     = @abap_off.

    IF sy-subrc = 0.
      w_saida_estoque-nro_sol_new = _0415_soli-nro_sol_new.
    ENDIF.

    w_saida_estoque-qte_sol       = lc_distribuicao_insumos->set_analisar_estoque( _zsdt0415 ).

    IF _zsdt0415-solic_pai       = abap_true OR
       _zsdt0415-solic_cancelada = abap_true.
      w_saida_estoque-line_color  = 'C310'.
    ELSE.
      lv_total_consum = lv_total_consum + w_saida_estoque-qte_sol.
    ENDIF.

    w_saida_estoque-solic_pai     = _zsdt0415-solic_pai.
    w_saida_estoque-solic_cancelada = _zsdt0415-solic_cancelada.
    w_saida_estoque-nro_sol_filha = _zsdt0415-nro_sol_filha.
    w_saida_estoque-user_create   = _zsdt0415-user_create.
    w_saida_estoque-date_create   = _zsdt0415-date_create.
    w_saida_estoque-time_create   = _zsdt0415-time_create.

    APPEND w_saida_estoque       TO t_saida_estoque.
  ENDLOOP.

  IF t_saida_estoque[] IS NOT INITIAL.
    CLEAR w_saida_estoque .
    w_saida_estoque-vbeln_new     = 'Total:'.
    w_saida_estoque-qte_sol       = lv_total_consum.
    w_saida_estoque-line_color    = 'C410'.
    APPEND w_saida_estoque       TO t_saida_estoque.
  ENDIF.

  SORT t_saida_estoque BY date_create DESCENDING
                          time_create DESCENDING.

ENDFORM.

************************************************************************
* ALV ITENS
************************************************************************
FORM f_init_alv2.

  w_stable-row               = abap_true.
  w_stable-col               = abap_true.
*
* w_layout_wf-no_rowmark     = abap_true.
  w_layout_item-zebra        = abap_false.
  w_layout_item-sel_mode     = 'A'.
  w_layout_item-edit         = abap_false.
  w_layout_item-no_totarr    = abap_true.
  w_layout_item-no_totexp    = abap_true.
  w_layout_item-no_totline   = abap_true.
  w_layout_item-no_toolbar   = abap_true.
* w_layout_item-stylefname   = 'STYLE'.
  w_layout_item-info_fname   = 'LINE_COLOR'.
* w_layout_item-ctab_fname   = 'CELLCOLOR'.
* w_layout_item-info_fname   = 'LINE_COLOR'.
  w_layout_item-no_rowmark   = abap_true.

  IF g_grid IS INITIAL.
    PERFORM f_fieldcatalog2.
    PERFORM f_sort2.

    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = 'CC_CONTAINER'.

    CREATE OBJECT g_grid
      EXPORTING
        i_parent          = g_custom_container
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD g_grid->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    SET HANDLER: lcl_event_handler=>on_hotspot_click FOR g_grid,
                 lcl_event_handler=>on_data_changed  FOR g_grid,
                 lcl_event_handler=>user_command     FOR g_grid,
                 lcl_event_handler=>toolbar_item     FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout_item
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida_estoque
        it_sort                       = t_sort[]
        it_fieldcatalog               = t_fieldcat
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ELSE.
    CALL METHOD g_grid->refresh_table_display
      EXPORTING
        is_stable = w_stable.
  ENDIF.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_sort2.

  FREE: t_sort.

* CLEAR w_sort.
* w_sort-fieldname = 'ORDEM_ETAPA'.
**w_sort-subtot    = 'X'.
* w_sort-spos      = 3.
* w_sort-up        = abap_on.
* APPEND w_sort   TO t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog2.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''      ''       'T_SAIDA_ESTOQUE'  'NRO_SOL'         'Nro.Solicitacao'        '08'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    02  ''      ''       'T_SAIDA_ESTOQUE'  'SEQ'             'Sequencia'              '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    03  'VBAK'  'VBELN'  'T_SAIDA_ESTOQUE'  'VBELN'           'Ord.Venda'              '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    04  ''      ''       'T_SAIDA_ESTOQUE'  'POSNR'           'Item'                   '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    05  ''      ''       'T_SAIDA_ESTOQUE'  'NRO_SOL_NEW'     'Solicit.Criada'         '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    06  'VBAK'  'VBELN'  'T_SAIDA_ESTOQUE'  'VBELN_NEW'       'OV Criada'              '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    07  ''      ''       'T_SAIDA_ESTOQUE'  'POSNR_NEW'       'Item'                   '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    08  ''      ''       'T_SAIDA_ESTOQUE'  'NRO_SOL_REF'     'Solic.Distrib'          '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''      ''       'T_SAIDA_ESTOQUE'  'SEQ_REF'         'Seq.Distrib'            '05'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''      ''       'T_SAIDA_ESTOQUE'  'QTE_SOL'         'Quantidade Distr.'      '12'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    11  ''      ''       'T_SAIDA_ESTOQUE'  'SOLIC_PAI'       'Solic.Pai'              '07'  ' ' ' ' ' ' ' '  ' ' 'X' ' ' ' ' ' ' ' ',
    12  ''      ''       'T_SAIDA_ESTOQUE'  'NRO_SOL_FILHA'   'Solic.Filha'            '08'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    13  ''      ''       'T_SAIDA_ESTOQUE'  'SOLIC_CANCELADA' 'Solic.Cancelada'        '10'  ' ' ' ' ' ' ' '  ' ' 'X' ' ' ' ' ' ' ' ',
    14  ''      ''       'T_SAIDA_ESTOQUE'  'USER_CREATE'     'Usuario'                '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    15  ''      ''       'T_SAIDA_ESTOQUE'  'DATE_CREATE'     'Data'                   '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    16  ''      ''       'T_SAIDA_ESTOQUE'  'TIME_CREATE'     'Hora'                   '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

ENDFORM.

**********************************************************************
**********************************************************************

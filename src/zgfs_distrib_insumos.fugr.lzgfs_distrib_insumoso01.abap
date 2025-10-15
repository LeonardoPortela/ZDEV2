*----------------------------------------------------------------------*
***INCLUDE LZGFS_DISTRIB_INSUMOSO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  DATA: w_fcode       TYPE slis_extab,
        t_fcode       TYPE slis_t_extab,
        lv_pend_aprov TYPE char01,
        w_0082        TYPE zsdt0082.

  FREE: t_fcode, ok_code.

  w_0082        = lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = lc_zsds093-nro_sol i_seq = lc_zsds093-seq i_vbeln = lc_zsds093-vbeln
                                                         i_posnr   = lc_zsds093-posnr ).
  lv_pend_aprov = lc_distribuicao_insumos->get_pendente_aprovacao( i_ch_referencia = w_0082-ch_referencia ).

  IF lv_start           = abap_true OR
     lv_visualizar      = abap_true OR
   ( w_0082-processando = abap_true AND lc_distribuicao_insumos->get_ck_job_cancelado( w_0082 ) = abap_false ) OR "*-CS2025000249-08.09.2025-#189853-JT
     lv_pend_aprov      = abap_true.
    APPEND VALUE #( fcode = 'PROCESSAR'  ) TO t_fcode[].
  ENDIF.

  IF lv_visualizar = abap_true OR lc_distribuicao_insumos->get_permite_cancelar_distr( i_ch_referencia = w_0082-ch_referencia ) = abap_false.
    APPEND VALUE #( fcode = 'CANCELAR'  )  TO t_fcode[].
  ENDIF.

* IF lv_visualizar = abap_true.
*   APPEND VALUE #( fcode = 'REFRESH'  )   TO t_fcode[].
* ENDIF.

  SET PF-STATUS 'PF_0100'           EXCLUDING t_fcode.
  SET TITLEBAR 'PT_0100'.

  PERFORM f_init_alv.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  DATA: lv_erro TYPE char01.

  CASE ok_code.

    WHEN 'VOLTAR'.
      CALL METHOD g_custom_container->free.
      FREE: g_grid, g_custom_container.
      LEAVE TO SCREEN 0.

    WHEN 'REFRESH'.
      PERFORM f_selecao_dados.
      PERFORM f_exibir_dados.

    WHEN 'PROCESSAR'.
      PERFORM f_processar.

    WHEN 'CANCELAR'.
      PERFORM f_cancelar CHANGING lv_erro.

      IF lv_erro = abap_false.
        CALL METHOD g_custom_container->free.
        FREE: g_grid, g_custom_container.
        LEAVE TO SCREEN 0.
      ENDIF.

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
FORM f_selecao_inicial.

  FREE: t_zsdt0415_pend.

  w_zsdt0082   = lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = lc_zsds093-nro_sol
                                                        i_seq     = lc_zsds093-seq
                                                        i_vbeln   = lc_zsds093-vbeln
                                                        i_posnr   = lc_zsds093-posnr ).
  CHECK w_zsdt0082-ch_referencia IS NOT INITIAL.

  t_zsdt0415_pend = lc_distribuicao_insumos->get_zsdt0415( i_ch_referencia      = w_zsdt0082-ch_referencia
                                                           i_somente_pendentes  = abap_true ).

ENDFORM.

************************************************************************
* SELECAO DADOS
************************************************************************
FORM f_selecao_dados.

  FREE: t_zsdt0415.

  w_zsdt0082   = lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = lc_zsds093-nro_sol
                                                        i_seq     = lc_zsds093-seq
                                                        i_vbeln   = lc_zsds093-vbeln
                                                        i_posnr   = lc_zsds093-posnr ).
  CHECK w_zsdt0082-ch_referencia IS NOT INITIAL.

  t_zsdt0415   = lc_distribuicao_insumos->get_zsdt0415( i_ch_referencia        = w_zsdt0082-ch_referencia
                                                        i_zsdt0415             = t_zsdt0415_pend
                                                        i_somente_selecionados = abap_true ).

  SORT t_zsdt0415 BY ch_referencia item_distrib DESCENDING.

ENDFORM.

************************************************************************
* SELECAO DADOS
************************************************************************
FORM f_selecao_total.

  FREE: t_zsdt0415.

  w_zsdt0082   = lc_distribuicao_insumos->get_zsdt0082( i_nro_sol = lc_zsds093-nro_sol
                                                        i_seq     = lc_zsds093-seq
                                                        i_vbeln   = lc_zsds093-vbeln
                                                        i_posnr   = lc_zsds093-posnr ).
  CHECK w_zsdt0082-ch_referencia IS NOT INITIAL.

  t_zsdt0415   = lc_distribuicao_insumos->get_zsdt0415( i_ch_referencia = w_zsdt0082-ch_referencia i_incluir_cancelado = abap_true ).

  SORT t_zsdt0415 BY ch_referencia item_distrib DESCENDING.

ENDFORM.

************************************************************************
* EXIBIR DADOS
************************************************************************
FORM f_exibir_dados.

  DATA: t_domain TYPE TABLE OF dd07v,
        w_domain TYPE dd07v.

  FREE: t_saida.

  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname   = 'ZDE_ETAPA'
      text      = 'X'
      langu     = sy-langu
    TABLES
      dd07v_tab = t_domain.

  LOOP AT t_zsdt0415            INTO DATA(_zsdt0415).
    CLEAR w_saida.

    MOVE-CORRESPONDING _zsdt0415  TO w_saida.

    SELECT SINGLE observacao
      INTO @DATA(_observacao)
      FROM zsdt0411
     WHERE nro_sol          = @_zsdt0415-nro_sol
       AND id_distribuicao  = @_zsdt0415-id_distribuicao.

    IF _observacao IS NOT INITIAL.
      w_saida-mensagem      = _observacao.
    ENDIF.

    SELECT SINGLE *
      FROM zsdt0082 INTO @DATA(lwa_dist_sol)
     WHERE nro_sol  = @_zsdt0415-nro_sol_ref
       AND seq      = @_zsdt0415-seq_ref
       AND vbeln    = @_zsdt0415-vbeln_ref
       AND posnr    = @_zsdt0415-posnr_ref.

    IF sy-subrc EQ 0 AND lwa_dist_sol-status = '4'.
      w_saida-dist_cancelada = icon_okay.
    ENDIF.

    w_saida-qte_sol2        = w_saida-qte_sol.
    w_saida-rejeitado       = w_saida-cancelado.
    w_saida-cancelar_dist   = COND #( WHEN _zsdt0415-etapa = 'LIBE' AND w_saida-rejeitado = abap_off AND lv_visualizar = abap_true
                                           THEN icon_display_more
                                           ELSE abap_off ).

    CASE w_saida-status.
      WHEN 'C'.
        w_saida-icon_status = icon_system_okay. "icon_okay.
      WHEN 'P'.
        w_saida-icon_status = icon_system_cancel. "icon_cancel.
      WHEN 'W'.
        w_saida-icon_status = icon_warning.
    ENDCASE.

    READ TABLE t_domain  INTO DATA(_domain) WITH KEY domvalue_l = w_saida-etapa.
    IF sy-subrc = 0.
      w_saida-descr_etapa   = _domain-ddtext.
    ENDIF.

    APPEND w_saida         TO t_saida.
  ENDLOOP.

  SORT t_saida BY item_distrib ordem_etapa.

ENDFORM.

************************************************************************
* processamento
************************************************************************
FORM f_processar.

  lv_start = abap_true.

*-CS2025000249-08.09.2025-#189941-JT-inicio
  LOOP AT t_saida   INTO w_saida.
    w_saida-mensagem   = abap_false.
    MODIFY t_saida  FROM w_saida INDEX sy-tabix.
  ENDLOOP.

  CALL METHOD g_grid->refresh_table_display
    EXPORTING
      is_stable = w_stable.
*-CS2025000249-08.09.2025-#189941-JT-fim

*-CS2025000249-08.09.2025-#189853-JT-inicio
*  IF 1 = 2.
*    CALL FUNCTION 'ZSD_INSUMOS_DISTRIBUICAO'
*      EXPORTING
*        i_zsds093 = lc_zsds093
*      TABLES
*        t_zsds094 = lc_zsds094.
*    RETURN.
*  ENDIF.
*
*  DATA(l_task) = 'DISTRIB_INSUMOS' && lc_zsds093-nro_sol && lc_zsds093-seq && lc_zsds093-vbeln && lc_zsds093-posnr.
*
*  CALL FUNCTION 'ZSD_INSUMOS_DISTRIBUICAO' STARTING NEW TASK l_task
*    EXPORTING
*      i_zsds093 = lc_zsds093
*    TABLES
*      t_zsds094 = lc_zsds094.

*-CS2025000249-08.09.2025-#189853-JT-inicio
*--------------------------------------------------------------------------------
*-criar JOB
*--------------------------------------------------------------------------------
  TRY.
      lc_distribuicao_insumos->set_criar_job( i_zsds093 = lc_zsds093 i_zsds094 = lc_zsds094 ).
    CATCH zcx_error INTO DATA(ex_error).
      MESSAGE ID ex_error->msgid TYPE 'S' NUMBER ex_error->msgno WITH ex_error->msgv1 ex_error->msgv2
                                                                      ex_error->msgv3 ex_error->msgv4.
      RETURN.
  ENDTRY.
*-CS2025000249-08.09.2025-#189853-JT-fim

ENDFORM.

************************************************************************
* processamento
************************************************************************
FORM f_cancelar CHANGING p_erro.

  DATA: t_0415 TYPE TABLE OF zsdt0415.

  FREE: p_erro.

  READ TABLE t_zsdt0415 INTO DATA(_zsdt0415) INDEX 1.
  IF sy-subrc <> 0.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Não é possivel Cancelar esta Distribuição!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lc_distribuicao_insumos->get_permite_cancelar_distr( i_ch_referencia = _zsdt0415-ch_referencia ) = abap_false.
    p_erro = abap_true.
    MESSAGE s024(sd) WITH 'Não é possivel Cancelar esta Distribuição!' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  t_0415[]     = t_zsdt0415[].
  SORT t_0415 BY item_distrib.
  DELETE ADJACENT DUPLICATES FROM t_0415 COMPARING item_distrib.

  LOOP AT t_0415 INTO DATA(_0415).
    lc_distribuicao_insumos->set_estornar_solicitacao( i_ch_referencia   = _0415-ch_referencia
                                                       i_item_distrib    = _0415-item_distrib ).
  ENDLOOP.

  MESSAGE s024(sd) WITH 'Distribuição cancelada com Sucesso!'.

ENDFORM.

************************************************************************
* ALV ITENS
************************************************************************
FORM f_init_alv.

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
  w_layout_item-stylefname   = 'STYLE'.
* w_layout_item-ctab_fname   = 'CELLCOLOR'.
* w_layout_item-info_fname   = 'LINE_COLOR'.
  w_layout_item-no_rowmark   = abap_true.

  IF g_grid IS INITIAL.
    PERFORM f_fieldcatalog.
    PERFORM f_sort.

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

    SET HANDLER: lcl_event_handler=>on_hotspot_click       FOR g_grid,
                 lcl_event_handler=>on_data_changed        FOR g_grid,
                 lcl_event_handler=>user_command           FOR g_grid,
                 lcl_event_handler=>toolbar_item           FOR g_grid,
                 lcl_event_handler=>handle_on_button_click FOR g_grid.

    CALL METHOD g_grid->set_table_for_first_display
      EXPORTING
        is_layout                     = w_layout_item
        i_save                        = 'A'
*       it_toolbar_excluding          = t_function
      CHANGING
        it_outtab                     = t_saida
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
FORM f_sort.

  FREE: t_sort.

  CLEAR w_sort.
  w_sort-fieldname = 'ITEM_DISTRIB'.
* w_sort-subtot    = 'X'.
  w_sort-spos      = 1.
  w_sort-down      = abap_on.
  APPEND w_sort   TO t_sort.

  CLEAR w_sort.
  w_sort-fieldname = 'QTE_SOL2'.
* w_sort-subtot    = 'X'.
  w_sort-spos      = 2.
  w_sort-up        = abap_on.
  APPEND w_sort   TO t_sort.

  CLEAR w_sort.
  w_sort-fieldname = 'ORDEM_ETAPA'.
* w_sort-subtot    = 'X'.
  w_sort-spos      = 3.
  w_sort-up        = abap_on.
  APPEND w_sort   TO t_sort.

ENDFORM.

**********************************************************************
*  catalogo
**********************************************************************
FORM f_fieldcatalog.

  FREE t_fieldcat[].

  PERFORM f_estrutura_alv USING:
    01  ''      ''       'T_SAIDA'      'ITEM_DISTRIB'      'Item'                   '06'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    02  ''      ''       'T_SAIDA'      'ORDEM_ETAPA'       'Etapa'                  '03'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' 'X' ' ' 'X',
    03  ''      ''       'T_SAIDA'      'DESCR_ETAPA'       'Etapa'                  '35'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' 'X',
    04  ''      ''       'T_SAIDA'      'ICON_STATUS'       'Status'                 '08'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' 'X',
    05  ''      ''       'T_SAIDA'      'CANCELAR_DIST'     'Canc.Distrib'           '08'  ' ' ' ' ' ' ' '  ' ' ' ' cl_gui_alv_grid=>mc_style_button ' ' ' ' 'X',
    05  ''      ''       'T_SAIDA'      'DIST_CANCELADA'    'Dist.Cancel.'           '08'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' 'X' 'X',
    06  ''      ''       'T_SAIDA'      'REJEITADO'         'Rejeitado'              '08'  ' ' ' ' ' ' ' '  ' ' 'X' ' ' ' ' ' ' 'X',
    07  ''      ''       'T_SAIDA'      'NRO_SOL_NEW'       'Solic.Criada'           '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    08  'VBAK'  'VBELN'  'T_SAIDA'      'VBELN_NEW'         'OV Criada'              '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    09  ''      ''       'T_SAIDA'      'POSNR_NEW'         'Item'                   '08'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    10  ''      ''       'T_SAIDA'      'NRO_SOL_REF'       'Solicitação'            '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    11  ''      ''       'T_SAIDA'      'SEQ_REF'           'Sequencia'              '08'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
*   12  ''      ''       'T_SAIDA'      'VBELN_REF'         'OV Refer.'              '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
*   13  ''      ''       'T_SAIDA'      'POSNR_REF'         'Item'                   '08'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    14  ''      ''       'T_SAIDA'      'QTE_SOL2'          'Quantidade Distr.'      '15'  ' ' ' ' 'X' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    15  ''      ''       'T_SAIDA'      'MENSAGEM'          'Mensagem'               '90' ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    16  ''      ''       'T_SAIDA'      'USER_CREATE'       'Usuario'                '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    17  ''      ''       'T_SAIDA'      'DATE_CREATE'       'Data'                   '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' ',
    18  ''      ''       'T_SAIDA'      'TIME_CREATE'       'Hora'                   '10'  ' ' ' ' ' ' ' '  ' ' ' ' ' ' ' ' ' ' ' '.

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


FORM f_busca_dados .

  FREE: t_bordero,
        t_itens_carga,
        t_notas,
        t_de_para.

  zcl_carga_saida_insumos=>busca_dados_carga(
    EXPORTING
      i_nr_carga_single   = gv_carga
      i_dados_conferencia = abap_true
    IMPORTING
      e_solicitacoes      = t_itens_carga
      e_notas_conferencia = t_notas
      e_bordero           = t_bordero
      e_dados_conferencia = t_de_para
  ).

ENDFORM.


FORM f_exibe_dados .

*** ALV's Borderô, carga e notas
  IF go_custom_container_1000 IS INITIAL.

    PERFORM f_cria_objetos_alv.

    PERFORM f_exibe_dados_alv_bordero.

    PERFORM f_exibe_dados_alv_carga.

    PERFORM f_exibe_dados_alv_notas.

  ELSE.

    CALL METHOD go_bordero_1000->refresh_table_display.
    CALL METHOD go_carga_1000->refresh_table_display.
    CALL METHOD go_notas_1000->refresh_table_display.

  ENDIF.

*** Alv DE-PARA
  IF go_custom_container_1000_2 IS INITIAL.

    CREATE OBJECT go_custom_container_1000_2
      EXPORTING
        container_name              = 'CONTAINER2'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    PERFORM f_exibe_dados_alv_de_para.

  ELSE.

    CALL METHOD go_de_para_1000->refresh_table_display.

  ENDIF.

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

  IF p_fieldname EQ 'ACAO'.
    wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
  ENDIF.

  APPEND wa_fieldcatalog TO p_it_fieldacatalog.

ENDFORM.

FORM fill_it_fieldcatalog_v2  TABLES p_it_fieldacatalog STRUCTURE lvc_s_fcat
                           USING VALUE(p_colnum)
                                 VALUE(p_fieldname)
                                 VALUE(p_tabname)
                                 VALUE(p_emphasize)
                                 VALUE(p_edit)
                                 VALUE(p_icon)
                                 VALUE(p_hotspot)
                                 VALUE(p_opt)
                                 VALUE(p_outputlen)
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
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-f4availabl = p_f4.

  IF p_fieldname EQ 'ACAO'.
    wa_fieldcatalog-style = cl_gui_alv_grid=>mc_style_button.
  ENDIF.

  APPEND wa_fieldcatalog TO p_it_fieldacatalog.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_exibe_dados_alv_bordero
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_dados_alv_bordero .

  DATA: lt_f4 TYPE lvc_t_f4,
        ls_f4 TYPE lvc_s_f4.

  "ALV Lotes
  FREE: lt_f4.

  ls_f4-fieldname  = 'NR_FASE'.
  ls_f4-register   = 'X'.
  APPEND ls_f4 TO lt_f4.

  PERFORM fill_it_fieldcatalog_v2 TABLES t_fieldcatalog_bordero_1000 USING:
    01 'CHECK'              ' '           ' '      'X'  ' '  ' '   ' ' '02' 'X'   ' '   ' '   '',
    03 'ID_ITEM'            ' '           ' '      ' '  ' '  ' '   ' ' '04' ' '   ' '   ' '   'Item',
    05 'MATNR'              'MARA'        ' '      ' '  ' '  ' '   ' ' '10' ' '   ' '   ' '   'Cd.Produto',
    04 'DS_PRODUTO'         ' '           ' '      ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'Produto',
    06 'LOTE'               ' '           ' '      ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'Lote',
    07 'NR_FASE'            'ZSDT0134'    'C310'   ' '  ' '  ' '   ' ' '20' ' '   ' '   'X'   'Fase',
    08 'QUANTIDADE'         ' '           ' '      ' '  ' '  ' '   ' ' '13' ' '   ' '   ' '   'Qtde Borderô',
    09 'EMBALAGEM'          ' '           ' '      ' '  ' '  ' '   ' ' '09' ' '   ' '   ' '   'Embalagem',
    10 'SALDO_CONF'         ' '           'C510'   ' '  ' '  ' '   ' ' '13' ' '   ' '   ' '   'Saldo'.

  gs_layout_1000_bordero-info_fname = 'COLOR'.
  "gs_layout_1000_bordero-cwidth_opt = 'X'.
  gs_layout_1000_bordero-grid_title = 'Dados Borderô'.
  gs_layout_1000_bordero-no_toolbar = abap_true.
  gs_layout_1000_bordero-no_rowmark = abap_true.

  CREATE OBJECT go_bordero_1000
    EXPORTING
      i_parent = go_parent_1_1000.           "ALV borderô

  CALL METHOD go_bordero_1000->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4.

*  CALL METHOD go_bordero_1000->set_ready_for_input
*    EXPORTING
*      i_ready_for_input = 0.

  SET HANDLER lcl_bordero=>on_f4 FOR go_bordero_1000.

  CALL METHOD go_bordero_1000->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout_1000_bordero
      i_save          = 'A'
    CHANGING
      it_fieldcatalog = t_fieldcatalog_bordero_1000
      it_outtab       = t_bordero.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_dados_alv_carga
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_dados_alv_carga .

  PERFORM fill_it_fieldcatalog_v2 TABLES t_fieldcatalog_carga_1000 USING:
            01 'CHECK'       ' '       ' '     'X'  ' '  ' '   ' ' '02'  'X'   ' '   ' '   '',
            02 'NRO_SOL'     ' '       ' '     ' '  ' '  ' '   ' ' '10'  ' '   ' '   ' '   'Nro.Sol.',
            03 'SEQ'         ' '       ' '     ' '  ' '  ' '   ' ' '09'  ' '   ' '   ' '   'Item Sol.',
            04 'VBELN'       ' '       ' '     ' '  ' '  ' '   ' ' '10'  ' '   ' '   ' '   'Ordem',
            05 'POSNR'       ' '       ' '     ' '  ' '  ' '   ' ' '08'  ' '   ' '   ' '   'Item OV.',
            06 'EBELN'       ' '       ' '     ' '  ' '  ' '   ' ' '10'  ' '   ' '   ' '   'Pedido',
            07 'EBELP'       ' '       ' '     ' '  ' '  ' '   ' ' '08'  ' '   ' '   ' '   'Itm.Ped.',
            08 'MATNR'       'MARA'    ' '     ' '  ' '  ' '   ' ' '10'  ' '   ' '   ' '   'Produto',
            09 'MAKTX'       ' '       ' '     ' '  ' '  ' '   'X' ' '   ' '   ' '   ' '   'Desc. Produto',
            10 'QTD_VINC'    ' '       ' '     ' '  ' '  ' '   ' ' '14'  ' '   ' '   ' '   'Qtd Itm Sol.CG',
            11 'MEINS'       'ZSDS381' ' '     ' '  ' '  ' '   ' ' '06'  ' '   ' '   ' '   'UM OV.',
            12 'SALDO_CONF'  ' '       'C510'  ' '  ' '  ' '   ' ' '13'  ' '   ' '   ' '   'Saldo'.

  gs_layout_1000_carga-info_fname = 'COLOR'.
  "gs_layout_1000_carga-cwidth_opt = 'X'.
  gs_layout_1000_carga-grid_title = 'Dados da Carga'.
  gs_layout_1000_carga-no_toolbar = abap_true.
  gs_layout_1000_carga-no_rowmark = abap_true.

  CREATE OBJECT go_carga_1000
    EXPORTING
      i_parent = go_parent_2_1000.           "ALV borderô

  CALL METHOD go_carga_1000->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout_1000_carga
      i_save          = 'A'
    CHANGING
      it_fieldcatalog = t_fieldcatalog_carga_1000
      it_outtab       = t_itens_carga.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_dados_notas
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_dados_alv_notas .
  "
  PERFORM fill_it_fieldcatalog_v2 TABLES t_fieldcatalog_notas_1000 USING:
            01 'CHECK'              ' '         ' '     'X'  ' '  ' '   ' ' ' '  'X'   ' '   ' '   '',
            02 'CHAVE_NFE'          ' '         ' '     ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'Chave NFe',
            03 'NUMERO_NFE'         ' '         ' '     ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'Numero',
            04 'PROD_ITEM'          ' '         ' '     ' '  ' '  ' '   ' ' '04' ' '   ' '   ' '   'Item',
            05 'CODIGO_PRODUTO'     'ZSDS390'   ' '     ' '  ' '  ' '   ' ' '10' ' '   ' '   ' '   'Cd.Produto',
            06 'DESC_PRODUTO'       ' '         ' '     ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'Desc.Produto',
            07 'COD_CONTROLE'       ' '         ' '     ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'NCM',
            08 'QUANTIDADE'         ' '         ' '     ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'Qtde',
            09 'UNIDADE'            ' '         ' '     ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'UM',
            10 'PRECO_LIQUIDO'      ' '         ' '     ' '  ' '  ' '   ' ' '10' ' '   ' '   ' '   'Preço Liq.',
            11 'MONTANTE'           ' '         ' '     ' '  ' '  ' '   ' ' '10' ' '   ' '   ' '   'Mont. Básico',
            12 'ICMS'               ' '         ' '     ' '  ' '  ' '   'X' ' '  ' '   ' '   ' '   'ICMS',
            13 'PESO_CONV'          'ZSDS390'   ' '     'X'  ' '  ' '   ' ' '10' ' '   ' '   ' '   'Peso Conv.',
            13 'QTD_CONV'           ' '         ' '     ' '  ' '  ' '   ' ' '13' ' '   ' '   ' '   'Qtd.Conv.',
            14 'SALDO_CONF'         ' '         'C510'  ' '  ' '  ' '   ' ' '13' ' '   ' '   ' '   'Saldo'.

  gs_layout_1000_notas-info_fname = 'COLOR'.
  "gs_layout_1000_notas-cwidth_opt = 'X'.
  gs_layout_1000_notas-grid_title = 'Dados das Nota Fiscal'.
  gs_layout_1000_notas-no_toolbar = abap_true.
  gs_layout_1000_notas-no_rowmark = abap_true.

  CREATE OBJECT go_notas_1000
    EXPORTING
      i_parent = go_parent_3_1000.           "ALV borderô

  CALL METHOD go_notas_1000->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD go_notas_1000->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER lcl_carga=>on_data_changed FOR go_notas_1000.

  CALL METHOD go_notas_1000->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout_1000_notas
      i_save          = 'A'
    CHANGING
      it_fieldcatalog = t_fieldcatalog_notas_1000
      it_outtab       = t_notas.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_dados_alv_de_para
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_dados_alv_de_para .

  PERFORM fill_it_fieldcatalog_v2 TABLES t_fieldcatalog_de_para_1000 USING:
            01 'CHECK'              ''          ' '        'X'  ' '  ' '   ' ' '02'  'X'   ' '   ' '   '',
            02 'COD_PROD_BORDERO'   'ZSDS388'   'C510'     ' '  ' '  ' '   ' ' '12'  ' '   ' '   ' '   'Prod.Borderô',
            03 'DESC_PROD_BORDERO'  'ZSDS388'   'C510'     ' '  ' '  ' '   'X' ' '   ' '   ' '   ' '   'Ds. Produto Borderô',
            04 'LOTE_BORDERO'       'ZSDS388'   'C510'     ' '  ' '  ' '   ' ' ' '   ' '   ' '   ' '   'Lote Borderô',
            04 'NR_FASE'            'ZSDS388'   'C510'     ' '  ' '  ' '   ' ' '20'  ' '   ' '   ' '   'Fase',
            05 'EMBALAGEM'          'ZSDS388'   'C510'     ' '  ' '  ' '   ' ' '10'  ' '   ' '   ' '   'UM.Borderô',
            06 'NRO_SOL'            'ZSDS388'   'C610'     ' '  ' '  ' '   ' ' '10'  ' '   ' '   ' '   'Nro.Sol',
            07 'SEQ'                'ZSDS388'   'C610'     ' '  ' '  ' '   ' ' '07'  ' '   ' '   ' '   'Itm.Sol',
            08 'COD_PROD_OV'        'ZSDS388'   'C610'     ' '  ' '  ' '   ' ' '11'  ' '   ' '   ' '   'Cod.Prod.OV',
            09 'DESC_PROD_OV'       'ZSDS388'   'C610'     ' '  ' '  ' '   'X' ' '   ' '   ' '   ' '   'Desc. Produto OV',
            "08 'UNIDADE_OV'         'ZSDS388'   'C610'     ' '  ' '  ' '   ' ' '05'  ' '   ' '   ' '   'UM OV',
            10 'NUMERO_NFE'         'ZSDS388'   ' '        ' '  ' '  ' '   ' ' '10'  ' '   ' '   ' '   'Nro NFe',
            11 'PROD_ITEM'          'ZSDS388'   ' '        ' '  ' '  ' '   ' ' '08'  ' '   ' '   ' '   'Item NFe',
            12 'COD_PROD_NFE'       'ZSDS388'   ' '        ' '  ' '  ' '   ' ' '12'  ' '   ' '   ' '   'Cod.Prod.NFe',
            13 'DESC_PROD_NFE'      'ZSDS388'   ' '        ' '  ' '  ' '   'X' ' '   ' '   ' '   ' '   'Desc. Produto NFe',
            14 'UNIDADE_NFE'        'ZSDS388'   ' '        ' '  ' '  ' '   ' ' '06'  ' '   ' '   ' '   'UM NFe',
            15 'QUANTIDADE'         'ZSDS388'   ' '        'X'  ' '  ' '   ' ' '13'  ' '   ' '   ' '   'Quantidade'.

  gs_layout_1000_de_para-info_fname = 'COLOR'.
  "gs_layout_1000_de_para-cwidth_opt = 'X'.
  gs_layout_1000_de_para-grid_title = 'Dados de conferência DE/PARA'.
  gs_layout_1000_de_para-no_toolbar = abap_true.
  gs_layout_1000_de_para-no_rowmark = abap_true.
  gs_layout_1000_de_para-stylefname = 'CELLTAB'.

  CREATE OBJECT go_de_para_1000
    EXPORTING
      i_parent = go_custom_container_1000_2.           "ALV borderô

  CALL METHOD go_de_para_1000->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD go_de_para_1000->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER lcl_carga=>on_data_changed FOR go_de_para_1000.

  CALL METHOD go_de_para_1000->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout_1000_de_para
      i_save          = 'A'
    CHANGING
      it_fieldcatalog = t_fieldcatalog_de_para_1000
      it_outtab       = t_de_para.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form f_cria_objetos_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_cria_objetos_alv .

  CREATE OBJECT go_custom_container_1000
    EXPORTING
      container_name              = 'CONTAINER1'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  CREATE OBJECT go_splitter_1_1000
    EXPORTING
      parent  = go_custom_container_1000
      rows    = 3
      columns = 1.

  CALL METHOD go_splitter_1_1000->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = go_parent_1_1000.

  CALL METHOD go_splitter_1_1000->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = go_parent_2_1000.

  CALL METHOD go_splitter_1_1000->get_container
    EXPORTING
      row       = 3
      column    = 1
    RECEIVING
      container = go_parent_3_1000.

  CALL METHOD go_splitter_1_1000->set_row_mode
    EXPORTING
      mode = go_splitter_1_1000->mode_relative.

  CALL METHOD go_splitter_1_1000->set_row_height
    EXPORTING
      id     = 1
      height = 20.

  CALL METHOD go_splitter_1_1000->set_row_height
    EXPORTING
      id     = 2
      height = 20.

  CALL METHOD go_splitter_1_1000->set_row_height
    EXPORTING
      id     = 3
      height = 20.

ENDFORM.

FORM f_vincular_conferencia .

  CALL METHOD go_bordero_1000->check_changed_data( ).
  CALL METHOD go_carga_1000->check_changed_data( ).
  CALL METHOD go_notas_1000->check_changed_data( ).

  DATA(lt_bordero) = t_bordero.
  DATA(lt_itens_carga)   = t_itens_carga.
  DATA(lt_notas)   = t_notas.

  DELETE lt_bordero       WHERE check = space.
  DELETE lt_itens_carga   WHERE check = space.
  DELETE lt_notas         WHERE check = space.

  IF lines( lt_bordero ) > 1.
    MESSAGE 'Favor selecionar apenas uma linha do borderô' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lines( lt_itens_carga ) > 1.
    MESSAGE 'Favor selecionar apenas uma linha da carga' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF lines( lt_notas ) > 1.
    MESSAGE 'Favor selecionar apenas uma linha de nota' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  READ TABLE lt_bordero INTO DATA(ls_bordero) INDEX 1.
  READ TABLE lt_itens_carga  INTO DATA(ls_item_carga)   INDEX 1.
  READ TABLE lt_notas   INTO DATA(ls_nota)   INDEX 1.

  zcl_carga_saida_insumos=>vincular_conferencia(
    EXPORTING
      i_nro_carga  = gv_carga
      i_bordero    = ls_bordero
      i_item_carga = ls_item_carga
      i_nota       = ls_nota
    IMPORTING
      e_msg_erro = DATA(lv_msg_erro)
      e_zsdt0420 = DATA(lt_zsdt0420) ).

  IF lv_msg_erro IS NOT INITIAL.
    MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM f_valida_qtd_conf USING i_de_para TYPE zsds388 CHANGING c_error.

  DATA: ls_stable      TYPE lvc_s_stbl,
        ls_cell_row_no TYPE lvc_s_roid,
        ls_cell_row_id TYPE lvc_s_row,
        ls_cell_col_id TYPE lvc_s_col,
        ls_mod_cell    TYPE lvc_s_modi,
        lv_new_value   TYPE string,
        lv_old_value   TYPE string.

  CLEAR: c_error.

  SORT t_bordero BY id_autorizacao_embarque id_item lote.
  SORT t_itens_carga BY nro_sol seq.
  SORT t_notas BY chave_nfe prod_item.

  READ TABLE t_bordero ASSIGNING FIELD-SYMBOL(<fs_bordero>) WITH KEY id_autorizacao_embarque = i_de_para-nro_cg
                                                                     id_item                 = i_de_para-id_item_carregamento
                                                                     lote                    = i_de_para-lote_carregamento
                                                                     BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    DATA(ls_bordero) = <fs_bordero>.
  ENDIF.

  READ TABLE t_itens_carga ASSIGNING FIELD-SYMBOL(<fs_item_carga>) WITH KEY nro_sol = i_de_para-nro_sol seq = i_de_para-seq BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    DATA(ls_item_carga) = <fs_item_carga>.
  ENDIF.

  READ TABLE t_notas ASSIGNING FIELD-SYMBOL(<fs_nota>)  WITH KEY chave_nfe = i_de_para-chave_nfe prod_item = i_de_para-prod_item BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    DATA(ls_nota) = <fs_nota>.
  ENDIF.

  zcl_carga_saida_insumos=>informa_qtd_conferencia(
    EXPORTING
      i_nro_carga = gv_carga
      i_de_para   = i_de_para
    IMPORTING
      e_msg_erro  = DATA(lv_msg_erro)
    CHANGING
      c_bordero     = ls_bordero
      c_item_carga  = ls_item_carga
      c_nota        = ls_nota ).

  IF lv_msg_erro IS NOT INITIAL.
    c_error = abap_true.
    MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
    CLEAR i_de_para-quantidade.
  ELSE.

    IF <fs_bordero> IS ASSIGNED.
      <fs_bordero> = ls_bordero.
    ENDIF.

    IF <fs_nota> IS ASSIGNED.
      <fs_nota> = ls_nota.
    ENDIF.

    IF <fs_item_carga> IS ASSIGNED.
      <fs_item_carga> = ls_item_carga.
    ENDIF.

  ENDIF.

*  ls_stable-col = abap_true.
*  ls_stable-row = abap_true.
*
*  CALL METHOD go_bordero_1000->refresh_table_display( ).
*  CALL METHOD go_carga_1000->refresh_table_display( ).
*  CALL METHOD go_notas_1000->refresh_table_display( ).
*  CALL METHOD go_de_para_1000->refresh_table_display
*    EXPORTING
*      is_stable = ls_stable.

  go_de_para_1000->get_current_cell(
    IMPORTING
      es_row_id = ls_cell_row_id
      es_col_id = ls_cell_col_id
      es_row_no = ls_cell_row_no
  ).

  go_de_para_1000->set_current_cell_via_id(
  is_row_id    = ls_cell_row_id
  is_column_id = ls_cell_col_id
  is_row_no    = ls_cell_row_no ).

ENDFORM.

FORM f_desvicular_conferencia .

  DATA: lt_conferencia TYPE TABLE OF zsdt0420.

  CALL METHOD go_de_para_1000->check_changed_data( ).

  DATA(lt_de_para) = t_de_para.
  DELETE lt_de_para WHERE check IS INITIAL.

  LOOP AT lt_de_para ASSIGNING FIELD-SYMBOL(<fs_de_para>).
    APPEND INITIAL LINE TO lt_conferencia ASSIGNING FIELD-SYMBOL(<fs_conferencia>).
    MOVE-CORRESPONDING <fs_de_para> TO <fs_conferencia>.
  ENDLOOP.

  zcl_carga_saida_insumos=>desvincula_conferencia(
  EXPORTING
      i_conferencia = lt_conferencia
      i_nro_cg      = gv_carga
    IMPORTING
      e_msg_erro = DATA(lv_msg_erro) ).

  IF lv_msg_erro IS NOT INITIAL.
    MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.


FORM f_monta_alv_aceite .

  IF t_aceite IS INITIAL.
    CASE sy-ucomm.
      WHEN 'ACEITE'.
        DATA(r_conferida_total) = zcl_carga_saida_insumos=>check_and_set_carga_conferida(  i_nro_cg = gv_carga ).

        IF r_conferida_total EQ abap_false.
          MESSAGE 'Não há notas para realizar aceite' TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.

      WHEN 'CANC_AC'.
        MESSAGE 'Não há notas para realizar cancelamento' TYPE 'S' DISPLAY LIKE 'E'.
    ENDCASE.

    LEAVE TO SCREEN 0.
  ENDIF.


  IF go_custom_container_1002 IS NOT INITIAL.
    go_aceite_1002->free( ).
    go_custom_container_1002->free( ).

    FREE go_custom_container_1002.
  ENDIF.

  FREE: t_fieldcatalog_aceite_1002.

  PERFORM fill_it_fieldcatalog TABLES t_fieldcatalog_aceite_1002 USING:
  01 'CHAVE_NFE'         ' '  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Chave NFe',
  02 'COD_NFE'           ' '  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cod. NFe',
  03 'MATNR'             ' '  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Cod. Material',
  04 'MAKTX'             ' '  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Desc. Material',
  05 'ACAO'              ' '  ' '     ' '  ' '  ' '   'X'   ' '   ' '   ' '   'Ação'.


  gs_layout_1002-cwidth_opt = 'X'.
  gs_layout_1002-no_toolbar = abap_true.
  gs_layout_1002-no_rowmark = abap_true.

  CREATE OBJECT go_custom_container_1002
    EXPORTING
      container_name              = 'CONTAINER_ACEITE'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  CREATE OBJECT go_aceite_1002
    EXPORTING
      i_parent = go_custom_container_1002.

  CALL METHOD go_aceite_1002->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD go_aceite_1002->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER lcl_aceite=>handle_on_button_click FOR go_aceite_1002.

  CALL METHOD go_aceite_1002->set_table_for_first_display
    EXPORTING
      is_layout       = gs_layout_1002
      i_save          = 'A'
    CHANGING
      it_fieldcatalog = t_fieldcatalog_aceite_1002
      it_outtab       = t_aceite.


ENDFORM.

FORM f_aceite_fiscal .

  gv_operacao_aceite = sy-ucomm.

  CALL SCREEN 1002 STARTING AT 10 5 ENDING AT 150 21.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_dados_alv_aceite
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_dados_alv_aceite .

  FREE: t_aceite.

  DATA(_canc_aceite) = abap_false.
  IF gv_operacao_aceite EQ 'CANC_AC'.
    _canc_aceite = abap_true.
  ENDIF.

  zcl_carga_saida_insumos=>monta_dados_aceite_fiscal(
    EXPORTING
      i_bordero             = t_bordero
      i_itens_carga         = t_itens_carga
      i_notas               = t_notas
      i_de_para             = t_de_para
      i_cancelar_aceite     = _canc_aceite
    IMPORTING
      e_dados_aceite_fiscal = t_aceite
      e_msg_erro            = DATA(lv_msg_erro) ).

  IF lv_msg_erro IS NOT INITIAL.
    MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.

FORM f_corrigir_bordero USING p_aceite TYPE zmme_alv_aceite_fiscal.

*  DATA: lt_dados TYPE zsde_integra_luft_aceite_receb.
*
*  APPEND INITIAL LINE TO lt_dados-pedido ASSIGNING FIELD-SYMBOL(<fs_dados>).
*  <fs_dados>-chave_acesso_nota_fiscal = p_aceite-chave_nfe.
*
*  APPEND INITIAL LINE TO <fs_dados>-itens_recebidos_correcao ASSIGNING FIELD-SYMBOL(<fs_itens>).
*
*  <fs_itens>-codigo_produto_amaggi = p_aceite-matnr.
*  <fs_itens>-id_item_recebimento = p_aceite-id_item_chegada.
*
*  zcl_carga_saida_insumos=>inconsistencia_bordero(
*  EXPORTING
*    i_dados = lt_dados
*  IMPORTING
*    e_msg_erro = DATA(lv_msg_erro) ).
*  IF lv_msg_erro IS NOT INITIAL .
*    MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
*  ENDIF.

ENDFORM.

FORM f_realiza_aceite_fiscal USING p_aceite TYPE zsds391.

  DATA: lt_dados_aceite_zmm0110 TYPE TABLE OF zsds389.

  IF gv_boleto_fase IS INITIAL OR
     "gv_bordero IS INITIAL OR
     gv_termo IS INITIAL.
    MESSAGE 'O Check List de Documentos está incompleto' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP --->>
*  DATA: lva_ebeln_item_nf TYPE ekpo-ebeln,
*        lva_ebelp_item_nf TYPE ekpo-ebelp.
*
*  LOOP AT t_notas ASSIGNING FIELD-SYMBOL(<fs_notas>) WHERE chave_nfe = p_aceite-chave_nfe.
*
*    CLEAR: lva_ebeln_item_nf, lva_ebelp_item_nf.
*
*    LOOP AT t_de_para ASSIGNING FIELD-SYMBOL(<fs_de_para1>) WHERE chave_nfe = <fs_notas>-chave_nfe
*                                                             AND  prod_item = <fs_notas>-prod_item.
*
*      READ TABLE t_itens_carga ASSIGNING FIELD-SYMBOL(<fs_item_carga>) WITH KEY nro_sol  = <fs_de_para1>-nro_sol
*                                                                                seq      = <fs_de_para1>-seq.
*      CHECK sy-subrc IS INITIAL.
*
*      IF lva_ebeln_item_nf IS INITIAL.
*        lva_ebeln_item_nf      = <fs_item_carga>-ebeln.
*        lva_ebelp_item_nf      = <fs_item_carga>-ebelp.
*      ELSEIF NOT ( <fs_item_carga>-ebeln EQ lva_ebeln_item_nf  AND
*                   <fs_item_carga>-ebelp EQ lva_ebelp_item_nf ).
*        MESSAGE |Chave { <fs_notas>-chave_nfe } Item { <fs_notas>-prod_item } possui mais de um Pedido/Item associado no depara! Operação não permitida| TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.
*    ENDLOOP.
*  ENDLOOP.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP <<<--

  LOOP AT t_notas ASSIGNING FIELD-SYMBOL(<fs_notas>) WHERE chave_nfe = p_aceite-chave_nfe.

    READ TABLE t_de_para ASSIGNING FIELD-SYMBOL(<fs_de_para1>) WITH KEY chave_nfe = <fs_notas>-chave_nfe
                                                                        prod_item = <fs_notas>-prod_item.
    CHECK sy-subrc IS INITIAL.

    READ TABLE t_itens_carga ASSIGNING FIELD-SYMBOL(<fs_item_carga>) WITH KEY nro_sol  = <fs_de_para1>-nro_sol
                                                                              seq      = <fs_de_para1>-seq.
    CHECK sy-subrc IS INITIAL.

    APPEND INITIAL LINE TO lt_dados_aceite_zmm0110 ASSIGNING FIELD-SYMBOL(<fs_aceite_nfe_zmm0110>).

    <fs_aceite_nfe_zmm0110>-chave_nfe  = <fs_notas>-chave_nfe.
    <fs_aceite_nfe_zmm0110>-prod_item  = <fs_notas>-prod_item.
    <fs_aceite_nfe_zmm0110>-ebeln      = <fs_item_carga>-ebeln.
    <fs_aceite_nfe_zmm0110>-ebelp      = <fs_item_carga>-ebelp.

    SELECT SINGLE lgort
      FROM ekpo INTO <fs_aceite_nfe_zmm0110>-lgort
     WHERE ebeln EQ <fs_item_carga>-ebeln
       AND ebelp EQ <fs_item_carga>-ebelp.

    IF sy-subrc NE 0 OR <fs_aceite_nfe_zmm0110>-lgort IS INITIAL.
      MESSAGE |Não foi possivel identificar o deposito do Pedido { <fs_item_carga>-ebeln } item { <fs_item_carga>-ebelp }| TYPE 'S'.
      RETURN.
    ENDIF.

    <fs_aceite_nfe_zmm0110>-matnr      = <fs_item_carga>-matnr.
    <fs_aceite_nfe_zmm0110>-meins      = <fs_item_carga>-meins.

    LOOP AT t_de_para ASSIGNING <fs_de_para1> WHERE chave_nfe = <fs_notas>-chave_nfe
                                                AND prod_item = <fs_notas>-prod_item.
      ADD <fs_de_para1>-quantidade TO <fs_aceite_nfe_zmm0110>-quantidade.
    ENDLOOP.

  ENDLOOP.

  zcl_carga_saida_insumos=>aceite_fiscal(
    EXPORTING
      i_nro_cg = gv_carga
      i_notas  = lt_dados_aceite_zmm0110
    IMPORTING
      e_msg_erro = DATA(lv_msg_erro) ).

  IF lv_msg_erro IS NOT INITIAL.
    MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.

FORM f_cancela_aceite_fiscal  USING p_aceite TYPE zmme_alv_aceite_fiscal.

  zcl_carga_saida_insumos=>cancelar_aceite(
    EXPORTING
      i_chave_nfe = p_aceite-chave_nfe
      i_nro_cg    = gv_carga
    IMPORTING
      e_msg_erro = DATA(lv_msg_erro) ).

  IF lv_msg_erro IS NOT INITIAL.
    MESSAGE lv_msg_erro TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

ENDFORM.

FORM f_conferir_carga .

  DATA(_embarque_luft_filial)  = zcl_carga_saida_insumos=>get_embarque_luft( EXPORTING i_nro_cg = gv_carga ).
  DATA(_embarque_armazem)      = zcl_carga_saida_insumos=>check_embarque_armazem( EXPORTING i_nro_cg = gv_carga ).

  IF gv_boleto_fase IS INITIAL OR
     "gv_bordero IS INITIAL OR
     gv_termo IS INITIAL.
    MESSAGE 'O Check List de Documentos está incompleto' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF _embarque_luft_filial EQ abap_true.
    DATA(_msg_error) = zcl_carga_saida_insumos=>conferir_carga_sem_nf( EXPORTING i_nro_carga = gv_carga ).
  ELSEIF _embarque_armazem EQ abap_true.
    _msg_error = zcl_carga_saida_insumos=>conferir_carga_com_nf( EXPORTING i_nro_carga = gv_carga ).
  ELSE.
    RETURN.
  ENDIF.

  IF _msg_error IS NOT INITIAL.
    MESSAGE _msg_error TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  LEAVE TO SCREEN 0.

ENDFORM.

FORM f_anular_conferencia .

  DATA(_msg_error) =
  zcl_carga_saida_insumos=>anular_conferencia( EXPORTING i_nro_cg = gv_carga ).

  IF _msg_error IS NOT INITIAL.
    MESSAGE _msg_error TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

ENDFORM.

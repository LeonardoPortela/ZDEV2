*----------------------------------------------------------------------*
***INCLUDE ZSDR0060_7200.
*----------------------------------------------------------------------*


FORM f_build_fc_7200_01 USING p_estrutura TYPE dd02l-tabname
                     CHANGING ch_fieldcat TYPE lvc_t_fcat.

  PERFORM f_fill_fieldcatalog TABLES ch_fieldcat USING:                   "Opt                          "Just
          01 'NRO_SOL'       'ZSDT0082'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Nro Sol.',
          02 'VBELN'         'ZSDT0082'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Ordem',
          03 'POSNR'         'ZSDT0082'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '06' ' '      'Item',
          04 'AUART'         'ZSDT0082'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '05' ' '      'Tp.OV',
          05 'VKBUR'         'ZSDT0082'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Esc.Vendas',
          06 'VKORG'         'ZSDT0082'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Org.Vendas',
          07 'SPART'         'ZSDT0082'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '09' ' '      'Setor.Atv',
          08 'WERKS'         'ZSDT0082'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '12' ' '      'Centro Forn.',
          09 'MATNR'         'MARA'      ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Material',
          10 'MAKTX'         'MAKT'      ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '15' ' '      'Descrição',
          11 'SALDO'         ''          ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '20' ' '      'Saldo à Formar Carga',
          12 'QTD_VINC'      'ZSDT0131'  ' '    ' '  ' '  'X'  ' '  ' '   ''     ' '   ' '   ' '   '15' ' '      'Qtd.Vinc.Carga',
          13 'MEINS'         'VBAP'      ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '04' ' '      'U.M.',

          "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New --->>>
          14 'ORIGEM_ESTOQUE' 'ZSDT0082' ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '14' 'C'      'Origem Estoque',
          "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New <<----

          "PC
          15 'LIFNR'         'ZSDT0132'  'C310' ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Fornecedor',
          16 'NAME1_LIFNR'   'LFA1'      'C310' ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Ds.Fornecedor',
          17 'COD_LOC_EMB'   'ZSDT0132'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Roteiro PC',
          18 'LOCAL_EMBARQ'  'ZSDT0132'  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Ds.Roteiro PC',
**<<<------"169508 - NMS - INI------>>>
          18 'CITY_LOC_EMB'  'ZSDT0132'  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Município PC',
          18 'TXROT_LOC_EMB' 'ZSDS381'   ' '    ' '  ' '  ' '  ' '  ' '   ' '    ' '   ' '   ' '   '10' 'C'      'Roteiro PC',
**<<<------"169508 - NMS - FIM------>>>
          "LR
          19 'KUNNR'         'ZSDT0132'  'C310' ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Cliente',
          20 'NAME1_KUNNR'   'KNA1'      'C310' ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Ds.Cliente',
          21 'NR_ROT'        'ZSDT0132'  ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Roteiro LR',
          22 'DS_ROT_LR'     'ZSDT0132'  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Ds.Roteiro LR',
**<<<------"169508 - NMS - INI------>>>
          22 'CITY_ROT_LR'   'ZSDT0132'  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Município LR',
          22 'TXROT_ROT_LR'  'ZSDS381'   ' '    ' '  ' '  ' '  ' '  ' '   ' '    ' '   ' '   ' '   '10' 'C'      'Roteiro LR',
**<<<------"169508 - NMS - FIM------>>>
          23 'DT_ENTREGA'    ''          ' '    ' '  ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Dt.Entrega'.

ENDFORM.


FORM f_build_fc_7200_02 USING p_estrutura TYPE dd02l-tabname
                     CHANGING ch_fieldcat TYPE lvc_t_fcat.

  PERFORM f_fill_fieldcatalog2 TABLES ch_fieldcat USING:                       "Opt                         "Just
          01 'ICON_STATUS'              ' '  ' '    ' '    ' '  ' '  'X'  ' '   ''     ' '   ' '   ' '   '06' ' '  'Status'    ' ',
          02 'DS_STATUS'                ' '  ' '    ' '    ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Ds.Status' ' ',
          02 'DOCS_ENV_CARGUERO'        ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     'X'   ' '   ' '   '10' ' '  'Doc.Disp.Carguero' ' ',
          02 'NRO_CG'                   ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '  'Nro Carga' ' ',
          03 'QTD_TOTAL_KG'             ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '  'Qtd.Tot.Kg' ' ',
          04 'VLR_CARGA'                ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '  'Vlr.Carga' ' ',
          05 'COD_TRANSPORTADORA'       ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '  'Transp.' ' ',
          06 'DESC_TRANSPORTADORA'      ' '  ' '    ' '    ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '09' ' '  'Ds.Transportadora' ' ',
          07 'PRECO_FRETE'              ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '  'Valor Frete' ' ',
          08 'PLACA_CAV'                ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '12' ' '  'Placa Cavalo' ' ',
          09 'PLACA_CAR1'               ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '12' ' '  'Placa Car.1' ' ',
          10 'PLACA_CAR2'               ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '12' ' '  'Placa Car.2' ' ',
          11 'PLACA_CAR3'               ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '12' ' '  'Placa Car.3' ' ',
          12 'MOTORISTA'                ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '  'Motorista' ' ',
          13 'NOME_MOTORISTA'           ' '  ' '    ' '    ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Ds.Motorista' ' ',
          14 'DT_ENTREGA'               ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '14' ' '  'Prv.Dt.Entrega' ' ',
          15 'INCO1'                    ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '08' ' '  'Incoterms' ' ', "FF #169665
          16 'DATA_ATUAL'               ' '  ' '    ' '    ' '  ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '  'Data Carga' ' ',
          17 'INTEGRADO_CARGUERO'       ' '  ' '    ' '    ' '  ' '  'X'  ' '   ''     ' '   ' '   ' '   '17' ' '  'Lote Emb.Carguero' ' ',
          18 'VIAGEM_ID'                ' '  ' '    ' '    ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Id.Viagem Carguero' ' ',
          18 'ID_CARGA_SAFRA_CONTROL'   ' '  ' '    ' '    ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '16' ' '  'Id.Safra Control' ' ',
          18 'NRO_PEDIDO_LUFT'          ' '  ' '    ' '    ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '16' ' '  'Nro Pedido Luft' ' ',
"FF #169665 - inicio
          19 'QTD_BAGS'                 ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Qtd BAGS de carga' ' ',
          20 'DS_FRETE_POR'             ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Frete por'         ' ',
          21 'DT_ENVIO_COTACAO'         ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Data Carga enviada para cotação' 'DATS',
          22 'DT_FRETE_CONTRATADO'      ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Data Frete Contratado' 'DATS',
          23 'DT_AUTORIZACAO_EMBARQUE'  ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Data Autorizada Embarque' 'DATS',
          24 'DT_TROCA_NF_FONEC'        ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Data Troca Nota Forn x Forn' 'DATS',   "<<<------"169665 - NMS ------->>>
          25 'DT_CARREGA_FORNEC'        ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Data Carregamento Forn' 'DATS',
          26 'DT_CARREGA_CD'            ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Data Carregamento CD' 'DATS',
          27 'STATUS_ENTREGA'           ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '  'Status Entrega' ' ',
          28 'LOCAL_EMBARQ'             ' '  ' '    ' '  ' '  ' '  ' '  ' '   'X'    ' '   ' '   ' '   '15' ' '  'Local Embarq. ' ' '.   "<<<------"169665 - NMS ------->>>
  "FF #169665 - fim


ENDFORM.

MODULE pbo_7201_create_objects OUTPUT.

  IF gob_cc_7200_sol_carga IS NOT INITIAL.
    gob_grid_7200_solicitacoes->refresh_table_display( ).
    RETURN.
  ENDIF.

  FREE: lt_fieldcat,
        lt_f4,
        ls_layout.

  "ls_layout-stylefname = 'CELLTAB'.
  ls_layout-info_fname = 'COLOR'.
  ls_layout-cwidth_opt = 'X'.

  CREATE OBJECT gob_cc_7200_sol_carga
    EXPORTING
      container_name = 'CC_7200_SOLICITACOES'.

  CREATE OBJECT gob_grid_7200_solicitacoes
    EXPORTING
      i_parent = gob_cc_7200_sol_carga.

  CALL METHOD gob_grid_7200_solicitacoes->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gob_grid_7200_solicitacoes->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER:
  lcl_carga_7200_sol=>set_toolbar                 FOR gob_grid_7200_solicitacoes,
  lcl_carga_7200_sol=>get_ucomm                   FOR gob_grid_7200_solicitacoes,
  lcl_carga_7200_sol=>on_data_changed             FOR gob_grid_7200_solicitacoes,
  lcl_carga_7200_sol=>on_data_changed_finished    FOR gob_grid_7200_solicitacoes,
  lcl_carga_7200_sol=>on_f4                       FOR gob_grid_7200_solicitacoes.


  PERFORM f_fill_fieldcatalog TABLES lt_fieldcat USING:                    "Opt                          "Just
        01 'NRO_SOL'              'ZSDT0082'  ' '      ' '  'C310' ' '  ' '  ' '   ''     ' '   ' '   'X'   '10' ' '      'Nro Sol.',
        01 'SEQ'                  'ZSDT0082'  ' '      ' '  ''     ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Seq.',
        02 'VBELN'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Ordem',
        03 'POSNR'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '06' ' '      'Item',
        04 'AUART'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '05' ' '      'Tp.OV',
        05 'VKBUR'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Esc.Vendas',
        06 'VKORG'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Org.Vendas',
        07 'SPART'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '09' ' '      'Setor.Atv',
        08 'WERKS'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '12' ' '      'Centro Forn.',
        09 'MATNR'                'MARA'      ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Material',
        10 'MAKTX'                'MAKT'      ' '      ' '  ' '    ' '  ' '  ' '   'X'    ' '   ' '   ' '   '15' ' '      'Descrição',
        11 'SALDO'                ''          ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '20' ' '      'Saldo à Formar Carga',
        12 'QTD_VINC'             'ZSDT0131'  ' '      ' '  ' '    'X'  ' '  ' '   ''     ' '   'X'   ' '   '15' ' '      'Qtd.Vinc.Carga',
        13 'QTD_VINC_KG'          ''          ' '      ' '  ' '    ' '  ' '  ' '   ' '    ' '   'X'   ' '   '17' ' '      'Qtd.Vinc.Carga KG',
        14 'SEQ_ENTREGA'          ''           ''      ' '  ' '    'X'  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Seq Entrega',
        15 'MEINS'                'VBAP'      ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '04' ' '      'U.M.',


        16 'EBELN'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Pedido',
        17 'EBELP'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '07' ' '      'It.Ped.',
        18 'FLEXIBILIDADE'        'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '07' 'C'      'Flexib.',
        18 'CHARG'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Lote',
        19 'MARCA'                'ZSDT0082'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Marca',

        "LR
        20 'KUNNR'                'ZSDT0132'  'C310'   ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Cliente',
        21 'NAME1_KUNNR'          'KNA1'      'C310'   ' '  ' '    ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Ds.Cliente',
        22 'NR_ROT'               'ZSDT0132'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Roteiro LR',
        23 'DS_ROT_LR'            'ZSDT0132'  ' '      ' '  ' '    ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Ds.Roteiro LR',
        24 'DT_ENTREGA'           ''          ' '    ' '  ' '  '    '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '       'Dt.Entrega Prevista',
        25 'DT_ENTREGA_EFETIVA'   ''          ' '    ' '  ' '  '    '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '       'Dt.Entrega Efetiva'.


  ls_f4-fieldname  = 'NRO_SOL'.
  ls_f4-register   = 'X'.
  APPEND ls_f4 TO lt_f4.

  CALL METHOD gob_grid_7200_solicitacoes->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4.

  CALL METHOD gob_grid_7200_solicitacoes->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

  CALL METHOD gob_grid_7200_solicitacoes->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = lt_fieldcat
      it_outtab       = git_7200_solicitacoes.

ENDMODULE.


MODULE pbo_7203_create_objects OUTPUT.

  IF gob_cc_7200_bordero IS NOT INITIAL.
    gob_grid_7200_bordero->refresh_table_display( ).
    RETURN.
  ENDIF.

  FREE: lt_fieldcat,
        lt_f4,
        ls_layout.

  "ls_layout-cwidth_opt = 'X'.

  CREATE OBJECT gob_cc_7200_bordero
    EXPORTING
      container_name = 'CC_7200_BORDERO'.

  CREATE OBJECT gob_grid_7200_bordero
    EXPORTING
      i_parent = gob_cc_7200_bordero.

  CALL METHOD gob_grid_7200_bordero->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gob_grid_7200_bordero->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER:
  lcl_bordero_7200=>set_toolbar     FOR gob_grid_7200_bordero,
  lcl_bordero_7200=>get_ucomm       FOR gob_grid_7200_bordero.

  PERFORM f_fill_fieldcatalog TABLES lt_fieldcat USING:                    "Opt                          "Just
        01 'ID_ITEM'              ' '     ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '07' ' '      'Id.Item',
        02 'MATNR'                'MARA'  ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Material',
        03 'DS_PRODUTO'           ' '     ' '      ' '  ' '    ' '  ' '  ' '   'X'    ' '   ' '   ' '   '30' ' '      'Ds.Produto',
        04 'LOTE'                 ' '     ' '      ' '  ' '    ' '  ' '  ' '   'X'    ' '   ' '   ' '   '15' ' '      'Lote',
        04 'LOTE_FORNECEDOR'      ' '     ' '      ' '  ' '    ' '  ' '  ' '   'X'    ' '   ' '   ' '   '15' ' '      'Lote Fornecedor',
        05 'QUANTIDADE'           ' '     ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '13' ' '      'Quantidade',
        06 'EMBALAGEM'            ' '     ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '06' ' '      'U.M',
        07 'DATE_CREATE'          ' '     ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '22' ' '      'Dt.Recebimento Bordero',
        08 'TIME_CREATE'          ' '     ' '      ' '  ' '    ' '  ' '  ' '   ''     ' '   ' '   ' '   '22' ' '      'Hr.Recebimento Bordero'.

  CALL METHOD gob_grid_7200_bordero->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = lt_fieldcat
      it_outtab       = git_7200_bordero.

ENDMODULE.


MODULE pbo_7204_create_objects OUTPUT.

  IF gob_cc_7200_lotes IS NOT INITIAL.
    gob_grid_7200_ov_lotes->refresh_table_display( ).
    gob_grid_7200_lotes->refresh_table_display( ).
    RETURN.
  ENDIF.

  CREATE OBJECT gob_cc_7200_lotes
    EXPORTING
      container_name              = 'CC_7200_LOTES'
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5.

  CREATE OBJECT gob_splitter_lotes_7200
    EXPORTING
      parent  = gob_cc_7200_lotes
      rows    = 1
      columns = 2.

  CALL METHOD gob_splitter_lotes_7200->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = gob_parent_ov_lotes_7200.

  CALL METHOD gob_splitter_lotes_7200->get_container
    EXPORTING
      row       = 1
      column    = 2
    RECEIVING
      container = gob_parent_inf_lotes_7200.


  CALL METHOD gob_splitter_lotes_7200->set_column_mode
    EXPORTING
      mode = gob_splitter_lotes_7200->mode_relative.

  CALL METHOD gob_splitter_lotes_7200->set_column_width
    EXPORTING
      id    = 1
      width = 50.

  CALL METHOD gob_splitter_lotes_7200->set_column_width
    EXPORTING
      id    = 2
      width = 50.

  "ALV Ordems Lote
  FREE: lt_fieldcat,
        lt_f4,
        ls_layout.

  CREATE OBJECT gob_grid_7200_ov_lotes
    EXPORTING
      i_parent = gob_parent_ov_lotes_7200.

  CALL METHOD gob_grid_7200_ov_lotes->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gob_grid_7200_ov_lotes->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER:
      lcl_carga_7200_ov_lotes=>on_double_click_ordem FOR gob_grid_7200_ov_lotes,
      lcl_carga_7200_ov_lotes=>user_command FOR gob_grid_7200_ov_lotes,
      lcl_carga_7200_ov_lotes=>toolbar_ordem FOR gob_grid_7200_ov_lotes.

  PERFORM f_fill_fieldcatalog TABLES lt_fieldcat USING:                             "Opt                          "Just
       03 'VBELN'               ''          ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   ' '   ' '   '05' ' '      'Ordem Venda',
       04 'POSNR'               ''          ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   ' '   ' '   '05' ' '      'Item',
       05 'NR_ROT'              ''          ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   ' '   ' '   '09' ' '      'Roteiro',
       06 'MATNR'               'MARA'      ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   ' '   ' '   '04' ' '      'Material',
       07 'MAKTX'               'MAKTX'     ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   ' '   ' '   '12' ' '      'Desc. Material',
       08 'WERKS'               ''          ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   ' '   ' '   '10' ' '      'Centro F.',
       09 'TOT_VINC_OV'         ''          ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   'X'   ' '   '15' ' '      'Qtd.',
       10 'QTDE_LOTE_VINC_OV'   ''          ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   'X'   ' '   '15' ' '      'Qtd. c/ Lote',
       11 'MEINS'               ''          ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   ' '   ' '   '15' ' '      'U.M.'.

  CALL METHOD gob_grid_7200_ov_lotes->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = lt_fieldcat
      it_outtab       = git_7200_ov_lotes.


  "ALV Lotes
  FREE: lt_fieldcat,
        lt_f4,
        ls_layout.

  CREATE OBJECT gob_grid_7200_lotes
    EXPORTING
      i_parent = gob_parent_inf_lotes_7200.

  CALL METHOD gob_grid_7200_lotes->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gob_grid_7200_lotes->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER:
      lcl_carga_7200_lotes=>toolbar                    FOR gob_grid_7200_lotes,
      lcl_carga_7200_lotes=>user_command               FOR gob_grid_7200_lotes,
      lcl_carga_7200_lotes=>data_changed_finished      FOR gob_grid_7200_lotes,
      lcl_carga_7200_lotes=>on_f4                      FOR gob_grid_7200_lotes.

  PERFORM f_fill_fieldcatalog TABLES lt_fieldcat USING:                             "Opt                          "Just
       02 'VBELN'                ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '11' ' '      'Ordem Venda',
       03 'POSNR'                ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '05' ' '      'Item',
       04 'NR_ROT'              ''          ' '      ' '   ' '      ' '  ' '  ' '   'X'    ' '   ' '   ' '   '09' ' '      'Roteiro',
       05 'CHARG'                ''          ' '      ' '   'C310'   ' '  ' '  ' '   'X'   ' '   ' '   'X'   '05' ' '      'Lote',
       06 'CATEGORIA'            ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '09' ' '      'Categoria',
       07 'NR_FASE'              'ZSDT0134'  ' '      ' '   'C310'   ' '  ' '  ' '   'X'   ' '   ' '   'X'   '04' ' '      'Fase',
       "08 'EBELN'                ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '09' ' '      'Pedido',
       09 'LFIMG'                'ZSDT0134'  ' '      ' '   ' '      'X'  ' '  ' '   ''    ' '   'X'   ' '   '12' ' '      'Quantidade',
       10 'BRGEW'                'ZSDT0134'  ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '10' ' '      'Peso Unit.',
       11 'PESO_LIQ_BRT'         'ZSDT0134'  ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   'X'   ' '   '15' ' '      'Peso Bruto/Liq.'.

  ls_f4-fieldname  = 'CHARG'.
  ls_f4-register   = 'X'.
  APPEND ls_f4 TO lt_f4.

  ls_f4-fieldname  = 'NR_FASE'.
  ls_f4-register   = 'X'.
  APPEND ls_f4 TO lt_f4.

  CALL METHOD gob_grid_7200_lotes->register_f4_for_fields
    EXPORTING
      it_f4 = lt_f4.

  CALL METHOD gob_grid_7200_lotes->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

  CALL METHOD gob_grid_7200_lotes->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = lt_fieldcat
      it_outtab       = git_7200_lotes.

ENDMODULE.


MODULE pbo_7205_create_objects OUTPUT.

  IF gob_cc_7200_romaneios IS NOT INITIAL.
    gob_grid_7200_romaneios->refresh_table_display( ).
    RETURN.
  ENDIF.

  FREE: lt_fieldcat,
        lt_f4,
        ls_layout.

  ls_layout-info_fname = 'COLOR'.

  CREATE OBJECT gob_cc_7200_romaneios
    EXPORTING
      container_name = 'CC_7200_ROMANEIOS'.

  CREATE OBJECT gob_grid_7200_romaneios
    EXPORTING
      i_parent = gob_cc_7200_romaneios.

  CALL METHOD gob_grid_7200_romaneios->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gob_grid_7200_romaneios->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER:
      lcl_carga_7200_romaneios=>toolbar         FOR gob_grid_7200_romaneios,
      lcl_carga_7200_romaneios=>user_command    FOR gob_grid_7200_romaneios.

  PERFORM f_fill_fieldcatalog TABLES lt_fieldcat USING:                             "Opt                          "Just
       04 'CH_REFERENCIA'        ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '16' ' '      'Chave Referência',
       06 'VBELN'                ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '11' ' '      'Ordem Venda',
       05 'NR_ROMANEIO'          ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '11' ' '      'Nr.Romaneio',
       06 'BRANCH'               ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '06' ' '      'Filial',
       07 'DT_MOVIMENTO'         ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '12' ' '      'Dt.Movimento',
       08 'DOC_REM'              ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '10' ' '      'Remessa',
       09 'FATURA_PROD'          ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '10' ' '      'Fat.NF-e',
       10 'NRO_NF_PROD'          ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '10' ' '      'Danfe',
       11 'DOC_TRANSP'           ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '11' ' '      'Doc.Transp.',
       12 'FKNUM'                ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '10' ' '      'Doc.Custo',
       13 'OV_FRETE'             ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '10' ' '      'OV.Frete',
       14 'FATURA_FRETE'         ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '10' ' '      'Fat.Frete',
       15 'NRO_NF_FRETE'         ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '10' ' '      'DACTE',
       16 'ICON_STATUS'          ''          ' '      ' '   ' '      ' '  'X'  ' '   ''    ' '   ' '   ' '   '10' 'C'      'Finalizado',
       17 'DT_SAIDA_CD'          ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '11' ' '      'Dt.Saida CD',
       18 'HR_SAIDA_CD'          ''          ' '      ' '   ' '      ' '  ' '  ' '   ''    ' '   ' '   ' '   '11' ' '      'Hr.Saida CD'.

  CALL METHOD gob_grid_7200_romaneios->set_ready_for_input
    EXPORTING
      i_ready_for_input = 0.

  CALL METHOD gob_grid_7200_romaneios->set_table_for_first_display
    EXPORTING
      is_layout       = ls_layout
    CHANGING
      it_fieldcatalog = lt_fieldcat
      it_outtab       = git_7200_romaneios.

ENDMODULE.

MODULE pbo_7202_create_objects OUTPUT.


  IF gob_cc_7200_nf_venda IS NOT INITIAL OR gob_cc_7200_nf_transf_forn IS NOT INITIAL.
    gob_grid_7200_nf_venda->refresh_table_display( ).
    gob_grid_7200_nf_transf_forn->refresh_table_display( ).
    RETURN.
  ENDIF.

  FREE: lt_excluded.

  APPEND cl_gui_alv_grid=>mc_fc_sum TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_find TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_graph TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_help TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_info TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_filter TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_maximum TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_minimum TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_sort TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_sort_asc TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_sort_dsc TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_refresh TO  lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_detail TO  lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_print TO lt_excluded.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row TO lt_excluded.

*-----------------------------------------------------------------------------------------------------------------*
*  Instancia Objetos ALV Notas de Venda
*-----------------------------------------------------------------------------------------------------------------*

  FREE: lt_fieldcat,
        lt_f4,
        ls_layout.

  CREATE OBJECT gob_cc_7200_nf_venda
    EXPORTING
      container_name = 'CONTAINER_NOTAS1'.

  CREATE OBJECT gob_grid_7200_nf_venda
    EXPORTING
      i_parent = gob_cc_7200_nf_venda.

  CALL METHOD gob_grid_7200_nf_venda->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gob_grid_7200_nf_venda->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER:
  lcl_notas_venda_7200=>set_toolbar     FOR gob_grid_7200_nf_venda,
  lcl_notas_venda_7200=>get_ucomm       FOR gob_grid_7200_nf_venda,
  lcl_notas_venda_7200=>on_data_changed FOR gob_grid_7200_nf_venda.

  PERFORM f_fill_fieldcatalog TABLES lt_fieldcat USING:  "Edit           "Opt                          "Just
        01 'CHAVE_NFE'     'ZSDT0410'  ' '      ' '  ' ' 'X'  ' '  ' '   ''     ' '   ' '   ' '   '44' ' '      'Chave NF-e'.

  "ls_layout-stylefname = 'CELLTAB'.
  ls_layout-grid_title = 'Notas de venda do Fornecedor'.
  "ls_layout-cwidth_opt = 'X'.

  CALL METHOD gob_grid_7200_nf_venda->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = lt_excluded
      is_layout            = ls_layout
    CHANGING
      it_fieldcatalog      = lt_fieldcat
      it_outtab            = git_7200_notas_venda.


*-----------------------------------------------------------------------------------------------------------------*
*  Instancia Objetos ALV Notas de Transferencia
*-----------------------------------------------------------------------------------------------------------------*

  FREE: lt_fieldcat,
        lt_f4,
        ls_layout.

  CREATE OBJECT gob_cc_7200_nf_transf_forn
    EXPORTING
      container_name = 'CONTAINER_NOTAS2'.

  CREATE OBJECT gob_grid_7200_nf_transf_forn
    EXPORTING
      i_parent = gob_cc_7200_nf_transf_forn.

  CALL METHOD gob_grid_7200_nf_transf_forn->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_modified.

  CALL METHOD gob_grid_7200_nf_transf_forn->register_edit_event
    EXPORTING
      i_event_id = cl_gui_alv_grid=>mc_evt_enter.

  SET HANDLER:
    lcl_notas_transf_7200=>set_toolbar     FOR gob_grid_7200_nf_transf_forn,
    lcl_notas_transf_7200=>get_ucomm       FOR gob_grid_7200_nf_transf_forn,
    lcl_notas_transf_7200=>on_data_changed FOR gob_grid_7200_nf_transf_forn.

  PERFORM f_fill_fieldcatalog TABLES lt_fieldcat USING:  "Edit           "Opt                          "Just
        01 'CHAVE_NFE'     'ZSDT0410'  ' '      ' '  ' ' 'X'  ' '  ' '   ''     ' '   ' '   ' '   '44' ' '      'Chave NF-e',
        02 'DATA_NFE'      'ZSDT0410'  ' '      ' '  ' ' 'X'  ' '  ' '   ''     ' '   ' '   ' '   '10' ' '      'Dt.Emissão',
        03 'CHAVE_CTE'     'ZSDT0410'  ' '      ' '  ' ' 'X'  ' '  ' '   ''     ' '   ' '   ' '   '44' ' '      'Chave CT-e'.


  ls_layout-grid_title = 'Notas de transferência entre o Fornecedor'.
  "ls_layout-cwidth_opt = 'X'.

  CALL METHOD gob_grid_7200_nf_transf_forn->set_table_for_first_display
    EXPORTING
      it_toolbar_excluding = lt_excluded
      is_layout            = ls_layout
    CHANGING
      it_fieldcatalog      = lt_fieldcat
      it_outtab            = git_7200_notas_transf.

ENDMODULE.



FORM f_refresh_inf_carga_7200 USING p_nro_cg  TYPE zsdt0133-nro_cg
                                    p_load_bd.

  IF p_load_bd EQ abap_true.

    DATA(lra_nro_carga) = VALUE zsdt_range_nro_cg( ( sign = 'I' option = 'EQ' low = p_nro_cg ) ).

    "Recarregar Informaçoes
    CLEAR: gwa_cab_carga_saida,
           git_7200_solicitacoes[],
           git_7200_notas_venda[],
           git_7200_notas_transf[],
           git_7200_ov_lotes[],
           git_7200_lotes[],
           git_7200_romaneios[],
           git_7200_bordero[].

    zcl_carga_saida_insumos=>busca_dados_carga(
      EXPORTING
        i_nr_carga     =  lra_nro_carga
      IMPORTING
        e_cargas       = DATA(lit_carga)
        e_solicitacoes = DATA(lit_solicitacoes)
        e_notas_venda  = DATA(lit_notas_venda)
        e_notas_transf = DATA(lit_notas_transf)
        e_ov_lotes     = DATA(lit_ov_lotes)
        e_lotes        = DATA(lit_lotes)
        e_romaneios    = DATA(lit_romaneios)
        e_bordero      = DATA(lit_bordero) ).

    READ TABLE lit_carga INTO DATA(lwa_carga) INDEX 1.

    CHECK sy-subrc EQ 0 AND lit_carga[] IS NOT INITIAL.

    MOVE-CORRESPONDING lwa_carga TO gwa_cab_carga_saida.
    MOVE-CORRESPONDING lit_solicitacoes[] TO git_7200_solicitacoes[].
    MOVE-CORRESPONDING lit_notas_venda[] TO git_7200_notas_venda[].
    MOVE-CORRESPONDING lit_notas_transf[] TO git_7200_notas_transf[].
    MOVE-CORRESPONDING lit_ov_lotes[] TO git_7200_ov_lotes[].
    MOVE-CORRESPONDING lit_lotes[] TO git_7200_lotes[].
    MOVE-CORRESPONDING lit_romaneios[] TO git_7200_romaneios[].
    MOVE-CORRESPONDING lit_bordero[] TO git_7200_bordero[].

    SORT: git_7200_ov_lotes BY vbeln posnr nr_rot,
          git_7200_lotes    BY vbeln posnr nr_rot.

    IF gv_ucomm EQ c_inf_lotes_7200 AND gva_vbeln_lote_sel IS NOT INITIAL.
      DELETE git_7200_lotes WHERE NOT ( vbeln  EQ gva_vbeln_lote_sel AND
                                        posnr  EQ gva_posnr_lote_sel AND
                                        nr_rot EQ gva_nr_rot_lote_sel ).
    ENDIF.

  ENDIF.

  CASE gv_ucomm.
    WHEN c_cria_carga_7200 OR
         c_edit_solic_7200 OR
         c_edit_frete_7200 OR  "*-US192364-14.10.2025-#192364-JT
         c_edit_log_7200.
      "Presseguir atualizando os dados da tela...
    WHEN OTHERS.
      RETURN. "Não recalcular os dados da tela
  ENDCASE.

  CLEAR: gwa_cab_carga_saida-qtd_total_kg, gwa_cab_carga_saida-dt_entrega.

  LOOP AT git_7200_solicitacoes INTO DATA(lwa_solicitacao).
    gwa_cab_carga_saida-qtd_total_kg = gwa_cab_carga_saida-qtd_total_kg + ( lwa_solicitacao-qtd_vinc * lwa_solicitacao-brgew ).

    IF ( gwa_cab_carga_saida-dt_entrega IS INITIAL ) OR ( lwa_solicitacao-dt_entrega > gwa_cab_carga_saida-dt_entrega ).
      gwa_cab_carga_saida-dt_entrega = lwa_solicitacao-dt_entrega.
    ENDIF.
  ENDLOOP.

  zcl_carga_saida_insumos=>calc_categoria_veiculo(
    EXPORTING
      i_quantidade_kg = CONV #( gwa_cab_carga_saida-qtd_total_kg )
    CHANGING
      c_ctg_transp    = gwa_cab_carga_saida-ctg_transp
      c_ctg_transp_d  = gwa_cab_carga_saida-ctg_transp_d  ).

  DATA(_integrado_carguero) = abap_false.
  SELECT SINGLE *
    FROM zlest0181 INTO @DATA(lwa_zlest0181)
   WHERE id_lote_frete EQ @gwa_cab_carga_saida-id_lote_frete.

  IF sy-subrc EQ 0 AND lwa_zlest0181-id_carguero IS NOT INITIAL.
    _integrado_carguero = abap_true.
  ENDIF.

  IF _integrado_carguero IS NOT INITIAL.
    gwa_cab_carga_saida-integrado_carguero = icon_led_green.
  ELSE.
    gwa_cab_carga_saida-integrado_carguero = icon_led_yellow.
  ENDIF.

  "Dados Motorista
  CLEAR: gwa_cab_carga_saida-nome_motorista.
  IF gwa_cab_carga_saida-motorista IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_cab_carga_saida-motorista
      IMPORTING
        output = gwa_cab_carga_saida-motorista.

    SELECT SINGLE name1
      FROM lfa1 INTO gwa_cab_carga_saida-nome_motorista
      WHERE lifnr = gwa_cab_carga_saida-motorista.
    IF sy-subrc NE 0.
      MESSAGE |Motorista informado { gwa_cab_carga_saida-motorista } não existe| TYPE 'S'.
      CLEAR: gwa_cab_carga_saida-motorista, gwa_cab_carga_saida-nome_motorista.
    ENDIF.
  ENDIF.

  CLEAR: gwa_cab_carga_saida-desc_transportadora.
  IF gwa_cab_carga_saida-cod_transportadora IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_cab_carga_saida-cod_transportadora
      IMPORTING
        output = gwa_cab_carga_saida-cod_transportadora.

    SELECT SINGLE name1
      FROM lfa1 INTO gwa_cab_carga_saida-desc_transportadora
     WHERE lifnr = gwa_cab_carga_saida-cod_transportadora.

    IF sy-subrc NE 0.
      MESSAGE |Transportada informada { gwa_cab_carga_saida-cod_transportadora } não existe| TYPE 'S'.
      CLEAR: gwa_cab_carga_saida-cod_transportadora, gwa_cab_carga_saida-desc_transportadora.
    ENDIF.

  ENDIF.

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New --->>>
  CLEAR: gwa_cab_carga_saida-rot_desc.
  IF gwa_cab_carga_saida-roteiro_pc IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_cab_carga_saida-roteiro_pc
      IMPORTING
        output = gwa_cab_carga_saida-roteiro_pc.

    SELECT SINGLE rot_desc
      FROM zsdt0132 INTO gwa_cab_carga_saida-rot_desc
      WHERE nr_rot = gwa_cab_carga_saida-roteiro_pc.

    IF sy-subrc NE 0.
      MESSAGE | Roteiro { gwa_cab_carga_saida-roteiro_pc } não existe! | TYPE 'S'.
      CLEAR: gwa_cab_carga_saida-roteiro_pc, gwa_cab_carga_saida-rot_desc.
    ENDIF.
  ENDIF.


  CLEAR: gwa_cab_carga_saida-descricao_pc.
  IF gwa_cab_carga_saida-codigo_pc IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = gwa_cab_carga_saida-codigo_pc
      IMPORTING
        output = gwa_cab_carga_saida-codigo_pc.

    SELECT SINGLE name1
      FROM lfa1 INTO gwa_cab_carga_saida-descricao_pc
     WHERE lifnr = gwa_cab_carga_saida-codigo_pc.

    IF sy-subrc NE 0.
      MESSAGE | Ponto de Coleta { gwa_cab_carga_saida-codigo_pc } não existe! | TYPE 'S'.
      CLEAR: gwa_cab_carga_saida-codigo_pc, gwa_cab_carga_saida-descricao_pc.
    ENDIF.
  ENDIF.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New <<----

ENDFORM.


FORM f_build_inf_carga_7200 USING p_solicitacoes TYPE zsds381_t
                         CHANGING p_error.

  CLEAR: gwa_cab_carga_saida,
         git_7200_solicitacoes[],
         git_7200_notas_venda[],
         git_7200_notas_transf[],
         git_7200_bordero[],
         p_error.

  READ TABLE p_solicitacoes INTO DATA(lwa_solicitacao_sel) INDEX 1.
  CHECK sy-subrc EQ 0 AND p_solicitacoes[] IS NOT INITIAL.

  IF sy-sysid NE 'DEV'.

    "Validar Roteiro PC
    DATA(lit_check_roteiro_pc) = p_solicitacoes[].
    SORT lit_check_roteiro_pc BY nr_rot_pc.
    DELETE ADJACENT DUPLICATES FROM lit_check_roteiro_pc COMPARING nr_rot_pc.

    IF lines( lit_check_roteiro_pc ) > 1.
      p_error = abap_true.
      MESSAGE 'Não pode ser selecionada solicitações com roteiro do ponto de coleta distintos!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lwa_solicitacao_sel-nr_rot_pc IS INITIAL.
      p_error = abap_true.
      MESSAGE |Solicitação { lwa_solicitacao_sel-nro_sol } sem roteiro do ponto de coleta informado!| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New --->>>
    "Validar Origem Estoque
    DATA(lit_check_origem_estoque) = p_solicitacoes[].
    SORT lit_check_origem_estoque BY origem_estoque.
    DELETE ADJACENT DUPLICATES FROM lit_check_origem_estoque COMPARING origem_estoque.

    IF lines( lit_check_origem_estoque ) > 1.
      p_error = abap_true.
      MESSAGE 'Não pode ser selecionada solicitações com origens de estoque distintas!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.


    IF lwa_solicitacao_sel-origem_estoque IS INITIAL.
      p_error = abap_true.
      MESSAGE |Solicitação { lwa_solicitacao_sel-nro_sol } sem origem de estoque informada!| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    "Validar Incoterms
*    DATA(lit_check_inco1) = p_solicitacoes[].
*    SORT lit_check_inco1 BY inco1.
*    DELETE ADJACENT DUPLICATES FROM lit_check_inco1 COMPARING inco1.
*
*    IF lines( lit_check_inco1 ) > 1.
*      p_error = abap_true.
*      MESSAGE 'Não pode ser selecionada solicitações com Incoterms distintos!' TYPE 'S' DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.
*
*
*    IF lwa_solicitacao_sel-inco1 IS INITIAL.
*      p_error = abap_true.
*      MESSAGE |Solicitação { lwa_solicitacao_sel-nro_sol } sem Incoterms informado!| TYPE 'S' DISPLAY LIKE 'E'.
*      RETURN.
*    ENDIF.
    "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New <<----

    "Validar Organização Vendas
    DATA(lit_check_vkorg) = p_solicitacoes[].
    SORT lit_check_vkorg BY vkorg.
    DELETE ADJACENT DUPLICATES FROM lit_check_vkorg COMPARING vkorg.

    IF lines( lit_check_vkorg ) > 1.
      p_error = abap_true.
      MESSAGE 'Não pode ser selecionada solicitações com Organização de Vendas distintas!' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    IF lwa_solicitacao_sel-vkorg IS INITIAL.
      p_error = abap_true.
      MESSAGE |Solicitação { lwa_solicitacao_sel-nro_sol } sem Organização Vendas informada!| TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDIF.


*------------------------------------------------------------------------------------------------------------------*
* Monta Cabeçalho
*------------------------------------------------------------------------------------------------------------------*
  gwa_cab_carga_saida-codigo_pc              = lwa_solicitacao_sel-lifnr.
  gwa_cab_carga_saida-descricao_pc           = lwa_solicitacao_sel-name1_lifnr.
  gwa_cab_carga_saida-roteiro_pc             = lwa_solicitacao_sel-nr_rot_pc.
  gwa_cab_carga_saida-rot_desc               = lwa_solicitacao_sel-local_embarq.

  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New --->>>
  "gwa_cab_carga_saida-inco1                  = lwa_solicitacao_sel-inco1.
  "SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New <<----


  gwa_cab_carga_saida-frete_por_v            = abap_true.
  gwa_cab_carga_saida-integrar_carguero      = abap_true.

*------------------------------------------------------------------------------------------------------------------*
* Monta Solicitações
*------------------------------------------------------------------------------------------------------------------*
  MOVE-CORRESPONDING p_solicitacoes[] TO git_7200_solicitacoes[].

  LOOP AT git_7200_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_sol_sel>).
    <fs_sol_sel>-qtd_vinc_kg = <fs_sol_sel>-qtd_vinc * <fs_sol_sel>-brgew.
  ENDLOOP.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg
                                      abap_false.

ENDFORM.

FORM f_show_alv_mont_carga_sem_sai.

  DATA: lw_layout   TYPE lvc_s_layo,
        lt_fieldcat TYPE lvc_t_fcat,
        lt_events   TYPE slis_t_event,
        lv_title    TYPE lvc_title.

  PERFORM f_build_fc_7200_01 USING 'ZSDS381' CHANGING lt_fieldcat.

  lw_layout-zebra     = abap_true.

  APPEND INITIAL LINE TO lt_events ASSIGNING FIELD-SYMBOL(<fs_events>).

  <fs_events>-form = 'XPF_STATUS_SET_7200'.
  <fs_events>-name = slis_ev_pf_status_set.

  APPEND INITIAL LINE TO lt_events ASSIGNING <fs_events>.

  <fs_events>-form = 'XUSER_COMMAND_7200'.
  <fs_events>-name = slis_ev_user_command.

  CASE abap_true.
    WHEN p_spart2.
      lv_title = 'Fertilizantes'.
    WHEN p_spart3.
      lv_title = 'Defensivos'.
    WHEN p_spart4.
      lv_title = 'Sementes'.
    WHEN OTHERS.
  ENDCASE.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = lw_layout
      i_grid_title       = lv_title
      it_fieldcat_lvc    = lt_fieldcat
      it_events          = lt_events
    TABLES
      t_outtab           = git_monta_carga_saida_sem
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.

ENDFORM.

FORM f_show_alv_list_carga_sem_sai.
  DATA: lw_layout   TYPE lvc_s_layo,
        lt_fieldcat TYPE lvc_t_fcat,
        lt_events   TYPE slis_t_event.

  CHECK git_lista_carga_saida_sem IS NOT INITIAL.

  PERFORM f_build_fc_7200_02 USING 'ZSDS382' CHANGING lt_fieldcat.

  lw_layout-zebra = abap_true.
  lw_layout-box_fname = 'BOX'.   ""<<<------"169508 - NMS ------->>>

  APPEND INITIAL LINE TO lt_events ASSIGNING FIELD-SYMBOL(<fs_events>).

  <fs_events>-form = 'XPF_STATUS_SET_7200_02'.
  <fs_events>-name = slis_ev_pf_status_set.

  APPEND INITIAL LINE TO lt_events ASSIGNING <fs_events>.

  <fs_events>-form = 'XUSER_COMMAND_7200_02'.
  <fs_events>-name = slis_ev_user_command.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY_LVC'
    EXPORTING
      i_callback_program = sy-repid
      is_layout_lvc      = lw_layout
      it_fieldcat_lvc    = lt_fieldcat
      it_events          = lt_events
    TABLES
      t_outtab           = git_lista_carga_saida_sem
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
ENDFORM.

FORM xuser_command_7200 USING ucomm    LIKE sy-ucomm
                              selfield TYPE kkblo_selfield.

  DATA: lw_sel_hide TYPE slis_sel_hide_alv,
        lt_sel_rows TYPE lvc_t_row,
        lw_sel_rows TYPE lvc_s_row.

  DATA: lit_sol_selecionadas TYPE zsds381_t.

  CLEAR: lit_sol_selecionadas[].

  REFRESH it_values.

  gv_ucomm = sy-ucomm.

  CASE ucomm.

    WHEN c_determinar_pc_7200.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = lw_sel_hide
          e_grid      = ref_7200.

      CALL METHOD ref_7200->get_selected_rows
        IMPORTING
          et_index_rows = git_sel_rows_7200.

      IF git_sel_rows_7200 IS INITIAL.
        MESSAGE 'Favor selecionar ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      CLEAR: gwa_set_ponto_coleta_7200.

      CALL SCREEN 7210 STARTING AT 02 02.

    WHEN c_cria_carga_7200.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = lw_sel_hide
          e_grid      = ref_7200.

      CALL METHOD ref_7200->get_selected_rows
        IMPORTING
          et_index_rows = lt_sel_rows.


      CASE sy-sysid.
        WHEN 'DEV'.
          APPEND INITIAL LINE TO lit_sol_selecionadas ASSIGNING FIELD-SYMBOL(<fs_sol_selecionada>).
          <fs_sol_selecionada>-nro_sol = '999999'.
          <fs_sol_selecionada>-vbeln   = '8787878787'.
        WHEN OTHERS.

          IF lt_sel_rows IS INITIAL.
            MESSAGE 'Favor selecionar ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

          LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_rows>).
            READ TABLE git_monta_carga_saida_sem INTO DATA(lwa_saida_mont_carga) INDEX <fs_rows>-index.
            CHECK sy-subrc EQ 0.

            APPEND INITIAL LINE TO lit_sol_selecionadas ASSIGNING <fs_sol_selecionada>.
            MOVE-CORRESPONDING lwa_saida_mont_carga TO <fs_sol_selecionada>.
          ENDLOOP.

      ENDCASE.

      DATA(_error) = abap_false.
      PERFORM f_build_inf_carga_7200 USING lit_sol_selecionadas CHANGING _error.
      CHECK _error EQ abap_false.

      CALL SCREEN '7200'.
**<<<------"169508 - NMS - INI------>>>
    WHEN c_duplo_click_7200. "Duplo click na Grid ALV.
      DATA(cll_carga_saida_insumos) = NEW zcl_carga_saida_insumos( ).

      READ TABLE git_monta_carga_saida_sem INTO lwa_saida_mont_carga INDEX selfield-tabindex.

      CASE selfield-fieldname.
        WHEN 'TXROT_LOC_EMB'. "Texto roteiro Local de Embarque (Ponto de Coleta)
* Exibe texto do Roteiro ou Valida texto Roteiro - Local de Embarque (Ponto de Coleta)
          DATA(vl_valor) = cll_carga_saida_insumos->get_texto_roteito( i_name    = CONV #( lwa_saida_mont_carga-nr_rot_pc )
                                                                       i_tp_exec = CONV #( sy-abcde+4(1) )         "E - Exibir Texto
                                                                      ).

        WHEN 'TXROT_ROT_LR'.  "Texto roteiro Local de Retirada
* Exibe texto do Roteiro ou Valida texto Roteiro - Local de Retirada.
          vl_valor = cll_carga_saida_insumos->get_texto_roteito( i_name    = CONV #( lwa_saida_mont_carga-nr_rot )
                                                                 i_tp_exec = CONV #( sy-abcde+4(1) )               "E - Exibir Texto
                                                                ).

        WHEN OTHERS.
*         Do nothing
      ENDCASE.
**<<<------"169508 - NMS - FIM------>>>
  ENDCASE.

ENDFORM.

FORM xuser_command_7200_02 USING ucomm    LIKE sy-ucomm
                                 selfield TYPE kkblo_selfield.

  DATA: lw_sel_hide TYPE slis_sel_hide_alv,
        lt_sel_rows TYPE lvc_t_row.

  DATA: ls_stable   TYPE lvc_s_stbl.

  CASE ucomm.

    WHEN c_refresh_lista_7200.

      CLEAR: git_lista_carga_saida_sem[].

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = lw_sel_hide
          e_grid      = ref_7200_02.

      zcl_carga_saida_insumos=>busca_dados_carga(
        EXPORTING
          i_nr_carga    =  s_carga[]
          i_vkorg       =  s_vkorg[]
          i_vkbur       =  s_vkbur[]
          i_spart       =  CONV #( gv_spart )
          i_kunnr       =  s_kunnr[]
          i_nro_sol     =  s_nrsol[]
          i_id_viagem   =  s_idvgm[]
          i_id_carga_safra_ctrl = s_id_sf[]
          i_ordem_venda =  s_ordem[]
          i_dt_carga    =  s_dtcar[]
       IMPORTING
          e_cargas      =  git_lista_carga_saida_sem ).


      ls_stable-row = 'X'.
      ls_stable-col = 'X'.


      ref_7200_02->refresh_table_display(
        EXPORTING
          is_stable = ls_stable
       ).

      IF git_lista_carga_saida_sem[] IS INITIAL.
        MESSAGE 'Nenhuma registro encontrado com os parametros de seleção!' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

    WHEN c_view_carga_7200.

      gv_ucomm = c_view_carga_7200.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = lw_sel_hide
          e_grid      = ref_7200_02.

      CALL METHOD ref_7200_02->get_selected_rows
        IMPORTING
          et_index_rows = lt_sel_rows.

      IF lt_sel_rows IS INITIAL.
        MESSAGE 'Favor selecionar ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      IF lines( lt_sel_rows ) > 1.
        MESSAGE 'Favor selecionar apenas uma linha' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      READ TABLE lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_rows>) INDEX 1.
      CHECK sy-subrc IS INITIAL.

      READ TABLE git_lista_carga_saida_sem ASSIGNING FIELD-SYMBOL(<fs_alv_lista_carga>) INDEX <fs_rows>-index.
      CHECK sy-subrc IS INITIAL.

      PERFORM f_refresh_inf_carga_7200 USING <fs_alv_lista_carga>-nro_cg abap_true.

      tabstrip_7200-activetab = 'PUSH1'.

      CALL SCREEN '7200'.

      selfield-refresh = 'X'.

    WHEN 'REFRESH'.

      PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.
      selfield-refresh = 'X'.
**<<<------"169508 - NMS - INI------>>>
    WHEN c_exibe_legenda_7200.
      DATA(cll_carga_saida_insumos) = NEW zcl_carga_saida_insumos( ).
      cll_carga_saida_insumos->lista_status_carga( ).

    WHEN c_gera_cotacao_7200.   "Gerar Cotação de Frete
      IF ref_7200_02 IS BOUND.
        FREE ref_7200_02.
        CLEAR lt_sel_rows.

      ENDIF.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = lw_sel_hide
          e_grid      = ref_7200_02.

      IF ref_7200_02 IS BOUND.
        CALL METHOD ref_7200_02->get_selected_rows
          IMPORTING
            et_index_rows = lt_sel_rows.

      ENDIF.

      IF lt_sel_rows IS INITIAL.
        MESSAGE 'Favor selecionar ao menos uma linha' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.

      ENDIF.
* Gera Cotação de frete.
      PERFORM f_gera_cotacao_7200 TABLES lt_sel_rows.
**<<<------"169508 - NMS - FIM------>>>
  ENDCASE.

ENDFORM.



FORM f_start_select_saida_spart_04.

  DATA: lva_msg_error TYPE string.

  "PERFORM f_limpa_variaveis.

  IF s_vkorg IS INITIAL.
    MESSAGE 'Campo Organização Vendas é obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CASE abap_true.
    WHEN p_spart2.
      gv_spart = '02'.
    WHEN p_spart3.
      gv_spart = '03'.
    WHEN p_spart4.
      gv_spart = '04'.
    WHEN OTHERS.
  ENDCASE.

  CASE abap_true.
    WHEN rb_mont. "Montar Carga |===============================================================================================================

*      IF s_inco1 IS INITIAL.
*        MESSAGE 'Incoterms é obrigatório!' TYPE 'S' DISPLAY LIKE 'E'.
*        RETURN.
*      ENDIF.

      LOOP AT s_vkorg ASSIGNING FIELD-SYMBOL(<fs_empresa>).

        zcl_carga_saida_insumos=>check_permissao_carga_core(
          EXPORTING
            i_atividade = '01'
            i_bukrs     = <fs_empresa>-low
            i_spart     = gv_spart
          RECEIVING
            r_msg_error = DATA(lv_erro) ).

        IF lv_erro IS NOT INITIAL.
          MESSAGE lv_erro TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

      ENDLOOP.

      PERFORM f_sel_dados_mont_carga_7200 CHANGING lva_msg_error.

      IF lva_msg_error IS NOT INITIAL.
        MESSAGE lva_msg_error TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      PERFORM f_show_alv_mont_carga_sem_sai.

    WHEN rb_list. "Listar Carga |===============================================================================================================

      "tabstrip_7200-activetab = 'PUSH1'.

      LOOP AT s_vkorg ASSIGNING <fs_empresa>.

        zcl_carga_saida_insumos=>check_permissao_carga_core(
          EXPORTING
            i_atividade = '02'
            i_bukrs     = <fs_empresa>-low
            i_spart     = gv_spart
          RECEIVING
            r_msg_error = lv_erro ).

        IF lv_erro IS NOT INITIAL.
          MESSAGE lv_erro TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

      ENDLOOP.

      zcl_carga_saida_insumos=>busca_dados_carga(
        EXPORTING
          i_nr_carga    =  s_carga[]
          i_vkorg       =  s_vkorg[]
          i_vkbur       =  s_vkbur[]
          i_spart       =  CONV #( gv_spart )
          i_kunnr       =  s_kunnr[]
          i_nro_sol     =  s_nrsol[]
          i_id_viagem   =  s_idvgm[]
          i_id_carga_safra_ctrl = s_id_sf[]
          i_ordem_venda =  s_ordem[]
          i_dt_carga    =  s_dtcar[]
       IMPORTING
          e_cargas      =  git_lista_carga_saida_sem ).

      IF git_lista_carga_saida_sem[] IS INITIAL.
        MESSAGE 'Nenhuma registro encontrado com os parametros de seleção!' TYPE 'S' DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      SORT git_lista_carga_saida_sem BY data_atual DESCENDING hora_atual DESCENDING.

      PERFORM f_show_alv_list_carga_sem_sai.

  ENDCASE.


ENDFORM.

MODULE status_7200 OUTPUT.

  DATA: lva_title TYPE string.

  DATA: lva_nro_carga_out TYPE string.

  FREE: lt_fcode.

  APPEND c_vinc_pedidos_7200 TO lt_fcode. "Opção esta inativa permanente...

  CASE gv_ucomm .
    WHEN c_cria_carga_7200  OR
         c_inf_chave_7200   OR
         c_edit_solic_7200  OR
         c_edit_log_7200    OR
         c_inf_lotes_7200.

      APPEND c_edit_solic_7200 TO lt_fcode.
      APPEND c_autor_emb_7200 TO lt_fcode.
      APPEND c_edit_log_7200 TO lt_fcode.
      APPEND c_inf_chave_7200 TO lt_fcode.
      APPEND c_conf_carga_7200 TO lt_fcode.
      APPEND c_canc_carga_7200 TO lt_fcode.
      APPEND c_send_carguero_7200 TO lt_fcode.
      APPEND c_inf_lotes_7200 TO lt_fcode.
      APPEND c_gerar_romaneios_7200 TO lt_fcode.
      APPEND c_troca_nota_7200 TO lt_fcode.
      APPEND c_fatura_auto_7200 TO lt_fcode.
      APPEND c_env_doc_carguero_7200 TO lt_fcode.
      APPEND c_reenv_transp_safra_7200 TO lt_fcode.
  ENDCASE.

  IF gwa_cab_carga_saida-nro_cg IS NOT INITIAL.
    DATA(_preenche_lote_manual) = zcl_carga_saida_insumos=>get_preenchimento_lote_manual( EXPORTING i_nro_cg  = gwa_cab_carga_saida-nro_cg ).
    IF _preenche_lote_manual EQ abap_false.
      APPEND c_inf_lotes_7200 TO lt_fcode.
    ENDIF.
  ENDIF.

  IF gwa_cab_carga_saida-integrar_carguero IS NOT INITIAL.
    APPEND c_edit_log_7200 TO lt_fcode.
  ELSE.
    APPEND c_send_carguero_7200 TO lt_fcode.
    APPEND c_env_doc_carguero_7200 TO lt_fcode.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = gwa_cab_carga_saida-nro_cg
    IMPORTING
      output = lva_nro_carga_out.

  CASE gv_ucomm.
    WHEN c_cria_carga_7200.
      lva_title = |Criando uma Nova Carga|.
    WHEN c_edit_solic_7200.
      lva_title = |Editando Itens da Carga { lva_nro_carga_out } |.
    WHEN c_inf_lotes_7200.
      lva_title = |Editando Lotes da Carga { lva_nro_carga_out } |.
    WHEN c_inf_chave_7200.
      lva_title = |Editando Notas da Carga { lva_nro_carga_out } |.
    WHEN c_edit_log_7200.
      lva_title = |Editando Dados Logísticos da Carga { lva_nro_carga_out } |.
    WHEN OTHERS.
      lva_title = |Exibição da Carga { lva_nro_carga_out } |.
  ENDCASE.

  SET TITLEBAR 'T7200' WITH lva_title.
  SET PF-STATUS 'PF7200_CARGA' EXCLUDING lt_fcode.

  CASE sy-ucomm.
    WHEN 'PUSH1' OR 'PUSH2' OR 'PUSH3' OR 'PUSH4' OR 'PUSH5'.
      tabstrip_7200-activetab = sy-ucomm.
    WHEN c_edit_solic_7200.
      tabstrip_7200-activetab = 'PUSH1'.
    WHEN c_inf_chave_7200.
      tabstrip_7200-activetab = 'PUSH2'.
    WHEN c_inf_lotes_7200.
      tabstrip_7200-activetab = 'PUSH4'.
    WHEN c_gerar_romaneios_7200.
      tabstrip_7200-activetab = 'PUSH5'.

  ENDCASE.

  IF gv_ucomm NE c_cria_carga_7200.

    "D -  Modo de exibição
    "E -  Modo de modificação

    ip_mode = 'E'.

    CLEAR obj.
    obj-objtype = 'ZSDT0112'.
    obj-objkey  = gwa_cab_carga_saida-nro_cg.

    CREATE OBJECT manager
      EXPORTING
        is_object        = obj
        ip_no_commit     = 'R'
        ip_mode          = ip_mode
      EXCEPTIONS
        object_invalid   = 1
        callback_invalid = 2
        OTHERS           = 3.

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_5900  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_7200 INPUT.

  CASE sy-ucomm.
    WHEN c_save_carga_7200.

      PERFORM f_grava_carga_7200.

    WHEN c_cancel_opr_7200.

      gv_ucomm = c_view_carga_7200.

      zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = gwa_cab_carga_saida-nro_cg
                                                                     i_bloqueio = abap_false ).

      IF gwa_cab_carga_saida-nro_cg IS NOT INITIAL.
        PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.

    WHEN 'BACK' OR 'LEAVE' OR 'EXIT'.

      IF manager IS NOT INITIAL.
        CALL METHOD manager->unpublish.
        FREE manager.
      ENDIF.

      zcl_carga_saida_insumos=>bloqueio_desbloqueio_carga( EXPORTING i_nro_cg   = gwa_cab_carga_saida-nro_cg
                                                                     i_bloqueio = abap_false ).
      LEAVE TO SCREEN 0.

    WHEN c_edit_solic_7200.

      PERFORM f_edita_solicitacoes_7200.

    WHEN c_edit_log_7200.

      PERFORM f_edit_dados_log_7200.

    WHEN c_autor_emb_7200.

      PERFORM f_autoriza_embarque_7200.

*-US192364-14.10.2025-#192364-JT-inicio
    WHEN c_edit_frete_7200.

      PERFORM f_editar_frete_7200.
*-US192364-14.10.2025-#192364-JT-fim

    WHEN c_send_carguero_7200.

      DATA(lva_msg_error) = zcl_carga_saida_insumos=>gerar_lote_embarcador_carguero( EXPORTING i_nro_cg =  gwa_cab_carga_saida-nro_cg ).
      IF lva_msg_error IS NOT INITIAL.
        MESSAGE |Não foi possivel integrar a Carga ao Carguero/Strada! Msg: { lva_msg_error }| TYPE 'I'.
      ENDIF.

    WHEN c_canc_carga_7200.

      PERFORM f_cancela_carga_7200.

    WHEN c_inf_chave_7200.

      PERFORM f_informar_chave_7200.

    WHEN c_inf_lotes_7200.

      PERFORM f_informar_lotes_7200.

    WHEN c_gerar_romaneios_7200.

      PERFORM f_gerar_romaneios_7200.

    WHEN c_troca_nota_7200.

      PERFORM f_exec_troca_nota_7200.

    WHEN c_fatura_auto_7200.

      PERFORM f_exec_fatura_auto_7200.

    WHEN c_conf_carga_7200.

      PERFORM f_conferir_carga_7200.

    WHEN c_vinc_pedidos_7200.

      PERFORM f_vincular_pedidos_7200.

    WHEN c_env_doc_carguero_7200.

      PERFORM f_env_documentos_carguero_7200.

    WHEN c_reenv_transp_safra_7200.

      PERFORM f_reenv_transp_safra_7200.

    WHEN OTHERS.

      IF gv_ucomm EQ c_view_carga_7200.
        DATA(_load_bd) = abap_true.
      ELSE.
        _load_bd = abap_false.
      ENDIF.

      PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg _load_bd.

  ENDCASE.


ENDMODULE.

FORM f_exec_fatura_auto_7200.

  DATA(lva_msg_error) = zcl_carga_saida_insumos=>exec_faturamento_saida_auto( EXPORTING i_nro_cg  =  gwa_cab_carga_saida-nro_cg ).

  IF lva_msg_error IS NOT INITIAL.
    MESSAGE lva_msg_error TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

ENDFORM.

FORM f_conferir_carga_7200.

  DATA(lva_msg_error) = zcl_carga_saida_insumos=>conferir_carga( EXPORTING i_nro_carga  =  gwa_cab_carga_saida-nro_cg ).

  IF lva_msg_error IS NOT INITIAL.
    MESSAGE lva_msg_error TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

ENDFORM.

FORM f_cancela_carga_7200 .

  zcl_carga_saida_insumos=>cancela_carga(
  EXPORTING
    i_nro_carga = gwa_cab_carga_saida-nro_cg
  IMPORTING
    e_msg_erro = DATA(lv_msg) ).

  IF lv_msg IS NOT INITIAL.
    MESSAGE lv_msg TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_list_carga_7200.

  LEAVE TO SCREEN 0.


ENDFORM.

FORM f_refresh_list_carga_7200.

  CLEAR: git_lista_carga_saida_sem[].

  zcl_carga_saida_insumos=>busca_dados_carga(
       EXPORTING
         i_nr_carga    =  s_carga[]
         i_vkorg       =  s_vkorg[]
         i_vkbur       =  s_vkbur[]
         i_spart       =  CONV #( gv_spart )
         i_kunnr       =  s_kunnr[]
         i_nro_sol     =  s_nrsol[]
         i_id_viagem   =  s_idvgm[]
         i_id_carga_safra_ctrl = s_id_sf[]
         i_ordem_venda =  s_ordem[]
         i_dt_carga    =  s_dtcar[]
      IMPORTING
         e_cargas      =  git_lista_carga_saida_sem ).

ENDFORM.

FORM f_edita_solicitacoes_7200.

  DATA(r_msg_error) = zcl_carga_saida_insumos=>habilitar_edicao_solicitacoes( EXPORTING i_nro_carga = gwa_cab_carga_saida-nro_cg ).

  IF r_msg_error IS NOT INITIAL.
    MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

  gv_ucomm = c_edit_solic_7200.


ENDFORM.

FORM f_edit_dados_log_7200.

  DATA(r_msg_error) = zcl_carga_saida_insumos=>habilitar_edicao_dados_logist( EXPORTING i_nro_carga = gwa_cab_carga_saida-nro_cg ).
  IF r_msg_error IS NOT INITIAL.
    MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

  gv_ucomm = c_edit_log_7200.

ENDFORM.

FORM f_informar_chave_7200 .

  DATA(r_msg_error) = zcl_carga_saida_insumos=>habilitar_edicao_chaves( EXPORTING i_nro_carga = gwa_cab_carga_saida-nro_cg ).

  IF r_msg_error IS NOT INITIAL.
    MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

  gv_ucomm = c_inf_chave_7200.

ENDFORM.

FORM f_informar_lotes_7200 .

  DATA(r_msg_error) = zcl_carga_saida_insumos=>habilitar_edicao_lotes( EXPORTING i_nro_carga = gwa_cab_carga_saida-nro_cg ).

  IF r_msg_error IS NOT INITIAL.
    MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

  CLEAR: git_7200_lotes[].
  CLEAR: gva_vbeln_lote_sel,  gva_posnr_lote_sel,  gva_nr_rot_lote_sel.

  gv_ucomm = c_inf_lotes_7200.

ENDFORM.


FORM f_autoriza_embarque_7200.

  zcl_carga_saida_insumos=>gerar_autorizacao_embarque(
    EXPORTING
      i_nro_cg      = gwa_cab_carga_saida-nro_cg
    IMPORTING
      e_msg_error   = DATA(lva_msg_error)
    RECEIVING
      r_sucesso     = DATA(lva_sucesso) ).

  IF lva_msg_error IS NOT INITIAL.
    MESSAGE lva_msg_error TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

ENDFORM.

*-US192364-14.10.2025-#192364-JT-inicio
FORM f_editar_frete_7200.

  DATA(r_msg_error) = zcl_carga_saida_insumos=>habilitar_edicao_frete( EXPORTING i_nro_carga = gwa_cab_carga_saida-nro_cg ).

  IF r_msg_error IS NOT INITIAL.
    MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  lwa_preco_frete = gwa_cab_carga_saida-preco_frete.
  SPLIT gwa_cab_carga_saida-motivo_ajuste_frete AT '|' INTO lwa_motivo_ajuste_frete1 lwa_motivo_ajuste_frete2 lwa_motivo_ajuste_frete3.

*-Edicao Frete
  CALL SCREEN 7211 STARTING AT  50 07
                     ENDING AT 130 15.

ENDFORM.
*-US192364-14.10.2025-#192364-JT-fim

FORM f_gerar_romaneios_7200.

  DATA(lva_msg_error) = zcl_carga_saida_insumos=>gerar_romaneios( EXPORTING i_nro_cg  =  gwa_cab_carga_saida-nro_cg ).

  IF lva_msg_error IS NOT INITIAL.
    MESSAGE lva_msg_error TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

ENDFORM.


FORM f_grava_carga_7200.

  DATA: lwa_ov_lote_save TYPE zsdt0134.

  CLEAR: lwa_ov_lote_save.

  IF gv_ucomm EQ c_inf_lotes_7200. "Modo Edição Lotes

    IF gva_vbeln_lote_sel IS INITIAL.
      MESSAGE 'Não foi selecionada uma Ordem para salvar os lotes!' TYPE 'I'.
      RETURN.
    ENDIF.

    lwa_ov_lote_save-vbeln  = gva_vbeln_lote_sel.
    lwa_ov_lote_save-posnr  = gva_posnr_lote_sel.
    lwa_ov_lote_save-nr_rot = gva_nr_rot_lote_sel.
  ENDIF.
**<<<------"169665 - NMS - INI------>>>
* Verifica se ao garvar a ação é de Edição Logistica
* é se Transportadora e Preço de Frete estão preenchidos.
  IF gv_ucomm                               EQ c_edit_log_7200 AND
     gwa_cab_carga_saida-cod_transportadora IS NOT INITIAL     AND
     gwa_cab_carga_saida-preco_frete        IS NOT INITIAL.
    IF gwa_cab_carga_saida-dt_frete_contratado IS INITIAL.
      gwa_cab_carga_saida-dt_frete_contratado = sy-datum.

    ENDIF.

    gwa_cab_carga_saida-cod_transportadora  = |{ gwa_cab_carga_saida-cod_transportadora ALPHA = IN WIDTH = 10 }|.
    gwa_cab_carga_saida-motorista           = |{ gwa_cab_carga_saida-motorista ALPHA = IN WIDTH = 10 }|.

  ENDIF.
**<<<------"169665 - NMS - FIM------>>>
  zcl_carga_saida_insumos=>gravar_carga(
    EXPORTING
      i_header              = gwa_cab_carga_saida
      i_solicitacoes        = git_7200_solicitacoes
      i_notas_venda         = git_7200_notas_venda
      i_notas_transferencia = git_7200_notas_transf
      i_lotes               = git_7200_lotes
      i_lote_ov_save        = lwa_ov_lote_save
    IMPORTING
      e_carga               = DATA(lva_nro_carga)
      e_msg_erro            = DATA(lva_msg_error)  ).

  IF lva_msg_error IS NOT INITIAL.
    MESSAGE lva_msg_error TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF gv_ucomm EQ c_inf_lotes_7200. "Se estiver no modo de Informar Lotes
    "Continuar no modo de edição
  ELSE.
    gv_ucomm = c_view_carga_7200.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING lva_nro_carga abap_true.

  MESSAGE |Carga { lva_nro_carga } gravada com sucesso!| TYPE 'S'.

ENDFORM.



FORM xpf_status_set_7200 USING ucomm TYPE kkblo_t_extab.

  DATA: tl_fcode TYPE TABLE OF sy-ucomm,
        wl_fcode TYPE sy-ucomm.

  DATA: gt_f4 TYPE lvc_t_f4.
  DATA: gs_f4 TYPE lvc_s_f4.

  DATA: gr_events       TYPE REF TO lcl_event_receiver,
        ls_sel_hide     TYPE slis_sel_hide_alv,
        it_fieldcatalog TYPE lvc_t_fcat,
        wa_fieldcatalog TYPE lvc_s_fcat,
        is_table        TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      es_sel_hide = ls_sel_hide
      e_grid      = ref_7200.

  CALL METHOD ref_7200->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcatalog.

  is_table-row = 'X'.
  is_table-col = 'X'.

  CALL METHOD ref_7200->refresh_table_display
    EXPORTING
      is_stable      = is_table
      i_soft_refresh = 'X'.

  IF init_7200 IS INITIAL.

    CALL METHOD ref_7200->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ref_7200->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    "CREATE OBJECT gr_events.
    "SET HANDLER: gr_events->handle_on_button_click FOR ref_7200.

    init_7200  = 'X'.
  ENDIF.

  SET PF-STATUS 'PF7200' EXCLUDING tl_fcode.

ENDFORM. "XPF_STATUS_SET


FORM xpf_status_set_7200_02 USING ucomm TYPE kkblo_t_extab. "#EC CALLED

  DATA: tl_fcode TYPE TABLE OF sy-ucomm,
        wl_fcode TYPE sy-ucomm.

  DATA: gt_f4 TYPE lvc_t_f4.
  DATA: gs_f4 TYPE lvc_s_f4.

  DATA: gr_events       TYPE REF TO lcl_event_receiver,
        ls_sel_hide     TYPE slis_sel_hide_alv,
        it_fieldcatalog TYPE lvc_t_fcat,
        wa_fieldcatalog TYPE lvc_s_fcat,
        is_table        TYPE lvc_s_stbl.

  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      es_sel_hide = ls_sel_hide
      e_grid      = ref_7200_02.

  CALL METHOD ref_7200_02->get_frontend_fieldcatalog
    IMPORTING
      et_fieldcatalog = it_fieldcatalog.

  is_table-row = 'X'.
  is_table-col = 'X'.

  CALL METHOD ref_7200_02->refresh_table_display
    EXPORTING
      is_stable      = is_table
      i_soft_refresh = 'X'.

  IF init_7200_02 IS INITIAL.
    CALL METHOD ref_7200_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_modified.

    CALL METHOD ref_7200_02->register_edit_event
      EXPORTING
        i_event_id = cl_gui_alv_grid=>mc_evt_enter.

    "CREATE OBJECT gr_events.
    "SET HANDLER: gr_events->handle_on_button_click FOR ref_7200_02.

    init_7200_02  = 'X'.
  ENDIF.

  SET PF-STATUS 'PF7200_02' EXCLUDING tl_fcode.

ENDFORM. "XPF_STATUS_SET

CLASS lcl_carga_7200_sol IMPLEMENTATION.

  METHOD set_toolbar.

    DATA: wl_toolbar  TYPE stb_button.

    CHECK gob_grid_7200_solicitacoes->is_ready_for_input( ) EQ 1.

    READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>)
    WITH KEY function = '&LOCAL&INSERT_ROW'.
    IF sy-subrc IS INITIAL.
      <fs_toolbar>-function = 'INSERT_ROW'.
    ENDIF.

    READ TABLE e_object->mt_toolbar ASSIGNING <fs_toolbar>
    WITH KEY function = '&LOCAL&DELETE_ROW'.
    IF sy-subrc IS INITIAL.
      <fs_toolbar>-function = 'DELETE_ROW'.
    ENDIF.

    wl_toolbar-butn_type    = 3.
    APPEND wl_toolbar TO e_object->mt_toolbar.
    CLEAR wl_toolbar.


  ENDMETHOD.                    "SET_TOOLBAR

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'INSERT_ROW'.
        PERFORM f_insere_novo_sol_7200.
      WHEN 'DELETE_ROW'.
        PERFORM f_del_sol_7200.

    ENDCASE.
  ENDMETHOD.                    "GET_UCOMM

  METHOD on_data_changed.

    DATA: ls_good     TYPE lvc_s_modi,
          lv_value    TYPE lvc_value,
          lv_sol      TYPE vbap-kwmeng,
          lv_roteiro  TYPE zsdt0132-nr_rot,
          lv_total    TYPE zmmt0201-qtd_total_kg,
          lv_valor_un TYPE zmmt0201-qtd_total_kg,
          lv_tot_kg   TYPE zmmt0202-qtd_vinc_carga,
          lv_menge    TYPE bstmg,
          lv_modif    TYPE c,
          ls_coluna   TYPE lvc_s_col,
          ls_linha    TYPE lvc_s_row,
          ls_row_no   TYPE lvc_s_roid,
          ls_stable   TYPE lvc_s_stbl.

    DATA: e_route  TYPE trolz-route,
          e_return TYPE bapiret2.

    FIELD-SYMBOLS: <fs_field> TYPE any.

    LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<ls_good>).

      READ TABLE git_7200_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao_mod>) INDEX <ls_good>-row_id.
      CHECK sy-subrc EQ 0.

      CASE <ls_good>-fieldname.

        WHEN 'QTD_VINC'.

          DATA(_new_qtde_vinc_kg) = <ls_good>-value * <fs_solicitacao_mod>-brgew.

          CALL METHOD er_data_changed->modify_cell
            EXPORTING
              i_row_id    = <ls_good>-row_id
              i_fieldname = 'QTD_VINC_KG'
              i_value     = _new_qtde_vinc_kg.

          lv_modif = abap_true.

        WHEN 'NRO_SOL'.

          DATA(lra_nr_sol) = VALUE zsdt_range_nro_sol( ( sign = 'I' option = 'EQ' low = <ls_good>-value ) ).

          zcl_carga_saida_insumos=>busca_dados_montar_carga(
             EXPORTING
               i_nro_sol               = lra_nr_sol
               i_nro_cg_no_check_saldo = gwa_cab_carga_saida-nro_cg
               i_tipo_saldo            = 'T'
             IMPORTING
               e_tabela_monta_carga = DATA(lit_solicitacoes_saldo) ).

          DELETE lit_solicitacoes_saldo WHERE seq NE <fs_solicitacao_mod>-seq.

          READ TABLE lit_solicitacoes_saldo INTO DATA(lwa_sol_modify) WITH KEY nro_sol = <ls_good>-value
                                                                               seq     = <fs_solicitacao_mod>-seq.
          CHECK sy-subrc EQ 0.

          MOVE-CORRESPONDING lwa_sol_modify TO <fs_solicitacao_mod>.

          lv_modif = abap_true.

      ENDCASE.

    ENDLOOP.

    IF lv_modif = abap_true.
      ls_stable-row = 'X'.
      ls_stable-col = 'X'.

      CALL METHOD gob_grid_7200_solicitacoes->refresh_table_display
        EXPORTING
          is_stable = ls_stable.
    ENDIF.


  ENDMETHOD.

  METHOD on_data_changed_finished.

    DATA: wa_good_cells TYPE lvc_s_modi.

    READ TABLE et_good_cells INTO wa_good_cells INDEX 1.

    CHECK e_modified EQ abap_true.

    IF wa_good_cells-fieldname EQ 'QTD_VINC'.
      PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_false.
      "vg_subt_lote = '5140'.
      LEAVE TO SCREEN 7200.
    ELSEIF wa_good_cells-fieldname EQ 'SEQ_ENTREGA'.
      PERFORM f_atualiza_seq_entrega_7200 USING wa_good_cells.
      "vg_subt_lote = '5140'.
      LEAVE TO SCREEN 7200.
    ENDIF.


  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_


  METHOD on_f4.

    DATA: lt_saldo       TYPE zsdt_saldo_solic,
          lt_rettab      TYPE TABLE OF ddshretval,
          lw_modi        TYPE lvc_s_modi,
          t_mapping      TYPE TABLE OF  dselc,
          s_mapping      TYPE dselc,
          lt_saldo_popup TYPE TABLE OF zsde_saldo_solic_sh_s.

    FIELD-SYMBOLS: <lfst_modi> TYPE lvc_t_modi.

    CHECK gob_grid_7200_solicitacoes->is_ready_for_input( ) = 1.

    ASSIGN er_event_data->m_data->* TO <lfst_modi>.
    CHECK sy-subrc = 0.

    READ TABLE git_7200_solicitacoes INTO DATA(lwa_solicitacao_ref) INDEX 1.
    CHECK sy-subrc EQ 0 AND git_7200_solicitacoes[] IS NOT INITIAL.

    DATA(lra_vkorg)      = VALUE sd_vkorg_ranges( ( sign = 'I' option = 'EQ' low = lwa_solicitacao_ref-vkorg ) ).
    DATA(lra_roteiro_pc) = VALUE zrange_nr_rot( ( sign = 'I' option = 'EQ' low = gwa_cab_carga_saida-roteiro_pc ) ).

**<<<------"169508 - NMS - INI------>>>
    DATA(lra_nro_sol)    = VALUE zsdt_range_nro_sol( FOR el_7200_solicitacoes IN git_7200_solicitacoes
                                                   ( sign = 'I' option = 'EQ' low = el_7200_solicitacoes-nro_sol ) ).
**<<<------"169508 - NMS - FIM------>>>

    IF gwa_cab_carga_saida-carga_automatica = abap_true.
      DATA(_tp_saldo) = 'T'. "Todos
    ELSE.
      _tp_saldo = 'M'. "Manual
    ENDIF.

    zcl_carga_saida_insumos=>busca_dados_montar_carga(
      EXPORTING
        i_vkorg                 = lra_vkorg
        i_inco1                 = gwa_cab_carga_saida-inco1
        i_roteiro_pc            = lra_roteiro_pc
        i_origem_estoque        = lwa_solicitacao_ref-origem_estoque ""SD - Faturamento Saida Insumos - Sementes US 169508 - WPP - Versão New --->>>
        i_spart                 = lwa_solicitacao_ref-spart
        i_tipo_saldo            = _tp_saldo
        i_nro_cg_no_check_saldo = gwa_cab_carga_saida-nro_cg
      IMPORTING
        e_tabela_monta_carga = DATA(lit_solicitacoes_saldo) ).

**<<<------"169508 - NMS - INI------>>>
*   Elimina as solicitações já utilizadas com o mesmo Ponto de Coleta.
    DELETE lit_solicitacoes_saldo WHERE nro_sol IN lra_nro_sol.

    SELECT vbeln, name1, ort01, regio
      FROM vbak AS a
      INNER JOIN kna1 AS b
       ON a~kunnr EQ b~kunnr
      INTO TABLE @DATA(lt_cliente)
      FOR ALL ENTRIES IN @lit_solicitacoes_saldo
    WHERE a~vbeln EQ @lit_solicitacoes_saldo-vbeln.
**<<<------"169508 - NMS - FIM------>>>

    LOOP AT lit_solicitacoes_saldo ASSIGNING FIELD-SYMBOL(<fs_sol_saldo>).
      APPEND INITIAL LINE TO lt_saldo_popup ASSIGNING FIELD-SYMBOL(<fs_sol_popup>).

      <fs_sol_popup>-nro_solic           = <fs_sol_saldo>-nro_sol.
      <fs_sol_popup>-seq                 = <fs_sol_saldo>-seq.
      <fs_sol_popup>-vbeln               = <fs_sol_saldo>-vbeln.
      <fs_sol_popup>-posnr               = <fs_sol_saldo>-posnr.
      <fs_sol_popup>-matnr               = <fs_sol_saldo>-matnr.
      <fs_sol_popup>-saldo               = <fs_sol_saldo>-saldo.
      <fs_sol_popup>-embalagem           = <fs_sol_saldo>-meins.
      <fs_sol_popup>-saldo_kg            = <fs_sol_saldo>-saldo * <fs_sol_saldo>-brgew.
      <fs_sol_popup>-carga_automatica    = <fs_sol_saldo>-carga_automatica.
**<<<------"169508 - NMS - INI------>>>
      <fs_sol_popup>-werks               = <fs_sol_saldo>-werks.
      <fs_sol_popup>-maktx               = <fs_sol_saldo>-maktx.
      <fs_sol_popup>-dt_enterga          = <fs_sol_saldo>-dt_entrega.
      READ TABLE lt_cliente INTO DATA(el_cliente) WITH KEY vbeln = <fs_sol_saldo>-vbeln.
      IF sy-subrc IS INITIAL.
        <fs_sol_popup>-name1             = el_cliente-name1.
        <fs_sol_popup>-ort01             = el_cliente-ort01.
        <fs_sol_popup>-regio             = el_cliente-regio.
      ENDIF.
**<<<------"169508 - NMS - FIM------>>>
    ENDLOOP.

    s_mapping-fldname     = 'NRO_SOL'.
    s_mapping-dyfldname   = 'NRO_SOL'.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'SEQ'.
    s_mapping-dyfldname   = 'SEQ'.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    s_mapping-fldname     = 'CARGA_AUTOMATICA'.
    s_mapping-dyfldname   = 'CARGA_AUTOMATICA'.
    APPEND s_mapping TO t_mapping.
    CLEAR s_mapping.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        ddic_structure  = 'ZSDE_SALDO_SOLIC_SH_S'
        retfield        = 'NRO_SOLIC'
        value_org       = 'S'
      TABLES
        value_tab       = lt_saldo_popup
        return_tab      = lt_rettab
        dynpfld_mapping = t_mapping
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    er_event_data->m_event_handled = 'X'.   "<<<------"169508 - NMS ------->>>
    CHECK lt_rettab IS NOT INITIAL.

    READ TABLE git_7200_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao_carga>) INDEX es_row_no-row_id.
    CHECK sy-subrc IS INITIAL.

    READ TABLE lt_rettab ASSIGNING FIELD-SYMBOL(<fs_rettab>) WITH KEY fieldname = 'NRO_SOLIC'.
    IF sy-subrc IS INITIAL.
      CLEAR lw_modi.
      lw_modi-row_id    = es_row_no-row_id.
      lw_modi-fieldname = e_fieldname.
      lw_modi-value     = <fs_rettab>-fieldval.
      APPEND lw_modi TO <lfst_modi>.
    ENDIF.

    READ TABLE lt_rettab ASSIGNING <fs_rettab> WITH KEY fieldname = 'SEQ'.
    IF sy-subrc IS INITIAL.
      <fs_solicitacao_carga>-seq = <fs_rettab>-fieldval.
    ENDIF.

    READ TABLE lt_rettab ASSIGNING <fs_rettab> WITH KEY fieldname = 'CARGA_AUTOMATICA'.
    IF sy-subrc IS INITIAL.
      <fs_solicitacao_carga>-carga_automatica = <fs_rettab>-fieldval.
    ENDIF.

*    er_event_data->m_event_handled = 'X'.   "<<<------"169508 - NMS ------->>>

  ENDMETHOD.

ENDCLASS.

FORM f_insere_novo_sol_7200 .

  DATA: lt_celltab TYPE lvc_t_styl,
        lw_celltab TYPE lvc_s_styl,
        lt_color   TYPE lvc_t_scol,
        ls_color   TYPE lvc_s_scol.

  APPEND INITIAL LINE TO git_7200_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_solicitacao>).

  CALL METHOD gob_grid_7200_solicitacoes->refresh_table_display
    EXPORTING
      is_stable = wl_stable.

ENDFORM.

FORM f_del_sol_7200.

  DATA: lt_sel_rows TYPE lvc_t_row,
        lw_sel_rows TYPE lvc_s_row.

  CALL METHOD gob_grid_7200_solicitacoes->get_selected_rows
    IMPORTING
      et_index_rows = lt_sel_rows.

  CHECK lt_sel_rows IS NOT INITIAL.

  SORT lt_sel_rows BY index DESCENDING.

  LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>).
    DELETE git_7200_solicitacoes INDEX <fs_sel_rows>-index.
  ENDLOOP.

  gob_grid_7200_solicitacoes->refresh_table_display( ).


ENDFORM.

FORM f_fill_fieldcatalog TABLES p_it_fieldacatalog STRUCTURE lvc_s_fcat
                         USING VALUE(p_colnum)
                               VALUE(p_fieldname)
                               VALUE(p_tabname)
                               VALUE(p_ref_table)
                               VALUE(p_ref_field)
                               VALUE(p_emphasize)
                               VALUE(p_edit)
                               VALUE(p_icon)
                               VALUE(p_hotspot)
                               VALUE(p_opt)
                               VALUE(p_checkbox)
                               VALUE(p_dosum)
                               VALUE(p_f4)
                               VALUE(p_outputlen)
                               VALUE(p_just)
                               VALUE(p_header).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-ref_table  = p_ref_table.
  wa_fieldcatalog-ref_field  = p_ref_field.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-scrtext_l  = p_header.
  wa_fieldcatalog-scrtext_m  = p_header.
  wa_fieldcatalog-scrtext_s  = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-just       = p_just.

  APPEND wa_fieldcatalog TO p_it_fieldacatalog.

ENDFORM.


"FF #169665 - inicio
FORM f_fill_fieldcatalog2 TABLES p_it_fieldacatalog STRUCTURE lvc_s_fcat
                         USING VALUE(p_colnum)
                               VALUE(p_fieldname)
                               VALUE(p_tabname)
                               VALUE(p_ref_table)
                               VALUE(p_ref_field)
                               VALUE(p_emphasize)
                               VALUE(p_edit)
                               VALUE(p_icon)
                               VALUE(p_hotspot)
                               VALUE(p_opt)
                               VALUE(p_checkbox)
                               VALUE(p_dosum)
                               VALUE(p_f4)
                               VALUE(p_outputlen)
                               VALUE(p_just)
                               VALUE(p_header)
                               VALUE(p_datatype).

  DATA: wa_fieldcatalog TYPE lvc_s_fcat.

  wa_fieldcatalog-col_pos    = p_colnum.
  wa_fieldcatalog-fieldname  = p_fieldname.
  wa_fieldcatalog-tabname    = p_tabname.
  wa_fieldcatalog-ref_table  = p_ref_table.
  wa_fieldcatalog-ref_field  = p_ref_field.
  wa_fieldcatalog-emphasize  = p_emphasize.
  wa_fieldcatalog-coltext    = p_header.
  wa_fieldcatalog-scrtext_l  = p_header.
  wa_fieldcatalog-scrtext_m  = p_header.
  wa_fieldcatalog-scrtext_s  = p_header.
  wa_fieldcatalog-edit       = p_edit.
  wa_fieldcatalog-icon       = p_icon.
  wa_fieldcatalog-ref_table  = p_tabname.
  wa_fieldcatalog-hotspot    = p_hotspot.
  wa_fieldcatalog-col_opt    = p_opt.
  wa_fieldcatalog-checkbox   = p_checkbox.
  wa_fieldcatalog-do_sum     = p_dosum.
  wa_fieldcatalog-f4availabl = p_f4.
  wa_fieldcatalog-outputlen  = p_outputlen.
  wa_fieldcatalog-just       = p_just.

  wa_fieldcatalog-datatype   = p_datatype.

  APPEND wa_fieldcatalog TO p_it_fieldacatalog.

ENDFORM.
"FF #169665 - fim


MODULE pbo_7201_config_objetos OUTPUT.


  CASE gv_ucomm.
    WHEN c_cria_carga_7200 OR
         c_edit_solic_7200.

      IF gob_grid_7200_solicitacoes->is_ready_for_input( ) = 0.

        CALL METHOD gob_grid_7200_solicitacoes->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.

      ENDIF.

    WHEN OTHERS.

      IF gob_grid_7200_solicitacoes->is_ready_for_input( ) = 1.

        CALL METHOD gob_grid_7200_solicitacoes->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.

      ENDIF.

  ENDCASE.



ENDMODULE.


MODULE pbo_7204_config_objetos OUTPUT.


  CASE gv_ucomm.
    WHEN c_inf_lotes_7200.

      IF gob_grid_7200_lotes->is_ready_for_input( ) = 0.

        CALL METHOD gob_grid_7200_lotes->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.

      ENDIF.

    WHEN OTHERS.

      IF gob_grid_7200_lotes->is_ready_for_input( ) = 1.

        CALL METHOD gob_grid_7200_lotes->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.

      ENDIF.

  ENDCASE.



ENDMODULE.


MODULE pbo_7205_config_objetos OUTPUT.


ENDMODULE.

MODULE pbo_7202_config_objetos OUTPUT.


  CASE gv_ucomm.
    WHEN c_inf_chave_7200.

      IF gob_grid_7200_nf_venda->is_ready_for_input( ) = 0.

        CALL METHOD gob_grid_7200_nf_venda->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.

      ENDIF.

      IF gob_grid_7200_nf_transf_forn->is_ready_for_input( ) = 0.

        CALL METHOD gob_grid_7200_nf_transf_forn->set_ready_for_input
          EXPORTING
            i_ready_for_input = 1.

      ENDIF.

    WHEN OTHERS.

      IF gob_grid_7200_nf_venda->is_ready_for_input( ) = 1.

        CALL METHOD gob_grid_7200_nf_venda->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.

      ENDIF.

      IF gob_grid_7200_nf_transf_forn->is_ready_for_input( ) = 1.

        CALL METHOD gob_grid_7200_nf_transf_forn->set_ready_for_input
          EXPORTING
            i_ready_for_input = 0.

      ENDIF.

  ENDCASE.



ENDMODULE.

MODULE pbo_7200_config_campos OUTPUT.

  LOOP AT SCREEN.

    IF screen-name EQ 'TAB1' OR
       screen-name EQ 'TAB2' OR
       screen-name EQ 'TAB3' OR
       screen-name EQ 'TAB4' OR
       screen-name EQ 'TAB5'.
      CONTINUE.
    ENDIF.

    CASE gv_ucomm.
      WHEN c_cria_carga_7200 OR c_edit_log_7200.
        "Habilitar Campos...

        IF gv_ucomm NE c_cria_carga_7200.
          IF screen-name CS 'INTEGRAR_CARGUERO'.
            screen-input = 0.
            MODIFY SCREEN.
          ENDIF.
        ENDIF.

      WHEN OTHERS.

        screen-input = 0.
        MODIFY SCREEN.

    ENDCASE.

    "Desabilitar Campos Preenchimento Automatico Carguero
    CASE gwa_cab_carga_saida-integrar_carguero.
      WHEN abap_true.
        IF screen-group1 = 'CRG'. "Campos Preenchimento Carguero
          screen-input = 0.
          MODIFY SCREEN.
        ENDIF.
      WHEN abap_false.
        IF screen-group1 = 'STG'. "Campos Status Integração Carguero
          screen-input     = 0.
          screen-invisible = 1.
          MODIFY SCREEN.
        ENDIF.
    ENDCASE.

  ENDLOOP.

  CLEAR: sy-ucomm.

ENDMODULE.

FORM f_atualiza_seq_entrega_7200 USING wa_good_cells TYPE lvc_s_modi.

  READ TABLE git_7200_solicitacoes INTO DATA(lwa_solicitacao_7200) INDEX wa_good_cells-row_id.

  DATA(lva_roteiro) = lwa_solicitacao_7200-nr_rot.

  LOOP AT git_7200_solicitacoes ASSIGNING FIELD-SYMBOL(<fs_sol_7200>) WHERE nr_rot EQ lva_roteiro.
    <fs_sol_7200>-seq_entrega = wa_good_cells-value.
  ENDLOOP.

ENDFORM.

CLASS lcl_notas_venda_7200 IMPLEMENTATION.

  METHOD set_toolbar.

    DATA: wl_toolbar         TYPE stb_button.

    CHECK gob_grid_7200_nf_venda IS BOUND.

    IF gob_grid_7200_nf_venda->is_ready_for_input( ) EQ 0.
      wl_toolbar-function = 'PDF_NFE'.
      wl_toolbar-icon     = icon_base_planning_object.
      wl_toolbar-text      = 'PDF NF-e'.
      wl_toolbar-quickinfo = 'PDF NF-e'.
      APPEND wl_toolbar TO e_object->mt_toolbar.
    ENDIF.

    CHECK gob_grid_7200_nf_venda->is_ready_for_input( ) EQ 1.

    READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>)
      WITH KEY function = '&LOCAL&DELETE_ROW'.

    IF sy-subrc IS INITIAL.
      <fs_toolbar>-function = 'DELETE_ROW'.
    ENDIF.



  ENDMETHOD.


  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'DELETE_ROW'.
        PERFORM f_delete_nf_venda_7200.
      WHEN 'PDF_NFE'.
        PERFORM f_pdf_nfe_venda_7200.

    ENDCASE.
  ENDMETHOD.

  METHOD on_data_changed.

  ENDMETHOD.
ENDCLASS.


CLASS lcl_bordero_7200 IMPLEMENTATION.

  METHOD set_toolbar.

    DATA: wl_toolbar         TYPE stb_button.

    CHECK gob_grid_7200_bordero IS BOUND.

    wl_toolbar-function = 'REC_LOTE'.
    wl_toolbar-icon     = icon_interchange.
    wl_toolbar-text      = 'Recuperar Lote SAP'.
    wl_toolbar-quickinfo = 'Recuperar Lote SAP'.
    APPEND wl_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.


  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'REC_LOTE'.
        PERFORM f_recuperar_lote_bordero_7200.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.


CLASS lcl_notas_transf_7200 IMPLEMENTATION.

  METHOD set_toolbar.

    DATA: wl_toolbar         TYPE stb_button.

    CHECK gob_grid_7200_nf_transf_forn IS BOUND.

    CHECK gob_grid_7200_nf_transf_forn->is_ready_for_input( ) EQ 1.

    READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>)
    WITH KEY function = '&LOCAL&DELETE_ROW'.
    IF sy-subrc IS INITIAL.
      <fs_toolbar>-function = 'DELETE_ROW'.
    ENDIF.

  ENDMETHOD.

  METHOD get_ucomm.
    CASE e_ucomm.
      WHEN 'DELETE_ROW'.
        PERFORM f_delete_nf_transf_7200.
    ENDCASE.
  ENDMETHOD.

  METHOD on_data_changed.

  ENDMETHOD.

ENDCLASS.

FORM f_delete_nf_venda_7200 .

  DATA: lw_sel_hide TYPE slis_sel_hide_alv,
        lt_sel_rows TYPE lvc_t_row,
        lw_sel_rows TYPE lvc_s_row.

  CALL METHOD gob_grid_7200_nf_venda->get_selected_rows
    IMPORTING
      et_index_rows = lt_sel_rows.

  CHECK lt_sel_rows IS NOT INITIAL.

  LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>).
    READ TABLE git_7200_notas_venda ASSIGNING FIELD-SYMBOL(<fs_chaves_nf>) INDEX <fs_sel_rows>-index.
    CHECK sy-subrc IS INITIAL.

    <fs_chaves_nf>-chave_nfe = 'X'.
  ENDLOOP.

  DELETE git_7200_notas_venda WHERE chave_nfe = 'X'.

  CALL METHOD gob_grid_7200_nf_venda->refresh_table_display( ).

ENDFORM.

FORM f_pdf_nfe_venda_7200.

  DATA: lw_sel_hide TYPE slis_sel_hide_alv,
        lt_sel_rows TYPE lvc_t_row,
        lw_sel_rows TYPE lvc_s_row.

  CALL METHOD gob_grid_7200_nf_venda->get_selected_rows
    IMPORTING
      et_index_rows = lt_sel_rows.

  CHECK lt_sel_rows[] IS NOT INITIAL.

  IF lines( lt_sel_rows[] ) > 1 .
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    RETURN.
  ENDIF.

  READ TABLE lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>) INDEX 1.
  CHECK sy-subrc EQ 0.

  READ TABLE git_7200_notas_venda ASSIGNING FIELD-SYMBOL(<fs_chaves_nf>) INDEX <fs_sel_rows>-index.
  CHECK sy-subrc EQ 0.

  IF gwa_cab_carga_saida-integrado_carguero IS INITIAL.
    MESSAGE 'Carga não integrada ao Carguero/Strada!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF gwa_cab_carga_saida-viagem_id IS INITIAL.
    MESSAGE 'Carga sem viagem do Carguero/Strada!' TYPE 'S'.
    RETURN.
  ENDIF.

  PERFORM f_imprimir_nfe_viagem USING <fs_chaves_nf>-chave_nfe gwa_cab_carga_saida-viagem_id.


ENDFORM.


FORM f_recuperar_lote_bordero_7200.

  DATA(lva_rec_lote) = abap_false.

  LOOP AT git_7200_bordero ASSIGNING FIELD-SYMBOL(<fs_bordero>) WHERE lote IS INITIAL AND lote_fornecedor IS NOT INITIAL AND matnr IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = <fs_bordero>-matnr
      IMPORTING
        output = <fs_bordero>-matnr.

    SELECT SINGLE charg
      FROM mch1 INTO <fs_bordero>-lote
     WHERE matnr EQ <fs_bordero>-matnr
       AND licha EQ <fs_bordero>-lote_fornecedor.

    CHECK sy-subrc EQ 0 AND <fs_bordero>-lote IS NOT INITIAL.

    UPDATE zsdt0376 SET lote = <fs_bordero>-lote
     WHERE id_autorizacao_embarque = <fs_bordero>-id_autorizacao_embarque
       AND id_item                 = <fs_bordero>-id_item
       AND lote                    = space
       AND lote_fornecedor         = <fs_bordero>-lote_fornecedor.

    lva_rec_lote = abap_true.

  ENDLOOP.

  IF lva_rec_lote EQ abap_true.
    MESSAGE 'Lotes em branco recuperados com sucesso!' TYPE 'S'.
    CALL METHOD gob_grid_7200_bordero->refresh_table_display( ).
  ENDIF.


ENDFORM.


FORM f_delete_nf_transf_7200.

  DATA: lw_sel_hide TYPE slis_sel_hide_alv,
        lt_sel_rows TYPE lvc_t_row,
        lw_sel_rows TYPE lvc_s_row.

  CALL METHOD gob_grid_7200_nf_transf_forn->get_selected_rows
    IMPORTING
      et_index_rows = lt_sel_rows.

  CHECK lt_sel_rows IS NOT INITIAL.

  LOOP AT lt_sel_rows ASSIGNING FIELD-SYMBOL(<fs_sel_rows>).
    READ TABLE git_7200_notas_transf ASSIGNING FIELD-SYMBOL(<fs_chaves_nf>) INDEX <fs_sel_rows>-index.
    CHECK sy-subrc IS INITIAL.

    <fs_chaves_nf>-chave_nfe = 'X'.
  ENDLOOP.

  DELETE git_7200_notas_transf WHERE chave_nfe = 'X'.

  CALL METHOD gob_grid_7200_nf_transf_forn->refresh_table_display( ).

ENDFORM.



CLASS lcl_carga_7200_lotes IMPLEMENTATION.

  METHOD toolbar.

    DATA wl_toolbar TYPE stb_button.

    CHECK gob_grid_7200_lotes->is_ready_for_input( ) EQ 1.

    READ TABLE e_object->mt_toolbar ASSIGNING FIELD-SYMBOL(<fs_toolbar>)
    WITH KEY function = '&LOCAL&INSERT_ROW'.
    IF sy-subrc IS INITIAL.
      <fs_toolbar>-function = 'ADD_ROW'.
    ENDIF.

    READ TABLE e_object->mt_toolbar ASSIGNING <fs_toolbar>
    WITH KEY function = '&LOCAL&DELETE_ROW'.
    IF sy-subrc IS INITIAL.
      <fs_toolbar>-function = 'DEL_ROW'.
    ENDIF.

    wl_toolbar-butn_type    = 3.
    APPEND wl_toolbar TO e_object->mt_toolbar.
    CLEAR wl_toolbar.

  ENDMETHOD.             "DISPLAY


  METHOD user_command.

    DATA: it_selected_rows TYPE lvc_t_row,
          wa_selected_rows TYPE lvc_s_row.

    CASE e_ucomm.
      WHEN 'ADD_ROW'.

        IF gva_vbeln_lote_sel IS INITIAL.
          MESSAGE 'Selecione uma OV para determinar os Lotes!' TYPE 'S' DISPLAY LIKE 'W'.
          EXIT.
        ENDIF.

        APPEND INITIAL LINE TO git_7200_lotes ASSIGNING FIELD-SYMBOL(<fs_lote_carga>).

        <fs_lote_carga>-vbeln  = gva_vbeln_lote_sel.
        <fs_lote_carga>-posnr  = gva_posnr_lote_sel.
        <fs_lote_carga>-nr_rot = gva_nr_rot_lote_sel.

        CALL METHOD gob_grid_7200_lotes->refresh_table_display
          EXPORTING
            is_stable = _stable.

      WHEN 'DEL_ROW'.

        CLEAR: it_selected_rows, wa_selected_rows.

        CALL METHOD gob_grid_7200_lotes->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        LOOP AT it_selected_rows INTO wa_selected_rows.
          DELETE git_7200_lotes INDEX wa_selected_rows-index.
        ENDLOOP.

        CALL METHOD gob_grid_7200_lotes->refresh_table_display
          EXPORTING
            is_stable = _stable.

    ENDCASE.


  ENDMETHOD.                    "USER_COMMAND

  METHOD data_changed_finished.

    DATA: it_stable     TYPE lvc_s_stbl,
          wa_good_cells TYPE lvc_s_modi,
          wa_nlote_5320 TYPE ty_nlote_5320,
          it_f4_5320    TYPE STANDARD TABLE OF ty_f4_nlote,
          vl_cont       TYPE i,
          it_zsdt0062   TYPE STANDARD TABLE OF zsdt0062,
          it_ekbe       TYPE STANDARD TABLE OF ekbe,
          wa_ekbe       TYPE ekbe,
          it_ekpo       TYPE STANDARD TABLE OF ekpo,
          wa_ekpo       TYPE ekpo,
          it_ekbe_aux   TYPE STANDARD TABLE OF ekbe.

    it_stable-row = 'X'.
    it_stable-col = 'X'.

    IF e_modified EQ abap_true.
      READ TABLE et_good_cells INTO wa_good_cells INDEX 1.
      CHECK sy-subrc = 0.

      CASE wa_good_cells-fieldname.
        WHEN 'VBELN' OR 'POSNR' OR 'CHARG' OR 'LFIMG'.

          READ TABLE git_7200_lotes ASSIGNING FIELD-SYMBOL(<fs_lote_7200>) INDEX wa_good_cells-row_id.
          CHECK ( sy-subrc = 0 ).

          CLEAR: <fs_lote_7200>-brgew, <fs_lote_7200>-peso_liq_brt.

          CHECK <fs_lote_7200>-vbeln IS NOT INITIAL AND
                <fs_lote_7200>-posnr IS NOT INITIAL AND
                <fs_lote_7200>-charg IS NOT INITIAL.

          "Determinar Peso Lote, e Peso Liquido Bruto

          SELECT SINGLE vbeln, matnr
            FROM vbap INTO @DATA(lwa_vbap)
           WHERE vbeln EQ @<fs_lote_7200>-vbeln
             AND posnr EQ @<fs_lote_7200>-posnr.

          IF sy-subrc NE 0.
            CLEAR: <fs_lote_7200>-vbeln, <fs_lote_7200>-posnr, <fs_lote_7200>-charg.
            MESSAGE |Ordem { <fs_lote_7200>-vbeln } item { <fs_lote_7200>-posnr } não encontrado! | TYPE 'S'.
          ELSE.

            DATA(_valor_bag) = zcl_charg=>get_valor_caracteristica(
              EXPORTING
                iv_nome_attr = 'PESO_BAG'
                iv_matnr     = CONV #( lwa_vbap-matnr )
                iv_charg     = CONV #( <fs_lote_7200>-charg )
            ).

            <fs_lote_7200>-brgew = _valor_bag.

          ENDIF.

          IF <fs_lote_7200>-lfimg IS NOT INITIAL.
            <fs_lote_7200>-peso_liq_brt = <fs_lote_7200>-lfimg * <fs_lote_7200>-brgew.
          ENDIF.

      ENDCASE.

      CALL METHOD gob_grid_7200_lotes->refresh_table_display
        EXPORTING
          is_stable = it_stable.

    ENDIF.

  ENDMETHOD.                    "ON_DATA_CHANGED_FINISHED_

  METHOD on_f4.

    DATA: it_ret_7200 TYPE STANDARD TABLE OF ddshretval,
          it_f4_7200  TYPE STANDARD TABLE OF ty_f4_nlote,
          it_f4_nfase TYPE STANDARD TABLE OF ty_f4_nfase,
          vl_cont     TYPE i,
          it_zsdt0062 TYPE STANDARD TABLE OF zsdt0062,
          it_ekbe_aux TYPE STANDARD TABLE OF ekbe,
          it_mchb     TYPE STANDARD TABLE OF mchb,
          wa_mchb     TYPE mchb,
          it_mch1     TYPE STANDARD TABLE OF mch1,
          wa_mch1     TYPE mch1,
          it_mslb     TYPE STANDARD TABLE OF mslb,
          wa_mslb     TYPE mslb,
          it_zmmt0102 TYPE STANDARD TABLE OF zmmt0102,
          wa_zmmt0102 TYPE zmmt0102.

    DATA: wa_ret       TYPE ddshretval,
          wa_modi      TYPE lvc_s_modi,
          l_lfimg      TYPE zsdt0134-lfimg,
          l_saldo_fase TYPE zsdt0134-lfimg.

    FIELD-SYMBOLS: <itab> TYPE lvc_t_modi.

    DATA : it_fmap      TYPE STANDARD TABLE OF dselc,
           wa_fmap      TYPE dselc,
           it_field_tab TYPE TABLE OF dfies,
           wa_field_tab TYPE dfies.

    DATA: vg_charg TYPE mch1-charg.

    CASE e_fieldname.
      WHEN 'CHARG'.

        READ TABLE git_7200_lotes INTO DATA(lwa_lote_7200) INDEX es_row_no-row_id.
        CHECK sy-subrc IS INITIAL.

        CLEAR: it_fmap[].

        wa_fmap-fldname = 'F0001'.
        wa_fmap-dyfldname = 'CHARG'.
        APPEND wa_fmap TO it_fmap.

        wa_fmap-fldname = 'F0008'.
        wa_fmap-dyfldname = 'CATEGORIA'.
        APPEND wa_fmap TO it_fmap.


        READ TABLE git_7200_solicitacoes INTO DATA(lwa_solicitacao) WITH KEY vbeln = lwa_lote_7200-vbeln
                                                                             posnr = lwa_lote_7200-posnr.
        CHECK sy-subrc EQ 0.

        SELECT *
          FROM mchb INTO TABLE it_mchb
         WHERE matnr EQ lwa_solicitacao-matnr
           AND werks EQ lwa_solicitacao-werks.

        SELECT *
          FROM mslb INTO TABLE it_mslb
         WHERE matnr EQ lwa_solicitacao-matnr
           AND werks EQ lwa_solicitacao-werks.

        IF it_mchb IS NOT INITIAL.
          SELECT *
            FROM mch1 INTO TABLE it_mch1
            FOR ALL ENTRIES IN it_mchb
           WHERE charg EQ it_mchb-charg
             AND matnr EQ it_mchb-matnr.
        ENDIF.

        IF it_mslb IS NOT INITIAL.
          SELECT *
            FROM mch1 APPENDING TABLE it_mch1
             FOR ALL ENTRIES IN it_mslb
           WHERE charg EQ it_mslb-charg
             AND matnr EQ it_mslb-matnr.
        ENDIF.

        IF it_mchb[] IS NOT INITIAL.
          SELECT *
            FROM zmmt0102 APPENDING TABLE it_zmmt0102
             FOR ALL ENTRIES IN it_mchb
           WHERE charg EQ it_mchb-charg
             AND matnr EQ it_mchb-matnr.
        ENDIF.

        IF it_mslb[] IS NOT INITIAL.
          SELECT *
            FROM zmmt0102 APPENDING TABLE it_zmmt0102
             FOR ALL ENTRIES IN it_mslb
           WHERE charg EQ it_mslb-charg
             AND matnr EQ it_mslb-matnr.
        ENDIF.

        LOOP AT it_mchb INTO wa_mchb.

          APPEND INITIAL LINE TO it_f4_7200 ASSIGNING FIELD-SYMBOL(<fs_f4_7200>).

          <fs_f4_7200>-charg = wa_mchb-charg.
          <fs_f4_7200>-lgort = wa_mchb-lgort.
          <fs_f4_7200>-werks = wa_mchb-werks.
          <fs_f4_7200>-matnr = wa_mchb-matnr.

          READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = <fs_f4_7200>-matnr
                                                   charg = <fs_f4_7200>-charg.
          IF sy-subrc IS INITIAL.
            <fs_f4_7200>-vfdat = wa_mch1-vfdat.
          ENDIF.

          <fs_f4_7200>-clabs = wa_mchb-clabs.

          READ TABLE it_zmmt0102 INTO wa_zmmt0102 WITH KEY  matnr = <fs_f4_7200>-matnr
                                                            charg = <fs_f4_7200>-charg.
          IF sy-subrc = 0.
            <fs_f4_7200>-categoria = wa_zmmt0102-categoria.
          ENDIF.
        ENDLOOP.

        LOOP AT it_mslb INTO wa_mslb.

          APPEND INITIAL LINE TO it_f4_7200 ASSIGNING <fs_f4_7200>.

          <fs_f4_7200>-charg = wa_mslb-charg.
          <fs_f4_7200>-werks = wa_mslb-werks.
          <fs_f4_7200>-matnr = wa_mslb-matnr.

          READ TABLE it_mch1 INTO wa_mch1 WITH KEY matnr = <fs_f4_7200>-matnr
                                                   charg = <fs_f4_7200>-charg.
          IF sy-subrc IS INITIAL.
            <fs_f4_7200>-vfdat = wa_mch1-vfdat.
          ENDIF.

          <fs_f4_7200>-clabs = wa_mslb-lblab.

          READ TABLE it_zmmt0102 INTO wa_zmmt0102 WITH KEY  matnr = <fs_f4_7200>-matnr
                                                            charg = <fs_f4_7200>-charg.
          IF sy-subrc = 0.
            <fs_f4_7200>-categoria = wa_zmmt0102-categoria.
          ENDIF.
        ENDLOOP.

        DELETE it_f4_7200 WHERE clabs IS INITIAL.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'CHARG'
            window_title    = 'Lista de Lotes'(002)
            value_org       = 'S'
          TABLES
            value_tab       = it_f4_7200
            return_tab      = it_ret_7200
            dynpfld_mapping = it_fmap
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.


        CHECK sy-subrc = 0.

        ASSIGN er_event_data->m_data->* TO <itab>.
        READ TABLE it_ret_7200 INTO wa_ret INDEX 1.
        wa_modi-row_id   = es_row_no-row_id.
        wa_modi-fieldname = 'CHARG'.
        wa_modi-value     = wa_ret-fieldval.
        APPEND wa_modi TO <itab>.

        READ TABLE it_ret_7200 INTO wa_ret INDEX 2.
        wa_modi-fieldname = 'CATEGORIA'.
        wa_modi-value     = wa_ret-fieldval.
        APPEND wa_modi TO <itab>.

        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)

      WHEN 'NR_FASE'.

        READ TABLE git_7200_lotes INTO lwa_lote_7200 INDEX es_row_no-row_id.
        CHECK sy-subrc IS INITIAL.

        READ TABLE git_7200_solicitacoes INTO lwa_solicitacao WITH KEY vbeln = lwa_lote_7200-vbeln
                                                                       posnr = lwa_lote_7200-posnr.
        CHECK sy-subrc EQ 0.

        FREE: it_field_tab, it_fmap, it_zmmt0102.

        CLEAR: wa_fmap.
        wa_fmap-fldname   = 'F0002'.
        wa_fmap-dyfldname = 'CATEGORIA'.
        APPEND wa_fmap TO it_fmap.

        wa_fmap-fldname   = 'F0003'.
        wa_fmap-dyfldname = 'MATNR'.
        APPEND wa_fmap TO it_fmap.

        PERFORM f_fieldinfo_get USING 'ZMMT0102' 'NR_FASE' CHANGING wa_field_tab.

        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-position  = 1.
        wa_field_tab-offset    = 0.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZMMT0102' 'CATEGORIA' CHANGING wa_field_tab.

        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-position  = 2.
        wa_field_tab-offset    = 40.
        wa_field_tab-reptext   = 'Categoria'.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZMMT0102' 'MATNR' CHANGING wa_field_tab.
        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-position  = 3.
        wa_field_tab-offset    = 44.
        wa_field_tab-reptext   = 'Material'.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZMMT0102' 'MATNR' CHANGING wa_field_tab.

        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-fieldname = 'MENGE'.
        wa_field_tab-position  = 4.
        wa_field_tab-offset    = 80.
        wa_field_tab-reptext   = 'Quant.Entrada'.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZSDT0102'
                                      'MATNR'
                             CHANGING wa_field_tab.
        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-fieldname = 'LFIMG'.
        wa_field_tab-position  = 5.
        wa_field_tab-offset    = 116.
        wa_field_tab-reptext   = 'Quant.Saida'.
        APPEND wa_field_tab  TO it_field_tab.

        PERFORM f_fieldinfo_get USING 'ZMMT0102'
                                      'MATNR'
                             CHANGING wa_field_tab.
        wa_field_tab-tabname   = 'IT_F4_NFASE'.
        wa_field_tab-fieldname = 'SDO_FASE'.
        wa_field_tab-position  = 6.
        wa_field_tab-offset    = 152.
        wa_field_tab-reptext   = 'Saldo Fase'.
        APPEND wa_field_tab  TO it_field_tab.

        SELECT *
          FROM zmmt0102  APPENDING TABLE it_zmmt0102
         WHERE charg EQ lwa_lote_7200-charg
           AND matnr EQ lwa_solicitacao-matnr.

        LOOP AT it_zmmt0102 INTO wa_zmmt0102.
          APPEND INITIAL LINE TO it_f4_nfase ASSIGNING FIELD-SYMBOL(<fs_f4_fase>).

          CLEAR l_lfimg.

          SELECT SINGLE lfimg
            FROM zsdt0134 INTO l_lfimg
           WHERE charg   = wa_zmmt0102-charg
             AND nr_fase = wa_zmmt0102-nr_fase.

          <fs_f4_fase>-categoria   = wa_zmmt0102-categoria.
          <fs_f4_fase>-matnr       = wa_zmmt0102-matnr.
          <fs_f4_fase>-nr_fase     = wa_zmmt0102-nr_fase.
          <fs_f4_fase>-menge       = wa_zmmt0102-menge.
          <fs_f4_fase>-lfimg       = l_lfimg.
          <fs_f4_fase>-sdo_fase    = wa_zmmt0102-menge - l_lfimg.
        ENDLOOP.

        CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
          EXPORTING
            retfield        = 'NR_FASE'
            window_title    = 'Lista de Fase'(002)
            value_org       = 'S'
          TABLES
            value_tab       = it_f4_nfase
            return_tab      = it_ret_7200
            dynpfld_mapping = it_fmap
            field_tab       = it_field_tab
          EXCEPTIONS
            parameter_error = 1
            no_values_found = 2
            OTHERS          = 3.

        IF sy-subrc = 0.
          ASSIGN er_event_data->m_data->* TO <itab>.
          READ TABLE it_ret_7200 INTO wa_ret INDEX 1.
          wa_modi-row_id   = es_row_no-row_id.
          wa_modi-fieldname = 'NR_FASE'.
          wa_modi-value     = wa_ret-fieldval.
          APPEND wa_modi TO <itab>.
        ENDIF.

        er_event_data->m_event_handled = 'X'."(to inform grid that f4 was handled manually)
      WHEN OTHERS.
    ENDCASE.


  ENDMETHOD.

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION

CLASS lcl_carga_7200_romaneios IMPLEMENTATION.

  METHOD toolbar.

    DATA wl_toolbar TYPE stb_button.

    wl_toolbar-butn_type    = 3.
    APPEND wl_toolbar TO e_object->mt_toolbar.
    CLEAR wl_toolbar.

    wl_toolbar-function = 'ESTORNAROM'.
    wl_toolbar-icon     = '@VI@'.
    wl_toolbar-text     = 'Estornar Todos Romaneios'.
    wl_toolbar-quickinfo = 'Estornar Todos Romaneios'.
    APPEND wl_toolbar TO e_object->mt_toolbar.

    wl_toolbar-function = 'FATURAR_ROM'.
    wl_toolbar-icon     = icon_report.
    wl_toolbar-text     = 'Faturamento Romaneios'.
    wl_toolbar-quickinfo = 'Faturamento Romaneios'.
    APPEND wl_toolbar TO e_object->mt_toolbar.


  ENDMETHOD.             "DISPLAY


  METHOD user_command.

    DATA: rg_chaves    TYPE RANGE OF zsdt0001-ch_referencia.
    DATA: wa_chaves    LIKE LINE OF rg_chaves.

    DATA: it_selected_rows TYPE lvc_t_row,
          wa_selected_rows TYPE lvc_s_row.

    DATA: it_stable     TYPE lvc_s_stbl.

    it_stable-row = 'X'.
    it_stable-col = 'X'.

    CASE e_ucomm.
      WHEN 'FATURAR_ROM'.

        DATA: lt_sel_rows TYPE lvc_t_row,
              lw_sel_rows TYPE lvc_s_row.

        CALL METHOD gob_grid_7200_romaneios->get_selected_rows
          IMPORTING
            et_index_rows = it_selected_rows.

        CHECK it_selected_rows[] IS NOT INITIAL.

        READ TABLE it_selected_rows INTO DATA(lwa_selected_row) INDEX 1.
        CHECK sy-subrc EQ 0.
        READ TABLE git_7200_romaneios INTO DATA(lwa_romaneio_sel) INDEX lwa_selected_row-index.
        CHECK sy-subrc EQ 0.

        LOOP AT git_7200_romaneios INTO DATA(wa_romaneio) WHERE branch = lwa_romaneio_sel-branch.
          wa_chaves-sign   = 'I'.
          wa_chaves-option = 'EQ'.
          wa_chaves-low    = wa_romaneio-ch_referencia.
          wa_chaves-high   = wa_romaneio-ch_referencia.
          APPEND wa_chaves TO rg_chaves.
        ENDLOOP.

        SUBMIT zlesr0102 WITH p_bukrs  EQ lwa_romaneio_sel-bukrs
                         WITH p_branch EQ lwa_romaneio_sel-branch
                         WITH r_cp_01  EQ abap_false
                         WITH r_cp_05  EQ abap_true
                         WITH r_dt_a   EQ abap_false
                         WITH r_dt_t   EQ abap_true
                         WITH s_chave  IN rg_chaves
                         WITH p_inter  EQ '48'
                         AND RETURN.

      WHEN 'ESTORNAROM'.

        DATA(r_msg_error) = zcl_carga_saida_insumos=>estornar_romaneios( i_nro_cg = gwa_cab_carga_saida-nro_cg ).

        IF r_msg_error IS NOT INITIAL.
          MESSAGE r_msg_error TYPE 'S' DISPLAY LIKE 'E'.
          RETURN.
        ENDIF.

        PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

        CALL METHOD gob_grid_7200_romaneios->refresh_table_display
          EXPORTING
            is_stable = it_stable.

    ENDCASE.


  ENDMETHOD.                    "USER_COMMAND

ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION



CLASS lcl_carga_7200_ov_lotes IMPLEMENTATION.

  METHOD on_double_click_ordem.

    DATA: vl_row TYPE i.

    PERFORM f_alv_ordem_click_7200 USING e_row-index.

    CALL METHOD gob_grid_7200_lotes->refresh_table_display
      EXPORTING
        is_stable = _stable.

  ENDMETHOD.

  METHOD user_command.

    CASE e_ucomm .
      WHEN 'PEDIDOS'.

*        IF it_ordem_5320 IS NOT INITIAL.
*          CALL SCREEN 5322 STARTING AT 5 5 ENDING AT 80 20.
*          LEAVE TO SCREEN 5000.
*        ENDIF.

    ENDCASE.

  ENDMETHOD.                    "USER_COMMAND


  METHOD toolbar_ordem.

*    DATA wa_tool TYPE stb_button.
*
*    MOVE 3 TO wa_tool-butn_type.
*    APPEND wa_tool TO e_object->mt_toolbar.
*    CLEAR wa_tool.
*
*    wa_tool-function = 'PEDIDOS'.
*    wa_tool-icon     = '@BB@'.
*    wa_tool-quickinfo = 'Listar Pedidos'.
*    APPEND wa_tool TO e_object->mt_toolbar.
*    CLEAR wa_tool.
*
  ENDMETHOD.             "DISPLAY



ENDCLASS.                    "LCL_EVENT_HANDLER IMPLEMENTATION


FORM f_alv_ordem_click_7200 USING p_e_row_index.

  READ TABLE git_7200_ov_lotes INTO DATA(lwa_ov_lote) INDEX p_e_row_index.
  CHECK sy-subrc IS INITIAL.

  zcl_carga_saida_insumos=>busca_dados_carga(
    EXPORTING
      i_nr_carga_single = gwa_cab_carga_saida-nro_cg
    IMPORTING
      e_lotes           = DATA(lit_lotes)
  ).

  DELETE lit_lotes WHERE NOT ( vbeln  EQ lwa_ov_lote-vbeln AND
                               posnr  EQ lwa_ov_lote-posnr AND
                               nr_rot EQ lwa_ov_lote-nr_rot ).

  MOVE-CORRESPONDING lit_lotes[] TO git_7200_lotes[].

  gva_vbeln_lote_sel   = lwa_ov_lote-vbeln.
  gva_posnr_lote_sel   = lwa_ov_lote-posnr.
  gva_nr_rot_lote_sel  = lwa_ov_lote-nr_rot.


ENDFORM.

FORM f_vincular_pedidos_7200 .

  DATA: it_selected_rows  TYPE lvc_t_row,
        t_row_no          TYPE lvc_t_roid,
        wa_selected_rows  TYPE lvc_s_row,
        it_zsdt0129       TYPE STANDARD TABLE OF zsdt0129,
        wa_zsdt0129       TYPE zsdt0129,
        it_zsdt0133       TYPE STANDARD TABLE OF zsdt0133,
        wa_zsdt0133       TYPE zsdt0133,
        it_carga_aux_5230 TYPE STANDARD TABLE OF ty_carga_5230,
        wa_carga_5230     TYPE ty_carga_5230,
        it_sol_aux_5230   TYPE STANDARD TABLE OF ty_sol_5230,
        wa_sol_aux_5230   TYPE ty_sol_5230,
        wa_sol_5230       TYPE ty_sol_5230,
        it_rsparams       TYPE TABLE OF rsparams,
        wa_rsparams       TYPE rsparams,
        it_zsdt0062       TYPE STANDARD TABLE OF zsdt0062,
        wa_zsdt0062       TYPE zsdt0062,
        vl_lines          TYPE i,
        vl_spart          TYPE zsdt0131-spart,
        vl_vkorg          TYPE zsdt0131-vkorg,
        vl_check          TYPE char1,
        vl_check2         TYPE char1,
        vl_block          TYPE sy-tabix,
        vl_vinc1          TYPE zsdt0131-qtd_vinc,
        vl_vinc2          TYPE zsdt0131-qtd_vinc,
        wa_edit_cg_5230   TYPE ty_edit_cg_5230,
        vl_cont           TYPE i,
        vl_index          TYPE i,
        wa_sol_click_5230 TYPE ty_sol_5230.

  READ TABLE git_7200_solicitacoes INTO DATA(w_sol_click_5230) INDEX 1.

  SELECT SINGLE mtart INTO @DATA(_mtart) FROM mara WHERE matnr EQ @w_sol_click_5230-matnr.
  IF sy-subrc IS NOT INITIAL OR _mtart IS INITIAL.
    MESSAGE | Tipo de material do material { w_sol_click_5230-matnr } não encontrado| TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0281 INTO @DATA(it_zsdt0281)
   WHERE werks EQ @w_sol_click_5230-werks
     AND spart EQ @w_sol_click_5230-spart
     AND mtart EQ @_mtart.

  IF sy-subrc EQ 0.
    TRY.
        PERFORM validar_material
          USING
            w_sol_click_5230-matnr  "//Material
            w_sol_click_5230-spart    "//St. Atividade: Sementes
            _mtart. "//Tipo Material: Importação

        TRY.
            PERFORM call_popup_vinculacao.
          CATCH cx_abap_util_exception.
            EXIT.
        ENDTRY.

      CATCH cx_abap_util_exception.
    ENDTRY.
  ENDIF.

  SELECT SINGLE *
    FROM zsdt0133 INTO wa_zsdt0133
   WHERE nro_cg EQ gwa_cab_carga_saida-nro_cg.

  IF sy-subrc NE 0.
    MESSAGE 'Carga não localizada!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF gwa_cab_carga_saida-status GE 5.
    MESSAGE TEXT-070 TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'ZENQUEUE_SD_CARGA_INSUMOS'
    EXPORTING
      chave          = gwa_cab_carga_saida-nro_cg
    EXCEPTIONS
      foreign_lock   = 1
      system_failure = 2
      OTHERS         = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  "// Busca dados de Local de Entrega para validação
  SELECT c~*
    FROM zsdt0129 AS a
    INNER JOIN zsdt0131 AS b ON b~nro_lote EQ a~nro_lote
    INNER JOIN zsdt0132 AS c ON c~nr_rot EQ b~cod_loc_emb
    INTO TABLE @t_0132
    WHERE a~nro_cg EQ @gwa_cab_carga_saida-nro_cg.

  "// Se o Local de Embarque for do Tipo Armazem não Imprimir Autorização de Embarque
  IF line_exists( t_0132[ armazem = abap_true ] ).
    MESSAGE TEXT-140 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  "Nr. Carga
  wa_rsparams-selname = 'P_NROCG'.
  wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
  wa_rsparams-sign = 'I'.
  wa_rsparams-option = 'EQ'.
  wa_rsparams-low    = gwa_cab_carga_saida-nro_cg.
  APPEND wa_rsparams TO it_rsparams.
  CLEAR wa_rsparams.

  "Dt. Lançamento
  wa_rsparams-selname = 'P_ERDAT'.
  wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
  wa_rsparams-sign = 'I'.
  wa_rsparams-option = 'BT'.
  wa_rsparams-low    = 20150101.
  wa_rsparams-high   = sy-datum.
  APPEND wa_rsparams TO it_rsparams.
  CLEAR wa_rsparams.

  LOOP AT git_7200_solicitacoes INTO DATA(lwa_solicitacao).
    vl_spart = lwa_solicitacao-spart.
    vl_vkorg = lwa_solicitacao-vkorg.
    "Nr. Material
    wa_rsparams-selname = 'P_MATNR'.
    wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
    wa_rsparams-sign = 'I'.
    wa_rsparams-option = 'EQ'.
    wa_rsparams-low    = lwa_solicitacao-matnr.
    APPEND wa_rsparams TO it_rsparams.
    CLEAR wa_rsparams.
  ENDLOOP.

  "Nr. OrgaNização de Vendas
  wa_rsparams-selname = 'P_VKORG'.
  wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
  wa_rsparams-sign = 'I'.
  wa_rsparams-option = 'EQ'.
  wa_rsparams-low    = vl_vkorg.
  APPEND wa_rsparams TO it_rsparams.
  CLEAR wa_rsparams.

  "Nr. Setor Ativ.
  wa_rsparams-selname = 'P_SPART'.
  wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
  wa_rsparams-sign = 'I'.
  wa_rsparams-option = 'EQ'.
  wa_rsparams-low    = vl_spart.
  APPEND wa_rsparams TO it_rsparams.
  CLEAR wa_rsparams.

  IF vl_spart EQ '04'.
    "Tipo pedido de Compra.
    wa_rsparams-selname = 'P_BSART'.
    wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
    wa_rsparams-sign = 'I'.
    wa_rsparams-option = 'EQ'.
    wa_rsparams-low    = 'ZSEM'.
    APPEND wa_rsparams TO it_rsparams.
    CLEAR wa_rsparams.
    "Tipo pedido de Compra.
    wa_rsparams-selname = 'P_BSART'.
    wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
    wa_rsparams-sign = 'I'.
    wa_rsparams-option = 'EQ'.
    wa_rsparams-low    = 'ZOSM'.
    APPEND wa_rsparams TO it_rsparams.
    CLEAR wa_rsparams.

    "Tipo pedido de Compra.
    wa_rsparams-selname = 'P_BSART'.
    wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
    wa_rsparams-sign = 'I'.
    wa_rsparams-option = 'EQ'.
    wa_rsparams-low    = 'ZSON'.
    APPEND wa_rsparams TO it_rsparams.
    CLEAR wa_rsparams.
    "Tipo pedido de Compra.
    wa_rsparams-selname = 'P_BSART'.
    wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
    wa_rsparams-sign = 'I'.
    wa_rsparams-option = 'EQ'.
    wa_rsparams-low    = 'ZEFI'.
    APPEND wa_rsparams TO it_rsparams.
    CLEAR wa_rsparams.
    wa_rsparams-selname = 'P_BSART'.
    wa_rsparams-kind = 'S'.  "SELECT OPTIONS TO BE PASSED
    wa_rsparams-sign = 'I'.
    wa_rsparams-option = 'EQ'.
    wa_rsparams-low    = 'ZIMP'.
    APPEND wa_rsparams TO it_rsparams.
    CLEAR wa_rsparams.
  ENDIF.

  SUBMIT zsdr0062 WITH SELECTION-TABLE it_rsparams AND RETURN.

  CALL FUNCTION 'ZDENQUEUE_SD_CARGA_INSUMOS'
    EXPORTING
      chave = gwa_cab_carga_saida-nro_cg.

ENDFORM.

MODULE help_roteiro_pc_7200 INPUT.

  DATA: gt_return_tab TYPE TABLE OF ddshretval WITH HEADER LINE,
        gt_dselc      TYPE TABLE OF dselc      WITH HEADER LINE.

  DATA: lit_roteiro_pc TYPE zsds385_t.

  CLEAR: gwa_set_ponto_coleta_7200-nr_rot, lit_roteiro_pc[], gt_return_tab[].

  IF gwa_set_ponto_coleta_7200-lifnr IS INITIAL.
    MESSAGE 'Informar o Ponto de coleta!' TYPE 'S'.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_set_ponto_coleta_7200-lifnr
    IMPORTING
      output = gwa_set_ponto_coleta_7200-lifnr.

  SELECT *
    FROM zsdt0132 INTO CORRESPONDING FIELDS OF TABLE lit_roteiro_pc
    WHERE lifnr EQ gwa_set_ponto_coleta_7200-lifnr.

  SORT lit_roteiro_pc BY nr_rot.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'NR_ROT'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'GWA_SET_PONTO_COLETA_7200-NR_ROT'
      value_org       = 'S'
    TABLES
      value_tab       = lit_roteiro_pc
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  READ TABLE gt_return_tab WITH KEY retfield = 'GWA_SET_PONTO_COLETA_7200-NR_ROT'.
  IF sy-subrc = 0.
    READ TABLE lit_roteiro_pc INTO DATA(lwa_rot) WITH KEY nr_rot = gt_return_tab-fieldval.
    IF sy-subrc = 0.
      gwa_set_ponto_coleta_7200-nr_rot   = lwa_rot-nr_rot.
      gwa_set_ponto_coleta_7200-rot_desc = lwa_rot-rot_desc.
    ENDIF.
  ENDIF.

ENDMODULE.

MODULE status_7210 OUTPUT.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_set_ponto_coleta_7200-nr_rot
    IMPORTING
      output = gwa_set_ponto_coleta_7200-nr_rot.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      input  = gwa_set_ponto_coleta_7200-lifnr
    IMPORTING
      output = gwa_set_ponto_coleta_7200-lifnr.

  CLEAR: gwa_set_ponto_coleta_7200-name_pc.
  IF gwa_set_ponto_coleta_7200-lifnr IS NOT INITIAL.
    SELECT SINGLE name1
       FROM lfa1 INTO gwa_set_ponto_coleta_7200-name_pc
      WHERE lifnr EQ gwa_set_ponto_coleta_7200-lifnr.

    IF sy-subrc NE 0.
      CLEAR: gwa_set_ponto_coleta_7200-lifnr.
      MESSAGE 'Fornecedor não encontrado!' TYPE 'S'.
    ENDIF.
  ENDIF.

  CLEAR: gwa_set_ponto_coleta_7200-rot_desc.
  IF gwa_set_ponto_coleta_7200-nr_rot IS NOT INITIAL.
    SELECT SINGLE rot_desc
       FROM zsdt0132 INTO gwa_set_ponto_coleta_7200-rot_desc
      WHERE nr_rot EQ gwa_set_ponto_coleta_7200-nr_rot.

    IF sy-subrc NE 0.
      CLEAR: gwa_set_ponto_coleta_7200-nr_rot.
      MESSAGE 'Roteiro não encontrado!' TYPE 'S'.
    ENDIF.
  ENDIF.

  SET PF-STATUS 'PF7210'.
  SET TITLEBAR 'TITLE_7210'.
ENDMODULE.

MODULE user_command_7210 INPUT.

  DATA: lw_sel_hide TYPE slis_sel_hide_alv.

  CASE sy-ucomm.
    WHEN 'CONFIRM'.

      IF gwa_set_ponto_coleta_7200-lifnr IS INITIAL.
        MESSAGE 'Ponto de coleta não informado!' TYPE 'S'.
        RETURN.
      ENDIF.

      IF gwa_set_ponto_coleta_7200-nr_rot IS INITIAL.
        MESSAGE 'Roteiro não informado!' TYPE 'S'.
        RETURN.
      ENDIF.

      SELECT SINGLE *
        FROM zsdt0132 INTO @DATA(lwa_zsdt0132)
       WHERE nr_rot EQ @gwa_set_ponto_coleta_7200-nr_rot
         AND lifnr  EQ @gwa_set_ponto_coleta_7200-lifnr.

      IF sy-subrc NE 0.
        MESSAGE |Roteiro { gwa_set_ponto_coleta_7200-nr_rot } não pertence ao Fornecedor { gwa_set_ponto_coleta_7200-lifnr } | TYPE 'S'.
        RETURN.
      ENDIF.

      LOOP AT git_sel_rows_7200 ASSIGNING FIELD-SYMBOL(<fs_rows>).
        READ TABLE git_monta_carga_saida_sem INTO DATA(lwa_saida_mont_carga) INDEX <fs_rows>-index.
        CHECK sy-subrc EQ 0.

        SELECT SINGLE *
          FROM zsdt0131 INTO @DATA(lwa_zsdt0131)
         WHERE nro_sol EQ @lwa_saida_mont_carga-nro_sol
           AND seq     EQ @lwa_saida_mont_carga-seq
           AND vbeln   EQ @lwa_saida_mont_carga-vbeln
           AND posnr   EQ @lwa_saida_mont_carga-posnr
           AND status  NE 'X'.
**<<<------"169508 - NMS - INI------>>>
*        IF sy-subrc EQ 0.
        IF sy-subrc                   IS INITIAL AND
           lwa_saida_mont_carga-saldo IS INITIAL.
**<<<------"169508 - NMS - FIM------>>>
          MESSAGE |Solicitação { lwa_saida_mont_carga-nro_sol } Item: { lwa_saida_mont_carga-seq } já esta vinculada em Cargas! Operação não permitida!| TYPE 'S'.
          RETURN.
        ENDIF.

        UPDATE zsdt0082 SET nr_rot_pc = gwa_set_ponto_coleta_7200-nr_rot
         WHERE nro_sol = lwa_saida_mont_carga-nro_sol
           AND seq     = lwa_saida_mont_carga-seq
           AND vbeln   = lwa_saida_mont_carga-vbeln
           AND posnr   = lwa_saida_mont_carga-posnr.
      ENDLOOP.

      MESSAGE 'Roteiro de ponto de Coleta das solicitações atualizado com sucesso!' TYPE 'S'.

      PERFORM f_sel_dados_mont_carga_7200 CHANGING lva_msg_error.

      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          es_sel_hide = lw_sel_hide
          e_grid      = ref_7200.

      IF ref_7200 IS BOUND.
        ref_7200->refresh_table_display( ).
      ENDIF.

      LEAVE TO SCREEN 0.

    WHEN 'CANCELAR'.
      LEAVE TO SCREEN 0.

  ENDCASE.

ENDMODULE.

FORM f_sel_dados_mont_carga_7200 CHANGING r_msg_error TYPE string.

  CLEAR: r_msg_error.

  zcl_carga_saida_insumos=>busca_dados_montar_carga(
        EXPORTING
          i_vkbur              = s_vkbur[]
          i_vkorg              = s_vkorg[]
          i_inco1              = s_inco1-low
          i_spart              = CONV #( gv_spart )
          i_kunnr              = s_kunnr[]
          i_nro_sol            = s_nrsol[]
          i_dt_lib_sol         = s_dtlsol[]
          i_ordem_venda        = s_ordem[]
          i_tipo_saldo         = 'M'
        IMPORTING
          e_tabela_monta_carga = git_monta_carga_saida_sem
          e_msg_erro           = r_msg_error

      ).

ENDFORM.
**<<<------"169508 - NMS - INI------>>>
*&---------------------------------------------------------------------*
*& Form f_gera_cotacao_7200
*&---------------------------------------------------------------------*
*& Gera Cotação de frete
*&---------------------------------------------------------------------*
*&     -->PT_SEL_ROWS TI de linhas selecionadas no ALV
*&---------------------------------------------------------------------*
FORM f_gera_cotacao_7200 TABLES pt_sel_rows STRUCTURE lvc_s_row.

  DATA: rl_nro_cg TYPE zsdt_range_nro_cg.

  LOOP AT pt_sel_rows.
    READ TABLE git_lista_carga_saida_sem INTO DATA(el_lista_carga_saida_sem) INDEX pt_sel_rows-index.
    CHECK sy-subrc EQ 0.
    APPEND VALUE zsde_range_nro_cg( sign = 'I' option = 'EQ' low = el_lista_carga_saida_sem-nro_cg ) TO rl_nro_cg.
  ENDLOOP.

  zcl_carga_saida_insumos=>gerar_cotacao( EXPORTING it_nro_cg = rl_nro_cg IMPORTING e_nro_cg_sucess = DATA(lra_nro_range_sucess) RECEIVING r_msg_error = lva_msg_error ).

  IF lra_nro_range_sucess IS NOT INITIAL.

    IF lva_msg_error IS NOT INITIAL.
      MESSAGE |Algumas cargas não foram para o processo de cotação: Msg.: { lva_msg_error } | TYPE 'I'.
    ENDIF.

    PERFORM gerar_excel_cotacao TABLES lra_nro_range_sucess.
  ELSE.
    MESSAGE lva_msg_error TYPE 'I'.
    RETURN.
  ENDIF.

* Refazer a consulta para atualizar os dados na GRID do ALV.
  zcl_carga_saida_insumos=>busca_dados_carga(
    EXPORTING
      i_nr_carga    =  s_carga[]
      i_vkorg       =  s_vkorg[]
      i_vkbur       =  s_vkbur[]
      i_spart       =  CONV #( gv_spart )
      i_kunnr       =  s_kunnr[]
      i_nro_sol     =  s_nrsol[]
      i_id_viagem   =  s_idvgm[]
      i_id_carga_safra_ctrl = s_id_sf[]
      i_ordem_venda =  s_ordem[]
      i_dt_carga    =  s_dtcar[]
   IMPORTING
      e_cargas      =  git_lista_carga_saida_sem ).

  IF ref_7200_02 IS BOUND.
    ref_7200_02->refresh_table_display( ).
  ELSE.
    CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
      IMPORTING
        es_sel_hide = lw_sel_hide
        e_grid      = ref_7200_02.

    ref_7200_02->refresh_table_display( ).

  ENDIF.

ENDFORM.
**<<<------"169508 - NMS - FIM------>>>

FORM f_exec_troca_nota_7200 .

  DATA(lva_msg_error) = zcl_carga_saida_insumos=>executar_troca_nota( EXPORTING i_nro_cg  =  gwa_cab_carga_saida-nro_cg ).

  IF lva_msg_error IS NOT INITIAL.
    MESSAGE lva_msg_error TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

ENDFORM.

FORM f_env_documentos_carguero_7200.

  DATA(lva_msg_error) = zcl_carga_saida_insumos=>enviar_documentos_carguero( EXPORTING i_nro_cg  =  gwa_cab_carga_saida-nro_cg ).

  IF lva_msg_error IS NOT INITIAL.
    MESSAGE lva_msg_error TYPE 'I'.
    RETURN.
  ENDIF.

  PERFORM f_refresh_inf_carga_7200 USING gwa_cab_carga_saida-nro_cg abap_true.

ENDFORM.

FORM f_imprimir_nfe_viagem USING p_chave_nfe
                                 p_viagem_id .

  DATA: lwa_response_consulta TYPE zlese0274.

  DATA: lwa_zlest185   TYPE zlest0185.

  CHECK p_viagem_id IS NOT INITIAL AND p_chave_nfe IS NOT INITIAL.

  CLEAR: lwa_zlest185.

  lwa_zlest185-viagem_id = p_viagem_id.

  TRY.
      zcl_int_ob_get_viagem_carguero=>zif_integracao_outbound~get_instance(
                         )->execute_request( EXPORTING i_info_request = lwa_zlest185
                                             IMPORTING e_integracao   = DATA(lwa_zintegracao) ).

      CHECK lwa_zintegracao-ds_data_retorno IS NOT INITIAL.

      /ui2/cl_json=>deserialize( EXPORTING json = lwa_zintegracao-ds_data_retorno CHANGING data = lwa_response_consulta ).

      READ TABLE lwa_response_consulta-documents-fiscal INTO DATA(lwa_document_fiscal) WITH KEY access_key = p_chave_nfe.
      IF sy-subrc NE 0.
        MESSAGE |NF-e { p_chave_nfe } não foi anexada na viagem { p_viagem_id } no sistema Carguero!| TYPE 'S'.
        RETURN.
      ENDIF.

      IF lwa_document_fiscal-url IS INITIAL.
        MESSAGE |NF-e { p_chave_nfe } sem link de impressão no Carguero!| TYPE 'S'.
        RETURN.
      ENDIF.

      cl_gui_frontend_services=>execute(
         EXPORTING
           application            = CONV #( lwa_document_fiscal-url )
         EXCEPTIONS
           cntl_error             = 1
           error_no_gui           = 2
           bad_parameter          = 3
           file_not_found         = 4
           path_not_found         = 5
           file_extension_unknown = 6
           error_execute_failed   = 7
           synchronous_failed     = 8
           not_supported_by_gui   = 9
           OTHERS                 = 10  ).

    CATCH zcx_integracao INTO DATA(lwa_zcx_integracao). " Classe de Erro de Integração
      MESSAGE |Não foi possivel consultar a viagem { p_viagem_id } no Carguero'| TYPE 'S'.
      RETURN.
    CATCH zcx_error INTO DATA(lwa_zcx_error). " Classe de Erro de Integração
      MESSAGE |Não foi possivel consultar a viagem { p_viagem_id } no Carguero'| TYPE 'S'.
      RETURN.
  ENDTRY.


ENDFORM.

FORM f_reenv_transp_safra_7200.

  SELECT SINGLE *
    FROM zsdt0133 INTO @DATA(lwa_zsdt0133)
   WHERE nro_cg EQ @gwa_cab_carga_saida-nro_cg.

  CHECK sy-subrc EQ 0.

  IF lwa_zsdt0133-id_carga_safra_control IS INITIAL.
    MESSAGE 'Carga não originada do Safra Control!' TYPE 'S'.
    RETURN.
  ENDIF.

  IF lwa_zsdt0133-int_dados_transp_safra_ctrl IS INITIAL.
    MESSAGE 'Dados transporte da Carga ainda não foram integradsos ao Safra Control!' TYPE 'S'.
    RETURN.
  ENDIF.

  UPDATE zsdt0133 SET int_dados_transp_safra_ctrl = space
   WHERE nro_cg EQ gwa_cab_carga_saida-nro_cg.

  MESSAGE 'Dados de transporte da Carga serão reenviados para o Safra Control!' TYPE 'S'.

ENDFORM.

*&---------------------------------------------------------------------*
*&  Include           ZSDR0101_FORM
*&---------------------------------------------------------------------*

FORM f_grava_zsdt0287 TABLES t_zsdt0287 STRUCTURE zsdt0287
                       USING p_zsdt0287      TYPE zsdt0287.

  DATA: w_cdhdr  TYPE cdhdr,
        t_cdtxt  TYPE TABLE OF cdtxt,
        w_cdtxt  TYPE cdtxt,
        l_id_obs TYPE zsdt0287-id_obs.

  w_cdhdr-objectclas = 'ZSDT0287'.
  w_cdhdr-username   = sy-uname.
  w_cdhdr-udate      = sy-datum.
  w_cdhdr-utime      = sy-uzeit.
  w_cdhdr-tcode      = sy-tcode.

  SELECT *
    FROM zsdt0287
    INTO TABLE @DATA(t_0287_old).

*-BUG 65582-17.05.2022-JT-inicio
  IF t_zsdt0287[] IS NOT INITIAL OR p_zsdt0287 IS NOT INITIAL.
    IF t_zsdt0287[] IS INITIAL.
      MODIFY zsdt0287 FROM p_zsdt0287.
    ELSE.
      FREE: l_id_obs.
      SELECT SINGLE MAX( id_obs )
        INTO l_id_obs
        FROM zsdt0287.

      LOOP AT t_zsdt0287 INTO DATA(w_zsdt0287).
        IF w_zsdt0287-id_obs IS INITIAL.
          l_id_obs          = l_id_obs + 1.
          w_zsdt0287-id_obs = l_id_obs.
        ENDIF.
        MODIFY zsdt0287  FROM w_zsdt0287.
      ENDLOOP.
*     MODIFY zsdt0287 FROM TABLE t_zsdt0287.
    ENDIF.
  ENDIF.
*-BUG 65582-17.05.2022-JT-fim

  COMMIT WORK AND WAIT.

  SELECT *
    FROM zsdt0287
    INTO TABLE @DATA(t_0287_new).

  SORT t_0287_old BY id_obs.
  SORT t_0287_new BY id_obs.

  LOOP AT t_0287_old INTO DATA(w_0287_old).
    w_cdhdr-change_ind = abap_false.
    w_cdhdr-objectid   = sy-mandt && w_0287_old-id_obs.

    READ TABLE t_0287_new INTO DATA(w_0287_new) WITH KEY id_obs = w_0287_old-id_obs
                               BINARY SEARCH.
    IF sy-subrc <> 0.
      w_cdhdr-change_ind   = 'D'.
    ELSE.
      IF w_0287_old <> w_0287_new.
        w_cdhdr-change_ind = 'U'.
      ENDIF.
    ENDIF.

    CHECK w_cdhdr-change_ind IS NOT INITIAL.

    FREE: t_cdtxt.
    w_cdtxt-teilobjid   = w_cdhdr-objectid.
    w_cdtxt-textspr     = sy-langu.
    w_cdtxt-updkz       = w_cdhdr-change_ind.
    APPEND w_cdtxt     TO t_cdtxt.

    CALL FUNCTION 'ZSDT0287_WRITE_DOCUMENT'
      EXPORTING
        objectid        = w_cdhdr-objectid
        tcode           = w_cdhdr-tcode
        utime           = w_cdhdr-utime
        udate           = w_cdhdr-udate
        username        = w_cdhdr-username
        n_zsdt0287      = w_0287_new
        o_zsdt0287      = w_0287_old
        upd_zsdt0287    = w_cdhdr-change_ind
      TABLES
        icdtxt_zsdt0287 = t_cdtxt.
  ENDLOOP.

  LOOP AT t_0287_new INTO w_0287_new.

    w_cdhdr-change_ind = abap_false.
    w_cdhdr-objectid   = sy-mandt && w_0287_new-id_obs.

    CLEAR w_0287_old.
    READ TABLE t_0287_old INTO w_0287_old WITH KEY id_obs = w_0287_new-id_obs
                               BINARY SEARCH.
    CHECK sy-subrc <> 0.

    w_cdhdr-change_ind  = 'I'.

    FREE: t_cdtxt.
    w_cdtxt-teilobjid   = w_cdhdr-objectid.
    w_cdtxt-textspr     = sy-langu.
    w_cdtxt-updkz       = w_cdhdr-change_ind.
    APPEND w_cdtxt     TO t_cdtxt.

    CALL FUNCTION 'ZSDT0287_WRITE_DOCUMENT'
      EXPORTING
        objectid        = w_cdhdr-objectid
        tcode           = w_cdhdr-tcode
        utime           = w_cdhdr-utime
        udate           = w_cdhdr-udate
        username        = w_cdhdr-username
        n_zsdt0287      = w_0287_new
        o_zsdt0287      = w_0287_old
        upd_zsdt0287    = w_cdhdr-change_ind
      TABLES
        icdtxt_zsdt0287 = t_cdtxt.
  ENDLOOP.

  COMMIT WORK AND WAIT.

ENDFORM.

FORM f_refresh_alv USING p_alv.

  CASE p_alv.
    WHEN '0100'.
      CALL METHOD obj_alv_0100->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN '0120'.
      CALL METHOD obj_alv_0120->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN '0130'.
      CALL METHOD obj_alv_0130->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN '0140'.
      CALL METHOD obj_alv_0140->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN '0150'.
      CALL METHOD obj_alv_0150->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.

ENDFORM.

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  FREE: wa_fcat, it_fcat.

  CASE p_screen.
    WHEN '0100'.

      PERFORM f_estrutura_alv USING:

       01  'ZSDT0006'      'AUART'              'IT_SAIDA_0100'  'AUART'                 'Tp.O.V'        '06'   'X'    ''  ' ' 'C' ' ' ' ' ' ' ,
       01  'ZSDT0006'      'BRANCH'             'IT_SAIDA_0100'  'BRANCH'                'Filial'        '06'   'X'    ''  ' ' 'C' ' ' ' ' ' ' ,
       02  'ZSDT0006'      'SAFRA'              'IT_SAIDA_0100'  'SAFRA'                 'Safra'         '05'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       03  'ZSDT0006'      'NUM_ORD'            'IT_SAIDA_0100'  'NUM_ORD'               'Num.Ord'       '07'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       04  'ZSDT0006'      'DIR_FISC'           'IT_SAIDA_0100'  'DIR_FISC'              'Dir.Fisc'      '08'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       05  'ZSDT0006'      'NUM_LEIL'           'IT_SAIDA_0100'  'NUM_LEIL'              'Num.Leilão'    '10'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'DCO'                'IT_SAIDA_0100'  'DCO'                   'DCO'           '03'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'NAVIO'              'IT_SAIDA_0100'  'NAVIO'                 'Navio'         '05'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'BL'                 'IT_SAIDA_0100'  'BL'                    'BL'            '02'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'RE'                 'IT_SAIDA_0100'  'RE'                    'RE'            '02'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'DDE'                'IT_SAIDA_0100'  'DDE'                   'DDE'           '03'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'NF_COMPRA'          'IT_SAIDA_0100'  'NF_COMPRA'             'NF.Compra'     '09'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'CNPJ'               'IT_SAIDA_0100'  'CNPJ'                  'CNPJ'          '04'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'QUANT'              'IT_SAIDA_0100'  'QUANT'                 'Quantidade'    '10'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'NF_LOTE'            'IT_SAIDA_0100'  'NF_LOTE'               'NF.Lote'       '07'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'DADOS_TER'          'IT_SAIDA_0100'  'DADOS_TER'             'Terminal'      '08'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'DADOS_TRANS'        'IT_SAIDA_0100'  'DADOS_TRANS'           'Transbordo'    '10'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'FACS'               'IT_SAIDA_0100'  'FACS'                  'Facs'          '04'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'FETHAB'             'IT_SAIDA_0100'  'FETHAB'                'FETHAB'        '06'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'PLACA_CAV'          'IT_SAIDA_0100'  'PLACA_CAV'             'Placa.Veic.'   '11'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'PLACA_1'            'IT_SAIDA_0100'  'PLACA_1'               'P.Carreta 1'   '11'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'PLACA_2'            'IT_SAIDA_0100'  'PLACA_2'               'P.Carreta 2'   '11'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'PLACA_3'            'IT_SAIDA_0100'  'PLACA_3'               'P.Carreta 3'   '11'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'QTD_FAR'            'IT_SAIDA_0100'  'QTD_FAR'               'Qtd.Fardos'    '10'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'NUM_LOTE'           'IT_SAIDA_0100'  'NUM_LOTE'              'Num.Lote'      '08'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'LC_RET'             'IT_SAIDA_0100'  'LC_RET'                'Lcl.Ret.'      '08'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'TX_FATURA'          'IT_SAIDA_0100'  'TX_FATURA'             'Tx.Fatura'     '09'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'TX_OV'              'IT_SAIDA_0100'  'TX_OV'                 'Tx.O.V.'       '07'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'TX_ITEM_OV'         'IT_SAIDA_0100'  'TX_ITEM_OV'            'Tx.Itm.OV'     '09'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'TX_MATERIAL'        'IT_SAIDA_0100'  'TX_MATERIAL'           'Tx.Material'   '11'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'TX_ITEM_OV_ITM_'    'IT_SAIDA_0100'  'TX_ITEM_OV_ITM_'       'Tx.Itm.NFe'    '10'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'NUM_LOTE_ITEM'      'IT_SAIDA_0100'  'NUM_LOTE_ITEM'         'Nr.Lote.Itm'   '11'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'TX_OBS_ROM'         'IT_SAIDA_0100'  'TX_OBS_ROM'            'Obs.Rom.'      '08'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'TX_ITEM_REM'        'IT_SAIDA_0100'  'TX_ITEM_REM'           'Txt.It.Rem'    '10'   'X'    ''  ' ' 'C' ' ' ' ' 'X' ,
       06  'ZSDT0006'      'TX_ITEM_MATERIAL'   'IT_SAIDA_0100'  'TX_ITEM_MATERIAL'      'Txt.It.Mat.'   '11'   'X'    ''  ' ' 'C' ' ' ' ' 'X' .


    WHEN '0120'.

      PERFORM f_estrutura_alv USING:

       01  'ZSDT0294'      'BRANCH'            'IT_SAIDA_0120'  'BRANCH'              'Filial'                               '06'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0294'      'OBSERV'            'IT_SAIDA_0120'  'OBSERV'              'Observ.'                              '100'  'X'    ''  ' ' ' ' ' ' ' ' '' ,
       03  'ZSDT0294'      'OBSERV2'           'IT_SAIDA_0120'  'OBSERV2'             'Observ.2'                             '100'  'X'    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZSDT0294'      'DT_REGISTRO'       'IT_SAIDA_0120'  'DT_REGISTRO'         'Dt.Registro'                          '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZSDT0294'      'HR_REGISTRO'       'IT_SAIDA_0120'  'HR_REGISTRO'         'Hr.Registro'                          '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'ZSDT0294'      'US_REGISTRO'       'IT_SAIDA_0120'  'US_REGISTRO'         'Us.Registro'                          '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .


    WHEN '0130'.
      PERFORM f_estrutura_alv USING:

       01  'ZSDT0287'      'ID_OBS'            'IT_SAIDA_0130'  'ID_OBS'              'ID OBS'                                '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0287'      'OBSERV'            'IT_SAIDA_0130'  'OBSERV'              'OBSERVAÇÃO 1'                          '120'  'X'    ''  ' ' ' ' ' ' ' ' '' ,
       03  'ZSDT0287'      'OBSERV2'           'IT_SAIDA_01230'  'OBSERV2'            'OBSERVAÇÃO 2'                          '120'  'X'    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZSDT0287'      'DT_REGISTRO'       'IT_SAIDA_0130'  'DT_REGISTRO'         'Dt.Registro'                           '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZSDT0287'      'HR_REGISTRO'       'IT_SAIDA_0130'  'HR_REGISTRO'         'HR.Registro'                           '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'ZSDT0287'      'US_REGISTRO'       'IT_SAIDA_0130'  'US_REGISTRO'         'Usuário'                               '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .
      " 06  'ZSDT0287'     'US_REGISTRO'      'IT_SAIDA_0130'  'US_REGISTRO'         'Us.Registro'                           '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0140'.
      PERFORM f_estrutura_alv USING:

       01  'ZSDT0294'      'AUART'             'IT_SAIDA_0140'  'AUART'              'Tp. O.V'                                '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0294'      'BRANCH'            'IT_SAIDA_0140'  'BRANCH'             'Filial'                                 '06'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
*-CS2022000324-25.08.2022-#84903-JT-inicio
       03  'ZSDT0294'      'MATKL'             'IT_SAIDA_0140'  'MATKL'              'Grp.Material'                           '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       04  'T023T'         'WGBEZ'             'IT_SAIDA_0140'  'WGBEZ'              'Desc.Grp.Material'                      '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*-CS2022000324-25.08.2022-#84903-JT-fim
       05  'ZSDT0294'      'MATNR'             'IT_SAIDA_0140'  'MATNR'              'Cod.Material'                           '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       06  'MAKT'          'MAKTG'             'IT_SAIDA_0140'  'MAKTG'              'Desc.Material'                          '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       07  'ZSDT0287'      'ID_OBS'            'IT_SAIDA_0140'  'ID_OBS'             'ID OBS'                                 '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       08  'ZSDT0287'      'OBSERV'            'IT_SAIDA_0140'  'OBSERV_287'         'Observação'                             '50'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       09  'ZSDT0294'      'DT_REGISTRO'       'IT_SAIDA_0140'  'DT_REGISTRO'        'Dt.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'ZSDT0294'      'HR_REGISTRO'       'IT_SAIDA_0140'  'HR_REGISTRO'        'Hr.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       11  'ZSDT0294'      'US_REGISTRO'       'IT_SAIDA_0140'  'US_REGISTRO'        'Us.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0150'.
      PERFORM f_estrutura_alv USING:

       01  'ZSDT0294'      'INF_ADD_PROD'      'IT_SAIDA_0140'  'INF_ADD_PROD'       'Inf.Adic.Prod.'                         '14'   'X'    ''  ' ' 'C' ' ' ' ' 'X',  "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP
       02  'ZSDT0294'      'BRANCH'            'IT_SAIDA_0140'  'BRANCH'             'Filial'                                 '06'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
*-CS2022000324-25.08.2022-#84903-JT-inicio
       03  'ZSDT0294'      'MATKL'             'IT_SAIDA_0140'  'MATKL'              'Grp.Material'                           '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       04  'T023T'         'WGBEZ'             'IT_SAIDA_0140'  'WGBEZ'              'Desc.Grp.Material'                      '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*-CS2022000324-25.08.2022-#84903-JT-fim
       05  'ZSDT0294'      'MATNR'             'IT_SAIDA_0140'  'MATNR'              'Cod.Material'                           '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       06  'MAKT'          'MAKTG'             'IT_SAIDA_0140'  'MAKTG'              'Desc.Material'                          '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       08  'ZSDT0287'      'ID_OBS'            'IT_SAIDA_0140'  'ID_OBS'             'ID OBS'                                 '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       09  'ZSDT0287'      'OBSERV'            'IT_SAIDA_0140'  'OBSERV_287'         'Observação'                             '50'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'ZSDT0294'      'TX_ITEM_MATERIAL'  'IT_SAIDA_0140'  'TX_ITEM_MATERIAL'   'Txt.It.Mat.'                            '11'   'X'    ''  ' ' 'C' ' ' ' ' 'X',  "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP
       11  'ZSDT0294'      'DT_REGISTRO'       'IT_SAIDA_0140'  'DT_REGISTRO'        'Dt.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       12  'ZSDT0294'      'HR_REGISTRO'       'IT_SAIDA_0140'  'HR_REGISTRO'        'Hr.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       13  'ZSDT0294'      'US_REGISTRO'       'IT_SAIDA_0140'  'US_REGISTRO'        'Us.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .

  ENDCASE.

ENDFORM.

FORM f_estrutura_alv USING VALUE(p_col_pos)       TYPE i
                           VALUE(p_ref_tabname)   LIKE dd02d-tabname
                           VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                           VALUE(p_tabname)       LIKE dd02d-tabname
                           VALUE(p_field)         LIKE dd03d-fieldname
                           VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                           VALUE(p_outputlen)
                           VALUE(p_edit)
                           VALUE(p_sum)
                           VALUE(p_emphasize)
                           VALUE(p_just)
                           VALUE(p_hotspot)
                           VALUE(p_f4)
                           VALUE(p_check).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = ' '.
  wa_fcat-do_sum      = p_sum.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_check.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

  CASE p_screen.
    WHEN '0100' OR '0120' OR '0130' OR '0140' OR '0150'.
      APPEND cl_gui_alv_grid=>mc_fc_refresh           TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row    TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row    TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_append_row    TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy          TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row      TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_cut           TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_undo          TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste         TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row TO it_exclude_fcode.
      APPEND cl_gui_alv_grid=>mc_fc_check             TO it_exclude_fcode.

  ENDCASE.


ENDFORM.

FORM f_limpa_variaveis .

  CLEAR: wa_saida_0100,
         it_saida_0100[],
         tg_zsdt0006[],
         tg_zsdt0294[].

ENDFORM.

FORM f_selecionar_dados .

  PERFORM f_limpa_variaveis.

  SELECT *
    FROM zsdt0006 INTO TABLE tg_zsdt0006.

  CHECK tg_zsdt0006[] IS NOT INITIAL.

ENDFORM.

FORM f_processa_dados .

  LOOP AT tg_zsdt0006.

    CLEAR: wa_saida_0100.

    MOVE-CORRESPONDING tg_zsdt0006 TO wa_saida_0100.

    "Chave Primaria
    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'AUART'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0100-estilo.

    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'BRANCH'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0100-estilo.

    APPEND wa_saida_0100 TO it_saida_0100.

  ENDLOOP.

ENDFORM.

FORM f_call_screen_0120.

  PERFORM f_selecionar_dados_0120.

  CALL SCREEN 0120 STARTING AT 04 04." ENDING AT 120 20.

ENDFORM.

FORM f_selecionar_dados_0120.

  CLEAR: it_saida_0120[].

  SELECT *
    FROM zsdt0294 INTO TABLE tg_zsdt0294
   WHERE auart  = wa_saida_0100-auart.

  LOOP AT tg_zsdt0294.
    CLEAR: wa_saida_0120.

    MOVE-CORRESPONDING tg_zsdt0294 TO wa_saida_0120.

    "Chave Primaria
    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'AUART'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0120-estilo.

    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'BRANCH'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0120-estilo.


    APPEND wa_saida_0120 TO it_saida_0120.
  ENDLOOP.

ENDFORM.


FORM f_call_screen_0130.

  PERFORM f_selecionar_dados_0130.

  CALL SCREEN 0130 STARTING AT 04 04." ENDING AT 120 20.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_DADOS_0130
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_selecionar_dados_0130 .

  CLEAR: it_saida_0130[].

  SELECT *
    FROM zsdt0287 INTO TABLE tg_zsdt0287
   WHERE cancelado NE 'X'.

  SORT tg_zsdt0287 BY id_obs.

  LOOP AT tg_zsdt0287.
    CLEAR: wa_saida_0130.

    MOVE-CORRESPONDING tg_zsdt0287 TO wa_saida_0130.

    "Chave Primaria
    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'ID_OBS'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0130-estilo.

*    CLEAR: WL_ESTILO.
*    WL_ESTILO-FIELDNAME = 'BRANCH'.
*    WL_ESTILO-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
*    APPEND WL_ESTILO    TO WA_SAIDA_0130-ESTILO.


    APPEND wa_saida_0130 TO it_saida_0130.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CALL_SCREEN_0140
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_call_screen_0140 .

  PERFORM f_selecionar_dados_0140.

  CALL SCREEN 0140 STARTING AT 25 02." ENDING AT 120 20.

ENDFORM.

FORM f_call_screen_0150 .

  PERFORM f_selecionar_dados_0150.

  CALL SCREEN 0150 STARTING AT 25 02." ENDING AT 120 20.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONAR_DADOS_0140
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

FORM f_selecionar_dados_0140 .

  CLEAR: it_saida_0140[].

  SELECT *
     FROM zsdt0294 INTO TABLE tg_zsdt0294
    WHERE auart  = wa_saida_0100-auart
    AND cancelado NE 'X'.

  LOOP AT tg_zsdt0294.

    CLEAR: wa_saida_0140.

    MOVE-CORRESPONDING tg_zsdt0294 TO wa_saida_0140.

    "Chave Primaria
    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'AUART'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0140-estilo.

    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'BRANCH'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0140-estilo.

    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'ID_OBS'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0140-estilo.

*-CS2022000324-25.08.2022-#84903-JT-inicio
    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'MATKL'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0140-estilo.

    IF wa_saida_0140-matkl IS NOT INITIAL.
      SELECT SINGLE wgbez
        FROM t023t
        INTO wa_saida_0140-wgbez
       WHERE spras = sy-langu
         AND matkl = wa_saida_0140-matkl.
    ENDIF.
*-CS2022000324-25.08.2022-#84903-JT-fim

    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'MATNR'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0140-estilo.

    IF wa_saida_0140-matnr IS NOT INITIAL.
      SELECT SINGLE maktg
         FROM  makt
         INTO wa_saida_0140-maktg
         WHERE matnr EQ wa_saida_0140-matnr
         AND spras EQ 'P'.
      "  MODIFY it_saida_0140 FROM wa_saida_0140.
    ENDIF.

    IF wa_saida_0140-id_obs IS NOT INITIAL.
      SELECT SINGLE observ
         FROM  zsdt0287
         INTO wa_saida_0140-observ_287
         WHERE id_obs EQ wa_saida_0140-id_obs.
    ENDIF.


*    CLEAR: wl_estilo.
*    wl_estilo-fieldname = 'BRANCH'.
*    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
*    APPEND wl_estilo    TO wa_saida_0140-estilo.


    APPEND wa_saida_0140 TO it_saida_0140.
  ENDLOOP.

ENDFORM.

FORM f_selecionar_dados_0150 .

  CLEAR: it_saida_0150[].

  SELECT *
     FROM zsdt0294 INTO TABLE tg_zsdt0294
    WHERE auart      = abap_off
      AND cancelado NE 'X'.

  LOOP AT tg_zsdt0294.

    CLEAR: wa_saida_0150.

    MOVE-CORRESPONDING tg_zsdt0294 TO wa_saida_0150.

    "Chave Primaria
    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'BRANCH'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0150-estilo.

    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'ID_OBS'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0150-estilo.

    "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP -->>>
    wl_estilo-fieldname = 'INF_ADD_PROD'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0150-estilo.
    "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP <<---

*-CS2022000324-25.08.2022-#84903-JT-inicio
    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'MATKL'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0150-estilo.

    IF wa_saida_0150-matkl IS NOT INITIAL.
      SELECT SINGLE wgbez
        FROM t023t
        INTO wa_saida_0150-wgbez
       WHERE spras = sy-langu
         AND matkl = wa_saida_0150-matkl.
    ENDIF.
*-CS2022000324-25.08.2022-#84903-JT-fim

    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'MATNR'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0150-estilo.

    IF wa_saida_0150-matnr IS NOT INITIAL.
      SELECT SINGLE maktg
         FROM  makt
         INTO wa_saida_0150-maktg
         WHERE matnr EQ wa_saida_0150-matnr
         AND spras EQ 'P'.

      "  MODIFY it_saida_0150 FROM wa_saida_0150.
    ENDIF.

    IF wa_saida_0150-id_obs IS NOT INITIAL.
      SELECT SINGLE observ
         FROM  zsdt0287
         INTO wa_saida_0150-observ_287
         WHERE id_obs EQ wa_saida_0150-id_obs.
    ENDIF.

    "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP -->>>
    CLEAR: wl_estilo.
    wl_estilo-fieldname = 'TX_ITEM_MATERIAL'.
    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
    APPEND wl_estilo    TO wa_saida_0150-estilo.
    "SD - ZSDT0150 - Melhorias Observações NF 191719 - WPP <<---


*    CLEAR: wl_estilo.
*    wl_estilo-fieldname = 'BRANCH'.
*    wl_estilo-style     = cl_gui_alv_grid=>mc_style_disabled.
*    APPEND wl_estilo    TO wa_saida_0150-estilo.

    APPEND wa_saida_0150 TO it_saida_0150.
  ENDLOOP.

ENDFORM.

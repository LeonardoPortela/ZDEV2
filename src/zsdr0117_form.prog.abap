*&---------------------------------------------------------------------*
*&  Include           ZSDR0117_FORM
*&---------------------------------------------------------------------*

FORM f_refresh_objetos .

  CLEAR: gs_layout,
         gs_variant.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  DATA: v_edit_bol TYPE c.

  FREE: wa_fcat, it_fcat.

  CASE vg_operacao_bol.
    WHEN c_new_bol OR c_edit_bol .
      v_edit_bol = abap_true.
    WHEN OTHERS.
      v_edit_bol = abap_false.
  ENDCASE.

  DATA: v_nf_emissao_lim  TYPE dd03p-scrtext_l,
        v_sld_emissao_lim TYPE dd03p-scrtext_l,
        v_dias_str        TYPE c LENGTH 3.

  CASE p_screen.
    WHEN '0100_01'.

      CASE tp_boletim.
        WHEN '02' OR '03'. "Biodiesel ou Oleo neutro.
          PERFORM f_estrutura_alv USING:

             01  '        ' '            ' 'IT_SAIDA_0100_01' 'DS_PROD_CONSUMO   ' 'Dados de Consumo ' '33'   ' '            ' '  '' '' '' '' '' '',
             02  '        ' '            ' 'IT_SAIDA_0100_01' 'CHECK_MAT_TERCEIRO' 'Produto/terceiro?' '14    '  v_edit_bol  'X'  '' '' '' '' '' '',
             03  'ZSDT0247' 'QTDE_CONSUMO' 'IT_SAIDA_0100_01' 'QTDE_CONSUMO      ' 'Quantidade(KG)   ' '14    '  v_edit_bol  'X'  '' '' '' '' '' ''.

        WHEN OTHERS.
          PERFORM f_estrutura_alv USING:

         01  ''         ''             'IT_SAIDA_0100_01' 'DS_PROD_CONSUMO'      'Consumo de Soja para Produto'   '33'     ' '         '' '' '' '' '' '' ' ',
         02  'ZSDT0247' 'QTDE_CONSUMO' 'IT_SAIDA_0100_01' 'QTDE_CONSUMO'         'Quantidade(KG)'                 '14'     v_edit_bol  'X' '' '' '' '' '' ' '.
      ENDCASE.

    WHEN '0100_02'.
      CASE tp_boletim.
        WHEN '02' OR '03'. "Biodiesel ou Oleo neutro.
          PERFORM f_estrutura_alv USING:

           01  ''          ''             'IT_SAIDA_0100_02' 'DS_PROD_RENDIMENTO'   'Produto'                        '25'     ' '         ''  ' ' ' ' '' ' ' ' ' '',
           02  'ZSDT0248'  'QTDE'         'IT_SAIDA_0100_02' 'QTDE'                 'Quantidade(KG)'                 '14'     v_edit_bol  'X' ' ' ' ' '' ' ' ' ' '',
           03  ''          ''             'IT_SAIDA_0100_02' 'PERC_RENDIMENTO'      'Rend.% Calc.'                   '12'     ' '         ' ' ' ' ' ' '' ' ' ' ' ''.
*           04  'ZSDT0248'  'QTDE_ME'      'IT_SAIDA_0100_02' 'QTDE_ME'              'Qtdade(KG) Mer.Ext'              '18'     ' '        'X' ' ' ' ' '' ' ' ' ' 'X',
*           05  'ZSDT0248'  'QTDE_MI'      'IT_SAIDA_0100_02' 'QTDE_MI'              'Qtdade(KG) Mer.Int'              '18'     ' '        'X' ' ' ' ' '' ' ' ' ' 'X'.

        WHEN OTHERS.
          PERFORM f_estrutura_alv USING:

         01  ''          ''             'IT_SAIDA_0100_02' 'DS_PROD_RENDIMENTO'   'Produto'                        '25'     ' '         ''  ' ' ' ' '' ' ' ' ' '',
         02  'ZSDT0248'  'QTDE'         'IT_SAIDA_0100_02' 'QTDE'                 'Quantidade(KG)'                 '14'     v_edit_bol  'X' ' ' ' ' '' ' ' ' ' '',
         03  ''          ''             'IT_SAIDA_0100_02' 'PERC_RENDIMENTO'      'Rend.% Calc.'                   '12'     ' '         ' ' ' ' ' ' '' ' ' ' ' '',
         04  'ZSDT0248'  'QTDE_ME'      'IT_SAIDA_0100_02' 'QTDE_ME'              'Qtdade(KG) Mer.Ext'              '18'     ' '        'X' ' ' ' ' '' ' ' ' ' 'X',  "Adicionado CS2020001194
         05  'ZSDT0248'  'QTDE_MI'      'IT_SAIDA_0100_02' 'QTDE_MI'              'Qtdade(KG) Mer.Int'              '18'     ' '        'X' ' ' ' ' '' ' ' ' ' 'X'.  "Adicionado CS2020001194

          PERFORM f_mostrar_cpos_mercado_interno USING '0100_02'.

      ENDCASE.
    WHEN '0120_01'.

      PERFORM f_estrutura_alv USING:

         01  'J_1BBRANCH'    'BRANCH'             'IT_SAIDA_0120_01' 'BRANCH'       'Filial'             '06'     ' '    ' ' ' ' ' ' ' ' ' ' ' ' ' ',
         02  'J_1BBRANCH'    'NAME'               'IT_SAIDA_0120_01' 'DS_BRANCH'    'Ds.Filial'          '25'     ' '    ' ' ' ' ' ' ' ' ' ' ' ' ' ',
         03  'ZSDT0251'      'QTDE_SALDO'         'IT_SAIDA_0120_01' 'SALDO'        'Saldo(KG)'          '18'     ' '    'X' ' ' ' ' 'X' ' ' ' ' ' '.


      SELECT SINGLE * FROM zsdt0253 INTO @DATA(wl_zsdt0253) WHERE branch EQ @wg_cab_boletim_prod-branch.

      IF ( sy-subrc EQ 0 ) AND ( wl_zsdt0253-qtde_dias_emi > 0 ).

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
          EXPORTING
            input  = wl_zsdt0253-qtde_dias_emi
          IMPORTING
            output = v_dias_str.

        v_nf_emissao_lim   = |NFs emissão acima { v_dias_str } dias|.
        v_sld_emissao_lim  = |Saldo(KG) emissão acima { v_dias_str } dias|.

        PERFORM f_estrutura_alv USING:

          04  'ZSDT0253'      'QTDE_DIAS_EMI'      'IT_SAIDA_0120_01' 'QTDE_NF_EMI_LIM'    v_nf_emissao_lim          '25'     ' '    'X' ' ' 'C' ' ' ' ' ' ' ' ',
          05  'ZSDT0251'      'QTDE_SALDO'         'IT_SAIDA_0120_01' 'SALDO_NF_EMI_LIM'   v_sld_emissao_lim         '32'     ' '    'X' ' ' ' ' 'X' ' ' ' ' ' '.

      ENDIF.


    WHEN '0120_02'.

      PERFORM f_estrutura_alv USING:

         01  'J_1BBRANCH'    'BRANCH'             'IT_SAIDA_0120_02' 'BRANCH'               'Filial'              '06'     ' '    ' ' ' ' ' ' ' ' ' ' ' ' ' ',
         02  'J_1BBRANCH'    'NAME'               'IT_SAIDA_0120_02' 'DS_BRANCH'            'Ds.Filial'           '25'     ' '    ' ' ' ' ' ' ' ' ' ' ' ' ' '.

      IF ( wg_cab_boletim_prod-id_boletim IS NOT INITIAL ) AND ( wg_cab_boletim_prod-com_nf EQ abap_true ).

        PERFORM f_estrutura_alv USING:

         03  'ZSDT0249'      'ID_AGRP'            'IT_SAIDA_0120_02' 'ID_AGRP'              'Agrp.'               '05'     ' '    ' ' ' ' ' ' ' ' ' ' ' ' ' '.

      ENDIF.

      PERFORM f_estrutura_alv USING:

         04  'ZSDT0249'      'QTDE_VINC'          'IT_SAIDA_0120_02' 'SALDO_VINC'           'Saldo(KG)'           '18'     ' '    'X' ' ' ' ' 'X' ' ' ' ' ' '.

      IF ( wg_cab_boletim_prod-id_boletim IS NOT INITIAL ) AND ( wg_cab_boletim_prod-com_nf EQ abap_true ).

        PERFORM f_estrutura_alv USING:

         05  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_DEVOL'        'Seq.Dev.'            '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         06  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_DEVOL'         'Doc.Dev.'            '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         07  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_DEVOL'         'NFe.Dev.'            '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         08  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_ENT_DEV'      'Seq.Ent.Dev.'        '12'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         09  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_ENT_DEV'       'Doc.Ent.Dev.'        '12'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         10  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_ENT_DEV'       'NFe.Ent.Dev.'        '12'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',


         11  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_IND'          'Seq.Ind.'            '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         12  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_IND'           'Doc.Ind.'            '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         13  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_IND'           'NFe.Ind.'            '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         14  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_ENT_IND'      'Seq.Ent.Ind.'        '12'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         15  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_ENT_IND'       'Doc.Ent.Ind.'        '12'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         16  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_ENT_IND'       'NFe.Ent.Ind.'        '12'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' '.

      ENDIF.

      PERFORM f_estrutura_alv USING:

         17  'ZSDT0252'      'DOC_PROD_01'        'IT_SAIDA_0120_02' 'DOC_PROD_01'          'Doc.Prod.01'         '11'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         18  'ZSDT0252'      'DOC_PROD_02'        'IT_SAIDA_0120_02' 'DOC_PROD_02'          'Doc.Prod.02'         '11'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         19  'ZSDT0252'      'DOC_PROD_03'        'IT_SAIDA_0120_02' 'DOC_PROD_03'          'Doc.Prod.03'         '11'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         20  'ZSDT0252'      'DOC_PROD_04'        'IT_SAIDA_0120_02' 'DOC_PROD_04'          'Doc.Prod.04'         '11'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         21  'ZSDT0252'      'DOC_PROD_05'        'IT_SAIDA_0120_02' 'DOC_PROD_05'          'Doc.Prod.05'         '11'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' '.

      IF ( wg_cab_boletim_prod-id_boletim IS NOT INITIAL ) AND ( wg_cab_boletim_prod-com_nf EQ abap_true ).

        PERFORM f_estrutura_alv USING:

         22  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_RFL_01'       'Seq.RFL 01'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         23  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_RFL_01'        'Doc.RFL 01'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         24  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_RFL_01'        'NFe.RFL 01'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         25  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_RFL_02'       'Seq.RFL 02'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         26  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_RFL_02'        'Doc.RFL 02'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         27  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_RFL_02'        'NFe.RFL 02'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         28  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_03' 'SEQLCTO_RFL_03'       'Seq.RFL 03'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         29  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_03' 'DOCNUM_RFL_03'        'Doc.RFL 03'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         30  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_03' 'NFENUM_RFL_03'        'NFe.RFL 03'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',


         31  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_RCO_01'       'Seq.RCO 01'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         32  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_RCO_01'        'Doc.RCO 01'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         33  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_RCO_01'        'NFe.RCO 01'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         34  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_ENT_RCO_01'   'Seq.Ent.RCO 01'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         35  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_ENT_RCO_01'    'Doc.Ent.RCO 01'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         36  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_ENT_RCO_01'    'NFe.Ent.RCO 01'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         37  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_RCO_02'       'Seq.RCO 02'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         38  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_RCO_02'        'Doc.RCO 02'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         39  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_RCO_02'        'NFe.RCO 02'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         40  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_ENT_RCO_02'   'Seq.Ent.RCO 02'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         41  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_ENT_RCO_02'    'Doc.Ent.RCO 02'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         42  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_ENT_RCO_02'    'NFe.Ent.RCO 02'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         43  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_RCO_03'       'Seq.RCO 03'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         44  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_RCO_03'        'Doc.RCO 03'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         45  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_RCO_03'        'NFe.RCO 03'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',

         46  'ZFIWRT0008'    'SEQ_LCTO'           'IT_SAIDA_0120_02' 'SEQLCTO_ENT_RCO_03'   'Seq.Ent.RCO 03'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         47  'ZFIWRT0008'    'DOCNUM'             'IT_SAIDA_0120_02' 'DOCNUM_ENT_RCO_03'    'Doc.Ent.RCO 03'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         48  'ZFIWRT0008'    'NFENUM'             'IT_SAIDA_0120_02' 'NFENUM_ENT_RCO_03'    'NFe.Ent.RCO 03'      '14'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' '.

      ENDIF.

    WHEN '0121'.

      PERFORM f_estrutura_alv USING:

         02  'J_1BNFDOC'    'DOCNUM'             'IT_SAIDA_0121'     'DOCNUM'               'Docnum'          '10'     ' '    ' ' ' ' 'C' 'X' ' ' ' ' ' ',
         03  'J_1BNFDOC'    'NFENUM'             'IT_SAIDA_0121'     'NFENUM'               'NF-e'            '10'     ' '    ' ' ' ' 'C' ' ' ' ' ' ' ' ',
         04  'J_1BNFDOC'    'DOCDAT'             'IT_SAIDA_0121'     'DOCDAT'               'Dt.Emissão'      '10'     ' '    ' ' ' ' 'C' ' ' ' ' ' ' ' ',
         04  'J_1BNFLIN'    'MENGE'              'IT_SAIDA_0121'     'SALDO'                'Saldo(KG)'       '13'     ' '    'X' ' ' 'C' ' ' ' ' ' ' ' '.

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
                           VALUE(p_checkbox)
                           VALUE(p_out).

  CLEAR wa_fcat.

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
  wa_fcat-col_pos     = p_col_pos.
  wa_fcat-outputlen   = p_outputlen.
  wa_fcat-no_out      = p_out.
  wa_fcat-reptext     = p_scrtext_l.
  wa_fcat-scrtext_s   = p_scrtext_l.
  wa_fcat-scrtext_m   = p_scrtext_l.
  wa_fcat-scrtext_l   = p_scrtext_l.
  wa_fcat-emphasize   = p_emphasize.
  wa_fcat-style       =
  wa_fcat-just        = p_just.
  wa_fcat-hotspot     = p_hotspot.
  wa_fcat-f4availabl  = p_f4.
  wa_fcat-checkbox    = p_checkbox.
  wa_fcat-do_sum      = p_sum.

  APPEND wa_fcat TO it_fcat.

ENDFORM.                    " ESTRUTURA_ALV

FORM f_exclude_fcode USING p_screen.

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
  APPEND cl_gui_alv_grid=>mc_fc_print             TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_help              TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_info              TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_graph             TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_views             TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_save_variant      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_load_variant      TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant   TO it_exclude_fcode.
  APPEND cl_gui_alv_grid=>mc_fc_maintain_variant  TO it_exclude_fcode.

ENDFORM.

FORM f_new_boletim_producao.

  DATA: it_produtos_prod TYPE zsdt0250_t,
        it_produtos_rend TYPE zsdt0250_t.

  FREE: it_zsdt0247_aux, it_zsdt0248_aux.
  CLEAR: g_qtde_si_aux.

*---------------------------------------------------------------------------------------------------------*
* Validações
*---------------------------------------------------------------------------------------------------------*

  TRY.
      it_produtos_prod = objeto->get_tp_produtos_producao( ).
      it_produtos_rend = objeto->get_tp_produtos_rendimento( ).


*---------------------------------------------------------------------------------------------------------*
* Inicia Variaveis
*---------------------------------------------------------------------------------------------------------*

      CLEAR: wg_cab_boletim_prod.

      CLEAR: it_saida_0100_01[], it_saida_0100_02[], it_saida_0120_01[], it_saida_0120_02[].

*---------------------------------------------------------------------------------------------------------*
* Inclusão dados Cabeçalho
*---------------------------------------------------------------------------------------------------------*

      wg_cab_boletim_prod-dt_lancamento   = sy-datum.
      wg_cab_boletim_prod-tp_boletim      = tp_boletim.
      IF tp_boletim EQ '01'. "Farelo/óleo
        DATA(wl_zsdt0250) = objeto->get_material_boletim( i_tp_produto = 'SG' ).
        wg_cab_boletim_prod-produto_rem_ind = wl_zsdt0250-matnr.
      ENDIF.


*---------------------------------------------------------------------------------------------------------*
* Inclusão Produtos - Dados Produção Soja
*---------------------------------------------------------------------------------------------------------*
      LOOP AT it_produtos_prod INTO DATA(wl_prod_producao).

        CLEAR: wa_saida_0100_01.

        wa_saida_0100_01-tp_produto_producao = wl_prod_producao-tp_produto_producao.
        zcl_boletim_producao=>zif_boletim_producao~get_instance(
        )->get_und_material(
          EXPORTING
            i_matnr =    wl_prod_producao-matnr " Nº do material
          IMPORTING
            e_meins =  wa_saida_0100_01-unid_consumo   " Unidade de medida básica
        ).

*        wa_saida_0100_01-unid_consumo        = 'KG'.

        PERFORM f_atrib_ds_produto_consumo CHANGING wa_saida_0100_01.

        APPEND wa_saida_0100_01 TO it_saida_0100_01.

      ENDLOOP.

*---------------------------------------------------------------------------------------------------------*
* Inclusão Produtos - Rendimento
*---------------------------------------------------------------------------------------------------------*
      DATA: v_order_view TYPE zsdt0248-order_view.

      CLEAR: v_order_view.

      LOOP AT it_produtos_rend INTO DATA(wl_prod_rend).

        CLEAR: wa_saida_0100_02.

        ADD 1 TO v_order_view.
        wa_saida_0100_02-order_view          = v_order_view.
        wa_saida_0100_02-tp_produto_producao = wl_prod_rend-tp_produto_producao.

        zcl_boletim_producao=>zif_boletim_producao~get_instance(
        )->get_und_material(
          EXPORTING
            i_matnr =    wl_prod_rend-matnr " Nº do material
          IMPORTING
            e_meins =  wa_saida_0100_02-unid   " Unidade de medida básica
        ).

*        wa_saida_0100_02-unid                = 'KG'.

        wa_saida_0100_02-ds_prod_rendimento = zcl_util=>get_desc_value_domain(  i_domname = 'ZDM_TP_PRODUTO_PRODUCAO' i_domvalue = CONV #( wl_prod_rend-tp_produto_producao ) ).

        APPEND wa_saida_0100_02 TO it_saida_0100_02.

      ENDLOOP.

      PERFORM f_set_operacao_boletim USING c_new_bol.

      LEAVE TO SCREEN 0100.

    CATCH zcx_boletim_producao INTO DATA(zcx_bol_prod).
      zcx_bol_prod->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
      RETURN.
  ENDTRY.


ENDFORM.


FORM f_free_alv USING p_screen.

  CASE p_screen.
    WHEN '0100_01'.

      IF obj_alv_0100_01 IS NOT INITIAL.
        CALL METHOD obj_alv_0100_01->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE obj_alv_0100_01.
      ENDIF.

    WHEN '0100_02'.

      IF obj_alv_0100_02 IS NOT INITIAL.
        CALL METHOD obj_alv_0100_02->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE obj_alv_0100_02.
      ENDIF.

    WHEN '0120_01'.

      wg_refresh_splitter = abap_true.

      IF obj_alv_0120_01 IS NOT INITIAL.
        CALL METHOD obj_alv_0120_01->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE obj_alv_0120_01.
      ENDIF.

    WHEN '0120_02'.

      IF obj_alv_0120_02 IS NOT INITIAL.
        CALL METHOD obj_alv_0120_02->free.
        CALL METHOD cl_gui_cfw=>flush.
        FREE obj_alv_0120_02.
      ENDIF.

  ENDCASE.

ENDFORM.

FORM f_gravar_boletim_producao.
  DATA: check_modif TYPE char01,
        w_0246      TYPE zsdt0246.

  CLEAR: w_0246.
*  FREE: it_zsdt0247_aux, it_zsdt0248_aux.

  IF it_msg_return IS INITIAL.

    MOVE-CORRESPONDING wg_cab_boletim_prod TO wa_zsdt0246.
*  MOVE-CORRESPONDING wg_cab_boletim_prod TO w_0246.
    CLEAR: it_zsdt0247.
    LOOP AT it_saida_0100_01 INTO wa_saida_0100_01.
      CLEAR: wa_zsdt0247.

      wa_zsdt0247-qtde_consumo        = wa_saida_0100_01-qtde_consumo.
      wa_zsdt0247-tp_produto_producao = wa_saida_0100_01-tp_produto_producao.
      wa_zsdt0247-unid_consumo        = wa_saida_0100_01-unid_consumo.
      wa_zsdt0247-check_mat_terceiro  = wa_saida_0100_01-check_mat_terceiro.
      APPEND wa_zsdt0247 TO it_zsdt0247.
    ENDLOOP.

    LOOP AT it_saida_0100_02 INTO wa_saida_0100_02.
      CLEAR: wa_zsdt0248.

      wa_zsdt0248-qtde                = wa_saida_0100_02-qtde.
      wa_zsdt0248-tp_produto_producao = wa_saida_0100_02-tp_produto_producao.
      wa_zsdt0248-unid                = wa_saida_0100_02-unid.
      wa_zsdt0248-perc_rendimento     = wa_saida_0100_02-perc_rendimento.
      wa_zsdt0248-order_view          = wa_saida_0100_02-order_view.
      wa_zsdt0248-qtde_me             = wa_saida_0100_02-qtde_me.
      wa_zsdt0248-qtde_mi             = wa_saida_0100_02-qtde_mi.

      APPEND wa_zsdt0248 TO it_zsdt0248.
    ENDLOOP.

    "Check modif.
    CLEAR: check_modif.
    IF wg_cab_boletim_prod-id_boletim IS NOT INITIAL AND it_zsdt0247_aux IS NOT INITIAL AND it_zsdt0247_aux IS NOT INITIAL.
      LOOP AT it_zsdt0247 ASSIGNING FIELD-SYMBOL(<ws_247>).
        READ TABLE it_zsdt0247_aux INTO DATA(ws_247_aux) WITH KEY tp_produto_producao = <ws_247>-tp_produto_producao.
        IF sy-subrc EQ 0.
          IF <ws_247>-qtde_consumo NE ws_247_aux-qtde_consumo.
            check_modif = abap_true.
          ENDIF.
        ENDIF.
      ENDLOOP.
      CLEAR: ws_247_aux.

      IF check_modif IS INITIAL.
        LOOP AT it_zsdt0248 ASSIGNING FIELD-SYMBOL(<ws_248>).
          READ TABLE it_zsdt0248_aux INTO DATA(ws_248_aux) WITH KEY tp_produto_producao = <ws_248>-tp_produto_producao.
          IF sy-subrc EQ 0.
            IF <ws_248>-qtde NE ws_248_aux-qtde.
              check_modif = abap_true.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF check_modif IS INITIAL.
          IF g_qtde_si_aux NE wg_cab_boletim_prod-qtde_si.
            check_modif = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.


      IF check_modif IS NOT INITIAL.
        "Se houver modificação dos dados, retornar o status de aprovação.
        wa_zsdt0246-aprovado = abap_false.
        wa_zsdt0246-status   = abap_false.
        MODIFY zsdt0246 FROM wa_zsdt0246.
        COMMIT WORK.
      ENDIF.
    ENDIF.




    TRY.

*      zcl_boletim_producao=>zif_boletim_producao~get_instance( )->novo_registro(
*                                                               )->set_dados_boletim( i_dados_boletim = wa_zsdt0246
*                                                               )->set_dados_producao( i_dados_producao = it_zsdt0247
*                                                               )->set_dados_rendimento( i_dados_rendimento = it_zsdt0248
*                                                               )->gravar_registro( IMPORTING e_id_boletim = DATA(_id_boletim_prod) ).

        objeto->novo_registro(
                                              )->set_dados_boletim( i_dados_boletim = wa_zsdt0246
                                              )->set_dados_producao( i_dados_producao = it_zsdt0247
                                              )->set_dados_rendimento( i_dados_rendimento = it_zsdt0248
                                              )->gravar_registro( IMPORTING e_id_boletim = DATA(_id_boletim_prod) ).

        IF vg_operacao_bol EQ c_new_bol.
          PERFORM f_free_alv USING: '0120_01', '0120_02'.
        ENDIF.

        PERFORM f_set_operacao_boletim USING c_view_bol.

        PERFORM f_load_boletim USING _id_boletim_prod.

        LEAVE TO SCREEN 0100.

      CATCH zcx_boletim_producao INTO DATA(zcx_boletim).
        zcx_boletim->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
        RETURN.
    ENDTRY.
  ELSE.
    MESSAGE s836(sd) WITH 'Existem erros para serem corrigidos' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.
ENDFORM.

FORM f_calc_perc_rendimento CHANGING p_saida_0100_02 TYPE ty_saida_0100_02.

  DATA: t_zsdt0247  TYPE zsdt0247_t,
        t_zsdt0248  TYPE zsdt0248_t,
        wl_zsdt0247 TYPE zsdt0247,
        wl_zsdt0248 TYPE zsdt0248.

  CLEAR: t_zsdt0247[], t_zsdt0248[].

  LOOP AT it_saida_0100_01 INTO DATA(_wl_saida_0100_01).
    CLEAR: wl_zsdt0247.
    MOVE-CORRESPONDING _wl_saida_0100_01 TO wl_zsdt0247.
    APPEND wl_zsdt0247 TO t_zsdt0247.

    "======================================Inicio CS2021000885 / 28/03/2022 - Anderson Oenning
    CASE tp_boletim.
      WHEN '02'.
        IF _wl_saida_0100_01-tp_produto_producao EQ 'NI'.
          wg_cab_boletim_prod-qtde_si = _wl_saida_0100_01-qtde_consumo.
        ENDIF.
      WHEN '03'.
        IF _wl_saida_0100_01-tp_produto_producao EQ 'OI'.
          wg_cab_boletim_prod-qtde_si = _wl_saida_0100_01-qtde_consumo.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    "======================================Inicio CS2021000885 / 28/03/2022 - Anderson Oenning
  ENDLOOP.

  CLEAR: wl_zsdt0248.
  MOVE-CORRESPONDING p_saida_0100_02 TO wl_zsdt0248.
  APPEND wl_zsdt0248 TO t_zsdt0248.

  objeto->get_perc_rendimento( EXPORTING i_zsdt0247           = t_zsdt0247
                               CHANGING  c_zsdt0248           = t_zsdt0248 ).

  READ TABLE t_zsdt0248 INTO wl_zsdt0248 INDEX 1.
  IF sy-subrc EQ 0.
    p_saida_0100_02-perc_rendimento = wl_zsdt0248-perc_rendimento.
  ENDIF.

ENDFORM.


FORM f_load_boletim USING p_id_boletim TYPE zsdt0246-id_boletim.

  DATA: v_id_boletim TYPE zsdt0246-id_boletim.

  v_id_boletim = p_id_boletim.

  PERFORM f_limpa_dados_screen.

  CHECK v_id_boletim IS NOT INITIAL.

*---------------------------------------------------------------------*
* Carregar Dados Cabeçalho Boletim
*---------------------------------------------------------------------*

  SELECT SINGLE *
    FROM zsdt0246 INTO wa_zsdt0246
   WHERE id_boletim EQ v_id_boletim.

  IF sy-subrc IS NOT INITIAL.
    MESSAGE |Boletim ID { v_id_boletim } não existente! | TYPE 'I'.
    PERFORM f_limpa_dados_screen.
    RETURN.
  ELSEIF wa_zsdt0246-loekz IS NOT INITIAL.
    MESSAGE |Boletim ID { v_id_boletim } foi eliminado! | TYPE 'I'.
    PERFORM f_limpa_dados_screen.
    RETURN.
  ENDIF.

  CHECK sy-subrc EQ 0 AND wa_zsdt0246-loekz IS INITIAL.

  AUTHORITY-CHECK OBJECT 'M_MATE_WRK' ID 'WERKS' FIELD  wa_zsdt0246-branch
                                      ID 'ACTVT' FIELD '03'.
  IF sy-subrc NE 0.
    MESSAGE |Sem acesso a filial { wa_zsdt0246-branch }! | TYPE 'I'.
    PERFORM f_limpa_dados_screen.
    RETURN.
  ENDIF.

  MOVE-CORRESPONDING wa_zsdt0246 TO wg_cab_boletim_prod.

*---------------------------------------------------------------------*
* Carregar Dados Produtos Produção
*---------------------------------------------------------------------*

  SELECT *
    FROM zsdt0247 INTO TABLE it_zsdt0247
   WHERE id_boletim EQ v_id_boletim.

  LOOP AT it_zsdt0247 INTO wa_zsdt0247.
    CLEAR: wa_saida_0100_01.

    MOVE-CORRESPONDING wa_zsdt0247 TO wa_saida_0100_01.

    PERFORM f_atrib_ds_produto_consumo CHANGING wa_saida_0100_01.

    APPEND wa_saida_0100_01 TO it_saida_0100_01.

  ENDLOOP.

*---------------------------------------------------------------------*
* Carregar Dados Rendimento
*---------------------------------------------------------------------*

  SELECT *
    FROM zsdt0248 INTO TABLE it_zsdt0248
   WHERE id_boletim EQ v_id_boletim.

  SORT it_zsdt0248 BY order_view.

*  PERFORM zf_campos_venda_interna. "Adicionado CS2020001194

  LOOP AT it_zsdt0248 INTO wa_zsdt0248.
    CLEAR: wa_saida_0100_02.

    MOVE-CORRESPONDING wa_zsdt0248 TO wa_saida_0100_02.

    wa_saida_0100_02-ds_prod_rendimento = zcl_util=>get_desc_value_domain(  i_domname = 'ZDM_TP_PRODUTO_PRODUCAO' i_domvalue = CONV #( wa_saida_0100_02-tp_produto_producao ) ).

* Habilita campo edição do campo Mercado Intermo
*    CASE wa_zsdt0248-tp_produto_producao.
*      WHEN 'FC'."Farelo Comum
*        wa_saida_0100_02-celltab[] = t_celltab[].
*      WHEN 'FH'."Farelo Hipro
*        wa_saida_0100_02-celltab[] = t_celltab[].
*      WHEN 'OD'."Óleo Degomado
*        wa_saida_0100_02-celltab[] = t_celltab[].
**          WHEN 'CM'."Casca Moída
**          WHEN 'CP'."Casca Peletizada
**          WHEN 'RS'."Residuo Soja
**          WHEN 'SG'."Soja em Grãos
**          WHEN 'SI'."Soja Industrialização
**          WHEN 'HX'."Hexano
*      WHEN OTHERS.
*        wa_saida_0100_02-celltab[] = t_celltab2[].
*    ENDCASE.

    APPEND wa_saida_0100_02 TO it_saida_0100_02.

  ENDLOOP.

*---------------------------------------------------------------------*
* Totalizar Saldos Vinculação
*---------------------------------------------------------------------*

  PERFORM f_totaliza_saldos_vinc.

*---------------------------------------------------------------------*
* Carregar Notas
*---------------------------------------------------------------------*

  PERFORM f_load_notas.


  PERFORM f_disable_time.

  objeto->check_status_boletim( i_id_boletim_producao =  v_id_boletim ).

ENDFORM.

FORM f_atrib_ds_produto_consumo CHANGING p_saida_0100_01 TYPE ty_saida_0100_01.

  CASE tp_boletim.
    WHEN '02' OR '03'. "Biodiesel ou óleo Neutro..
      p_saida_0100_01-ds_prod_consumo = zcl_util=>get_desc_value_domain(  i_domname = 'ZDM_TP_PRODUTO_PRODUCAO' i_domvalue = CONV #( p_saida_0100_01-tp_produto_producao ) ).

      CONCATENATE 'Consumo de' p_saida_0100_01-ds_prod_consumo INTO p_saida_0100_01-ds_prod_consumo SEPARATED BY space.

    WHEN OTHERS. "Farelo e óleo.
      p_saida_0100_01-ds_prod_consumo = zcl_util=>get_desc_value_domain(  i_domname = 'ZDM_TP_PRODUTO_PRODUCAO' i_domvalue = CONV #( p_saida_0100_01-tp_produto_producao ) ).

      CONCATENATE 'Consumo de Soja para' p_saida_0100_01-ds_prod_consumo INTO p_saida_0100_01-ds_prod_consumo SEPARATED BY space.
  ENDCASE.



ENDFORM.

FORM f_search_boletim_producao.

  TYPES: BEGIN OF ty_zsdt0246,
           id_boletim    TYPE zsdt0246-id_boletim,
           dt_producao   TYPE zsdt0246-dt_producao,
           dt_lancamento TYPE zsdt0246-dt_lancamento,
           branch        TYPE zsdt0246-branch,
*-CS2021000386 - 28.04.2021 - JT - inicio
           categ_soja    TYPE zsdt0246-categ_soja,
           descr_soja    TYPE zdescricao_char, "zsdt0246-categ_soja,
*-CS2021000386 - 28.04.2021 - JT - fim
           charg         TYPE zsdt0246-charg,
           status        TYPE zsdt0246-status,
           dt_registro   TYPE zsdt0246-dt_registro,
           hr_registro   TYPE zsdt0246-hr_registro,
           us_registro   TYPE zsdt0246-us_registro,
           dt_aprovacao  TYPE zsdt0246-dt_aprovacao,
           hr_aprovacao  TYPE zsdt0246-hr_aprovacao,
           us_aprovacao  TYPE zsdt0246-us_aprovacao,
         END OF ty_zsdt0246.

  DATA: gt_zsdt0246     TYPE TABLE OF ty_zsdt0246 WITH HEADER LINE,
        gt_zsdt0246_aux TYPE TABLE OF ty_zsdt0246 WITH HEADER LINE.

  CLEAR: gt_return_tab, gt_return_tab[], gt_zsdt0246[].

  CASE tp_boletim.
    WHEN '01'.
      SELECT *
      FROM zsdt0246 INTO CORRESPONDING FIELDS OF TABLE gt_zsdt0246
      WHERE loekz EQ abap_false AND tp_boletim = tp_boletim OR tp_boletim EQ space.
    WHEN '02' OR '03'.
      SELECT *
      FROM zsdt0246 INTO CORRESPONDING FIELDS OF TABLE gt_zsdt0246
      WHERE loekz EQ abap_false AND tp_boletim = tp_boletim.
    WHEN OTHERS.
  ENDCASE.

  LOOP AT gt_zsdt0246 INTO DATA(w_zsdt0246).
    CASE tp_boletim.
      WHEN '02'.
      WHEN '03'.
      WHEN OTHERS.
        IF  w_zsdt0246-categ_soja = 'RR'.
          w_zsdt0246-descr_soja = 'Transgênica'.
        ELSEIF w_zsdt0246-categ_soja = 'CO'.
          w_zsdt0246-descr_soja = 'Convencional'.

          "FF #191283 - inicio
        ELSEIF w_zsdt0246-categ_soja = 'RE'.
          w_zsdt0246-descr_soja = 'Transgênica EUDR'.

        ELSEIF w_zsdt0246-categ_soja = 'CE'.
          w_zsdt0246-descr_soja = 'Convencional EUDR'.
          "FF #191283 - fim

        ENDIF.
        MODIFY gt_zsdt0246 FROM w_zsdt0246 INDEX sy-tabix.
    ENDCASE.
  ENDLOOP.

  "Valida permissão Filial...
  gt_zsdt0246_aux[] = gt_zsdt0246[].
  SORT gt_zsdt0246_aux BY branch.
  DELETE ADJACENT DUPLICATES FROM gt_zsdt0246_aux COMPARING branch.

  LOOP AT gt_zsdt0246_aux.
    AUTHORITY-CHECK OBJECT 'M_MATE_WRK'
      ID 'WERKS' FIELD  gt_zsdt0246_aux-branch
      ID 'ACTVT' FIELD '03'.

    IF sy-subrc NE 0.
      DELETE gt_zsdt0246 WHERE branch = gt_zsdt0246_aux-branch.
    ENDIF.
  ENDLOOP.

  SORT gt_zsdt0246 BY id_boletim DESCENDING.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'ID_BOLETIM'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      "DYNPROFIELD     = 'V_BOLETIM_SEL'
      value_org       = 'S'
    TABLES
      value_tab       = gt_zsdt0246
      return_tab      = gt_return_tab
      dynpfld_mapping = gt_dselc.

  READ TABLE gt_return_tab INDEX 1.
  IF sy-subrc = 0.
    READ TABLE gt_zsdt0246 WITH KEY id_boletim = gt_return_tab-fieldval.
    IF sy-subrc = 0.
      PERFORM f_free_alv USING: '0120_01', '0120_02'.
      PERFORM f_set_operacao_boletim USING c_view_bol.
      PERFORM f_load_boletim USING gt_zsdt0246-id_boletim.
      LEAVE TO SCREEN 0100.
    ENDIF.
  ENDIF.

ENDFORM.

FORM f_set_operacao_boletim USING p_operacao.

  PERFORM: f_free_alv USING '0100_01',
           f_free_alv USING '0100_02'.

  vg_operacao_bol = p_operacao.

ENDFORM.

FORM f_del_boletim_producao.

  TRY.
      objeto->deletar_registro( i_id_boletim_producao = wg_cab_boletim_prod-id_boletim ).

      PERFORM f_limpa_dados_screen.

      PERFORM f_set_operacao_boletim USING c_view_bol.

      LEAVE TO SCREEN 0100.

    CATCH zcx_boletim_producao INTO DATA(zcx_boletim).
      zcx_boletim->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
      RETURN.
  ENDTRY.


ENDFORM.

FORM f_limpa_dados_screen.

  CLEAR: wa_zsdt0246, it_zsdt0247, it_zsdt0248.

  CLEAR: wg_cab_boletim_prod,
         it_saida_0100_01[],
         it_saida_0100_02[],
         it_saida_0120_01[],
         it_saida_0120_02[].

ENDFORM.

FORM f_config_exc_tcode_0100.

  DATA: v_fcode  TYPE sy-ucomm,
        v_id_bol TYPE zsdt0246-id_boletim.

  CLEAR: tg_exc_tcode_0100[].

  CASE vg_operacao_bol.
    WHEN c_new_bol OR c_edit_bol.

      v_fcode = c_new_bol.
      APPEND v_fcode TO tg_exc_tcode_0100.

*      v_fcode = c_sel_show_msg.
*      APPEND v_fcode TO tg_exc_tcode_0100.


      v_fcode = c_edit_bol.
      APPEND v_fcode TO tg_exc_tcode_0100.

      v_fcode = c_del_bol.
      APPEND v_fcode TO tg_exc_tcode_0100.

      v_fcode = c_search_bol.
      APPEND v_fcode TO tg_exc_tcode_0100.

      v_fcode = c_atua_bol.
      APPEND v_fcode TO tg_exc_tcode_0100.

      v_fcode = c_aprov_bol.
      APPEND v_fcode TO tg_exc_tcode_0100.

      v_fcode = c_desaprov_bol.
      APPEND v_fcode TO tg_exc_tcode_0100.

      LOOP AT SCREEN.
        IF screen-name EQ 'BTN_MIN_DETAIL_BOL'.
          screen-active  = '0'.
          MODIFY SCREEN.
        ENDIF.
      ENDLOOP.

    WHEN OTHERS.

      v_fcode = c_canc_edit.
      APPEND v_fcode TO tg_exc_tcode_0100.

      v_fcode = c_save_bol.
      APPEND v_fcode TO tg_exc_tcode_0100.

*      v_fcode = c_sel_show_msg.
*      APPEND v_fcode TO tg_exc_tcode_0100.

      IF wg_cab_boletim_prod-id_boletim IS INITIAL.

        v_fcode = c_aprov_bol.
        APPEND v_fcode TO tg_exc_tcode_0100.

        v_fcode = c_desaprov_bol.
        APPEND v_fcode TO tg_exc_tcode_0100.

        v_fcode = c_del_bol.
        APPEND v_fcode TO tg_exc_tcode_0100.

        v_fcode = c_edit_bol.
        APPEND v_fcode TO tg_exc_tcode_0100.

        v_fcode = c_atua_bol.
        APPEND v_fcode TO tg_exc_tcode_0100.

      ELSE.
        v_id_bol = wg_cab_boletim_prod-id_boletim.
        SELECT SINGLE * FROM zsdt0246 INTO @DATA(w_246) WHERE id_boletim EQ @v_id_bol.
        IF w_246-aprovado EQ abap_true.
          v_fcode = c_aprov_bol.
          APPEND v_fcode TO tg_exc_tcode_0100.
        ELSE.
          v_fcode = c_desaprov_bol.
          APPEND v_fcode TO tg_exc_tcode_0100.
        ENDIF.
      ENDIF.

      AUTHORITY-CHECK OBJECT 'ZSDT0170'
      ID 'ZACT_BOL' FIELD '01' . "Autorizar aprovar boletim de produção
      IF sy-subrc <> 0.
        v_fcode = c_aprov_bol.
        APPEND v_fcode TO tg_exc_tcode_0100.
      ENDIF.

      AUTHORITY-CHECK OBJECT 'ZSDT0170'
      ID 'ZACT_BOL' FIELD '02'. "Autorizar desaprova boletim de produção
      IF sy-subrc <> 0.
        v_fcode = c_desaprov_bol.
        APPEND v_fcode TO tg_exc_tcode_0100.
      ENDIF.
  ENDCASE.

  PERFORM f_config_tcode_time TABLES tg_exc_tcode_0100.

ENDFORM.


FORM f_config_exc_tcode_0120.

  DATA: v_fcode TYPE sy-ucomm.

  CLEAR: tg_exc_tcode_0120[].

  PERFORM f_config_tcode_time TABLES tg_exc_tcode_0120.

ENDFORM.

FORM f_config_tcode_time TABLES t_tcode LIKE tg_exc_tcode_0100.

  DATA: v_fcode TYPE sy-ucomm.

  CLEAR: tg_exc_tcode_0120[].

  DATA(_active_time) = abap_false.
  PERFORM f_get_status_time CHANGING _active_time.

  IF ( wg_cab_boletim_prod-id_boletim IS NOT INITIAL ) AND
     ( vg_operacao_bol NE c_new_bol  ) AND
     ( vg_operacao_bol NE c_edit_bol ).

    IF ( _active_time EQ abap_true ).
      APPEND c_enable_tim  TO t_tcode.
    ELSE.
      APPEND c_disable_tim TO t_tcode.
    ENDIF.

  ELSE.
    APPEND c_enable_tim  TO t_tcode.
    APPEND c_disable_tim TO t_tcode.
  ENDIF.

ENDFORM.

FORM f_load_notas.

  PERFORM: f_load_notas_avinc,
           f_load_notas_vinc.

  PERFORM f_refresh_alv USING: '0120_01',
                               '0120_02'.

ENDFORM.

FORM f_valida_notas_avinc TABLES t_zsdt0251 STRUCTURE zsdt0251.

  DATA: t_active     TYPE TABLE OF j_1bnfe_active WITH HEADER LINE,
        t_doc        TYPE TABLE OF j_1bnfdoc      WITH HEADER LINE,
        t_zfiwrt0008 TYPE TABLE OF zfiwrt0008     WITH HEADER LINE,
        t_zlest0060  TYPE TABLE OF zlest0060      WITH HEADER LINE.

  IF sy-sysid EQ 'DEV'.
    CHECK 1 = 2.
  ENDIF.

  CHECK t_zsdt0251[] IS NOT INITIAL.

*----------------------------------------------------------------------------------------*
* Validação se Nota de Saida esta autorizada
*----------------------------------------------------------------------------------------*
  CLEAR: t_active[], t_doc[].

  SELECT *
    FROM j_1bnfe_active INTO TABLE t_active
     FOR ALL ENTRIES IN t_zsdt0251
   WHERE docnum  EQ t_zsdt0251-docnum.

  SELECT *
    FROM j_1bnfdoc INTO TABLE t_doc
     FOR ALL ENTRIES IN t_zsdt0251
   WHERE docnum  EQ t_zsdt0251-docnum.

  PERFORM f_check_auth_tables_nfe TABLES t_active t_doc.

  LOOP AT t_zsdt0251 ASSIGNING FIELD-SYMBOL(<fs_0251>).

    READ TABLE t_active WITH KEY docnum = <fs_0251>-docnum.
    IF sy-subrc NE 0.
      <fs_0251>-loekz = abap_true.
      CONTINUE.
    ENDIF.

    READ TABLE t_doc WITH KEY docnum   = <fs_0251>-docnum.
    IF sy-subrc NE 0.
      <fs_0251>-loekz = abap_true.
      CONTINUE.
    ENDIF.

  ENDLOOP.

  DELETE t_zsdt0251 WHERE loekz EQ abap_true.

*----------------------------------------------------------------------------------------*
* Validação se NF-e de Saida esta escriturada na filial de Destino
*----------------------------------------------------------------------------------------*

  CHECK t_zsdt0251[] IS NOT INITIAL.

  CLEAR: t_zfiwrt0008[], t_active[], t_doc[].

  "Busca Entrada pelo fluxo da ZNFW
  LOOP AT t_zsdt0251 INTO DATA(wl_zsdt0251).

    SELECT SINGLE *
      FROM zfiwrt0008 INTO @DATA(wl_0008)
     WHERE docnum_saida     EQ @wl_zsdt0251-docnum
       AND docs_estornados  EQ @abap_false
       AND loekz            EQ @abap_false.

    CHECK ( sy-subrc EQ 0 ).

    APPEND wl_0008 TO t_zfiwrt0008.

  ENDLOOP.

  IF t_zfiwrt0008[] IS NOT INITIAL.
    SELECT *
      FROM j_1bnfdoc INTO TABLE t_doc
       FOR ALL ENTRIES IN t_zfiwrt0008
     WHERE docnum EQ t_zfiwrt0008-docnum.

    SELECT *
      FROM j_1bnfe_active INTO TABLE t_active
       FOR ALL ENTRIES IN t_zfiwrt0008
     WHERE docnum EQ t_zfiwrt0008-docnum.

    PERFORM f_check_auth_tables_nfe TABLES t_active t_doc.
  ENDIF.

  LOOP AT t_zsdt0251 ASSIGNING <fs_0251>.

    READ TABLE t_zfiwrt0008 WITH KEY docnum_saida = <fs_0251>-docnum.
    IF sy-subrc NE 0.
      <fs_0251>-loekz = abap_true.
      CONTINUE.
    ENDIF.

    READ TABLE t_active WITH KEY docnum = t_zfiwrt0008-docnum.
    IF sy-subrc NE 0.
      <fs_0251>-loekz = abap_true.
      CONTINUE.
    ENDIF.

    READ TABLE t_doc WITH KEY docnum = t_zfiwrt0008-docnum.
    IF sy-subrc NE 0.
      <fs_0251>-loekz = abap_true.
      CONTINUE.
    ENDIF.

    <fs_0251>-docdat = t_doc-docdat.

  ENDLOOP.

  DELETE t_zsdt0251 WHERE loekz EQ abap_true.

*----------------------------------------------------------------------------------------*
* Atribuição Data Chegada
*----------------------------------------------------------------------------------------*
  CHECK t_zsdt0251[] IS NOT INITIAL.

  CLEAR: t_zlest0060[].

  SELECT *
    FROM zlest0060 INTO TABLE t_zlest0060
     FOR ALL ENTRIES IN t_zsdt0251
   WHERE docnum_rem = t_zsdt0251-docnum.

  LOOP AT t_zsdt0251 ASSIGNING <fs_0251>.
    READ TABLE t_zlest0060 WITH KEY docnum = <fs_0251>-docnum.
    IF sy-subrc EQ 0.
      <fs_0251>-dt_chegada = t_zlest0060-dt_chegada.
    ENDIF.
  ENDLOOP.

ENDFORM.

FORM f_check_auth_tables_nfe  TABLES t_active STRUCTURE j_1bnfe_active
                                     t_doc    STRUCTURE j_1bnfdoc.

  DATA: v_candat_null TYPE j_1bnfdoc-candat.

  CLEAR: v_candat_null.

  DELETE t_active WHERE NOT ( docsta  EQ '1' AND cancel EQ abap_false AND scssta NE '2' ).
  DELETE t_doc    WHERE NOT ( docstat EQ '1' AND cancel EQ abap_false AND candat EQ v_candat_null ).

ENDFORM.

FORM f_load_notas_avinc.

  DATA: it_notas_agr TYPE zsdt0251_t.

  DATA: v_count_dias TYPE i.

  DATA: t_doc TYPE TABLE OF j_1bnfdoc WITH HEADER LINE.

  CLEAR: it_j_1bbranch[], it_zsdt0251[], it_zsdt0251_agr[], it_saida_0120_01[], wa_zsdt0253.

  CHECK wg_cab_boletim_prod-id_boletim IS NOT INITIAL.

  CHECK ( vg_operacao_bol NE c_new_bol ) AND ( vg_operacao_bol NE c_edit_bol  ).

  SELECT SINGLE * FROM zsdt0253 INTO wa_zsdt0253 WHERE branch EQ wg_cab_boletim_prod-branch.

  SELECT *
    FROM zsdt0251 INTO TABLE it_zsdt0251
   WHERE qtde_saldo   GT 0
     AND loekz        EQ abap_false.

  DELETE it_zsdt0251 WHERE NOT ( branch_destino EQ wg_cab_boletim_prod-branch AND charg EQ wg_cab_boletim_prod-charg ).

*-CS2021000386 - 28.04.2021 - JT - inicio
  SELECT SINGLE emissao_nf
         INTO @DATA(l_emissao_nf)
         FROM zsdt0253
        WHERE branch = @wg_cab_boletim_prod-branch.

  IF l_emissao_nf = abap_true.
    LOOP AT it_zsdt0251.
      IF wg_cab_boletim_prod-categ_soja = 'CO' AND
         it_zsdt0251-lgort_v <> 'PO58'.
        DELETE it_zsdt0251 INDEX sy-tabix.
        CONTINUE.
      ENDIF.
      IF wg_cab_boletim_prod-categ_soja = 'RR' AND
         it_zsdt0251-lgort_v <> 'PO17'.
        DELETE it_zsdt0251 INDEX sy-tabix.
        CONTINUE.
      ENDIF.

      "FF #191283 - inicio
      IF wg_cab_boletim_prod-categ_soja = 'RE' AND
         it_zsdt0251-lgort_v <> 'POD2'.
        DELETE it_zsdt0251 INDEX sy-tabix.
        CONTINUE.
      ENDIF.

      IF wg_cab_boletim_prod-categ_soja = 'CE' AND
   it_zsdt0251-lgort_v <> 'POD3'.
        DELETE it_zsdt0251 INDEX sy-tabix.
        CONTINUE.
      ENDIF.
      "FF #191283 - fim.



    ENDLOOP.
  ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

  CHECK it_zsdt0251[] IS NOT INITIAL.

  PERFORM f_valida_notas_avinc TABLES it_zsdt0251.

  it_zsdt0251_agr[] = it_zsdt0251[].

  SORT it_zsdt0251_agr BY branch charg.
  DELETE ADJACENT DUPLICATES FROM it_zsdt0251_agr COMPARING branch charg.

  CHECK it_zsdt0251_agr[] IS NOT INITIAL.

  SELECT *
    FROM j_1bbranch INTO TABLE it_j_1bbranch
     FOR ALL ENTRIES IN it_zsdt0251_agr
   WHERE branch EQ it_zsdt0251_agr-branch.

  LOOP AT it_zsdt0251_agr.

    CLEAR: wa_saida_0120_01, it_notas_agr[].

    wa_saida_0120_01-branch    = it_zsdt0251_agr-branch.

    READ TABLE it_j_1bbranch WITH KEY branch = wa_saida_0120_01-branch.
    IF sy-subrc EQ 0.
      wa_saida_0120_01-ds_branch = it_j_1bbranch-name.
    ENDIF.

    wa_saida_0120_01-charg = it_zsdt0251_agr-charg.

    LOOP AT it_zsdt0251 WHERE branch EQ it_zsdt0251_agr-branch
                          AND charg  EQ it_zsdt0251_agr-charg.

      ADD it_zsdt0251-qtde_saldo TO wa_saida_0120_01-saldo.

      IF wa_zsdt0253-qtde_dias_emi > 0.

        v_count_dias = sy-datum - it_zsdt0251-docdat.

        IF v_count_dias > wa_zsdt0253-qtde_dias_emi.
          ADD 1                      TO wa_saida_0120_01-qtde_nf_emi_lim.
          ADD it_zsdt0251-qtde_saldo TO wa_saida_0120_01-saldo_nf_emi_lim.
          it_zsdt0251-nf_emissao_old  = abap_true.
        ENDIF.
      ENDIF.

      APPEND it_zsdt0251 TO it_notas_agr.
    ENDLOOP.

    SORT it_notas_agr BY docdat.

    wa_saida_0120_01-notas = it_notas_agr.

    APPEND wa_saida_0120_01 TO it_saida_0120_01.

  ENDLOOP.

  SORT it_saida_0120_01 BY qtde_nf_emi_lim DESCENDING.


ENDFORM.

FORM f_load_notas_vinc.

  DATA: t_active     TYPE TABLE OF j_1bnfe_active WITH HEADER LINE,
        t_active_doc TYPE TABLE OF j_1bnfe_active WITH HEADER LINE,
        t_doc        TYPE TABLE OF j_1bnfdoc      WITH HEADER LINE,
        t_zfiwrt0008 TYPE TABLE OF zfiwrt0008     WITH HEADER LINE,
        t_mseg       TYPE TABLE OF mseg           WITH HEADER LINE,
        t_zfiwrt1000 TYPE TABLE OF zfiwrt1000     WITH HEADER LINE.

  DATA: wl_color  TYPE kkblo_specialcol.

  CLEAR: it_j_1bbranch[], it_zsdt0252[], it_saida_0120_02[],
         t_active[], t_doc[], t_zfiwrt0008[], t_mseg[], t_zfiwrt1000[], t_active_doc[].

  CHECK wg_cab_boletim_prod-id_boletim IS NOT INITIAL.

  CHECK ( vg_operacao_bol NE c_new_bol ) AND ( vg_operacao_bol NE c_edit_bol  ).

  SELECT *
    FROM zsdt0252 INTO TABLE it_zsdt0252
   WHERE id_boletim EQ wg_cab_boletim_prod-id_boletim.

  CHECK it_zsdt0252[] IS NOT INITIAL.

  SELECT *
    FROM j_1bbranch INTO TABLE it_j_1bbranch
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE branch EQ it_zsdt0252-branch.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_devol.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_ent_dev.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_ind.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_ent_ind.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_rfl_01.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_rfl_02.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_rfl_03.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_rco_01.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_rco_02.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_rco_03.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_ent_rco_01.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_ent_rco_02.

  SELECT *
    FROM zfiwrt0008 APPENDING TABLE t_zfiwrt0008
     FOR ALL ENTRIES IN it_zsdt0252
   WHERE seq_lcto EQ it_zsdt0252-seqlcto_ent_rco_03.

  IF t_zfiwrt0008[] IS NOT INITIAL.
    SELECT *
      FROM j_1bnfdoc INTO TABLE t_doc
       FOR ALL ENTRIES IN t_zfiwrt0008
     WHERE docnum EQ t_zfiwrt0008-docnum.

    SELECT *
      FROM j_1bnfe_active INTO TABLE t_active
       FOR ALL ENTRIES IN t_zfiwrt0008
     WHERE docnum EQ t_zfiwrt0008-docnum.

    SELECT *
      FROM j_1bnfe_active INTO TABLE t_active_doc
       FOR ALL ENTRIES IN t_zfiwrt0008
     WHERE docnum EQ t_zfiwrt0008-docnum.

    PERFORM f_check_auth_tables_nfe TABLES t_active t_doc.

    SELECT *
      FROM zfiwrt1000 INTO TABLE t_zfiwrt1000
       FOR ALL ENTRIES IN t_zfiwrt0008
     WHERE seq_lcto EQ t_zfiwrt0008-seq_lcto.

  ENDIF.

  it_zsdt0252_aux[] = it_zsdt0252[].
  DELETE it_zsdt0252_aux WHERE doc_prod_01 IS INITIAL.
  IF it_zsdt0252_aux[] IS NOT INITIAL.
    SELECT *
      FROM mseg APPENDING TABLE t_mseg
       FOR ALL ENTRIES IN it_zsdt0252_aux
     WHERE smbln EQ it_zsdt0252_aux-doc_prod_01.
  ENDIF.

  it_zsdt0252_aux[] = it_zsdt0252[].
  DELETE it_zsdt0252_aux WHERE doc_prod_02 IS INITIAL.
  IF it_zsdt0252_aux[] IS NOT INITIAL.
    SELECT *
      FROM mseg APPENDING TABLE t_mseg
       FOR ALL ENTRIES IN it_zsdt0252_aux
     WHERE smbln EQ it_zsdt0252_aux-doc_prod_02.
  ENDIF.

  it_zsdt0252_aux[] = it_zsdt0252[].
  DELETE it_zsdt0252_aux WHERE doc_prod_03 IS INITIAL.
  IF it_zsdt0252_aux[] IS NOT INITIAL.
    SELECT *
      FROM mseg APPENDING TABLE t_mseg
       FOR ALL ENTRIES IN it_zsdt0252_aux
     WHERE smbln EQ it_zsdt0252_aux-doc_prod_03.
  ENDIF.

  it_zsdt0252_aux[] = it_zsdt0252[].
  DELETE it_zsdt0252_aux WHERE doc_prod_04 IS INITIAL.
  IF it_zsdt0252_aux[] IS NOT INITIAL.
    SELECT *
      FROM mseg APPENDING TABLE t_mseg
       FOR ALL ENTRIES IN it_zsdt0252_aux
     WHERE smbln EQ it_zsdt0252_aux-doc_prod_04.
  ENDIF.

  it_zsdt0252_aux[] = it_zsdt0252[].
  DELETE it_zsdt0252_aux WHERE doc_prod_05 IS INITIAL.
  IF it_zsdt0252_aux[] IS NOT INITIAL.
    SELECT *
      FROM mseg APPENDING TABLE t_mseg
       FOR ALL ENTRIES IN it_zsdt0252_aux
     WHERE smbln EQ it_zsdt0252_aux-doc_prod_05.
  ENDIF.

  LOOP AT it_zsdt0252.

    CLEAR: wa_saida_0120_02.

    wa_saida_0120_02-id_boletim  = wg_cab_boletim_prod-id_boletim.
    wa_saida_0120_02-branch      = it_zsdt0252-branch.

    READ TABLE it_j_1bbranch WITH KEY branch = wa_saida_0120_02-branch.
    IF sy-subrc EQ 0.
      wa_saida_0120_02-bukrs     = it_j_1bbranch-bukrs.
      wa_saida_0120_02-ds_branch = it_j_1bbranch-name.
    ENDIF.

    wa_saida_0120_02-charg            = it_zsdt0252-charg.
    wa_saida_0120_02-id_agrp          = it_zsdt0252-id_agrp.
    wa_saida_0120_02-saldo_vinc       = it_zsdt0252-qtde_vinc.

*------------------------------------------------------------------------------------------------------*
*   NF Devolução - Saida e Entrada
*------------------------------------------------------------------------------------------------------*
    IF it_zsdt0252-seqlcto_devol IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_devol.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_devol = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_devol  = it_zsdt0252-seqlcto_devol.
          wa_saida_0120_02-docnum_devol   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_devol   = icon_execute_object.

          IF wa_saida_0120_02-docnum_devol IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_devol.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_devol = t_active-nfnum9.
            ELSE.

              READ TABLE t_active_doc WITH KEY docnum = t_zfiwrt0008-docnum.
              IF ( sy-subrc EQ 0 ) AND ( t_active_doc-action_requ IS INITIAL ).
                wa_saida_0120_02-nfenum_devol = icon_activity.
              ENDIF.

            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_devol = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    IF it_zsdt0252-seqlcto_ent_dev IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_ent_dev.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_ent_dev = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_ent_dev  = it_zsdt0252-seqlcto_ent_dev.
          wa_saida_0120_02-docnum_ent_dev   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_ent_dev   = icon_execute_object.

          IF wa_saida_0120_02-docnum_ent_dev IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_ent_dev.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_ent_dev = t_active-nfnum9.
            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_ent_dev = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

*------------------------------------------------------------------------------------------------------*
*   NF Industrialização - Saida e Entrada
*------------------------------------------------------------------------------------------------------*
    IF it_zsdt0252-seqlcto_ind IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_ind.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_ind = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_ind  = it_zsdt0252-seqlcto_ind.
          wa_saida_0120_02-docnum_ind   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_ind   = icon_execute_object.

          IF wa_saida_0120_02-docnum_ind IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_ind.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_ind = t_active-nfnum9.
            ELSE.

              READ TABLE t_active_doc WITH KEY docnum = t_zfiwrt0008-docnum.
              IF ( sy-subrc EQ 0 ) AND ( t_active_doc-action_requ IS INITIAL ).
                wa_saida_0120_02-nfenum_ind = icon_activity.
              ENDIF.

            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_ind = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    IF it_zsdt0252-seqlcto_ent_ind IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_ent_ind.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_ent_ind = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_ent_ind  = it_zsdt0252-seqlcto_ent_ind.
          wa_saida_0120_02-docnum_ent_ind   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_ent_ind   = icon_execute_object.

          IF wa_saida_0120_02-docnum_ent_ind IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_ent_ind.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_ent_ind = t_active-nfnum9.
            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_ent_ind = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

*------------------------------------------------------------------------------------------------------*
*   Docs. Produção
*------------------------------------------------------------------------------------------------------*

    IF it_zsdt0252-doc_prod_01 IS NOT INITIAL.
      READ TABLE t_mseg WITH KEY smbln = it_zsdt0252-doc_prod_01.
      IF ( sy-subrc EQ 0 ).
        UPDATE zsdt0252 SET doc_prod_01 = space
         WHERE id_boletim EQ it_zsdt0252-id_boletim
           AND branch     EQ it_zsdt0252-branch
           AND charg      EQ it_zsdt0252-charg
           AND id_agrp    EQ it_zsdt0252-id_agrp.
      ELSE.
        wa_saida_0120_02-doc_prod_01 = it_zsdt0252-doc_prod_01.

        TRY.
            DATA(_valida_mat_doc) = objeto->validar_materiais_doc_prod( i_mblnr =  CONV #( it_zsdt0252-doc_prod_01 )  ).
          CATCH zcx_boletim_producao.
            CLEAR: _valida_mat_doc.
        ENDTRY.

        IF _valida_mat_doc EQ abap_false.
          wl_color-fieldname = 'DOC_PROD_01'. wl_color-color-col = 6. wl_color-color-int = 0. wl_color-color-inv = 0.
          APPEND wl_color TO wa_saida_0120_02-color.
        ENDIF.

      ENDIF.
    ENDIF.

    IF it_zsdt0252-doc_prod_02 IS NOT INITIAL.
      READ TABLE t_mseg WITH KEY smbln = it_zsdt0252-doc_prod_02.
      IF ( sy-subrc EQ 0 ).
        UPDATE zsdt0252 SET doc_prod_02 = space
         WHERE id_boletim EQ it_zsdt0252-id_boletim
           AND branch     EQ it_zsdt0252-branch
           AND charg      EQ it_zsdt0252-charg
           AND id_agrp    EQ it_zsdt0252-id_agrp.
      ELSE.
        wa_saida_0120_02-doc_prod_02 = it_zsdt0252-doc_prod_02.

        TRY.
            _valida_mat_doc = objeto->validar_materiais_doc_prod( i_mblnr =  CONV #( it_zsdt0252-doc_prod_02 )  ).
          CATCH zcx_boletim_producao.
            CLEAR: _valida_mat_doc.
        ENDTRY.

        IF _valida_mat_doc EQ abap_false.
          wl_color-fieldname = 'DOC_PROD_02'. wl_color-color-col = 6. wl_color-color-int = 0. wl_color-color-inv = 0.
          APPEND wl_color TO wa_saida_0120_02-color.
        ENDIF.

      ENDIF.
    ENDIF.

    IF it_zsdt0252-doc_prod_03 IS NOT INITIAL.
      READ TABLE t_mseg WITH KEY smbln = it_zsdt0252-doc_prod_03.
      IF ( sy-subrc EQ 0 ).
        UPDATE zsdt0252 SET doc_prod_03 = space
         WHERE id_boletim EQ it_zsdt0252-id_boletim
           AND branch     EQ it_zsdt0252-branch
           AND charg      EQ it_zsdt0252-charg
           AND id_agrp    EQ it_zsdt0252-id_agrp.
      ELSE.
        wa_saida_0120_02-doc_prod_03 = it_zsdt0252-doc_prod_03.

        TRY.
            _valida_mat_doc = objeto->validar_materiais_doc_prod( i_mblnr =  CONV #( it_zsdt0252-doc_prod_03 )  ).
          CATCH zcx_boletim_producao.
            CLEAR: _valida_mat_doc.
        ENDTRY.

        IF _valida_mat_doc EQ abap_false.
          wl_color-fieldname = 'DOC_PROD_03'. wl_color-color-col = 6. wl_color-color-int = 0. wl_color-color-inv = 0.
          APPEND wl_color TO wa_saida_0120_02-color.
        ENDIF.

      ENDIF.
    ENDIF.

    IF it_zsdt0252-doc_prod_04 IS NOT INITIAL.
      READ TABLE t_mseg WITH KEY smbln = it_zsdt0252-doc_prod_04.
      IF ( sy-subrc EQ 0 ).
        UPDATE zsdt0252 SET doc_prod_04 = space
         WHERE id_boletim EQ it_zsdt0252-id_boletim
           AND branch     EQ it_zsdt0252-branch
           AND charg      EQ it_zsdt0252-charg
           AND id_agrp    EQ it_zsdt0252-id_agrp.
      ELSE.
        wa_saida_0120_02-doc_prod_04 = it_zsdt0252-doc_prod_04.

        TRY.
            _valida_mat_doc = objeto->validar_materiais_doc_prod( i_mblnr =  CONV #( it_zsdt0252-doc_prod_04 )  ).
          CATCH zcx_boletim_producao.
            CLEAR: _valida_mat_doc.
        ENDTRY.

        IF _valida_mat_doc EQ abap_false.
          wl_color-fieldname = 'DOC_PROD_04'. wl_color-color-col = 6. wl_color-color-int = 0. wl_color-color-inv = 0.
          APPEND wl_color TO wa_saida_0120_02-color.
        ENDIF.

      ENDIF.
    ENDIF.

    IF it_zsdt0252-doc_prod_05 IS NOT INITIAL.
      READ TABLE t_mseg WITH KEY smbln = it_zsdt0252-doc_prod_05.
      IF ( sy-subrc EQ 0 ).
        UPDATE zsdt0252 SET doc_prod_05 = space
         WHERE id_boletim EQ it_zsdt0252-id_boletim
           AND branch     EQ it_zsdt0252-branch
           AND charg      EQ it_zsdt0252-charg
           AND id_agrp    EQ it_zsdt0252-id_agrp.
      ELSE.
        wa_saida_0120_02-doc_prod_05 = it_zsdt0252-doc_prod_05.

        TRY.
            _valida_mat_doc = objeto->validar_materiais_doc_prod( i_mblnr =  CONV #( it_zsdt0252-doc_prod_05 )  ).
          CATCH zcx_boletim_producao.
            CLEAR: _valida_mat_doc.
        ENDTRY.

        IF _valida_mat_doc EQ abap_false.
          wl_color-fieldname = 'DOC_PROD_05'. wl_color-color-col = 6. wl_color-color-int = 0. wl_color-color-inv = 0.
          APPEND wl_color TO wa_saida_0120_02-color.
        ENDIF.

      ENDIF.
    ENDIF.


*------------------------------------------------------------------------------------------------------*
*   Rem. Formação Lote 1
*------------------------------------------------------------------------------------------------------*
    IF it_zsdt0252-seqlcto_rfl_01 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_rfl_01.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_rfl_01 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_rfl_01  = it_zsdt0252-seqlcto_rfl_01.
          wa_saida_0120_02-docnum_rfl_01   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_rfl_01   = icon_execute_object.

          IF wa_saida_0120_02-docnum_rfl_01 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_rfl_01.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_rfl_01 = t_active-nfnum9.
            ELSE.

              READ TABLE t_active_doc WITH KEY docnum = t_zfiwrt0008-docnum.
              IF ( sy-subrc EQ 0 ) AND ( t_active_doc-action_requ IS INITIAL ).
                wa_saida_0120_02-nfenum_rfl_01 = icon_activity.
              ENDIF.

            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_rfl_01 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

*------------------------------------------------------------------------------------------------------*
*   Rem. Formação Lote 2
*------------------------------------------------------------------------------------------------------*
    IF it_zsdt0252-seqlcto_rfl_02 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_rfl_02.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_rfl_02 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_rfl_02  = it_zsdt0252-seqlcto_rfl_02.
          wa_saida_0120_02-docnum_rfl_02   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_rfl_02   = icon_execute_object.

          IF wa_saida_0120_02-docnum_rfl_02 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_rfl_02.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_rfl_02 = t_active-nfnum9.
            ELSE.

              READ TABLE t_active_doc WITH KEY docnum = t_zfiwrt0008-docnum.
              IF ( sy-subrc EQ 0 ) AND ( t_active_doc-action_requ IS INITIAL ).
                wa_saida_0120_02-nfenum_rfl_02 = icon_activity.
              ENDIF.

            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_rfl_02 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

*------------------------------------------------------------------------------------------------------*
*   Rem. Formação Lote 3
*------------------------------------------------------------------------------------------------------*
    IF it_zsdt0252-seqlcto_rfl_03 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_rfl_03.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_rfl_03 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_rfl_03  = it_zsdt0252-seqlcto_rfl_03.
          wa_saida_0120_02-docnum_rfl_03   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_rfl_03   = icon_execute_object.

          IF wa_saida_0120_02-docnum_rfl_03 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_rfl_03.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_rfl_03 = t_active-nfnum9.
            ELSE.

              READ TABLE t_active_doc WITH KEY docnum = t_zfiwrt0008-docnum.
              IF ( sy-subrc EQ 0 ) AND ( t_active_doc-action_requ IS INITIAL ).
                wa_saida_0120_02-nfenum_rfl_03 = icon_activity.
              ENDIF.

            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_rfl_03 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

*------------------------------------------------------------------------------------------------------*
*   NF Remessa Conta e Ordem 01 - Saida e Entrada
*------------------------------------------------------------------------------------------------------*
    IF it_zsdt0252-seqlcto_rco_01 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_rco_01.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_rco_01 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_rco_01  = it_zsdt0252-seqlcto_rco_01.
          wa_saida_0120_02-docnum_rco_01   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_rco_01   = icon_execute_object.

          IF wa_saida_0120_02-docnum_rco_01 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_rco_01.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_rco_01 = t_active-nfnum9.
            ELSE.

              READ TABLE t_active_doc WITH KEY docnum = t_zfiwrt0008-docnum.
              IF ( sy-subrc EQ 0 ) AND ( t_active_doc-action_requ IS INITIAL ).
                wa_saida_0120_02-nfenum_rco_01 = icon_activity.
              ENDIF.

            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_rco_01 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    IF it_zsdt0252-seqlcto_ent_rco_01 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_ent_rco_01.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_ent_rco_01 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_ent_rco_01  = it_zsdt0252-seqlcto_ent_rco_01.
          wa_saida_0120_02-docnum_ent_rco_01   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_ent_rco_01   = icon_execute_object.

          IF wa_saida_0120_02-docnum_ent_rco_01 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_ent_rco_01.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_ent_rco_01 = t_active-nfnum9.
            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_ent_rco_01 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

*------------------------------------------------------------------------------------------------------*
*   NF Remessa Conta e Ordem 02 - Saida e Entrada
*------------------------------------------------------------------------------------------------------*

    IF it_zsdt0252-seqlcto_rco_02 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_rco_02.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_rco_02 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_rco_02  = it_zsdt0252-seqlcto_rco_02.
          wa_saida_0120_02-docnum_rco_02   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_rco_02   = icon_execute_object.

          IF wa_saida_0120_02-docnum_rco_02 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_rco_02.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_rco_02 = t_active-nfnum9.
            ELSE.

              READ TABLE t_active_doc WITH KEY docnum = t_zfiwrt0008-docnum.
              IF ( sy-subrc EQ 0 ) AND ( t_active_doc-action_requ IS INITIAL ).
                wa_saida_0120_02-nfenum_rco_02 = icon_activity.
              ENDIF.

            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_rco_02 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    IF it_zsdt0252-seqlcto_ent_rco_02 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_ent_rco_02.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_ent_rco_02 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_ent_rco_02  = it_zsdt0252-seqlcto_ent_rco_02.
          wa_saida_0120_02-docnum_ent_rco_02   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_ent_rco_02   = icon_execute_object.

          IF wa_saida_0120_02-docnum_ent_rco_02 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_ent_rco_02.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_ent_rco_02 = t_active-nfnum9.
            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_ent_rco_02 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

*------------------------------------------------------------------------------------------------------*
*   NF Remessa Conta e Ordem 03 - Saida e Entrada
*------------------------------------------------------------------------------------------------------*

    IF it_zsdt0252-seqlcto_rco_03 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_rco_03.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_rco_03 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_rco_03  = it_zsdt0252-seqlcto_rco_03.
          wa_saida_0120_02-docnum_rco_03   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_rco_03   = icon_execute_object.

          IF wa_saida_0120_02-docnum_rco_03 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_rco_03.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_rco_03 = t_active-nfnum9.
            ELSE.

              READ TABLE t_active_doc WITH KEY docnum = t_zfiwrt0008-docnum.
              IF ( sy-subrc EQ 0 ) AND ( t_active_doc-action_requ IS INITIAL ).
                wa_saida_0120_02-nfenum_rco_03 = icon_activity.
              ENDIF.

            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_rco_03 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

    IF it_zsdt0252-seqlcto_ent_rco_03 IS NOT INITIAL.

      READ TABLE t_zfiwrt0008 WITH KEY seq_lcto = it_zsdt0252-seqlcto_ent_rco_03.
      IF ( sy-subrc EQ 0 ).

        IF ( t_zfiwrt0008-docs_estornados EQ abap_true ) OR ( t_zfiwrt0008-loekz EQ abap_true ).

          UPDATE zsdt0252 SET seqlcto_ent_rco_03 = space
           WHERE id_boletim EQ it_zsdt0252-id_boletim
             AND branch     EQ it_zsdt0252-branch
             AND charg      EQ it_zsdt0252-charg
             AND id_agrp    EQ it_zsdt0252-id_agrp.

        ELSE.

          wa_saida_0120_02-seqlcto_ent_rco_03  = it_zsdt0252-seqlcto_ent_rco_03.
          wa_saida_0120_02-docnum_ent_rco_03   = t_zfiwrt0008-docnum.
          wa_saida_0120_02-nfenum_ent_rco_03   = icon_execute_object.

          IF wa_saida_0120_02-docnum_ent_rco_03 IS NOT INITIAL.
            READ TABLE t_active WITH KEY docnum = wa_saida_0120_02-docnum_ent_rco_03.
            IF sy-subrc EQ 0.
              wa_saida_0120_02-nfenum_ent_rco_03 = t_active-nfnum9.
            ENDIF.
          ELSE.

            READ TABLE t_zfiwrt1000 WITH KEY seq_lcto = t_zfiwrt0008-seq_lcto
                                             field    = 'DOCNUM'.
            IF sy-subrc IS INITIAL.
              wa_saida_0120_02-nfenum_ent_rco_03 = icon_status_critical.
            ENDIF.

          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.


    APPEND wa_saida_0120_02 TO it_saida_0120_02.

  ENDLOOP.


ENDFORM.

FORM f_vincular_saldo_filial USING p_filial p_charg p_vinc_nf_emi_lim.

  RANGES: r_filial_filter FOR zsdt0249-branch,
          r_charg_filter  FOR zsdt0249-charg.

  DATA: t_notas      TYPE zsdt0249_t,
        t_notas_bloq TYPE zsdt0249_t.

  DATA: v_saldo_vincular TYPE zsdt0251-qtde_saldo,
        v_qtde_vinc_nf   TYPE zsdt0251-qtde_saldo.

  CHECK wg_cab_boletim_prod-id_boletim IS NOT INITIAL.

  CLEAR: t_notas[], r_filial_filter[], r_charg_filter[].

  IF p_filial IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_filial ) TO r_filial_filter.
  ENDIF.

  IF p_charg IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_charg ) TO r_charg_filter.
  ENDIF.

  PERFORM f_totaliza_saldos_vinc.

  v_saldo_vincular = wg_cab_boletim_prod-saldo_vincular.

*-CS2021000386 - 28.04.2021 - JT - inicio
  IF v_saldo_vincular <= 0.
    l_saldo_erro = abap_true.
*   MESSAGE 'Boletim de Produção sem saldo para vinculação!' TYPE 'I'.
    EXIT.
  ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

  TRY.

      IF p_vinc_nf_emi_lim EQ abap_true. "Vincular notas com data de emissão mais antiga...

        CLEAR: t_notas[].

        LOOP AT it_saida_0120_01 INTO wa_saida_0120_01.

          CHECK v_saldo_vincular > 0.

          LOOP AT wa_saida_0120_01-notas INTO DATA(wl_nf) WHERE nf_emissao_old EQ abap_true.

            IF v_saldo_vincular > wl_nf-qtde_saldo.
              v_qtde_vinc_nf = wl_nf-qtde_saldo.
            ELSE.
              v_qtde_vinc_nf = v_saldo_vincular.
            ENDIF.

            IF v_qtde_vinc_nf > 0.
              APPEND VALUE #( docnum = wl_nf-docnum qtde_vinc = v_qtde_vinc_nf ) TO t_notas.
              APPEND VALUE #( docnum = wl_nf-docnum qtde_vinc = v_qtde_vinc_nf ) TO t_notas_bloq.

              SUBTRACT v_qtde_vinc_nf FROM v_saldo_vincular.

              IF v_saldo_vincular <= 0.
                EXIT.
              ENDIF.
            ENDIF.

          ENDLOOP.

        ENDLOOP.

        IF t_notas[] IS NOT INITIAL.
          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->vincular_nf( i_notas = t_notas ).
          l_saldo_ok = abap_true.
        ENDIF.

      ELSE.

        LOOP AT it_saida_0120_01 INTO wa_saida_0120_01 WHERE branch IN r_filial_filter
                                                         AND charg  IN r_charg_filter.

          CLEAR: t_notas[].

          CHECK v_saldo_vincular > 0.

          LOOP AT wa_saida_0120_01-notas INTO wl_nf.

            IF v_saldo_vincular > wl_nf-qtde_saldo.
              v_qtde_vinc_nf = wl_nf-qtde_saldo.
            ELSE.
              v_qtde_vinc_nf = v_saldo_vincular.
            ENDIF.

            IF v_qtde_vinc_nf > 0.
              APPEND VALUE #( docnum = wl_nf-docnum qtde_vinc = v_qtde_vinc_nf ) TO t_notas.
              APPEND VALUE #( docnum = wl_nf-docnum qtde_vinc = v_qtde_vinc_nf ) TO t_notas_bloq.

              SUBTRACT v_qtde_vinc_nf FROM v_saldo_vincular.

              IF v_saldo_vincular <= 0.
                EXIT.
              ENDIF.
            ENDIF.

          ENDLOOP.

          IF t_notas[] IS NOT INITIAL.

            objeto->novo_registro(
                                  )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                 i_com_bloqueio = abap_true
                                 )->vincular_nf( i_notas = t_notas ).
            l_saldo_ok = abap_true.
          ENDIF.

        ENDLOOP.

      ENDIF.

    CATCH zcx_boletim_producao INTO DATA(zcx_boletim).
      ROLLBACK WORK.
      zcx_boletim->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

  objeto->desbloquear_registros( i_id_boletim =  wg_cab_boletim_prod-id_boletim ).

  LOOP AT t_notas_bloq INTO DATA(wl_nota_bloq).
    objeto->desbloquear_registros( i_docnum_zsdt0251 = wl_nota_bloq-docnum ).
  ENDLOOP.

  PERFORM f_totaliza_saldos_vinc.
  PERFORM f_load_notas.

ENDFORM.


FORM f_desvincular_saldo_filial USING p_filial p_charg p_id_agrp.

  RANGES: r_filial_filter  FOR zsdt0249-branch,
          r_charg_filter   FOR zsdt0249-charg,
          r_id_agrp_filter FOR zsdt0249-id_agrp.

  DATA: t_notas      TYPE zsdt0249_t,
        t_notas_bloq TYPE zsdt0249_t.

  CHECK wg_cab_boletim_prod-id_boletim IS NOT INITIAL.

  CLEAR: t_notas_bloq[], r_filial_filter[], r_charg_filter[], r_id_agrp_filter[].

  IF p_filial IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_filial ) TO r_filial_filter.
  ENDIF.

  IF p_charg IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_charg ) TO r_charg_filter.
  ENDIF.

  IF p_id_agrp IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_id_agrp ) TO r_id_agrp_filter.
  ENDIF.

  TRY.

      LOOP AT it_saida_0120_02 INTO wa_saida_0120_02 WHERE id_boletim  EQ wg_cab_boletim_prod-id_boletim
                                                       AND branch      IN r_filial_filter
                                                       AND charg       IN r_charg_filter
                                                       AND id_agrp     IN r_id_agrp_filter.

        CLEAR: t_notas[].

        SELECT *
          FROM zsdt0249 INTO TABLE t_notas
         WHERE id_boletim EQ wa_saida_0120_02-id_boletim
           AND branch     EQ wa_saida_0120_02-branch
           AND charg      EQ wa_saida_0120_02-charg
           AND id_agrp    EQ wa_saida_0120_02-id_agrp.

        LOOP AT t_notas INTO DATA(wl_nota).
          APPEND wl_nota TO t_notas_bloq.
        ENDLOOP.

        IF t_notas[] IS NOT INITIAL.

          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->desvincular_nf( i_notas = t_notas ).
        ENDIF.

      ENDLOOP.

    CATCH zcx_boletim_producao INTO DATA(zcx_boletim).
      ROLLBACK WORK.
      zcx_boletim->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

  objeto->desbloquear_registros( i_id_boletim =  wg_cab_boletim_prod-id_boletim ).

  LOOP AT t_notas_bloq INTO DATA(wl_nota_bloq).
    objeto->desbloquear_registros( i_docnum_zsdt0251 = wl_nota_bloq-docnum ).
  ENDLOOP.

  PERFORM f_totaliza_saldos_vinc.
  PERFORM f_load_notas.

  PERFORM f_leave_to_screen.

ENDFORM.

FORM f_atualizar_saldos.

  PERFORM f_load_notas.

ENDFORM.

FORM f_edit_boletim_producao .

  DATA: v_id_bol TYPE zsdt0246-id_boletim.

  v_id_bol = wg_cab_boletim_prod-id_boletim.

  CHECK v_id_bol IS NOT INITIAL.

  CLEAR: g_qtde_si_aux.
  FREE: it_zsdt0247_aux, it_zsdt0248_aux.
  it_zsdt0247_aux = it_zsdt0247.
  it_zsdt0248_aux = it_zsdt0248.
  g_qtde_si_aux = wg_cab_boletim_prod-qtde_si.

  TRY.
      objeto->check_permissao_modificacao( i_id_boletim = v_id_bol ).

      PERFORM f_set_operacao_boletim USING c_edit_bol.

      PERFORM f_load_boletim USING v_id_bol.

      LEAVE TO SCREEN 0100.

    CATCH zcx_boletim_producao INTO DATA(zcx_bol).
      zcx_bol->published_erro( EXPORTING i_msgty = 'I' i_msgty_display =  'W' ).
  ENDTRY.

ENDFORM.

FORM f_refresh_alv USING p_screen.

  CASE p_screen.
    WHEN '0120_01'.

      CHECK obj_alv_0120_01 IS NOT INITIAL.

      CALL METHOD obj_alv_0120_01->refresh_table_display
        EXPORTING
          is_stable = wa_stable.

    WHEN '0120_02'.

      CHECK obj_alv_0120_02 IS NOT INITIAL.

      CALL METHOD obj_alv_0120_02->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
  ENDCASE.

ENDFORM.

FORM f_totaliza_saldos_vinc.

  objeto->get_saldos( EXPORTING i_id_boletim     = wg_cab_boletim_prod-id_boletim
                      IMPORTING e_saldo_vincular = wg_cab_boletim_prod-saldo_vincular ).

ENDFORM.

FORM f_vincular_saldo.

  DATA: v_saldo_vincular TYPE zsdt0251-qtde_saldo.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0120_01->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS NOT INITIAL.

    CLEAR: l_saldo_ok, l_saldo_erro.

    LOOP AT it_sel_rows INTO wa_sel_rows.

      READ TABLE it_saida_0120_01 INTO wa_saida_0120_01 INDEX wa_sel_rows-index.
      CHECK sy-subrc = 0.

      PERFORM f_vincular_saldo_filial USING wa_saida_0120_01-branch wa_saida_0120_01-charg abap_false.

    ENDLOOP.

*-CS2021000386 - 28.04.2021 - JT - inicio
    IF l_saldo_erro = abap_true AND l_saldo_ok = abap_false.
      MESSAGE 'Boletim de Produção sem saldo para vinculação!' TYPE 'I'.
    ENDIF.
*-CS2021000386 - 28.04.2021 - JT - fim

  ELSE.
*-CS2021000386 - 28.04.2021 - JT - inicio
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
*
*    "Vinculação de Notas Automaticas
*    PERFORM f_vincular_saldo_filial USING '' '' abap_true.  "Vincular notas mais antigas por limite de dias pré definido
*
*    "Verificar Saldo
*    PERFORM f_totaliza_saldos_vinc.
*    v_saldo_vincular = wg_cab_boletim_prod-saldo_vincular.
*    CHECK v_saldo_vincular > 0. "Continuar se possuir saldo.
*
*    PERFORM f_vincular_saldo_filial USING '' '' abap_false. "Vincular notas por filial
*-CS2021000386 - 28.04.2021 - JT - fim
  ENDIF.

  PERFORM f_leave_to_screen.

ENDFORM.

FORM f_desvincular_saldo.

  PERFORM f_desvincular_saldo_filial USING '' '' ''.

ENDFORM.

FORM f_cancel_edicao_bol.

  DATA: v_id_boletim TYPE zsdt0246-id_boletim.

  v_id_boletim = wg_cab_boletim_prod-id_boletim.

  PERFORM f_set_operacao_boletim USING c_view_bol.

  IF v_id_boletim IS NOT INITIAL.
    PERFORM f_load_boletim USING v_id_boletim.
  ELSE.
    PERFORM f_limpa_dados_screen.
  ENDIF.

  LEAVE TO SCREEN 0100.

ENDFORM.

FORM f_leave_to_screen.

  DATA(v_min) = abap_false.
  DATA(v_max) = abap_false.

  DATA: v_screen TYPE c LENGTH 4.
  CLEAR: v_screen.

  LOOP AT SCREEN.

    IF screen-name = 'BTN_MIN_DETAIL_BOL'.
      v_screen = '0100'.
    ENDIF.

    IF screen-name = 'BTN_MAX_DETAIL_BOL'.
      v_screen = '0120'.
    ENDIF.

  ENDLOOP.

  LEAVE TO SCREEN v_screen.


ENDFORM.

FORM f_preencher_dynpro USING l_start TYPE c l_name TYPE c l_value.

  MOVE l_start TO wl_bdc-dynbegin.
  IF l_start = 'X'.
    MOVE:
  l_name  TO wl_bdc-program,
  l_value TO wl_bdc-dynpro.
  ELSE.
    MOVE:
      l_name  TO wl_bdc-fnam,
      l_value TO wl_bdc-fval.
  ENDIF.
  APPEND wl_bdc TO tl_bdc.
  CLEAR: wl_bdc.

ENDFORM.

FORM f_aprovar_boletim_producao.

  DATA: v_id_bol TYPE zsdt0246-id_boletim.

  v_id_bol = wg_cab_boletim_prod-id_boletim.

  CHECK v_id_bol IS NOT INITIAL.

  TRY.
      objeto->aprovar_lancamento( i_id_boletim_producao = v_id_bol ).

      PERFORM f_load_boletim USING v_id_bol.

      LEAVE TO SCREEN 0100.

    CATCH zcx_boletim_producao INTO DATA(zcx_bol).
      zcx_bol->published_erro( EXPORTING i_msgty = 'I' i_msgty_display =  'W' ).
  ENDTRY.

ENDFORM.

FORM f_call_znfe_nf_devol.

  DATA: it_docnum_filter TYPE ty_j_1bnfdoc.

  CLEAR: it_docnum_filter[].

  CALL METHOD obj_alv_0120_02->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX wa_sel_rows-index.
    CHECK ( sy-subrc EQ 0 ) AND ( wa_saida_0120_02-docnum_devol IS NOT INITIAL ).

    APPEND VALUE #( docnum = wa_saida_0120_02-docnum_devol ) TO it_docnum_filter[].
  ENDLOOP.

  CHECK it_docnum_filter[] IS NOT INITIAL.

  EXPORT it_docnum_filter FROM it_docnum_filter TO MEMORY ID 'IT_DOCNUM_FILTER'.
  SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida_0120_02-bukrs.

  CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

ENDFORM.


FORM f_call_znfe_nf_ind.

  DATA: it_docnum_filter TYPE ty_j_1bnfdoc.

  CLEAR: it_docnum_filter[].

  CALL METHOD obj_alv_0120_02->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX wa_sel_rows-index.
    CHECK ( sy-subrc EQ 0 ) AND ( wa_saida_0120_02-docnum_ind IS NOT INITIAL ).

    APPEND VALUE #( docnum = wa_saida_0120_02-docnum_ind ) TO it_docnum_filter[].
  ENDLOOP.

  CHECK it_docnum_filter[] IS NOT INITIAL.

  EXPORT it_docnum_filter FROM it_docnum_filter TO MEMORY ID 'IT_DOCNUM_FILTER'.
  SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida_0120_02-bukrs.

  CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

ENDFORM.


FORM f_call_znfe_nf_rfl.

  DATA: it_docnum_filter TYPE ty_j_1bnfdoc.

  CLEAR: it_docnum_filter[].

  CALL METHOD obj_alv_0120_02->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX wa_sel_rows-index.
    CHECK ( sy-subrc EQ 0 ).

    IF ( wa_saida_0120_02-docnum_rfl_01 IS NOT INITIAL ).
      APPEND VALUE #( docnum = wa_saida_0120_02-docnum_rfl_01 ) TO it_docnum_filter[].
    ENDIF.

    IF ( wa_saida_0120_02-docnum_rfl_02 IS NOT INITIAL ).
      APPEND VALUE #( docnum = wa_saida_0120_02-docnum_rfl_02 ) TO it_docnum_filter[].
    ENDIF.

    IF ( wa_saida_0120_02-docnum_rfl_03 IS NOT INITIAL ).
      APPEND VALUE #( docnum = wa_saida_0120_02-docnum_rfl_03 ) TO it_docnum_filter[].
    ENDIF.

  ENDLOOP.

  CHECK it_docnum_filter[] IS NOT INITIAL.

  EXPORT it_docnum_filter FROM it_docnum_filter TO MEMORY ID 'IT_DOCNUM_FILTER'.
  SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida_0120_02-bukrs.

  CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

ENDFORM.

FORM f_call_znfe_nf_rco.

  DATA: it_docnum_filter TYPE ty_j_1bnfdoc.

  CLEAR: it_docnum_filter[].

  CALL METHOD obj_alv_0120_02->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX wa_sel_rows-index.
    CHECK ( sy-subrc EQ 0 ).

    IF ( wa_saida_0120_02-docnum_rco_01 IS NOT INITIAL ).
      APPEND VALUE #( docnum = wa_saida_0120_02-docnum_rco_01 ) TO it_docnum_filter[].
    ENDIF.

    IF ( wa_saida_0120_02-docnum_rco_02 IS NOT INITIAL ).
      APPEND VALUE #( docnum = wa_saida_0120_02-docnum_rco_02 ) TO it_docnum_filter[].
    ENDIF.

    IF ( wa_saida_0120_02-docnum_rco_03 IS NOT INITIAL ).
      APPEND VALUE #( docnum = wa_saida_0120_02-docnum_rco_03 ) TO it_docnum_filter[].
    ENDIF.

  ENDLOOP.

  CHECK it_docnum_filter[] IS NOT INITIAL.

  EXPORT it_docnum_filter FROM it_docnum_filter TO MEMORY ID 'IT_DOCNUM_FILTER'.
  SET PARAMETER ID 'Z_MY_PARAMETER_2' FIELD wa_saida_0120_02-bukrs.

  CALL TRANSACTION 'ZNFE' AND SKIP FIRST SCREEN.

ENDFORM.

FORM f_gerar_documentos.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0120_02->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.



  IF it_sel_rows[] IS NOT INITIAL.

    LOOP AT it_sel_rows INTO wa_sel_rows.

      READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX wa_sel_rows-index.

      CHECK sy-subrc EQ 0.

      PERFORM f_gerar_documentos_filial USING wa_saida_0120_02-branch wa_saida_0120_02-charg wa_saida_0120_02-id_agrp.

    ENDLOOP.

  ELSE.
    PERFORM f_gerar_documentos_filial USING '' '' ''.
  ENDIF.


ENDFORM.

FORM f_gerar_documentos_filial USING p_filial p_charg p_id_agrp.

  RANGES: r_filial_filter  FOR zsdt0249-branch,
          r_charg_filter   FOR zsdt0249-charg,
          r_id_agrp_filter FOR zsdt0249-id_agrp.

  DATA: wl_zsdt0252 TYPE zsdt0252.

  CHECK wg_cab_boletim_prod-id_boletim IS NOT INITIAL.

  CLEAR: r_filial_filter[], r_charg_filter[], r_id_agrp_filter[].

  IF p_filial IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_filial ) TO r_filial_filter.
  ENDIF.

  IF p_charg IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_charg ) TO r_charg_filter.
  ENDIF.

  IF p_id_agrp IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_id_agrp ) TO r_id_agrp_filter.
  ENDIF.

*------------------------Inicio verifica produto terceiro / CS20230000708 / AOENNING
  "Check produto terceiro.
  LOOP AT it_saida_0100_01 ASSIGNING FIELD-SYMBOL(<ws_saida_0100_01>).
    CASE <ws_saida_0100_01>-tp_produto_producao.
      	WHEN 'OI'.
        IF <ws_saida_0100_01>-check_mat_terceiro EQ abap_true.
          objeto->at_oi_mat_terceiro = abap_true.
        ENDIF.
      	WHEN 'SC'.
      WHEN 'AF'.
      	WHEN OTHERS.
    ENDCASE.
  ENDLOOP.
*------------------------Fim verifica produto terceiro / CS20230000708 / AOENNING

  TRY.

      LOOP AT it_saida_0120_02 ASSIGNING FIELD-SYMBOL(<fs_saida_0120_02>) WHERE id_boletim  EQ wg_cab_boletim_prod-id_boletim
                                                                            AND branch      IN r_filial_filter
                                                                            AND charg       IN r_charg_filter
                                                                            AND id_agrp     IN r_id_agrp_filter.

        CLEAR: wl_zsdt0252.

        SELECT SINGLE *
          FROM zsdt0252 INTO wl_zsdt0252
         WHERE id_boletim EQ <fs_saida_0120_02>-id_boletim
           AND branch     EQ <fs_saida_0120_02>-branch
           AND charg      EQ <fs_saida_0120_02>-charg
           AND id_agrp    EQ <fs_saida_0120_02>-id_agrp.

        CHECK sy-subrc EQ 0.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF de Devolução - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wg_cab_boletim_prod-com_nf EQ abap_true ).

          IF ( wl_zsdt0252-seqlcto_devol IS INITIAL ).

            objeto->novo_registro(
                                  )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                  i_com_bloqueio = abap_true
                                  )->gerar_nf_devolucao( EXPORTING i_zsdt0252      = wl_zsdt0252
                                  IMPORTING e_seq_lcto_znfw = <fs_saida_0120_02>-seqlcto_devol ).
            <fs_saida_0120_02>-nfenum_devol  = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_devol.

            CONTINUE.

          ENDIF.

          IF ( <fs_saida_0120_02>-seqlcto_devol   IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-docnum_devol    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_devol    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_devol(1) EQ '@'           ).
            PERFORM f_enviar_sefaz USING <fs_saida_0120_02>-seqlcto_devol.
            CONTINUE.
          ENDIF.

          IF ( wl_zsdt0252-seqlcto_ent_dev        IS INITIAL             ) AND
             ( <fs_saida_0120_02>-docnum_devol    IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_devol    IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_devol(1) NE '@'                 ).

            objeto->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                 i_com_bloqueio = abap_true
                                 )->gerar_nf_ent_devolucao( EXPORTING i_zsdt0252      = wl_zsdt0252
                                 IMPORTING e_seq_lcto_znfw = <fs_saida_0120_02>-seqlcto_ent_dev ).
            <fs_saida_0120_02>-nfenum_ent_dev = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_ent_dev.

            CONTINUE.

          ENDIF.

        ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF de Industrialização - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wg_cab_boletim_prod-com_nf EQ abap_true ).

          IF ( wl_zsdt0252-seqlcto_ind              IS INITIAL             ) AND
             ( <fs_saida_0120_02>-docnum_ent_dev    IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_ent_dev    IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_ent_dev(1) NE '@'  ).

            objeto->novo_registro(
                                 )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                 i_com_bloqueio = abap_true
                                 )->gerar_nf_industrializacao( EXPORTING i_zsdt0252      = wl_zsdt0252
                                 IMPORTING e_seq_lcto_znfw = <fs_saida_0120_02>-seqlcto_ind ).
            <fs_saida_0120_02>-nfenum_ind  = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_ind.

            CONTINUE.

          ENDIF.

          IF ( <fs_saida_0120_02>-seqlcto_ind   IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-docnum_ind    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_ind    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_ind(1) EQ '@'           ).
            PERFORM f_enviar_sefaz USING <fs_saida_0120_02>-seqlcto_ind.
            CONTINUE.
          ENDIF.

          IF ( wl_zsdt0252-seqlcto_ent_ind      IS INITIAL             ) AND
             ( <fs_saida_0120_02>-docnum_ind    IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_ind    IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_ind(1) NE '@'  ).

            objeto->novo_registro(
                                 )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                 i_com_bloqueio = abap_true
                                 )->gerar_nf_ent_industrializacao( EXPORTING i_zsdt0252      = wl_zsdt0252
                                 IMPORTING e_seq_lcto_znfw = <fs_saida_0120_02>-seqlcto_ent_ind ).
            <fs_saida_0120_02>-nfenum_ent_ind = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_ent_ind.

            CONTINUE.

          ENDIF.

        ENDIF.
*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     Documentos de Produção
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wg_cab_boletim_prod-com_nf  EQ abap_true ).

          CHECK ( wl_zsdt0252-seqlcto_ent_ind          IS NOT INITIAL         ) AND
                ( <fs_saida_0120_02>-docnum_ent_ind    IS NOT INITIAL         ) AND
                ( <fs_saida_0120_02>-nfenum_ent_ind    IS NOT INITIAL         ) AND
                ( <fs_saida_0120_02>-nfenum_ent_ind(1) NE '@'  ).

        ENDIF.


        CHECK wl_zsdt0252-id_agrp EQ 1.

        "Boletim produção 01
        IF wl_zsdt0252-doc_prod_01 IS INITIAL.

          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim i_com_bloqueio = abap_true
                               )->gerar_doc_producao_01( EXPORTING i_zsdt0252 = wl_zsdt0252
                               IMPORTING e_mblnr    = <fs_saida_0120_02>-doc_prod_01 ).
          WAIT UP TO 2 SECONDS.
        ENDIF.

        "Boletim produção 02
        IF wl_zsdt0252-doc_prod_02 IS INITIAL.

          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->gerar_doc_producao_02( EXPORTING i_zsdt0252 = wl_zsdt0252
                               IMPORTING e_mblnr    = <fs_saida_0120_02>-doc_prod_02 ).
          WAIT UP TO 2 SECONDS.
        ENDIF.

        "Boletim produção 03
        IF ( wl_zsdt0252-doc_prod_03 IS INITIAL ).

          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->gerar_doc_producao_03( EXPORTING i_zsdt0252 = wl_zsdt0252
                               IMPORTING e_mblnr    = <fs_saida_0120_02>-doc_prod_03 ).
          WAIT UP TO 2 SECONDS.
        ENDIF.

        "Boletim produção 04
        IF ( wl_zsdt0252-doc_prod_04 IS INITIAL ).
          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->gerar_doc_producao_04( EXPORTING i_zsdt0252 = wl_zsdt0252
                               IMPORTING e_mblnr    = <fs_saida_0120_02>-doc_prod_04 ).
          WAIT UP TO 2 SECONDS.
        ENDIF.

        "Boletim produção 05
        IF ( wl_zsdt0252-doc_prod_05 IS INITIAL ).
          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->gerar_doc_producao_05( EXPORTING i_zsdt0252 = wl_zsdt0252
                               IMPORTING e_mblnr    = <fs_saida_0120_02>-doc_prod_05 ).
          WAIT UP TO 2 SECONDS.
        ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Remessa Formação de Lote - 01 , 02 , 03
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wg_cab_boletim_prod-com_nf EQ abap_true ).

          IF ( wl_zsdt0252-qtde_cp_doc_05 IS NOT INITIAL ).

            CHECK wl_zsdt0252-doc_prod_05 IS NOT INITIAL.

          ELSEIF ( wl_zsdt0252-qtde_fc_doc_04 IS NOT INITIAL ).

            CHECK wl_zsdt0252-doc_prod_04 IS NOT INITIAL.

          ELSEIF ( wl_zsdt0252-qtde_fh_doc_03 IS NOT INITIAL ).

            CHECK wl_zsdt0252-doc_prod_03 IS NOT INITIAL.

          ELSE.

            CHECK wl_zsdt0252-doc_prod_02 IS NOT INITIAL.

          ENDIF.


          IF ( wl_zsdt0252-seqlcto_rfl_01 IS INITIAL ) AND ( wl_zsdt0252-qtde_od_me IS NOT INITIAL ).
            "( wg_cab_boletim_prod-od_dest_mi = abap_false ).

            objeto->novo_registro(
                                 )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                 i_com_bloqueio = abap_true
                                 )->gerar_nf_rfl_01( EXPORTING i_zsdt0252        = wl_zsdt0252
                                 IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_rfl_01 ).
            <fs_saida_0120_02>-nfenum_rfl_01  = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_rfl_01.

          ENDIF.

          IF ( <fs_saida_0120_02>-seqlcto_rfl_01   IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-docnum_rfl_01    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_rfl_01    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_rfl_01(1) EQ '@'           ).
            PERFORM f_enviar_sefaz USING <fs_saida_0120_02>-seqlcto_rfl_01.
          ENDIF.

          IF ( wl_zsdt0252-seqlcto_rfl_02 IS INITIAL        ) AND ( wl_zsdt0252-qtde_fh_me IS NOT INITIAL ).
*             ( wl_zsdt0252-qtde_fh_doc_03 IS NOT INITIAL    ) AND
*             ( wg_cab_boletim_prod-fh_dest_mi EQ abap_false ).

            objeto->novo_registro(
                                 )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                 i_com_bloqueio = abap_true
                                 )->gerar_nf_rfl_02( EXPORTING i_zsdt0252        = wl_zsdt0252
                                 IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_rfl_02 ).
            <fs_saida_0120_02>-nfenum_rfl_02  = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_rfl_02.
          ENDIF.

          IF ( <fs_saida_0120_02>-seqlcto_rfl_02   IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-docnum_rfl_02    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_rfl_02    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_rfl_02(1) EQ '@'           ).
            PERFORM f_enviar_sefaz USING <fs_saida_0120_02>-seqlcto_rfl_02.
          ENDIF.

          IF ( wl_zsdt0252-seqlcto_rfl_03 IS INITIAL        ) AND ( wl_zsdt0252-qtde_fc_me IS NOT INITIAL ).
*             ( wl_zsdt0252-qtde_fc_doc_04 IS NOT INITIAL    ) AND
*             ( wg_cab_boletim_prod-fc_dest_mi EQ abap_false ).

            objeto->novo_registro(
                                 )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                 i_com_bloqueio = abap_true
                                 )->gerar_nf_rfl_03( EXPORTING i_zsdt0252        = wl_zsdt0252
                                 IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_rfl_03 ).
            <fs_saida_0120_02>-nfenum_rfl_03  = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_rfl_03.
          ENDIF.

          IF ( <fs_saida_0120_02>-seqlcto_rfl_03   IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-docnum_rfl_03    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_rfl_03    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_rfl_03(1) EQ '@'           ).
            PERFORM f_enviar_sefaz USING <fs_saida_0120_02>-seqlcto_rfl_03.
          ENDIF.

          IF ( wl_zsdt0252-qtde_od_me > 0 ).
            CHECK ( <fs_saida_0120_02>-nfenum_rfl_01 IS NOT INITIAL ).
          ENDIF.

          IF ( wl_zsdt0252-qtde_fh_doc_03 IS NOT INITIAL ) AND ( wl_zsdt0252-qtde_fh_me > 0 ). "( wg_cab_boletim_prod-fh_dest_mi EQ abap_false ).
            CHECK ( <fs_saida_0120_02>-nfenum_rfl_02 IS NOT INITIAL ).
          ENDIF.

          IF ( wl_zsdt0252-qtde_fc_doc_04 IS NOT INITIAL ) AND ( wl_zsdt0252-qtde_fc_me > 0 )."( wg_cab_boletim_prod-fc_dest_mi EQ abap_false ).
            CHECK ( <fs_saida_0120_02>-nfenum_rfl_03 IS NOT INITIAL ).
          ENDIF.

        ENDIF. "IF ( wg_cab_boletim_prod-com_nf EQ abap_true ).

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Remessa Conta e Ordem 01 - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wg_cab_boletim_prod-com_nf       EQ abap_true           ).

          IF ( wl_zsdt0252-seqlcto_rco_01       IS INITIAL             ) AND
             ( wl_zsdt0252-seqlcto_rfl_01       IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-docnum_rfl_01 IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_rfl_01 IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_rfl_01(1) NE '@'  ).


            objeto->novo_registro(
                                )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                i_com_bloqueio = abap_true
                                )->gerar_nf_rco_01( EXPORTING i_zsdt0252        = wl_zsdt0252
                                IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_rco_01 ).
            <fs_saida_0120_02>-nfenum_rco_01  = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_rco_01.

          ENDIF.

          IF ( <fs_saida_0120_02>-seqlcto_rco_01   IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-docnum_rco_01    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_rco_01    IS NOT INITIAL   ) AND
             ( <fs_saida_0120_02>-nfenum_rco_01(1) EQ '@'           ).
            PERFORM f_enviar_sefaz USING <fs_saida_0120_02>-seqlcto_rco_01.
          ENDIF.

          IF ( wl_zsdt0252-seqlcto_ent_rco_01   IS INITIAL             ) AND
             ( wl_zsdt0252-seqlcto_rco_01       IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-docnum_rco_01 IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_rco_01 IS NOT INITIAL         ) AND
             ( <fs_saida_0120_02>-nfenum_rco_01(1) NE '@'  ).

            objeto->novo_registro(
                                 )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                 i_com_bloqueio = abap_true
                                 )->gerar_nf_ent_rco_01( EXPORTING i_zsdt0252        = wl_zsdt0252
                                 IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_ent_rco_01 ).
            <fs_saida_0120_02>-nfenum_ent_rco_01  = icon_execute_object.

            CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
              EXPORTING
                i_seq_lcto = <fs_saida_0120_02>-seqlcto_ent_rco_01.

          ENDIF.

        ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Remessa Conta e Ordem 02 - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wg_cab_boletim_prod-com_nf          EQ abap_true           ) AND
           ( wl_zsdt0252-seqlcto_rco_02          IS INITIAL             ) AND
           ( wl_zsdt0252-seqlcto_rfl_02          IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-docnum_rfl_02    IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-nfenum_rfl_02    IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-nfenum_rfl_02(1) NE '@'  ).


          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->gerar_nf_rco_02( EXPORTING i_zsdt0252        = wl_zsdt0252
                               IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_rco_02 ).
          <fs_saida_0120_02>-nfenum_rco_02  = icon_execute_object.

          CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
            EXPORTING
              i_seq_lcto = <fs_saida_0120_02>-seqlcto_rco_02.

        ENDIF.

        IF ( wg_cab_boletim_prod-com_nf          EQ abap_true     ) AND
           ( <fs_saida_0120_02>-seqlcto_rco_02   IS NOT INITIAL   ) AND
           ( <fs_saida_0120_02>-docnum_rco_02    IS NOT INITIAL   ) AND
           ( <fs_saida_0120_02>-nfenum_rco_02    IS NOT INITIAL   ) AND
           ( <fs_saida_0120_02>-nfenum_rco_02(1) EQ '@'           ).
          PERFORM f_enviar_sefaz USING <fs_saida_0120_02>-seqlcto_rco_02.
        ENDIF.

        IF ( wg_cab_boletim_prod-com_nf       EQ abap_true           ) AND
           ( wl_zsdt0252-seqlcto_ent_rco_02   IS INITIAL             ) AND
           ( wl_zsdt0252-seqlcto_rco_02       IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-docnum_rco_02 IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-nfenum_rco_02 IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-nfenum_rco_02(1) NE '@'  ).

          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->gerar_nf_ent_rco_02( EXPORTING i_zsdt0252        = wl_zsdt0252
                               IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_ent_rco_02 ).
          <fs_saida_0120_02>-nfenum_ent_rco_02  = icon_execute_object.

          CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
            EXPORTING
              i_seq_lcto = <fs_saida_0120_02>-seqlcto_ent_rco_02.

        ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*       NF Remessa Conta e Ordem 03 - Saida e Entrada
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wg_cab_boletim_prod-com_nf       EQ abap_true           ) AND
           ( wl_zsdt0252-seqlcto_rco_03       IS INITIAL             ) AND
           ( wl_zsdt0252-seqlcto_rfl_03       IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-docnum_rfl_03 IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-nfenum_rfl_03 IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-nfenum_rfl_03(1) NE '@'  ).


          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->gerar_nf_rco_03( EXPORTING i_zsdt0252        = wl_zsdt0252
                               IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_rco_03 ).
          <fs_saida_0120_02>-nfenum_rco_03  = icon_execute_object.

          CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
            EXPORTING
              i_seq_lcto = <fs_saida_0120_02>-seqlcto_rco_03.

        ENDIF.

        IF ( wg_cab_boletim_prod-com_nf          EQ abap_true     ) AND
           ( <fs_saida_0120_02>-seqlcto_rco_03   IS NOT INITIAL   ) AND
           ( <fs_saida_0120_02>-docnum_rco_03    IS NOT INITIAL   ) AND
           ( <fs_saida_0120_02>-nfenum_rco_03    IS NOT INITIAL   ) AND
           ( <fs_saida_0120_02>-nfenum_rco_03(1) EQ '@'           ).
          PERFORM f_enviar_sefaz USING <fs_saida_0120_02>-seqlcto_rco_03.
        ENDIF.

        IF ( wg_cab_boletim_prod-com_nf       EQ abap_true           ) AND
           ( wl_zsdt0252-seqlcto_ent_rco_03   IS INITIAL             ) AND
           ( wl_zsdt0252-seqlcto_rco_03       IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-docnum_rco_03 IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-nfenum_rco_03 IS NOT INITIAL         ) AND
           ( <fs_saida_0120_02>-nfenum_rco_03(1) NE '@'  ).

          objeto->novo_registro(
                               )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                               i_com_bloqueio = abap_true
                               )->gerar_nf_ent_rco_03( EXPORTING i_zsdt0252        = wl_zsdt0252
                               IMPORTING e_seq_lcto_znfw   = <fs_saida_0120_02>-seqlcto_ent_rco_03 ).
          <fs_saida_0120_02>-nfenum_ent_rco_03  = icon_execute_object.

          CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
            EXPORTING
              i_seq_lcto = <fs_saida_0120_02>-seqlcto_ent_rco_03.

        ENDIF.

      ENDLOOP.

    CATCH zcx_boletim_producao INTO DATA(zcx_boletim).
      zcx_boletim->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_nf_writer  INTO DATA(zcx_nf_writer).
      zcx_nf_writer->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_preco INTO DATA(zcx_preco).
      zcx_preco->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

  DATA(_change_status_bol) = abap_false.
  DATA(_status_bol) = objeto->check_status_boletim( EXPORTING i_id_boletim_producao = wg_cab_boletim_prod-id_boletim
                                                    IMPORTING e_change_status       = _change_status_bol ).

  IF _status_bol EQ 'F'. "Finalizado
    PERFORM f_disable_time.
  ENDIF.

  PERFORM: f_load_notas_vinc,
           f_refresh_alv USING '0120_02'.

  IF _change_status_bol EQ abap_true.
    PERFORM f_leave_to_screen.
  ENDIF.


ENDFORM.

FORM f_estornar_documentos.

  DATA: v_answer                TYPE c,
        v_estorno_all_documents TYPE c.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0120_02->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS NOT INITIAL.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        titlebar       = 'Confirmação'
        text_question  = 'Quais documentos deseja estornar?'
        text_button_1  = 'Ultimo(s)'
        text_button_2  = 'Todos'
        default_button = '1'
        "DISPLAY_CANCEL_BUTTON = ''
      IMPORTING
        answer         = v_answer
      EXCEPTIONS
        text_not_found = 1
        OTHERS         = 2.

    CASE v_answer.
      WHEN '1'.
        v_estorno_all_documents = abap_false.
      WHEN '2'.
        v_estorno_all_documents = abap_true.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    CLEAR: sdate.
*     CALL SCREEN 0122 STARTING AT 05 05.

    LOOP AT it_sel_rows INTO wa_sel_rows.

      READ TABLE it_saida_0120_02 INTO wa_saida_0120_02 INDEX wa_sel_rows-index.
      CHECK ( sy-subrc = 0 ).

      PERFORM f_estornar_documentos_filial USING wa_saida_0120_02-branch wa_saida_0120_02-charg wa_saida_0120_02-id_agrp v_estorno_all_documents.

    ENDLOOP.

  ELSE.
    MESSAGE 'Selecione pelo menos uma linha!' TYPE 'S'.
  ENDIF.


ENDFORM.


FORM f_estornar_documentos_filial USING p_filial p_charg p_id_agrp p_estorno_all_documents.

  RANGES: r_filial_filter  FOR zsdt0249-branch,
          r_charg_filter   FOR zsdt0249-charg,
          r_id_agrp_filter FOR zsdt0249-id_agrp.

  DATA: wl_zsdt0252         TYPE zsdt0252,
        wl_zsdt0252_agrp_01 TYPE zsdt0252.

  CHECK wg_cab_boletim_prod-id_boletim IS NOT INITIAL.

  CLEAR: r_filial_filter[], r_charg_filter[], r_id_agrp_filter[].

  IF p_filial IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_filial ) TO r_filial_filter.
  ENDIF.

  IF p_charg IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_charg ) TO r_charg_filter.
  ENDIF.

  IF p_id_agrp IS NOT INITIAL.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = p_id_agrp ) TO r_id_agrp_filter.
  ENDIF.

  TRY.

      LOOP AT it_saida_0120_02 ASSIGNING FIELD-SYMBOL(<fs_saida_0120_02>) WHERE id_boletim  EQ wg_cab_boletim_prod-id_boletim
                                                                            AND branch      IN r_filial_filter
                                                                            AND charg       IN r_charg_filter
                                                                            AND id_agrp     IN r_id_agrp_filter.

        CLEAR: wl_zsdt0252, wl_zsdt0252_agrp_01.

        SELECT SINGLE *
          FROM zsdt0252 INTO wl_zsdt0252
         WHERE id_boletim EQ <fs_saida_0120_02>-id_boletim
           AND branch     EQ <fs_saida_0120_02>-branch
           AND charg      EQ <fs_saida_0120_02>-charg
           AND id_agrp    EQ <fs_saida_0120_02>-id_agrp.

        CHECK sy-subrc EQ 0.

        SELECT SINGLE *
          FROM zsdt0252 INTO wl_zsdt0252_agrp_01
         WHERE id_boletim EQ <fs_saida_0120_02>-id_boletim
           AND branch     EQ <fs_saida_0120_02>-branch
           AND charg      EQ <fs_saida_0120_02>-charg
           AND id_agrp    EQ 1.

        CHECK sy-subrc EQ 0.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Conta e Ordem 03 - Entrada e Saida
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wl_zsdt0252_agrp_01-seqlcto_ent_rco_03 IS NOT INITIAL ).

          DATA(_doc_znfw_estornado) = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_ent_rco_03
                                                                                       i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_ent_rco_03, <fs_saida_0120_02>-seqlcto_ent_rco_03.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252_agrp_01-seqlcto_ent_rco_03   IS INITIAL     ) AND
           ( wl_zsdt0252_agrp_01-seqlcto_rco_03       IS NOT INITIAL ) AND
           ( <fs_saida_0120_02>-nfenum_rco_03(1)      EQ '@'         ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_rco_03
                                                                                 i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_rco_03, <fs_saida_0120_02>-seqlcto_rco_03.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Entrada Remessa Conta e Ordem 02 - Entrada e Saida
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wl_zsdt0252_agrp_01-seqlcto_ent_rco_02 IS NOT INITIAL ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_ent_rco_02
                                                                                 i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_ent_rco_02, <fs_saida_0120_02>-seqlcto_ent_rco_02.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252_agrp_01-seqlcto_ent_rco_02   IS INITIAL     ) AND
           ( wl_zsdt0252_agrp_01-seqlcto_rco_02       IS NOT INITIAL ) AND
           ( <fs_saida_0120_02>-nfenum_rco_02(1)      EQ '@'     ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_rco_02
                                                                                 i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_rco_02, <fs_saida_0120_02>-seqlcto_rco_02.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Entrada Remessa Conta e Ordem 01 - Entrada e Saida
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wl_zsdt0252_agrp_01-seqlcto_ent_rco_01 IS NOT INITIAL ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_ent_rco_01
                                                                                 i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_ent_rco_01, <fs_saida_0120_02>-seqlcto_ent_rco_01.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252_agrp_01-seqlcto_ent_rco_01   IS INITIAL     ) AND
           ( wl_zsdt0252_agrp_01-seqlcto_rco_01       IS NOT INITIAL ) AND
           ( <fs_saida_0120_02>-nfenum_rco_01(1)      EQ '@'         ).


          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_rco_01
                                                                                 i_wait_estorno = abap_true ).
          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_rco_01, <fs_saida_0120_02>-seqlcto_rco_01.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        CHECK ( wl_zsdt0252_agrp_01-seqlcto_rco_01 IS INITIAL ) AND
              ( wl_zsdt0252_agrp_01-seqlcto_rco_02 IS INITIAL ) AND
              ( wl_zsdt0252_agrp_01-seqlcto_rco_03 IS INITIAL ).

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Remessa Formação de Lote - 03 , 02 , 01
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wl_zsdt0252_agrp_01-seqlcto_rfl_03       IS NOT INITIAL ) AND
           ( <fs_saida_0120_02>-nfenum_rfl_03(1)      EQ '@' ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_rfl_03
                                                                                 i_wait_estorno = abap_true ).
          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_rfl_03, <fs_saida_0120_02>-seqlcto_rfl_03.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252_agrp_01-seqlcto_rfl_02       IS NOT INITIAL ) AND
           ( <fs_saida_0120_02>-nfenum_rfl_02(1)      EQ '@'    ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_rfl_02
                                                                                 i_wait_estorno = abap_true ).
          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_rfl_02, <fs_saida_0120_02>-seqlcto_rfl_02.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252_agrp_01-seqlcto_rfl_01       IS NOT INITIAL ) AND
           ( <fs_saida_0120_02>-nfenum_rfl_01(1)      EQ '@'     ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252_agrp_01-seqlcto_rfl_01
                                                                                 i_wait_estorno = abap_true ).
          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-seqlcto_rfl_01, <fs_saida_0120_02>-seqlcto_rfl_01.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        CHECK ( wl_zsdt0252_agrp_01-seqlcto_rfl_01 IS INITIAL ) AND
              ( wl_zsdt0252_agrp_01-seqlcto_rfl_02 IS INITIAL ) AND
              ( wl_zsdt0252_agrp_01-seqlcto_rfl_03 IS INITIAL ).

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     Documentos de Produção
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wl_zsdt0252_agrp_01-doc_prod_05 IS NOT INITIAL ).
          DATA(_mblnr_estorno) = objeto->novo_registro(
                                                      )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                      i_com_bloqueio = abap_true
                                                      )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_05 ).
          IF _mblnr_estorno IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-doc_prod_05, <fs_saida_0120_02>-doc_prod_05.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ELSE.
            "Se houver erro referente a data de processamento, abrir uma opção para inserir a data de referencia para estorno do documento.
            CALL SCREEN 0122 STARTING AT 05 05.

            _mblnr_estorno = objeto->novo_registro(
                                                  )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                  i_com_bloqueio = abap_true
                                                  )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_05 ).
            IF _mblnr_estorno IS NOT INITIAL.
              CLEAR: wl_zsdt0252_agrp_01-doc_prod_05, <fs_saida_0120_02>-doc_prod_05.
              MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
              COMMIT WORK.
            ENDIF.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252_agrp_01-doc_prod_04 IS NOT INITIAL ).
          _mblnr_estorno = objeto->novo_registro(
                                                )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                i_com_bloqueio = abap_true
                                                )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_04 ).
          IF _mblnr_estorno IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-doc_prod_04, <fs_saida_0120_02>-doc_prod_04.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ELSE.
            "Se houver erro referente a data de processamento, abrir uma opção para inserir a data de referencia para estorno do documento.
            CALL SCREEN 0122 STARTING AT 05 05.

            _mblnr_estorno = objeto->novo_registro(
                                                  )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                  i_com_bloqueio = abap_true
                                                  )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_04 ).
            IF _mblnr_estorno IS NOT INITIAL.
              CLEAR: wl_zsdt0252_agrp_01-doc_prod_04, <fs_saida_0120_02>-doc_prod_04.
              MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
              COMMIT WORK.
            ENDIF.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252_agrp_01-doc_prod_03 IS NOT INITIAL ).
          _mblnr_estorno = objeto->novo_registro(
                                                )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                i_com_bloqueio = abap_true
                                                )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_03 ).
          IF _mblnr_estorno IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-doc_prod_03, <fs_saida_0120_02>-doc_prod_03.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ELSE.
            "Se houver erro referente a data de processamento, abrir uma opção para inserir a data de referencia para estorno do documento.
            CALL SCREEN 0122 STARTING AT 05 05.

            _mblnr_estorno = objeto->novo_registro(
                                                  )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                  i_com_bloqueio = abap_true
                                                  )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_03 ).
            IF _mblnr_estorno IS NOT INITIAL.
              CLEAR: wl_zsdt0252_agrp_01-doc_prod_03, <fs_saida_0120_02>-doc_prod_03.
              MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
              COMMIT WORK.
            ENDIF.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.
        ENDIF.

        IF ( wl_zsdt0252_agrp_01-doc_prod_02 IS NOT INITIAL ).
          _mblnr_estorno = objeto->novo_registro(
                                                )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                i_com_bloqueio = abap_true
                                                )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_02 ).
          IF _mblnr_estorno IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-doc_prod_02, <fs_saida_0120_02>-doc_prod_02.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ELSE.
            "Se houver erro referente a data de processamento, abrir uma opção para inserir a data de referencia para estorno do documento.
            CALL SCREEN 0122 STARTING AT 05 05.
            _mblnr_estorno = objeto->novo_registro(
                                                  )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                  i_com_bloqueio = abap_true
                                                  )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_02 ).
            IF _mblnr_estorno IS NOT INITIAL.
              CLEAR: wl_zsdt0252_agrp_01-doc_prod_02, <fs_saida_0120_02>-doc_prod_02.
              MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
              COMMIT WORK.
            ENDIF.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252_agrp_01-doc_prod_01 IS NOT INITIAL ).
          _mblnr_estorno = objeto->novo_registro(
                                                )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                i_com_bloqueio = abap_true
                                                )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_01 ).
          IF _mblnr_estorno IS NOT INITIAL.
            CLEAR: wl_zsdt0252_agrp_01-doc_prod_01, <fs_saida_0120_02>-doc_prod_01.
            MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
            COMMIT WORK.
          ELSE.
            "Se houver erro referente a data de processamento, abrir uma opção para inserir a data de referencia para estorno do documento.
            CALL SCREEN 0122 STARTING AT 05 05.
            _mblnr_estorno = objeto->novo_registro(
                                                  )->set_registro( i_id_boletim   = wg_cab_boletim_prod-id_boletim
                                                  i_com_bloqueio = abap_true
                                                  )->estorno_doc_producao( i_mblnr = wl_zsdt0252_agrp_01-doc_prod_01 ).
            IF _mblnr_estorno IS NOT INITIAL.
              CLEAR: wl_zsdt0252_agrp_01-doc_prod_01, <fs_saida_0120_02>-doc_prod_01.
              MODIFY zsdt0252 FROM wl_zsdt0252_agrp_01.
              COMMIT WORK.
            ENDIF.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        CHECK ( wl_zsdt0252_agrp_01-doc_prod_01  IS INITIAL ) AND
              ( wl_zsdt0252_agrp_01-doc_prod_02  IS INITIAL ) AND
              ( wl_zsdt0252_agrp_01-doc_prod_03  IS INITIAL ) AND
              ( wl_zsdt0252_agrp_01-doc_prod_04  IS INITIAL ) AND
              ( wl_zsdt0252_agrp_01-doc_prod_05  IS INITIAL ).

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Industrialização - Entrada e Saida
*------------------------------------------------------------------------------------------------------------------------------------------------------------*

        IF ( wl_zsdt0252-seqlcto_ent_ind IS NOT INITIAL ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252-seqlcto_ent_ind
                                                                                 i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252-seqlcto_ent_ind, <fs_saida_0120_02>-seqlcto_ent_ind.
            MODIFY zsdt0252 FROM wl_zsdt0252.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252-seqlcto_ent_ind      IS INITIAL     ) AND
           ( wl_zsdt0252-seqlcto_ind          IS NOT INITIAL ) AND
           ( <fs_saida_0120_02>-nfenum_ind(1) EQ '@'    ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252-seqlcto_ind
                                                                                 i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252-seqlcto_ind, <fs_saida_0120_02>-seqlcto_ind.
            MODIFY zsdt0252 FROM wl_zsdt0252.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

*------------------------------------------------------------------------------------------------------------------------------------------------------------*
*     NF Devolução - Entrada e Saida
*------------------------------------------------------------------------------------------------------------------------------------------------------------*
        CHECK ( wl_zsdt0252-seqlcto_ind     IS INITIAL ) AND
              ( wl_zsdt0252-seqlcto_ent_ind IS INITIAL ).

        IF ( wl_zsdt0252-seqlcto_ent_dev IS NOT INITIAL ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252-seqlcto_ent_dev
                                                                                 i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252-seqlcto_ent_dev, <fs_saida_0120_02>-seqlcto_ent_dev.
            MODIFY zsdt0252 FROM wl_zsdt0252.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

        IF ( wl_zsdt0252-seqlcto_ent_dev        IS INITIAL     ) AND
           ( wl_zsdt0252-seqlcto_devol          IS NOT INITIAL ) AND
           ( <fs_saida_0120_02>-nfenum_devol(1) EQ '@'     ).

          _doc_znfw_estornado = zcl_nf_writer=>zif_nf_writer~estornar_documento( i_seq_lcto     = wl_zsdt0252-seqlcto_devol
                                                                                 i_wait_estorno = abap_true ).

          IF _doc_znfw_estornado IS NOT INITIAL.
            CLEAR: wl_zsdt0252-seqlcto_devol, <fs_saida_0120_02>-seqlcto_devol.
            MODIFY zsdt0252 FROM wl_zsdt0252.
            COMMIT WORK.
          ENDIF.

          CHECK p_estorno_all_documents EQ abap_true.

        ENDIF.

      ENDLOOP.

    CATCH zcx_boletim_producao INTO DATA(zcx_boletim).
      zcx_boletim->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
    CATCH zcx_nf_writer INTO DATA(zcx_nf_writer).
      zcx_nf_writer->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'W' ).
  ENDTRY.

  objeto->desbloquear_registros( i_id_boletim =  wg_cab_boletim_prod-id_boletim ).

  DATA(_change_status_bol) = abap_false.
  DATA(_status_bol) = objeto->check_status_boletim( EXPORTING i_id_boletim_producao = wg_cab_boletim_prod-id_boletim
                                                    IMPORTING e_change_status       = _change_status_bol ).

  PERFORM: f_load_notas_vinc,
           f_refresh_alv USING '0120_02'.

  IF _change_status_bol EQ abap_true.
    PERFORM f_leave_to_screen.
  ENDIF.



ENDFORM.

FORM f_enable_time.

  DATA: v_status TYPE c.

  v_status = abap_true.

  EXPORT v_status TO MEMORY ID 'ZSDT0170_TIME_ACTIVE'.

  IF go_clock IS INITIAL.
    CREATE OBJECT: go_clock.
  ENDIF.

  IF go_alarm IS INITIAL.
    CREATE OBJECT: go_alarm.
    SET HANDLER go_alarm->on_finished FOR go_clock.
  ENDIF.

  go_clock->interval = c_time_interval.
  CALL METHOD go_clock->run.

  PERFORM: f_gerar_documentos,
           f_load_notas_vinc,
           f_refresh_alv USING '0120_02',
           f_leave_to_screen.

ENDFORM.

FORM f_disable_time.

  DATA: v_status TYPE c.

  v_status = abap_false.

  EXPORT v_status TO MEMORY ID 'ZSDT0170_TIME_ACTIVE'.

ENDFORM.

FORM f_get_status_time CHANGING r_status.

  DATA: v_status TYPE c.

  IMPORT v_status FROM MEMORY ID 'ZSDT0170_TIME_ACTIVE'.

  r_status = v_status.

ENDFORM.

FORM f_enviar_sefaz USING p_seq_lcto.

  DATA: v_docnum TYPE j_1bnfdoc-docnum.

  SELECT SINGLE *
    FROM zfiwrt0008 INTO @DATA(wl_zfiwrt0008)
   WHERE seq_lcto EQ @p_seq_lcto.

  CHECK ( sy-subrc EQ 0 ) AND ( wl_zfiwrt0008-docnum IS NOT INITIAL ).

  SELECT SINGLE *
    FROM j_1bnfe_active INTO @DATA(wl_active_doc)
   WHERE docnum EQ @wl_zfiwrt0008-docnum.

  CHECK ( sy-subrc EQ 0 ) AND ( wl_active_doc-action_requ IS NOT INITIAL ).

  CHECK ( sy-subrc EQ 0 ) AND ( wl_active_doc-docsta NE '1' ).

  v_docnum = wl_active_doc-docnum.

  TRY.

      "Verificar Número Não Determinado

      zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = CONV #( v_docnum )
        )->set_registro(
             EXPORTING
               i_docnum = CONV #( v_docnum )
               i_sem_bloqueio = abap_true
        )->get_ck_determinar_numero(
        )->set_det_numero(
        )->get_registro(
             IMPORTING
               e_documento           = DATA(wl_doc)
               e_info_doc_eletronico = DATA(wl_active)
        ).

    CATCH zcx_doc_eletronico INTO DATA(ex_doc_eletronico).

      TRY.

          zcl_nfe=>zif_doc_eletronico~get_instance( i_docnum = CONV #( v_docnum )
            )->set_registro(
                 EXPORTING
                   i_docnum = CONV #( v_docnum )
                   i_sem_bloqueio = abap_true
            )->set_autorizar(
            )->get_registro(
                 IMPORTING
                   e_documento           = wl_doc
                   e_info_doc_eletronico = wl_active
            ).

        CATCH zcx_doc_eletronico.
        CATCH cx_root.
      ENDTRY.

    CATCH cx_root.
  ENDTRY.


ENDFORM.

FORM f_set_titlebar.

  IF wg_cab_boletim_prod-id_boletim IS INITIAL.
    SET TITLEBAR 'T0100' WITH g_desc_titulo.
  ELSE.

    TRY.
        objeto->get_status_boletim( EXPORTING i_id_boletim_producao = wg_cab_boletim_prod-id_boletim
                                    IMPORTING e_ds_status           = DATA(_ds_status)  ).

        CONCATENATE '-' _ds_status INTO _ds_status SEPARATED BY space.

      CATCH zcx_boletim_producao.
    ENDTRY.

    CASE wg_cab_boletim_prod-aprovado.
      WHEN abap_true.
        SET TITLEBAR 'T0100_APROV'  WITH g_desc_titulo _ds_status.
      WHEN abap_false.
        SET TITLEBAR 'T0100_NAPROV' WITH g_desc_titulo _ds_status.
    ENDCASE.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_MOSTRAR_CPOS_MERCADO_INTERNO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_mostrar_cpos_mercado_interno  USING p_screen.

  DATA: w_celltab LIKE LINE OF t_celltab.

  DATA: v_edit TYPE c.

  CASE vg_operacao_bol.
    WHEN c_new_bol OR c_edit_bol .
      v_edit = abap_true.
    WHEN OTHERS.
      v_edit = abap_false.
  ENDCASE.

*----------------------------------------------------------------------*
* Verifica se é o grid de Dados de Rendimento
*----------------------------------------------------------------------*
  IF p_screen = '0100_02'.

*----------------------------------------------------------------------*
* Para filial cadastra para NF mostra os campos de Mercado Interno
*----------------------------------------------------------------------*
    IF wg_cab_boletim_prod-branch IS NOT INITIAL.

      AUTHORITY-CHECK OBJECT 'M_MATE_WRK' ID 'WERKS' FIELD  wg_cab_boletim_prod-branch
                                          ID 'ACTVT' FIELD '03'.

      IF sy-subrc NE 0.
        MESSAGE |Sem acesso a filial { wg_cab_boletim_prod-branch }! | TYPE 'I'.
        CLEAR wg_cab_boletim_prod-branch.
      ELSE.
        SELECT SINGLE * FROM zsdt0253 INTO @DATA(wl_merc) WHERE branch EQ @wg_cab_boletim_prod-branch.
        IF sy-subrc EQ 0.
          CASE tp_boletim.
            WHEN '01'. "Boletim produção farelo/oleo
              IF wl_merc-boletim_f IS INITIAL.
                MESSAGE |Filial { wg_cab_boletim_prod-branch } sem parâmetros gerais cadastrados (ZSDT0169)! | TYPE 'I'.
                CLEAR wg_cab_boletim_prod-branch.
              ENDIF.
            WHEN '02'. "Boletim produção biodiesel
              IF wl_merc-boletim_b IS INITIAL.
                MESSAGE |Filial { wg_cab_boletim_prod-branch } sem parâmetros gerais cadastrados (ZSDT0169)! | TYPE 'I'.
                CLEAR wg_cab_boletim_prod-branch.
              ENDIF.
            WHEN '03'.  "Boletim produção oleo neutro
              IF wl_merc-boletim_o IS INITIAL.
                MESSAGE |Filial { wg_cab_boletim_prod-branch } sem parâmetros gerais cadastrados (ZSDT0169)! | TYPE 'I'.
                CLEAR wg_cab_boletim_prod-branch.
              ENDIF.
            WHEN OTHERS.
          ENDCASE.
        ELSE.
          IF wl_merc-boletim_o IS INITIAL.
            MESSAGE |Filial { wg_cab_boletim_prod-branch } sem parâmetros gerais cadastrados (ZSDT0169)! | TYPE 'I'.
            CLEAR wg_cab_boletim_prod-branch.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDIF.

*----------------------------------------------------------------------*
* Consiste de a Filial está habilitada para gerar nota fiscal
*----------------------------------------------------------------------*
* validar cadastro.
    DATA(vl_nf) = abap_true.
    IF wl_merc-emissao_nf IS NOT  INITIAL.
      vl_nf = abap_false.
    ELSE.
*----------------------------------------------------------------------*
*  Se a filial não está habilita para nota fiscal
* Move o valor do campo QTE para QTDE_ME e limpa QTDE_MI
*----------------------------------------------------------------------*
      LOOP AT it_saida_0100_02 ASSIGNING FIELD-SYMBOL(<fs_saida_0100_02>).
        <fs_saida_0100_02>-qtde_me = <fs_saida_0100_02>-qtde.
        CLEAR: <fs_saida_0100_02>-qtde_mi.
      ENDLOOP.

    ENDIF.

*----------------------------------------------------------------------*
* Atualiza tabela de campos do ALV
*----------------------------------------------------------------------*
    IF obj_alv_0100_02 IS NOT INITIAL.

      CALL METHOD obj_alv_0100_02->get_frontend_fieldcatalog
        IMPORTING
          et_fieldcatalog = it_fcat[].

    ENDIF.


    REFRESH: t_celltab.
    READ TABLE it_fcat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
                                          WITH KEY fieldname = 'QTDE'.
    IF sy-subrc IS INITIAL.

      w_celltab-fieldname = <fs_fcat>-fieldname.
      w_celltab-style     = cl_gui_alv_grid=>mc_style_enabled.
      INSERT w_celltab INTO TABLE t_celltab.
      CLEAR w_celltab.

    ENDIF.

    READ TABLE it_fcat ASSIGNING <fs_fcat> WITH KEY fieldname = 'QTDE_ME'.
    IF sy-subrc IS INITIAL.
      <fs_fcat>-no_out = vl_nf.
    ENDIF.

    READ TABLE it_fcat ASSIGNING <fs_fcat> WITH KEY fieldname = 'QTDE_MI'.
    IF sy-subrc IS INITIAL.
      <fs_fcat>-no_out = vl_nf.

      w_celltab-fieldname = <fs_fcat>-fieldname.
      w_celltab-style     = cl_gui_alv_grid=>mc_style_enabled.
      INSERT w_celltab INTO TABLE t_celltab.
      CLEAR w_celltab.

    ENDIF.

*----------------------------------------------------------------------*
* Atualiza tabela de campos do ALV
*----------------------------------------------------------------------*
    IF obj_alv_0100_02 IS NOT INITIAL.

      CALL METHOD obj_alv_0100_02->set_frontend_fieldcatalog
        EXPORTING
          it_fieldcatalog = it_fcat[].

    ENDIF.

    LOOP AT it_saida_0100_02 ASSIGNING FIELD-SYMBOL(<fs_0100_02>).

      REFRESH: <fs_0100_02>-celltab[].

      IF v_edit IS NOT INITIAL.
        CASE <fs_0100_02>-tp_produto_producao.
          WHEN 'FC'."Farelo Comum
            INSERT LINES OF t_celltab[] INTO TABLE <fs_0100_02>-celltab[].
          WHEN 'FH'."Farelo Hipro
            INSERT LINES OF t_celltab[] INTO TABLE <fs_0100_02>-celltab[].
          WHEN 'OD'."Óleo Degomado
            INSERT LINES OF t_celltab[] INTO TABLE <fs_0100_02>-celltab[].
          WHEN OTHERS.
            INSERT LINES OF t_celltab2[] INTO TABLE <fs_0100_02>-celltab[].
        ENDCASE.
      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0122  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0122 OUTPUT.
  SET PF-STATUS 'PF0122'.
  SET TITLEBAR 'T0122'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0122  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0122 INPUT.
  CASE sy-ucomm .
    WHEN 'SAVE'.
      IF sdate IS INITIAL.
        MESSAGE 'Informe a data de referencia' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        objeto->set_date_processamento( i_date =  sdate ).
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.

    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.

    WHEN 'EXEC'.
      IF sdate IS INITIAL.
        MESSAGE 'Informe a data de referencia' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        objeto->set_date_processamento( i_date =  sdate ).
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANC'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  STATUS_0123  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0123 OUTPUT.
  IF check_auth_b_farelo IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GR1'. "Farelo.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF check_auth_b_oleo_netro IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GR2'. "Farelo.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  IF check_auth_b_biodiesel IS INITIAL.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'GR3'. "Farelo.
        screen-input = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDIF.

  SET PF-STATUS 'PF0123'.
  SET TITLEBAR 'T0123'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0123  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0123 INPUT.
  CASE sy-ucomm.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'OK'.
      PERFORM fm_set_tp_boletim.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Form  FM_CHECK_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_check_user .
  DATA: qtd1    TYPE char3 VALUE '0'.

  CLEAR: check_auth_b_farelo, tp_boletim.
  FREE: r_parid, _param.
  "check perfil user.
* // pega os parametros do usuario
  CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
    EXPORTING
      user_name           = sy-uname
    TABLES
      user_parameters     = _param
    EXCEPTIONS
      user_name_not_exist = 1
      OTHERS              = 2.
*
*  DELETE _param WHERE parid NE 'ZCANC_EXTEMP_INT'.

  "Check parametro ZCANC_EXTEMP_INT existente no perfil usuario.
  IF _param[] IS NOT INITIAL.
    LOOP AT _param ASSIGNING FIELD-SYMBOL(<ws_param>).
      IF <ws_param>-parid = 'ZSDT0170_F' OR <ws_param>-parid = 'ZSDT0170_B' OR <ws_param>-parid = 'ZSDT0170_O'.
        APPEND VALUE #( option = 'I' sign = 'EQ' low = <ws_param>-parid ) TO r_parid.
      ENDIF.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE r_parid LINES qtd1.
  IF qtd1 > 1. "Verifica se usuario tem autorização para mais de um processo de produção.

    LOOP AT r_parid INTO DATA(ws_parid).
      CASE ws_parid-low.
        WHEN 'ZSDT0170_B'.
          check_auth_b_biodiesel = abap_true.
        WHEN 'ZSDT0170_O'.
          check_auth_b_oleo_netro = abap_true.
        WHEN 'ZSDT0170_F'.
          check_auth_b_farelo = abap_true.
        WHEN OTHERS.
      ENDCASE.
    ENDLOOP.

    CALL  SCREEN 0123 STARTING AT 8 8.
  ELSE.
    CLEAR: ws_parid.
    READ TABLE r_parid INTO ws_parid INDEX 1.
    CASE ws_parid-low.
      WHEN 'ZSDT0170_F'.
        tp_boletim = '01'.
      WHEN 'ZSDT0170_B'.
        tp_boletim = '02'.
      WHEN 'ZSDT0170_O'.
        tp_boletim = '03'.
      WHEN OTHERS.
    ENDCASE.

    IF tp_boletim IS INITIAL.
      MESSAGE |{ sy-uname } você não tem acesso a nenhum tipo de boletim, entre em contato com Key User responsável! | TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      PERFORM fm_call_scrren_0100.
    ENDIF.


  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SET_TP_BOLETIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_set_tp_boletim .
  CLEAR tp_boletim.
  IF b_farelo EQ abap_true.
    tp_boletim = '01'.
  ENDIF.

  IF b_biodiesel EQ abap_true.
    tp_boletim = '02'.
  ENDIF.

  IF b_oleo_neutro EQ abap_true.
    tp_boletim = '03'.
  ENDIF.

  PERFORM fm_call_scrren_0100.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  FM_CALL_SCRREN_0100
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_call_scrren_0100 .
  CLEAR: g_desc_titulo, qtde_ent_soja_ind.
* "Instanciar classe.
  FREE: objeto.
  CASE tp_boletim.
    WHEN '02'.
      CREATE OBJECT objeto TYPE ('ZCL_BOLETIM_PRODUCAO_02').
      g_desc_titulo = 'Biodiesel '.
      qtde_ent_soja_ind = 'Qtde Óleo Neutro'.
    WHEN '03'.
      CREATE OBJECT objeto TYPE ('ZCL_BOLETIM_PRODUCAO_03').
      g_desc_titulo = 'Óleo Neutro '.
      qtde_ent_soja_ind = 'Qtde Óleo Degomado'.
    WHEN OTHERS.
      CREATE OBJECT objeto TYPE ('ZCL_BOLETIM_PRODUCAO_01').
      g_desc_titulo = 'Farelo/Óleo '.
      qtde_ent_soja_ind = 'Qtde Soja'.
  ENDCASE.

  CALL  SCREEN 0100.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_SEL_BOLETIM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_sel_boletim .
  DATA: v_id_boletim TYPE zsdt0246-id_boletim.

  v_id_boletim = wg_cab_boletim_prod-id_boletim.

  PERFORM f_set_operacao_boletim USING c_view_bol.

*  IF v_id_boletim IS NOT INITIAL.
*    PERFORM f_load_boletim USING v_id_boletim.
*  ELSE.
  PERFORM f_limpa_dados_screen.
*  ENDIF.

  LEAVE TO SCREEN 0.
  PERFORM fm_check_user.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CALL_SCREEN_0124
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_call_screen_adc .
  CASE tp_boletim.
    WHEN '02'. "Caso tenha informações adicionais para boletim Biodiesel, criar tela com os campos adicionais.
      CALL  SCREEN 0125 STARTING AT 8 8.
    WHEN '03'. "Caso tenha informações adicionais para boletim Óleo Neutro, criar tela com os campos adicionais.
      CALL  SCREEN 0126 STARTING AT 8 8.
    WHEN OTHERS.
      CALL  SCREEN 0124 STARTING AT 8 8.
  ENDCASE.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DESAPROVAR_BOLETIM_PRODUCAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_desaprovar_boletim_producao .
  DATA: v_id_bol TYPE zsdt0246-id_boletim.

  v_id_bol = wg_cab_boletim_prod-id_boletim.

  CHECK v_id_bol IS NOT INITIAL.

  TRY.
      objeto->desaprovar_lancamento( i_id_boletim_producao = v_id_bol ).

      PERFORM f_load_boletim USING v_id_bol.

      LEAVE TO SCREEN 0100.

    CATCH zcx_boletim_producao INTO DATA(zcx_bol).
      zcx_bol->published_erro( EXPORTING i_msgty = 'I' i_msgty_display =  'W' ).
  ENDTRY.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  FM_CHECK_ERRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM fm_check_erro .

  "Check quantidade.

  IF tp_boletim NE '01'.

    LOOP AT it_saida_0100_01 ASSIGNING FIELD-SYMBOL(<fs_saida_0100_01>).
      IF <fs_saida_0100_01>-qtde_consumo IS INITIAL.

        "Tipo de processo.
        CASE tp_boletim.
          WHEN '02'.

            "Biodiesel.
            CASE sy-tabix.
              WHEN 1.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade Oleo Neutro'|.
                APPEND wa_msg_return TO it_msg_return.
              WHEN 2.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade metanol'|.
                APPEND wa_msg_return TO it_msg_return.
              WHEN 3.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade metilato de sodio'|.
                APPEND wa_msg_return TO it_msg_return.
              WHEN 4.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade acido citrico'|.
                APPEND wa_msg_return TO it_msg_return.
              WHEN 5.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade soda caustica'|.
                APPEND wa_msg_return TO it_msg_return.
              WHEN 6.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade cloridico'|.
                APPEND wa_msg_return TO it_msg_return.

            ENDCASE.

          WHEN '03'.
            "Óleo degomado

            CASE sy-tabix.
              WHEN 1.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade óleo degomado'|.
                APPEND wa_msg_return TO it_msg_return.
              WHEN 2.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade soda caústica'|.
                APPEND wa_msg_return TO it_msg_return.
              WHEN 3.
                CLEAR wa_msg_return.
                wa_msg_return-msg = |'Preencha a quantidade acido fosforico'|.
                APPEND wa_msg_return TO it_msg_return.
            ENDCASE.
        ENDCASE.

      ENDIF.
    ENDLOOP.

    CALL FUNCTION 'Z_DOC_CHECK_NEW'
      EXPORTING
        i_screen   = '100'
        i_show     = ''
        i_repid    = sy-repid
      IMPORTING
        e_messagem = wa_mensagem
      TABLES
        it_msgs    = it_msg_return.

  ENDIF.

ENDFORM.

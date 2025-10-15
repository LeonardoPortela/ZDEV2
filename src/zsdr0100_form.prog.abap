*&-------------------------------------------------------------------------------------------------------*
*& Método         : ZSDR0100_FORM                                                                        *
*& Chamado        : USER STORY 169312                                                                    *
*& Data           : 27/03/2025                                                                           *
*& Especificado   : Samuel Cabana                                                                     *
*& Desenvolvimento: Nilton Marcelo Segantin                                                              *
*--------------------------------------------------------------------------------------------------------*
*& Histórico de Alterações:                                                                              *
*--------------------------------------------------------------------------------------------------------*
*&  Data     |Request    | Autor         | Alteração                                                     *
*&-------------------------------------------------------------------------------------------------------*
*&-------------------------------------------------------------------------------------------------------*
*& 27/03/2025|DEVK9A1XAW |NSEGATIN       | Ajsute nos valores dos campos Qtde.NF (KG), Saldo Exportar e  *
*&                                       |Qtde. NFe. Exportação. BUG 172407.        *
*--------------------------------------------------------------------------------------------------------*
*& 16/04/2025|DEVK9A2IKE |NSEGATIN       | Ajsute nos valores dos campos Qtde Vinc., Saldo Exportar,     *
*&                                       |Saldo a Exportar RFL e Qtde.Comercial(XML). BUG 174186.        *
*--------------------------------------------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&  Include           ZLESR0115_FORM
*&---------------------------------------------------------------------*

FORM f_refresh_alv USING p_alv.

  CASE p_alv.
    WHEN '0100'.
      CHECK obj_alv_0100 IS NOT INITIAL.

      CALL METHOD obj_alv_0100->refresh_table_display
        EXPORTING
          is_stable = wa_stable.
    WHEN '0110'.

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

      lv_pos_col = 0. "// WBARBOSA 19112024 US-158263

      PERFORM f_estrutura_alv USING:
         ''                     ''                   'IT_SAIDA_0100'  'EUDR'                    'EUDR'                      '05'   ' '    ''  ' ' ' ' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263 INCLUSÃO DE COLUNA
         'J_1BNFDOC'            'BUKRS'              'IT_SAIDA_0100'  'BUKRS'                   'Empresa'                   '07'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
         'J_1BNFDOC'            'BRANCH'             'IT_SAIDA_0100'  'BRANCH'                  'Centro'                    '06'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
         'J_1BNFDOC'            'DOCNUM'             'IT_SAIDA_0100'  'DOCNUM'                  'Docnum'                    '10'   ' '    ''  ' ' ' ' 'X' ' ' 'X' ,
         'J_1BNFLIN'            'ITMNUM'             'IT_SAIDA_0100'  'ITMNUM'                  'Item'                      '06'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
         'J_1BNFDOC'            'NFENUM'             'IT_SAIDA_0100'  'NFENUM'                  'Nro.NF'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
         'J_1BNFDOC'            'SERIES'             'IT_SAIDA_0100'  'SERIES'                  'Série'                     '05'   ' '    ''  ' ' ' ' ' ' ' ' 'X' ,
         'J_1BNFDOC'            'MODEL'              'IT_SAIDA_0100'  'MODEL'                   'Modelo'                    '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         'J_1BNFDOC'            'CREDAT'             'IT_SAIDA_0100'  'CREDAT'                  'Dt.Criação'                '10'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         'J_1BNFDOC'            'DOCDAT'             'IT_SAIDA_0100'  'DOCDAT'                  'Dt.Emissao'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFDOC'            'PARID'              'IT_SAIDA_0100'  'PARID'                   'Parceiro'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'NAME1'              'IT_SAIDA_0100'  'NAME1'                   'Nome'                      '35'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'REGIO'              'IT_SAIDA_0100'  'REGIO'                   'Estado'                    '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MATNR'              'IT_SAIDA_0100'  'MATNR'                   'Material'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'MAKT'                 'MAKTX'              'IT_SAIDA_0100'  'MAKTX'                   'Ds.Material'               '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'CFOP'               'IT_SAIDA_0100'  'CFOP'                    'CFOP'                      '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'NBM'                'IT_SAIDA_0100'  'NBM'                     'NCM'                       '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZIB_NFE_DIST_ITM'     'CHAVE_NFE'          'IT_SAIDA_0100'  'CHAVE_NFE'               'Chv.NFe'                   '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZIB_NFE_DIST_ITM'     'PROD_UND_TRIB'      'IT_SAIDA_0100'  'UND_TRIB_XML'            'Un.Trib.XML'               '11'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         'ZIB_NFE_DIST_ITM'     'PROD_NCM'           'IT_SAIDA_0100'  'NCM_XML'                 'NCM.XML'                   '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZIB_NFE_DIST_ITM'     'PROD_CFOP'          'IT_SAIDA_0100'  'CFOP_XML'                'CFOP XML'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         ''                     ''                   'IT_SAIDA_0100'  'ENT_PROP'                'Ent.Própria'               '11'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         ''                     ''                   'IT_SAIDA_0100'  'ROM_COMPLETO'            'Rom.Completo'              '12'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         'J_1BNFLIN'            'MEINS'              'IT_SAIDA_0100'  'MEINS'                   'Unid.'                     '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'MENGE'                   'Qtde.NF (KG)'              '13'   ' '    'X' ' ' ' ' ' ' ' ' '' ,
         'ZIB_NFE_DIST_ITM'     'PROD_QTD_COMERCI'   'IT_SAIDA_0100'  'QCOM_XML'                'Qtde.Comercial(XML)'       '13'   ' '    'X' ' ' ' ' ' ' ' ' '' ,
         'J_1BNFDOC'            'DOCDAT'             'IT_SAIDA_0100'  'DT_RECEPCAO_CCT'         'Dt.Recepcao CCT'           '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'PESO_CCT'                'Peso CCT'                  '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'PESO_AFERIDO_RECEPCAO'   'Peso Aferido CCT'          '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'PESO_FISCAL_CCT'         'Peso Fiscal  CCT'          '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'LIFNR'              'IT_SAIDA_0100'  'TERM_CCT'                'Term.CCT'                  '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'NAME1'              'IT_SAIDA_0100'  'DS_TERM_CCT'             'Ds.Term.CCT'               '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'DIF_PESO_CCT_NF'         'Dif.Peso.NFxCCT'           '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         ''                     ''                   'IT_SAIDA_0100'  'CONF_CCT_PORTAL'         'Conf.CCT Portal'           '15'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         'ZLEST0186'            'DT_RECEPCAO'        'IT_SAIDA_0100'  'DT_RECEPCAO_PORTAL'      'Dt.CCT Portal'             '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'LIFNR'              'IT_SAIDA_0100'  'TERM_CCT_PORTAL'         'Term.CCT Portal'           '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'NAME1'              'IT_SAIDA_0100'  'DS_TERM_CCT_PORTAL'      'Ds.Term.CCT Portal'        '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFDOC'            'DOCNUM'             'IT_SAIDA_0100'  'DOCNUM_RFL'              'Docnum.RFL'                '10'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
         'ZSDTVINC_P_FLOTE'     'ID_VINC'            'IT_SAIDA_0100'  'ID_VINC'                 'Id Vinc'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263
         'ZIB_NFE_DIST_ITM'     'CHAVE_NFE'          'IT_SAIDA_0100'  'CHAVE_NFE_RFL'           'Chv.NFe.RFL'               '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFDOC'            'NFENUM'             'IT_SAIDA_0100'  'NFENUM_RFL'              'Nr.NF.RFL'                 '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'QTD_NF_RFL'              'Qtde.NF.FLote'             '13'   ' '    'X' ' ' ' ' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263 INCLUSÃO DE COLUNA
         'LFA1'                 'LIFNR'              'IT_SAIDA_0100'  'TERMINAL_RFL'            'Terminal RFL'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'NAME1'              'IT_SAIDA_0100'  'DS_TERMINAL_RFL'         'Ds.Terminal RFL'           '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         ''                     ''                   'IT_SAIDA_0100'  'CCT_RFL'                 'CCT RFL'                   '07'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         'J_1BNFDOC'            'DOCDAT'             'IT_SAIDA_0100'  'DT_CCT_RFL'              'Dt.CCT RFL'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'PESO_CCT_RFL'            'Peso CCT RFL'              '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'LIFNR'              'IT_SAIDA_0100'  'TERM_CCT_RFL'            'Term.CCT RFL'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'NAME1'              'IT_SAIDA_0100'  'DS_TERM_CCT_RFL'         'Ds.Term.CCT RFL'           '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         ''                     ''                   'IT_SAIDA_0100'  'CONF_CCT_PORTAL_RFL'     'Conf.CCT Portal RFL'       '19'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
         'ZLEST0186'            'DT_RECEPCAO'        'IT_SAIDA_0100'  'DT_RECEPCAO_PORTAL_RFL'  'Dt.CCT Portal RFL'         '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'LIFNR'              'IT_SAIDA_0100'  'TERM_CCT_PORTAL_RFL'     'Term.CCT Portal RFL'       '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'LFA1'                 'NAME1'              'IT_SAIDA_0100'  'DS_TERM_CCT_PORTAL_RFL'  'Ds.Term.CCT Portal RFL'    '34'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'SALDO_EXPORTAR_RFL'      'Saldo a Exportar RFL'      '13'   ' '    'X' ' ' ' ' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263 inclusão de coluna
         ''                     ''                   'IT_SAIDA_0100'  'VINCULADA_XML_RFL'       'XML Vinculada'             '04'   ' '    ''  ' ' 'C' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263
         ''                     ''                   'IT_SAIDA_0100'  'CANCEL_RFL'              'Cancelado'                 '04'   ' '    ''  ' ' 'C' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263
*"// WBARBOSA 19112024 US-158263
         'J_1BNFDOC'            'DOCNUM'             'IT_SAIDA_0100'  'DOCNUM_RETORNO'          'Docnum Retorno'            '10'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
         'J_1BNFDOC'            'DOCDAT'             'IT_SAIDA_0100'  'DATA_RETORNO'            'Data Docnum Retorno'       '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFDOC'            'NFENUM'             'IT_SAIDA_0100'  'NF_RETORNO'              'nr.NFe.Ret'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZIB_NFE_DIST_ITM'     'CHAVE_NFE'          'IT_SAIDA_0100'  'CHAVE_RETORNO'           'Chave Retorno'             '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
*"// WBARBOSA 19112024 US-158263

         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'QTD_NF_RETORNO'          'Qtde NF Retorno'           '13'   ' '    'X' ' ' ' ' ' ' ' ' '' ,
         'ZSDT_EXPORT'          'FINALIDADE'         'IT_SAIDA_0100'  'FINALIDADE'              'Finalidade'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263 inclusão de coluna
         'ZSDT0053'             'INSTRUCAO'          'IT_SAIDA_0100'  'INSTRUCAO'               'Instrução'                 '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZSDT0170'             'ID_DUE'             'IT_SAIDA_0100'  'ID_DUE'                  'Id. DU-e'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZSDT0170'             'NUMERO_DUE'         'IT_SAIDA_0100'  'NUMERO_DUE'              'Numero DU-e'               '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZSDT0170'             'CHAVE_ACESSO'       'IT_SAIDA_0100'  'CHAVE_ACESSO'            'Chave Acesso DU-e'         '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZSDT0170'             'DT_REGISTRO'        'IT_SAIDA_0100'  'DT_DUE'                  'Dt.DU-e'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZSDT0170'             'ID_DUE'             'IT_SAIDA_0100'  'ID_DUE_RET'              'Id.DU-e Retif.'            '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZNOM_REME_NOTAS'      'TP_NF_REM'          'IT_SAIDA_0100'  'TP_NF_REM'               'Tipo de Exportação'        '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFDOC'            'NFENUM'             'IT_SAIDA_0100'  'NFENUM_EXP'              'Nro. NFe Exportação'       '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'ZSDT0172'             'FATURA_ID'          'IT_SAIDA_0100'  'FATURA_ID'               'Chave NFe Exportação'      '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'QTD_NF_EXPORT'           'Qtde. NFe. Exportação'     '13'   ' '    'X' ' ' ' ' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263 inclusão de coluna
         'ZNOM_TRANSPORTE'      'DS_NOME_TRANSPOR'   'IT_SAIDA_0100'  'NAVIO'                   'Navio'                     '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'QTDE_VINC_DUE'           'Qtde Vinc.DU-e'            '13'   ' '    'X'  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'SALDO_EXPORTAR'          'Saldo Exportar'            '13'   ' '    'X'  ' ' ' ' ' ' ' ' '' ,
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'SALDO_DEVOLUCAO'         'Saldo Devolução'           '13'   ' '    'X'  ' ' ' ' ' ' ' ' '' , "US #131067 - MMSILVA - 10.07.2025
         'J_1BNFLIN'            'MENGE'              'IT_SAIDA_0100'  'QTDE_BAIXADA'            'Qtde. Baixada'             '13'   ' '    'X'  ' ' ' ' 'X' ' ' '' ,
*         ''                     ''                   'IT_SAIDA_0100'  'ICONE'                   'Form.Lote'                 '07'   ' '    ''  ' ' 'C' ' ' ' ' '' , "// WBARBOSA 19112024 US-158263 - REMOVER
         'J_1BNFDOC'            'CANCEL'             'IT_SAIDA_0100'  'CANCEL'                  'Cancelado'                 '10'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0110'.

  ENDCASE.

ENDFORM.

FORM f_estrutura_alv USING
*                           VALUE(p_col_pos)       TYPE i "// WBARBOSA 19112024 US-158263
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
                           VALUE(p_key).

  IF NOT line_exists( t_field[ fieldname = p_field ] ) AND user_full IS INITIAL.
    EXIT.
  ENDIF.

  CLEAR wa_fcat.

  ADD 1 TO lv_pos_col. "// WBARBOSA 19112024 US-158263

  wa_fcat-col_pos     = lv_pos_col. "// WBARBOSA 19112024 US-158263

  wa_fcat-fieldname   = p_field.
  wa_fcat-tabname     = p_tabname.
  wa_fcat-ref_table   = p_ref_tabname.
  wa_fcat-ref_field   = p_ref_fieldname.
  wa_fcat-key         = ' '.
  wa_fcat-edit        = p_edit.
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
  wa_fcat-key         = p_key.

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

ENDFORM.

FORM f_limpa_variaveis .

  CLEAR: wa_saida_0100,
         it_saida_0100[],
         r_cfops[],
         r_model[],
         tg_lfa1[],
         tg_kna1[],
         tg_zsdt0170[],
         tg_zsdt0173[],
         tg_znom_reme_notas_vinc[],
         tg_zlest0146[],
         tg_zlest0147[],
         tg_j_1bbranch[],
         tg_j_1bnfdoc[],
         tg_j_1bnfe_active[],
         tg_zib_nfe_dist_itm[],
         tg_znom_transporte[].

ENDFORM.

FORM f_selecionar_dados .

  "Check p_matnr p_matkl
  IF p_matnr IS INITIAL AND p_matkl IS INITIAL.
    MESSAGE e024(sd) WITH 'Informar o código material'.
  ENDIF.

  PERFORM f_limpa_variaveis.

  PERFORM f_define_ranges.

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
   FROM j_1bnfdoc AS dc
    INNER JOIN j_1bnflin AS li ON li~docnum EQ dc~docnum
  WHERE dc~bukrs  IN p_bukrs
    AND dc~branch IN p_branch
    AND dc~docdat IN p_docdat
    AND dc~direct EQ '1'
    AND dc~model  IN r_model
    AND dc~cancel EQ abap_false
    AND dc~doctyp NE '5'
    AND dc~docnum IN p_docnum
    AND ( ( dc~nfnum IN p_nf ) OR ( dc~nfenum IN p_nf ) )
    AND dc~parid  IN p_parid
    AND li~cfop   IN r_cfops
    AND li~matnr  IN p_matnr
    AND li~charg  IN p_safra "// WBARBOSA 18112024 US-158263
    AND li~matkl  IN p_matkl.

  CHECK tg_j_1bnfdoc[] IS NOT INITIAL.

*"// WBARBOSA 18112024 US-158263
  PERFORM: f_check_eudr.
  CHECK tg_j_1bnfdoc[] IS NOT INITIAL.
*"// WBARBOSA 18112024 US-158263

  PERFORM: f_get_vinc_f_lote, "// WBARBOSA 18112024 US-158263
           f_get_j_1bnfe_active,
           f_set_chave_doc,
           f_get_zib_nfe_dist_itm,
           f_get_lfa1,
           f_get_kna1,
           f_get_j_1bbranch,
           f_get_makt,
           f_get_due,
           f_get_cct,
           f_get_znom_transporte,
           f_get_nfe_exportacao,
           f_get_nfe_exportacao_algodao,
           f_get_qnt_baixada.

ENDFORM.

FORM f_processa_dados.

  DATA: v_menge             TYPE ekpo-menge,
        v_rom_completo      TYPE char01,
        v_cct_cp            TYPE char01,
        wl_zlest0146_cp     TYPE zlest0146,
        v_count_dues        TYPE i,
        v_count_nf_prod     TYPE i,
        v_count_saida_nf    TYPE i,
        v_count_saida_doc   TYPE i,
        it_zsdt0001_ro_vinc TYPE zsdt0001_ro_vinc_t,
        chave_nf            TYPE zib_nfe_dist_itm-chave_nfe,
**<<<------"172407 - NMS - INI------>>>
*        docnum              TYPE j_1bnfdoc-docnum.
        docnum              TYPE j_1bnfdoc-docnum,
        docnum2             TYPE j_1bnfdoc-docnum,
        docnum3             TYPE j_1bnfdoc-docnum,
**<<<------"172407 - NMS - FIM------>>>

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
        navio               TYPE ty_saida_0100-navio,
        navio2              TYPE ty_saida_0100-navio,
        navio3              TYPE ty_saida_0100-navio.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

  DATA: wl_zlest0146 TYPE zlest0146,
        lt_zlest0147 TYPE zlest0147_t,
        v_doc_rateio TYPE char01.

  SORT: tg_j_1bnfdoc BY docnum,
        tg_zsdt0173  BY id_due,
        tg_lfa1      BY lifnr,
        tg_kna1      BY kunnr.

  DATA: ls_acckey     TYPE j_1b_nfe_access_key.

  DATA: wa_nf_produtor LIKE LINE OF tg_nf_produtor,
        wa_vbfa        LIKE LINE OF tg_vbfa,
        wa_nfdoc       LIKE LINE OF tg_nfdoc,
        wa_lin         LIKE LINE OF tg_lin,
        wa_active      LIKE LINE OF tg_active.

  DATA cont TYPE  i.

  CLEAR:  chave_nf.

  IF p_tp_nf IS NOT INITIAL.
    MOVE tg_j_1bnfdoc[] TO tg_j_1bnfdoc_2[].
    LOOP AT tg_j_1bnfdoc_2 INTO DATA(wa_doc).
      READ TABLE tg_znom_reme_notas INTO DATA(wa_notas) WITH KEY docnum =  wa_doc-docnum.
      IF sy-subrc = 4.
        DELETE tg_j_1bnfdoc[] WHERE docnum = wa_doc-docnum.
      ENDIF.
    ENDLOOP.
    REFRESH tg_j_1bnfdoc_2[].
  ENDIF.

  SORT tg_j_1bnfdoc BY docnum menge.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*  SORT tg_znom_reme_notas BY docnum.
  SORT tg_znom_reme_notas BY docnum      ASCENDING
                             dt_registro DESCENDING
                             hr_registro DESCENDING.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

**<<<------"173077 - NMS - INI------>>>
  zcl_im_cl_fluxo_exportacao=>get_cfop(  EXPORTING
                                           i_transferencia   = abap_on  "CFOP Entrada por Transferência
                                           i_comercializacao = abap_on   "CFOP Comercialização
                                           i_fim_especifico  = abap_off "CFOP Com Fins Especifico
                                         RECEIVING
                                           r_cfop            = DATA(rl_cfops_com)
                                       ).
**<<<------"173077 - NMS - FIM------>>>
  LOOP AT tg_j_1bnfdoc.

    CLEAR: wa_saida_0100.

    CLEAR wa_saida_0100-docnum_ligacao.
    READ TABLE gt_docnum_log INTO DATA(ls_docnum_log) WITH KEY docnum_eprod = tg_j_1bnfdoc-docnum.
    IF sy-subrc IS INITIAL.
      wa_saida_0100-docnum_ligacao = ls_docnum_log-docnum_prod_vinc_xml.
      wa_saida_0100-seq_doc = |{ ls_docnum_log-docnum_prod_vinc_xml }_{ ls_docnum_log-docnum_flote }_{ ls_docnum_log-docnum_eprod }|.
    ENDIF.

    "Fill Color Saida
    PERFORM f_fill_color_saida USING 3 CHANGING wa_saida_0100.

    READ TABLE lt_docnum_eudr WITH KEY docnum = tg_j_1bnfdoc-docnum eudr = 'S' TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      wa_saida_0100-eudr = icon_okay.
    ENDIF.

*    CLEAR gs_sumret.
*    READ TABLE gt_sumret INTO gs_sumret
*          WITH KEY docnum = tg_j_1bnfdoc-docnum.
*    IF sy-subrc IS INITIAL.
*      wa_saida_0100-qtd_nf_rfl = tg_j_1bnfdoc-menge - gs_sumret-quant_vinc.
*    ENDIF.

    wa_saida_0100-bukrs      = tg_j_1bnfdoc-bukrs.
    wa_saida_0100-branch     = tg_j_1bnfdoc-branch.
    wa_saida_0100-docnum     = tg_j_1bnfdoc-docnum.
    wa_saida_0100-itmnum     = tg_j_1bnfdoc-itmnum.
    wa_saida_0100-docdat     = tg_j_1bnfdoc-docdat.
    wa_saida_0100-credat     = tg_j_1bnfdoc-credat.
    wa_saida_0100-entrad     = tg_j_1bnfdoc-entrad.
    wa_saida_0100-chave_nfe  = tg_j_1bnfdoc-chave_nfe.
    READ TABLE tg_j_1bnfe_active INTO DATA(ws_active) WITH KEY docnum = tg_j_1bnfdoc-docnum.
    IF sy-subrc EQ 0.
      wa_saida_0100-cancel     = ws_active-cancel.
    ENDIF.

    PERFORM f_busca_inf_cte_rfl CHANGING wa_saida_0100.

    IF tg_j_1bnfdoc-chave_nfe IS NOT INITIAL.
      READ TABLE tg_zib_nfe_dist_itm WITH KEY chave_nfe = tg_j_1bnfdoc-chave_nfe.
      IF sy-subrc EQ 0 .
        wa_saida_0100-und_trib_xml  = tg_zib_nfe_dist_itm-prod_und_trib.
        wa_saida_0100-ncm_xml       = tg_zib_nfe_dist_itm-prod_ncm.
        wa_saida_0100-qcom_xml       = tg_zib_nfe_dist_itm-prod_qtd_comerci. "79982
        wa_saida_0100-cfop_xml      = tg_zib_nfe_dist_itm-prod_cfop.
      ENDIF.
    ENDIF.

    IF wa_saida_0100-entrad IS NOT INITIAL.
      wa_saida_0100-ent_prop =  icon_okay.
    ENDIF.

    IF tg_j_1bnfdoc-nfe IS NOT INITIAL.
      wa_saida_0100-nfenum  = tg_j_1bnfdoc-nfenum.
    ELSE.
      wa_saida_0100-nfenum  = tg_j_1bnfdoc-nfnum.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_saida_0100-nfenum
        IMPORTING
          output = wa_saida_0100-nfenum.
    ENDIF.

    wa_saida_0100-model   = tg_j_1bnfdoc-model.
    wa_saida_0100-series  = tg_j_1bnfdoc-series.

    "Processa Cliente/Fonecedor/Local de Negócio
    CASE tg_j_1bnfdoc-partyp.
      WHEN 'V'.
        READ TABLE tg_lfa1 WITH KEY lifnr = tg_j_1bnfdoc-parid BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_saida_0100-parid = tg_lfa1-lifnr.
          wa_saida_0100-name1 = tg_lfa1-name1.
          wa_saida_0100-regio = tg_lfa1-regio.
        ENDIF.
      WHEN 'C'.
        READ TABLE tg_kna1 WITH KEY kunnr = tg_j_1bnfdoc-parid BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_saida_0100-parid = tg_kna1-kunnr.
          wa_saida_0100-name1 = tg_kna1-name1.
          wa_saida_0100-regio = tg_kna1-regio.
        ENDIF.
      WHEN 'B'.
        READ TABLE tg_lfa1 WITH KEY lifnr = tg_j_1bnfdoc-parid_b BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_saida_0100-parid = tg_lfa1-lifnr.
          wa_saida_0100-name1 = tg_lfa1-name1.
          wa_saida_0100-regio = tg_lfa1-regio.
        ENDIF.
    ENDCASE.

    wa_saida_0100-matnr   = tg_j_1bnfdoc-matnr.
    READ TABLE tg_makt WITH KEY matnr = tg_j_1bnfdoc-matnr.
    IF sy-subrc EQ 0.
      wa_saida_0100-maktx = tg_makt-maktx.
    ENDIF.

    wa_saida_0100-cfop    = tg_j_1bnfdoc-cfop.
    wa_saida_0100-nbm     = tg_j_1bnfdoc-nbm.
    wa_saida_0100-meins   = tg_j_1bnfdoc-meins.

    IF tg_j_1bnfdoc-meins NE 'KG'.
      CALL FUNCTION 'ME_CONVERSION_MEINS'
        EXPORTING
          i_matnr             = tg_j_1bnfdoc-matnr
          i_mein1             = tg_j_1bnfdoc-meins
          i_meins             = 'KG'
          i_menge             = tg_j_1bnfdoc-menge
        IMPORTING
          menge               = v_menge
        EXCEPTIONS
          error_in_conversion = 1
          no_success          = 2
          OTHERS              = 3.
      IF NOT sy-subrc IS INITIAL.
        MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.
*        IF chave_nf NE tg_j_1bnfdoc-chave_nfe.
        wa_saida_0100-menge = v_menge.
*        ENDIF.
*        chave_nf = tg_j_1bnfdoc-chave_nfe.
      ENDIF.
    ELSE.

*      IF chave_nf NE tg_j_1bnfdoc-chave_nfe.
      wa_saida_0100-menge = tg_j_1bnfdoc-menge.
*      ENDIF.
*      chave_nf = tg_j_1bnfdoc-chave_nfe.

    ENDIF.

*-------------------------------------------------------------------------------*
*   Dados Romaneio Completo
*-------------------------------------------------------------------------------*
    CALL FUNCTION 'ZROMANEIO_VINCULADO_NF'
      EXPORTING
        i_docnum            = tg_j_1bnfdoc-docnum
        i_ck_cct_cp         = abap_true
        i_ck_cfop_e_zmemo00 = abap_true
      IMPORTING
        e_zsdt0001_ro_vinc  = it_zsdt0001_ro_vinc
        e_cct_cp            = v_cct_cp
        e_zlest0146_cp      = wl_zlest0146_cp
        e_romaneio_completo = v_rom_completo.

    IF v_rom_completo EQ abap_true.
      wa_saida_0100-rom_completo = icon_okay.
    ENDIF.

    READ TABLE it_zsdt0001_ro_vinc INTO DATA(_wl_rom_vinc) INDEX 1.
    IF ( sy-subrc EQ 0                           ) AND "Romaneio Completo
       ( it_zsdt0001_ro_vinc[]    IS NOT INITIAL ) AND
       ( _wl_rom_vinc-docnum_vinc IS NOT INITIAL ) AND
       ( wa_saida_0100-chave_nfe_rfl IS INITIAL ). "// wbarbosa 18112024 US-158263

      SELECT SINGLE *
        FROM j_1bnfe_active INTO @DATA(_wl_active)
       WHERE docnum = @_wl_rom_vinc-docnum_vinc.

      IF sy-subrc EQ 0.
        CONCATENATE _wl_active-regio   "Região do emissor NF-e
                    _wl_active-nfyear  "Ano da data do documento da NF-e
                    _wl_active-nfmonth "Mês da data do documento da NF-e
                    _wl_active-stcd1   "Nº CNPJ do emissor da NF-e
                    _wl_active-model   "Modelo da nota fiscal
                    _wl_active-serie   "SERIE
                    _wl_active-nfnum9  "Nº NF-e de nove posições
                    _wl_active-docnum9 "NF-e: nº aleatório
                    _wl_active-cdv     "Dígito controle p/chave de acesso NF-e
               INTO wa_saida_0100-chave_nfe_rfl.
      ENDIF.

      wa_saida_0100-docnum_rfl   = _wl_rom_vinc-docnum_vinc.
      wa_saida_0100-nfenum_rfl   = _wl_rom_vinc-nfenum_vinc.

      PERFORM f_get_terminal_rfl USING wa_saida_0100-docnum_rfl
                              CHANGING wa_saida_0100-terminal_rfl.

      IF wa_saida_0100-terminal_rfl IS NOT INITIAL.
        SELECT SINGLE name1
          FROM lfa1 INTO wa_saida_0100-ds_terminal_rfl
         WHERE lifnr = wa_saida_0100-terminal_rfl.
      ENDIF.

      IF p_cdterm[] IS NOT INITIAL.
        CHECK wa_saida_0100-terminal_rfl IN p_cdterm.
      ENDIF.

      IF v_cct_cp EQ abap_true.
        wa_saida_0100-cct_rfl      = icon_okay.
        wa_saida_0100-dt_cct_rfl   = wl_zlest0146_cp-dt_recepcao.
        wa_saida_0100-peso_cct_rfl = wl_zlest0146_cp-peso_aferido_recepcao.

        SELECT SINGLE *
          FROM zsdt0168 INTO @DATA(_wl_0168)
         WHERE codigo_ra EQ @wl_zlest0146_cp-local_codigo_ra.

        IF ( sy-subrc EQ 0 ) AND ( wl_zlest0146_cp-local_codigo_ra IS NOT INITIAL ).
          wa_saida_0100-term_cct_rfl = _wl_0168-lifnr.

          IF _wl_0168-lifnr IS NOT INITIAL.
            SELECT SINGLE name1
              FROM lfa1 INTO wa_saida_0100-ds_term_cct_rfl
             WHERE lifnr = _wl_0168-lifnr.
          ENDIF.

        ENDIF.

      ENDIF.
    ENDIF.

*-------------------------------------------------------------------------------*
*   Dados CCT
*-------------------------------------------------------------------------------*
    CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
      EXPORTING
        i_docnum     = wa_saida_0100-docnum
      IMPORTING
        e_zlest0146  = wl_zlest0146
        e_zlest0147  = lt_zlest0147
        e_doc_rateio = v_doc_rateio.

    IF ( wl_zlest0146 IS NOT INITIAL ) AND ( v_doc_rateio IS INITIAL ).
      wa_saida_0100-peso_aferido_recepcao = wl_zlest0146-peso_aferido_recepcao.

      READ TABLE lt_zlest0147 INTO DATA(wl_0147) INDEX 1.
      IF sy-subrc EQ 0.
        wa_saida_0100-peso_fiscal_cct = wl_0147-peso_fiscal.

        IF wl_0147-complemento EQ abap_true.
          wa_saida_0100-peso_cct = wl_0147-peso_fiscal.
        ELSE.
          wa_saida_0100-peso_cct = wl_zlest0146-peso_aferido_recepcao.
        ENDIF.

        IF wa_saida_0100-entrad EQ abap_true AND "Entrada Propria
           wa_saida_0100-chave_nfe NE wl_0147-chave_nfe AND
           wl_0147-chave_nfe IS NOT INITIAL.
          wa_saida_0100-chave_nfe_ref = wl_0147-chave_nfe.
        ENDIF.

      ENDIF.

      wa_saida_0100-dt_recepcao_cct       = wl_zlest0146-dt_recepcao.
      wa_saida_0100-dif_peso_cct_nf       = wa_saida_0100-menge - wa_saida_0100-peso_cct.

      SELECT SINGLE *
        FROM zsdt0168 INTO _wl_0168
       WHERE codigo_ra EQ wl_zlest0146-local_codigo_ra.

      IF ( sy-subrc EQ 0 ) AND ( wl_zlest0146-local_codigo_ra IS NOT INITIAL ).
        wa_saida_0100-term_cct = _wl_0168-lifnr.

        IF _wl_0168-lifnr IS NOT INITIAL.
          SELECT SINGLE name1
            FROM lfa1 INTO wa_saida_0100-ds_term_cct
           WHERE lifnr = _wl_0168-lifnr.
        ENDIF.
      ENDIF.
    ENDIF.


*-------------------------------------------------------------------------------*
*   Caso nota estiver vinculada em uma ou mais DU-e's, saida deve ser gerada por DU-e
*-------------------------------------------------------------------------------*
    CLEAR: v_count_dues.
    LOOP AT tg_znom_reme_notas WHERE docnum EQ wa_saida_0100-docnum
                                 AND id_due IS NOT INITIAL.
      READ TABLE tg_zsdt0170 WITH KEY id_due = tg_znom_reme_notas-id_due.
      CHECK sy-subrc EQ 0.
      ADD 1 TO v_count_dues.
    ENDLOOP.

**************************************************************************************
* Processo especifico para Algodão IR047064
**************************************************************************************
    CLEAR v_count_nf_prod.
    LOOP AT tg_nf_produtor_algodao INTO wa_nf_produtor WHERE docnum_prod EQ wa_saida_0100-docnum.
      ADD 1 TO v_count_nf_prod.
    ENDLOOP.

    IF tg_j_1bnfdoc-matkl EQ '700140'.

      v_count_saida_nf = 0.
      v_count_dues = 0.
      LOOP AT tg_nf_produtor_algodao INTO wa_nf_produtor WHERE docnum_prod EQ wa_saida_0100-docnum.

        READ TABLE tg_znom_reme_notas WITH KEY docnum = wa_nf_produtor-docnum_prod id_nomeacao_tran = wa_nf_produtor-id_nomeacao_tran.
        IF sy-subrc IS INITIAL AND tg_znom_reme_notas-id_due IS NOT INITIAL.
          READ TABLE tg_zsdt0170 WITH KEY id_due = tg_znom_reme_notas-id_due.
          CHECK sy-subrc EQ 0.
          wa_saida_0100-id_due            = tg_zsdt0170-id_due.
          wa_saida_0100-numero_due        = tg_zsdt0170-numero_due.
          wa_saida_0100-dt_due            = tg_zsdt0170-dt_registro.
          wa_saida_0100-chave_acesso      = tg_zsdt0170-chave_acesso.
          wa_saida_0100-qtde_vinc_due     = tg_znom_reme_notas-nr_quantidade.
          wa_saida_0100-tp_nf_rem         = tg_znom_reme_notas-tp_nf_rem.
        ELSE.
          CLEAR: wa_saida_0100-id_due,wa_saida_0100-numero_due, wa_saida_0100-dt_due, wa_saida_0100-chave_acesso, wa_saida_0100-qtde_vinc_due, wa_saida_0100-tp_nf_rem, tg_zsdt0170.

        ENDIF.

        ADD 1 TO v_count_saida_nf.
        wa_saida_0100-qtde_vinc_due     = wa_nf_produtor-menge.
        READ TABLE tg_vbfa INTO wa_vbfa WITH KEY vbelv = wa_nf_produtor-vbeln.
        IF sy-subrc IS INITIAL.
          READ TABLE tg_lin INTO wa_lin WITH KEY refkey = wa_vbfa-vbeln refitm = wa_vbfa-posnn.
          IF sy-subrc IS INITIAL.
            READ TABLE tg_nfdoc INTO wa_nfdoc WITH KEY docnum = wa_lin-docnum.
            IF sy-subrc IS INITIAL.
              READ TABLE tg_active INTO wa_active WITH KEY docnum = wa_nfdoc-docnum.
              IF sy-subrc IS INITIAL.
                CLEAR ls_acckey.
                MOVE-CORRESPONDING wa_active TO ls_acckey.
                wa_saida_0100-docnum_exp = ls_acckey-docnum9.
                wa_saida_0100-nfenum_exp = ls_acckey-nfnum9.
                wa_saida_0100-fatura_id  = ls_acckey.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        READ TABLE tg_zsdt0170_ret WITH KEY id_due_ref = tg_zsdt0170-id_due.
        IF sy-subrc EQ 0.
          wa_saida_0100-id_due_ret = tg_zsdt0170_ret-id_due.
        ENDIF.

        READ TABLE tg_znom_transporte WITH KEY id_nomeacao_tran = tg_zsdt0170-id_nomeacao_tran.
        IF sy-subrc EQ 0.
          wa_saida_0100-navio = tg_znom_transporte-ds_nome_transpor.
        ENDIF.

        tg_znom_reme_notas-docnum = wa_nf_produtor-docnum_prod.
        tg_znom_reme_notas-nr_quantidade = wa_nf_produtor-menge.
        APPEND tg_znom_reme_notas TO tg_znom_reme_notas_vinc.

        IF v_count_nf_prod EQ v_count_saida_nf.

          IF ( wa_saida_0100-peso_cct > 0 ) AND ( wa_saida_0100-peso_cct <= wa_saida_0100-menge ).
            wa_saida_0100-saldo_exportar = wa_saida_0100-peso_cct.
          ELSE.
            wa_saida_0100-saldo_exportar = wa_saida_0100-menge.
          ENDIF.

          LOOP AT tg_znom_reme_notas_vinc WHERE docnum EQ wa_saida_0100-docnum.
            SUBTRACT tg_znom_reme_notas_vinc-nr_quantidade FROM wa_saida_0100-saldo_exportar.
          ENDLOOP.
        ENDIF.

*** PBI 54906 - Inicio
        READ TABLE tg_zsdt0276_tot WITH  KEY docnum = wa_saida_0100-docnum
                                             itmnum = wa_saida_0100-itmnum.
        IF sy-subrc EQ 0.
          wa_saida_0100-qtde_baixada = tg_zsdt0276_tot-menge.
        ENDIF.

        wa_saida_0100-saldo_exportar = wa_saida_0100-saldo_exportar - wa_saida_0100-qtde_baixada.

*** PBI 54906 - Fim

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*        APPEND wa_saida_0100 TO it_saida_0100.
        PERFORM append_it_saida_0100 USING wa_saida_0100.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

      ENDLOOP.
      IF sy-subrc IS NOT INITIAL.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*        APPEND wa_saida_0100 TO it_saida_0100.
        PERFORM append_it_saida_0100 USING wa_saida_0100.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

      ENDIF.
    ENDIF.
**************************************************************************************
* Processo especifico para Algodão IR047064
**************************************************************************************

    IF v_count_dues EQ 0.

      CHECK tg_j_1bnfdoc-matkl NE '700140'.

      IF ( wa_saida_0100-peso_cct > 0 ) AND ( wa_saida_0100-peso_cct <= wa_saida_0100-menge ).
        wa_saida_0100-saldo_exportar = wa_saida_0100-peso_cct.
      ELSE.
        wa_saida_0100-saldo_exportar = wa_saida_0100-menge.
      ENDIF.

*** PBI 54906 - Inicio
      READ TABLE tg_zsdt0276_tot WITH  KEY docnum = wa_saida_0100-docnum
                                           itmnum = wa_saida_0100-itmnum.
      IF sy-subrc EQ 0.
        wa_saida_0100-qtde_baixada = tg_zsdt0276_tot-menge.
      ENDIF.

      wa_saida_0100-saldo_exportar = wa_saida_0100-saldo_exportar - wa_saida_0100-qtde_baixada.
*** PBI 54906 - Fim

******************************** Incluir Vinculo de Formação de Lote
*"// WBARBOSA 19112024 US-158263
      LOOP AT gt_flote_vinc-zsdtvinc_p_flote INTO DATA(ls_flot) WHERE docnum_eprod = wa_saida_0100-docnum.
**<<<------"173077 - NMS - INI------>>>
        DATA(vl_loop_vinc) = abap_on.
**<<<------"173077 - NMS - FIM------>>>
        READ TABLE gt_docnum_log INTO ls_docnum_log WITH KEY docnum_prod_vinc_xml = ls_flot-docnum_eprod.
        IF sy-subrc IS INITIAL.
          wa_saida_0100-docnum_ligacao = ls_docnum_log-docnum_prod_vinc_xml.
          wa_saida_0100-seq_doc = |{ ls_docnum_log-docnum_prod_vinc_xml }_{ ls_docnum_log-docnum_flote }_{ ls_docnum_log-docnum_eprod }|.
        ENDIF.

        CLEAR gs_sumret.
        READ TABLE gt_sumret INTO gs_sumret
              WITH KEY docnum = ls_flot-docnum_flote.
        IF sy-subrc IS INITIAL.
          wa_saida_0100-qtd_nf_rfl = tg_j_1bnfdoc-menge - gs_sumret-quant_vinc.
        ELSE.
          wa_saida_0100-saldo_exportar_rfl = tg_j_1bnfdoc-menge.
        ENDIF.

        IF ls_flot-cancel IS NOT INITIAL.
          PERFORM f_fill_color_saida USING 6 CHANGING wa_saida_0100.
        ELSE.
          PERFORM f_fill_color_saida USING 3 CHANGING wa_saida_0100.
        ENDIF.

        wa_saida_0100-id_vinc = ls_flot-id_vinc.
        wa_saida_0100-vinculada_xml_rfl = COND #( WHEN ls_flot-vinculada_xml IS NOT INITIAL THEN icon_okay ELSE abap_false ).
        wa_saida_0100-cancel_rfl = COND #( WHEN ls_flot-cancel IS NOT INITIAL THEN icon_delete ELSE abap_false ).

        READ TABLE gt_doc_ret INTO DATA(ls_doc_ret) WITH KEY docnum = ls_flot-docnum_flote.
        IF sy-subrc IS INITIAL.
          wa_saida_0100-qtd_nf_rfl = ls_doc_ret-menge.

          READ TABLE gt_sumret TRANSPORTING NO FIELDS
                WITH KEY docnum = ls_flot-docnum_flote.
          IF sy-subrc IS NOT INITIAL.
            wa_saida_0100-saldo_exportar_rfl = ls_doc_ret-menge.
          ENDIF.

        ENDIF.
        CLEAR ls_doc_ret.

        CLEAR _wl_active.
        READ TABLE gt_active_ret INTO _wl_active WITH KEY docnum = ls_flot-docnum_flote.
        IF sy-subrc IS INITIAL.

          wa_saida_0100-docnum_rfl    = _wl_active-docnum.
          wa_saida_0100-nfenum_rfl    = _wl_active-nfnum9.

          CONCATENATE _wl_active-regio   "Região do emissor NF-e
                      _wl_active-nfyear  "Ano da data do documento da NF-e
                      _wl_active-nfmonth "Mês da data do documento da NF-e
                      _wl_active-stcd1   "Nº CNPJ do emissor da NF-e
                      _wl_active-model   "Modelo da nota fiscal
                      _wl_active-serie   "SERIE
                      _wl_active-nfnum9  "Nº NF-e de nove posições
                      _wl_active-docnum9 "NF-e: nº aleatório
                      _wl_active-cdv     "Dígito controle p/chave de acesso NF-e
                 INTO wa_saida_0100-chave_nfe_rfl.

        ENDIF.

        PERFORM f_get_terminal_rfl USING wa_saida_0100-docnum_rfl
                                CHANGING wa_saida_0100-terminal_rfl.

        IF wa_saida_0100-terminal_rfl IS NOT INITIAL.
          SELECT SINGLE name1
            FROM lfa1 INTO wa_saida_0100-ds_terminal_rfl
           WHERE lifnr = wa_saida_0100-terminal_rfl.
        ENDIF.

        IF p_cdterm[] IS NOT INITIAL.
          CHECK wa_saida_0100-terminal_rfl IN p_cdterm.
        ENDIF.

        CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
          EXPORTING
            i_docnum     = wa_saida_0100-docnum_rfl
          IMPORTING
            e_zlest0146  = wl_zlest0146
            e_doc_rateio = v_doc_rateio.

*        IF v_doc_rateio IS NOT INITIAL AND wl_zlest0146 IS NOT INITIAL.  "// Comentado - WBARBOSA 04122024 US-158263
        IF v_doc_rateio IS INITIAL AND wl_zlest0146 IS NOT INITIAL. "// Aplicado a mesma regra na ZSDT0034 WBARBOSA 04122024 US-158263
          wa_saida_0100-cct_rfl      = icon_okay.
          wa_saida_0100-dt_cct_rfl   = wl_zlest0146-dt_recepcao.
          wa_saida_0100-peso_cct_rfl = wl_zlest0146-peso_aferido_recepcao.

          CLEAR _wl_0168.
          SELECT SINGLE *
            FROM zsdt0168 INTO @_wl_0168
           WHERE codigo_ra EQ @wl_zlest0146-local_codigo_ra.

          IF ( sy-subrc EQ 0 ) AND ( wl_zlest0146-local_codigo_ra IS NOT INITIAL ).
            wa_saida_0100-term_cct_rfl = _wl_0168-lifnr.

            IF _wl_0168-lifnr IS NOT INITIAL.
              SELECT SINGLE name1
                FROM lfa1 INTO wa_saida_0100-ds_term_cct_rfl
               WHERE lifnr = _wl_0168-lifnr.
            ENDIF.

          ENDIF.

        ENDIF.

        CLEAR: _wl_active, _wl_0168, wl_zlest0146.

        READ TABLE it_saida_0100 TRANSPORTING NO FIELDS WITH KEY docnum = wa_saida_0100-docnum
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
                                                                 cancel_rfl = abap_off.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração
        IF sy-subrc IS INITIAL.
          CLEAR wa_saida_0100-menge.
        ENDIF.

        READ TABLE it_saida_0100 TRANSPORTING NO FIELDS WITH KEY docnum_rfl = wa_saida_0100-docnum_rfl
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
                                                                 cancel_rfl = abap_off.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração
        IF sy-subrc IS INITIAL.
          CLEAR wa_saida_0100-saldo_exportar_rfl.
        ENDIF.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*        APPEND wa_saida_0100 TO it_saida_0100.
        PERFORM append_it_saida_0100 USING wa_saida_0100.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

      ENDLOOP. "// WBARBOSA 19112024 US-158263
**<<<------"172407 - NMS - INI------>>>
*      IF sy-subrc IS NOT INITIAL.
*        APPEND wa_saida_0100 TO it_saida_0100.
*      ENDIF.
* Verifica se há vínculo e se não é Comercial.
      IF vl_loop_vinc       IS INITIAL AND
         wa_saida_0100-cfop NOT IN rl_cfops_com.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*        APPEND wa_saida_0100 TO it_saida_0100.
        PERFORM append_it_saida_0100 USING wa_saida_0100.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

      ELSE.
        CLEAR vl_loop_vinc.

      ENDIF.
**<<<------"172407 - NMS - FIM------>>>
    ELSE.

      v_count_saida_doc = 0.

*"// WBARBOSA 19112024 US-158263
******************************** Incluir Vinculo de Formação de Lote
*"// WBARBOSA 19112024 US-158263
      LOOP AT gt_flote_vinc-zsdtvinc_p_flote INTO ls_flot WHERE docnum_eprod = wa_saida_0100-docnum.
**<<<------"173077 - NMS - INI------>>>
        vl_loop_vinc = abap_on.
**<<<------"173077 - NMS - FIM------>>>
        READ TABLE gt_docnum_log INTO ls_docnum_log WITH KEY docnum_prod_vinc_xml = ls_flot-docnum_eprod.
        IF sy-subrc IS INITIAL.
          wa_saida_0100-docnum_ligacao = ls_docnum_log-docnum_prod_vinc_xml.
          wa_saida_0100-seq_doc = |{ ls_docnum_log-docnum_prod_vinc_xml }_{ ls_docnum_log-docnum_flote }_{ ls_docnum_log-docnum_eprod }|.
        ENDIF.

        CLEAR gs_sumret.
        READ TABLE gt_sumret INTO gs_sumret
              WITH KEY docnum = ls_flot-docnum_flote.
        IF sy-subrc IS INITIAL.
          wa_saida_0100-qtd_nf_rfl = tg_j_1bnfdoc-menge - gs_sumret-quant_vinc.
        ENDIF.

        IF ls_flot-cancel IS NOT INITIAL.
          PERFORM f_fill_color_saida USING 6 CHANGING wa_saida_0100.
        ELSE.
          PERFORM f_fill_color_saida USING 3 CHANGING wa_saida_0100.
        ENDIF.

        wa_saida_0100-id_vinc = ls_flot-id_vinc.
        wa_saida_0100-vinculada_xml_rfl = COND #( WHEN ls_flot-vinculada_xml IS NOT INITIAL THEN icon_okay ELSE abap_false ).
        wa_saida_0100-cancel_rfl = COND #( WHEN ls_flot-cancel IS NOT INITIAL THEN icon_delete ELSE abap_false ).

        READ TABLE gt_doc_ret INTO ls_doc_ret WITH KEY docnum = ls_flot-docnum_flote.
        IF sy-subrc IS INITIAL.
          wa_saida_0100-qtd_nf_rfl = ls_doc_ret-menge.
        ENDIF.
        CLEAR ls_doc_ret.

        wa_saida_0100-saldo_exportar_rfl = wa_saida_0100-qtd_nf_rfl - gs_sumret-quant_vinc.

        READ TABLE gt_sumret TRANSPORTING NO FIELDS WITH KEY docnum = ls_flot-docnum_flote.
        IF sy-subrc IS NOT INITIAL.
          wa_saida_0100-saldo_exportar_rfl = wa_saida_0100-qtd_nf_rfl.
        ENDIF.

        CLEAR _wl_active.
        READ TABLE gt_active_ret INTO _wl_active WITH KEY docnum = ls_flot-docnum_flote.
        IF sy-subrc IS INITIAL.

          wa_saida_0100-docnum_rfl    = _wl_active-docnum.
          wa_saida_0100-nfenum_rfl    = _wl_active-nfnum9.

          CONCATENATE _wl_active-regio   "Região do emissor NF-e
                      _wl_active-nfyear  "Ano da data do documento da NF-e
                      _wl_active-nfmonth "Mês da data do documento da NF-e
                      _wl_active-stcd1   "Nº CNPJ do emissor da NF-e
                      _wl_active-model   "Modelo da nota fiscal
                      _wl_active-serie   "SERIE
                      _wl_active-nfnum9  "Nº NF-e de nove posições
                      _wl_active-docnum9 "NF-e: nº aleatório
                      _wl_active-cdv     "Dígito controle p/chave de acesso NF-e
                 INTO wa_saida_0100-chave_nfe_rfl.

        ENDIF.

        PERFORM f_get_terminal_rfl USING wa_saida_0100-docnum_rfl
                                CHANGING wa_saida_0100-terminal_rfl.

        IF wa_saida_0100-terminal_rfl IS NOT INITIAL.
          SELECT SINGLE name1
            FROM lfa1 INTO wa_saida_0100-ds_terminal_rfl
           WHERE lifnr = wa_saida_0100-terminal_rfl.
        ENDIF.

        IF p_cdterm[] IS NOT INITIAL.
          CHECK wa_saida_0100-terminal_rfl IN p_cdterm.
        ENDIF.

        CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
          EXPORTING
            i_docnum     = wa_saida_0100-docnum_rfl
          IMPORTING
            e_zlest0146  = wl_zlest0146
            e_doc_rateio = v_doc_rateio.

*        IF v_doc_rateio IS NOT INITIAL AND wl_zlest0146 IS NOT INITIAL.  "// Comentado WBARBOSA 04122024 US-158263
        IF v_doc_rateio IS INITIAL AND wl_zlest0146 IS NOT INITIAL.  "// Aplicado a mesma regra na ZSDT0034 WBARBOSA 04122024 US-158263
          wa_saida_0100-cct_rfl      = icon_okay.
          wa_saida_0100-dt_cct_rfl   = wl_zlest0146-dt_recepcao.
          wa_saida_0100-peso_cct_rfl = wl_zlest0146-peso_aferido_recepcao.

          CLEAR _wl_0168.
          SELECT SINGLE *
            FROM zsdt0168 INTO @_wl_0168
           WHERE codigo_ra EQ @wl_zlest0146-local_codigo_ra.

          IF ( sy-subrc EQ 0 ) AND ( wl_zlest0146-local_codigo_ra IS NOT INITIAL ).
            wa_saida_0100-term_cct_rfl = _wl_0168-lifnr.

            IF _wl_0168-lifnr IS NOT INITIAL.
              SELECT SINGLE name1
                FROM lfa1 INTO wa_saida_0100-ds_term_cct_rfl
               WHERE lifnr = _wl_0168-lifnr.
            ENDIF.

          ENDIF.

        ENDIF.

        CLEAR: _wl_active, _wl_0168, wl_zlest0146.
*"// WBARBOSA 19112024 US-158263

        LOOP AT tg_znom_reme_notas WHERE docnum EQ wa_saida_0100-docnum
                                     AND id_due IS NOT INITIAL.
*"// WBARBOSA 19112024 US-158263
          READ TABLE gt_nom_remetente INTO DATA(ls_nom_remetente)
          WITH KEY id_nomeacao_tran = tg_znom_reme_notas-id_nomeacao_tran
                   id_empresa       = tg_znom_reme_notas-id_empresa
                   id_filial        = tg_znom_reme_notas-id_filial
                   id_material      = tg_znom_reme_notas-id_material
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*                   id_remetente     = tg_znom_reme_notas-id_remetente
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração
                   grp_retorno      = tg_znom_reme_notas-grp_retorno.
          IF sy-subrc IS INITIAL.
            wa_saida_0100-docnum_retorno = ls_nom_remetente-docnum_rt.

            READ TABLE gt_doc_ret INTO ls_doc_ret WITH KEY docnum = ls_nom_remetente-docnum_rt.
            IF sy-subrc IS INITIAL.
              wa_saida_0100-nf_retorno = ls_doc_ret-nfenum.
              wa_saida_0100-data_retorno = ls_doc_ret-docdat.
              wa_saida_0100-qtd_nf_retorno = ls_doc_ret-menge.

              READ TABLE gt_export INTO DATA(ls_export) WITH KEY docnum = ls_doc_ret-docnum.
              IF sy-subrc IS INITIAL.

                wa_saida_0100-qtd_nf_export = ls_export-quant.

                READ TABLE gt_dd07t INTO DATA(ls_dd07t)   WITH KEY domvalue_l = ls_export-finalidade.
                IF sy-subrc IS INITIAL.
                  wa_saida_0100-finalidade = ls_export-finalidade.
                  CONCATENATE ls_export-finalidade ls_dd07t-ddtext INTO wa_saida_0100-finalidade SEPARATED BY '-'.
                ENDIF.

              ENDIF.
            ENDIF.

            READ TABLE gt_active_ret INTO DATA(ls_active_ret) WITH KEY docnum = ls_nom_remetente-docnum_rt.
            IF sy-subrc IS INITIAL.
              CONCATENATE ls_active_ret-regio
                          ls_active_ret-nfyear
                          ls_active_ret-nfmonth
                          ls_active_ret-stcd1
                          ls_active_ret-model
                          ls_active_ret-serie
                          ls_active_ret-nfnum9
                          ls_active_ret-docnum9
                          ls_active_ret-cdv
                     INTO wa_saida_0100-chave_retorno.
            ENDIF.

          ENDIF.
*"// WBARBOSA 19112024 US-158263

          READ TABLE tg_zsdt0170 WITH KEY id_due = tg_znom_reme_notas-id_due.
          CHECK sy-subrc EQ 0.

          ADD 1 TO v_count_saida_doc.

          wa_saida_0100-id_due            = tg_zsdt0170-id_due.
          wa_saida_0100-numero_due        = tg_zsdt0170-numero_due.
          wa_saida_0100-dt_due            = tg_zsdt0170-dt_registro.
          wa_saida_0100-chave_acesso      = tg_zsdt0170-chave_acesso.
**<<<------"172407 - NMS - INI------>>>
* Verifica se o Documento do Produtor e diferente para carregar a Qtd Vinc da DUe só na linha cabeçalho.
          IF docnum NE tg_znom_reme_notas-docnum
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
          OR navio  NE wa_saida_0100-navio.

            navio = wa_saida_0100-docnum.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

            docnum = wa_saida_0100-docnum.
**<<<------"172407 - NMS - FIM------>>>
            wa_saida_0100-qtde_vinc_due     = tg_znom_reme_notas-nr_quantidade.
**<<<------"172407 - NMS - INI------>>>
          ENDIF.
**<<<------"172407 - NMS - FIM------>>>
          wa_saida_0100-tp_nf_rem         = tg_znom_reme_notas-tp_nf_rem.

          READ TABLE tg_nf_produtor INTO wa_nf_produtor WITH KEY docnum_prod     = tg_znom_reme_notas-docnum
                                                                itmnum_prod      = tg_znom_reme_notas-itmnum
                                                                id_nomeacao_tran = tg_znom_reme_notas-id_nomeacao_tran
                                                                grp_retorno      = tg_znom_reme_notas-grp_retorno.

          IF sy-subrc IS INITIAL.

            READ TABLE tg_vbfa INTO wa_vbfa WITH KEY vbelv = wa_nf_produtor-vbeln.

            IF sy-subrc IS INITIAL.

              READ TABLE tg_lin INTO wa_lin WITH KEY refkey = wa_vbfa-vbeln
                                                    refitm = wa_vbfa-posnn.
              IF sy-subrc IS INITIAL.

                READ TABLE tg_nfdoc INTO wa_nfdoc WITH KEY docnum = wa_lin-docnum.

                IF sy-subrc IS INITIAL.

                  READ TABLE tg_active INTO wa_active WITH KEY docnum = wa_nfdoc-docnum.
                  IF sy-subrc IS INITIAL.

                    CLEAR ls_acckey.
                    MOVE-CORRESPONDING wa_active TO ls_acckey.

                    wa_saida_0100-docnum_exp = ls_acckey-docnum9.
                    wa_saida_0100-nfenum_exp = ls_acckey-nfnum9.
                    wa_saida_0100-fatura_id  = ls_acckey.

                  ENDIF.

                ENDIF.

              ENDIF.

            ENDIF.

          ENDIF.

          READ TABLE tg_zsdt0170_ret WITH KEY id_due_ref = tg_zsdt0170-id_due.
          IF sy-subrc EQ 0.
            wa_saida_0100-id_due_ret = tg_zsdt0170_ret-id_due.
          ENDIF.

          APPEND tg_znom_reme_notas TO tg_znom_reme_notas_vinc.

          READ TABLE tg_znom_transporte WITH KEY id_nomeacao_tran = tg_zsdt0170-id_nomeacao_tran.
          IF sy-subrc EQ 0.
            wa_saida_0100-navio = tg_znom_transporte-ds_nome_transpor.
          ENDIF.
**<<<------"172407 - NMS - INI------>>>
* Verifica se o Documento do Produtor e diferente para carregar a Qtd Vinc da DUe só na linha cabeçalho.
          IF docnum3 NE wa_saida_0100-docnum.
            docnum3 = wa_saida_0100-docnum.
**<<<------"172407 - NMS - FIM------>>>
            IF v_count_dues = v_count_saida_doc.
              IF ( wa_saida_0100-peso_cct > 0 ) AND ( wa_saida_0100-peso_cct <= wa_saida_0100-menge ).
                wa_saida_0100-saldo_exportar = wa_saida_0100-peso_cct.
              ELSE.
                wa_saida_0100-saldo_exportar = wa_saida_0100-menge.
              ENDIF.

              LOOP AT tg_znom_reme_notas_vinc WHERE docnum EQ wa_saida_0100-docnum.
                SUBTRACT tg_znom_reme_notas_vinc-nr_quantidade FROM wa_saida_0100-saldo_exportar.
              ENDLOOP.
            ENDIF.
**<<<------"172407 - NMS - INI------>>>
          ENDIF.
**<<<------"172407 - NMS - FIM------>>>
*** PBI 54906 - Inicio
          READ TABLE tg_zsdt0276_tot WITH  KEY docnum = wa_saida_0100-docnum
                                               itmnum = wa_saida_0100-itmnum.
          IF sy-subrc EQ 0.
**<<<------"172407 - NMS - INI------>>>
* Verifica se o Documento do Produtor e diferente para carregar a Qtd Vinc da DUe só na linha cabeçalho.
            IF docnum2 NE tg_zsdt0276_tot-docnum.
              docnum2 = wa_saida_0100-docnum.
**<<<------"172407 - NMS - FIM------>>>
              wa_saida_0100-qtde_baixada = tg_zsdt0276_tot-menge.
**<<<------"172407 - NMS - INI------>>>
            ENDIF.
**<<<------"172407 - NMS - FIM------>>>
          ENDIF.

          wa_saida_0100-saldo_exportar = wa_saida_0100-saldo_exportar - wa_saida_0100-qtde_baixada.
*** PBI 54906 - Fim

          READ TABLE it_saida_0100 TRANSPORTING NO FIELDS WITH KEY docnum_rfl = wa_saida_0100-docnum_rfl
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
                                                                   cancel_rfl = abap_off.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração
          IF sy-subrc IS INITIAL.
            CLEAR wa_saida_0100-saldo_exportar_rfl.
          ENDIF.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*          APPEND wa_saida_0100 TO it_saida_0100.
          PERFORM append_it_saida_0100 USING wa_saida_0100.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

**<<<------"172407 - NMS - INI------>>>
*          CLEAR: wa_saida_0100-menge.
          CLEAR: wa_saida_0100-menge, wa_saida_0100-qtde_vinc_due, wa_saida_0100-saldo_exportar, wa_saida_0100-qtde_baixada, wa_saida_0100-qcom_xml.
**<<<------"172407 - NMS - FIM------>>>
        ENDLOOP.

      ENDLOOP. "// WBARBOSA 19112024 US-158263
* -------------------------------------------------------------------------------------------------------
* >>>>><<<<<< Ajuste para contemplar os dados do passado >>>>>> INICIO - 174083 SMC >>>>>>>>><<<<<<<<<<
      LOOP AT tg_znom_reme_notas WHERE docnum EQ wa_saida_0100-docnum
                                   AND id_due IS NOT INITIAL.
**<<<------"174186 - NMS - INI------>>>
        READ TABLE tg_znom_reme_notas_vinc TRANSPORTING NO FIELDS WITH KEY docnum = tg_znom_reme_notas-docnum.
        CHECK NOT sy-subrc IS INITIAL.
**<<<------"174186 - NMS - FIM------>>>
        vl_loop_vinc = abap_on.
*"// WBARBOSA 19112024 US-158263
        READ TABLE gt_nom_remetente INTO ls_nom_remetente
        WITH KEY id_nomeacao_tran = tg_znom_reme_notas-id_nomeacao_tran
                 id_empresa       = tg_znom_reme_notas-id_empresa
                 id_filial        = tg_znom_reme_notas-id_filial
                 id_material      = tg_znom_reme_notas-id_material
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*                 id_remetente     = tg_znom_reme_notas-id_remetente
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração
                 grp_retorno      = tg_znom_reme_notas-grp_retorno.
        IF sy-subrc IS INITIAL.
          wa_saida_0100-docnum_retorno = ls_nom_remetente-docnum_rt.

          READ TABLE gt_doc_ret INTO ls_doc_ret WITH KEY docnum = ls_nom_remetente-docnum_rt.
          IF sy-subrc IS INITIAL.
            wa_saida_0100-nf_retorno = ls_doc_ret-nfenum.
            wa_saida_0100-data_retorno = ls_doc_ret-docdat.
            wa_saida_0100-qtd_nf_retorno = ls_doc_ret-menge.

            READ TABLE gt_export INTO ls_export WITH KEY docnum = ls_doc_ret-docnum.
            IF sy-subrc IS INITIAL.

              wa_saida_0100-qtd_nf_export = ls_export-quant.

              READ TABLE gt_dd07t INTO ls_dd07t  WITH KEY domvalue_l = ls_export-finalidade.
              IF sy-subrc IS INITIAL.
                wa_saida_0100-finalidade = ls_export-finalidade.
                CONCATENATE ls_export-finalidade ls_dd07t-ddtext INTO wa_saida_0100-finalidade SEPARATED BY '-'.
              ENDIF.

            ENDIF.
          ENDIF.

          READ TABLE gt_active_ret INTO ls_active_ret WITH KEY docnum = ls_nom_remetente-docnum_rt.
          IF sy-subrc IS INITIAL.
            CONCATENATE ls_active_ret-regio
                        ls_active_ret-nfyear
                        ls_active_ret-nfmonth
                        ls_active_ret-stcd1
                        ls_active_ret-model
                        ls_active_ret-serie
                        ls_active_ret-nfnum9
                        ls_active_ret-docnum9
                        ls_active_ret-cdv
                   INTO wa_saida_0100-chave_retorno.
          ENDIF.

        ENDIF.
*"// WBARBOSA 19112024 US-158263

        READ TABLE tg_zsdt0170 WITH KEY id_due = tg_znom_reme_notas-id_due.
        CHECK sy-subrc EQ 0.

        ADD 1 TO v_count_saida_doc.

        wa_saida_0100-id_due            = tg_zsdt0170-id_due.
        wa_saida_0100-numero_due        = tg_zsdt0170-numero_due.
        wa_saida_0100-dt_due            = tg_zsdt0170-dt_registro.
        wa_saida_0100-chave_acesso      = tg_zsdt0170-chave_acesso.
**<<<------"174186 - NMS - INI------>>>
***<<<------"172407 - NMS - INI------>>>
** Verifica se o Documento do Produtor e diferente para carregar a Qtd Vinc da DUe só na linha cabeçalho.
*          IF docnum NE tg_znom_reme_notas-docnum.
*            docnum = wa_saida_0100-docnum.
***<<<------"172407 - NMS - FIM------>>>
*            wa_saida_0100-qtde_vinc_due     = tg_znom_reme_notas-nr_quantidade.
***<<<------"172407 - NMS - INI------>>>
*          ENDIF.
***<<<------"172407 - NMS - FIM------>>>
        wa_saida_0100-qtde_vinc_due     = tg_znom_reme_notas-nr_quantidade.
**<<<------"174186 - NMS - FIM------>>>
        wa_saida_0100-tp_nf_rem         = tg_znom_reme_notas-tp_nf_rem.

        READ TABLE tg_nf_produtor INTO wa_nf_produtor WITH KEY docnum_prod     = tg_znom_reme_notas-docnum
                                                              itmnum_prod      = tg_znom_reme_notas-itmnum
                                                              id_nomeacao_tran = tg_znom_reme_notas-id_nomeacao_tran
                                                              grp_retorno      = tg_znom_reme_notas-grp_retorno.

        IF sy-subrc IS INITIAL.

          READ TABLE tg_vbfa INTO wa_vbfa WITH KEY vbelv = wa_nf_produtor-vbeln.

          IF sy-subrc IS INITIAL.

            READ TABLE tg_lin INTO wa_lin WITH KEY refkey = wa_vbfa-vbeln
                                                  refitm = wa_vbfa-posnn.
            IF sy-subrc IS INITIAL.

              READ TABLE tg_nfdoc INTO wa_nfdoc WITH KEY docnum = wa_lin-docnum.

              IF sy-subrc IS INITIAL.

                READ TABLE tg_active INTO wa_active WITH KEY docnum = wa_nfdoc-docnum.
                IF sy-subrc IS INITIAL.

                  CLEAR ls_acckey.
                  MOVE-CORRESPONDING wa_active TO ls_acckey.

                  wa_saida_0100-docnum_exp = ls_acckey-docnum9.
                  wa_saida_0100-nfenum_exp = ls_acckey-nfnum9.
                  wa_saida_0100-fatura_id  = ls_acckey.

                ENDIF.

              ENDIF.

            ENDIF.

          ENDIF.

        ENDIF.

        READ TABLE tg_zsdt0170_ret WITH KEY id_due_ref = tg_zsdt0170-id_due.
        IF sy-subrc EQ 0.
          wa_saida_0100-id_due_ret = tg_zsdt0170_ret-id_due.
        ENDIF.
**<<<------"174186 - NMS - INI------>>>
*        APPEND tg_znom_reme_notas TO tg_znom_reme_notas_vinc.
**<<<------"174186 - NMS - FIM------>>>
        READ TABLE tg_znom_transporte WITH KEY id_nomeacao_tran = tg_zsdt0170-id_nomeacao_tran.
        IF sy-subrc EQ 0.
          wa_saida_0100-navio = tg_znom_transporte-ds_nome_transpor.
        ENDIF.
**<<<------"172407 - NMS - INI------>>>
* Verifica se o Documento do Produtor e diferente para carregar a Qtd Vinc da DUe só na linha cabeçalho.
        IF docnum3 NE wa_saida_0100-docnum
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
          OR navio3 NE wa_saida_0100-navio.

          navio3 = wa_saida_0100-docnum.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

          docnum3 = wa_saida_0100-docnum.
**<<<------"172407 - NMS - FIM------>>>
          IF v_count_dues = v_count_saida_doc.
            IF ( wa_saida_0100-peso_cct > 0 ) AND ( wa_saida_0100-peso_cct <= wa_saida_0100-menge ).
              wa_saida_0100-saldo_exportar = wa_saida_0100-peso_cct.
            ELSE.
              wa_saida_0100-saldo_exportar = wa_saida_0100-menge.
            ENDIF.

            LOOP AT tg_znom_reme_notas_vinc WHERE docnum EQ wa_saida_0100-docnum.
              SUBTRACT tg_znom_reme_notas_vinc-nr_quantidade FROM wa_saida_0100-saldo_exportar.
            ENDLOOP.
          ENDIF.
**<<<------"172407 - NMS - INI------>>>
        ENDIF.
**<<<------"172407 - NMS - FIM------>>>
*** PBI 54906 - Inicio
        READ TABLE tg_zsdt0276_tot WITH  KEY docnum = wa_saida_0100-docnum
                                             itmnum = wa_saida_0100-itmnum.
        IF sy-subrc EQ 0.
**<<<------"172407 - NMS - INI------>>>
* Verifica se o Documento do Produtor e diferente para carregar a Qtd Vinc da DUe só na linha cabeçalho.
          IF docnum2 NE tg_zsdt0276_tot-docnum
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
          OR navio2  NE wa_saida_0100-navio.

            navio2 = wa_saida_0100-docnum.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

            docnum2 = wa_saida_0100-docnum.
**<<<------"172407 - NMS - FIM------>>>
            wa_saida_0100-qtde_baixada = tg_zsdt0276_tot-menge.
**<<<------"172407 - NMS - INI------>>>
          ENDIF.
**<<<------"172407 - NMS - FIM------>>>
        ENDIF.

        wa_saida_0100-saldo_exportar = wa_saida_0100-saldo_exportar - wa_saida_0100-qtde_baixada.
*** PBI 54906 - Fim

        READ TABLE it_saida_0100 TRANSPORTING NO FIELDS WITH KEY docnum_rfl = wa_saida_0100-docnum_rfl
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
                                                                 cancel_rfl = abap_off.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração
        IF sy-subrc IS INITIAL.
          CLEAR wa_saida_0100-saldo_exportar_rfl.
        ENDIF.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*        APPEND wa_saida_0100 TO it_saida_0100.
        PERFORM append_it_saida_0100 USING wa_saida_0100.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

**<<<------"172407 - NMS - INI------>>>
*          CLEAR: wa_saida_0100-menge.
        CLEAR: wa_saida_0100-menge, wa_saida_0100-qtde_vinc_due, wa_saida_0100-saldo_exportar, wa_saida_0100-qtde_baixada, wa_saida_0100-qcom_xml.
**<<<------"172407 - NMS - FIM------>>>
      ENDLOOP.
* >>>>><<<<<< Ajuste para contemplar os dados do passado >>>>>>>>> FIM - 174083 SMC >>>>>>>>><<<<<<<<<<
* -------------------------------------------------------------------------------------------------------

* Verifica se há vínculo e se não é Comercial.
      IF vl_loop_vinc       IS INITIAL AND
         wa_saida_0100-cfop NOT IN rl_cfops_com.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*        APPEND wa_saida_0100 TO it_saida_0100.
        PERFORM append_it_saida_0100 USING wa_saida_0100.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

      ELSE.
        CLEAR vl_loop_vinc.

      ENDIF.
**<<<------"172407 - NMS - FIM------>>>
    ENDIF.
  ENDLOOP.

  SORT it_saida_0100 DESCENDING BY saldo_exportar docnum.


  PERFORM: f_consulta_notas_cct,
           f_atrib_conf_cct_portal.


*"// wbarbosa 18112024 US-158263
  IF p_ccct IS NOT INITIAL.
    SORT it_saida_0100 DESCENDING BY conf_cct_portal .
    DELETE it_saida_0100 WHERE conf_cct_portal NE icon_okay.
  ENDIF.

  IF p_scct IS NOT INITIAL.
    SORT it_saida_0100 DESCENDING BY conf_cct_portal .
    DELETE it_saida_0100 WHERE conf_cct_portal EQ icon_okay.
  ENDIF.
*"// wbarbosa 18112024 US-158263

*Inicio ajuste #71835 - Anderson Oenning
  "Ajustar saldo.

  DATA(it_saida_0100_aux) = it_saida_0100.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
  DELETE it_saida_0100_aux WHERE cancel_rfl NE abap_off.
  SORT it_saida_0100_aux BY docnum.

  SORT gt_flote_vinc-zsdtvinc_p_flote BY docnum_eprod.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

  DATA: lt_docnum TYPE zsdt_doc_devolucao_sigam.

  MOVE-CORRESPONDING it_saida_0100 TO lt_docnum.

  PERFORM f_consultar_devolucao_sigam USING lt_docnum CHANGING lt_docnum.

  SORT lt_docnum BY docnum.

  LOOP AT it_saida_0100 ASSIGNING FIELD-SYMBOL(<ls_saida>).
    IF <ls_saida>-menge IS NOT INITIAL.
      CLEAR: qtde_total.

      IF <ls_saida>-conf_cct_portal EQ icon_okay.
        IF <ls_saida>-peso_aferido_recepcao > <ls_saida>-menge.
          qtde_total = <ls_saida>-menge.
        ELSE.
          qtde_total = <ls_saida>-peso_aferido_recepcao.
        ENDIF.
      ELSE.
        qtde_total = <ls_saida>-menge.
      ENDIF.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
*      LOOP AT it_saida_0100_aux ASSIGNING FIELD-SYMBOL(<ls_saida_aux>) WHERE docnum EQ <ls_saida>-docnum.
***<<<------"174186 - NMS - INI------>>>
**        CHECK <ls_saida_aux>-menge IS NOT INITIAL. "<<<------"172407 - NMS------>>>
*        CHECK <ls_saida_aux>-qtde_vinc_due IS NOT INITIAL. "<<<------"174186 - NMS------>>>
***<<<------"174186 - NMS - FIM------>>>
*        qtde_total = ( qtde_total - <ls_saida_aux>-qtde_vinc_due - <ls_saida_aux>-qtde_baixada ).
*      ENDLOOP.

      READ TABLE gt_flote_vinc-zsdtvinc_p_flote INTO DATA(ls_vinc_flote)
                                                WITH KEY docnum_eprod = <ls_saida>-docnum
                                                                            BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        qtde_total = ls_vinc_flote-qtd_vinc.

      ENDIF.

      READ TABLE it_saida_0100_aux TRANSPORTING NO FIELDS
                                   WITH KEY docnum =  <ls_saida>-docnum
                                                          BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        LOOP AT it_saida_0100_aux INTO DATA(ls_doc) FROM sy-tabix.


          IF ls_doc-docnum NE <ls_saida>-docnum.
            EXIT.
          ENDIF.


          IF ls_doc-qtde_vinc_due IS NOT INITIAL.

            qtde_total = ( qtde_total - ls_doc-qtde_vinc_due - ls_doc-qtde_baixada ).

          ELSE.

            qtde_total = ( qtde_total - ls_doc-qtde_baixada ).

          ENDIF.

        ENDLOOP.

      ENDIF.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

      <ls_saida>-saldo_exportar = qtde_total.

*** US #131067 - MMSILVA - 09.07.2025 - Ini ***
*      PERFORM f_consultar_devolucao_sigam USING <ls_saida> CHANGING <ls_saida>.
*** US #131067 - MMSILVA - 09.07.2025 - Fim ***
      READ TABLE lt_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>)
      WITH KEY docnum = <ls_saida>-docnum
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        <ls_saida>-saldo_devolucao = <fs_docnum>-valor.
      ENDIF.

    ELSE.
      <ls_saida>-saldo_exportar = ' '.
    ENDIF.
    CLEAR: qtde_total.
  ENDLOOP.


*Fim ajuste #71835 - Anderson Oenning

  "FI 133795 CS2024000104 Incluir a colunas ZSDT0149 - PSA
  LOOP AT it_saida_0100 ASSIGNING FIELD-SYMBOL(<get_id_due>) WHERE chave_nfe_rfl IS NOT INITIAL.

    IF  <get_id_due>-chave_nfe_rfl IS NOT INITIAL.
      SELECT SINGLE a~id_due FROM zsdt0173 AS a
      WHERE a~id_fatura_ref = @<get_id_due>-chave_nfe_rfl
      INTO (@<get_id_due>-id_due).

      SELECT SINGLE a~numero_due,a~dt_registro
      FROM zsdt0170 AS a
      WHERE a~id_due = @<get_id_due>-id_due
      INTO (@<get_id_due>-numero_due,@<get_id_due>-dt_due).

    ENDIF.

  ENDLOOP.

  LOOP AT it_saida_0100 ASSIGNING FIELD-SYMBOL(<get_instrucao>) WHERE docnum_exp IS NOT INITIAL AND branch IS NOT INITIAL.

    SELECT SINGLE docnum FROM j_1bnfdoc WHERE nfenum = @<get_instrucao>-nfenum_exp AND branch = @<get_instrucao>-branch INTO @DATA(l_docnum).

    SELECT SINGLE refkey FROM j_1bnflin WHERE docnum = @l_docnum INTO @DATA(l_refkey).

    IF l_refkey IS NOT INITIAL.
      DATA(l_key) =  l_refkey+0(10).

      SELECT SINGLE vbelv FROM vbfa WHERE vbeln = @l_key INTO @DATA(l_vbelv).

      IF l_vbelv IS NOT INITIAL.
        SELECT SINGLE instrucao FROM zsdt0053 WHERE vbeln = @l_vbelv INTO (@<get_instrucao>-instrucao).
      ENDIF.

    ENDIF.

  ENDLOOP.

*"// WBARBOSA 18112024 US-158263
  IF p_instru IS NOT INITIAL.
    DELETE it_saida_0100 WHERE instrucao NOT IN p_instru.
  ENDIF.
*"// WBARBOSA 18112024 US-158263

  MOVE-CORRESPONDING it_saida_0100 TO it_saida_0200.

  SORT it_saida_0100 BY bukrs branch credat docnum_ligacao docnum id_due.

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
***<<<------"172407 - NMS - INI------>>>
*  DATA(vl_qtlin) = lines( it_saida_0100 ).
*  LOOP AT it_saida_0100 ASSIGNING FIELD-SYMBOL(<fs_saida>) WHERE cancel_rfl IS NOT INITIAL.
*    DATA(vl_tabix) = sy-tabix + 1.
** Verifica se no calculo da VL_TABIX não excede a quantidade de linhas da TI (última linha).
*    IF vl_tabix LE vl_qtlin.
*      LOOP AT it_saida_0100 ASSIGNING FIELD-SYMBOL(<fs_saida2>) FROM vl_tabix.
*        IF <fs_saida2>-docnum     EQ <fs_saida>-docnum AND
*           <fs_saida2>-cancel_rfl EQ abap_off.
*          <fs_saida2>-menge          = <fs_saida>-menge.
*          <fs_saida2>-saldo_exportar = <fs_saida>-saldo_exportar.
*          <fs_saida2>-qcom_xml       = <fs_saida>-qcom_xml.
*          EXIT.
*
*        ENDIF.
*
*      ENDLOOP.
*
*    ENDIF.
*
*    CLEAR: <fs_saida>-menge, <fs_saida>-saldo_exportar, <fs_saida>-saldo_exportar_rfl, <fs_saida>-qcom_xml.
*
*  ENDLOOP.
***<<<------"172407 - NMS - FIM------>>>
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

ENDFORM.

FORM f_define_ranges.

  FREE r_cfops.

  r_model-sign   = 'I'.
  r_model-option = 'EQ'.
  r_model-low    = '55'.
  APPEND r_model.

  r_model-low    = '01'.
  APPEND r_model.

  r_model-low    = '04'.
  APPEND r_model.

*"// wbarbosa 25112024 US-158263
  zcl_im_cl_fluxo_exportacao=>get_cfop(
    EXPORTING
      i_transferencia   = p_tran "// CFOP Entrada por Transferência
      i_comercializacao = p_come "// CFOP Comercialização
      i_fim_especifico  = p_ccfe "// CFOP Com Fins Especifico
    RECEIVING
      r_cfop            = r_cfops
  ).
*"// wbarbosa 25112024 US-158263

ENDFORM.

FORM f_get_lfa1 .

  tg_j_1bnfdoc_aux[] = tg_j_1bnfdoc[].
  DELETE tg_j_1bnfdoc_aux WHERE partyp NE 'V'.
  SORT tg_j_1bnfdoc_aux BY parid.
  DELETE ADJACENT DUPLICATES FROM tg_j_1bnfdoc_aux COMPARING parid.

  CHECK tg_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM lfa1 INTO TABLE tg_lfa1
     FOR ALL ENTRIES IN tg_j_1bnfdoc_aux
   WHERE lifnr EQ tg_j_1bnfdoc_aux-parid.

ENDFORM.

FORM f_get_kna1 .

  tg_j_1bnfdoc_aux[] = tg_j_1bnfdoc[].
  DELETE tg_j_1bnfdoc_aux WHERE partyp NE 'C'.
  SORT tg_j_1bnfdoc_aux BY parid.
  DELETE ADJACENT DUPLICATES FROM tg_j_1bnfdoc_aux COMPARING parid.

  CHECK tg_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM kna1 INTO TABLE tg_kna1
     FOR ALL ENTRIES IN tg_j_1bnfdoc_aux
   WHERE kunnr EQ tg_j_1bnfdoc_aux-parid.

ENDFORM.

FORM f_get_j_1bbranch .

  LOOP AT tg_j_1bnfdoc WHERE partyp EQ 'B'.
    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = tg_j_1bnfdoc-parid+4(4)
      IMPORTING
        output = tg_j_1bnfdoc-parid_b.

    MODIFY tg_j_1bnfdoc.
  ENDLOOP.

  tg_j_1bnfdoc_aux[] = tg_j_1bnfdoc[].
  DELETE tg_j_1bnfdoc_aux WHERE partyp NE 'B'.
  SORT tg_j_1bnfdoc_aux BY parid_b.
  DELETE ADJACENT DUPLICATES FROM tg_j_1bnfdoc_aux COMPARING parid_b.

  CHECK tg_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM lfa1 APPENDING TABLE tg_lfa1
     FOR ALL ENTRIES IN tg_j_1bnfdoc_aux
   WHERE lifnr EQ tg_j_1bnfdoc_aux-parid_b.

ENDFORM.

FORM f_get_makt.

  tg_j_1bnfdoc_aux[] = tg_j_1bnfdoc[].
  SORT tg_j_1bnfdoc_aux BY matnr.
  DELETE ADJACENT DUPLICATES FROM tg_j_1bnfdoc_aux COMPARING matnr.

  CHECK tg_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM makt INTO TABLE tg_makt
     FOR ALL ENTRIES IN tg_j_1bnfdoc_aux
   WHERE matnr EQ tg_j_1bnfdoc_aux-matnr
     AND spras EQ sy-langu.

ENDFORM.

FORM f_get_due.

  CHECK tg_j_1bnfdoc[] IS NOT INITIAL.

  SELECT *
    FROM znom_reme_notas INTO TABLE tg_znom_reme_notas
     FOR ALL ENTRIES IN tg_j_1bnfdoc
   WHERE docnum    EQ tg_j_1bnfdoc-docnum
    AND  tp_nf_rem IN p_tp_nf.

  DATA(tg_znom_reme_notas_aux) = tg_znom_reme_notas[].
  DELETE tg_znom_reme_notas_aux WHERE id_due IS INITIAL.

  CHECK tg_znom_reme_notas_aux[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0170 INTO TABLE tg_zsdt0170
     FOR ALL ENTRIES IN tg_znom_reme_notas_aux
   WHERE id_due = tg_znom_reme_notas_aux-id_due
     AND status = '1'.

  SELECT *
    FROM zsdt0170 INTO TABLE tg_zsdt0170_ret
     FOR ALL ENTRIES IN tg_znom_reme_notas_aux
   WHERE id_due_ref  EQ tg_znom_reme_notas_aux-id_due
     AND loekz       EQ abap_false
     AND status      EQ '1'.

  DELETE tg_zsdt0170 WHERE loekz            IS NOT INITIAL OR
                           bloqueio_interno IS NOT INITIAL.

  SORT tg_zsdt0170 BY id_due.
  DELETE ADJACENT DUPLICATES FROM tg_zsdt0170 COMPARING id_due.


ENDFORM.

FORM f_get_cct.

  CHECK tg_j_1bnfdoc[] IS NOT INITIAL.

  SELECT *
    FROM zlest0147 AS a INTO TABLE tg_zlest0147
     FOR ALL ENTRIES IN tg_j_1bnfdoc
   WHERE docnum EQ tg_j_1bnfdoc-docnum
     AND EXISTS ( SELECT id_recepcao
                    FROM zlest0146 AS b
                   WHERE b~id_recepcao EQ a~id_recepcao
                     AND b~cancel      EQ abap_false ).

  CHECK tg_zlest0147[] IS NOT INITIAL.

  SELECT *
    FROM zlest0146 INTO TABLE tg_zlest0146
     FOR ALL ENTRIES IN tg_zlest0147
   WHERE id_recepcao EQ tg_zlest0147-id_recepcao
     AND cancel      EQ abap_false.

  CHECK tg_zlest0146[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0168 INTO TABLE tg_zsdt0168
     FOR ALL ENTRIES IN tg_zlest0146
   WHERE codigo_ra = tg_zlest0146-local_codigo_ra.

  CHECK tg_zsdt0168[] IS NOT INITIAL.

  SELECT *
    FROM lfa1 APPENDING TABLE tg_lfa1
     FOR ALL ENTRIES IN tg_zsdt0168
   WHERE lifnr = tg_zsdt0168-lifnr.

ENDFORM.

FORM f_get_znom_transporte.

  CHECK tg_zsdt0170[] IS NOT INITIAL.

  SELECT *
    FROM znom_transporte INTO TABLE tg_znom_transporte
     FOR ALL ENTRIES IN tg_zsdt0170
   WHERE id_nomeacao_tran = tg_zsdt0170-id_nomeacao_tran.

ENDFORM.

FORM f_get_terminal_rfl  USING p_docnum_rfl
                      CHANGING p_terminal_rfl.

  DATA: it_carta_correcao TYPE TABLE OF zcarta_correcao,
        wa_ultima_carta   TYPE zcarta_correcao.

  CLEAR: p_terminal_rfl.

  CHECK p_docnum_rfl IS NOT INITIAL.

  SELECT SINGLE *
    FROM j_1bnflin INTO @DATA(_lin)
   WHERE docnum EQ @p_docnum_rfl.

  CHECK ( sy-subrc EQ 0 ) AND ( _lin-reftyp EQ 'BI' ).

  "Get Fatura
  SELECT SINGLE *
    FROM vbrp INTO @DATA(_fat)
   WHERE vbeln = @_lin-refkey(10).

  CHECK ( sy-subrc EQ 0 ).

  "Get Remessa
  SELECT SINGLE *
    FROM lips INTO @DATA(_rem)
   WHERE vbeln = @_fat-vgbel.

  CHECK ( sy-subrc EQ 0 ).

  "Get Parceiro Z1
  SELECT SINGLE *
    FROM vbpa INTO @DATA(_parc)
   WHERE vbeln = @_rem-vbeln
     AND parvw = 'Z1'.

  CHECK ( sy-subrc EQ 0 ).

  p_terminal_rfl = _parc-lifnr.

  "Check Carta Correção Terminal
  CLEAR: wa_ultima_carta, it_carta_correcao[].

  SELECT *
    FROM zcarta_correcao INTO TABLE it_carta_correcao
   WHERE docnum        EQ p_docnum_rfl
     AND novo_terminal NE space.

  CHECK it_carta_correcao[] IS NOT INITIAL.

  LOOP AT it_carta_correcao INTO DATA(wa_carta_correcao).
    IF wa_ultima_carta IS INITIAL.
      wa_ultima_carta = wa_carta_correcao.
    ELSEIF wa_ultima_carta-id_cc LT wa_carta_correcao-id_cc.
      wa_ultima_carta = wa_carta_correcao.
    ENDIF.
  ENDLOOP.

  "Possui Carta de Correção
  IF ( wa_ultima_carta IS NOT INITIAL ) AND ( wa_ultima_carta-novo_terminal IS NOT INITIAL ).
    "Novo Terminal (parceiro Z1)
    p_terminal_rfl = wa_ultima_carta-novo_terminal.
  ENDIF.

ENDFORM.

FORM f_get_j_1bnfe_active .

  CHECK tg_j_1bnfdoc[] IS NOT INITIAL.

  SELECT *
    FROM j_1bnfe_active INTO TABLE tg_j_1bnfe_active
     FOR ALL ENTRIES IN tg_j_1bnfdoc
   WHERE docnum = tg_j_1bnfdoc-docnum.

ENDFORM.

FORM f_set_chave_doc.

  LOOP AT tg_j_1bnfdoc.

    READ TABLE tg_j_1bnfe_active WITH KEY docnum = tg_j_1bnfdoc-docnum.

    CHECK sy-subrc EQ 0.

    CONCATENATE tg_j_1bnfe_active-regio   "Região do emissor NF-e
                tg_j_1bnfe_active-nfyear  "Ano da data do documento da NF-e
                tg_j_1bnfe_active-nfmonth "Mês da data do documento da NF-e
                tg_j_1bnfe_active-stcd1   "Nº CNPJ do emissor da NF-e
                tg_j_1bnfe_active-model   "Modelo da nota fiscal
                tg_j_1bnfe_active-serie   "SERIE
                tg_j_1bnfe_active-nfnum9  "Nº NF-e de nove posições
                tg_j_1bnfe_active-docnum9 "NF-e: nº aleatório
                tg_j_1bnfe_active-cdv     "Dígito controle p/chave de acesso NF-e
           INTO tg_j_1bnfdoc-chave_nfe.

    IF strlen( tg_j_1bnfdoc-chave_nfe ) NE 44.
      CLEAR: tg_j_1bnfdoc-chave_nfe.
    ENDIF.

    MODIFY tg_j_1bnfdoc.
  ENDLOOP.
  "" AHSS - Chamado 140376 - 18/06/2024 - Ajuste para filtrar pela chave
  IF p_knf IS NOT INITIAL.

    DELETE tg_j_1bnfdoc WHERE chave_nfe NOT IN p_knf.

  ENDIF.

ENDFORM.

FORM f_get_zib_nfe_dist_itm .

  tg_j_1bnfdoc_aux[] = tg_j_1bnfdoc[].
  DELETE tg_j_1bnfdoc_aux WHERE chave_nfe IS INITIAL.

  CHECK tg_j_1bnfdoc_aux[] IS NOT INITIAL.

  SELECT *
    FROM zib_nfe_dist_itm INTO TABLE tg_zib_nfe_dist_itm
     FOR ALL ENTRIES IN tg_j_1bnfdoc_aux
   WHERE chave_nfe EQ tg_j_1bnfdoc_aux-chave_nfe.

ENDFORM.

FORM f_consulta_notas_cct.

  DATA: it_nfe_cons TYPE zde_chave_doc_e_t,
        wl_nfe_cons TYPE zde_chave_doc_e.

  CHECK p_srvcct IS NOT INITIAL.

  CLEAR: it_nfe_cons[].

  LOOP AT it_saida_0100 INTO wa_saida_0100.

    IF ( wa_saida_0100-dt_recepcao_portal_rfl IS INITIAL  AND  wa_saida_0100-dt_recepcao_portal IS INITIAL ).

      IF wa_saida_0100-chave_nfe IS NOT INITIAL.
        wl_nfe_cons = wa_saida_0100-chave_nfe.
        APPEND wl_nfe_cons TO it_nfe_cons.
      ENDIF.

      IF wa_saida_0100-chave_nfe_rfl IS NOT INITIAL.
        wl_nfe_cons = wa_saida_0100-chave_nfe_rfl.
        APPEND wl_nfe_cons TO it_nfe_cons.
      ENDIF.

    ENDIF.

  ENDLOOP.

  CHECK it_nfe_cons[] IS NOT INITIAL.

  CALL FUNCTION 'ZCCT_CONFIRMA_REC_NF_PORTAL'
    EXPORTING
      i_job    = p_report
      i_chaves = it_nfe_cons.

ENDFORM.

FORM f_atrib_conf_cct_portal .

  it_saida_0100_aux[] = it_saida_0100[].
  DELETE it_saida_0100_aux WHERE chave_nfe IS INITIAL.

  IF it_saida_0100_aux[] IS NOT INITIAL.
    SELECT *
      FROM zlest0186 APPENDING TABLE tg_zlest0186
       FOR ALL ENTRIES IN it_saida_0100_aux
     WHERE chave = it_saida_0100_aux-chave_nfe.
  ENDIF.

  it_saida_0100_aux[] = it_saida_0100[].
  DELETE it_saida_0100_aux WHERE chave_nfe_ref IS INITIAL.

  IF it_saida_0100_aux[] IS NOT INITIAL.
    SELECT *
      FROM zlest0186 APPENDING TABLE tg_zlest0186
       FOR ALL ENTRIES IN it_saida_0100_aux
     WHERE chave = it_saida_0100_aux-chave_nfe_ref.
  ENDIF.

  it_saida_0100_aux[] = it_saida_0100[].
  DELETE it_saida_0100_aux WHERE chave_nfe_rfl IS INITIAL.

  IF it_saida_0100_aux[] IS NOT INITIAL.
    SELECT *
      FROM zlest0186 APPENDING TABLE tg_zlest0186
       FOR ALL ENTRIES IN it_saida_0100_aux
     WHERE chave = it_saida_0100_aux-chave_nfe_rfl.
  ENDIF.

  SORT tg_zlest0186 BY chave.
  DELETE ADJACENT DUPLICATES FROM tg_zlest0186 COMPARING chave.

  CHECK tg_zlest0186[] IS NOT INITIAL.

  SELECT *
    FROM zsdt0168 APPENDING TABLE tg_zsdt0168
     FOR ALL ENTRIES IN tg_zlest0186
   WHERE codigo_ra EQ tg_zlest0186-codigo_ra.

  IF tg_zsdt0168[] IS NOT INITIAL.
    SELECT *
      FROM lfa1 APPENDING TABLE tg_lfa1
       FOR ALL ENTRIES IN tg_zsdt0168
     WHERE lifnr EQ tg_zsdt0168-lifnr.
  ENDIF.

  SORT tg_lfa1 BY lifnr.
  DELETE ADJACENT DUPLICATES FROM tg_lfa1 COMPARING lifnr.

  LOOP AT it_saida_0100 ASSIGNING FIELD-SYMBOL(<fs_saida_0100>).

    READ TABLE tg_zlest0186 WITH KEY chave = <fs_saida_0100>-chave_nfe.

    IF ( sy-subrc NE 0 ) AND ( <fs_saida_0100>-chave_nfe_ref IS NOT INITIAL ).
      READ TABLE tg_zlest0186 WITH KEY chave = <fs_saida_0100>-chave_nfe_ref.
    ENDIF.

    IF ( sy-subrc EQ 0 ) AND ( tg_zlest0186-chave IS NOT INITIAL ).
      <fs_saida_0100>-conf_cct_portal    = icon_okay.
      <fs_saida_0100>-dt_recepcao_portal = tg_zlest0186-dt_recepcao.

      READ TABLE tg_zsdt0168 WITH KEY codigo_ra = tg_zlest0186-codigo_ra.
      IF sy-subrc EQ 0.
        <fs_saida_0100>-term_cct_portal   = tg_zsdt0168-lifnr.

        READ TABLE tg_lfa1 WITH KEY lifnr = tg_zsdt0168-lifnr.
        IF sy-subrc EQ 0.
          <fs_saida_0100>-ds_term_cct_portal = tg_lfa1-name1.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE tg_zlest0186 WITH KEY chave = <fs_saida_0100>-chave_nfe_rfl.

    IF ( sy-subrc EQ 0 ) AND ( <fs_saida_0100>-chave_nfe_rfl IS NOT INITIAL ).
      <fs_saida_0100>-conf_cct_portal_rfl    = icon_okay.
      <fs_saida_0100>-dt_recepcao_portal_rfl = tg_zlest0186-dt_recepcao.

      READ TABLE tg_zsdt0168 WITH KEY codigo_ra = tg_zlest0186-codigo_ra.
      IF sy-subrc EQ 0.
        <fs_saida_0100>-term_cct_portal_rfl   = tg_zsdt0168-lifnr.

        READ TABLE tg_lfa1 WITH KEY lifnr = tg_zsdt0168-lifnr.
        IF sy-subrc EQ 0.
          <fs_saida_0100>-ds_term_cct_portal_rfl = tg_lfa1-name1.
        ENDIF.
      ENDIF.
    ENDIF.


  ENDLOOP.

ENDFORM.

FORM f_fill_color_saida  USING p_cor CHANGING p_saida_0100 TYPE ty_saida_0100.

  DATA: wl_color  TYPE kkblo_specialcol.

  FREE p_saida_0100-color.

  CLEAR: wl_color.
  wl_color-fieldname = 'DOCNUM_RFL'.
  wl_color-color-col = p_cor.
  wl_color-color-int = 0.
  wl_color-color-inv = 0.
  APPEND wl_color TO p_saida_0100-color.

  wl_color-fieldname = 'NFENUM_RFL'.             APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'ID_VINC'.                APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'CCT_RFL'.                APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'PESO_CCT_RFL'.           APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'DT_CCT_RFL'.             APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'TERM_CCT_RFL'.           APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'DS_TERM_CCT_RFL'.        APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'TERMINAL_RFL'.           APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'QTD_NF_RFL'.             APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'DS_TERMINAL_RFL'.        APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'CHAVE_NFE_RFL'.          APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'CONF_CCT_PORTAL_RFL'.    APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'DT_RECEPCAO_PORTAL_RFL'. APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'TERM_CCT_PORTAL_RFL'.    APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'DS_TERM_CCT_PORTAL_RFL'. APPEND wl_color TO p_saida_0100-color.
  wl_color-fieldname = 'SALDO_EXPORTAR_RFL'.     APPEND wl_color TO p_saida_0100-color.


  IF p_saida_0100-docnum_ligacao IS NOT INITIAL AND
     p_saida_0100-docnum NE p_saida_0100-docnum_ligacao.

    FREE p_saida_0100-color.
*"// Cor Verde
    LOOP AT it_fcat INTO DATA(ls_fcat).
      APPEND
      VALUE #(
               fieldname = ls_fcat-fieldname
               color-col = 5
               color-int = 1
               color-inv = 1
             ) TO p_saida_0100-color.
    ENDLOOP.

    p_saida_0100-seq_doc = |{ p_saida_0100-seq_doc }_1|.

  ENDIF.

  IF p_saida_0100-cancel_rfl IS NOT INITIAL.

    FREE p_saida_0100-color.
*"// Cor Vermelha
    LOOP AT it_fcat INTO ls_fcat.
      APPEND
      VALUE #(
               fieldname = ls_fcat-fieldname
               color-col = 6
               color-int = 0
               color-inv = 0
             ) TO p_saida_0100-color.
    ENDLOOP.

    p_saida_0100-seq_doc = |{ p_saida_0100-seq_doc }_0|.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_NFE_EXPORTACAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_nfe_exportacao .
  CHECK tg_znom_reme_notas[] IS NOT INITIAL.

* Nota Fiscal do Produtor Vinculadas na Nota de Exportação
  SELECT * FROM zdoc_nf_produtor
    INTO TABLE tg_nf_produtor
    FOR ALL ENTRIES IN tg_znom_reme_notas
    WHERE docnum_prod      = tg_znom_reme_notas-docnum
      AND itmnum_prod      = tg_znom_reme_notas-itmnum
      AND id_nomeacao_tran = tg_znom_reme_notas-id_nomeacao_tran
      AND grp_retorno      = tg_znom_reme_notas-grp_retorno.

* Fluxo de documentos de vendas e distribuição
  IF tg_nf_produtor[] IS NOT INITIAL.

* Fluxo de documentos de vendas e distribuição
    SELECT vbelv vbeln posnn FROM vbfa
          INTO TABLE tg_vbfa
          FOR ALL ENTRIES IN tg_nf_produtor
          WHERE vbelv   = tg_nf_produtor-vbeln
            AND vbtyp_n = 'M'.

  ENDIF.

* Partidas individuais da nota fiscal
  IF tg_vbfa[] IS NOT INITIAL.

* Item nota fiscal
    SELECT docnum refkey refitm FROM j_1bnflin
      INTO TABLE tg_lin
      FOR ALL ENTRIES IN tg_vbfa
      WHERE refkey = tg_vbfa-vbeln
        AND refitm = tg_vbfa-posnn.

  ENDIF.

  IF tg_lin[] IS NOT INITIAL.

* cabeçalho nota fiscal
    SELECT docnum cancel FROM j_1bnfdoc
      INTO TABLE tg_nfdoc
      FOR ALL ENTRIES IN tg_lin
      WHERE docnum = tg_lin-docnum
        AND cancel = space.

  ENDIF.

* Electronic Nota Fiscal: Actual Status
  IF tg_lin[] IS NOT INITIAL.

    SELECT docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv FROM j_1bnfe_active
      INTO TABLE tg_active
      FOR ALL ENTRIES IN tg_nfdoc
      WHERE docnum = tg_nfdoc-docnum.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_NFE_EXPORTACAO_ALGODAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_nfe_exportacao_algodao.
  CHECK tg_znom_reme_notas[] IS NOT INITIAL.

* Nota Fiscal do Produtor Vinculadas na Nota de Exportação
  SELECT * FROM zdoc_nf_produtor
    INTO TABLE tg_nf_produtor_algodao
    FOR ALL ENTRIES IN tg_znom_reme_notas
    WHERE docnum_prod      = tg_znom_reme_notas-docnum
      AND itmnum_prod      = tg_znom_reme_notas-itmnum.

* Fluxo de documentos de vendas e distribuição
  IF tg_nf_produtor_algodao[] IS NOT INITIAL.

* Fluxo de documentos de vendas e distribuição
    SELECT vbelv vbeln posnn FROM vbfa
          APPENDING TABLE tg_vbfa
          FOR ALL ENTRIES IN tg_nf_produtor_algodao
          WHERE vbelv   = tg_nf_produtor_algodao-vbeln
            AND vbtyp_n = 'M'.

  ENDIF.

* Partidas individuais da nota fiscal
  IF tg_vbfa[] IS NOT INITIAL.

* Item nota fiscal
    SELECT docnum refkey refitm FROM j_1bnflin
      APPENDING TABLE tg_lin
      FOR ALL ENTRIES IN tg_vbfa
      WHERE refkey = tg_vbfa-vbeln
        AND refitm = tg_vbfa-posnn.

  ENDIF.

  IF tg_lin[] IS NOT INITIAL.

* cabeçalho nota fiscal
    SELECT docnum cancel FROM j_1bnfdoc
      APPENDING TABLE tg_nfdoc
      FOR ALL ENTRIES IN tg_lin
      WHERE docnum = tg_lin-docnum
        AND cancel = space.

  ENDIF.

* Electronic Nota Fiscal: Actual Status
  IF tg_lin[] IS NOT INITIAL.

    SELECT docnum regio nfyear nfmonth stcd1 model serie nfnum9 docnum9 cdv FROM j_1bnfe_active
      APPENDING TABLE tg_active
      FOR ALL ENTRIES IN tg_nfdoc
      WHERE docnum = tg_nfdoc-docnum.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_USER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_check_user .

  CLEAR user_full .

*-------------------------------------------------
* Usuarios ALV Full
*-------------------------------------------------
  CALL FUNCTION 'G_SET_GET_ALL_VALUES'
    EXPORTING
      setnr           = 'ZSDT0149_USUARIO'
      class           = '0000'
      no_descriptions = ''
    TABLES
      set_values      = t_value
    EXCEPTIONS
      set_not_found   = 1
      OTHERS          = 2.

  IF line_exists( t_value[ from = sy-uname ] ).
    user_full = abap_true.
  ENDIF.

  t_field =
  VALUE #(
          ( fieldname = 'CFOP' )
          ( fieldname = 'NBM' )
          ( fieldname = 'CHAVE_NFE' )
          ( fieldname = 'UND_TRIB_XML' )
          ( fieldname = 'NCM_XML' )
          ( fieldname = 'CFOP_XML' )
          ( fieldname = 'MEINS' )
          ( fieldname = 'MENGE' )
          ( fieldname = 'QCOM_XML' )
          ( fieldname = 'NUMERO_DUE' )
          ( fieldname = 'DT_DUE' )
          ( fieldname = 'NFENUM_EXP' )
          ( fieldname = 'FATURA_ID' )
          ( fieldname = 'QTDE_VINC_DUE' )
          ( fieldname = 'SALDO_EXPORTAR' )
          ( fieldname = 'SALDO_DEVOLUCAO' ) "US #131067 - MMSILVA - 10.07.2025
         ).


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_INF_CTE_RFL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_SAIDA_0100  text
*----------------------------------------------------------------------*
FORM f_busca_inf_cte_rfl  CHANGING p_saida_0100  TYPE ty_saida_0100.

  DATA: it_zsdt0001 TYPE TABLE OF zsdt0001,
        wa_zsdt0001 TYPE zsdt0001.

  SELECT SINGLE obj_key  FROM zmmt_ee_zgr_docs
    INTO @DATA(v_obj_key)
    WHERE docnum   EQ @p_saida_0100-docnum.

  CHECK v_obj_key IS NOT INITIAL.

  SELECT SINGLE ch_referencia  FROM zmmt_ee_zgr
    INTO @DATA(v_ch_referencia)
    WHERE obj_key   EQ @v_obj_key.

  CHECK v_ch_referencia IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_zsdt0001_aux)
    FROM zsdt0001
   WHERE ch_referencia EQ @v_ch_referencia
    AND tp_movimento = 'E'.

  FREE: it_zsdt0001.
  LOOP AT it_zsdt0001_aux INTO DATA(wa_zsdt0001_aux).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zsdt0001_aux-nr_romaneio
      IMPORTING
        output = wa_zsdt0001-id_referencia.

    wa_zsdt0001-branch = wa_zsdt0001_aux-branch.
    wa_zsdt0001-nr_safra  = wa_zsdt0001_aux-nr_safra.

    APPEND wa_zsdt0001 TO it_zsdt0001.
    CLEAR: wa_zsdt0001 .

  ENDLOOP.

  CHECK it_zsdt0001[] IS NOT INITIAL.

  SELECT * INTO TABLE @DATA(it_zsdt0001_doc)
     FROM zsdt0001
      FOR ALL ENTRIES IN @it_zsdt0001
    WHERE tp_movimento = 'S'
      AND branch = @it_zsdt0001-branch
      AND nr_safra = @it_zsdt0001-nr_safra
      AND id_referencia = @it_zsdt0001-id_referencia.

  READ TABLE it_zsdt0001_doc INTO DATA(wa_zsdt0001_doc) INDEX 1.

  IF sy-subrc IS INITIAL AND wa_zsdt0001_doc-doc_transp IS NOT INITIAL.
    MOVE icon_okay TO p_saida_0100-icone.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_QNT_BAIXADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_get_qnt_baixada .

  CHECK tg_j_1bnfdoc[] IS NOT INITIAL.

  FREE: tg_zsdt0276_tot.

  SELECT *
    FROM zsdt0276 INTO TABLE tg_zsdt0276
     FOR ALL ENTRIES IN tg_j_1bnfdoc
   WHERE docnum  = tg_j_1bnfdoc-docnum
     AND itmnum  = tg_j_1bnfdoc-itmnum
     AND baixar  = 'X'
     AND status IN ('','A').

  LOOP AT tg_zsdt0276.
    MOVE-CORRESPONDING tg_zsdt0276 TO tg_zsdt0276_tot.
    COLLECT tg_zsdt0276_tot.
  ENDLOOP.

  SORT tg_zsdt0276_tot BY docnum itmnum.

*  IF  tg_zsdt0276[] IS NOT INITIAL.
*    tg_zsdt0276_aux[] = tg_zsdt0276[].
*    SORT tg_zsdt0276_aux BY docnum.
*    DELETE ADJACENT DUPLICATES FROM tg_zsdt0276_aux COMPARING docnum.
*
*    LOOP AT tg_zsdt0276_aux.
*      LOOP AT tg_zsdt0276 WHERE docnum EQ tg_zsdt0276_aux-docnum.
*        tg_zsdt0276_tot-menge = tg_zsdt0276_tot-menge + tg_zsdt0276-menge.
*      ENDLOOP.
*      tg_zsdt0276_tot-docnum = tg_zsdt0276_aux-docnum.
*      APPEND tg_zsdt0276_tot.
*    ENDLOOP.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
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
*&---------------------------------------------------------------------*
*&      Form  F_AJUSTA_TOTAIS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ajusta_totais.

  DATA: lref_data TYPE REF TO data.
  DATA: vl_contsum TYPE i,
        docnum     TYPE j_1bnflin-docnum,
        menge      TYPE j_1bnflin-menge.

  FIELD-SYMBOLS: <l_sum_tab> TYPE table,
                 <l_sum>     TYPE  ty_saida_0100.


  "Ajuste de valores da linha de TOTAIS
  CALL METHOD obj_alv_0100->get_subtotals
    IMPORTING
      ep_collect00 = lref_data.

  ASSIGN lref_data->* TO <l_sum_tab>.
  IF <l_sum_tab> IS ASSIGNED.
    READ TABLE <l_sum_tab> ASSIGNING <l_sum> INDEX 1.
    IF sy-subrc EQ 0.
      CLEAR: <l_sum>.
      SORT it_saida_0100 BY docnum.
      CLEAR: docnum, menge.
      LOOP AT it_saida_0100 INTO DATA(wa_saida).
        vl_contsum = vl_contsum + 1.

        IF docnum EQ wa_saida-docnum.
          IF menge IS INITIAL.
            menge = wa_saida-menge.
          ELSE.
            menge = menge + wa_saida-menge.
          ENDIF.
        ENDIF.
        docnum = wa_saida-docnum.

        IF wa_saida-menge IS NOT INITIAL.
          IF <l_sum>-menge IS INITIAL.
            <l_sum>-menge = wa_saida-menge.
          ELSE.
            <l_sum>-menge = <l_sum>-menge + wa_saida-menge.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF <l_sum>-menge IS NOT INITIAL.
        <l_sum>-menge = <l_sum>-menge - menge.
      ENDIF.

      CLEAR: vl_contsum.

      CALL METHOD obj_alv_0100->refresh_table_display
        EXPORTING
          i_soft_refresh = 'X'.

    ENDIF.
  ENDIF.

  CALL METHOD obj_alv_0100->get_frontend_layout
    IMPORTING
      es_layout = gs_layout.

  gs_layout-cwidth_opt = 'X'.

  CALL METHOD obj_alv_0100->set_frontend_layout
    EXPORTING
      is_layout = gs_layout.

  CALL METHOD obj_alv_0100->refresh_table_display
    EXPORTING
      i_soft_refresh = 'X'.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_get_vinc_f_lote
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_get_vinc_f_lote .

  SELECT *
    FROM dd07t
    INTO TABLE gt_dd07t
  WHERE  domname    EQ 'ZFIN_EXPORT_D'
    AND  ddlanguage EQ sy-langu.

  FREE r_docnum.

  r_docnum = VALUE #(
         FOR ws_doc IN tg_j_1bnfdoc (
             sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
             option = zcl_les_utils=>if_stab_constants~mc_option_equal
             low    = ws_doc-docnum ) ).

  IF r_docnum IS NOT INITIAL.
    zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo(
      EXPORTING
        i_docnum        = CONV #( r_docnum )
      RECEIVING
        r_formacao_lote = gt_flote_vinc
    ).
  ENDIF.

  DELETE gt_flote_vinc-zsdtprod_flote WHERE cancel IS NOT INITIAL.
  CHECK gt_flote_vinc-zsdtvinc_p_flote IS NOT INITIAL.

  SELECT *
    FROM zsdt_docnum_log
  INTO TABLE gt_docnum_log
  FOR ALL ENTRIES IN gt_flote_vinc-zsdtvinc_p_flote
    WHERE docnum_prod_vinc_xml EQ gt_flote_vinc-zsdtvinc_p_flote-docnum_eprod
      AND cancel EQ abap_false.

  FREE r_docnum.
  r_docnum = VALUE #(
           FOR ws_log IN gt_docnum_log (
               sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
               option = zcl_les_utils=>if_stab_constants~mc_option_equal
               low    = ws_log-docnum_eprod ) ).

  IF r_docnum IS NOT INITIAL.

    SELECT *
        APPENDING CORRESPONDING FIELDS OF TABLE tg_j_1bnfdoc
       FROM j_1bnfdoc AS dc
        INNER JOIN j_1bnflin AS li ON li~docnum EQ dc~docnum
      WHERE dc~docnum IN r_docnum.

    SORT tg_j_1bnfdoc BY docnum itmnum.
    DELETE ADJACENT DUPLICATES FROM tg_j_1bnfdoc COMPARING docnum itmnum.

    FREE r_docnum.
    r_docnum = VALUE #(
             FOR ws_doc IN tg_j_1bnfdoc (
                 sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
                 option = zcl_les_utils=>if_stab_constants~mc_option_equal
                 low    = ws_doc-docnum ) ).

    IF r_docnum IS NOT INITIAL.
      zcl_im_cl_fluxo_exportacao=>check_nfe_fila_vinculo(
        EXPORTING
          i_docnum        = CONV #( r_docnum )
        RECEIVING
          r_formacao_lote = gt_flote_vinc_log
      ).
    ENDIF.

    LOOP AT gt_flote_vinc_log-zsdtvinc_p_flote ASSIGNING FIELD-SYMBOL(<fs_zsdtvinc_p_flote>).
      READ TABLE gt_docnum_log INTO DATA(ls_docnum_log)
          WITH KEY docnum_flote = <fs_zsdtvinc_p_flote>-docnum_flote
                   docnum_eprod = <fs_zsdtvinc_p_flote>-docnum_eprod.
      IF sy-subrc IS NOT INITIAL.
        CLEAR: <fs_zsdtvinc_p_flote>-docnum_flote,
               <fs_zsdtvinc_p_flote>-docnum_eprod.
      ENDIF.

    ENDLOOP.

    DELETE gt_flote_vinc_log-zsdtvinc_p_flote WHERE docnum_flote IS INITIAL AND docnum_eprod IS INITIAL.

  ENDIF.

  IF gt_flote_vinc_log-zsdtvinc_p_flote IS NOT INITIAL.
    APPEND LINES OF gt_flote_vinc_log-zsdtvinc_p_flote TO gt_flote_vinc-zsdtvinc_p_flote.
  ENDIF.

  SELECT docnum nfenum werks nf_retorno docnum_ret quant_vinc data_criacao id_export
    FROM zsdt_retlote
  INTO TABLE gt_retlote
  FOR ALL ENTRIES IN gt_flote_vinc-zsdtvinc_p_flote
    WHERE docnum EQ gt_flote_vinc-zsdtvinc_p_flote-docnum_flote.

  IF gt_retlote IS NOT INITIAL.

    SELECT *
      FROM zsdt_export
      INTO TABLE gt_export
      FOR ALL ENTRIES IN gt_retlote
      WHERE docnum EQ gt_retlote-docnum_ret.

  ENDIF.

  SORT gt_retlote BY docnum ASCENDING.

* Soma Quantidades
  PERFORM z_soma_qtd.


  FREE r_docnum.
  r_docnum_aux = VALUE #(
       FOR ws_ret IN gt_retlote (
           sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
           option = zcl_les_utils=>if_stab_constants~mc_option_equal
           low    = ws_ret-docnum_ret ) ).

  APPEND LINES OF r_docnum_aux TO r_docnum.

  r_docnum_aux = VALUE #(
     FOR ws_ret IN gt_retlote (
         sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
         option = zcl_les_utils=>if_stab_constants~mc_option_equal
         low    = ws_ret-docnum ) ).

  APPEND LINES OF r_docnum_aux TO r_docnum.

  r_docnum_aux = VALUE #(
     FOR ws_flot IN gt_flote_vinc-zsdtvinc_p_flote (
         sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
         option = zcl_les_utils=>if_stab_constants~mc_option_equal
         low    = ws_flot-docnum_flote ) ).

  APPEND LINES OF r_docnum_aux TO r_docnum.

  r_docnum_aux = VALUE #(
     FOR ws_flot IN gt_flote_vinc-zsdtvinc_p_flote (
         sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
         option = zcl_les_utils=>if_stab_constants~mc_option_equal
         low    = ws_flot-docnum_eprod ) ).

  APPEND LINES OF r_docnum_aux TO r_docnum.

  FREE r_docnum_aux.

  CHECK r_docnum IS NOT INITIAL.

  " Inicio - Rubenilson - 01.10.2025 #192188
  DATA: lr_docnum    TYPE RANGE OF j_1bnfdoc-docnum,
        lr_docnum_rt TYPE RANGE OF znom_remetente-docnum_rt.

  MOVE-CORRESPONDING r_docnum TO lr_docnum.
  MOVE-CORRESPONDING r_docnum TO lr_docnum_rt.
  " Fim - Rubenilson - 01.10.2025 #192188

  SELECT *
    INTO CORRESPONDING FIELDS OF TABLE gt_doc_ret
   FROM j_1bnfdoc AS dc
    INNER JOIN j_1bnflin AS li ON li~docnum EQ dc~docnum
    FOR ALL ENTRIES IN lr_docnum     " Rubenilson - 01.10.2025 #192188
    WHERE dc~docnum = lr_docnum-low. " Rubenilson - 01.10.2025 #192188
*  WHERE dc~docnum IN r_docnum.      " Rubenilson - 01.10.2025 #192188

  SELECT *
    FROM j_1bnfe_active
    INTO TABLE gt_active_ret
    FOR ALL ENTRIES IN lr_docnum  " Rubenilson - 01.10.2025 #192188
*    WHERE docnum IN r_docnum.    " Rubenilson - 01.10.2025 #192188
    WHERE docnum = lr_docnum-low. " Rubenilson - 01.10.2025 #192188

  SELECT *
    FROM znom_remetente
    INTO TABLE gt_nom_remetente
    FOR ALL ENTRIES IN lr_docnum_rt " Rubenilson - 01.10.2025 #192188
*    WHERE docnum_rt IN r_docnum.   " Rubenilson - 01.10.2025 #192188
    WHERE docnum_rt = lr_docnum_rt-low. " Rubenilson - 01.10.2025 #192188

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_eudr
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_check_eudr .

*"// wbarbosa 18112024 US-158263

  FREE r_eudr.
  APPEND VALUE #( sign   = 'I'
                  option = 'EQ'
                  low    = zcl_eudr_utils=>lc_s_eudr ) TO r_eudr.

  r_docnum = VALUE #(
        FOR ws_doc IN tg_j_1bnfdoc (
            sign   = zcl_les_utils=>if_stab_constants~mc_sign_include
            option = zcl_les_utils=>if_stab_constants~mc_option_equal
            low    = ws_doc-docnum ) ).

  zcl_eudr_utils=>check_doc_fiscal_eudr(
    EXPORTING
      i_docnum_t    = r_docnum
    IMPORTING
      e_docnum_eudr = lt_docnum_eudr ).

  IF p_eudr IS NOT INITIAL.

    DELETE lt_docnum_eudr WHERE eudr NOT IN r_eudr.

    FREE r_docnum.
    r_docnum =  VALUE #( FOR ws_docnum IN lt_docnum_eudr (
                           sign = 'I'
                           option = 'EQ'
                           low    = ws_docnum-docnum ) ).

    IF r_docnum IS NOT INITIAL.
      DELETE tg_j_1bnfdoc WHERE docnum NOT IN r_docnum.
    ELSE.
      FREE tg_j_1bnfdoc.
    ENDIF.

  ENDIF.
*"// wbarbosa 18112024 US-158263

ENDFORM.
*&---------------------------------------------------------------------*
*& Form z_soma_qtd
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM z_soma_qtd .

  FREE gt_sumret.

  LOOP AT gt_retlote INTO DATA(ls_retlote).
    gs_sumret-docnum     = ls_retlote-docnum.
    gs_sumret-quant_vinc = ls_retlote-quant_vinc.
    COLLECT gs_sumret INTO gt_sumret.
    CLEAR: ls_retlote, gs_sumret .
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_color_sequencia
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_color_sequencia.

  DATA(lt_saida_0100) = it_saida_0100.

  DELETE lt_saida_0100 WHERE docnum_ligacao IS INITIAL.
  DELETE it_saida_0100 WHERE docnum_ligacao IS NOT INITIAL.

  LOOP AT it_saida_0100 ASSIGNING FIELD-SYMBOL(<fs_saida_0100>).
    PERFORM f_fill_color_saida USING 3 CHANGING <fs_saida_0100>.
  ENDLOOP.
  SORT it_saida_0100 BY bukrs branch docnum_ligacao docnum id_due.

  LOOP AT lt_saida_0100 ASSIGNING <fs_saida_0100>.
    PERFORM f_fill_color_saida USING 3 CHANGING <fs_saida_0100>.
  ENDLOOP.
  SORT lt_saida_0100 BY seq_doc.

  APPEND LINES OF lt_saida_0100 TO it_saida_0100.

ENDFORM.

* US #179396 - MMSILVA - 20.05.2025 - Inicio
FORM f_alv_variant_f4 CHANGING pa_vari.

  rs_variant-report   = sy-repid.
  rs_variant-username = sy-uname.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = rs_variant
      i_save     = 'A'
    IMPORTING
      es_variant = rs_variant
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc = 0.
    p_layout = rs_variant-variant.
  ENDIF.

ENDFORM.
* US #179396 - MMSILVA - 20.05.2025 - Fim

*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Início de Alteração
FORM append_it_saida_0100 USING i_w_saida_0100 TYPE ty_saida_0100.

  DATA:
        lw_saida_0100 TYPE ty_saida_0100.

  lw_saida_0100 = i_w_saida_0100.

  IF lw_saida_0100-cancel_rfl IS NOT INITIAL.

    CLEAR: lw_saida_0100-menge,
           lw_saida_0100-saldo_exportar,
           lw_saida_0100-saldo_exportar_rfl,
           lw_saida_0100-qcom_xml.

  ENDIF.

  APPEND lw_saida_0100 TO it_saida_0100.

ENDFORM.
*** Stefanini - IR241122 - 10/06/2025 - LAZAROSR - Fim de Alteração

*** US #131067 - MMSILVA - 09.07.2025 - Ini ***
FORM f_consultar_devolucao_sigam USING wa_saida_devolucao TYPE zsdt_doc_devolucao_sigam
                                 CHANGING wa_saida_result TYPE zsdt_doc_devolucao_sigam.

  DATA: wa_consulta_nfe_sigam       TYPE zfie_consultar_nfe_sigam,
        wa_resultado_cons_nfe_sigam TYPE zfie_result_cons_nfe_sigam,
        it_resultado_cons_nfe_sigam TYPE TABLE OF zfie_result_cons_nfe_sigam.

  "US #131067 - SMCABANA - 22-08-2025
  "Adicione no início do PERFORM
  DATA: lv_simular_erro TYPE abap_bool.
  "US #131067 - SMCABANA - 22-08-2025
  DATA: lt_docnum TYPE string_table.

  DESCRIBE TABLE wa_saida_devolucao LINES DATA(lv_lines_tot).

  LOOP AT wa_saida_devolucao ASSIGNING FIELD-SYMBOL(<fs_saida_dev>).

    APPEND INITIAL LINE TO lt_docnum ASSIGNING FIELD-SYMBOL(<fs_docnum>).
    <fs_docnum> = <fs_saida_dev>-docnum.

    DESCRIBE TABLE lt_docnum LINES DATA(lv_lines).
    IF lv_lines >= 3800 OR lv_lines = lv_lines_tot.

      lv_lines_tot = lv_lines_tot - lv_lines.

      wa_consulta_nfe_sigam = VALUE #( numerodocumentosaplista = lt_docnum
                                       item               = 'true'
                                       imposto            = 'true'
                                       devolucao          = 'true' ).

      FREE: lt_docnum.

      "US #131067 - SMCABANA - 22-08-2025 - "SIMULAÇÃO DE ERRO - APENAS PARA TESTE
      "Defina quando simular
*  lv_simular_erro = COND #( WHEN sy-uname = 'SMCABANA' AND sy-sysid = 'QAS' THEN abap_true ELSE abap_false ).
      "US #131067 - SMCABANA - 22-08-2025 - "SIMULAÇÃO DE ERRO - APENAS PARA TESTE

      TRY .
          "Chama API para consultar o documento
          zcl_int_ob_dados_nf_sigam=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = wa_consulta_nfe_sigam IMPORTING e_integracao = DATA(r_response) ).

          "US #131067 - SMCABANA - 22-08-2025
          "SIMULAÇÃO DE ERRO - APENAS PARA TESTE
*      IF lv_simular_erro = abap_true.
*        BREAK-POINT.
*        RAISE EXCEPTION TYPE zcx_integracao.
*      ENDIF.
          "US #131067 - SMCABANA - 22-08-2025

          IF r_response-nm_code EQ '0200'.
            CALL METHOD /ui2/cl_json=>deserialize
              EXPORTING
                json = r_response-ds_data_retorno
              CHANGING
                data = wa_resultado_cons_nfe_sigam.

            IF wa_resultado_cons_nfe_sigam IS NOT INITIAL.
              LOOP AT wa_resultado_cons_nfe_sigam-data INTO DATA(ls_data).
                DATA(lv_tabix) = sy-tabix.
                IF ls_data-devolucao IS NOT INITIAL.
                  LOOP AT ls_data-devolucao INTO DATA(ls_devolucao).

                    READ TABLE wa_saida_devolucao ASSIGNING FIELD-SYMBOL(<fs_saida_devolucao>)
                    WITH KEY docnum = ls_data-numerodocumentosap.
                    IF sy-subrc IS INITIAL.
                      <fs_saida_devolucao>-valor = ls_devolucao-quantidade.
                    ENDIF.

*              "US #131067 - SMCABANA     - 16-09-2025
*               ESSA PARTE FOI COMENTADA TEMPORARIAMENTE POIS PASSARÁ POR ANALISE DA ÁREA PARA AJUSTES DO PROCESSO INTERNO - FAVOR NAO DELETAR ESSE TRECHO DO CODIGO
*                wa_saida_result-saldo_exportar = wa_saida_result-saldo_exportar - wa_saida_result-saldo_devolucao.
*                  IF wa_saida_result-saldo_exportar < 0.
*                      wa_saida_result-saldo_exportar = 0.
*                  ENDIF.
*               ESSA PARTE FOI COMENTADA TEMPORARIAMENTE POIS PASSARÁ POR ANALISE DA ÁREA PARA AJUSTES DO PROCESSO INTERNO - FAVOR NAO DELETAR ESSE TRECHO DO CODIGO
*              "US #131067 - SMCABANA     - 16-09-2025
                  ENDLOOP.
                ENDIF.
              ENDLOOP.

            ENDIF.

          ENDIF.
        CATCH zcx_integracao INTO DATA(ex_integra).    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          RAISE RESUMABLE EXCEPTION TYPE zcx_integracao."US #131067 - SMCABANA - 22-08-2025 - QUANDO FALHAR O SERVIÇO API DAR ESSA MSG
        CATCH zcx_error INTO DATA(ex_error).    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
          RAISE RESUMABLE EXCEPTION TYPE zcx_error. "US #131067 - SMCABANA - 22-08-2025 - QUANDO FALHAR O SERVIÇO API DAR ESSA MSG
      ENDTRY.

    ENDIF.

  ENDLOOP.
  wa_saida_result = wa_saida_devolucao.

  CLEAR: wa_consulta_nfe_sigam, wa_resultado_cons_nfe_sigam, r_response.

ENDFORM.
*** US #131067 - MMSILVA - 09.07.2025 - Fim ***

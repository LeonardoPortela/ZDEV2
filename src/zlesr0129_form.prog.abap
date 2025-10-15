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

      CASE abap_true.
        WHEN p_l1.

          PERFORM f_estrutura_alv USING:

           01  'ZLEST0174'             'ID_REGISTRO'                      'IT_SAIDA_0100_L1'  'ID_REGISTRO'                         'Id.Registro'               '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           02  ''                      ''                                 'IT_SAIDA_0100_L1'  'PROC'                                'Processado'                '10'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           02  'ZLEST0174'             'ID_PROC'                          'IT_SAIDA_0100_L1'  'ID_PROC'                             'Id.Proc.'                  '10'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
           03  'ZLEST0174'             'SRV_INTEGRACAO'                   'IT_SAIDA_0100_L1'  'SRV_INTEGRACAO'                      'Srv.Int.'                  '08'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           04  'ZLEST0174'             'PROTOCOLO_REC'                    'IT_SAIDA_0100_L1'  'PROTOCOLO_REC'                       'Prot.Rec.'                 '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           05  'ZLEST0174'             'RECEBEDOR_CNPJ'                   'IT_SAIDA_0100_L1'  'RECEBEDOR_CNPJ'                      'Recebedor CNPJ'            '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           06  'ZLEST0174'             'RECEBEDOR_NAME'                   'IT_SAIDA_0100_L1'  'RECEBEDOR_NAME'                      'Recebedor Nome'            '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           07  'ZLEST0174'             'SIGLA_TERMINAL_TRANSB'            'IT_SAIDA_0100_L1'  'SIGLA_TERMINAL_TRANSB'               'Sigla Terminal'            '14'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           08  'ZLEST0174'             'TERMINAL_TRANSB'                  'IT_SAIDA_0100_L1'  'TERMINAL_TRANSB'                     'Terminal Transb.'          '40'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           09  'ZLEST0174'             'DS_TERMINAL_TRANSB'               'IT_SAIDA_0100_L1'  'DS_TERMINAL_TRANSB'                  'Ds.Term.Transb.'           '40'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           10  'ZLEST0174'             'CNPJ_TERMINAL_TRANSB'             'IT_SAIDA_0100_L1'  'CNPJ_TERMINAL_TRANSB'                'CNPJ Term.Transb.'         '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           11  'ZLEST0174'             'NM_FANTASIA_TERMINAL_DESTINO'     'IT_SAIDA_0100_L1'  'NM_FANTASIA_TERMINAL_DESTINO'        'Nm.Fant.Term.Dest.'        '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           12  'ZLEST0174'             'RZ_SOCIAL_TERMINAL_DESTINO'       'IT_SAIDA_0100_L1'  'RZ_SOCIAL_TERMINAL_DESTINO'          'Rz.Soc.Term.Dest.'         '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           13  'ZLEST0174'             'CNPJ_TERMINAL_DESTINO'            'IT_SAIDA_0100_L1'  'CNPJ_TERMINAL_DESTINO'               'CNPJ Term.Dest.'           '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           14  'ZLEST0174'             'DT_CHEGADA'                       'IT_SAIDA_0100_L1'  'DT_CHEGADA'                          'Dt.Chegada'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           15  'ZLEST0174'             'DT_ENTRADA'                       'IT_SAIDA_0100_L1'  'DT_ENTRADA'                          'Dt.Entrada'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           16  'ZLEST0174'             'DT_SAIDA'                         'IT_SAIDA_0100_L1'  'DT_SAIDA'                            'Dt.Pesagem'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           17  'ZLEST0174'             'NOME_MOTORISTA'                   'IT_SAIDA_0100_L1'  'NOME_MOTORISTA'                      'Nm.Motorista'              '35'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           18  'ZLEST0174'             'PLACA'                            'IT_SAIDA_0100_L1'  'PLACA'                               'Placa'                     '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           19  'ZLEST0174'             'PESO_BRUTO'                       'IT_SAIDA_0100_L1'  'PESO_BRUTO'                          'Peso Bruto'                '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           20  'ZLEST0174'             'PESO_TARA'                        'IT_SAIDA_0100_L1'  'PESO_TARA'                           'Peso Tara'                 '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           21  'ZLEST0174'             'PESO_LIQUIDO'                     'IT_SAIDA_0100_L1'  'PESO_LIQUIDO'                        'Peso Liq.'                 '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           22  'ZLEST0175'             'CHAVE_NF'                         'IT_SAIDA_0100_L1'  'CHAVE_NF'                            'Chave NF'                  '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           23  'ZLEST0175'             'MODEL'                            'IT_SAIDA_0100_L1'  'MODEL'                               'Modelo'                    '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           24  'ZLEST0175'             'NFENUM'                           'IT_SAIDA_0100_L1'  'NFENUM'                              'Número'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           25  'ZLEST0175'             'SERIES'                           'IT_SAIDA_0100_L1'  'SERIES'                              'Série'                     '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           26  'ZLEST0175'             'DOCDAT'                           'IT_SAIDA_0100_L1'  'DOCDAT'                              'Dt.Emissão'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           "27  'ZLEST0175'             'DOCNUM'                           'IT_SAIDA_0100_L1'  'DOCNUM'                              'Docnum'                    '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           28  'ZLEST0175'             'PESO_DECLARADO'                   'IT_SAIDA_0100_L1'  'PESO_DECLARADO'                      'Peso NF'                   '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           29  'ZLEST0175'             'PESO_DESCARGA'                    'IT_SAIDA_0100_L1'  'PESO_DESCARGA'                       'Peso Descarga'             '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           30  'ZLEST0174'             'DT_REGISTRO'                      'IT_SAIDA_0100_L1'  'DT_REGISTRO'                         'Dt.Registro'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           31  'ZLEST0174'             'HR_REGISTRO'                      'IT_SAIDA_0100_L1'  'HR_REGISTRO'                         'Hr.Registro'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' .

        WHEN p_l2.

          PERFORM f_estrutura_alv USING:

           01  'ZLEST0177'             'ID_REGISTRO'                      'IT_SAIDA_0100_L2'  'ID_REGISTRO'                         'Id.Registro'               '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           02  ''                      ''                                 'IT_SAIDA_0100_L2'  'PROC'                                'Processado'                '10'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           02  'ZLEST0177'             'ID_PROC'                          'IT_SAIDA_0100_L2'  'ID_PROC'                             'Id.Proc.'                  '10'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
           03  'ZLEST0177'             'SRV_INTEGRACAO'                   'IT_SAIDA_0100_L2'  'SRV_INTEGRACAO'                      'Srv.Int.'                  '08'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           04  'ZLEST0177'             'PROTOCOLO_REC'                    'IT_SAIDA_0100_L2'  'PROTOCOLO_REC'                       'Prot.Rec.'                 '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           05  'ZLEST0177'             'RECEBEDOR_CNPJ'                   'IT_SAIDA_0100_L2'  'RECEBEDOR_CNPJ'                      'Recebedor CNPJ'            '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           06  'ZLEST0177'             'RECEBEDOR_NAME'                   'IT_SAIDA_0100_L2'  'RECEBEDOR_NAME'                      'Recebedor Nome'            '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           07  'ZLEST0177'             'SIGLA_TERMINAL_TRANSB'            'IT_SAIDA_0100_L2'  'SIGLA_TERMINAL_TRANSB'               'Sigla Terminal'            '14'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           08  'ZLEST0177'             'TERMINAL_TRANSB'                  'IT_SAIDA_0100_L2'  'TERMINAL_TRANSB'                     'Terminal Transb.'          '40'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           09  'ZLEST0177'             'DS_TERMINAL_TRANSB'               'IT_SAIDA_0100_L2'  'DS_TERMINAL_TRANSB'                  'Ds.Term.Transb.'           '40'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           10  'ZLEST0177'             'CNPJ_TERMINAL_TRANSB'             'IT_SAIDA_0100_L2'  'CNPJ_TERMINAL_TRANSB'                'CNPJ Term.Transb.'         '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           11  'ZLEST0177'             'NM_FANTASIA_TERMINAL_DESTINO'     'IT_SAIDA_0100_L2'  'NM_FANTASIA_TERMINAL_DESTINO'        'Nm.Fant.Term.Dest.'        '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           12  'ZLEST0177'             'RZ_SOCIAL_TERMINAL_DESTINO'       'IT_SAIDA_0100_L2'  'RZ_SOCIAL_TERMINAL_DESTINO'          'Rz.Soc.Term.Dest.'         '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           13  'ZLEST0177'             'CNPJ_TERMINAL_DESTINO'            'IT_SAIDA_0100_L2'  'CNPJ_TERMINAL_DESTINO'               'CNPJ Term.Dest.'           '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           14  'ZLEST0177'             'DT_FATURAMENTO'                   'IT_SAIDA_0100_L2'  'DT_FATURAMENTO'                      'Dt.Faturamento'            '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           15  'ZLEST0177'             'DT_CARREGAMENTO'                  'IT_SAIDA_0100_L2'  'DT_CARREGAMENTO'                     'Dt.Carregamento'           '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           16  'ZLEST0177'             'IDVAGAO'                          'IT_SAIDA_0100_L2'  'IDVAGAO'                             'Id.Vagão'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           17  'ZLEST0177'             'SERIE_VAGAO'                      'IT_SAIDA_0100_L2'  'SERIE_VAGAO'                         'Sér.Vagão'                 '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           17  ''                      ''                                 'IT_SAIDA_0100_L2'  'PLACA_VAGAO'                         'Placa Vagão'               '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           18  'ZLEST0177'             'NR_DESPACHO'                      'IT_SAIDA_0100_L2'  'NR_DESPACHO'                         'Nr.Despacho'               '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           19  'ZLEST0177'             'SERIE_DESPACHO'                   'IT_SAIDA_0100_L2'  'SERIE_DESPACHO'                      'Sér.Despacho'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           20  'ZLEST0177'             'NR_CTE'                           'IT_SAIDA_0100_L2'  'NR_CTE'                              'Nr.CT-e'                   '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           21  'ZLEST0177'             'SERIE_CTE'                        'IT_SAIDA_0100_L2'  'SERIE_CTE'                           'Sér.CT-e'                  '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           22  'ZLEST0177'             'CHAVE_CTE'                        'IT_SAIDA_0100_L2'  'CHAVE_CTE'                           'Chave CT-e'                '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           23  'ZLEST0177'             'PESO_BRUTO'                       'IT_SAIDA_0100_L2'  'PESO_BRUTO'                          'Peso Bruto'                '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           24  'ZLEST0177'             'PESO_TARA'                        'IT_SAIDA_0100_L2'  'PESO_TARA'                           'Peso Tara'                 '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           25  'ZLEST0177'             'PESO_LIQUIDO'                     'IT_SAIDA_0100_L2'  'PESO_LIQUIDO'                        'Peso Liq.'                 '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           26  'ZLEST0178'             'CHAVE_NF'                         'IT_SAIDA_0100_L2'  'CHAVE_NF'                            'Chave NF'                  '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           27  'ZLEST0178'             'MODEL'                            'IT_SAIDA_0100_L2'  'MODEL'                               'Modelo'                    '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           28  'ZLEST0178'             'NFENUM'                           'IT_SAIDA_0100_L2'  'NFENUM'                              'Número'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           29  'ZLEST0178'             'SERIES'                           'IT_SAIDA_0100_L2'  'SERIES'                              'Série'                     '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           30  'ZLEST0178'             'DOCDAT'                           'IT_SAIDA_0100_L2'  'DOCDAT'                              'Dt.Emissão'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           "31  'ZLEST0178'             'DOCNUM'                           'IT_SAIDA_0100_L2'  'DOCNUM'                              'Docnum'                    '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           32  'ZLEST0178'             'PESO_DECLARADO'                   'IT_SAIDA_0100_L2'  'PESO_DECLARADO'                      'Peso NF'                   '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           33  'ZLEST0178'             'PESO_CARREGADO'                   'IT_SAIDA_0100_L2'  'PESO_CARREGADO'                      'Peso Carregado'            '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           34  'ZLEST0177'             'DT_REGISTRO'                      'IT_SAIDA_0100_L2'  'DT_REGISTRO'                         'Dt.Registro'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           35  'ZLEST0177'             'HR_REGISTRO'                      'IT_SAIDA_0100_L2'  'HR_REGISTRO'                         'Hr.Registro'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' .

        WHEN p_l3.

          PERFORM f_estrutura_alv USING:

           01  'ZLEST0179'             'ID_REGISTRO'                      'IT_SAIDA_0100_L3'  'ID_REGISTRO'                         'Id.Registro'               '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           02  ''                      ''                                 'IT_SAIDA_0100_L3'  'PROC'                                'Processado'                '10'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           02  'ZLEST0179'             'ID_PROC'                          'IT_SAIDA_0100_L3'  'ID_PROC'                             'Id.Proc.'                  '10'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
           03  'ZLEST0179'             'SRV_INTEGRACAO'                   'IT_SAIDA_0100_L3'  'SRV_INTEGRACAO'                      'Srv.Int.'                  '08'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           07  'ZLEST0179'             'SIGLA_TERMINAL_DESCARGA'          'IT_SAIDA_0100_L3'  'SIGLA_TERMINAL_DESCARGA'             'Sigla Terminal'            '14'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
           08  'ZLEST0179'             'TERMINAL_DESCARGA'                'IT_SAIDA_0100_L3'  'TERMINAL_DESCARGA'                   'Terminal Descarga'         '40'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           09  'ZLEST0179'             'DS_TERMINAL_DESCARGA'             'IT_SAIDA_0100_L3'  'DS_TERMINAL_DESCARGA'                'Ds.Term.Descarga'          '40'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           10  'ZLEST0179'             'CNPJ_TERMINAL_DESCARGA'           'IT_SAIDA_0100_L3'  'CNPJ_TERMINAL_DESCARGA'              'CNPJ Term.Descarga.'       '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           11  'ZLEST0179'             'NM_FANTASIA_TERMINAL_DESTINO'     'IT_SAIDA_0100_L3'  'NM_FANTASIA_TERMINAL_DESTINO'        'Nm.Fant.Term.Dest.'        '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           12  'ZLEST0179'             'RZ_SOCIAL_TERMINAL_DESTINO'       'IT_SAIDA_0100_L3'  'RZ_SOCIAL_TERMINAL_DESTINO'          'Rz.Soc.Term.Dest.'         '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           13  'ZLEST0179'             'CNPJ_TERMINAL_DESTINO'            'IT_SAIDA_0100_L3'  'CNPJ_TERMINAL_DESTINO'               'CNPJ Term.Dest.'           '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           14  'ZLEST0179'             'DT_CHEGADA'                       'IT_SAIDA_0100_L3'  'DT_CHEGADA'                          'Dt.Chegada'                '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           16  'ZLEST0179'             'IDVAGAO'                          'IT_SAIDA_0100_L3'  'IDVAGAO'                             'Id.Vagão'                  '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           17  'ZLEST0179'             'SERIE_VAGAO'                      'IT_SAIDA_0100_L3'  'SERIE_VAGAO'                         'Sér.Vagão'                 '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           17  'ZLEST0179'             'MEINS'                            'IT_SAIDA_0100_L3'  'MEINS'                               'Und.Med.'                  '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           17  ''                      ''                                 'IT_SAIDA_0100_L3'  'PLACA_VAGAO'                         'Placa Vagão'               '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           20  'ZLEST0179'             'NR_CTE'                           'IT_SAIDA_0100_L3'  'NR_CTE'                              'Nr.CT-e'                   '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           21  'ZLEST0179'             'SERIE_CTE'                        'IT_SAIDA_0100_L3'  'SERIE_CTE'                           'Sér.CT-e'                  '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           22  'ZLEST0179'             'CHAVE_CTE'                        'IT_SAIDA_0100_L3'  'CHAVE_CTE'                           'Chave CT-e'                '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           23  'ZLEST0179'             'PESO_BRUTO'                       'IT_SAIDA_0100_L3'  'PESO_BRUTO'                          'Peso Bruto'                '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           24  'ZLEST0179'             'PESO_TARA'                        'IT_SAIDA_0100_L3'  'PESO_TARA'                           'Peso Tara'                 '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           25  'ZLEST0179'             'PESO_LIQUIDO'                     'IT_SAIDA_0100_L3'  'PESO_LIQUIDO'                        'Peso Liq. Origem'                 '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           26  'ZLEST0180'             'CHAVE_NF'                         'IT_SAIDA_0100_L3'  'CHAVE_NF'                            'Chave NF'                  '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           27  'ZLEST0180'             'MODEL'                            'IT_SAIDA_0100_L3'  'MODEL'                               'Modelo'                    '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           28  'ZLEST0180'             'NFENUM'                           'IT_SAIDA_0100_L3'  'NFENUM'                              'Número'                    '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           29  'ZLEST0180'             'SERIES'                           'IT_SAIDA_0100_L3'  'SERIES'                              'Série'                     '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           30  'ZLEST0180'             'DOCDAT'                           'IT_SAIDA_0100_L3'  'DOCDAT'                              'Dt.Emissão'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           "31  'ZLEST0180'             'DOCNUM'                           'IT_SAIDA_0100_L3'  'DOCNUM'                              'Docnum'                    '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           32  'ZLEST0180'             'PESO_DECLARADO'                   'IT_SAIDA_0100_L3'  'PESO_DECLARADO'                      'Peso NF'                   '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           33  'ZLEST0180'             'PESO_RATEADO'                     'IT_SAIDA_0100_L3'  'PESO_RATEADO'                        'Peso Rateado Descarga'              '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           34  'ZLEST0179'             'DT_REGISTRO'                      'IT_SAIDA_0100_L3'  'DT_REGISTRO'                         'Dt.Registro'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
           35  'ZLEST0179'             'HR_REGISTRO'                      'IT_SAIDA_0100_L3'  'HR_REGISTRO'                         'Hr.Registro'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' .


      ENDCASE.

    WHEN '0110'.

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

  CLEAR: it_saida_0100_l1[],
         it_saida_0100_l2[],
         it_saida_0100_l3[],
         tg_zlest0174[],
         tg_zlest0175[],
         tg_zlest0177[],
         tg_zlest0178[].

ENDFORM.

FORM f_selecionar_dados .

  PERFORM f_limpa_variaveis.

  CASE abap_true.
    WHEN p_l1.

      SELECT *
        FROM zlest0174 INTO TABLE tg_zlest0174
       WHERE dt_chegada IN p_data
         AND srv_integracao IN p_srvint.

      CHECK tg_zlest0174[] IS NOT INITIAL.

      SELECT *
        FROM zlest0175 INTO TABLE tg_zlest0175
         FOR ALL ENTRIES IN tg_zlest0174
       WHERE id_registro = tg_zlest0174-id_registro.

      DELETE tg_zlest0175 WHERE NOT ( nfenum IN p_nfenum AND
                                      docdat IN p_docdat ).

    WHEN p_l2.

      SELECT *
        FROM zlest0177 INTO TABLE tg_zlest0177
       WHERE dt_carregamento IN p_data
          AND srv_integracao IN p_srvint.

      CHECK tg_zlest0177[] IS NOT INITIAL.

      SELECT *
        FROM zlest0178 INTO TABLE tg_zlest0178
         FOR ALL ENTRIES IN tg_zlest0177
       WHERE id_registro = tg_zlest0177-id_registro.

      DELETE tg_zlest0178 WHERE NOT ( nfenum IN p_nfenum AND
                                      docdat IN p_docdat ).

    WHEN p_l3.

      SELECT *
        FROM zlest0179 INTO TABLE tg_zlest0179
       WHERE dt_chegada IN p_data
         AND srv_integracao IN p_srvint.

      CHECK tg_zlest0179[] IS NOT INITIAL.

      SELECT *
        FROM zlest0180 INTO TABLE tg_zlest0180
         FOR ALL ENTRIES IN tg_zlest0179
       WHERE id_registro = tg_zlest0179-id_registro.

      DELETE tg_zlest0180 WHERE NOT ( nfenum IN p_nfenum AND
                                      docdat IN p_docdat ).

  ENDCASE.



ENDFORM.

FORM f_processa_dados .

  CASE abap_true.
    WHEN p_l1.

      LOOP AT tg_zlest0175.

        CLEAR: wa_saida_0100_l1.

        READ TABLE tg_zlest0174 WITH KEY id_registro = tg_zlest0175-id_registro.

        CHECK sy-subrc EQ 0.
        MOVE-CORRESPONDING tg_zlest0174 TO wa_saida_0100_l1.
        MOVE-CORRESPONDING tg_zlest0175 TO wa_saida_0100_l1.

        IF tg_zlest0174-processado EQ abap_true.
          wa_saida_0100_l1-proc = icon_led_green.

          IF tg_zlest0174-erro_proc EQ abap_true.
            wa_saida_0100_l1-proc = icon_led_red.
          ENDIF.

        ELSE.
          wa_saida_0100_l1-proc = icon_led_yellow.
        ENDIF.

        APPEND wa_saida_0100_l1 TO it_saida_0100_l1.

      ENDLOOP.

      SORT it_saida_0100_l1 BY id_registro DESCENDING.


    WHEN p_l2.

      LOOP AT tg_zlest0178.

        CLEAR: wa_saida_0100_l2.

        READ TABLE tg_zlest0177 WITH KEY id_registro = tg_zlest0178-id_registro.

        CHECK sy-subrc EQ 0.
        MOVE-CORRESPONDING tg_zlest0177 TO wa_saida_0100_l2.
        MOVE-CORRESPONDING tg_zlest0178 TO wa_saida_0100_l2.

        IF tg_zlest0177-processado EQ abap_true.
          wa_saida_0100_l2-proc = icon_led_green.

          IF tg_zlest0177-erro_proc EQ abap_true.
            wa_saida_0100_l2-proc = icon_led_red.
          ENDIF.

        ELSE.
          wa_saida_0100_l2-proc = icon_led_yellow.
        ENDIF.

        wa_saida_0100_l2-placa_vagao = wa_saida_0100_l2-serie_vagao && wa_saida_0100_l2-idvagao.

        APPEND wa_saida_0100_l2 TO it_saida_0100_l2.

      ENDLOOP.

      SORT it_saida_0100_l2 BY id_registro DESCENDING.

    WHEN p_l3.

      LOOP AT tg_zlest0180.

        CLEAR: wa_saida_0100_l3.

        READ TABLE tg_zlest0179 WITH KEY id_registro = tg_zlest0180-id_registro.

        CHECK sy-subrc EQ 0.
        MOVE-CORRESPONDING tg_zlest0179 TO wa_saida_0100_l3.
        MOVE-CORRESPONDING tg_zlest0180 TO wa_saida_0100_l3.

        IF tg_zlest0179-processado EQ abap_true.
          wa_saida_0100_l3-proc = icon_led_green.

          IF tg_zlest0179-erro_proc EQ abap_true.
            wa_saida_0100_l3-proc = icon_led_red.
          ENDIF.

        ELSE.
          wa_saida_0100_l3-proc = icon_led_yellow.
        ENDIF.

        wa_saida_0100_l3-placa_vagao = wa_saida_0100_l3-serie_vagao && wa_saida_0100_l3-idvagao.

        APPEND wa_saida_0100_l3 TO it_saida_0100_l3.

      ENDLOOP.

      SORT it_saida_0100_l3 BY id_registro DESCENDING.


  ENDCASE.



ENDFORM.


FORM f_consultar_dados USING p_dt_cons TYPE sy-datum
                             p_inter   TYPE zlest0174-srv_integracao.

  CHECK p_data IS NOT INITIAL.

  CASE abap_true.
    WHEN p_l1.
      SUBMIT zlesr0128 WITH p_dtche   = p_dt_cons
                       WITH p_tpproc  = '1' "Consultar
         AND RETURN.
    WHEN p_l2.
      SUBMIT zlesr0130 WITH p_dtsai   = p_dt_cons
                       WITH p_tpproc  = '1' "Consultar
         AND RETURN.
    WHEN p_l3.
      SUBMIT zlesr0131 WITH p_dtche   = p_dt_cons
                       WITH p_tpproc  = '1' "Consultar
         AND RETURN.
  ENDCASE.


ENDFORM.

FORM f_inf_dt_consulta .

  CLEAR: vg_dt_consulta.

  CALL SCREEN 0101 STARTING AT 05 05.

ENDFORM.

FORM f_processar_registros .

  CASE abap_true.
    WHEN p_l1.
      SUBMIT zlesr0128 WITH p_tpproc  = '2' "Processar
         AND RETURN.
    WHEN p_l2.
      SUBMIT zlesr0130 WITH p_tpproc  = '2' "Processar
         AND RETURN.
    WHEN p_l3.
      SUBMIT zlesr0131 WITH p_tpproc  = '2' "Processar
         AND RETURN.
  ENDCASE.

ENDFORM.


FORM f_logs_proc USING p_cont.

  DATA: tg_zlest0176_out TYPE TABLE OF zlest0176 WITH HEADER LINE.

  CHECK p_cont IS NOT INITIAL.

  CLEAR: tg_zlest0176_out[].

  SELECT *
    FROM zlest0176 INTO TABLE tg_zlest0176_out
   WHERE cont EQ p_cont.

  CHECK  tg_zlest0176_out[] IS NOT INITIAL.

  SORT tg_zlest0176_out BY cont_item.

  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:
     01  ''   ''            'TG_0141_RESUMO_OUT' 'CONT'           'Cont.'           '10' '' '',
     02  ''   ''            'TG_0141_RESUMO_OUT' 'CONT_ITEM'      'Cont.Item'       '10' '' '',
     03  ''   ''            'TG_0141_RESUMO_OUT' 'MSGTYP'         'Tipo'            '04' '' '',
     04  ''   ''            'TG_0141_RESUMO_OUT' 'MSGNR'          'Nro.'            '06' '' '',
     05  ''   ''            'TG_0141_RESUMO_OUT' 'MSGV1'          'Mensagem'        '60' '' '',
     06  ''   ''            'TG_0141_RESUMO_OUT' 'DT_REGISTRO'    'Data'            '10' '' '',
     07  ''   ''            'TG_0141_RESUMO_OUT' 'HR_REGISTRO'    'Hora'            '10' '' '',
     08  ''   ''            'TG_0141_RESUMO_OUT' 'US_REGISTRO'    'Usuário'         '10' '' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 140
      i_screen_end_line     = 25
    TABLES
      t_outtab              = tg_zlest0176_out.


ENDFORM.


FORM f_montar_estrutura USING VALUE(p_col_pos)       TYPE i
                              VALUE(p_ref_tabname)   LIKE dd02d-tabname
                              VALUE(p_ref_fieldname) LIKE dd03d-fieldname
                              VALUE(p_tabname)       LIKE dd02d-tabname
                              VALUE(p_field)         LIKE dd03d-fieldname
                              VALUE(p_scrtext_l)     LIKE dd03p-scrtext_l
                              VALUE(p_outputlen)
                              VALUE(p_edit)
                              VALUE(p_do_sum).

  CLEAR: wa_estrutura.

  wa_estrutura-fieldname     = p_field.
  wa_estrutura-tabname       = p_tabname.
  wa_estrutura-ref_tabname   = p_ref_tabname.
  wa_estrutura-ref_fieldname = p_ref_fieldname.
  wa_estrutura-key           = ' '.
  wa_estrutura-key_sel       = 'X'.
  wa_estrutura-col_pos       = p_col_pos.
  wa_estrutura-no_out        = ' '.
  wa_estrutura-seltext_s     = p_scrtext_l.
  wa_estrutura-seltext_m     = p_scrtext_l.
  wa_estrutura-seltext_l     = p_scrtext_l.
  wa_estrutura-do_sum        = p_do_sum.

  IF p_scrtext_l IS NOT INITIAL.
    wa_estrutura-reptext_ddic  = p_scrtext_l.
  ENDIF.

  wa_estrutura-outputlen = p_outputlen.

  TRANSLATE  wa_estrutura-fieldname     TO UPPER CASE.
  TRANSLATE  wa_estrutura-tabname       TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_tabname   TO UPPER CASE.
  TRANSLATE  wa_estrutura-ref_fieldname TO UPPER CASE.

  APPEND wa_estrutura TO estrutura.

ENDFORM.                    " MONTAR_ESTRUTURA

FORM f_visualizar_dacte.

  DATA: v_chv_cte TYPE zde_chave_doc_e.

  CLEAR: v_chv_cte.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows[] ) NE 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  CASE abap_true.
    WHEN p_l2.

      READ TABLE it_saida_0100_l2 INTO wa_saida_0100_l2 INDEX wa_sel_rows-index.
      CHECK sy-subrc EQ 0.

      v_chv_cte = wa_saida_0100_l2-chave_cte.

    WHEN p_l3.

      READ TABLE it_saida_0100_l3 INTO wa_saida_0100_l3 INDEX wa_sel_rows-index.
      CHECK sy-subrc EQ 0.

      v_chv_cte = wa_saida_0100_l3-chave_cte.

  ENDCASE.

  CHECK v_chv_cte IS NOT INITIAL.

  TRY .
      zcl_cte_dist_g=>dacte( i_cte = v_chv_cte ).
    CATCH zcx_cte_inbound INTO DATA(ex_cte_inbound).
      ex_cte_inbound->published_erro( EXPORTING i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.


ENDFORM.

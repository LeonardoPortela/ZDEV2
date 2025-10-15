*&---------------------------------------------------------------------*
*&  Include           ZLESR0111_FORM
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

  CLEAR: gs_layout.
  "GS_VARIANT.

  REFRESH: it_exclude_fcode.

ENDFORM.

FORM f_criar_catalog USING p_screen.

  FREE: wa_fcat, it_fcat.

  CASE p_screen.
    WHEN '0100'.

      CASE abap_true.
        WHEN p_tp_rcc. "Recepcionar Carga

          CASE abap_true.
            WHEN p_vw_nf. "Notas Fiscais

              PERFORM f_estrutura_alv USING:

                01  ''               ''                       'IT_SAIDA_0100_01'   'IC_LIB_ENVIO_PARC'        'L.E.P.'               '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
                02  'ZLEST0142'      'CHAVE_NFE'              'IT_SAIDA_0100_01'   'CHAVE_NFE'                'Chave NF-e'           '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                03  'ZLEST0142'      'CHAVE_NFF'              'IT_SAIDA_0100_01'   'CHAVE_NFF'                'Chave NF-f'           '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                04  'ZLEST0142'      'CNPJ_EMISSOR'           'IT_SAIDA_0100_01'   'CNPJ_EMISSOR'             'CNPJ Fornecedor'      '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                05  'ZLEST0142'      'CPF_EMISSOR'            'IT_SAIDA_0100_01'   'CPF_EMISSOR'              'CPF Fornecedor'       '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                06  'ZLEST0142'      'CNPJ_EMISSOR'           'IT_SAIDA_0100_01'   'CNPJ_DEST'                'CNPJ Destinatario'    '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                06  'ZLEST0142'      'CPF_EMISSOR'            'IT_SAIDA_0100_01'   'CPF_DEST'                 'CPF Destinatario'     '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                06  'ZLEST0142'      'NUMERO'                 'IT_SAIDA_0100_01'   'NUMERO'                   'Núm. NF'              '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                07  'ZLEST0142'      'DT_EMISSAO'             'IT_SAIDA_0100_01'   'DT_EMISSAO'               'Dt.Emissão'           '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                08  'ZLEST0142'      'SERIE'                  'IT_SAIDA_0100_01'   'SERIE'                    'Série'                '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                09  'ZLEST0142'      'MODEL'                  'IT_SAIDA_0100_01'   'MODEL'                    'Modelo'               '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                10  ''               ''                       'IT_SAIDA_0100_01'   'CFOP'                     'CFOP'                 '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                11  'LFA1'           'LIFNR'                  'IT_SAIDA_0100_01'   'RM_CODIGO'                'Cd.Remetente'         '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                12  'LFA1'           'NAME1'                  'IT_SAIDA_0100_01'   'NAME1'                    'Remetente'            '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                13  'MAKT'           'MATNR'                  'IT_SAIDA_0100_01'   'MATNR'                    'Cd.Material'          '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                14  'MAKT'           'MAKTX'                  'IT_SAIDA_0100_01'   'MAKTX'                    'Material'             '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                15  'ZLEST0142'      'PESO_CHEGADA'           'IT_SAIDA_0100_01'   'PESO_CHEGADA'             'Peso Chegada'         '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                16  'ZLEST0142'      'PESO_FISCAL'            'IT_SAIDA_0100_01'   'PESO_FISCAL'              'Peso NF'              '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                17  'ZLEST0142'      'PESO_RATEIO_ORIGEM'     'IT_SAIDA_0100_01'   'PESO_RATEIO_ORIGEM'       'Peso Rat.Orig.'       '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                18  'ZLEST0142'      'PESO_RATEIO_ORIGEM'     'IT_SAIDA_0100_01'   'DIF_PESO'                 'Peso Trans. x Rat.'   '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                19  'ZLEST0142'      'DT_CHEGADA'             'IT_SAIDA_0100_01'   'DT_CHEGADA'               'Dt.Chegada'           '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                20  'ZLEST0142'      'HR_CHEGADA'             'IT_SAIDA_0100_01'   'HR_CHEGADA'               'Hr.Chegada'           '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                21  'ZLEST0142'      'BUKRS_ROM'              'IT_SAIDA_0100_01'   'BUKRS_ROM'                'Empresa Rom.'         '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                22  'ZLEST0142'      'BRANCH_ROM'             'IT_SAIDA_0100_01'   'BRANCH_ROM'               'Filial Rom.'          '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                23  'ZLEST0142'      'LOCAL_DESCARGA'         'IT_SAIDA_0100_01'   'LOCAL_DESCARGA'           'Local Descarga'       '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                24  'ZLEST0142'      'CHAVE_CTE'              'IT_SAIDA_0100_01'   'CHAVE_CTE'                'Chave CT-e'           '44'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                25  'ZLEST0142'      'MODAL'                  'IT_SAIDA_0100_01'   'MODAL'                    'Modal'                '05'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                26  'ZLEST0142'      'CNPJ_TRANSP'            'IT_SAIDA_0100_01'   'CNPJ_TRANSP'              'CNPJ Transportador'   '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                27  'ZLEST0142'      'CPF_TRANSP'             'IT_SAIDA_0100_01'   'CPF_TRANSP'               'CPF Transportador'    '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                28  ''               ''                       'IT_SAIDA_0100_01'   'NF_COMPLEMENTO'           'Complemento'          '11'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
                29  ''               ''                       'IT_SAIDA_0100_01'   'NFS_COMPLEMENTADAS'       'NFs. Complementadas'  '100'  ' '    ''  ' ' ' ' ' ' ' ' '' ,
                30  ''               ''                       'IT_SAIDA_0100_01'   'NR_TK_GUARDIAN '          'Ticket Guardian'      '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                31  'ZLEST0142'      'PESO_TRANSBORDO'        'IT_SAIDA_0100_01'   'PESO_TRANSBORDO'          'Peso Transbordo'      '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                32  'ZLEST0142'      'DATA_REG'               'IT_SAIDA_0100_01'   'DATA_REG'                 'Data Reg.'            '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                33  'ZLEST0142'      'HORA_REG'               'IT_SAIDA_0100_01'   'HORA_REG'                 'Hora Reg.'            '10'   ' '    ''  ' ' ' ' ' ' ' ' '' .



            WHEN p_vw_due. "DU-e's
            WHEN p_rc_nf. "Recepções de Carga por NF

              PERFORM f_estrutura_alv USING:

                01  'ZLEST0146'      'ID_RECEPCAO'              'IT_SAIDA_0100_02'   'ID_RECEPCAO'             'Id.Recepção'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                02  'ZLEST0146'      'BUKRS'                    'IT_SAIDA_0100_02'   'BUKRS'                   'Empresa'              '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                03  'ZLEST0146'      'BRANCH'                   'IT_SAIDA_0100_02'   'BRANCH'                  'Filial'               '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                04  'ZLEST0146'      'CNPJ_RESPONSAVEL'         'IT_SAIDA_0100_02'   'CNPJ_RESPONSAVEL'        'CNPJ Responsável'     '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                05  'ZLEST0146'      'LOCAL_CODIGO_URF'         'IT_SAIDA_0100_02'   'LOCAL_CODIGO_URF'        'Cod.URF'              '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                06  'ZLEST0146'      'LOCAL_CODIGO_RA'          'IT_SAIDA_0100_02'   'LOCAL_CODIGO_RA'         'Cod.RA '              '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                07  'ZLEST0146'      'TRANSPORTADOR_CNPJ'       'IT_SAIDA_0100_02'   'TRANSPORTADOR_CNPJ'      'Transportador CNPJ'   '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                08  'ZLEST0146'      'TRANSPORTADOR_CPF'        'IT_SAIDA_0100_02'   'TRANSPORTADOR_CPF'       'Transportador CPF'    '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                09  'ZLEST0146'      'PESO_AFERIDO_RECEPCAO'    'IT_SAIDA_0100_02'   'PESO_AFERIDO_RECEPCAO'   'Peso Aferido.Rec.'    '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                10  ''               ''                         'IT_SAIDA_0100_02'   'ENVIADA'                 'Enviada'              '07'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
                11  'ZLEST0142'      'CHAVE_NFE'                'IT_SAIDA_0100_02'   'CHAVE_NFE'               'Chave NF-e'           '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                12  'ZLEST0142'      'CHAVE_NFF'                'IT_SAIDA_0100_02'   'CHAVE_NFF'               'Chave NF-f'           '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                13  'ZLEST0147'      'DOCNUM'                   'IT_SAIDA_0100_02'   'DOCNUM'                  'Docnum'               '10'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
                14  'ZLEST0142'      'CNPJ_EMISSOR'             'IT_SAIDA_0100_02'   'CNPJ_EMISSOR'            'CNPJ Fornecedor'      '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                15  'ZLEST0142'      'CPF_EMISSOR'              'IT_SAIDA_0100_02'   'CPF_EMISSOR'             'CPF Fornecedor'       '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,

                16  'ZLEST0142'      'CNPJ_EMISSOR'             'IT_SAIDA_0100_02'   'CNPJ_DEST'               'CNPJ Destinatario'    '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                16  'ZLEST0142'      'CPF_EMISSOR'              'IT_SAIDA_0100_02'   'CPF_DEST'                'CPF Destinatario'     '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,

                16  'MAKT'           'MATNR'                    'IT_SAIDA_0100_02'   'MATNR'                   'Cd.Material'          '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                17  'MAKT'           'MAKTX'                    'IT_SAIDA_0100_02'   'MAKTX'                   'Material'             '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                18  'ZLEST0142'      'NUMERO'                   'IT_SAIDA_0100_02'   'NUMERO'                  'Núm. NF'              '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                19  ''               ''                         'IT_SAIDA_0100_02'   'CFOP'                    'CFOP'                 '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                20  'ZLEST0142'      'DT_CHEGADA'               'IT_SAIDA_0100_02'   'DT_CHEGADA'              'Dt.Chegada'           '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                21  'ZLEST0142'      'DT_EMISSAO'               'IT_SAIDA_0100_02'   'DT_EMISSAO'              'Dt.Emissão'           '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                22  'ZLEST0142'      'PESO_CHEGADA'             'IT_SAIDA_0100_02'   'PESO_CHEGADA'            'Peso Chegada'         '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                23  'ZLEST0142'      'PESO_FISCAL'              'IT_SAIDA_0100_02'   'PESO_FISCAL'             'Peso NF'              '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
               " 24  'ZLEST0142'      'PESO_TRANSBORDO'          'IT_SAIDA_0100_01'   'PESO_TRANSBORDO'         'Peso Transbordo'      '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                24  ''               ''                         'IT_SAIDA_0100_01'   'NF_COMPLEMENTO'          'Complemento'          '11'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
                25  ''               ''                         'IT_SAIDA_0100_01'   'NFS_COMPLEMENTADAS'      'NFs. Complementadas'  '100'  ' '    ''  ' ' ' ' ' ' ' ' '' ,
                26  'ZLEST0146'      'DT_RECEPCAO'              'IT_SAIDA_0100_02'   'DT_RECEPCAO'             'Dt.Recepção'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                27  'ZLEST0146'      'HR_RECEPCAO'              'IT_SAIDA_0100_02'   'HR_RECEPCAO'             'Hr.Recepção'          '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                28  'ZLEST0146'      'US_RECEPCAO'              'IT_SAIDA_0100_02'   'US_RECEPCAO'             'Usuário'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                29  'ZLEST0142'      'LIB_ENVIO_PARC'           'IT_SAIDA_0100_02'   'IC_LIB_ENVIO_PARC'       'L.E.P'                '06'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
                30  'ZLEST0142'      'DT_LIB_ENVIO_PARC'        'IT_SAIDA_0100_02'   'DT_LIB_ENVIO_PARC'       'Dt.Lib.'              '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                31  'ZLEST0142'      'HR_LIB_ENVIO_PARC'        'IT_SAIDA_0100_02'   'HR_LIB_ENVIO_PARC'       'Hr.Lib.'              '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                32  'ZLEST0142'      'US_LIB_ENVIO_PARC'        'IT_SAIDA_0100_02'   'US_LIB_ENVIO_PARC'       'Us.Lib.'              '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .


            WHEN p_rc_due. "Recepções de Carga por DU-e
            WHEN p_ec_due. "Entregas de Carga por DU-e
          ENDCASE.

        WHEN p_tp_ecg. "Entregar Carga

          CASE abap_true.
            WHEN p_vw_nf. "Notas Fiscais
            WHEN p_vw_due. "DU-e's

              PERFORM f_estrutura_alv USING:

                01  'ZSDT0170'              'ID_DUE'                        'IT_SAIDA_0100_03'  'ID_DUE'                      'Id.DU-e'                   '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                02  'ZSDT0170'              'BUKRS'                         'IT_SAIDA_0100_03'  'BUKRS'                       'Empresa'                   '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                03  'ZSDT0170'              'NUMERO_DUE'                    'IT_SAIDA_0100_03'  'NUMERO_DUE'                  'Número DU-e'               '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                04  'ZSDT0170'              'NUMERO_RUC'                    'IT_SAIDA_0100_03'  'NUMERO_RUC'                  'Número RUC'                '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                04  'ZNOM_TRANSPORTE'       'ID_NOMEACAO_TRAN'              'IT_SAIDA_0100_03'  'ID_NOMEACAO_TRAN'            'Id.Nomeação'               '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                04  'ZNOM_TRANSPORTE'       'DS_NOME_TRANSPOR'              'IT_SAIDA_0100_03'  'DS_NOME_TRANSPOR'            'Navio'                     '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                05  'ZSDT0170'              'CODIGO_URF_DESPACHO'           'IT_SAIDA_0100_03'  'CODIGO_URF_DESPACHO'         'URF Despacho'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                06  'ZSDT0170'              'CODIGO_RA_DESPACHO'            'IT_SAIDA_0100_03'  'CODIGO_RA_DESPACHO'          'RA Despacho'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                07  'ZSDT0170'              'TP_COD_LOCAL_DESPACHO'         'IT_SAIDA_0100_03'  'TP_COD_LOCAL_DESPACHO'       'Tp.Loc.Desp.'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                08  'ZSDT0170'              'CNPJ_CPF_RESP_LOC_DESP'        'IT_SAIDA_0100_03'  'CNPJ_CPF_RESP_LOC_DESP'      'CNPJ/CPF Resp.Loc.Desp.'   '24'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                09  'ZSDT0170'              'LOCAL_DESPACHO_LONGITUDE'      'IT_SAIDA_0100_03'  'LOCAL_DESPACHO_LONGITUDE'    'Loc.Desp.Longitude'        '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                10  'ZSDT0170'              'LOCAL_DESPACHO_LATITUDE'       'IT_SAIDA_0100_03'  'LOCAL_DESPACHO_LATITUDE'     'Loc.Desp.Latitude'         '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                11  'ZSDT0170'              'LOCAL_DESPACHO_END'            'IT_SAIDA_0100_03'  'LOCAL_DESPACHO_END'          'Loc.Desp.Endereço'         '18'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                12  'ZSDT0170'              'FORMA_EXPORTACAO'              'IT_SAIDA_0100_03'  'FORMA_EXPORTACAO'            'Forma Exportação'          '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                13  'ZSDT0170'              'CASO_ESPECIAL_TRANSPORTE'      'IT_SAIDA_0100_03'  'CASO_ESPECIAL_TRANSPORTE'    'Caso Esp.Transp.'          '17'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                14  'ZSDT0170'              'SITUACAO_ESPECIAL'             'IT_SAIDA_0100_03'  'SITUACAO_ESPECIAL'           'Situação Esp.'             '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                15  'ZSDT0170'              'OBSERVACOES_GERAIS'            'IT_SAIDA_0100_03'  'OBSERVACOES_GERAIS'          'Observ.Gerais'             '16'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                16  'ZSDT0170'              'MOEDA_CAMBIO'                  'IT_SAIDA_0100_03'  'MOEDA_CAMBIO'                'Moeda Neg.'                '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                17  'ZSDT0170'              'MOTIVO'                        'IT_SAIDA_0100_03'  'MOTIVO'                      'Motivo'                    '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                18  'ZSDT0170'              'CNPJ_DECLARANTE'               'IT_SAIDA_0100_03'  'CNPJ_DECLARANTE'             'CNPJ Declarante'           '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                19  'ZSDT0170'              'CODIGO_URF_EMBARQUE'           'IT_SAIDA_0100_03'  'CODIGO_URF_EMBARQUE'         'URF Embarque'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                20  'ZSDT0170'              'CODIGO_RA_EMBARQUE'            'IT_SAIDA_0100_03'  'CODIGO_RA_EMBARQUE'          'RA Embarque'               '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                21  'ZSDT0170'              'TP_DUE'                        'IT_SAIDA_0100_03'  'TP_DUE'                      'Tp.DU-e'                   '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                22  'ZSDT0170'              'ID_DUE_REF'                    'IT_SAIDA_0100_03'  'ID_DUE_REF'                  'Id.DU-e.Ref.'              '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                23  'ZSDT0170'              'STATUS'                        'IT_SAIDA_0100_03'  'STATUS'                      'Status'                    '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                23  'ZSDT0172'              'PESO_LIQ_TOTAL'                'IT_SAIDA_0100_03'  'PESO_LIQ_TOTAL'              'Peso Liq.Tot(KG)'          '19'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                23  'ZSDT0181'              'PESO_BRUTO_ENTREGUE'           'IT_SAIDA_0100_03'  'PESO_BRUTO_ENTREGUE'         'Peso Bruto Entregue(KG)'   '23'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                23  'ZSDT0181'              'PESO_BRUTO_ENTREGUE'           'IT_SAIDA_0100_03'  'SALDO_DISPONIVEL'            'Saldo Disponível(KG)'      '19'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                24  'ZSDT0170'              'DT_REGISTRO'                   'IT_SAIDA_0100_03'  'DT_REGISTRO'                 'Dt.Criação'                '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                25  'ZSDT0170'              'HR_REGISTRO'                   'IT_SAIDA_0100_03'  'HR_REGISTRO'                 'Hr.Criação'                '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                26  'ZSDT0170'              'US_REGISTRO'                   'IT_SAIDA_0100_03'  'US_REGISTRO'                 'Us.Criação'                '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                27  'ZSDT0170'              'DT_MODIFICACAO'                'IT_SAIDA_0100_03'  'DT_MODIFICACAO'              'Dt.Modificação'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                28  'ZSDT0170'              'HR_MODIFICACAO'                'IT_SAIDA_0100_03'  'HR_MODIFICACAO'              'Hr.Modificação'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                29  'ZSDT0170'              'US_MODIFICACAO'                'IT_SAIDA_0100_03'  'US_MODIFICACAO'              'Us.Modificação'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' .


            WHEN p_rc_nf.  "Recepções de Carga por NF
            WHEN p_rc_due. "Recepções de Carga por DU-e
            WHEN p_ec_due. "Entregas de Carga por DU-e

              PERFORM f_estrutura_alv USING:

                01  'ZSDT0179'            'ID_ENTREGA'                  'IT_SAIDA_0100_02'    'ID_ENTREGA'                 'Id.Entrega'             '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                02  'ZSDT0179'            'TP_ENTREGA'                  'IT_SAIDA_0100_02'    'TP_ENTREGA'                 'Tp.Entrega'             '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                03  'ZSDT0179'            'BUKRS'                       'IT_SAIDA_0100_02'    'BUKRS'                      'Empresa'                '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                04  'ZSDT0179'            'BRANCH'                      'IT_SAIDA_0100_02'    'BRANCH'                     'Filial'                 '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                05  'ZSDT0179'            'CNPJ_RESPONSAVEL'            'IT_SAIDA_0100_02'    'CNPJ_RESPONSAVEL'           'CNPJ Resp.'             '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                06  'ZSDT0179'            'LOCAL_CODIGO_URF'            'IT_SAIDA_0100_02'    'LOCAL_CODIGO_URF'           'Cod.URF'                '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                07  'ZSDT0179'            'LOCAL_CODIGO_RA'             'IT_SAIDA_0100_02'    'LOCAL_CODIGO_RA'            'Cod.RA'                 '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                09  'ZSDT0179'            'REC_CNPJ'                    'IT_SAIDA_0100_02'    'REC_CNPJ'                   'Rec.CNPJ'               '14'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
                10  'ZSDT0179'            'REC_CPF'                     'IT_SAIDA_0100_02'    'REC_CPF'                    'Rec.CPF'                '14'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                11  'ZSDT0179'            'REC_NOME_ESTRANGEIRO'        'IT_SAIDA_0100_02'    'REC_NOME_ESTRANGEIRO'       'Rec.Nome Estrangeiro'   '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                12  'ZSDT0179'            'REC_VIA_TRANSPORTE'          'IT_SAIDA_0100_02'    'REC_VIA_TRANSPORTE'         'Rec.Via Transporte'     '20'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                13  'ZSDT0179'            'REC_BALDEACAO_TRANSBORDO'    'IT_SAIDA_0100_02'    'REC_BALDEACAO_TRANSBORDO'   'Bald.Transb.'           '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                14  'ZSDT0179'            'PESO_AFERIDO'                'IT_SAIDA_0100_02'    'PESO_AFERIDO'               'Peso Aferido'           '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                15  'ZSDT0179'            'STATUS'                      'IT_SAIDA_0100_02'    'STATUS'                     'Status'                 '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                16  'ZSDT0170'            'BUKRS'                       'IT_SAIDA_0100_02'    'BUKRS_DUE'                  'Emp.DU-e'               '08'  ' '    ''  ' ' ' ' ' ' ' ' '' ,
                17  'ZSDT0170'            'NUMERO_DUE'                  'IT_SAIDA_0100_02'    'NUMERO_DUE'                 'Número DU-e'            '14'  ' '    ''  ' ' ' ' ' ' ' ' '' ,
                18  'ZSDT0170'            'NUMERO_RUC'                  'IT_SAIDA_0100_02'    'NUMERO_RUC'                 'Número RUC'             '35'  ' '    ''  ' ' ' ' ' ' ' ' '' ,
                19  'ZNOM_TRANSPORTE'     'ID_NOMEACAO_TRAN'            'IT_SAIDA_0100_02'    'ID_NOMEACAO_TRAN'           'Id.Nomeação'            '11'  ' '    ''  ' ' ' ' ' ' ' ' '' ,
                20  'ZNOM_TRANSPORTE'     'DS_NOME_TRANSPOR'            'IT_SAIDA_0100_02'    'DS_NOME_TRANSPOR'           'Navio'                  '30'  ' '    ''  ' ' ' ' ' ' ' ' '' ,
                21  'ZSDT0170'            'CNPJ_DECLARANTE'             'IT_SAIDA_0100_02'    'CNPJ_DECLARANTE'            'CNPJ Declarante'        '15'  ' '    ''  ' ' ' ' ' ' ' ' '' ,
                22  'ZSDT0179'            'DT_ENTREGA'                  'IT_SAIDA_0100_02'    'DT_ENTREGA'                 'Dt.Entrega'             '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
                23  'ZSDT0179'            'HR_ENTREGA'                  'IT_SAIDA_0100_02'    'HR_ENTREGA'                 'Hr.Entrega'             '12'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
                24  'ZSDT0179'            'US_ENTREGA'                  'IT_SAIDA_0100_02'    'US_ENTREGA'                 'Us.Entrega'             '12'   ' '    ''  ' ' ' ' ' ' ' ' '' .

          ENDCASE.

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

  CLEAR: wa_saida_0100_01,
         it_saida_0100_01[],
         wa_saida_0100_02,
         it_saida_0100_02[],
         wa_saida_0100_03,
         it_saida_0100_03[],
         wa_saida_0100_04,
         it_saida_0100_04[],
         wa_saida_0100_05,
         it_saida_0100_05[],
         tg_0142[],
         tg_0146[],
         tg_0147[],
         tg_0170[],
         tg_0172[],
         tg_lfa1[],
         tg_makt[],
         tg_zsdt0001[],
         tg_lin[],
         tg_znom_transporte[],
         tg_0192[],
         tg_zib_nfe_dist_ter[],
         vg_not_found.

ENDFORM.

FORM f_selecionar_dados .

  PERFORM f_limpa_variaveis.

  AUTHORITY-CHECK OBJECT 'M_MATE_BUK'
    ID 'BUKRS' FIELD  p_bukrs-low
    ID 'ACTVT' FIELD '03'.    "Visualização

  CASE sy-subrc.
    WHEN 0.
      "  tem autorização!
    WHEN 4.
      MESSAGE 'Sem autorização para esta empresa' TYPE 'I'.
      STOP.
    WHEN 12.
      MESSAGE 'Sem autorização neste objeto ' TYPE 'I'.
      STOP.
    WHEN OTHERS.
      STOP.
  ENDCASE.

  CASE abap_true.
    WHEN p_tp_rcc. "Recepcionar Carga

      CASE abap_true.
        WHEN p_vw_nf. "Notas Fiscais

          SELECT *
            FROM zlest0142 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0142
           WHERE a~bukrs_rom      IN p_bukrs
             AND a~branch_rom     IN p_branch
             AND a~dt_chegada     IN p_dtmov
             AND a~cnpj_emissor   IN p_stcd1
             AND a~dt_emissao     IN p_dtemi
             AND a~numero         IN p_numnf
             AND a~modal          IN p_modal
             AND a~model          IN p_model
             AND a~id_recepcao    EQ '0000000000'.

          SELECT *
            FROM zlest0142 AS a APPENDING CORRESPONDING FIELDS OF TABLE tg_0142
           WHERE a~bukrs_ra       IN p_bukrs
             AND a~branch_ra      IN p_branch
             AND a~dt_chegada     IN p_dtmov
             AND a~cnpj_emissor   IN p_stcd1
             AND a~dt_emissao     IN p_dtemi
             AND a~numero         IN p_numnf
             AND a~modal          IN p_modal
             AND a~model          IN p_model
             AND a~id_recepcao    EQ '0000000000'.

          SORT tg_0142 BY chave_nfe chave_nff.
          DELETE ADJACENT DUPLICATES FROM tg_0142 COMPARING chave_nfe chave_nff.

          CHECK tg_0142[] IS NOT INITIAL.

          "Romaneios -------------------------------------------------------------|
          tg_0142_aux[] = tg_0142[].
          DELETE tg_0142_aux WHERE chave_nfe IS INITIAL.
          IF tg_0142_aux[] IS NOT INITIAL.
            SELECT *
              FROM zsdt0001 INTO TABLE tg_zsdt0001
               FOR ALL ENTRIES IN tg_0142_aux
             WHERE chave_nfe EQ tg_0142_aux-chave_nfe.
          ENDIF.

          tg_0142_aux[] = tg_0142[].
          DELETE tg_0142_aux WHERE ch_referencia IS INITIAL.
          IF tg_0142_aux[] IS NOT INITIAL.
            SELECT *
              FROM zsdt0001 APPENDING TABLE tg_zsdt0001
               FOR ALL ENTRIES IN tg_0142_aux
             WHERE ch_referencia EQ tg_0142_aux-ch_referencia.
          ENDIF.

          LOOP AT tg_0142 WHERE matnr IS INITIAL.
            READ TABLE tg_zsdt0001 WITH KEY chave_nfe = tg_0142-chave_nfe.
            IF ( sy-subrc EQ 0 ) AND ( tg_zsdt0001-chave_nfe IS NOT INITIAL ).
              tg_0142-matnr = tg_zsdt0001-matnr.
              MODIFY tg_0142.
            ELSEIF tg_0142-ch_referencia IS NOT INITIAL.
              READ TABLE tg_zsdt0001 WITH KEY ch_referencia = tg_0142-ch_referencia.
              IF ( sy-subrc EQ 0 ).
                tg_0142-matnr = tg_zsdt0001-matnr.
                MODIFY tg_0142.
              ENDIF.
            ENDIF.
          ENDLOOP.

          "NFs de Complemento ----------------------------------------------------|
          tg_0142_aux[] = tg_0142[].
          DELETE tg_0142_aux WHERE chave_nfe IS INITIAL.
          IF tg_0142_aux[] IS NOT INITIAL.
            SELECT *
              FROM zlest0192 APPENDING TABLE tg_0192
               FOR ALL ENTRIES IN tg_0142_aux
             WHERE chave EQ tg_0142_aux-chave_nfe.
          ENDIF.

          tg_0142_aux[] = tg_0142[].
          DELETE tg_0142_aux WHERE chave_nff IS INITIAL.
          IF tg_0142_aux[] IS NOT INITIAL.
            SELECT *
              FROM zlest0192 APPENDING TABLE tg_0192
               FOR ALL ENTRIES IN tg_0142_aux
             WHERE chave EQ tg_0142_aux-chave_nff.
          ENDIF.

          "XML NF's ---------------------------------------------------------------|
          SELECT *
            FROM zib_nfe_dist_ter INTO TABLE tg_zib_nfe_dist_ter
             FOR ALL ENTRIES IN tg_0142
           WHERE chave_nfe = tg_0142-chave_nfe.

          SELECT *
            FROM j_1bnfdoc INTO TABLE tg_j_1bnfdoc
            FOR ALL ENTRIES IN tg_0142
           WHERE docnum EQ tg_0142-docnum.

          "Fornecedor
          SELECT *
            FROM lfa1 INTO TABLE tg_lfa1
             FOR ALL ENTRIES IN tg_0142
           WHERE lifnr = tg_0142-rm_codigo.

          "Desc. Material ---------------------------------------------------------------|
          SELECT *
            FROM makt INTO TABLE tg_makt
             FOR ALL ENTRIES IN tg_0142
           WHERE matnr = tg_0142-matnr
             AND spras = sy-langu.

        WHEN p_rc_nf. "Recepções de Carga por NF

          SELECT *
            FROM zlest0146 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0146
           WHERE a~dt_recepcao  IN p_dtrcc
             AND a~us_recepcao  IN p_usrcc
             AND a~importado    EQ abap_false
             AND a~tp_recepcao  IN ( '1', '2' ) "NF-e / NF-f
             AND a~cancel       EQ ''
             AND EXISTS ( SELECT *
                            FROM zlest0142 AS b
                           WHERE b~id_recepcao   EQ a~id_recepcao
                             AND b~bukrs_rom     IN p_bukrs
                             AND b~branch_rom    IN p_branch
                             AND b~dt_chegada    IN p_dtmov
                             AND b~cnpj_emissor  IN p_stcd1
                             AND b~dt_emissao    IN p_dtemi
                             AND b~numero        IN p_numnf
                             AND b~modal         IN p_modal
                             AND b~model         IN p_model
                          ).

          SELECT *
            FROM zlest0146 AS a APPENDING CORRESPONDING FIELDS OF TABLE tg_0146
           WHERE a~dt_recepcao  IN p_dtrcc
             AND a~us_recepcao  IN p_usrcc
             AND a~importado    EQ abap_false
             AND a~tp_recepcao  IN ( '1', '2' ) "NF-e / NF-f
             AND a~cancel       EQ ''
             AND EXISTS ( SELECT *
                            FROM zlest0142 AS b
                           WHERE b~id_recepcao   EQ a~id_recepcao
                             AND b~bukrs_ra      IN p_bukrs
                             AND b~branch_ra     IN p_branch
                             AND b~dt_chegada    IN p_dtmov
                             AND b~cnpj_emissor  IN p_stcd1
                             AND b~dt_emissao    IN p_dtemi
                             AND b~numero        IN p_numnf
                             AND b~modal         IN p_modal
                             AND b~model         IN p_model ).

          SORT tg_0146 BY id_recepcao.
          DELETE ADJACENT DUPLICATES FROM tg_0146 COMPARING id_recepcao.

          CHECK tg_0146[] IS NOT INITIAL.

          SELECT *
            FROM zlest0142 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0142
             FOR ALL ENTRIES IN tg_0146
           WHERE id_recepcao = tg_0146-id_recepcao.

          SELECT *
            FROM zlest0147 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0147
             FOR ALL ENTRIES IN tg_0146
           WHERE id_recepcao = tg_0146-id_recepcao.

          IF tg_0147[] IS NOT INITIAL.
            SELECT *
              FROM j_1bnflin INTO CORRESPONDING FIELDS OF TABLE tg_lin
               FOR ALL ENTRIES IN tg_0147
             WHERE docnum EQ tg_0147-docnum.

            SELECT *
               FROM j_1bnfdoc INTO TABLE tg_j_1bnfdoc
               FOR ALL ENTRIES IN tg_0147
             WHERE docnum EQ tg_0147-docnum.

          ENDIF.

          IF tg_lin[] IS NOT INITIAL.
            SELECT *
              FROM makt INTO CORRESPONDING FIELDS OF TABLE tg_makt
               FOR ALL ENTRIES IN tg_lin
             WHERE matnr EQ tg_lin-matnr
               AND spras EQ sy-langu.
          ENDIF.

          "Romaneios -------------------------------------------------------------|
          IF tg_0142[] IS NOT INITIAL.

            tg_0142_aux[] = tg_0142[].
            DELETE tg_0142_aux WHERE chave_nfe IS INITIAL.
            IF tg_0142_aux[] IS NOT INITIAL.
              SELECT *
                FROM zsdt0001 INTO TABLE tg_zsdt0001
                 FOR ALL ENTRIES IN tg_0142_aux
               WHERE chave_nfe EQ tg_0142_aux-chave_nfe.
            ENDIF.

            tg_0142_aux[] = tg_0142[].
            DELETE tg_0142_aux WHERE ch_referencia IS INITIAL.
            IF tg_0142_aux[] IS NOT INITIAL.
              SELECT *
                FROM zsdt0001 APPENDING TABLE tg_zsdt0001
                 FOR ALL ENTRIES IN tg_0142_aux
               WHERE ch_referencia EQ tg_0142_aux-ch_referencia.
            ENDIF.

            LOOP AT tg_0142 WHERE matnr IS INITIAL.
              READ TABLE tg_zsdt0001 WITH KEY chave_nfe = tg_0142-chave_nfe.
              IF ( sy-subrc EQ 0 ) AND ( tg_zsdt0001-chave_nfe IS NOT INITIAL ).
                tg_0142-matnr = tg_zsdt0001-matnr.
                MODIFY tg_0142.
              ELSEIF tg_0142-ch_referencia IS NOT INITIAL.
                READ TABLE tg_zsdt0001 WITH KEY ch_referencia = tg_0142-ch_referencia.
                IF ( sy-subrc EQ 0 ).
                  tg_0142-matnr = tg_zsdt0001-matnr.
                  MODIFY tg_0142.
                ENDIF.
              ENDIF.
            ENDLOOP.

            SELECT *
              FROM makt APPENDING TABLE tg_makt
               FOR ALL ENTRIES IN tg_0142
             WHERE matnr = tg_0142-matnr
               AND spras = sy-langu.




          ENDIF.

          "NFs de Complemento ----------------------------------------------------|

          tg_0142_aux[] = tg_0142[].
          DELETE tg_0142_aux WHERE chave_nfe IS INITIAL.
          IF tg_0142_aux[] IS NOT INITIAL.
            SELECT *
              FROM zlest0192 APPENDING TABLE tg_0192
               FOR ALL ENTRIES IN tg_0142_aux
             WHERE chave EQ tg_0142_aux-chave_nfe.
          ENDIF.

          tg_0142_aux[] = tg_0142[].
          DELETE tg_0142_aux WHERE chave_nff IS INITIAL.
          IF tg_0142_aux[] IS NOT INITIAL.
            SELECT *
              FROM zlest0192 APPENDING TABLE tg_0192
               FOR ALL ENTRIES IN tg_0142_aux
             WHERE chave EQ tg_0142_aux-chave_nff.
          ENDIF.

          "XML NF's ---------------------------------------------------------------|
          IF tg_0142[] IS NOT INITIAL.
            SELECT *
              FROM zib_nfe_dist_ter INTO TABLE tg_zib_nfe_dist_ter
               FOR ALL ENTRIES IN tg_0142
             WHERE chave_nfe = tg_0142-chave_nfe.
          ENDIF.

        WHEN p_vw_due. "DU-e's
          MESSAGE 'Opção não disponível!' TYPE 'S'.
          STOP.
        WHEN p_rc_due. "Recepções de Carga por DU-e
          MESSAGE 'Opção não disponível!' TYPE 'S'.
          STOP.
      ENDCASE.

    WHEN p_tp_ecg. "Entregar Carga

      CASE abap_true.
        WHEN p_vw_nf. "Notas Fiscais
        WHEN p_vw_due. "DU-e's

          SELECT *
            FROM zsdt0170 INTO TABLE tg_0170
           WHERE id_due                    IN p_iddue    "Indentificação
             AND bukrs                     IN p_bkdue
             AND numero_due                IN p_nrdue
             AND numero_ruc                IN p_nrruc
             AND codigo_urf_embarque       EQ wg_par_cct-local_codigo_urf
             AND codigo_ra_embarque        EQ wg_par_cct-local_codigo_ra
             AND status                    EQ '1'       "Registrada no Portal
             AND id_due_ref                EQ 0         "Não é DU-e de Retificação
             "Administração
             AND dt_registro               IN p_dcdue
             AND hr_registro               IN p_hcdue
             AND us_registro               IN p_ucdue.

          IF tg_0170[] IS NOT INITIAL.
            SELECT *
              FROM znom_transporte INTO TABLE tg_znom_transporte
               FOR ALL ENTRIES IN tg_0170
             WHERE id_nomeacao_tran EQ tg_0170-id_nomeacao_tran.

            SELECT *
              FROM zsdt0172 INTO TABLE tg_0172
               FOR ALL ENTRIES IN tg_0170
             WHERE id_due EQ tg_0170-id_due.

            "Documentos de Carga
            SELECT *
              FROM zsdt0181 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0181
               FOR ALL ENTRIES IN tg_0170
             WHERE numero_due = tg_0170-numero_due
               AND EXISTS ( SELECT id_entrega
                              FROM zsdt0179 AS b
                             WHERE b~id_entrega EQ a~id_entrega
                               AND b~cancel     EQ abap_false ).

          ENDIF.

        WHEN p_rc_nf.  "Recepções de Carga por NF
        WHEN p_rc_due. "Recepções de Carga por DU-e
        WHEN p_ec_due. "Entregas de Carga por DU-e

          SELECT *
            FROM zsdt0179 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0179
           WHERE a~bukrs       IN p_bukrs
             AND a~branch      IN p_branch
             AND a~dt_entrega  IN p_dtent
             AND a~us_entrega  IN p_usent
             AND a~tp_entrega  EQ '1' "Por DU-e
             AND a~cancel      EQ ''
             AND EXISTS ( SELECT *
                            FROM zsdt0180 AS b
                           WHERE b~id_entrega    EQ a~id_entrega
                             AND b~id_due        IN p_iddue
                             AND b~numero_due    IN p_nrdue
                             AND b~numero_ruc    IN p_nrruc
                          ).

          CHECK tg_0179[] IS NOT INITIAL.

          "Documentos
          SELECT *
            FROM zsdt0180 INTO CORRESPONDING FIELDS OF TABLE tg_0180
             FOR ALL ENTRIES IN tg_0179
           WHERE id_entrega = tg_0179-id_entrega.

          "Documentos de Carga
          SELECT *
            FROM zsdt0181 INTO CORRESPONDING FIELDS OF TABLE tg_0181
             FOR ALL ENTRIES IN tg_0179
           WHERE id_entrega = tg_0179-id_entrega.

          IF tg_0180[] IS NOT INITIAL .

            SELECT *
              FROM zsdt0170 INTO TABLE tg_0170
               FOR ALL ENTRIES IN tg_0180
             WHERE numero_due EQ tg_0180-numero_due
               AND status     EQ '1'   "Registrada no Portal
               AND id_due_ref EQ 0.    "Não é DU-e de Retificação

            IF tg_0170[] IS NOT INITIAL.

              SELECT *
                FROM znom_transporte INTO TABLE tg_znom_transporte
                 FOR ALL ENTRIES IN tg_0170
               WHERE id_nomeacao_tran EQ tg_0170-id_nomeacao_tran.

              SELECT *
                FROM zsdt0172 INTO TABLE tg_0172
                 FOR ALL ENTRIES IN tg_0170
               WHERE id_due EQ tg_0170-id_due.

            ENDIF.

          ENDIF.
      ENDCASE.

  ENDCASE.


ENDFORM.

FORM f_processa_dados .

  CHECK vg_not_found IS INITIAL.

  CASE abap_true.
    WHEN p_tp_rcc. "Recepcionar Carga

      CASE abap_true.

        WHEN p_vw_nf. "Notas Fiscais

          LOOP AT tg_0142.

            CLEAR: wa_saida_0100_01.

            wa_saida_0100_01-chave_nfe            =  tg_0142-chave_nfe.
            wa_saida_0100_01-chave_nff            =  tg_0142-chave_nff.
            wa_saida_0100_01-cnpj_emissor         =  tg_0142-cnpj_emissor.
            wa_saida_0100_01-cpf_emissor          =  tg_0142-cpf_emissor.
            wa_saida_0100_01-numero               =  tg_0142-numero.
            wa_saida_0100_01-cfop                 =  tg_0142-cfop.
            wa_saida_0100_01-dt_emissao           =  tg_0142-dt_emissao.
            wa_saida_0100_01-serie                =  tg_0142-serie.
            wa_saida_0100_01-model                =  tg_0142-model.
            wa_saida_0100_01-peso_chegada         =  tg_0142-peso_chegada.
            wa_saida_0100_01-peso_fiscal          =  tg_0142-peso_fiscal.
            wa_saida_0100_01-peso_rateio_origem   =  tg_0142-peso_rateio_origem.
            wa_saida_0100_01-complemento          =  tg_0142-complemento.

            IF wa_saida_0100_01-complemento IS NOT INITIAL.
              wa_saida_0100_01-nf_complemento = icon_okay.

              PERFORM f_atrib_nf_complemento USING wa_saida_0100_01-chave_nfe
                                                   wa_saida_0100_01-chave_nff
                                          CHANGING wa_saida_0100_01-nfs_complementadas.
            ENDIF.

            IF wa_saida_0100_01-chave_nfe IS NOT INITIAL.
              READ TABLE tg_zib_nfe_dist_ter WITH KEY chave_nfe = wa_saida_0100_01-chave_nfe.
              IF sy-subrc EQ 0.
                wa_saida_0100_01-cnpj_dest = tg_zib_nfe_dist_ter-destino_cnpj.
              ELSE.

                READ TABLE tg_j_1bnfdoc  WITH KEY docnum = tg_0142-docnum.
                IF sy-subrc EQ 0.

                  CASE tg_j_1bnfdoc-partyp.
                    WHEN 'C'.
                      SELECT SINGLE * FROM kna1 INTO @DATA(wg_kna1) WHERE kunnr EQ @tg_j_1bnfdoc-parid.
                      IF sy-subrc EQ 0.
                        wa_saida_0100_01-cnpj_dest = wg_kna1-stcd1.
                        wa_saida_0100_01-cpf_dest  = wg_kna1-stcd2.
                      ENDIF.

                    WHEN 'V'.
                      SELECT SINGLE * FROM lfa1 INTO @DATA(wg_lfa1) WHERE  lifnr EQ @tg_j_1bnfdoc-parid.
                      IF sy-subrc EQ 0.
                        wa_saida_0100_01-cnpj_dest = wg_kna1-stcd1.
                        wa_saida_0100_01-cpf_dest  = wg_kna1-stcd2.
                      ENDIF.

                    WHEN 'B'.
                      DATA _parid TYPE lfa1-lifnr.

                      _parid = tg_j_1bnfdoc-parid+4(4).

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = _parid
                        IMPORTING
                          output = _parid.

                      SELECT SINGLE * FROM lfa1 INTO wg_lfa1 WHERE  lifnr EQ _parid.
                      IF sy-subrc EQ 0.
                        wa_saida_0100_01-cnpj_dest = wg_kna1-stcd1.
                        wa_saida_0100_01-cpf_dest  = wg_kna1-stcd2.
                      ENDIF.
                  ENDCASE.
                ENDIF.
              ENDIF.
*** CS2019001041 - Inicio - CBRAND
              READ TABLE tg_zsdt0001 WITH KEY chave_nfe = wa_saida_0100_01-chave_nfe.
              wa_saida_0100_01-nr_tk_guardian  = tg_zsdt0001-nr_tk_guardian.
*** CS2019001041 - Fim - CBRAND

            ENDIF.

            IF tg_0142-modal = '04'.
              wa_saida_0100_01-dif_peso           =  tg_0142-peso_transbordo - tg_0142-peso_rateio_origem.
              wa_saida_0100_01-peso_transbordo    =  tg_0142-peso_transbordo. " CS2019001041 - CBRAND
            ENDIF.
            wa_saida_0100_01-dt_chegada           =  tg_0142-dt_chegada.
            wa_saida_0100_01-hr_chegada           =  tg_0142-hr_chegada.
            "WA_SAIDA_0100_01-NR_ROMANEIO          =  TG_0142-NR_ROMANEIO.
            "WA_SAIDA_0100_01-CH_REFERENCIA        =  TG_0142-CH_REFERENCIA.
            "WA_SAIDA_0100_01-DT_MOVIMENTO         =  TG_0142-DT_MOVIMENTO.
            "WA_SAIDA_0100_01-SAFRA                =  TG_0142-SAFRA.
            wa_saida_0100_01-bukrs_rom            =  tg_0142-bukrs_rom.
            wa_saida_0100_01-branch_rom           =  tg_0142-branch_rom.
            wa_saida_0100_01-local_descarga       =  tg_0142-local_descarga.
            wa_saida_0100_01-chave_cte            =  tg_0142-chave_cte.
            wa_saida_0100_01-modal                =  tg_0142-modal.
            wa_saida_0100_01-cnpj_transp          =  tg_0142-cnpj_transp.
            wa_saida_0100_01-cpf_transp           =  tg_0142-cpf_transp.
            wa_saida_0100_01-cnpj_cpf_transp_inf  =  tg_0142-cnpj_cpf_transp_inf.

            IF tg_0142-lib_envio_parc IS NOT INITIAL.
              wa_saida_0100_01-ic_lib_envio_parc = icon_okay.
            ENDIF.

            wa_saida_0100_01-rm_codigo            = tg_0142-rm_codigo.
            READ TABLE tg_lfa1 WITH KEY lifnr = tg_0142-rm_codigo.
            IF ( sy-subrc EQ 0 ) AND ( tg_0142-rm_codigo IS NOT INITIAL ).
              wa_saida_0100_01-name1 = tg_lfa1-name1.
            ENDIF.

            wa_saida_0100_01-matnr            = tg_0142-matnr.
            READ TABLE tg_makt WITH KEY matnr = tg_0142-matnr.
            IF ( sy-subrc EQ 0 ) AND ( tg_0142-matnr IS NOT INITIAL ).
              wa_saida_0100_01-maktx = tg_makt-maktx.
            ENDIF.

            wa_saida_0100_01-data_reg              =  tg_0142-data_reg.
            wa_saida_0100_01-hora_reg              =  tg_0142-hora_reg.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = wa_saida_0100_01-numero
              IMPORTING
                output = wa_saida_0100_01-numero.


            APPEND wa_saida_0100_01 TO it_saida_0100_01.

          ENDLOOP.

        WHEN p_vw_due. "DU-e's
        WHEN p_rc_nf. "Recepções de Carga por NF

          LOOP AT tg_0146.

            CLEAR: wa_saida_0100_02, tg_0142, tg_0147.

            READ TABLE tg_0142 WITH KEY id_recepcao = tg_0146-id_recepcao.

            wa_saida_0100_02-id_recepcao            =  tg_0146-id_recepcao.
            wa_saida_0100_02-bukrs                  =  tg_0146-bukrs.
            wa_saida_0100_02-branch                 =  tg_0146-branch.
            wa_saida_0100_02-cnpj_responsavel       =  tg_0146-cnpj_responsavel.
            wa_saida_0100_02-local_codigo_urf       =  tg_0146-local_codigo_urf.
            wa_saida_0100_02-local_codigo_ra        =  tg_0146-local_codigo_ra.
            wa_saida_0100_02-transportador_cnpj     =  tg_0146-transportador_cnpj.
            wa_saida_0100_02-transportador_cpf      =  tg_0146-transportador_cpf.
            wa_saida_0100_02-peso_aferido_recepcao  =  tg_0146-peso_aferido_recepcao.

            CASE tg_0146-status.
              WHEN ''.
                wa_saida_0100_02-enviada = icon_led_yellow.
              WHEN '1'.
                wa_saida_0100_02-enviada = icon_okay.
            ENDCASE.

            wa_saida_0100_02-chave_nfe             = tg_0142-chave_nfe.
            wa_saida_0100_02-chave_nff             = tg_0142-chave_nff.

            IF wa_saida_0100_02-chave_nfe IS NOT INITIAL.
              READ TABLE tg_0147 WITH KEY id_recepcao = wa_saida_0100_02-id_recepcao
                                          chave_nfe   = wa_saida_0100_02-chave_nfe.
              IF sy-subrc EQ 0.
                wa_saida_0100_02-docnum = tg_0147-docnum.
              ENDIF.
            ELSEIF wa_saida_0100_02-chave_nff IS NOT INITIAL.

              READ TABLE tg_0147 WITH KEY id_recepcao = wa_saida_0100_02-id_recepcao
                                          chave_nff   = wa_saida_0100_02-chave_nff.
              IF sy-subrc EQ 0.
                wa_saida_0100_02-docnum = tg_0147-docnum.
              ENDIF.
            ENDIF.

            wa_saida_0100_02-cnpj_emissor          = tg_0142-cnpj_emissor.
            wa_saida_0100_02-cpf_emissor           = tg_0142-cpf_emissor.
            wa_saida_0100_02-numero                = tg_0142-numero.
            wa_saida_0100_02-cfop                  = tg_0142-cfop.
            wa_saida_0100_02-dt_emissao            = tg_0142-dt_emissao.
            wa_saida_0100_02-dt_chegada            = tg_0142-dt_chegada.
            wa_saida_0100_02-peso_chegada          = tg_0142-peso_chegada.
            wa_saida_0100_02-peso_fiscal           = tg_0142-peso_fiscal.
            wa_saida_0100_02-peso_transbordo       = tg_0142-peso_transbordo.
            wa_saida_0100_02-complemento           = tg_0142-complemento.

            IF wa_saida_0100_02-complemento IS NOT INITIAL.
              wa_saida_0100_02-nf_complemento = icon_okay.

              PERFORM f_atrib_nf_complemento USING wa_saida_0100_02-chave_nfe
                                                   wa_saida_0100_02-chave_nff
                                          CHANGING wa_saida_0100_02-nfs_complementadas.
            ENDIF.

            IF wa_saida_0100_02-chave_nfe IS NOT INITIAL.
              READ TABLE tg_zib_nfe_dist_ter WITH KEY chave_nfe = wa_saida_0100_02-chave_nfe.
              IF sy-subrc EQ 0.
                wa_saida_0100_02-cnpj_dest = tg_zib_nfe_dist_ter-destino_cnpj.
              ELSE.
                READ TABLE tg_j_1bnfdoc  WITH KEY docnum = wa_saida_0100_02-docnum.
                IF sy-subrc EQ 0.

                  CASE tg_j_1bnfdoc-partyp.
                    WHEN 'C'.
                      SELECT SINGLE * FROM kna1 INTO wg_kna1 WHERE kunnr EQ tg_j_1bnfdoc-parid.
                      IF sy-subrc EQ 0.
                        wa_saida_0100_02-cnpj_dest = wg_kna1-stcd1.
                        wa_saida_0100_02-cpf_dest  = wg_kna1-stcd2.
                      ENDIF.

                    WHEN 'V'.
                      SELECT SINGLE * FROM lfa1 INTO wg_lfa1 WHERE  lifnr EQ tg_j_1bnfdoc-parid.
                      IF sy-subrc EQ 0.
                        wa_saida_0100_02-cnpj_dest = wg_kna1-stcd1.
                        wa_saida_0100_02-cpf_dest = wg_kna1-stcd2.
                      ENDIF.

                    WHEN 'B'.
                      CLEAR _parid.

                      _parid = tg_j_1bnfdoc-parid+4(4).

                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
                        EXPORTING
                          input  = _parid
                        IMPORTING
                          output = _parid.

                      SELECT SINGLE * FROM lfa1 INTO wg_lfa1 WHERE  lifnr EQ _parid.
                      IF sy-subrc EQ 0.
                        wa_saida_0100_02-cnpj_dest = wg_kna1-stcd1.
                        wa_saida_0100_02-cpf_dest  = wg_kna1-stcd2.
                      ENDIF.
                  ENDCASE.
                ENDIF.
              ENDIF.
            ENDIF.

            wa_saida_0100_02-lib_envio_parc        = tg_0142-lib_envio_parc.
            wa_saida_0100_02-dt_lib_envio_parc     = tg_0142-dt_lib_envio_parc.
            wa_saida_0100_02-hr_lib_envio_parc     = tg_0142-hr_lib_envio_parc.
            wa_saida_0100_02-us_lib_envio_parc     = tg_0142-us_lib_envio_parc.
            wa_saida_0100_02-dt_recepcao           = tg_0146-dt_recepcao.
            wa_saida_0100_02-hr_recepcao           = tg_0146-hr_recepcao.
            wa_saida_0100_02-us_recepcao           = tg_0146-us_recepcao.

            IF tg_0142-lib_envio_parc IS NOT INITIAL.
              wa_saida_0100_02-ic_lib_envio_parc = icon_okay.
            ENDIF.

            READ TABLE tg_lin WITH KEY docnum = wa_saida_0100_02-docnum.

            IF ( sy-subrc EQ 0 ) AND ( tg_lin-docnum IS NOT INITIAL ).
              wa_saida_0100_02-matnr = tg_lin-matnr.

              READ TABLE tg_makt WITH KEY matnr = tg_lin-matnr.
              IF sy-subrc EQ 0.
                wa_saida_0100_02-maktx = tg_makt-maktx.
              ENDIF.
            ENDIF.

            IF ( wa_saida_0100_02-maktx IS INITIAL ) AND ( tg_0142-matnr IS NOT INITIAL ).
              wa_saida_0100_02-matnr = tg_0142-matnr.

              READ TABLE tg_makt WITH KEY matnr = tg_0142-matnr.
              IF ( sy-subrc EQ 0 ) AND ( tg_0142-matnr IS NOT INITIAL ).
                wa_saida_0100_02-maktx = tg_makt-maktx.
              ENDIF.
            ENDIF.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
              EXPORTING
                input  = wa_saida_0100_02-numero
              IMPORTING
                output = wa_saida_0100_02-numero.

            IF wa_saida_0100_02-docnum IS INITIAL.
              wa_saida_0100_02-rowcolor = 'C600'.
            ENDIF.

            APPEND wa_saida_0100_02 TO it_saida_0100_02.

          ENDLOOP.

          SORT it_saida_0100_02 BY id_recepcao DESCENDING.

        WHEN p_rc_due. "Recepções de Carga por DU-e
        WHEN p_ec_due. "Entregas de Carga por DU-e
      ENDCASE.

    WHEN p_tp_ecg. "Entregar Carga

      CASE abap_true.
        WHEN p_vw_nf. "Notas Fiscais
        WHEN p_vw_due. "DU-e's

          LOOP AT tg_0170.

            CLEAR: wa_saida_0100_03.

            wa_saida_0100_03-id_due                          =   tg_0170-id_due.
            wa_saida_0100_03-bukrs                           =   tg_0170-bukrs.
            wa_saida_0100_03-land1                           =   tg_0170-land1.
            wa_saida_0100_03-regio                           =   tg_0170-regio.
            wa_saida_0100_03-numero_due                      =   tg_0170-numero_due.
            wa_saida_0100_03-numero_ruc                      =   tg_0170-numero_ruc.

            READ TABLE tg_znom_transporte WITH KEY id_nomeacao_tran =  tg_0170-id_nomeacao_tran.
            IF ( sy-subrc EQ 0 ) AND ( tg_0170-id_nomeacao_tran IS NOT INITIAL ).
              wa_saida_0100_03-id_nomeacao_tran              = tg_znom_transporte-id_nomeacao_tran.
              wa_saida_0100_03-ds_nome_transpor              = tg_znom_transporte-ds_nome_transpor.
            ENDIF.

            LOOP AT tg_0181 WHERE numero_due  = tg_0170-numero_due.
              wa_saida_0100_03-peso_liq_total       = tg_0181-peso_bruto_total.
              wa_saida_0100_03-peso_bruto_entregue  = tg_0181-peso_bruto_entregue.
            ENDLOOP.

            wa_saida_0100_03-saldo_disponivel = wa_saida_0100_03-peso_liq_total - wa_saida_0100_03-peso_bruto_entregue.

            wa_saida_0100_03-codigo_urf_despacho             =   tg_0170-codigo_urf_despacho.
            wa_saida_0100_03-codigo_ra_despacho              =   tg_0170-codigo_ra_despacho.
            wa_saida_0100_03-tp_cod_local_despacho           =   tg_0170-tp_cod_local_despacho.
            wa_saida_0100_03-cnpj_cpf_resp_loc_desp          =   tg_0170-cnpj_cpf_resp_loc_desp.
            wa_saida_0100_03-local_despacho_longitude        =   tg_0170-local_despacho_longitude.
            wa_saida_0100_03-local_despacho_latitude         =   tg_0170-local_despacho_latitude.
            wa_saida_0100_03-local_despacho_end              =   tg_0170-local_despacho_end.
            wa_saida_0100_03-forma_exportacao                =   tg_0170-forma_exportacao.
            wa_saida_0100_03-caso_especial_transporte        =   tg_0170-caso_especial_transporte.
            wa_saida_0100_03-situacao_especial               =   tg_0170-situacao_especial.
            wa_saida_0100_03-observacoes_gerais              =   tg_0170-observacoes_gerais .
            wa_saida_0100_03-moeda_cambio                    =   tg_0170-moeda_cambio.
            wa_saida_0100_03-motivo                          =   tg_0170-motivo.
            wa_saida_0100_03-cnpj_declarante                 =   tg_0170-cnpj_declarante.
            wa_saida_0100_03-codigo_urf_embarque             =   tg_0170-codigo_urf_embarque.
            wa_saida_0100_03-codigo_ra_embarque              =   tg_0170-codigo_ra_embarque.
            wa_saida_0100_03-tp_due                          =   tg_0170-tp_due.
            wa_saida_0100_03-id_due_ref                      =   tg_0170-id_due_ref.
            wa_saida_0100_03-status                          =   tg_0170-status.
            wa_saida_0100_03-dt_registro                     =   tg_0170-dt_registro.
            wa_saida_0100_03-hr_registro                     =   tg_0170-hr_registro.
            wa_saida_0100_03-us_registro                     =   tg_0170-us_registro.
            wa_saida_0100_03-dt_modificacao                  =   tg_0170-dt_modificacao.
            wa_saida_0100_03-hr_modificacao                  =   tg_0170-hr_modificacao.
            wa_saida_0100_03-us_modificacao                  =   tg_0170-us_modificacao.

            APPEND wa_saida_0100_03 TO it_saida_0100_03.

          ENDLOOP.

        WHEN p_rc_nf. "Recepções de Carga por NF
        WHEN p_rc_due. "Recepções de Carga por DU-e
        WHEN p_ec_due. "Entregas de Carga por DU-e

          LOOP AT tg_0179.

            CLEAR: wa_saida_0100_04.

            wa_saida_0100_04-id_entrega                =   tg_0179-id_entrega.
            wa_saida_0100_04-tp_entrega                =   tg_0179-tp_entrega.
            wa_saida_0100_04-bukrs                     =   tg_0179-bukrs.
            wa_saida_0100_04-branch                    =   tg_0179-branch.
            wa_saida_0100_04-cnpj_responsavel          =   tg_0179-cnpj_responsavel.
            wa_saida_0100_04-local_codigo_urf          =   tg_0179-local_codigo_urf.
            wa_saida_0100_04-local_codigo_ra           =   tg_0179-local_codigo_ra.
            wa_saida_0100_04-local_longitude           =   tg_0179-local_longitude.
            wa_saida_0100_04-local_latitude            =   tg_0179-local_latitude.
            wa_saida_0100_04-rec_cnpj                  =   tg_0179-rec_cnpj.
            wa_saida_0100_04-rec_cpf                   =   tg_0179-rec_cpf.
            wa_saida_0100_04-rec_nome_estrangeiro      =   tg_0179-rec_nome_estrangeiro.
            wa_saida_0100_04-rec_via_transporte        =   tg_0179-rec_via_transporte.
            wa_saida_0100_04-rec_baldeacao_transbordo  =   tg_0179-rec_baldeacao_transbordo.
            wa_saida_0100_04-peso_aferido              =   tg_0179-peso_aferido.
            wa_saida_0100_04-status                    =   tg_0179-status.
            wa_saida_0100_04-dt_entrega                =   tg_0179-dt_entrega.
            wa_saida_0100_04-hr_entrega                =   tg_0179-hr_entrega.
            wa_saida_0100_04-us_entrega                =   tg_0179-us_entrega.

            DATA(v_documentos) = 0.
            LOOP AT tg_0180 WHERE id_entrega = wa_saida_0100_04-id_entrega.
              ADD 1 TO v_documentos.
            ENDLOOP.

            IF v_documentos = 1.
              READ TABLE tg_0180 WITH KEY id_entrega = wa_saida_0100_04-id_entrega.
              IF sy-subrc EQ 0.

                wa_saida_0100_04-numero_due = tg_0180-numero_due.

*** BUG - 49846 - Camila Brand - Inicio
                wa_saida_0100_04-ds_nome_transpor = tg_0180-ds_nome_transpor.
                wa_saida_0100_04-cnpj_declarante = tg_0180-cnpj_declarante.
*** BUG - 49846 - Camila Brand - Fim

                READ TABLE tg_0170 WITH KEY numero_due = tg_0180-numero_due.

                IF sy-subrc EQ 0.
                  READ TABLE tg_znom_transporte WITH KEY id_nomeacao_tran =  tg_0170-id_nomeacao_tran.
                  IF ( sy-subrc EQ 0 ) AND ( tg_0170-id_nomeacao_tran IS NOT INITIAL ).
                    wa_saida_0100_04-id_nomeacao_tran   = tg_znom_transporte-id_nomeacao_tran.
                    wa_saida_0100_04-ds_nome_transpor   = tg_znom_transporte-ds_nome_transpor.
                  ENDIF.

                  wa_saida_0100_04-bukrs_due            = tg_0170-bukrs.
                  wa_saida_0100_04-numero_due           = tg_0170-numero_due.
                  wa_saida_0100_04-numero_ruc           = tg_0170-numero_ruc.
                  wa_saida_0100_04-cnpj_declarante      = tg_0170-cnpj_declarante.
                ENDIF.

              ENDIF.
            ENDIF.

            APPEND wa_saida_0100_04 TO it_saida_0100_04.

          ENDLOOP.

      ENDCASE.

  ENDCASE.


ENDFORM.

FORM f_recepcionar_carga_nf.

  DATA: v_id_recepcao        TYPE zlest0146-id_recepcao,
        v_motivo_nao_pesagem TYPE zlest0146-motivo_nao_pesagem,
        v_observacoes_gerais TYPE zlest0146-observacoes_gerais.

  DATA: tg_zlest0192  TYPE TABLE OF zlest0192 WITH HEADER LINE.

  DATA: zcl_cct_recepcao_carga TYPE REF TO zcl_cct_recepcao_carga.

  IF ( vg_st_logon = c_disconnected ) OR ( zcl_token_siscomex IS INITIAL ).
    MESSAGE 'Autenticação não foi realizada!' TYPE 'S'.
    EXIT.
  ENDIF.

  CREATE OBJECT zcl_cct_recepcao_carga.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente gerar Recepção de Carga para a(s) NF(s) selecionada(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_01 INTO wa_saida_0100_01 INDEX wa_sel_rows-index.

    CHECK sy-subrc = 0.

    FREE zcl_cct_recepcao_carga.
    CREATE OBJECT zcl_cct_recepcao_carga.

    zcl_cct_recepcao_carga->zif_cadastro~novo_registro( ).

    IF wa_saida_0100_01-chave_nfe IS NOT INITIAL.
      zcl_cct_recepcao_carga->set_tp_recepcao( '1' ). "NF-e
    ELSEIF wa_saida_0100_01-chave_nff IS NOT INITIAL.
      zcl_cct_recepcao_carga->set_tp_recepcao( '2' ). "NF-f
    ENDIF.

    zcl_cct_recepcao_carga->set_bukrs( wg_par_cct-bukrs_ra ).
    zcl_cct_recepcao_carga->set_branch( wg_par_cct-branch_ra ).
    zcl_cct_recepcao_carga->set_cnpj_responsavel( ).
    zcl_cct_recepcao_carga->set_local_codigo_urf( wg_par_cct-local_codigo_urf ).
    zcl_cct_recepcao_carga->set_local_codigo_ra( wg_par_cct-local_codigo_ra ).
    zcl_cct_recepcao_carga->set_transportador_cnpj( wa_saida_0100_01-cnpj_transp ).
    zcl_cct_recepcao_carga->set_transportador_cpf( wa_saida_0100_01-cpf_transp ).

    IF wa_saida_0100_01-complemento IS NOT INITIAL.

      CLEAR: tg_zlest0192[], v_motivo_nao_pesagem, v_observacoes_gerais.

      IF wa_saida_0100_01-chave_nfe IS NOT INITIAL.

        SELECT *
          FROM zlest0192 INTO TABLE tg_zlest0192
         WHERE chave EQ wa_saida_0100_01-chave_nfe.

      ELSEIF wa_saida_0100_01-chave_nff IS NOT INITIAL.

        SELECT *
          FROM zlest0192 INTO TABLE tg_zlest0192
         WHERE chave EQ wa_saida_0100_01-chave_nff.

      ENDIF.

      LOOP AT tg_zlest0192.
        IF v_motivo_nao_pesagem IS INITIAL .
          CONCATENATE 'Complemento NF(s):' tg_zlest0192-chave_comp INTO v_motivo_nao_pesagem SEPARATED BY space.
        ELSE.
          CONCATENATE v_motivo_nao_pesagem ',' tg_zlest0192-chave_comp INTO v_motivo_nao_pesagem SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF v_motivo_nao_pesagem IS NOT INITIAL.
        zcl_cct_recepcao_carga->set_motivo_nao_pesagem( i_motivo_nao_pesagem = v_motivo_nao_pesagem ).

        v_observacoes_gerais = v_motivo_nao_pesagem.
      ENDIF.

      IF v_observacoes_gerais IS NOT INITIAL.
        zcl_cct_recepcao_carga->set_observacoes_gerais( i_observacoes_gerais = v_observacoes_gerais ).
      ENDIF.

    ELSE.
      zcl_cct_recepcao_carga->set_peso_aferido_recepcao( wa_saida_0100_01-peso_chegada ).
    ENDIF.


    IF wa_saida_0100_01-chave_nfe IS NOT INITIAL.

      zcl_cct_recepcao_carga->add_nfe(
        EXPORTING
          i_chave_nfe   = wa_saida_0100_01-chave_nfe
          i_complemento = wa_saida_0100_01-complemento ).

    ELSEIF wa_saida_0100_01-chave_nff IS NOT INITIAL.

      zcl_cct_recepcao_carga->add_nff(
        EXPORTING
          i_chave_nff   =  wa_saida_0100_01-chave_nff
          i_complemento = wa_saida_0100_01-complemento ).

    ENDIF.


    zcl_cct_recepcao_carga->set_token( zcl_token_siscomex ). "Set token para Validação.
    zcl_cct_recepcao_carga->zif_cadastro~gravar_registro( RECEIVING i_gravou = DATA(_gravou) ).

    IF _gravou IS INITIAL.
      RETURN.
    ENDIF.

    v_id_recepcao = zcl_cct_recepcao_carga->get_id_recepcao( ).
    PERFORM f_processa_recepcao USING v_id_recepcao
                                      abap_false.

  ENDLOOP.

  MESSAGE 'Carga(s) recepcionada(s) com sucesso!' TYPE 'S'.


ENDFORM.

FORM f_autenticar.

  DATA: v_time_st_lim_aux TYPE timestamp,
        v_time_dif        TYPE timestamp,
        v_time_fim        TYPE timestamp,
        v_time_ini        TYPE timestamp,
        utc               TYPE tzonref-tzone VALUE 'UTC'.

  CREATE OBJECT zcl_token_siscomex.

  zcl_token_siscomex->zif_cadastro~novo_registro( ).
  zcl_token_siscomex->set_bukrs( p_bukrs-low ).
  zcl_token_siscomex->set_role_type( 'DEPOSIT' ). "Depositário

  zcl_token_siscomex->zif_cadastro~gravar_registro( RECEIVING i_gravou = DATA(_gravou) ).

  CHECK _gravou IS NOT INITIAL.

  vg_st_logon = c_connected.

  PERFORM f_define_time_lim.
  PERFORM f_atualiza_time.

  go_clock->interval = vg_time_interval.
  CALL METHOD go_clock->run.

  LEAVE TO SCREEN 0100.


ENDFORM.

FORM f_recepcionar_carga.

  CASE 'X'.
    WHEN p_vw_nf.
      PERFORM f_recepcionar_carga_nf.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.

FORM f_define_time_lim.

  DATA: v_date TYPE erdat,
        v_time TYPE erzet.

  DATA: v_utc TYPE tzonref-tzone VALUE 'UTC'.

  v_date = sy-datum.
  v_time = sy-uzeit + 3600.

  IF sy-uzeit(2) = '23' AND v_time(02) = '00'.
    v_date = v_date + 1.
  ENDIF.

  CONVERT DATE v_date TIME v_time INTO TIME STAMP vg_tst_lim TIME ZONE v_utc.

ENDFORM.

FORM f_atualiza_time.

  DATA: v_utc   TYPE tzonref-tzone VALUE 'UTC',
        lv_diff TYPE timedura.

  CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP vg_tst_atual TIME ZONE v_utc.

  IF ( vg_tst_lim > vg_tst_atual ).
    vg_tst_dif = vg_tst_lim - vg_tst_atual.

    CALL METHOD cl_foev_time_func_brf=>difference_timestamp
      EXPORTING
        iv_timestamp_1 = vg_tst_atual
        iv_timestamp_2 = vg_tst_lim
        iv_timezone_1  = 'CET'
        iv_timezone_2  = 'CET'
        iv_timeunit    = 'SECOND'
        iv_fac_cal     = 'CH'
      IMPORTING
        ev_difference  = lv_diff.

    vg_time_lim = lv_diff.
  ENDIF.

ENDFORM.


FORM f_conectar_server .

  BREAK-POINT.

  DATA http_client  TYPE REF TO if_http_client.
  DATA xml_return   TYPE REF TO cl_xml_document.
  DATA return_code  TYPE i.
  DATA e_resultado  TYPE string.
  DATA v_tamanho    TYPE string.
  DATA v_url        TYPE ui_src_url.
  DATA v_ssl_id     TYPE ssfapplssl.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = sy-tabix
      text       = text-004.


  v_url = 'https://portalunico.siscomex.gov.br/portal/api/autenticar'.
  v_ssl_id = 'Z_CRT1'.
  v_tamanho = '0'.

  "//Call service
  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url                = CONV #( v_url )
      ssl_id             = v_ssl_id
    IMPORTING
      client             = http_client
    EXCEPTIONS
      argument_not_found = 1
      plugin_not_active  = 2
      internal_error     = 3
      OTHERS             = 4.

  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = '~request_method'
      value = 'POST'.

  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = '~server_protocol'
      value = 'HTTP/1.1'.

  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = 'Content-Type'
      value = 'application/xml; charset=UTF-8'.

  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = 'Content-Length'
      value = v_tamanho.

  CALL METHOD http_client->request->set_header_field
    EXPORTING
      name  = 'Role-Type'
      value = 'DEPOSIT'.

  CALL METHOD http_client->send
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  CALL METHOD http_client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  "//Check return content
  CREATE OBJECT xml_return.

  CALL METHOD xml_return->parse_string
    EXPORTING
      stream = http_client->response->get_cdata( ).

  http_client->response->get_status( IMPORTING code = return_code ).

  e_resultado = http_client->response->get_cdata( ).

  "RETURN_CODE = "Código de Retorno.
  "E_RESULTADO = "Data Retorno

ENDFORM.

FORM f_cancel_recep_carga.

  DATA: zcl_cct_recepcao_carga TYPE REF TO zcl_cct_recepcao_carga.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente cancelar a(s) Recepção(ões) de Carga(s) selecionada(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_02 INTO wa_saida_0100_02 INDEX wa_sel_rows-index.

    CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100_02-id_recepcao IS NOT INITIAL ).

    FREE zcl_cct_recepcao_carga.
    CREATE OBJECT zcl_cct_recepcao_carga
      EXPORTING
        i_id_recepcao = wa_saida_0100_02-id_recepcao.

    zcl_cct_recepcao_carga->cancelar_recepcao( RECEIVING i_cancelada = DATA(_cancelada) ).

    IF _cancelada IS INITIAL.
      RETURN.
    ENDIF.

  ENDLOOP.

  MESSAGE 'Recepção de Carga(s) canceladas(s) com sucesso!' TYPE 'S'.


ENDFORM.

FORM f_envio_carga .

  DATA: zcl_cct_recepcao_carga TYPE REF TO zcl_cct_recepcao_carga.

  IF ( vg_st_logon = c_disconnected ) OR ( zcl_token_siscomex IS INITIAL ).
    MESSAGE 'Autenticação não foi realizada!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente enviar as Recepções de Carga selecionada(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_02 INTO wa_saida_0100_02 INDEX wa_sel_rows-index.

    CHECK sy-subrc = 0.

    FREE zcl_cct_recepcao_carga.
    CREATE OBJECT zcl_cct_recepcao_carga
      EXPORTING
        i_id_recepcao = wa_saida_0100_02-id_recepcao.

    zcl_cct_recepcao_carga->set_token( zcl_token_siscomex ). "Set token para Validação.
    zcl_cct_recepcao_carga->recepcionar_carga( RECEIVING i_recepcionada = DATA(_recepcionada) ).

    IF _recepcionada IS INITIAL.
      RETURN.
    ENDIF.

  ENDLOOP.

  MESSAGE 'Carga(s) recepcionada(s) com sucesso!' TYPE 'S'.

ENDFORM.

FORM f_lib_nfe_ajuste .

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente gerar Recepção de Carga para a(s) NF(s) selecionada(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_01 INTO wa_saida_0100_01 INDEX wa_sel_rows-index.

    CHECK ( sy-subrc = 0 ).

    SELECT SINGLE *
      FROM zlest0142 INTO @DATA(_wl_0142)
     WHERE chave_nfe = @wa_saida_0100_01-chave_nfe
       AND chave_nff = @wa_saida_0100_01-chave_nff.

    CHECK sy-subrc EQ 0.

    zcl_cct_control_nf=>remover_nf_cct( i_zlest0142 = _wl_0142 ).

  ENDLOOP.

  MESSAGE 'NF(s) disponibilizadas(s) para ajuste com sucesso!' TYPE 'S'.

ENDFORM.

FORM f_define_cnpj_cpf_transp .

  IF p_vw_nf IS INITIAL.
    MESSAGE 'Selecione a visão de Notas Fiscais!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL SCREEN 0101 STARTING AT 30 05 ENDING AT 01 01.

  PERFORM f_refresh_alv USING 0100.

ENDFORM.

FORM f_down_xml_rc_nfe .

  TYPES: BEGIN OF ty_xml,
           xml TYPE string,
         END OF ty_xml.

  DATA: zcl_cct_recepcao_carga TYPE REF TO zcl_cct_recepcao_carga.

  DATA: v_xml_rec   TYPE string,
        v_name_file TYPE string,
        xvalor      TYPE string,
        wa_xml      TYPE ty_xml,
        it_xml      TYPE STANDARD TABLE OF ty_xml.

  DATA: lv_filename TYPE string,
        lv_fullpath TYPE string,
        lv_path     TYPE string,
        lv_action   TYPE i,
        lv_file     TYPE string.

  DEFINE conc_xml.
    CLEAR: XVALOR.
    XVALOR = &1.
    CONCATENATE V_XML_REC XVALOR INTO V_XML_REC.
  END-OF-DEFINITION.

  CLEAR: it_sel_rows[], wa_sel_rows, v_xml_rec.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  conc_xml  '<recepcoesNFE xsi:schemaLocation="http://www.pucomex.serpro.gov.br/cct RecepcaoNFE.xsd"'.
  conc_xml               ' xmlns="http://www.pucomex.serpro.gov.br/cct"'.
  conc_xml               ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_02 INTO wa_saida_0100_02 INDEX wa_sel_rows-index.

    CHECK sy-subrc = 0.

    FREE zcl_cct_recepcao_carga.
    CREATE OBJECT zcl_cct_recepcao_carga
      EXPORTING
        i_id_recepcao = wa_saida_0100_02-id_recepcao.

    zcl_cct_recepcao_carga->get_xml_recepcao( RECEIVING e_xml_rec_nf = DATA(_xml) ).

    conc_xml _xml.

  ENDLOOP.

  conc_xml  '</recepcoesNFE>'.

  CLEAR: it_xml[].
  wa_xml-xml = v_xml_rec.
  APPEND wa_xml TO it_xml.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
    EXPORTING
      window_title      = 'Selecione o dirétório'
      default_extension = 'xml'
      default_file_name = lv_file
      file_filter       = '*.XML'
    CHANGING
      filename          = lv_filename
      path              = lv_path
      fullpath          = lv_fullpath
      user_action       = lv_action
    EXCEPTIONS
      cntl_error        = 1
      error_no_gui      = 2
      OTHERS            = 3.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
      filename                = lv_fullpath
    TABLES
      data_tab                = it_xml
    EXCEPTIONS
      file_write_error        = 1
      no_batch                = 2
      gui_refuse_filetransfer = 3
      invalid_type            = 4
      no_authority            = 5
      unknown_error           = 6
      header_not_allowed      = 7
      separator_not_allowed   = 8
      filesize_not_allowed    = 9
      header_too_long         = 10
      dp_error_create         = 11
      dp_error_send           = 12
      dp_error_write          = 13
      unknown_dp_error        = 14
      access_denied           = 15
      dp_out_of_memory        = 16
      disk_full               = 17
      dp_timeout              = 18
      file_not_found          = 19
      dataprovider_exception  = 20
      control_flush_error     = 21
      OTHERS                  = 22.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

FORM f_logout .

  IF zcl_token_siscomex IS NOT INITIAL.
    FREE zcl_token_siscomex.
  ENDIF.

  vg_st_logon = c_disconnected.

  CLEAR: vg_time_lim.

  MESSAGE 'Desconectado com sucesso!' TYPE 'S'.

  LEAVE TO SCREEN 0100.

ENDFORM.

FORM f_log_cancel_recepcao.

  CLEAR: tg_0146_cancel[].

  SELECT *
    FROM zlest0146 AS a INTO CORRESPONDING FIELDS OF TABLE tg_0146_cancel
   WHERE a~dt_recepcao  IN p_dtrcc
     AND a~us_recepcao  IN p_usrcc
     AND a~importado    EQ abap_false
     AND a~tp_recepcao  IN ( '1' , '2' ) "NF-e / NF-f
     AND a~cancel       EQ 'X'.

  CHECK tg_0146_cancel[] IS NOT INITIAL.

  SELECT *
    FROM zlest0147 INTO TABLE @DATA(tg_0147_cancel)
     FOR ALL ENTRIES IN @tg_0146_cancel
   WHERE id_recepcao EQ @tg_0146_cancel-id_recepcao.

  CHECK tg_0147_cancel[] IS NOT INITIAL.

  SELECT *
    FROM zlest0142 INTO TABLE @DATA(tg_0142_cancel)
     FOR ALL ENTRIES IN @tg_0147_cancel
   WHERE chave_nfe EQ @tg_0147_cancel-chave_nfe
     AND chave_nff EQ @tg_0147_cancel-chave_nff.

  LOOP AT tg_0146_cancel.

    READ TABLE tg_0147_cancel INTO DATA(wl_0147_cancel) WITH KEY id_recepcao = tg_0146_cancel-id_recepcao.
    IF sy-subrc = 0.

      tg_0146_cancel-chave_nfe   =  wl_0147_cancel-chave_nfe.
      tg_0146_cancel-chave_nff   =  wl_0147_cancel-chave_nff.

      READ TABLE tg_0142_cancel INTO DATA(wl_0142_cancel) WITH KEY chave_nfe = wl_0147_cancel-chave_nfe
                                                                   chave_nff = wl_0147_cancel-chave_nff.
      IF sy-subrc = 0.
        tg_0146_cancel-cnpj_emissor  =  wl_0142_cancel-cnpj_emissor.
        tg_0146_cancel-numero        =  wl_0142_cancel-numero.
        tg_0146_cancel-dt_emissao    =  wl_0142_cancel-dt_emissao.
      ENDIF.
    ENDIF.

    MODIFY tg_0146_cancel.
  ENDLOOP.

  PERFORM f_montar_layout_log_cancel.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 130
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_0146_cancel.


ENDFORM.

FORM f_doc_rat_recepcao.

  DATA: tg_0168_out TYPE TABLE OF zlest0168 WITH HEADER LINE.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  IF lines( it_sel_rows[] ) > 1.
    MESSAGE 'Selecione somente uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.
  CHECK sy-subrc EQ 0.

  READ TABLE it_saida_0100_02 INTO wa_saida_0100_02 INDEX wa_sel_rows-index.

  CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100_02-id_recepcao IS NOT INITIAL ).

  CLEAR: tg_0168_out[].

  SELECT *
    FROM zlest0168 INTO CORRESPONDING FIELDS OF TABLE tg_0168_out
   WHERE id_recepcao  EQ wa_saida_0100_02-id_recepcao.

  CHECK tg_0168_out[] IS NOT INITIAL.

  REFRESH estrutura.
  PERFORM f_montar_estrutura USING:
     01  ''   ''            'TG_0168_OUT' 'ID_RECEPCAO'   'Id.Rec.'         '10' '',
     02  ''   ''            'TG_0168_OUT' 'CHAVE_NFE'     'Chave NF-e'      '44' '',
     03  ''   ''            'TG_0168_OUT' 'CHAVE_NFF'     'Chave NF-f'      '37' '',
     04  ''   ''            'TG_0168_OUT' 'DOCNUM'        'Docnum'          '10' '',
     05  ''   ''            'TG_0168_OUT' 'PESO_AFERIDO'  'Peso Aferido'    '13' ''.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat           = estrutura[]
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 135
      i_screen_end_line     = 13
    TABLES
      t_outtab              = tg_0168_out.


ENDFORM.

FORM f_reprocessar_mov_rom .

  CLEAR: vg_dtini_proc_rom.

  CALL SCREEN 0102 STARTING AT 30 05 ENDING AT 01 01.

  PERFORM f_refresh_alv USING 0100.

ENDFORM.

FORM f_lib_envio_carga_parc .

  IF p_vw_nf IS INITIAL.
    MESSAGE 'Selecione a visão de Notas Fiscais!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja liberar o envio parcial para a(s) NF(s) selecionada(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_01 INTO wa_saida_0100_01 INDEX wa_sel_rows-index.

    CHECK ( sy-subrc = 0 ).

    SELECT SINGLE *
      FROM zlest0142 INTO @DATA(_wl_0142)
     WHERE chave_nfe = @wa_saida_0100_01-chave_nfe
       AND chave_nff = @wa_saida_0100_01-chave_nff.

    CHECK ( sy-subrc = 0 ) AND ( _WL_0142-PESO_TRANSBORDO <> _wl_0142-peso_rateio_origem ) AND ( _wl_0142-modal = '04' ).

    IF _wl_0142-lib_envio_parc IS INITIAL.
      _wl_0142-lib_envio_parc     = 'X'.
      _wl_0142-dt_lib_envio_parc  = sy-datum.
      _wl_0142-hr_lib_envio_parc  = sy-uzeit.
      _wl_0142-us_lib_envio_parc  = sy-uname.
    ELSE.
      CHECK _wl_0142-id_recepcao IS INITIAL.
      CLEAR: _wl_0142-dt_lib_envio_parc, _wl_0142-hr_lib_envio_parc, _wl_0142-us_lib_envio_parc, _wl_0142-lib_envio_parc.
    ENDIF.

    MODIFY zlest0142 FROM _wl_0142.

  ENDLOOP.

  PERFORM f_refresh_alv USING 0100.

ENDFORM.

FORM f_nova_entrega.

  CASE abap_true.
    WHEN p_vw_due.
      PERFORM f_nova_entrega_due.
  ENDCASE.

ENDFORM.

FORM f_nova_entrega_due.

  DATA: gt_zsdt0170 TYPE TABLE OF zsdt0170,
        wl_zsdt0170 TYPE zsdt0170.

  CLEAR: it_sel_rows[], wa_sel_rows, gt_zsdt0170[].

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_03 INTO wa_saida_0100_03 INDEX wa_sel_rows-index.

    CHECK sy-subrc = 0.

    MOVE-CORRESPONDING wa_saida_0100_03 TO wl_zsdt0170.
    APPEND wl_zsdt0170 TO gt_zsdt0170.
  ENDLOOP.

  FREE zcl_cct_entrega_carga.
  CREATE OBJECT zcl_cct_entrega_carga.

  CLEAR: wa_registro_entrega.
  wa_registro_entrega-modo                         = c_entrega_novo.
  wa_registro_entrega-tp_entrega                   = '1'. "Por DU-e/RUC
  wa_registro_entrega-bukrs                        = wg_par_cct-bukrs.
  wa_registro_entrega-branch                       = wg_par_cct-branch.
  wa_registro_entrega-local_codigo_urf             = wg_par_cct-local_codigo_urf.
  wa_registro_entrega-local_codigo_ra              = wg_par_cct-local_codigo_ra.
  wa_registro_entrega-rec_via_transporte           = '01'. "Marítima
  wa_registro_entrega-rec_baldeacao_transbordo     = 'N'.
  wa_registro_entrega-tipo_carga                   = '2'.
  wa_registro_entrega-tipo_granel                  = '02'.
  wa_registro_entrega-unid_medida                  = 'KG'.

  zcl_cct_entrega_carga->registro_entrega( EXPORTING i_registro_entrega = wa_registro_entrega
                                                     i_zsdt0170         = gt_zsdt0170 ).

ENDFORM.

FORM f_entregar_carga.

  DATA: zcl_cct_entrega_carga TYPE REF TO zcl_cct_entrega_carga.

  IF ( vg_st_logon = c_disconnected ) OR ( zcl_token_siscomex IS INITIAL ).
    MESSAGE 'Autenticação não foi realizada!' TYPE 'S'.
    EXIT.
  ENDIF.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente enviar as Entregas de Carga selecionada(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_04 INTO wa_saida_0100_04 INDEX wa_sel_rows-index.

    CHECK sy-subrc = 0.

    FREE zcl_cct_entrega_carga.
    CREATE OBJECT zcl_cct_entrega_carga
      EXPORTING
        i_id_entrega = wa_saida_0100_04-id_entrega.

    zcl_cct_entrega_carga->set_token( zcl_token_siscomex ). "Set token para Validação.
    zcl_cct_entrega_carga->enviar_entrega( RECEIVING r_enviada = DATA(_enviada) ).

    IF _enviada IS INITIAL.
      RETURN.
    ENDIF.

  ENDLOOP.

  MESSAGE 'Carga(s) Entregues(s) com sucesso!' TYPE 'S'.

ENDFORM.


FORM f_view_entrega .

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE 'Selecione uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF lines( it_sel_rows[] ) > 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  CHECK sy-subrc = 0.

  READ TABLE it_saida_0100_04 INTO wa_saida_0100_04 INDEX wa_sel_rows-index.

  CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100_04-id_entrega IS NOT INITIAL ).

  FREE zcl_cct_entrega_carga.
  CREATE OBJECT zcl_cct_entrega_carga.

  CLEAR: wa_registro_entrega.
  wa_registro_entrega-id_entrega    = wa_saida_0100_04-id_entrega.
  wa_registro_entrega-modo          = c_entrega_view.

  zcl_cct_entrega_carga->registro_entrega( wa_registro_entrega ).

ENDFORM.

FORM f_change_entrega .

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  IF it_sel_rows[] IS INITIAL.
    MESSAGE 'Selecione uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  IF lines( it_sel_rows[] ) > 1.
    MESSAGE 'Selecione apenas uma linha!' TYPE 'S'.
    EXIT.
  ENDIF.

  READ TABLE it_sel_rows INTO wa_sel_rows INDEX 1.

  CHECK sy-subrc = 0.

  READ TABLE it_saida_0100_04 INTO wa_saida_0100_04 INDEX wa_sel_rows-index.

  CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100_04-id_entrega IS NOT INITIAL ).

  FREE zcl_cct_entrega_carga.
  CREATE OBJECT zcl_cct_entrega_carga.

  CLEAR: wa_registro_entrega.
  wa_registro_entrega-id_entrega    = wa_saida_0100_04-id_entrega.
  wa_registro_entrega-modo          = c_entrega_change.
  wa_registro_entrega-tipo_carga    = '2'.
  wa_registro_entrega-tipo_granel   = '02'.
  wa_registro_entrega-unid_medida   = 'KG'.

  zcl_cct_entrega_carga->registro_entrega( wa_registro_entrega ).

ENDFORM.

FORM f_executar .

  IF p_bukrs-low IS INITIAL.
    MESSAGE 'Recinto-Empresa é um campo obrigatório!' TYPE 'S'.
    STOP.
  ENDIF.

  IF p_branch-low IS INITIAL.
    MESSAGE 'Recinto-Filial é um campo obrigatório!' TYPE 'S'.
    STOP.
  ENDIF.

  CLEAR: wg_par_cct.
  SELECT SINGLE *
    FROM zlest0149 INTO wg_par_cct
   WHERE bukrs_ra  EQ p_bukrs-low
     AND branch_ra EQ p_branch-low.

  IF sy-subrc NE 0.
    MESSAGE |Parâmetros CCT não encontrado para a Recinto-Empresa: { p_bukrs-low }  / Recinto-Filial: { p_branch-low } ! | TYPE 'S'.
    STOP.
  ENDIF.

  IF go_clock IS INITIAL.
    CREATE OBJECT: go_clock.
  ENDIF.

  IF go_alarm IS INITIAL.
    CREATE OBJECT: go_alarm.
    SET HANDLER go_alarm->on_finished FOR go_clock.
  ENDIF.

  IF obj_alv_0100 IS NOT INITIAL.
    CALL METHOD obj_alv_0100->free.
    CALL METHOD cl_gui_cfw=>flush.
    FREE: obj_alv_0100.
  ENDIF.

  PERFORM: f_selecionar_dados,
           f_processa_dados.

  "Desconectar Portal
  CLEAR: vg_time_lim.
  vg_st_logon = c_disconnected.

  IF zcl_token_siscomex IS NOT INITIAL.
    FREE zcl_token_siscomex.
  ENDIF.

  CALL SCREEN 0100.

ENDFORM.

FORM f_processa_recepcao_sel.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente processar a(s) Recepção(ões) de Carga(s) selecionada(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_02 INTO wa_saida_0100_02 INDEX wa_sel_rows-index.

    CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100_02-id_recepcao IS NOT INITIAL ).

    PERFORM f_processa_recepcao USING wa_saida_0100_02-id_recepcao
                                      abap_true.

  ENDLOOP.

ENDFORM.

FORM f_processa_recepcao USING p_id_recepcao TYPE zlest0146-id_recepcao
                               p_msg         TYPE c.

  DATA: wl_zlest0146    TYPE zlest0146,
        wl_zlest0142    TYPE zlest0142,
        wl_zlest0147    TYPE zlest0147,
        lt_zlest0168    TYPE zlest0168_t,
        wl_retorno_proc TYPE zde_retorno_proc.

  CLEAR: wl_zlest0146, wl_zlest0142, wl_zlest0147, lt_zlest0168, wl_retorno_proc.

  CHECK ( p_id_recepcao IS NOT INITIAL ).

  SELECT SINGLE *
    FROM zlest0146 INTO wl_zlest0146
   WHERE id_recepcao = p_id_recepcao.

  IF sy-subrc NE 0.
    MESSAGE 'Cabeçalho Recepção não encontrado!' TYPE 'S'.
    RETURN.
  ENDIF.

  SELECT SINGLE *
    FROM zlest0142 INTO wl_zlest0142
   WHERE id_recepcao = p_id_recepcao.

  IF ( sy-subrc NE 0 ) OR ( wl_zlest0142-chave_nfe IS INITIAL AND
                            wl_zlest0142-chave_nff IS INITIAL ).
    MESSAGE 'Notas Fiscais Recepção não encontradas!' TYPE 'S'.
    RETURN.
  ENDIF.

  wl_zlest0147-id_recepcao  =  wl_zlest0142-id_recepcao.

  IF wl_zlest0142-chave_nfe IS NOT INITIAL.

    wl_zlest0147-chave_nfe    =  wl_zlest0142-chave_nfe.
    wl_zlest0147-regio        =  wl_zlest0142-chave_nfe(2).
    wl_zlest0147-nfyear       =  wl_zlest0142-chave_nfe+2(2).
    wl_zlest0147-nfmonth      =  wl_zlest0142-chave_nfe+4(2).
    wl_zlest0147-emissor_cnpj =  wl_zlest0142-chave_nfe+6(14).
    wl_zlest0147-model        =  wl_zlest0142-chave_nfe+20(2).
    wl_zlest0147-serie        =  wl_zlest0142-chave_nfe+22(3).
    wl_zlest0147-nfnum9       =  wl_zlest0142-chave_nfe+25(9).
    wl_zlest0147-docnum9      =  wl_zlest0142-chave_nfe+34(9).
    wl_zlest0147-cdv          =  wl_zlest0142-chave_nfe+43(1).
    wl_zlest0147-dt_emissao   =  wl_zlest0142-dt_emissao.

  ELSEIF wl_zlest0142-chave_nff IS NOT INITIAL.

    wl_zlest0147-chave_nff         = wl_zlest0142-chave_nff.
    wl_zlest0147-emissor_cpf       = wl_zlest0142-cpf_emissor.
    wl_zlest0147-nfyear            = wl_zlest0142-dt_emissao(2).
    wl_zlest0147-nfmonth           = wl_zlest0142-dt_emissao+4(2).
    wl_zlest0147-model             = wl_zlest0142-model.
    wl_zlest0147-serie             = wl_zlest0142-serie.
    wl_zlest0147-nfnum9            = wl_zlest0142-numero.

    IF wl_zlest0142-model <> '55'.
      wl_zlest0147-nfnum             = wl_zlest0142-numero.
    ENDIF.

    wl_zlest0147-dt_emissao        = wl_zlest0142-dt_emissao.
    wl_zlest0147-sigla_uf_emissor  = wl_zlest0142-sigla_uf_emissor.


  ENDIF.

  CHECK ( wl_zlest0147-chave_nfe IS NOT INITIAL ) OR ( wl_zlest0147-chave_nff IS NOT INITIAL ).

  CALL FUNCTION 'ZCCT_PROC_RECEPCAO_CARGA'
    EXPORTING
      i_gravar_registro = abap_false
    CHANGING
      c_zlest0146       = wl_zlest0146
      c_zlest0147       = wl_zlest0147
      c_zlest0168       = lt_zlest0168
      c_retorno         = wl_retorno_proc.

  IF p_msg EQ abap_true.
    MESSAGE wl_retorno_proc-texto TYPE 'S'.
  ENDIF.

  IF ( wl_zlest0147-docnum IS NOT INITIAL ) AND ( wl_zlest0147-id_recepcao IS NOT INITIAL ).

    IF ( wl_zlest0147-chave_nfe IS NOT INITIAL ).

      UPDATE zlest0147 SET docnum = wl_zlest0147-docnum
       WHERE id_recepcao EQ wl_zlest0147-id_recepcao
         AND chave_nfe   EQ wl_zlest0147-chave_nfe.

    ELSEIF ( wl_zlest0147-chave_nff IS NOT INITIAL ).

      UPDATE zlest0147 SET docnum           = wl_zlest0147-docnum
                           entrada_propria  = wl_zlest0147-entrada_propria
                           chave_nfe_prop   = wl_zlest0147-chave_nfe_prop
                           nfnum            = wl_zlest0147-nfnum
                           model            = wl_zlest0147-model
                           serie            = wl_zlest0147-serie
                           dt_emissao       = wl_zlest0147-dt_emissao
                           sigla_uf_emissor = wl_zlest0147-sigla_uf_emissor
                           emissor_ie       = wl_zlest0147-emissor_ie
                           emissor_cpf      = wl_zlest0147-emissor_cpf
       WHERE id_recepcao EQ wl_zlest0147-id_recepcao
         AND chave_nff   EQ wl_zlest0147-chave_nff.

    ENDIF.

    COMMIT WORK.

  ENDIF.

  CHECK wl_retorno_proc-type = 'S'.

  DELETE FROM zlest0168 WHERE id_recepcao = wl_zlest0147-id_recepcao.
  IF  lt_zlest0168[] IS NOT INITIAL.
    MODIFY zlest0168 FROM TABLE lt_zlest0168.
  ENDIF.

ENDFORM.

FORM f_cancel_entrega_carga.

  DATA: zcl_cct_entrega_carga TYPE REF TO zcl_cct_entrega_carga.

  CLEAR: it_sel_rows[], wa_sel_rows.

  CALL METHOD obj_alv_0100->get_selected_rows
    IMPORTING
      et_index_rows = it_sel_rows.

  CHECK it_sel_rows[] IS NOT INITIAL.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar              = 'Confirmação'
      text_question         = 'Deseja realmente cancelar a(s) Entrega(s) de Carga(s) selecionada(s)?'
      text_button_1         = 'Sim'
      text_button_2         = 'Não'
      default_button        = '1'
      display_cancel_button = ''
    IMPORTING
      answer                = var_answer
    EXCEPTIONS
      text_not_found        = 1
      OTHERS                = 2.

  CHECK var_answer EQ '1'.

  LOOP AT it_sel_rows INTO wa_sel_rows.

    READ TABLE it_saida_0100_04 INTO wa_saida_0100_04 INDEX wa_sel_rows-index.

    CHECK ( sy-subrc = 0 ) AND ( wa_saida_0100_04-id_entrega IS NOT INITIAL ).

    FREE zcl_cct_entrega_carga.
    CREATE OBJECT zcl_cct_entrega_carga
      EXPORTING
        i_id_entrega = wa_saida_0100_04-id_entrega.

    zcl_cct_entrega_carga->cancelar_entrega( RECEIVING r_cancelada = DATA(_cancelada) ).

    IF _cancelada IS INITIAL.
      RETURN.
    ENDIF.

  ENDLOOP.

  MESSAGE 'Entrega(s) de Carga(s) canceladas(s) com sucesso!' TYPE 'S'.


ENDFORM.

FORM f_atrib_nf_complemento USING p_chave_nfe
                                  p_chave_nff
                         CHANGING p_nfs_complementadas.

  DATA: v_chave TYPE zlest0192-chave.

  CLEAR: v_chave.

  IF p_chave_nfe IS NOT INITIAL.
    v_chave = p_chave_nfe.
  ELSEIF p_chave_nff IS NOT INITIAL.
    v_chave = p_chave_nff.
  ENDIF.

  CHECK v_chave IS NOT INITIAL.

  LOOP AT tg_0192 WHERE chave EQ v_chave.

    IF p_nfs_complementadas IS INITIAL .
      p_nfs_complementadas = tg_0192-chave_comp.
    ELSE.
      CONCATENATE p_nfs_complementadas ',' tg_0192-chave_comp INTO p_nfs_complementadas SEPARATED BY space.
    ENDIF.

  ENDLOOP.

ENDFORM.

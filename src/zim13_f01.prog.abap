*&---------------------------------------------------------------------*
*&  Include           ZIM12_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_X  text
*----------------------------------------------------------------------*
FORM f_montar_layout_zim01  USING p_edit.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ''         ''       'IT_ZIM01' 'ANO'              'Ano'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'MES'              'Mes'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'CENTRO_CUSTO'     'Centro Custo'   '12' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'NOME_CENTRO'      'Nome Centro'    '30' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'COD_CONTA'        'Cod.Conta'      '12' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'NOME_CONTA'       'Nome Conta'     '30' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'COD_ITEM'         'Cod.Item'       '10' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'NOME_ITEM'        'Nome Item'      '30' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'COD_COMPRA'       'Cod.Compra'     '10' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'ID_SYSPHERA'      'Id SYSPHERA'    '10' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'VLR_LOCAL'        'Valor Local'    '16' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM01' 'VLR_DOLAR'        'Valor Dolar'    '16' ' '    ' ' 'X'.

ENDFORM.

*&      Form  F_MONTAR_LAYOUT_ZIM02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_X  text
*----------------------------------------------------------------------*
FORM f_montar_layout_zim02  USING    p_c_x.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ''         ''       'IT_ZIM02' 'ANO'              'Ano'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'MES'              'Mes'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'CODIGO_CENARIO'   'Código Cenáro'  '12' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'CENTRO_CUSTO'     'Centro Custo'   '12' ' '    ' ' 'X',
        "1   ''         ''       'IT_ZIM02' 'NOME_CENTRO'      'Nome Centro'    '30' ' '    ' ' 'X',
      "  1   ''         ''       'IT_ZIM02' 'COD_CONTA'        'Cod.Conta'      '12' ' '    ' ' 'X',
     "   1   ''         ''       'IT_ZIM02' 'NOME_CONTA'       'Nome Conta'     '30' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'COD_ITEM'         'Cod.Item'       '10' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'NOME_ITEM'        'Nome Item'      '30' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'COD_GRUPO'        'Cod.Grupo'      '10' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'NOME_GRUPO'       'Nome Grupo'     '30' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'STR_OBJETIVO'     'Objetivo'       '145' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'STR_DESCRICAO'    'Descrição'      '145' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'STR_FINALIDADE'   'Finalidade'     '30' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'QTD'              'Quantidade'     '10' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'VLR_LOCAL'        'Valor Local'    '16' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'VLR_DOLAR'        'Valor Dolar'    '16' ' '    ' ' 'X',
        1   ''         ''       'IT_ZIM02' 'ID_INVESTIMENTO'  'ID. Investimento'  '16' ' '    ' ' 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_X  text
*----------------------------------------------------------------------*
FORM f_montar_layout_dre  USING    p_c_x.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ''         ''       'IT_DRE' 'ANO'              'Ano'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_DRE' 'MES'              'Mes'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_DRE' 'CENTRO_CUSTO'     'Centro Custo'   '12' ' '    ' ' 'X',
        1   ''         ''       'IT_DRE' 'NOME_CENTRO'      'Nome Centro'    '30' ' '    ' ' 'X',
        1   ''         ''       'IT_DRE' 'COD_CONTA'        'Cod.Conta'      '12' ' '    ' ' 'X',
        1   ''         ''       'IT_DRE' 'NOME_CONTA'       'Nome Conta'     '30' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'ID_CONCATENADO'   'Id Concatenado' '20' ' '    ' ' 'X',
*        1   ''         ''       'IT_DRE' 'COD_PRODUTO'      'Cod.Produto'    '10' ' '    ' ' 'X',
*        1   ''         ''       'IT_DRE' 'NOME_PRODUTO'     'Nome Produto'   '30' ' '    ' ' 'X',
        1   ''         ''       'IT_DRE' 'VLR_LOCAL'        'Valor Local'    '16' ' '    ' ' 'X',
        1   ''         ''       'IT_DRE' 'VLR_DOLAR'        'Valor Dolar'    '16' ' '    ' ' 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT_KSB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_X  text
*----------------------------------------------------------------------*
FORM f_montar_layout_ksb1  USING    p_c_x.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ''         ''       'IT_KSB1' 'BUKRS'            'Empresa'        '06' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'BELNR'            'Documento'      '10' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'GJAHR'            'Ano'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'BUDAT'            'Dt.Lancto'      '06' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'KOSTL'            'Centro Custo'   '12' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'NOME_CENTRO'      'Nome Centro'    '30' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'KSTAR'            'Classe Custo'   '12' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'CEL_LTXT'         'Desc.Classe'    '30' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'AUFNR'            'Ordem'          '30' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'WTGBTR'           'Valor Objeto'   '16' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'WKGBTR'           'Valor Dolar'    '16' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'MBGBTR'           'Qtde entrada'   '16' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'MATNR'            'Material'       '10' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'MAKTX'            'Descrição'      '30' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'LIFNR'            'Fornecedor'     '10' ' '    ' ' 'X',
        1   ''         ''       'IT_KSB1' 'NAME1'            'Nome'           '30' ' '    ' ' 'X'.

ENDFORM.

FORM f_insere_cor USING p_field
                        p_cor.
  wa_color-fname     = p_field.
  wa_color-color-col = p_cor.
  APPEND wa_color   TO wa_inves-cellcolor.
ENDFORM.

FORM f_cores_colunas.

  DATA: l_tabix  TYPE sy-tabix.

  FREE: it_inves.

  LOOP AT it_inves_ret INTO wa_inves_ret.
    CLEAR wa_inves.
    MOVE-CORRESPONDING wa_inves_ret TO wa_inves.

    LOOP AT it_fields INTO wa_fields.
      CASE wa_fields-name(2).
        WHEN 'TC'.
          PERFORM f_insere_cor USING wa_fields-name 7.
        WHEN 'IM'.
          PERFORM f_insere_cor USING wa_fields-name 1.
        WHEN 'ZI'.
          PERFORM f_insere_cor USING wa_fields-name 2.
        WHEN 'AS'.
          PERFORM f_insere_cor USING wa_fields-name 3.
        WHEN 'AW'.
          PERFORM f_insere_cor USING wa_fields-name 4.
        WHEN 'K1'.
          PERFORM f_insere_cor USING wa_fields-name 5.
        WHEN 'K2'.
          PERFORM f_insere_cor USING wa_fields-name 6.
      ENDCASE.
    ENDLOOP.
    APPEND wa_inves               TO it_inves.
  ENDLOOP.

ENDFORM.

FORM f_montar_layout_inves  USING    p_c_x.
  REFRESH t_fieldcatalog.

  PERFORM f_montar_estrutura USING:
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 03  INICIO - BG

*        1   ''         ''       'IT_INVES' 'TCODE'               'TCODE'                            '07'     ' '      ' '  'X',
*        2   ''         ''       'IT_INVES' 'IM_POSNR'            'Solic. Investimento'              '12'     ' '      ' '  'X',
*        3   ''         ''       'IT_INVES' 'IM_TXT50'            'Denominação da solicitação'       '50'     ' '      ' '  'X',
*        4   ''         ''       'IT_INVES' 'IM_BUKRS'            'Empresa Solicitante'              '15'     ' '      ' '  'X',
*        5   ''         ''       'IT_INVES' 'IM_KOSTL'            'Centro de custo solic'            '18'     ' '      ' '  'X',
**       6   ''         ''       'IT_INVES' 'IM_ANLN1'            'Imobilizado'                      '12'     ' '      ' '  'X',
**       7   ''         ''       'IT_INVES' 'IM_ANLN2'            'Sbnº'                             '14'     ' '      ' '  'X',
**       8   ''         ''       'IT_INVES' 'IM_OBJNR'            'Ordem'                            '15'     ' '      ' '  'X',
**       9   ''         ''       'IT_INVES' 'IM_TEXT'             'Texto breve'                      '60'     ' '      ' '  'X',
**
*        10  ''         ''       'IT_INVES' 'AS_ANLN1_IMA'        'Imobilizado'                      '20'     ' '      ' '  'X',
*        11  ''         ''       'IT_INVES' 'AS_ANLN2_IMA'        'Sbnº'                             '12'     ' '      ' '  'X',
*        12  ''         ''       'IT_INVES' 'K1_OBJNR_IMA'        'Ordem'                            '18'     ' '      ' '  'X',
*        13  ''         ''       'IT_INVES' 'K1_TEXT_IMA'         'Texto Breve'                       '60'     ' '      ' '  'X',
**
*        14  ''         ''       'IT_INVES' 'ZI_IZWEK'            'Categoria do investimento'        '20'     ' '      ' '  'X',
*        15  ''         ''       'IT_INVES' 'ZI_KOSTL'            'Centro de Custo'                  '15'     ' '      ' '  'X',
*        16  ''         ''       'IT_INVES' 'ZI_KNTTP'            'Classificação-Contábil'           '15'     ' '      ' '  'X',
*        17  ''         ''       'IT_INVES' 'ZI_KTEXT'            'Descrição Centro Custo'           '20'     ' '      ' '  'X',
*        18  ''         ''       'IT_INVES' 'ZI_SAKNR'            'Nr.Conta-Contábil'                '18'     ' '      ' '  'X',
*        19  ''         ''       'IT_INVES' 'ZI_TXT20'            'Descrição da C.Contábil'          '20'     ' '      ' '  'X',
*        20  ''         ''       'IT_INVES' 'ZI_DESCR_ITEM'       'Descrição do Item'               '100'     ' '      ' '  'X',
*        21  ''         ''       'IT_INVES' 'ZI_NAME1'            'Descrição Filial'                 '30'     ' '      ' '  'X',
*        22  ''         ''       'IT_INVES' 'ZI_BUKRS'            'Empresa '                          '10'     ' '      ' '  'X',
*        23  ''         ''       'IT_INVES' 'ZI_ANO'              'Exercício'                         '10'     ' '      ' '  'X',
*        24  ''         ''       'IT_INVES' 'ZI_FASE'             'Fase'                              '5'     ' '      ' '  'X',
*        25  ''         ''       'IT_INVES' 'ZI_GSBER'            'Filial'                            '8'     ' '      ' '  'X',
*        26  ''         ''       'IT_INVES' 'ZI_FINAL'            'Finalidade'                        '12'     ' '      ' '  'X',
*        27  ''         ''       'IT_INVES' 'ZI_SYSPHERA'         'Investimento Sysphera'             '21'     ' '      ' '  'X',
*        28  ''         ''       'IT_INVES' 'ZI_OBJETIVO'         'Objetivo do investimento'        '100'     ' '      ' '  'X',
*        29  ''         ''       'IT_INVES' 'ZI_MENGE'            'Quantidade'                       '13'     ' '      ' '  'X',
*        30  ''         ''       'IT_INVES' 'ZI_POSNR'            'Solicitação de Investimento'       '16'     ' '      ' '  'X',
*        31  ''         ''       'IT_INVES' 'ZI_TX_USD'           'Tx US$'                            '13'     ' '      ' '  'X',
*        32  ''         ''       'IT_INVES' 'ZI_VLR_UNIT'         'Unitário R$'                      '13'     ' '      ' '  'X',
*        33  ''         ''       'IT_INVES' 'ZI_VLR_TOTAL'        'Vl. Total R$'                     '13'     ' '      ' '  'X',
*        34  ''         ''       'IT_INVES' 'ZI_VLR_TOT_US'       'Vl. Total US$'                    '13'     ' '      ' '  'X',
**
**       35  ''         ''       'IT_INVES' 'AS_ANLN1_IMA'        'Imobilizado (IMAK)'               '20'     ' '      ' '  'X',
**       36  ''         ''       'IT_INVES' 'AS_ANLN2_IMA'        'Sbnº (IMAK)'                      '12'     ' '      ' '  'X',
**
*        37  ''         ''       'IT_INVES' 'AS_ANLN1'            'Imobilizado'                      '12'     ' '      ' '  'X',
*        38  ''         ''       'IT_INVES' 'AS_ANLN2'            'Sbnº'                              '4'     ' '      ' '  'X',
*        39  ''         ''       'IT_INVES' 'AS_TXA50'            'Denominação'                      '50'     ' '      ' '  'X',
*        40  ''         ''       'IT_INVES' 'AS_TXA50_C'          'Denominação 2'                    '50'     ' '      ' '  'X',
*        41  ''         ''       'IT_INVES' 'AS_KOSTL'            'Centro de Custo'                  '15'     ' '      ' '  'X',
*        42  ''         ''       'IT_INVES' 'AW_BZDAT'            'Dt. Referencia'                   '10'     ' '      ' '  'X',
*        43  ''         ''       'IT_INVES' 'AW_LIFNR'            'Fornecedor'                       '15'     ' '      ' '  'X',
*        44  ''         ''       'IT_INVES' 'AW_ANBTRB'           'Montante BRL'                     '13'     ' '      ' '  'X',
*        45  ''         ''       'IT_INVES' 'AW_ANBTRU'           'Montante USD'                     '13'     ' '      ' '  'X',
*        46  ''         ''       'IT_INVES' 'AW_BWASL'            'TMV'                               '5'     ' '      ' '  'X',
*        47  ''         ''       'IT_INVES' 'AW_BWATXT'           'Denominação do tipo de movimento' '50'     ' '      ' '  'X',
**       48  ''         ''       'IT_INVES' 'K1_WERKS'            'Centro'                            '10'     ' '      ' '  'X',
*        49  ''         ''       'IT_INVES' 'K1_KSTAR'            'Classe de custo'                  '18'     ' '      ' '  'X',
*        50  ''         ''       'IT_INVES' 'K1_OBJNR_N1'         'Classif.cont.secund.1'            '22'     ' '      ' '  'X',
*        51  ''         ''       'IT_INVES' 'K1_BEKNZ'            'Cód.débito/crédito'                '16'     ' '      ' '  'X',
*        52  ''         ''       'IT_INVES' 'K1_GKONT'            'Conta lnçto.contrap.'             '10'     ' '      ' '  'X',
*        53  ''         ''       'IT_INVES' 'K1_CPUDT'            'Data de entrada'                  '18'     ' '      ' '  'X',
*        54  ''         ''       'IT_INVES' 'K1_BUDAT'            'Data de lançamento'               '18'     ' '      ' '  'X',
*        55  ''         ''       'IT_INVES' 'K1_BLDAT'            'Data do documento'                '18'     ' '      ' '  'X',
*        56  ''         ''       'IT_INVES' 'K1_WSDAT'            'Data efetiva'                     '15'     ' '      ' '  'X',
*        57  ''         ''       'IT_INVES' 'K1_CEL_KTXT'         'Denom.classe custo'               '20'     ' '      ' '  'X',
*        58  ''         ''       'IT_INVES' 'K1_GKONT_LTXT'       'Denomin.da contacontrapartida'    '50'     ' '      ' '  'X',
*        59  ''         ''       'IT_INVES' 'K1_SGTXT'            'Denominação'                      '50'     ' '      ' '  'X',
*        60  ''         ''       'IT_INVES' 'K1_OBJ_TXT'          'Denominação de objeto'             '2'     ' '      ' '  'X',
*        61  ''         ''       'IT_INVES' 'K1_CEL_LTXT'         'Descr.classe custo'               '40'     ' '      ' '  'X',
*        62  ''         ''       'IT_INVES' 'K1_GSBER'            'Divisão'                           '10'     ' '      ' '  'X',
*        63  ''         ''       'IT_INVES' 'K1_EBELN'            'Documento de compras'             '10'     ' '      ' '  'X',
*        64  ''         ''       'IT_INVES' 'K1_BUKRS'            'Empresa'                           '10'     ' '      ' '  'X',
*        65  ''         ''       'IT_INVES' 'K1_GJAHR'            'Exercicio'                         '10'     ' '      ' '  'X',
*        66  ''         ''       'IT_INVES' 'K1_REFGJ'            'Exercício referência'              '15'     ' '      ' '  'X',
*        67  ''         ''       'IT_INVES' 'K1_EBELP'            'Item'                              '8'     ' '      ' '  'X',
*        68  ''         ''       'IT_INVES' 'K1_BUZEI'            'Linha de lançamento'               '19'     ' '      ' '  'X',
*        69  ''         ''       'IT_INVES' 'K1_MATNR'            'Material'                         '18'     ' '      ' '  'X',
*        70  ''         ''       'IT_INVES' 'K1_KWAER'            'Moeda da ACC'                      '15'     ' '      ' '  'X',
*        71  ''         ''       'IT_INVES' 'K1_TWAER'            'Moeda da transação'                '15'     ' '      ' '  'X',
*        72  ''         ''       'IT_INVES' 'K1_OWAER'            'Moeda do objeto'                   '15'     ' '      ' '  'X',
*        73  ''         ''       'IT_INVES' 'K1_RWAER'            'Moeda do relatório'                '15'     ' '      ' '  'X',
*        74  ''         ''       'IT_INVES' 'K1_BELNR'            'Nº documento'                     '15'     ' '      ' '  'X',
*        75  ''         ''       'IT_INVES' 'K1_AUFNR'            'Ordem'                            '18'     ' '      ' '  'X',
**
**       76  ''         ''       'IT_INVES' 'K1_OBJNR_IMA'        'Ordem (IMAK)'                     '18'     ' '      ' '  'X',
**       77  ''         ''       'IT_INVES' 'K1_TEXT_IMA'         'Texto Breve (IMAK)'                '60'     ' '      ' '  'X',
**
*        78  ''         ''       'IT_INVES' 'K1_PERIO'            'Periodo'                           '13'     ' '      ' '  'X',
*        79  ''         ''       'IT_INVES' 'K1_MBGBTR'           'Qtd.total entrada'                '15'     ' '      ' '  'X',
*        80  ''         ''       'IT_INVES' 'K1_MAT_TXT'          'Texto breve material'             '40'     ' '      ' '  'X',
*        81  ''         ''       'IT_INVES' 'K1_EBTXT'            'Texto do pedido'                  '40'     ' '      ' '  'X',
*        82  ''         ''       'IT_INVES' 'K1_BLART'            'Tipo de documento'                 '12'     ' '      ' '  'X',
*        83  ''         ''       'IT_INVES' 'K1_MEINB'            'Unid.medida lançada'               '13'     ' '      ' '  'X',
*        84  ''         ''       'IT_INVES' 'K1_WRVBTR'           'Valor var./moeda relat'           '15'     ' '      ' '  'X',
*        85  ''         ''       'IT_INVES' 'K1_WTVBTR'           'Valor var./MTransaç.'             '15'     ' '      ' '  'X',
*        86  ''         ''       'IT_INVES' 'K1_WTGBTR'           'Valor/moed.transação'             '15'     ' '      ' '  'X',
*        87  ''         ''       'IT_INVES' 'K1_WKGBTR'           'Valor/moeda ACC'                  '15'     ' '      ' '  'X',
*        88  ''         ''       'IT_INVES' 'K1_WOGBTR'           'Valor/moeda objeto'               '15'     ' '      ' '  'X',
*        89  ''         ''       'IT_INVES' 'K1_WRGBTR'           'Valor/MR'                         '15'     ' '      ' '  'X',
*        90  ''         ''       'IT_INVES' 'K2_BLDAT'            'Data do documento'                '10'     ' '      ' '  'X',
*        91  ''         ''       'IT_INVES' 'K2_SAKTO'            'Classe de custo '                 '15'     ' '      ' '  'X',
*        92  ''         ''       'IT_INVES' 'K2_CEL_KTXT'         'Denom.classe custo'               '20'     ' '      ' '  'X',
*        93  ''         ''       'IT_INVES' 'K2_SGTXT'            'Denominação'                      '50'     ' '      ' '  'X',
*        94  ''         ''       'IT_INVES' 'K2_OBJ_TXT'          'Denominação de objeto'             '50'     ' '      ' '  'X',
*        95  ''         ''       'IT_INVES' 'K2_BUKRS'            'Empresa'                           '10'     ' '      ' '  'X',
*        96  ''         ''       'IT_INVES' 'K2_GJAHR'            'Exercicio'                         '10'     ' '      ' '  'X',
*        97  ''         ''       'IT_INVES' 'K2_LIFNR'            'Fornecedor'                       '10'     ' '      ' '  'X',
*        98  ''         ''       'IT_INVES' 'K2_RFPOS'            'Item referência '                  '15'     ' '      ' '  'X',
*        99  ''         ''       'IT_INVES' 'K2_MATNR'            'Material'                         '18'     ' '      ' '  'X',
*        100 ''         ''       'IT_INVES' 'K2_TWAER'            'Moeda da transação'                '15'     ' '      ' '  'X',
*        101 ''         ''       'IT_INVES' 'K2_OWAER'            'Moeda do objeto '                  '15'     ' '      ' '  'X',
*        102 ''         ''       'IT_INVES' 'K2_RWAER'            'Moeda do relatório'                '15'     ' '      ' '  'X',
*        103 ''         ''       'IT_INVES' 'K2_REFBN'            'Nº doc.de referência'             '20'     ' '      ' '  'X',
*        104 ''         ''       'IT_INVES' 'K2_AUFNR'            'Ordem'                            '12'     ' '      ' '  'X',
*        105 ''         ''       'IT_INVES' 'K2_PERIO'            'Periodo'                           '13'     ' '      ' '  'X',
*        106 ''         ''       'IT_INVES' 'K2_ORGWTR'           'Plan./MRel.'                      '15'     ' '      ' '  'X',
*        107 ''         ''       'IT_INVES' 'K2_ORGWTK'           'Plan/moeda ACC'                   '15'     ' '      ' '  'X',
*        108 ''         ''       'IT_INVES' 'K2_ORGWTO'           'Plan/Moeda obj.'                  '15'     ' '      ' '  'X',
*        109 ''         ''       'IT_INVES' 'K2_ORGWTT'           'Plan/MTransação'                  '15'     ' '      ' '  'X',
*        110 ''         ''       'IT_INVES' 'K2_MEGBTR'           'Quantidade total'                 '15'     ' '      ' '  'X',
*        111 ''         ''       'IT_INVES' 'K2_WKURS'            'Taxa de câmbio'                    '18'     ' '      ' '  'X',
*        112 ''         ''       'IT_INVES' 'K2_MAT_TXT'          'Texto breve material'             '40'     ' '      ' '  'X',
*        113 ''         ''       'IT_INVES' 'K2_OBJART_TXT'       'Tipo de objeto'                   '20'     ' '      ' '  'X',
*        114 ''         ''       'IT_INVES' 'K2_MEINH'            'Unidade de medida'                 '19'     ' '      ' '  'X',
*        115 ''         ''       'IT_INVES' 'K2_WTGBTR'           'Valor/moed.transação'             '20'     ' '      ' '  'X',
*        116 ''         ''       'IT_INVES' 'K2_WKGBTR'           'Valor/moeda ACC '                 '20'     ' '      ' '  'X',
*        117 ''         ''       'IT_INVES' 'K2_WOGBTR'           'Valor/moeda objeto'               '20'     ' '      ' '  'X',
*        118 ''         ''       'IT_INVES' 'K2_WRGBTR'           'Valor/MR'                         '20'     ' '      ' '  'X'.
        1   ''         ''       'IT_INVES' 'TCODE'               'TCODE'         '07'     ' '      ' '  'X',
        2   ''         ''       'IT_INVES' 'IM_POSNR'            'IM_POSNR'      '12'     ' '      ' '  'X',
        3   ''         ''       'IT_INVES' 'IM_TXT50'            'IM_TXT50'      '50'     ' '      ' '  'X',
        4   ''         ''       'IT_INVES' 'IM_BUKRS'            'IM_BUKRS'      '15'     ' '      ' '  'X',
        5   ''         ''       'IT_INVES' 'IM_KOSTL'            'IM_KOSTL'      '18'     ' '      ' '  'X',
*       6   ''         ''       'IT_INVES' 'IM_ANLN1'            'IM_ANLN1'      '12'     ' '      ' '  'X',
*       7   ''         ''       'IT_INVES' 'IM_ANLN2'            'IM_ANLN2'      '14'     ' '      ' '  'X',
*       8   ''         ''       'IT_INVES' 'IM_OBJNR'            'IM_OBJNR'      '15'     ' '      ' '  'X',
*       9   ''         ''       'IT_INVES' 'IM_TEXT'             'IM_TEXT'       '60'     ' '      ' '  'X',
*
        10  ''         ''       'IT_INVES' 'AS_ANLN1_IMA'        'AS_ANLN1_IMA'  '20'     ' '      ' '  'X',
        11  ''         ''       'IT_INVES' 'AS_ANLN2_IMA'        'AS_ANLN2_IMA'  '12'     ' '      ' '  'X',
        12  ''         ''       'IT_INVES' 'K1_OBJNR_IMA'        'K1_OBJNR_IMA'  '18'     ' '      ' '  'X',
        13  ''         ''       'IT_INVES' 'K1_TEXT_IMA'         'K1_TEXT_IMA'   '60'     ' '      ' '  'X',
*
        14  ''         ''       'IT_INVES' 'ZI_IZWEK'            'ZI_IZWEK'      '20'     ' '      ' '  'X',
        15  ''         ''       'IT_INVES' 'ZI_KOSTL'            'ZI_KOSTL'      '15'     ' '      ' '  'X',
        16  ''         ''       'IT_INVES' 'ZI_KNTTP'            'ZI_KNTTP'      '15'     ' '      ' '  'X',
        17  ''         ''       'IT_INVES' 'ZI_KTEXT'            'ZI_KTEXT'      '20'     ' '      ' '  'X',
        18  ''         ''       'IT_INVES' 'ZI_SAKNR'            'ZI_SAKNR'      '18'     ' '      ' '  'X',
        19  ''         ''       'IT_INVES' 'ZI_TXT20'            'ZI_TXT20'      '20'     ' '      ' '  'X',
        20  ''         ''       'IT_INVES' 'ZI_DESCR_ITEM'       'ZI_DESCR_ITEM' '100'    ' '      ' '  'X',
        21  ''         ''       'IT_INVES' 'ZI_NAME1'            'ZI_NAME1'      '30'     ' '      ' '  'X',
        22  ''         ''       'IT_INVES' 'ZI_BUKRS'            'ZI_BUKRS'      '10'     ' '      ' '  'X',
        23  ''         ''       'IT_INVES' 'ZI_ANO'              'ZI_ANO'        '10'     ' '      ' '  'X',
        24  ''         ''       'IT_INVES' 'ZI_FASE'             'ZI_FASE'       '5'      ' '      ' '  'X',
        25  ''         ''       'IT_INVES' 'ZI_GSBER'            'ZI_GSBER'      '8'      ' '      ' '  'X',
        26  ''         ''       'IT_INVES' 'ZI_FINAL'            'ZI_FINAL'      '12'     ' '      ' '  'X',
        27  ''         ''       'IT_INVES' 'ZI_SYSPHERA'         'ZI_SYSPHERA'   '21'     ' '      ' '  'X',
        28  ''         ''       'IT_INVES' 'ZI_OBJETIVO'         'ZI_OBJETIVO'   '100'    ' '      ' '  'X',
        29  ''         ''       'IT_INVES' 'ZI_MENGE'            'ZI_MENGE'      '13'     ' '      ' '  'X',
        30  ''         ''       'IT_INVES' 'ZI_POSNR'            'ZI_POSNR'      '16'     ' '      ' '  'X',
        31  ''         ''       'IT_INVES' 'ZI_TX_USD'           'ZI_TX_USD'     '13'     ' '      ' '  'X',
        32  ''         ''       'IT_INVES' 'ZI_VLR_UNIT'         'ZI_VLR_UNIT'   '13'     ' '      ' '  'X',
        33  ''         ''       'IT_INVES' 'ZI_VLR_TOTAL'        'ZI_VLR_TOTAL'  '13'     ' '      ' '  'X',
        34  ''         ''       'IT_INVES' 'ZI_VLR_TOT_US'       'ZI_VLR_TOT_US' '13'     ' '      ' '  'X',
*
*       35  ''         ''       'IT_INVES' 'AS_ANLN1_IMA'        'AS_ANLN1_IMA'  '20'     ' '      ' '  'X',
*       36  ''         ''       'IT_INVES' 'AS_ANLN2_IMA'        'AS_ANLN2_IMA'  '12'     ' '      ' '  'X',
*
        37  ''         ''       'IT_INVES' 'AS_ANLN1'            'AS_ANLN1'      '12'     ' '      ' '  'X',
        38  ''         ''       'IT_INVES' 'AS_ANLN2'            'AS_ANLN2'      '4'      ' '      ' '  'X',
        39  ''         ''       'IT_INVES' 'AS_TXA50'            'AS_TXA50'      '50'     ' '      ' '  'X',
        40  ''         ''       'IT_INVES' 'AS_TXA50_C'          'AS_TXA50_C'    '50'     ' '      ' '  'X',
        41  ''         ''       'IT_INVES' 'AS_KOSTL'            'AS_KOSTL'      '15'     ' '      ' '  'X',
        42  ''         ''       'IT_INVES' 'AW_BZDAT'            'AW_BZDAT'      '10'     ' '      ' '  'X',
        43  ''         ''       'IT_INVES' 'AW_LIFNR'            'AW_LIFNR'      '15'     ' '      ' '  'X',
        44  ''         ''       'IT_INVES' 'AW_ANBTRB'           'AW_ANBTRB'     '13'     ' '      ' '  'X',
        45  ''         ''       'IT_INVES' 'AW_ANBTRU'           'AW_ANBTRU'     '13'     ' '      ' '  'X',
        46  ''         ''       'IT_INVES' 'AW_BWASL'            'AW_BWASL'      '5'      ' '      ' '  'X',
        47  ''         ''       'IT_INVES' 'AW_BWATXT'           'AW_BWATXT'     '50'     ' '      ' '  'X',
*       48  ''         ''       'IT_INVES' 'K1_WERKS'            'K1_WERKS'      '10'     ' '      ' '  'X',
        49  ''         ''       'IT_INVES' 'K1_KSTAR'            'K1_KSTAR'      '18'     ' '      ' '  'X',
        50  ''         ''       'IT_INVES' 'K1_OBJNR_N1'         'K1_OBJNR_N1'   '22'     ' '      ' '  'X',
        51  ''         ''       'IT_INVES' 'K1_BEKNZ'            'K1_BEKNZ'      '16'     ' '      ' '  'X',
        52  ''         ''       'IT_INVES' 'K1_GKONT'            'K1_GKONT'      '10'     ' '      ' '  'X',
        53  ''         ''       'IT_INVES' 'K1_CPUDT'            'K1_CPUDT'      '18'     ' '      ' '  'X',
        54  ''         ''       'IT_INVES' 'K1_BUDAT'            'K1_BUDAT'      '18'     ' '      ' '  'X',
        55  ''         ''       'IT_INVES' 'K1_BLDAT'            'K1_BLDAT'      '18'     ' '      ' '  'X',
        56  ''         ''       'IT_INVES' 'K1_WSDAT'            'K1_WSDAT'      '15'     ' '      ' '  'X',
        57  ''         ''       'IT_INVES' 'K1_CEL_KTXT'         'K1_CEL_KTXT'   '20'     ' '      ' '  'X',
        58  ''         ''       'IT_INVES' 'K1_GKONT_LTXT'       'K1_GKONT_LTXT' '50'     ' '      ' '  'X',
        59  ''         ''       'IT_INVES' 'K1_SGTXT'            'K1_SGTXT'      '50'     ' '      ' '  'X',
        60  ''         ''       'IT_INVES' 'K1_OBJ_TXT'          'K1_OBJ_TXT'    '2'      ' '      ' '  'X',
        61  ''         ''       'IT_INVES' 'K1_CEL_LTXT'         'K1_CEL_LTXT'   '40'     ' '      ' '  'X',
        62  ''         ''       'IT_INVES' 'K1_GSBER'            'K1_GSBER'      '10'     ' '      ' '  'X',
        63  ''         ''       'IT_INVES' 'K1_EBELN'            'K1_EBELN'      '10'     ' '      ' '  'X',
        63  ''         ''       'IT_INVES' 'K1_BSART'            'K1_BSART'      '10'     ' '      ' '  'X',
        64  ''         ''       'IT_INVES' 'K1_BUKRS'            'K1_BUKRS'      '10'     ' '      ' '  'X',
        65  ''         ''       'IT_INVES' 'K1_GJAHR'            'K1_GJAHR'      '10'     ' '      ' '  'X',
        66  ''         ''       'IT_INVES' 'K1_REFGJ'            'K1_REFGJ'      '15'     ' '      ' '  'X',
        67  ''         ''       'IT_INVES' 'K1_EBELP'            'K1_EBELP'      '8'      ' '      ' '  'X',
        68  ''         ''       'IT_INVES' 'K1_BUZEI'            'K1_BUZEI'      '19'     ' '      ' '  'X',
        69  ''         ''       'IT_INVES' 'K1_MATNR'            'K1_MATNR'      '18'     ' '      ' '  'X',
        70  ''         ''       'IT_INVES' 'K1_KWAER'            'K1_KWAER'      '15'     ' '      ' '  'X',
        71  ''         ''       'IT_INVES' 'K1_TWAER'            'K1_TWAER'      '15'     ' '      ' '  'X',
        72  ''         ''       'IT_INVES' 'K1_OWAER'            'K1_OWAER'      '15'     ' '      ' '  'X',
        73  ''         ''       'IT_INVES' 'K1_RWAER'            'K1_RWAER'      '15'     ' '      ' '  'X',
        74  ''         ''       'IT_INVES' 'K1_BELNR'            'K1_BELNR'      '15'     ' '      ' '  'X',
        75  ''         ''       'IT_INVES' 'K1_AUFNR'            'K1_AUFNR'      '18'     ' '      ' '  'X',
*
*       76  ''         ''       'IT_INVES' 'K1_OBJNR_IMA'        'K1_OBJNR_IMA'  '18'     ' '      ' '  'X',
*       77  ''         ''       'IT_INVES' 'K1_TEXT_IMA'         'K1_TEXT_IMA'   '60'     ' '      ' '  'X',
*
        78  ''         ''       'IT_INVES' 'K1_PERIO'            'K1_PERIO'      '13'     ' '      ' '  'X',
        79  ''         ''       'IT_INVES' 'K1_MBGBTR'           'K1_MBGBTR'     '15'     ' '      ' '  'X',
        80  ''         ''       'IT_INVES' 'K1_MAT_TXT'          'K1_MAT_TXT'    '40'     ' '      ' '  'X',
        81  ''         ''       'IT_INVES' 'K1_EBTXT'            'K1_EBTXT'      '40'     ' '      ' '  'X',
        82  ''         ''       'IT_INVES' 'K1_BLART'            'K1_BLART'      '12'     ' '      ' '  'X',
        83  ''         ''       'IT_INVES' 'K1_MEINB'            'K1_MEINB'      '13'     ' '      ' '  'X',
        84  ''         ''       'IT_INVES' 'K1_WRVBTR'           'K1_WRVBTR'     '15'     ' '      ' '  'X',
        85  ''         ''       'IT_INVES' 'K1_WTVBTR'           'K1_WTVBTR'     '15'     ' '      ' '  'X',
        86  ''         ''       'IT_INVES' 'K1_WTGBTR'           'K1_WTGBTR'     '15'     ' '      ' '  'X',
        87  ''         ''       'IT_INVES' 'K1_WKGBTR'           'K1_WKGBTR'     '15'     ' '      ' '  'X',
        88  ''         ''       'IT_INVES' 'K1_WOGBTR'           'K1_WOGBTR'     '15'     ' '      ' '  'X',
        89  ''         ''       'IT_INVES' 'K1_WRGBTR'           'K1_WRGBTR'     '15'     ' '      ' '  'X',
        90  ''         ''       'IT_INVES' 'K2_BLDAT'            'K2_BLDAT'      '10'     ' '      ' '  'X',
        91  ''         ''       'IT_INVES' 'K2_SAKTO'            'K2_SAKTO'      '15'     ' '      ' '  'X',
        92  ''         ''       'IT_INVES' 'K2_CEL_KTXT'         'K2_CEL_KTXT'   '20'     ' '      ' '  'X',
        93  ''         ''       'IT_INVES' 'K2_SGTXT'            'K2_SGTXT'      '50'     ' '      ' '  'X',
        94  ''         ''       'IT_INVES' 'K2_OBJ_TXT'          'K2_OBJ_TXT'    '50'     ' '      ' '  'X',
        95  ''         ''       'IT_INVES' 'K2_BUKRS'            'K2_BUKRS'      '10'     ' '      ' '  'X',
        96  ''         ''       'IT_INVES' 'K2_GJAHR'            'K2_GJAHR'      '10'     ' '      ' '  'X',
        97  ''         ''       'IT_INVES' 'K2_LIFNR'            'K2_LIFNR'      '10'     ' '      ' '  'X',
        98  ''         ''       'IT_INVES' 'K2_RFPOS'            'K2_RFPOS'      '15'     ' '      ' '  'X',
        99  ''         ''       'IT_INVES' 'K2_MATNR'            'K2_MATNR'      '18'     ' '      ' '  'X',
        100 ''         ''       'IT_INVES' 'K2_TWAER'            'K2_TWAER'      '15'     ' '      ' '  'X',
        101 ''         ''       'IT_INVES' 'K2_OWAER'            'K2_OWAER'      '15'     ' '      ' '  'X',
        102 ''         ''       'IT_INVES' 'K2_RWAER'            'K2_RWAER'      '15'     ' '      ' '  'X',
        103 ''         ''       'IT_INVES' 'K2_REFBN'            'K2_REFBN'      '20'     ' '      ' '  'X',
        104 ''         ''       'IT_INVES' 'K2_AUFNR'            'K2_AUFNR'      '12'     ' '      ' '  'X',
        105 ''         ''       'IT_INVES' 'K2_PERIO'            'K2_PERIO'      '13'     ' '      ' '  'X',
        106 ''         ''       'IT_INVES' 'K2_ORGWTR'           'K2_ORGWTR'     '15'     ' '      ' '  'X',
        107 ''         ''       'IT_INVES' 'K2_ORGWTK'           'K2_ORGWTK'     '15'     ' '      ' '  'X',
        108 ''         ''       'IT_INVES' 'K2_ORGWTO'           'K2_ORGWTO'     '15'     ' '      ' '  'X',
        109 ''         ''       'IT_INVES' 'K2_ORGWTT'           'K2_ORGWTT'     '15'     ' '      ' '  'X',
        110 ''         ''       'IT_INVES' 'K2_MEGBTR'           'K2_MEGBTR'     '15'     ' '      ' '  'X',
        111 ''         ''       'IT_INVES' 'K2_WKURS'            'K2_WKURS'      '18'     ' '      ' '  'X',
        112 ''         ''       'IT_INVES' 'K2_MAT_TXT'          'K2_MAT_TXT'    '40'     ' '      ' '  'X',
        113 ''         ''       'IT_INVES' 'K2_OBJART_TXT'       'K2_OBJART_TXT' '20'     ' '      ' '  'X',
        114 ''         ''       'IT_INVES' 'K2_MEINH'            'K2_MEINH'      '19'     ' '      ' '  'X',
        115 ''         ''       'IT_INVES' 'K2_WTGBTR'           'K2_WTGBTR'     '20'     ' '      ' '  'X',
        116 ''         ''       'IT_INVES' 'K2_WKGBTR'           'K2_WKGBTR'     '20'     ' '      ' '  'X',
        117 ''         ''       'IT_INVES' 'K2_WOGBTR'           'K2_WOGBTR'     '20'     ' '      ' '  'X',
        118 ''         ''       'IT_INVES' 'K2_WRGBTR'           'K2_WRGBTR'     '20'     ' '      ' '  'X',

        119 ''         ''       'IT_INVES' 'AS_EBELN '           'AS_EBELN '     '15'     ' '      ' '  'X',
        120 ''         ''       'IT_INVES' 'AS_RMWWR '           'AS_RMWWR '     '15'     ' '      ' '  'X',
        121 ''         ''       'IT_INVES' 'AS_BUDAT '           'AS_BUDAT '     '12'     ' '      ' '  'X',
        122 ''         ''       'IT_INVES' 'AS_XBLNR '           'AS_XBLNR '     '20'     ' '      ' '  'X',
        123 ''         ''       'IT_INVES' 'AS_NAME1 '           'AS_NAME1 '     '40'     ' '      ' '  'X',
        124 ''         ''       'IT_INVES' 'AS_STCD1 '           'AS_STCD1 '     '20'     ' '      ' '  'X',
        125 ''         ''       'IT_INVES' 'AS_BSART '           'AS_BSART '     '05'     ' '      ' '  'X',
        126 ''         ''       'IT_INVES' 'AS_KANSW '           'AS_KANSW '     '13'     ' '      ' '  'X'.

*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 03  FIM - BG

ENDFORM.

FORM f_montar_layout_mapa  USING    p_c_x.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ''         ''       'IT_CONF2' 'CHAVE_NFE'        'CHAVE_NFE'        '30' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'PRODUTO'          'PRODUTO'          '10' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'MAKTX'            'DESCRICAO'        '30' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'COD_CLIFOR'       'FORNECEDOR'       '10' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'NOME_CLIFOR'      'NOME'             '30' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'CPF_PROD'         'CNPJ'             '12' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'NFENUM'           'NFENUM'           '10' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'SERIES'           'SERIES'           '05' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'DOCDAT'           'DATA DOC'         '10' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'NETWRT'           'VALOR'            '16' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'PROD_CFOP'        'PROD_CFOP'        '10' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'ICMS_CST'         'ICMS_CST'         '10' ' '    ' ' 'X',
        1   ''         ''       'IT_CONF2' 'PSTDAT'           'PSTDAT'           '10' ' '    ' ' 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT_HCM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_X  text
*----------------------------------------------------------------------*
FORM f_montar_layout_hcm  USING    p_c_x.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ''         ''       'IT_HCM' 'ANO'              'Ano'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'MES'              'Mes'            '06' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'CENTRO_CUSTO'     'Centro Custo'   '12' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'NOME_CENTRO'      'Nome Centro'    '30' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'COD_CONTA'        'Cod.Conta'      '12' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'NOME_CONTA'       'Nome Conta'     '30' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'COD_CARGO'        'Cod.Cargo'     '12' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'NOME_CARGO'       'Nome Cargo'    '30' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM' 'VLR_LOCAL'        'Valor Local'    '16' ' '    ' ' 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT_HCM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_X  text
*----------------------------------------------------------------------*
FORM f_montar_layout_hcm2  USING    p_c_x.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ''         ''       'IT_HCM2' 'ANO'              'Ano'                   '06' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'MES'              'Mes'                   '06' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'CENTRO_CUSTO'     'Centro Custo'          '12' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'NOME_CENTRO'      'Nome Centro'           '30' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'COD_CARGO'        'Cod.Cargo'             '12' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'NOME_CARGO'       'Nome Cargo'            '30' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'COD_CHAPA'        'Matricula'             '12' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'NOME_FUNCIONARIO' 'Nome Funcionário'      '30' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'STR_SITUACAO'     'Situação'              '20' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'QTD_DEPENDENTES'  'Dependentes'           '10' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'DT_NASCIMENTO'    'Dt Nascimento'         '13' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'SEXO'             'Sexo'                  '10' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'SALDOATU'         'Saldo Acumulado'       '16' ' '    ' ' 'X', "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
        1   ''         ''       'IT_HCM2' 'SALDOPER'         'Saldo do periodo'      '18' ' '    ' ' 'X', "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
        1   ''         ''       'IT_HCM2' 'ANZHL_50'         'Unidade HE 50%'        '22' ' '    ' ' 'X', " BUG - 110727 - CBRAND
        1   ''         ''       'IT_HCM2' 'BETRG_50'         'Valor HE 50%'          '22' ' '    ' ' 'X', " BUG - 110727 - CBRAND
        1   ''         ''       'IT_HCM2' 'ANZHL_100'        'Unidade HE 100%'       '22' ' '    ' ' 'X', " BUG - 110727 - CBRAND
        1   ''         ''       'IT_HCM2' 'BETRG_100'        'Valor HE 100%'         '22' ' '    ' ' 'X', " BUG - 110727 - CBRAND
        1   ''         ''       'IT_HCM2' 'ANZHL_DIF'        'Unidade Diferença HE%' '24' ' '    ' ' 'X', " BUG - 110727 - CBRAND
        1   ''         ''       'IT_HCM2' 'BETRG_DIF'        'Valor Diferença HE'    '24' ' '    ' ' 'X', " BUG - 110727 - CBRAND
        1   ''         ''       'IT_HCM2' 'SLD_ACUM'         'Saldo Acumulado R$'    '24' ' '    ' ' 'X',
        1   ''         ''       'IT_HCM2' 'SLD_PER'          'Saldo do Período R$'   '24' ' '    ' ' 'X'.
* BUG - 110727 - CBRAND - Inicio
  "1   ''         ''       'IT_HCM2' 'ANZHL_50'         'Valor HrExtra 50%'  '22' ' '    ' ' 'X', "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
  "1   ''         ''       'IT_HCM2' 'BETRG_50'         'Valor QtdHora 50%'  '22' ' '    ' ' 'X', "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
  "1   ''         ''       'IT_HCM2' 'ANZHL_100'        'Valor HrExtra 100%' '24' ' '    ' ' 'X', "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
  "1   ''         ''       'IT_HCM2' 'BETRG_100'        'Valor QtdHora 100%' '24' ' '    ' ' 'X'. "MM 27062022 ADICIONAR COL RH ZIM15 #75555 GR
* BUG - 110727 - CBRAND - Fim
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT_KP06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_C_X  text
*----------------------------------------------------------------------*
FORM f_montar_layout_kp06  USING    p_c_x.
  REFRESH t_fieldcatalog.
  PERFORM f_montar_estrutura USING:
        1   ''         ''       'IT_KP06' 'ANO'              'Ano'             '06' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'MES'              'Mes'             '06' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'CODIGO_CENARIO'   'Código Cenário'  '12' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'NOME_CENARIO'     'Nome Cenário'    '30' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'CENTRO_CUSTO'     'Centro Custo'    '12' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'NOME_CENTRO'      'Nome Centro'     '30' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'COD_CONTA'        'Cod.Conta'       '12' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'NOME_CONTA'       'Nome Conta'      '30' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'VLR_LOCAL'        'Valor Local'     '16' ' '    ' ' 'X',
        1   ''         ''       'IT_KP06' 'VLR_DOLAR'        'Valor Dolar'     '16' ' '    ' ' 'X'.

ENDFORM.

*-CS2022000122 - 07.03.2022 - JT - inicio
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT_ZGL056
*&---------------------------------------------------------------------*
FORM f_montar_layout_zgl056  USING    p_c_x.
  REFRESH t_fieldcatalog.

  PERFORM f_montar_estrutura USING:
  01   'ZGLT050'  'NRO_APOLICE'       'IT_ZGL056' 'NRO_APOLICE'          'Nro. Apólice'       '' '' '' 'X',
  02   'ZGLT050'  'VIG_DE'            'IT_ZGL056' 'VIG_DE'               'Dt.Ini.Vig.'        '' '' '' 'X',
  03   'ZGLT050'  'VIG_ATE'           'IT_ZGL056' 'VIG_ATE'              'Dt.Fim.Vig.'        '' '' '' 'X',
  03   'ZGLT050'  'DT_LCTO_CTB'       'IT_ZGL056' 'DT_LCTO_CTB'          'Dt.Lcto.Contábil.'  '' '' '' 'X',
  04   'ZGLT050'  'SEQ_LCTO'          'IT_ZGL056' 'SEQ_LCTO'             'Seq.Lcto'           '' '' '' 'X',
  04   'ZGLT050'  'SEQ_TIPO'          'IT_ZGL056' 'SEQ_TIPO'             'Tipo'               '' '' '' 'X',
  04   'ZGLT064'  'DESCR'             'IT_ZGL056' 'DESCR_TIPO'           'Desc.Tp.'           '' '' '' 'X',
  04   'ZGLT050'  'TP_OPR'            'IT_ZGL056' 'TP_OPR'               'Tipo Opr.'          '' '' '' 'X',
  05   'ZGLT068'  'NR_ITEM'           'IT_ZGL056' 'NR_ITEM'              'Item'               '' '' '' 'X',
  06   'ZGLT050'  'BUKRS'             'IT_ZGL056' 'BUKRS'                'Empresa'            '' '' '' 'X',
  07   'ZGLT068'  'WERKS'             'IT_ZGL056' 'WERKS'                'Filial'             '' '' '' 'X',
  08   'ZGLT068'  'ANLN1'             'IT_ZGL056' 'ANLN1'                'Imobilizado'        '' '' '' 'X',
  09   'ZGLT068'  'ANLN2'             'IT_ZGL056' 'ANLN2'                'Sub.Nro'            '' '' '' 'X',
  10   'ANLA'     'INVNR'             'IT_ZGL056' 'INVNR'                'Chassi'             '' '' '' 'X',
  11   'ANLA'     'SERNR'             'IT_ZGL056' 'SERNR'                'Série'              '' '' '' 'X',
  12   'ZGLT068'  'MATNR'             'IT_ZGL056' 'MATNR'                'Material'           '' '' '' 'X',
  13   'ZGLT068'  'DESCR_BENS'        'IT_ZGL056' 'DESCR_BENS'           'Descrição'          '' '' '' 'X',
  14   'ZGLT032'  'HKONT'             'IT_ZGL056' 'HKONT'                'Conta Contábil'     '' '' '' 'X',
  15   'ANLA'     'TXA50'             'IT_ZGL056' 'TXA50'                'Descr.Pt2'          '' '' '' 'X',
  16   'ANLA'     'ZUGDT'             'IT_ZGL056' 'ZUGDT'                'Dt.Aquisição'       '' '' '' 'X',
  17   'ZGLT068'  'KOSTL'             'IT_ZGL056' 'KOSTL'                'C.Custo'            '' '' '' 'X',
  18   'ZGLT068'  'WKURS'             'IT_ZGL056' 'WKURS'                'Tx.Câmbio'          '' '' '' 'X',
  19   'ZGLT068'  'VLR_PREMIO_USD'    'IT_ZGL056' 'VLR_PREMIO_USD'       'Vlr.USD'            '' '' '' 'X',
  20   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_ZGL056' 'VLR_PREMIO_BRL'       'Vlr.BRL'            '' '' '' 'X',
  21   'ZGLT068'  'DT_BAIXA'          'IT_ZGL056' 'DT_BAIXA'             'Dt.Baixa'           '' '' '' 'X',
  22   'ZGLT068'  'VLR_PREMIO_USD'    'IT_ZGL056' 'VLR_BX_USD'           'Vlr.Bx.USD'         '' '' '' 'X',
  23   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_ZGL056' 'VLR_BX_BRL'           'Vlr.Bx.BRL'         '' '' '' 'X',
  24   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_ZGL056' 'APROP_USD'            'Apropriado USD'     '' '' '' 'X',
  25   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_ZGL056' 'APROP_BRL'            'Apropriado BRL'     '' '' '' 'X',
  26   'ZGLT068'  'VLR_APROP_AJ_USD'  'IT_ZGL056' 'VLR_APROP_AJ_USD'     'Vlr.Aj.Aprop.USD'   '' '' '' 'X',
  27   'ZGLT068'  'VLR_APROP_AJ_BRL'  'IT_ZGL056' 'VLR_APROP_AJ_BRL'     'Vlr.Aj.Aprop.BRL'   '' '' '' 'X',
  28   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_ZGL056' 'SALDO_USD'            'Saldo USD'          '' '' '' 'X',
  29   'ZGLT068'  'VLR_PREMIO_BRL'    'IT_ZGL056' 'SALDO_BRL'            'Saldo BRL'          '' '' '' 'X',
  30   ''         ''                  'IT_ZGL056' 'PERC_APROP'           '% Apropriado'       '' '' '' 'X',
  31   ''         ''                  'IT_ZGL056' 'NRO_APROP'            'Parc.Apropriadas'   '' '' '' 'X',
  32   'ZGLT068'  'VLR_RISCO_USD'     'IT_ZGL056' 'VLR_RISCO_USD'        'Vlr.Risco USD'      '' '' '' 'X',
  33   'ZGLT068'  'VLR_RISCO_BRL'     'IT_ZGL056' 'VLR_RISCO_BRL'        'Vlr.Risco BRL'      '' '' '' 'X',
  34   'ZGLT068'  'VLR_RISCO_BRL'     'IT_ZGL056' 'VLR_APROP_MES_USD'    'Vlr.Aprop.Mes.USD'  '' '' '' 'X',
  35   'ZGLT068'  'VLR_RISCO_BRL'     'IT_ZGL056' 'VLR_APROP_MES_BRL'    'Vlr.Aprop.Mes.BRL'  '' '' '' 'X',
  36   'ZGLT068'  'AUFNR'             'IT_ZGL056' 'AUFNR'                'Ordem'              '' '' '' 'X',
  36   'ZGLT068'  'VORNR'             'IT_ZGL056' 'VORNR'                'Nr.Op.'             '' '' '' 'X'.

ENDFORM.
*-CS2022000122 - 07.03.2022 - JT - fim

*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_ESTRUTURA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_montar_estrutura  USING  p_col_pos   p_ref_tabname p_ref_fieldname p_tabname p_field
                                p_scrtext_l p_outputlen   p_edit          p_sum     p_emphasize .

  CLEAR w_fieldcatalog.
  w_fieldcatalog-fieldname     = p_field.
  w_fieldcatalog-tabname       = p_tabname.
  w_fieldcatalog-ref_table     = p_ref_tabname.
  w_fieldcatalog-ref_field     = p_ref_fieldname.

  w_fieldcatalog-key           = ' '.

  IF p_field = 'IM_POSNR' OR
     p_field = 'TCODE'.
    w_fieldcatalog-key           = 'X'.
  ENDIF.

  w_fieldcatalog-edit          = p_edit.
  w_fieldcatalog-do_sum        = p_sum.

  w_fieldcatalog-col_pos       = p_col_pos.

  IF p_outputlen IS NOT INITIAL.
    w_fieldcatalog-outputlen   = p_outputlen.
  ENDIF.

  IF p_field = 'VLR_LOCAL' AND p_tabname = 'IT_HCM'.
    w_fieldcatalog-edit_mask = '***,**'.
  ENDIF.

* w_fieldcatalog-no_zero       = 'X'.
  w_fieldcatalog-no_out        = ' '.
  w_fieldcatalog-reptext       = p_scrtext_l.
  w_fieldcatalog-scrtext_s     = p_scrtext_l.
  w_fieldcatalog-scrtext_m     = p_scrtext_l.
  w_fieldcatalog-scrtext_l     = p_scrtext_l.
  w_fieldcatalog-emphasize     = p_emphasize.

  APPEND w_fieldcatalog TO t_fieldcatalog.
ENDFORM.                    " F_MONTAR_ESTRUTURA

*&---------------------------------------------------------------------*
*&      Form  F_ZIM01
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_zim01 .

  DATA: t_resultado TYPE TABLE OF  zhcme_sysphera_zim01,
        t_custo     TYPE TABLE OF  rsparams.

  REFRESH it_zim01.
  CALL FUNCTION 'Z_ZIM01_SYSPHERA'
    EXPORTING
      i_ano     = p_ano
      i_mes     = p_mes
      i_area    = p_kokrs
    TABLES
      resultado = t_resultado
      t_custo   = t_custo.

  LOOP AT t_resultado INTO DATA(w_resultado).
    MOVE-CORRESPONDING w_resultado TO wa_zim01.
    APPEND wa_zim01 TO  it_zim01.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_DRE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_dre .
  REFRESH it_dre.
  IF p_versn IS INITIAL.
    MESSAGE 'Informe a estrutura da DRE' TYPE 'I'.
    EXIT.
  ENDIF.
  IF p_bukrs IS INITIAL.
    MESSAGE 'Informe a empresa da DRE' TYPE 'I'.
    EXIT.
  ENDIF.
  DATA: it_excel          TYPE TABLE OF zgl030_est_d WITH HEADER LINE.
  DATA: it_excel_usd       TYPE TABLE OF zgl030_est_d WITH HEADER LINE.
  CALL FUNCTION 'Z_PESQUISA_DRE'
    EXPORTING
      i_bukrs      = p_bukrs
      i_gjahr      = p_ano
      i_monati     = p_mes
      i_monatf     = p_mes
      i_versn      = p_versn
      i_waers      = 'BRL'
    TABLES
      t_zgl030_est = it_excel.

  CALL FUNCTION 'Z_PESQUISA_DRE'
    EXPORTING
      i_bukrs      = p_bukrs
      i_gjahr      = p_ano
      i_monati     = p_mes
      i_monatf     = p_mes
      i_versn      = p_versn
      i_waers      = 'USD'
    TABLES
      t_zgl030_est = it_excel_usd.

  DELETE it_excel     WHERE saknr IS INITIAL.
  DELETE it_excel_usd WHERE saknr IS INITIAL.
  LOOP AT it_excel_usd.
    MOVE-CORRESPONDING it_excel_usd TO it_excel.
    it_excel-kosar = 'D'.
    APPEND it_excel.
  ENDLOOP.
  DATA: vlr_rea(20),

        wl_cont      TYPE sy-tabix,
        tx_aux       TYPE c,
        it_excel_aux LIKE TABLE OF it_excel WITH HEADER LINE.

  DATA: v_loopi    TYPE i,
        v_loopf    TYPE i,
        v_mesan    TYPE i,
        v_reac(10),
        v_mes(02).

  FIELD-SYMBOLS: <fs_rea> TYPE any.

  DELETE it_excel WHERE saknr IS INITIAL.
  LOOP AT it_excel.

    v_loopi = p_mes.
    v_loopf = p_mes.
    IF v_loopf = 0.
      v_loopf = v_loopi.
    ENDIF.
    CLEAR:  vlr_rea.
    WHILE v_loopi LE v_loopf.
      v_mes = v_loopi.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = v_mes
        IMPORTING
          output = v_mes.
      CONCATENATE 'VLR_REA' v_mes INTO v_reac.

      ASSIGN COMPONENT v_reac OF STRUCTURE it_excel TO <fs_rea>.
      "
      ADD 1 TO v_loopi.
    ENDWHILE.
    wa_dre-ano             = p_ano.
    wa_dre-mes             = p_mes.
    wa_dre-centro_custo    = it_excel-kostl.
    SELECT SINGLE ltext FROM cskt INTO wa_dre-nome_centro
         WHERE spras EQ sy-langu AND
               kokrs EQ p_kokrs   AND
               kostl EQ it_excel-kostl AND
               datbi GE sy-datum .
    wa_dre-cod_conta       = it_excel-saknr.
    wa_dre-nome_conta      = it_excel-txt50.
    CONCATENATE it_excel-nivel '-' it_excel-saknr INTO wa_dre-id_concatenado.
*    WA_DRE-COD_PRODUTO     = IT_EXCEL-.
    wa_dre-nome_produto    = it_excel-nitxt.
    IF it_excel-kosar NE 'D'.
      wa_dre-vlr_local       = <fs_rea>.
    ELSE.
      wa_dre-vlr_dolar       = <fs_rea>.
    ENDIF.
    IF <fs_rea> = 0.
      CONTINUE.
    ENDIF.
    COLLECT wa_dre INTO it_dre.
    CLEAR wa_dre.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_KSB1
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_ksb1 .
  DATA: t_resultado  TYPE TABLE OF zsys_ksb1,
        w_resultado  TYPE zsys_ksb1,
        it_selection TYPE TABLE OF rsparams,
        wa_selection LIKE LINE OF it_selection.

  REFRESH it_ksb1.
  LOOP AT s_custo INTO DATA(w_custo).
    wa_selection-selname = 'KOSTL'.
    wa_selection-kind    = 'S'. "S-Select-options P-Parameters
    wa_selection-sign    = w_custo-sign.
    wa_selection-option  = w_custo-option.
    wa_selection-low     = w_custo-low.
    wa_selection-high    = w_custo-high.
    APPEND wa_selection TO it_selection.
  ENDLOOP.
  CALL FUNCTION 'Z_KSB1_SYSPHERA'
    EXPORTING
      i_ano     = p_ano
      i_mes     = p_mes
      i_area    = p_kokrs
    TABLES
      resultado = t_resultado
      t_custo   = it_selection.

  LOOP AT t_resultado INTO w_resultado.
    APPEND w_resultado TO it_ksb1.
  ENDLOOP.

ENDFORM.

*-CS2022000122 - 07.03.2022 - JT - inicio
*&---------------------------------------------------------------------*
*&      Form  F_ZGL056
*&---------------------------------------------------------------------*
FORM f_zgl056.

  DATA: t_resultado TYPE TABLE OF zsys_zgl056,
        w_resultado TYPE zsys_zgl056.

  CALL FUNCTION 'Z_ZGL056_SYSPHERA'
    EXPORTING
      i_bukrs   = p_bukrs
      i_ano     = p_ano
      i_mes     = p_mes
    TABLES
      resultado = t_resultado.

  LOOP AT t_resultado INTO w_resultado.
    APPEND w_resultado  TO it_zgl056.
  ENDLOOP.

ENDFORM.
*-CS2022000122 - 07.03.2022 - JT - fim

FORM f_mapa.
  DATA(go) = NEW zcl_int_ib_fi_prot_mcomp_inv( ).

  DATA: vmonat TYPE monat,
        vgjahr TYPE gjahr.


  REFRESH: it_conf, it_ordens.
  LOOP AT s_aufnr ASSIGNING FIELD-SYMBOL(<fs_aufnr>).
    wa_ordens-aufnr = <fs_aufnr>-low.
    APPEND wa_ordens TO it_ordens.
    FREE wa_ordens.
  ENDLOOP.

  SORT it_ordens.
  DELETE ADJACENT DUPLICATES FROM it_ordens.

  DATA lv_string TYPE string.
  vmonat = p_mes.
  vgjahr = p_ano.
  go->set( i_area    = p_kokrs
           i_perio   = vmonat
           i_ano     = vgjahr
           i_prot    = lv_string
           it_ordens = it_ordens[] ).

  go->run( IMPORTING lt_message = lv_message
                         it_conf    = it_conf
                        ).

  REFRESH it_conf2.
  DATA wa_conf2 TYPE ty_conf.
  LOOP AT it_conf INTO DATA(wa_conf).
    SELECT SINGLE maktx INTO @DATA(_maktx) FROM makt WHERE  matnr = @wa_conf-produto AND spras = 'P'.
    MOVE-CORRESPONDING wa_conf TO wa_conf2.
    wa_conf2-maktx = _maktx.
    APPEND wa_conf2 TO it_conf2.
  ENDLOOP.

  FREE  go.
  cl_salv_bs_runtime_info=>clear_all( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_inves
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_inves .

  DATA: t_posnr TYPE TABLE OF zsds_posnr_range,
        w_posnr TYPE zsds_posnr_range.

  DATA: lv_first_day TYPE sy-datum,
        lv_last_day  TYPE sy-datum.

  DATA: lv_gjahr TYPE  t009b-bdatj,
        lv_periv TYPE  t009b-periv.

*------------------------------
* get campos da estrutura
*------------------------------
  FREE: it_fields.

  it_struct  ?= cl_abap_typedescr=>describe_by_name( 'zsys_inves' ).
  it_fields[] = it_struct->get_components( ).
*------------------------------

  REFRESH: it_inves, it_inves_ret, t_posnr.

  lv_first_day =  p_ano && p_mes && '01'. "VERIFICAR

  CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
    EXPORTING
      day_in            = lv_first_day
    IMPORTING
      last_day_of_month = lv_last_day
    EXCEPTIONS
      day_in_no_date    = 1
      OTHERS            = 2.

  LOOP AT s_posnr.
    MOVE-CORRESPONDING s_posnr TO w_posnr.
    APPEND w_posnr             TO t_posnr.
  ENDLOOP.

  IF s_perio-low IS NOT INITIAL.
    lv_first_day = s_perio-low.
    lv_last_day  = s_perio-high.
  ENDIF.

  CALL FUNCTION 'Z_INVES_SYSPHERA_REPORT'
    EXPORTING
      i_area       = p_kokrs
      i_empresa    = p_bukrs
      i_dt_inicial = lv_first_day
      i_dt_final   = lv_last_day
    TABLES
      posnr        = t_posnr
      resultado    = it_inves_ret.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ZIM02
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*

*** Stefanini - IR237530 - 27/05/2025 - LAZAROSR - Início de Alteração
*FORM f_zim02 TABLES t_inv STRUCTURE zim01_sol_ap_inv.
*
*  SORT it_zim02 BY centro_custo.
*  DATA vbuzei TYPE buzei.
*  DATA vsolicitacao_invest TYPE zim01_sol_ap_inv-solicitacao_invest.
*  DATA it_retorno_rest  TYPE TABLE OF zmm_sysphera_zim02.
*  DATA is_valid_retorno_rest TYPE c LENGTH 50.
*  DATA vloop(1).
*  DATA vmseq(1).
*
*  CALL FUNCTION 'ZMMF_SYSPHERA_ZIM15'
*    IMPORTING
*      is_valid        = is_valid_retorno_rest    " Comentário
*    TABLES
*      t_investimentos = it_retorno_rest.   " SYSPHERA - Estrutura ZIM01 .
*
*  CHECK ( is_valid_retorno_rest = 'Sucess' ).
*
*  IF it_retorno_rest IS INITIAL .
*    MESSAGE 'Não retornou dados de importação!' TYPE 'I'.
*    EXIT.
*  ENDIF.
*
*  DATA(it_retorno_rest_auxi) = it_retorno_rest.
*
*  SORT  it_retorno_rest BY cod_centro_custo .
*  SORT  it_retorno_rest_auxi BY cod_centro_custo identificador.
*
*  DELETE ADJACENT DUPLICATES FROM it_retorno_rest  COMPARING cod_centro_custo.
*
*  LOOP AT it_retorno_rest INTO DATA(wa_it_rest).
*    DATA(count) = 0.
*    LOOP AT it_retorno_rest_auxi INTO DATA(wa_it_rest_auxi) WHERE cod_centro_custo = wa_it_rest-cod_centro_custo .
*      wa_zim02-ano             = wa_it_rest_auxi-ano.
*      wa_zim02-centro_custo    =  |{ wa_it_rest_auxi-cod_centro_custo ALPHA = IN }|.
*      wa_zim02-id_investimento = wa_it_rest_auxi-identificador+22(10).
*      vsolicitacao_invest =  wa_zim02-id_investimento.
*      SELECT SINGLE *
*          INTO @DATA(w_csks)
*          FROM csks
*          WHERE kostl   EQ @wa_zim02-centro_custo
*          AND   datbi GE @sy-datum .
*      "
*      CLEAR vloop.
*      WHILE vloop = ' '.
*        "numeração das linhas
*        ADD 1 TO count.
*        vbuzei = count.
*        SELECT SINGLE  *
*         FROM zim01_sol_ap_inv
*         INTO @DATA(w_zim01_auxi) WHERE bukrs              = @w_csks-bukrs
*                                  AND   gsber              = @w_csks-gsber
*                                  AND   ano                = @wa_zim02-ano
*                                  AND   kostl              = @wa_zim02-centro_custo
*                                  AND   buzei              = @vbuzei.
*        IF sy-subrc = 0.
*          IF ( w_zim01_auxi-observacoes NE 'Importado Sysphera' AND w_zim01_auxi-observacoes NE 'IMPORTADO SYSPHERA' ).
*            CONTINUE. " Não adiciona essa linha pois não é do sysphera ou ja tem solicitacao
*          ELSEIF w_zim01_auxi-posnr NE ' ' AND w_zim01_auxi-solicitacao_invest EQ vsolicitacao_invest.
**            vloop = 'C'.
**            EXIT.
*            SELECT SINGLE  *
*                FROM zim01_sol_ap_inv
*                INTO @DATA(w_zim01_auxi_2) WHERE bukrs              = @w_csks-bukrs
*                                          AND   gsber              = @w_csks-gsber
*                                          AND   ano                = @wa_zim02-ano
*                                          AND   kostl              = @wa_zim02-centro_custo
*                                          AND   solicitacao_invest = @vsolicitacao_invest.
*            IF sy-subrc = 0.
*              wa_zim02-numero_linha = w_zim01_auxi_2-buzei.
*            ELSE.
*              CONTINUE. "adiciona +1 no contador para novo investimento aprovado
*            ENDIF.
*          ELSEIF w_zim01_auxi-solicitacao_invest NE vsolicitacao_invest.
*            SELECT SINGLE  *
*            FROM zim01_sol_ap_inv
*            INTO @DATA(w_zim01_auxi2) WHERE bukrs              = @w_csks-bukrs
*                                      AND   gsber              = @w_csks-gsber
*                                      AND   ano                = @wa_zim02-ano
*                                      AND   kostl              = @wa_zim02-centro_custo
*                                      AND   solicitacao_invest = @vsolicitacao_invest.
*            IF sy-subrc = 0.
*              wa_zim02-numero_linha = w_zim01_auxi2-buzei.
*            ELSE.
*              CONTINUE. "adiciona +1 no contador para novo investimento aprovado
*            ENDIF.
*          ELSE.
*            wa_zim02-numero_linha = w_zim01_auxi-buzei.
*          ENDIF.
*        ELSE.
*          SELECT SINGLE  *
*            FROM zim01_sol_ap_inv
*            INTO @DATA(w_zim01_auxi3) WHERE bukrs              = @w_csks-bukrs
*                                      AND   gsber              = @w_csks-gsber
*                                      AND   ano                = @wa_zim02-ano
*                                      AND   kostl              = @wa_zim02-centro_custo
*                                      AND   solicitacao_invest = @vsolicitacao_invest.
*          IF sy-subrc = 0.
*            wa_zim02-numero_linha = w_zim01_auxi3-buzei.
*            SUBTRACT 1 FROM count.
*          ELSE.
*            wa_zim02-numero_linha = count.
*          ENDIF.
*        ENDIF.
*        vloop = 'X'.
*      ENDWHILE.
*
*      IF vloop = 'C'.
*        CONTINUE.
*      ENDIF.
*
*      wa_zim02-ano = wa_it_rest_auxi-ano.
*      wa_zim02-mes = wa_it_rest_auxi-mes.
*      """"""""""US 128307"""""""""""""""
**      wa_zim02-fase = COND #( WHEN wa_it_rest_auxi-fase = 'Projetado' THEN  '01' ELSE '02' ) .
*      IF wa_it_rest_auxi-fase = 'Projetado'.
*        wa_zim02-fase = '01'.
*      ELSEIF wa_it_rest_auxi-fase = 'Extra'.
*        wa_zim02-fase = '02'.
*      ELSE.
*        wa_zim02-fase = '03'. "
*      ENDIF.
*      """"""""""US 128307"""""""""""""""
*
*      "WA_ZIM02-CODIGO_CENARIO = COND #( WHEN WA_IT_REST-FASE = 'Projetado' THEN 1 ELSE 2 ) .
*      wa_zim02-centro_custo     =  |{ wa_it_rest_auxi-cod_centro_custo ALPHA = IN }|.
*      wa_zim02-nome_centro      = ''.
*      wa_zim02-cod_conta        = ''.
*      wa_zim02-cod_item         = wa_it_rest_auxi-cod_item_investimento.
*      wa_zim02-nome_item        = wa_it_rest_auxi-nome_item_investimento.
*      wa_zim02-cod_grupo        = wa_it_rest_auxi-cod_grupo.
*      wa_zim02-nome_grupo       = wa_it_rest_auxi-nome_grupo.
*      wa_zim02-str_objetivo     = wa_it_rest_auxi-objetivo.
*      wa_zim02-str_descricao    = wa_it_rest_auxi-descricao.
*      wa_zim02-str_finalidade   = wa_it_rest_auxi-cod_finalidade+8(2).
*      wa_zim02-qtd              = wa_it_rest_auxi-quantidade.
*      wa_zim02-vlr_local        = wa_it_rest_auxi-total_local.
*      wa_zim02-vlr_dolar        = wa_it_rest_auxi-total_usd.
*      wa_zim02-id_investimento  = wa_it_rest_auxi-identificador+22(10).
*      wa_zim02-cod_natureza     = wa_it_rest_auxi-cod_natureza+8(2).
*      wa_zim02-nome_natureza    = wa_it_rest_auxi-nome_natureza.
*      APPEND wa_zim02 TO it_zim02.
*      CLEAR wa_zim02.
*    ENDLOOP.
*
*  ENDLOOP.
*
*
*
*  DATA ultimo_dia_mes TYPE sy-datum.
*  DATA primeiro_dia_mes TYPE sy-datum.
*
*  LOOP AT it_zim02 INTO wa_zim02.
*
*    primeiro_dia_mes = |{ wa_zim02-ano }{ wa_zim02-mes }01|.
*
*    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
*      EXPORTING
*        day_in            = primeiro_dia_mes
*      IMPORTING
*        last_day_of_month = ultimo_dia_mes.
*
*
*
*    SELECT SINGLE *
*      INTO @DATA(w_csks2)
*      FROM csks
*      WHERE kostl   EQ @wa_zim02-centro_custo
*      AND   datbi GE @sy-datum .
*
*
*    t_inv-bukrs           = w_csks2-bukrs.
*    t_inv-gsber           = w_csks2-gsber.
*    t_inv-buzei           = wa_zim02-numero_linha.
*    t_inv-ano             = wa_zim02-ano.
*    t_inv-safra           = ''. "WA_ZIM02-ANO. "WA_ZIM02-CODIGO_CENARIO+0(4).
*    t_inv-safra2          = ''.
*    t_inv-kostl           = wa_zim02-centro_custo.
*    t_inv-fase            = wa_zim02-fase.
*    t_inv-finalidade      =  wa_zim02-str_finalidade.  ""'05'. "05""WA_ZIM02-STR_FINALIDADE.
*    t_inv-objetivo        = wa_zim02-str_objetivo.
*    t_inv-descr_item      = wa_zim02-str_descricao.
*    t_inv-menge           = wa_zim02-qtd.
*    t_inv-vlr_unitario    = wa_zim02-vlr_local / wa_zim02-qtd.
*    t_inv-vlr_total       = wa_zim02-vlr_local.
*    t_inv-vl_usd          = wa_zim02-vlr_dolar.
*    t_inv-dt_inicio       = primeiro_dia_mes.
*    t_inv-dt_fim          = ultimo_dia_mes.
*    t_inv-observacoes   = 'Importado Sysphera'.
*    t_inv-saknr         = wa_zim02-cod_conta.
*    t_inv-txt20         = wa_zim02-nome_conta.
*    t_inv-status_aprov  = '1'.
*    t_inv-status_cta    = '2'.
*    t_inv-cod_gpo       = wa_zim02-cod_item .
*    t_inv-cod_item      = ''.
*    t_inv-izwek         = wa_zim02-cod_natureza.
*    t_inv-txt50         = wa_zim02-nome_natureza.
*    t_inv-solicitacao_invest   = |{ wa_zim02-id_investimento ALPHA = IN }|.
*    t_inv-posnr         = wa_zim02-posnr.
*    t_inv-usuario       = sy-uname.
*    t_inv-data_entr     = sy-datum.
*    t_inv-hora_entr     = sy-uzeit.
*    APPEND t_inv.
*
*  ENDLOOP.
*
*ENDFORM.

FORM f_zim02 TABLES t_inv STRUCTURE zim01_sol_ap_inv.

  DATA:
    it_retorno_rest       TYPE TABLE OF zmm_sysphera_zim02,
    is_valid_retorno_rest TYPE c LENGTH 50,

    vl_ultimo_dia_mes     TYPE sy-datum,
    vl_primeiro_dia_mes   TYPE sy-datum,
    vl_ano_vigente        TYPE gjahr,
    vl_buzei              TYPE zim01_sol_ap_inv-buzei.


  CALL FUNCTION 'ZMMF_SYSPHERA_ZIM15'
    IMPORTING
      is_valid        = is_valid_retorno_rest
    TABLES
      t_investimentos = it_retorno_rest.

  IF it_retorno_rest       IS INITIAL
  OR is_valid_retorno_rest NE 'Sucess'.

    MESSAGE 'Não retornou dados de importação!' TYPE 'I'.
    EXIT.

  ENDIF.

  vl_ano_vigente = sy-datum(4).
  DELETE it_retorno_rest WHERE ano NE vl_ano_vigente.


  LOOP AT it_retorno_rest INTO DATA(wa_retorno).

    wa_zim02-ano             = wa_retorno-ano.
    wa_zim02-centro_custo    =  |{ wa_retorno-cod_centro_custo ALPHA = IN }|.
    wa_zim02-id_investimento = wa_retorno-identificador+22(10).

    wa_zim02-ano = wa_retorno-ano.
    wa_zim02-mes = wa_retorno-mes.

    IF wa_retorno-fase = 'Projetado'.
      wa_zim02-fase = '01'.
    ELSEIF wa_retorno-fase = 'Extra'.
      wa_zim02-fase = '02'.
    ELSE.
      wa_zim02-fase = '03'.
    ENDIF.

    wa_zim02-centro_custo     =  |{ wa_retorno-cod_centro_custo ALPHA = IN }|.
    wa_zim02-nome_centro      = ''.
    wa_zim02-cod_conta        = ''.
    wa_zim02-cod_item         = wa_retorno-cod_item_investimento.
    wa_zim02-nome_item        = wa_retorno-nome_item_investimento.
    wa_zim02-cod_grupo        = wa_retorno-cod_grupo.
    wa_zim02-nome_grupo       = wa_retorno-nome_grupo.
    wa_zim02-str_objetivo     = wa_retorno-objetivo.
    wa_zim02-str_descricao    = wa_retorno-descricao.
    wa_zim02-str_finalidade   = wa_retorno-cod_finalidade+8(2).
    wa_zim02-qtd              = wa_retorno-quantidade.
    wa_zim02-vlr_local        = wa_retorno-total_local.
    wa_zim02-vlr_dolar        = wa_retorno-total_usd.
    wa_zim02-id_investimento  = wa_retorno-identificador+22(10).
    wa_zim02-cod_natureza     = wa_retorno-cod_natureza+8(2).
    wa_zim02-nome_natureza    = wa_retorno-nome_natureza.
    APPEND wa_zim02 TO it_zim02.
    CLEAR wa_zim02.

  ENDLOOP.

  IF it_zim02 IS NOT INITIAL.

    SELECT bukrs,
           gsber,
           kostl
      INTO TABLE @DATA(lt_csks)
      FROM csks
      FOR ALL ENTRIES IN @it_zim02
      WHERE kostl EQ @it_zim02-centro_custo
        AND datbi GE @sy-datum.

    IF sy-subrc IS INITIAL.
      SORT lt_csks BY kostl.
    ENDIF.

  ENDIF.

  LOOP AT it_zim02 INTO wa_zim02.

    vl_primeiro_dia_mes = |{ wa_zim02-ano }{ wa_zim02-mes }01|.

    CALL FUNCTION 'LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = vl_primeiro_dia_mes
      IMPORTING
        last_day_of_month = vl_ultimo_dia_mes.

    READ TABLE lt_csks INTO DATA(wa_csks)
                       WITH KEY kostl = wa_zim02-centro_custo
                                                BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      t_inv-bukrs = wa_csks-bukrs.
      t_inv-gsber = wa_csks-gsber.
    ENDIF.

    t_inv-ano                = wa_zim02-ano.
    t_inv-safra              = ''.
    t_inv-safra2             = ''.
    t_inv-kostl              = wa_zim02-centro_custo.
    t_inv-fase               = wa_zim02-fase.
    t_inv-finalidade         = wa_zim02-str_finalidade.
    t_inv-objetivo           = wa_zim02-str_objetivo.
    t_inv-descr_item         = wa_zim02-str_descricao.
    t_inv-menge              = wa_zim02-qtd.
    t_inv-vlr_unitario       = wa_zim02-vlr_local / wa_zim02-qtd.
    t_inv-vlr_total          = wa_zim02-vlr_local.
    t_inv-vl_usd             = wa_zim02-vlr_dolar.
    t_inv-dt_inicio          = vl_primeiro_dia_mes.
    t_inv-dt_fim             = vl_ultimo_dia_mes.
    t_inv-observacoes        = 'Importado Sysphera'.
    t_inv-saknr              = wa_zim02-cod_conta.
    t_inv-txt20              = wa_zim02-nome_conta.
    t_inv-status_aprov       = '1'.
    t_inv-status_cta         = '2'.
    t_inv-cod_gpo            = wa_zim02-cod_item .
    t_inv-cod_item           = ''.
    t_inv-izwek              = wa_zim02-cod_natureza.
    t_inv-txt50              = wa_zim02-nome_natureza.
    t_inv-solicitacao_invest = |{ wa_zim02-id_investimento ALPHA = IN }|.
    t_inv-posnr              = wa_zim02-posnr.
    t_inv-usuario            = sy-uname.
    t_inv-data_entr          = sy-datum.
    t_inv-hora_entr          = sy-uzeit.
    APPEND t_inv.

  ENDLOOP.

  SORT t_inv BY bukrs gsber ano safra safra2 kostl.
  LOOP AT t_inv ASSIGNING FIELD-SYMBOL(<fs_inv>).

    AT NEW kostl.
      vl_buzei = 1.
    ENDAT.

    <fs_inv>-buzei = vl_buzei.
    vl_buzei += 1.

  ENDLOOP.

ENDFORM.

*FORM f_zim02_add TABLES t_inv STRUCTURE zim01_sol_ap_inv.
*  DATA t_zim01_auxi TYPE TABLE OF zim01_sol_ap_inv.
*  DATA w_zim01     TYPE zim01_sol_ap_inv.
*
*
*  SELECT * FROM zim01_sol_ap_inv INTO TABLE t_zim01_auxi WHERE posnr <> ' ' AND (  observacoes = 'Importado Sysphera' OR observacoes = 'IMPORTADO SYSPHERA' ) .
*
*  t_inv_aux[] = t_inv[].
*
*
*  SORT  t_inv_aux BY ano.
*
*  DELETE ADJACENT DUPLICATES FROM t_inv_aux COMPARING ano.
*
*  LOOP AT t_inv_aux INTO DATA(w_invt_aux).
*
*    DELETE FROM zim01_sol_ap_inv WHERE ano = w_invt_aux-ano AND  ( observacoes  =  'Importado Sysphera' OR observacoes = 'IMPORTADO SYSPHERA' )
*    AND  posnr = ' ' .
*
*  ENDLOOP.
*  COMMIT WORK.
*
**  DELETE T_INV WHERE POSNR <> ' '.
*
*  LOOP AT t_zim01_auxi INTO DATA(w_zim01_auxi).
*    DELETE  t_inv
*    WHERE bukrs                 = w_zim01_auxi-bukrs
*    AND   gsber                 = w_zim01_auxi-gsber
*    AND   ano                   = w_zim01_auxi-ano
*    AND   kostl                 = w_zim01_auxi-kostl
*    AND   solicitacao_invest    = w_zim01_auxi-solicitacao_invest.
*  ENDLOOP.
*
*
*
*  "Atualiza
*  MODIFY zim01_sol_ap_inv FROM TABLE t_inv.
*  COMMIT WORK.
*
*  IF sy-calld NE 'X'.
*    MESSAGE 'Importação Realizada.' TYPE 'I'.
*    LEAVE TO SCREEN 0.
*  ENDIF.
*
*
*ENDFORM.

FORM f_zim02_add TABLES t_inv STRUCTURE zim01_sol_ap_inv.

  t_inv_aux[] = t_inv[].

  SORT t_inv_aux BY ano.
  DELETE ADJACENT DUPLICATES FROM t_inv_aux COMPARING ano.

  LOOP AT t_inv_aux INTO DATA(w_invt_aux).

    DELETE FROM zim01_sol_ap_inv
           WHERE ano       = w_invt_aux-ano
    AND  (     observacoes =  'Importado Sysphera'
            OR observacoes = 'IMPORTADO SYSPHERA' )
    AND  posnr = ' '.

  ENDLOOP.

  COMMIT WORK AND WAIT.

  "Atualiza
  MODIFY zim01_sol_ap_inv FROM TABLE t_inv.
  COMMIT WORK.

  IF sy-calld NE 'X'.
    MESSAGE 'Importação Realizada.' TYPE 'I'.
    LEAVE TO SCREEN 0.
  ENDIF.

ENDFORM.
*** Stefanini - IR237530 - 27/05/2025 - LAZAROSR - Fim de Alteração

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_HCM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_hcm .
  REFRESH it_hcm.

  DATA it_folha TYPE TABLE OF zhcme_sysphera_folha.
  CALL FUNCTION 'ZHCMF_SYSPHERA_FOLHA'
    EXPORTING
      mes      = p_mes
      ano      = p_ano
    TABLES
      it_folha = it_folha.

  LOOP AT it_folha INTO DATA(wa_folha).
    MOVE-CORRESPONDING wa_folha TO wa_hcm.
    APPEND wa_hcm TO it_hcm.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HCM2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_hcm2 .
  REFRESH it_hcm2.

  DATA it_funcionarios TYPE TABLE OF  zhcme_sysphera_funcionarios.

*** BUG - 110727 - Inicio - CBRAND
  DATA: t_kostl TYPE TABLE OF zhcms_kostl_sysphera,
        w_kostl LIKE LINE OF t_kostl,
        t_bukrs TYPE TABLE OF zhcms_bukrs,
        w_bukrs LIKE LINE OF t_bukrs.

  IF p_bukrs IS NOT INITIAL.
    w_bukrs-bukrs =  p_bukrs.
    APPEND w_bukrs TO t_bukrs.
    CLEAR: w_bukrs.
  ENDIF.

  IF s_custo IS NOT INITIAL.
    LOOP AT s_custo.
      w_kostl-sign = s_custo-sign.
      w_kostl-option = s_custo-option.
      w_kostl-low =  s_custo-low.
      w_kostl-high =  s_custo-high.
      APPEND w_kostl TO t_kostl.
      CLEAR: w_kostl.
    ENDLOOP.
  ENDIF.
*** BUG - 110727 - Fim - CBRAND
  CALL FUNCTION 'ZHCMF_SYSPHERA_FUNCIONARIOS'
    EXPORTING
      i_mes           = p_mes
      i_ano           = p_ano
    TABLES
      t_bukrs         = t_bukrs[]
      t_kostl         = t_kostl[]
      it_funcionarios = it_funcionarios.

  IF s_custo IS NOT INITIAL.
    DELETE it_funcionarios WHERE centro_custo NOT IN s_custo.
  ENDIF.

  LOOP AT it_funcionarios INTO DATA(wa_funcionarios).
    MOVE-CORRESPONDING wa_funcionarios TO wa_hcm2.

    CONCATENATE wa_funcionarios-dt_nascimento+6(2) '/' wa_funcionarios-dt_nascimento+4(2) '/' wa_funcionarios-dt_nascimento+0(4) INTO wa_hcm2-dt_nascimento.

    CASE wa_funcionarios-sexo.
      WHEN '1'.
        wa_hcm2-sexo = 'Masculino'.
      WHEN '2'.
        wa_hcm2-sexo = 'Feminino'.
      WHEN OTHERS.
        wa_hcm2-sexo = 'Desconhecido'.
    ENDCASE.

    APPEND wa_hcm2 TO it_hcm2.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_KP06
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_kp06 .



  DATA is_valid_retorno_rest TYPE c LENGTH 50.

  CALL FUNCTION 'ZMMF_SYSPHERA_KP06'
    IMPORTING
      is_valid   = is_valid_retorno_rest    " Comentário
    TABLES
      t_despesas = it_retorno_rest.   " SYSPHERA - Estrutura KP06 .

  CHECK ( is_valid_retorno_rest = 'Sucess' ).

  it_kp06 = VALUE t_kp06( FOR wa_it_retorno_rest IN it_retorno_rest[]
  (

            ano               = wa_it_retorno_rest-ano
            mes               = wa_it_retorno_rest-mes
            codigo_cenario    = wa_it_retorno_rest-cod_cenario
            nome_cenario      = wa_it_retorno_rest-nome_cenario
            centro_custo      = wa_it_retorno_rest-cod_centro_custo
            nome_centro       = wa_it_retorno_rest-nome_centro_custo
            cod_conta         = wa_it_retorno_rest-cod_conta
            nome_conta        = wa_it_retorno_rest-nome_conta
            vlr_local         = wa_it_retorno_rest-vlr_local
            vlr_dolar         = wa_it_retorno_rest-vlr_dolar
            ds_area           = wa_it_retorno_rest-ds_area

   ) ).



  IF it_kp06[] IS INITIAL.
    MESSAGE 'Não retornou dados de importação!' TYPE 'I'.
    EXIT.
  ENDIF.


ENDFORM.


FORM f_kp06_add.
  " VAI ZERAR TODOS OS LANÇAMENTOS QUE FORAM REALIZADOS NA ULTIMA IMPORTAÇÃO
  PERFORM f_kp06_lan_version.

  CLEAR : lv_object, lv_value, ls_headerinfo.
  REFRESH : lt_indexstruc, lt_coobject.


  it_kp06_aux[] =  it_kp06[].
  it_kp06_area[] = it_kp06[].

  SORT it_kp06_area BY  ds_area.
  DELETE ADJACENT DUPLICATES FROM it_kp06_area COMPARING ds_area.

  SORT it_kp06_aux BY centro_custo cod_conta.
  DELETE ADJACENT DUPLICATES FROM it_kp06_aux COMPARING centro_custo cod_conta.
  SORT it_kp06 BY centro_custo cod_conta mes.

  LOOP AT it_kp06_area INTO DATA(wa_kp06_area).

    lv_object = 1.
    lv_value =  1.

    LOOP AT it_kp06_aux INTO DATA(wa_kp06_aux) WHERE ds_area = wa_kp06_area-ds_area.

      ls_headerinfo-co_area       = wa_kp06_aux-ds_area. "P_KOKRS.
      ls_headerinfo-fisc_year     = wa_kp06_aux-ano . "P_ANO.
      ls_headerinfo-period_from   = '001'.
      ls_headerinfo-period_to     = '012'.
      ls_headerinfo-version       = |{ p_versio ALPHA = OUT }|.
      ls_headerinfo-plan_currtype = 'T'.
      ls_headerinfo-doc_hdr_tx    = 'Origem Sysphera'.


      lt_indexstruc-object_index = lv_object.
      lt_indexstruc-value_index  = lv_value.
      APPEND lt_indexstruc.


      lt_coobject-object_index = lv_object.
      lt_coobject-costcenter = wa_kp06_aux-centro_custo.

      APPEND lt_coobject.


      CLEAR ls_pervalue.
      ls_pervalue-value_index = lv_object.
      ls_pervalue-cost_elem =  wa_kp06_aux-cod_conta.
      ls_pervalue-trans_curr = 'BRL'.

      LOOP AT it_kp06 INTO wa_kp06 WHERE  centro_custo = wa_kp06_aux-centro_custo
                                   AND    cod_conta    = wa_kp06_aux-cod_conta.

        CASE wa_kp06-mes.
          WHEN '01'. ls_pervalue-fix_val_per01    = wa_kp06-vlr_local.
          WHEN '02'. ls_pervalue-fix_val_per02    = wa_kp06-vlr_local.
          WHEN '03'. ls_pervalue-fix_val_per03    = wa_kp06-vlr_local.
          WHEN '04'. ls_pervalue-fix_val_per04    = wa_kp06-vlr_local.
          WHEN '05'. ls_pervalue-fix_val_per05    = wa_kp06-vlr_local.
          WHEN '06'. ls_pervalue-fix_val_per06    = wa_kp06-vlr_local.
          WHEN '07'. ls_pervalue-fix_val_per07    = wa_kp06-vlr_local.
          WHEN '08'. ls_pervalue-fix_val_per08    = wa_kp06-vlr_local.
          WHEN '09'. ls_pervalue-fix_val_per09    = wa_kp06-vlr_local.
          WHEN '10'. ls_pervalue-fix_val_per10    = wa_kp06-vlr_local.
          WHEN '11'. ls_pervalue-fix_val_per11    = wa_kp06-vlr_local.
          WHEN '12'. ls_pervalue-fix_val_per12    = wa_kp06-vlr_local.
        ENDCASE.
      ENDLOOP.

      APPEND ls_pervalue.


      ADD 1 TO lv_object.
      ADD 1 TO lv_value.


    ENDLOOP.



    CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
      EXPORTING
        headerinfo     = ls_headerinfo
      TABLES
        indexstructure = lt_indexstruc
        coobject       = lt_coobject
        totvalue       = lt_totvalue
        pervalue       = ls_pervalue
        return         = lt_return.

    APPEND LINES OF lt_return TO  lt_return_auxi.


    IF lt_return IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      WAIT UP TO 5 SECONDS.

    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    ENDIF.

    CLEAR: lt_indexstruc[], lt_coobject[],lt_totvalue[], lt_return[], ls_pervalue[].

  ENDLOOP.


  IF lt_return_auxi[] IS NOT INITIAL.

    SORT lt_return_auxi BY message.
    DELETE ADJACENT DUPLICATES FROM lt_return_auxi COMPARING message.

    PERFORM alv_popup_erro TABLES lt_return_auxi[].
  ELSE.
    MESSAGE 'Importado com sucesso.' TYPE 'I'.
    LEAVE TO SCREEN 0.


  ENDIF.

ENDFORM.

FORM f_kp06_lan_version.
  DATA it_zim_kp06_sol_orc  TYPE TABLE OF zim_kp06_sol_orc.
  DATA wa_zim_kp06_sol_orc TYPE zim_kp06_sol_orc.
  DATA wa_kp6_item TYPE ty_kp06.

  CLEAR : lv_object, lv_value, ls_headerinfo.
  REFRESH : lt_indexstruc, lt_coobject.

  IF it_kp06[] IS NOT INITIAL.
    DATA(it_kp06_orc) = VALUE t_kp06( FOR wa_it_zim_kp06_sol_orc IN it_kp06[]
      (
                ano               = wa_it_zim_kp06_sol_orc-ano
                mes               = wa_it_zim_kp06_sol_orc-mes
                codigo_cenario    = wa_it_zim_kp06_sol_orc-codigo_cenario
                nome_cenario      = wa_it_zim_kp06_sol_orc-nome_cenario
                centro_custo      = wa_it_zim_kp06_sol_orc-centro_custo
                nome_centro       = wa_it_zim_kp06_sol_orc-nome_centro
                cod_conta         = wa_it_zim_kp06_sol_orc-cod_conta
                nome_conta        = wa_it_zim_kp06_sol_orc-nome_conta
                vlr_local         = wa_it_zim_kp06_sol_orc-vlr_local
                vlr_dolar         = wa_it_zim_kp06_sol_orc-vlr_dolar
                ds_area           = wa_it_zim_kp06_sol_orc-ds_area

       ) ).
    it_kp06_aux_v[] = it_kp06_orc[].
    it_kp06_area_v[] = it_kp06_orc[].

    SORT it_kp06_area_v BY  ds_area.
    DELETE ADJACENT DUPLICATES FROM it_kp06_area_v COMPARING ds_area.

    SORT it_kp06_aux_v BY centro_custo cod_conta.
    DELETE ADJACENT DUPLICATES FROM it_kp06_aux_v COMPARING centro_custo cod_conta.
    SORT it_kp06_orc BY centro_custo cod_conta mes.

    LOOP AT it_kp06_area_v INTO DATA(wa_kp06_area_v).

      lv_object = 1.
      lv_value =  1.

      LOOP AT it_kp06_aux_v INTO DATA(wa_kp06_aux_v) WHERE ds_area = wa_kp06_area_v-ds_area.

        ls_headerinfo-co_area       = wa_kp06_aux_v-ds_area. "P_KOKRS.
        ls_headerinfo-fisc_year     = wa_kp06_aux_v-ano . "P_ANO.
        ls_headerinfo-period_from   = '001'.
        ls_headerinfo-period_to     = '012'.
        ls_headerinfo-version       = |{ p_versio ALPHA = OUT }|.
        ls_headerinfo-plan_currtype = 'T'.
        ls_headerinfo-doc_hdr_tx    = 'Origem Sysphera'.


        lt_indexstruc-object_index = lv_object.
        lt_indexstruc-value_index  = lv_value.
        APPEND lt_indexstruc.


        lt_coobject-object_index = lv_object.
        lt_coobject-costcenter = wa_kp06_aux_v-centro_custo.

        APPEND lt_coobject.


        CLEAR ls_pervalue.
        ls_pervalue-value_index = lv_object.
        ls_pervalue-cost_elem =  wa_kp06_aux_v-cod_conta.
        ls_pervalue-trans_curr = 'BRL'.
        ls_pervalue-fix_val_per01    = '0'.
        ls_pervalue-fix_val_per02    = '0' .
        ls_pervalue-fix_val_per03    = '0'.
        ls_pervalue-fix_val_per04    = '0'.
        ls_pervalue-fix_val_per05    = '0'.
        ls_pervalue-fix_val_per06    = '0'.
        ls_pervalue-fix_val_per07    = '0'.
        ls_pervalue-fix_val_per08    = '0'.
        ls_pervalue-fix_val_per09    = '0'.
        ls_pervalue-fix_val_per10    = '0'.
        ls_pervalue-fix_val_per11    = '0'.
        ls_pervalue-fix_val_per12    = '0'.
        APPEND ls_pervalue.

        ADD 1 TO lv_object.
        ADD 1 TO lv_value.


      ENDLOOP.

      CALL FUNCTION 'BAPI_COSTACTPLN_POSTPRIMCOST'
        EXPORTING
          headerinfo     = ls_headerinfo
        TABLES
          indexstructure = lt_indexstruc
          coobject       = lt_coobject
          totvalue       = lt_totvalue
          pervalue       = ls_pervalue
          return         = lt_return.


      APPEND LINES OF lt_return TO  lt_return_auxi.


      IF lt_return[] IS INITIAL.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*        WAIT UP TO 5 SECONDS.
        WAIT UP TO 60 SECONDS. "IR102086-BAPI-BP06 tempo 60s-ALRS

      ELSE.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      CLEAR: lt_indexstruc[], lt_coobject[],lt_totvalue[], lt_return[], ls_pervalue[].

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM alv_popup_erro TABLES it_retunr_erro.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      "IT_FIELDCAT           = ESTRUTURA[]
      i_structure_name      = 'BAPIRET2'
      i_save                = 'A'
      i_screen_start_column = 3
      i_screen_start_line   = 3
      i_screen_end_column   = 100
      i_screen_end_line     = 13
    TABLES
      t_outtab              = it_retunr_erro.


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VALIDAR_OBRIGATORIO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IS_VALID  text
*----------------------------------------------------------------------*
FORM f_validar_obrigatorio  CHANGING p_is_valid.

  p_is_valid = ''.

*-CS2022000122 - 07.03.2022 - JT - inicio
  IF r_mapa  = abap_true.
    IF p_ano IS INITIAL.
      MESSAGE 'Campo Ano é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ELSEIF p_mes IS INITIAL.
      MESSAGE 'Campo Mês é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ELSEIF p_kokrs IS INITIAL.
      MESSAGE 'Campo Área de contabilidade de custos é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ELSEIF s_aufnr[] IS INITIAL.
      MESSAGE 'Campo ordens é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ENDIF.
  ELSEIF r_gl056 = abap_true.
    IF p_bukrs IS INITIAL.
      MESSAGE 'Informe a empresa.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ELSEIF p_ano IS INITIAL.
      MESSAGE 'Campo Ano é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ELSEIF p_mes IS INITIAL.
      MESSAGE 'Campo Mês é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ENDIF.
  ELSE.
*-CS2022000122 - 07.03.2022 - JT - fim

    IF s_posnr[] IS NOT INITIAL.
      IF s_perio[] IS INITIAL.
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 04  INICIO - BG
        MESSAGE 'Informe o Período  KOB1/2 para a solicitção' TYPE 'S' DISPLAY LIKE 'E'.
*CS2022000117 - Ajustes na transação ZIM15  US 72901 - ajuste 04  FIM - BG
        p_is_valid = '1'.
      ENDIF.
      "
      IF p_kokrs IS INITIAL.
        MESSAGE 'Campo Área de contabilidade de custos é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
        p_is_valid = '1'.
      ENDIF.

      IF p_bukrs IS INITIAL.
        MESSAGE 'Informe a empresa.' TYPE 'S' DISPLAY LIKE 'E'.
        p_is_valid = '1'.
      ENDIF.
    ELSEIF p_ano IS INITIAL.
      MESSAGE 'Campo Ano é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ELSEIF p_mes IS INITIAL.
      MESSAGE 'Campo Mês é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.
    ELSEIF p_kokrs IS INITIAL.
      MESSAGE 'Campo Área de contabilidade de custos é Obrigatório.' TYPE 'S' DISPLAY LIKE 'E'.
      p_is_valid = '1'.

    ENDIF.
*-CS2022000122 - 07.03.2022 - JT - inicio
  ENDIF.
*-CS2022000122 - 07.03.2022 - JT - fim

  IF p_is_valid IS INITIAL AND s_posnr[] IS INITIAL.
    vdatai = |{ p_ano }{ p_mes }01|. "VERIFICAR
    CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
      EXPORTING
        day_in            = vdatai
      IMPORTING
        last_day_of_month = vdataf.
  ENDIF.


ENDFORM.

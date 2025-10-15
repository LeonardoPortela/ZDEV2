*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_PBO_0160
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  PBO_0160  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pbo_0160 output.
  refresh: gt_header, gt_fcat_slis.

  case screen_item.

    when c_screen_0170.
      perform alv_preenche_cat_slis using:
      'BUKRS'       'Empresa'           '' '' '',
      'FILIAL'      'Filial'            '' '' '',
      'ANLN1'       'Imobilizado'       '' '' 'X',
      'ANLN2'       'Sub Nr'            '' '' 'X',
      'DT_INCOR'    'Data Incor'        '' '' '',
      'DT_DESAT'    'Data Desat'        '' '' '',
      'KFZKZ'       'Placa Veículo'     '' '' '',
      'CENTRO'      'Centro Custo'      '' '' '',
      'NR_CHASSI'   'Nr. Chassi'        '' '' '',
      'TXT_PRINC'   'Descrição'         '' '' '',
      'PAIS'        'País'              '' '' '',
      'REGIAO'      'Região'            '' '' '',
      'ANO_FABR'    'Ano Fab'           '' '' '',
      'ANO_MOD'     'Ano Mod'           '' '' '',
      'POTENCIA'    'Potência'          '' '' '',
      'COR'         'Cor'               '' '' '',
      'PG_ARQ'      'Pg Arq'            '' '' '',
      'RESP_VEIC'   'Resp Veíc'         '' '' '',
      'COD_RENAVAN' 'Cod Renavam'       '' '' '',
      'MES_IPVA'    'Mês Ipva'          '' '' '',
      'MES_LICENC'  'Mês Licenc'        '' '' '',
      'MES_DPVAT'   'Mês Dpvat'         '' '' '',
      'DUT'         'DUT'               '' '' '',
      'ALIENACAO'   'Alienação'         '' '' '',
      'PORTE_OBRIG' 'Porte Obrigatório' '' '' '',
      'DT_CRIACAO'  'Dt Criação'        '' '' '',
      'HR_CRIACAO'  'Hr Criação'        '' '' '',
      'USER_CRIAC'  'Usuário'           '' '' '',
      'DT_MODIF'    'Dt Modific'        '' '' '',
      'USER_MODIF'  'Usuário'           '' '' ''.

    when c_screen_0180.
      perform alv_preenche_cat_slis using:
      'BUKRS'       'Empresa'       '' '' '',
      'ANLN1'       'Imobilizado'   '' '' 'X',
      'ANLN2'       'Sub Nr'        '' '' 'X',
      'KFZKZ'       'Placa Veículo' '' '' '',
      'WERKS'       'Filial'        '' '' '',
      'TP_OBRIG'    'Tp obrig'      '' '' '',
      'ANO_VCTO'    'Ano Vcto'      '' '' '',
      'MES_VCTO'    'Mês Vcto'      '' '' '',
      'WAERS'       'Moeda'         '' '' '',
      'VLR_PRINC'   'Vlr Principal' '' '' '',
      'VLR_CORRE'   'Vlr Corre'     '' '' '',
      'VLR_MULTA'   'Vlr Multa'     '' '' '',
      'VLR_JUROS'   'Vlr Juros'     '' '' '',
      'VLR_TSE'     'Vlr TSE'       '' '' '',
      'VLR_TOTAL'   'Vlr Total'     '' '' '',
      'COD_BARRAS'  'Cod Barras'    '' '' '',
      'DT_VENC'     'Dt Venc'       '' '' '',
      'DEP_RESP'    'Dep Resp'      '' '' '',
      'ERDAT'       'Dt Criação'    '' '' '',
      'ERNAM'       'Responsável'   '' '' '',
      'LOTE'        'Lote'          '' '' '',
      'COD_IMPOSTO' 'Cod Imposto'   '' '' '',
      'DOC_IMPOSTO' 'Doc Imposto'   '' '' 'X'.
  endcase.

  wl_layout_slis-colwidth_optimize = 'X'.
  wl_layout_slis-zebra             = 'X'.

endmodule.                 " PBO_0160  OUTPUT

*----------------------------------------------------------------------*
***INCLUDE MZREMZARM_ROT.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM MONTAR_LAYOUT .
  REFRESH TG_FIELDCATALOG.
  PERFORM F_MONTAR_ESTRUTURA USING:
        1 ' '                ' '              'IT_ROMANEIO'  'ICONE'         ' '              '05' ' ' ' ' ' ' ' ' ' ',
        2 'ZSDT0001'         'VBELN'          'IT_ROMANEIO'  'VBELN'         'N°_Documento'   '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'DOC_MATERIAL'   'IT_ROMANEIO'  'DOC_MATERIAL'  'Doc._Material'  '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'ANO_MATERIAL'   'IT_ROMANEIO'  'ANO_MATERIAL'  'Ano'            '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'NR_ROMANEIO'    'IT_ROMANEIO'  'NR_ROMANEIO'   'Nr._Romaneio'   '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'DT_MOVIMENTO'   'IT_ROMANEIO'  'DT_MOVIMENTO'  'Dt._Mov.'       '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'NR_SAFRA'       'IT_ROMANEIO'  'NR_SAFRA'      'Safra'          '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'BUKRS'          'IT_ROMANEIO'  'BUKRS'         'Empresa'        '08' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'BRANCH'         'IT_ROMANEIO'  'BRANCH'        'Centro'         '08' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'PARID'          'IT_ROMANEIO'  'PARID'         'Cliente'        '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'ID_CLI_DEST'    'IT_ROMANEIO'  'ID_CLI_DEST'   'Destino'        '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'TP_FRETE'       'IT_ROMANEIO'  'TP_FRETE'      'Tp_Frete'       '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'MATNR'          'IT_ROMANEIO'  'MATNR'         'Material'       '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'PESO_LIQ'       'IT_ROMANEIO'  'PESO_LIQ'      'Peso_Liq.'      '15' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'PESO_FISCAL'    'IT_ROMANEIO'  'PESO_FISCAL'   'Peso_Fiscal'    '15' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'PLACA_CAV'      'IT_ROMANEIO'  'PLACA_CAV'     'Placa_Carreta'  '08' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'PLACA_CAR1'     'IT_ROMANEIO'  'PLACA_CAR1'    'Placa_Car.'     '08' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'PLACA_CAR2'     'IT_ROMANEIO'  'PLACA_CAR2'    'Placa_Car2'     '08' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'NR_TICKET'      'IT_ROMANEIO'  'NR_TICKET'     'Nr_Ticket'      '08' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'DT_FECHAMENTO'  'IT_ROMANEIO'  'DT_FECHAMENTO' 'Dt._Fech.'      '10' ' ' ' ' ' ' ' ' ' ',

        3 'ZSDT0001'         'DT_FECHAMENTO'  'IT_ROMANEIO'  'DATA_MOD'       'Dt.modif'      '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         'HR_FECHAMENTO'  'IT_ROMANEIO'  'HORA_MOD'       'Hr.modif.'     '10' ' ' ' ' ' ' ' ' ' ',
        3 'ZSDT0001'         ' '              'IT_ROMANEIO'  'USU_MOD'        'Usuário'       '10' ' ' ' ' ' ' ' ' ' '.
ENDFORM.

FORM F_MONTAR_ESTRUTURA  USING  P_COL_POS     P_REF_TABNAME   P_REF_FIELDNAME
                                P_TABNAME     P_FIELD         P_SCRTEXT_L
                                P_OUTPUTLEN   P_EDIT          P_SUM
                                P_EMPHASIZE   P_F4            P_ICO.

  CLEAR WG_FIELDCATALOG.
  WG_FIELDCATALOG-FIELDNAME    = P_FIELD.
  WG_FIELDCATALOG-TABNAME      = P_TABNAME.
  WG_FIELDCATALOG-REF_TABLE    = P_REF_TABNAME.
  WG_FIELDCATALOG-REF_FIELD    = P_REF_FIELDNAME.
  WG_FIELDCATALOG-KEY          = ' '.
  WG_FIELDCATALOG-EDIT         = P_EDIT.
  WG_FIELDCATALOG-DO_SUM       = P_SUM.

  WG_FIELDCATALOG-COL_POS      = P_COL_POS.

  IF P_OUTPUTLEN IS NOT INITIAL.
    WG_FIELDCATALOG-OUTPUTLEN  = P_OUTPUTLEN.
  ENDIF.
  IF P_ICO IS NOT INITIAL.
    WG_FIELDCATALOG-HOTSPOT    = C_X.
    WG_FIELDCATALOG-ICON       = C_X.
  ENDIF.
  WG_FIELDCATALOG-NO_OUT       = ' '.
  WG_FIELDCATALOG-REPTEXT      = P_SCRTEXT_L.
  WG_FIELDCATALOG-SCRTEXT_S    = P_SCRTEXT_L.
  WG_FIELDCATALOG-SCRTEXT_M    = P_SCRTEXT_L.
  WG_FIELDCATALOG-SCRTEXT_L    = P_SCRTEXT_L.
  WG_FIELDCATALOG-EMPHASIZE    = P_EMPHASIZE.

  APPEND WG_FIELDCATALOG TO TG_FIELDCATALOG.
ENDFORM.                    " F_MONTAR_ESTRUTURA

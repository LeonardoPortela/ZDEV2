*&---------------------------------------------------------------------*
*&  Include           ZSDR0082_FORM
*&---------------------------------------------------------------------*


FORM F_REFRESH_ALV USING P_ALV.

  CASE P_ALV.
    WHEN '0100'.
      CALL METHOD OBJ_ALV_0100->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    WHEN '0110'.

  ENDCASE.

ENDFORM.

FORM F_REFRESH_OBJETOS .

  CLEAR: GS_LAYOUT,
         GS_VARIANT.

  REFRESH: IT_EXCLUDE_FCODE.

ENDFORM.

FORM F_CRIAR_CATALOG USING P_SCREEN.

  FREE: WA_FCAT, IT_FCAT.

  CASE P_SCREEN.
    WHEN '0100'.

      PERFORM F_ESTRUTURA_ALV USING:

       01  'ZSDT0155'      'BUKRS'          'IT_SAIDA_0100'  'BUKRS'                     'Empresa'            '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0155'      'AUART'          'IT_SAIDA_0100'  'AUART'                     'Tp.OV'              '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0155'      'LANC_ZNFW'      'IT_SAIDA_0100'  'LANC_ZNFW'                 'Lanc.ZNFW'          '12'   ' '    ''  ' ' ' ' ' ' ' ' 'X',
       03  ''              ''               'IT_SAIDA_0100'  'DS_TRIB'                   'Tributo'            '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZSDT0155'      'DT_INI'         'IT_SAIDA_0100'  'DT_INI'                    'Dt.Val.Ini.'        '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZSDT0155'      'DT_FIM'         'IT_SAIDA_0100'  'DT_FIM'                    'Dt.Val.Fim'         '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'ZSDT0155'      'MATKL'          'IT_SAIDA_0100'  'MATKL'                     'Grp.Merc.'          '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       07  'ZSDT0155'      'MATNR'          'IT_SAIDA_0100'  'MATNR'                     'Material'           '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       08  'ZSDT0155'      'VLR_UPF'        'IT_SAIDA_0100'  'VLR_UPF'                   'Vlr.UPF'            '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       09  'ZSDT0155'      'PERC_UPF'       'IT_SAIDA_0100'  'PERC_UPF'                  'Perc.UPF'           '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'ZSDT0155'      'HKONT_DEB'      'IT_SAIDA_0100'  'HKONT_DEB'                 'Conta.Deb.'         '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       11  'ZSDT0155'      'HKONT_CRED'     'IT_SAIDA_0100'  'HKONT_CRED'                'Conta.Cred.'        '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       13  'ZSDT0155'      'REGION'         'IT_SAIDA_0100'  'REGION'                    'UF Tribut.'         '10'   ' '    ''  ' ' 'C' ' ' ' ' '',
       14  'ZSDT0155'      'CALC_VLR_PER'   'IT_SAIDA_0100'  'CALC_VLR_PER'              'Calc. Vlr. %'       '12'   ' '    ''  ' ' ' ' ' ' ' ' 'X',
       15  'ZSDT0155'      'CANCELADO'      'IT_SAIDA_0100'  'CANCELADO'                 'Cancelado'          '08'   ' '    ''  ' ' 'C' ' ' ' ' 'X' ,
       16  'ZSDT0155'      'DT_REGISTRO'    'IT_SAIDA_0100'  'DT_REGISTRO'               'Dt.Registro'        '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       17  'ZSDT0155'      'HR_REGISTRO'    'IT_SAIDA_0100'  'HR_REGISTRO'               'Hr.Registro'        '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       18  'ZSDT0155'      'US_REGISTRO'    'IT_SAIDA_0100'  'US_REGISTRO'               'Us.Registro'        '12'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0110'.


  ENDCASE.


ENDFORM.

FORM F_ESTRUTURA_ALV USING VALUE(P_COL_POS)       TYPE I
                           VALUE(P_REF_TABNAME)   LIKE DD02D-TABNAME
                           VALUE(P_REF_FIELDNAME) LIKE DD03D-FIELDNAME
                           VALUE(P_TABNAME)       LIKE DD02D-TABNAME
                           VALUE(P_FIELD)         LIKE DD03D-FIELDNAME
                           VALUE(P_SCRTEXT_L)     LIKE DD03P-SCRTEXT_L
                           VALUE(P_OUTPUTLEN)
                           VALUE(P_EDIT)
                           VALUE(P_SUM)
                           VALUE(P_EMPHASIZE)
                           VALUE(P_JUST)
                           VALUE(P_HOTSPOT)
                           VALUE(P_F4)
                           VALUE(P_CHECK).

  CLEAR WA_FCAT.

  WA_FCAT-FIELDNAME   = P_FIELD.
  WA_FCAT-TABNAME     = P_TABNAME.
  WA_FCAT-REF_TABLE   = P_REF_TABNAME.
  WA_FCAT-REF_FIELD   = P_REF_FIELDNAME.
  WA_FCAT-KEY         = ' '.
  WA_FCAT-EDIT        = P_EDIT.
  WA_FCAT-COL_POS     = P_COL_POS.
  WA_FCAT-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FCAT-NO_OUT      = ' '.
  WA_FCAT-DO_SUM      = P_SUM.
  WA_FCAT-REPTEXT     = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_S   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_M   = P_SCRTEXT_L.
  WA_FCAT-SCRTEXT_L   = P_SCRTEXT_L.
  WA_FCAT-EMPHASIZE   = P_EMPHASIZE.
  WA_FCAT-STYLE       =
  WA_FCAT-JUST        = P_JUST.
  WA_FCAT-HOTSPOT     = P_HOTSPOT.
  WA_FCAT-F4AVAILABL  = P_F4.
  WA_FCAT-CHECKBOX    = P_CHECK.

  APPEND WA_FCAT TO IT_FCAT.

ENDFORM.                    " ESTRUTURA_ALV

FORM F_EXCLUDE_FCODE USING P_SCREEN.

  APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO IT_EXCLUDE_FCODE.
  APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK             TO IT_EXCLUDE_FCODE.

ENDFORM.

FORM F_LIMPA_VARIAVEIS .

  CLEAR: WA_SAIDA_0100,
         IT_SAIDA_0100[],
         TG_0155[],
         TG_DD07T[].

ENDFORM.

FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPA_VARIAVEIS.

  SELECT *
    FROM ZSDT0155 INTO TABLE TG_0155.

  CHECK TG_0155[] IS NOT INITIAL.

  SELECT *
    FROM DD07T INTO TABLE TG_DD07T
   WHERE DOMNAME    = 'ZTP_TRIB_ESTADO'
     AND DDLANGUAGE = SY-LANGU.


ENDFORM.

FORM F_PROCESSA_DADOS .

  LOOP AT TG_0155.

    CLEAR: WA_SAIDA_0100.

    MOVE-CORRESPONDING TG_0155 TO WA_SAIDA_0100.

    READ TABLE TG_DD07T WITH KEY DOMVALUE_L = TG_0155-TP_TRIB.
    IF SY-SUBRC = 0.
      CONCATENATE TG_DD07T-DOMVALUE_L '-' TG_DD07T-DDTEXT
             INTO WA_SAIDA_0100-DS_TRIB SEPARATED BY SPACE.
    ENDIF.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.

  SORT IT_SAIDA_0100 BY BUKRS.

ENDFORM.

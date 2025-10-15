*&---------------------------------------------------------------------*
*&  Include           ZLESR0112_FORM
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

       01  'ZLEST0149'      'BUKRS'              'IT_SAIDA_0100'  'BUKRS'                     'Empresa Rom.'   '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZLEST0149'      'BRANCH'             'IT_SAIDA_0100'  'BRANCH'                    'Filial Rom.'    '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       03  'ZLEST0149'      'BUKRS'              'IT_SAIDA_0100'  'BUKRS_RA'                  'Empresa R.A.'   '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZLEST0149'      'BRANCH'             'IT_SAIDA_0100'  'BRANCH_RA'                 'Filial R.A.'    '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZLEST0149'      'LOCAL_CODIGO_URF'   'IT_SAIDA_0100'  'LOCAL_CODIGO_URF'          'Cód. URF'       '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'ZLEST0149'      'LOCAL_CODIGO_RA'    'IT_SAIDA_0100'  'LOCAL_CODIGO_RA'           'Cód. RA'        '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       07  'ZLEST0149'      'LOCAL_LATITUDE'     'IT_SAIDA_0100'  'LOCAL_LATITUDE'            'Latitude'       '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       08  'ZLEST0149'      'LOCAL_LONGITUDE'    'IT_SAIDA_0100'  'LOCAL_LONGITUDE'           'Longitude'      '09'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'ZLEST0149'      'DT_INI_ROM'         'IT_SAIDA_0100'  'DT_INI_ROM'                'Dt.Ini.Rom'     '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       11  'ZLEST0149'      'DT_REGISTRO'        'IT_SAIDA_0100'  'DT_REGISTRO'               'Dt.Registro'    '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       12  'ZLEST0149'      'HR_REGISTRO'        'IT_SAIDA_0100'  'HR_REGISTRO'               'Hr.Registro'    '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       13  'ZLEST0149'      'US_REGISTRO'        'IT_SAIDA_0100'  'US_REGISTRO'               'Us.Registro'    '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0120'.

      PERFORM F_ESTRUTURA_ALV USING:

       01  'LFA1'           'LIFNR'              'IT_SAIDA_0120'  'LIFNR'                     'Fornecedor'     '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       02  'LFA1'           'NAME1'              'IT_SAIDA_0120'  'NAME1'                     'Nome'           '40'   ' '    ''  ' ' ' ' ' ' ' ' '' .

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

  CHECK P_SCREEN <> '0120'.

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
         TG_0149[].

ENDFORM.

FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPA_VARIAVEIS.

  SELECT *
    FROM ZLEST0149 INTO TABLE TG_0149.

  CHECK TG_0149[] IS NOT INITIAL.

ENDFORM.

FORM F_PROCESSA_DADOS .

  LOOP AT TG_0149.

    CLEAR: WA_SAIDA_0100.

    MOVE-CORRESPONDING TG_0149 TO WA_SAIDA_0100.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.

ENDFORM.

FORM F_CALL_SCREEN_0120.

  DATA: WL_LFA1 TYPE LFA1.

  CLEAR: IT_SAIDA_0120[].

  SELECT *
    FROM ZLEST0161 INTO TABLE TG_0161.

  LOOP AT TG_0161.
    WA_SAIDA_0120-LIFNR = TG_0161-LIFNR.

    SELECT SINGLE * FROM LFA1 INTO WL_LFA1 WHERE LIFNR = WA_SAIDA_0120-LIFNR.

    IF SY-SUBRC = 0.
      WA_SAIDA_0120-NAME1 = WL_LFA1-NAME1.
    ENDIF.

    APPEND WA_SAIDA_0120 TO IT_SAIDA_0120.
  ENDLOOP.

  CALL SCREEN 0120 STARTING AT 04 04 ENDING AT 80 20.

ENDFORM.

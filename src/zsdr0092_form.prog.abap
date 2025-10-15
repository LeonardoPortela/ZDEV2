*&---------------------------------------------------------------------*
*&  Include           ZSDR0092_FORM
*&---------------------------------------------------------------------*


FORM F_REFRESH_ALV USING P_ALV.

  CASE P_ALV.
    WHEN '0100'.
      CALL METHOD OBJ_ALV_0100->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    WHEN '0120'.
      CALL METHOD OBJ_ALV_0120->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    WHEN '0130'.
      CALL METHOD OBJ_ALV_0130->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
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

       01  'ZSDT0169'      'CODIGO_RA'          'IT_SAIDA_0100'  'CODIGO_RA'           'Cod.RA'                                 '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0168'      'DS_RA'              'IT_SAIDA_0100'  'DS_RA'               'Ds.Recinto Alfandegado'                 '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       03  'ZSDT0169'      'CODIGO_URF'         'IT_SAIDA_0100'  'CODIGO_URF'          'Codigo URF'                             '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZSDT0167'      'DS_URF'             'IT_SAIDA_0100'  'DS_URF'              'Ds.Unidade Receita Federal'             '30'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZSDT0168'      'LOCAL_DESPACHO'     'IT_SAIDA_0100'  'LOCAL_DESPACHO'      'Loc.Despacho'                           '12'   ' '    ''  ' ' ' ' ' ' ' ' 'X',
       06  'ZSDT0168'      'LOCAL_EMBARQUE'     'IT_SAIDA_0100'  'LOCAL_EMBARQUE'      'Loc.Embarque'                           '12'   ' '    ''  ' ' ' ' ' ' ' ' 'X',
       06  'ZSDT0168'      'LIFNR'              'IT_SAIDA_0100'  'LIFNR'               'Fornecedor'                             '12'   ' '    ''  ' ' ' ' ' ' ' ' ' ',
       07  'ZSDT0169'      'DT_REGISTRO'        'IT_SAIDA_0100'  'DT_REGISTRO'         'Dt.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       08  'ZSDT0169'      'HR_REGISTRO'        'IT_SAIDA_0100'  'HR_REGISTRO'         'Hr.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       09  'ZSDT0169'      'US_REGISTRO'        'IT_SAIDA_0100'  'US_REGISTRO'         'Us.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0120'.

      PERFORM F_ESTRUTURA_ALV USING:

       01  'ZSDT0167'      'CODIGO_URF'         'IT_SAIDA_0120'  'CODIGO_URF'          'Codigo'                                 '10'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0167'      'DS_URF'             'IT_SAIDA_0120'  'DS_URF'              'Descrição'                              '35'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZSDT0167'      'DS_URF_ABREV'       'IT_SAIDA_0120'  'DS_URF_ABREV'        'Descrição Abrev.'                       '20'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       03  'ZSDT0167'      'DT_REGISTRO'        'IT_SAIDA_0120'  'DT_REGISTRO'         'Dt.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZSDT0167'      'HR_REGISTRO'        'IT_SAIDA_0120'  'HR_REGISTRO'         'Hr.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZSDT0167'      'US_REGISTRO'        'IT_SAIDA_0120'  'US_REGISTRO'         'Us.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .


    WHEN '0130'.

      PERFORM F_ESTRUTURA_ALV USING:

       01  'ZSDT0168'      'CODIGO_RA'          'IT_SAIDA_0130'   'CODIGO_RA'          'Código'                                 '10'   'X'    ''  ' ' ' ' ' ' ' ' ' ' ,
       02  'ZSDT0168'      'DS_RA'              'IT_SAIDA_0130'   'DS_RA'              'Descrição'                              '35'   'X'    ''  ' ' ' ' ' ' ' ' ' ' ,
       03  'ZSDT0168'      'LOCAL_DESPACHO'     'IT_SAIDA_0130'   'LOCAL_DESPACHO'     'Loc.Despacho'                           '12'   'X'    ''  ' ' ' ' ' ' ' ' 'X' ,
       04  'ZSDT0168'      'LOCAL_EMBARQUE'     'IT_SAIDA_0130'   'LOCAL_EMBARQUE'     'Loc.Embarque'                           '12'   'X'    ''  ' ' ' ' ' ' ' ' 'X' ,
       04  'ZSDT0168'      'LIFNR'              'IT_SAIDA_0130'   'LIFNR'              'Fornecedor'                             '12'   'X'    ''  ' ' ' ' ' ' ' ' ' ' ,
       05  'ZSDT0168'      'DT_REGISTRO'        'IT_SAIDA_0130'   'DT_REGISTRO'        'Dt.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' ' ' ,
       06  'ZSDT0168'      'HR_REGISTRO'        'IT_SAIDA_0130'   'HR_REGISTRO'        'Hr.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' ' ' ,
       07  'ZSDT0168'      'US_REGISTRO'        'IT_SAIDA_0130'   'US_REGISTRO'        'Us.Registro'                            '11'   ' '    ''  ' ' ' ' ' ' ' ' ' ' .


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

  CASE P_SCREEN.
    WHEN '0100' OR '0120' OR '0130'.
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
    WHEN '0130'.
  ENDCASE.


ENDFORM.

FORM F_LIMPA_VARIAVEIS .

  CLEAR: WA_SAIDA_0100,
         IT_SAIDA_0100[],
         TG_0167[],
         TG_0168[],
         TG_0169[].

ENDFORM.

FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPA_VARIAVEIS.

  SELECT *
    FROM ZSDT0169 INTO TABLE TG_0169.

  CHECK TG_0169[] IS NOT INITIAL.

  SELECT *
    FROM ZSDT0167 INTO TABLE TG_0167
     FOR ALL ENTRIES IN TG_0169
   WHERE CODIGO_URF = TG_0169-CODIGO_URF.

  SELECT *
    FROM ZSDT0168 INTO TABLE TG_0168
     FOR ALL ENTRIES IN TG_0169
   WHERE CODIGO_RA = TG_0169-CODIGO_RA.


ENDFORM.

FORM F_PROCESSA_DADOS .

  LOOP AT TG_0169.

    CLEAR: WA_SAIDA_0100.

    MOVE-CORRESPONDING TG_0169 TO WA_SAIDA_0100.

    READ TABLE TG_0167 WITH KEY CODIGO_URF = TG_0169-CODIGO_URF.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100-DS_URF = TG_0167-DS_URF.
    ENDIF.

    READ TABLE TG_0168 WITH KEY CODIGO_RA = TG_0169-CODIGO_RA.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100-DS_RA = TG_0168-DS_RA.
      WA_SAIDA_0100-LOCAL_DESPACHO = TG_0168-LOCAL_DESPACHO.
      WA_SAIDA_0100-LOCAL_EMBARQUE = TG_0168-LOCAL_EMBARQUE.
      WA_SAIDA_0100-LIFNR          = TG_0168-LIFNR.
    ENDIF.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.

ENDFORM.

FORM F_CALL_SCREEN_0120.

  PERFORM F_SELECIONAR_DADOS_0120.

  CALL SCREEN 0120 STARTING AT 04 04 ENDING AT 120 20.

ENDFORM.

FORM F_SELECIONAR_DADOS_0120.

  CLEAR: IT_SAIDA_0120[].

  SELECT *
    FROM ZSDT0167 INTO TABLE TG_0167.

  LOOP AT TG_0167.
    CLEAR: WA_SAIDA_0120.

    MOVE-CORRESPONDING TG_0167 TO WA_SAIDA_0120.

    "Chave Primaria
    CLEAR: WL_ESTILO.
    WL_ESTILO-FIELDNAME = 'CODIGO_URF'.
    WL_ESTILO-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    APPEND WL_ESTILO    TO WA_SAIDA_0120-ESTILO.

    APPEND WA_SAIDA_0120 TO IT_SAIDA_0120.
  ENDLOOP.

ENDFORM.

FORM F_SELECIONAR_DADOS_0130.

  CLEAR: IT_SAIDA_0130[].

  SELECT *
    FROM ZSDT0168 INTO TABLE TG_0168.

  LOOP AT TG_0168.
    CLEAR: WA_SAIDA_0130.

    MOVE-CORRESPONDING TG_0168 TO WA_SAIDA_0130.

    "Chave Primaria
    CLEAR: WL_ESTILO.
    WL_ESTILO-FIELDNAME = 'CODIGO_RA'.
    WL_ESTILO-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    APPEND WL_ESTILO    TO WA_SAIDA_0130-ESTILO.

    APPEND WA_SAIDA_0130 TO IT_SAIDA_0130.
  ENDLOOP.

ENDFORM.

FORM F_CALL_SCREEN_0130.

  PERFORM F_SELECIONAR_DADOS_0130.

  CALL SCREEN 0130 STARTING AT 04 04 ENDING AT 120 20.

ENDFORM.

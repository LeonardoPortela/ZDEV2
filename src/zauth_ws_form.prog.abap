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

       01  'ZAUTH_WEBSERVICE'      'SERVICE'   'IT_SAIDA_0100'  'SERVICE'           'Nome do Serviço'     '45'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZAUTH_WEBSERVICE'      'URL'       'IT_SAIDA_0100'  'URL'               'Endereço URL'        '90'   ' '    ''  ' ' ' ' ' ' ' ' '' .

    WHEN '0120'.

      PERFORM F_ESTRUTURA_ALV USING:

       01  'ZAUTH_WS_0001'      'BUKRS'             'IT_SAIDA_0120'  'BUKRS'               'Empresa'                              '07'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZAUTH_WS_0001'      'BRANCH'            'IT_SAIDA_0120'  'BRANCH'              'Filial'                               '06'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZAUTH_WS_0001'      'SSL_ID'            'IT_SAIDA_0120'  'SSL_ID'              'SSL.Id.'                              '08'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       03  'ZAUTH_WS_0001'      'USERNAME'          'IT_SAIDA_0120'  'USERNAME'            'Usuário'                              '15'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       03  'ZAUTH_WS_0001'      'PASSWORD'          'IT_SAIDA_0120'  'PASSWORD'            'Senha'                                '20'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZAUTH_WS_0001'      'ADD01'             'IT_SAIDA_0120'  'ADD01'               'Inf.Add 1'                            '70'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZAUTH_WS_0001'      'ADD02'             'IT_SAIDA_0120'  'ADD02'               'Inf.Add 2'                            '70'   'X'    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZAUTH_WS_0001'      'DT_REGISTRO'       'IT_SAIDA_0120'  'DT_REGISTRO'         'Dt.Registro'                          '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZAUTH_WS_0001'      'HR_REGISTRO'       'IT_SAIDA_0120'  'HR_REGISTRO'         'Hr.Registro'                          '11'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'ZAUTH_WS_0001'      'US_REGISTRO'       'IT_SAIDA_0120'  'US_REGISTRO'         'Us.Registro'                          '11'   ' '    ''  ' ' ' ' ' ' ' ' '' .

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

  IF P_FIELD EQ  'PASSWORD' .
    WA_FCAT-EDIT_MASK = '*************'.
  ENDIF.

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
         TG_ZAUTH_WEBSERVICE[],
         TG_ZAUTH_WS_0001[].

ENDFORM.

FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPA_VARIAVEIS.

  SELECT *
    FROM ZAUTH_WEBSERVICE INTO TABLE TG_ZAUTH_WEBSERVICE.

  CHECK TG_ZAUTH_WEBSERVICE[] IS NOT INITIAL.

ENDFORM.

FORM F_PROCESSA_DADOS .

  LOOP AT TG_ZAUTH_WEBSERVICE.

    CLEAR: WA_SAIDA_0100.

    MOVE-CORRESPONDING TG_ZAUTH_WEBSERVICE TO WA_SAIDA_0100.

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
    FROM ZAUTH_WS_0001 INTO TABLE TG_ZAUTH_WS_0001
   WHERE SERVICE = WA_SAIDA_0100-SERVICE.


  LOOP AT TG_ZAUTH_WS_0001.
    CLEAR: WA_SAIDA_0120.

    MOVE-CORRESPONDING TG_ZAUTH_WS_0001 TO WA_SAIDA_0120.

    "Chave Primaria
    CLEAR: WL_ESTILO.
    WL_ESTILO-FIELDNAME = 'BRANCH'.
    WL_ESTILO-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    APPEND WL_ESTILO    TO WA_SAIDA_0120-ESTILO.

    CLEAR: WL_ESTILO.
    WL_ESTILO-FIELDNAME = 'BUKRS'.
    WL_ESTILO-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    APPEND WL_ESTILO    TO WA_SAIDA_0120-ESTILO.

    CLEAR: WL_ESTILO.
    WL_ESTILO-FIELDNAME = 'SERVICE'.
    WL_ESTILO-STYLE     = CL_GUI_ALV_GRID=>MC_STYLE_DISABLED.
    APPEND WL_ESTILO    TO WA_SAIDA_0120-ESTILO.


    APPEND WA_SAIDA_0120 TO IT_SAIDA_0120.
  ENDLOOP.

ENDFORM.

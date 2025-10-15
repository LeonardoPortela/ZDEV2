*&---------------------------------------------------------------------*
*&  Include           ZMMR123_FORM
*&---------------------------------------------------------------------*

FORM F_REFRESH_ALV USING P_ALV.

  CASE P_ALV.
    WHEN '0100'.
      CALL METHOD OBJ_ALV_0100->REFRESH_TABLE_DISPLAY
        EXPORTING
          IS_STABLE = WA_STABLE.
    WHEN '0110'.
      CALL METHOD OBJ_ALV_0110->REFRESH_TABLE_DISPLAY
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

       01  'ZMMT0086'      'BUKRS'             'IT_SAIDA_0100' 'BUKRS'          'Empresa'        '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZMMT0086'      'EBELN'             'IT_SAIDA_0100' 'EBELN'          'Pedido'         '10'   ' '    ''  ' ' ' ' 'X' ' ' '' ,
       03  'ZMMT0086'      'LIFNR'             'IT_SAIDA_0100' 'LIFNR'          'Fornecedor'     '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       04  'ZMMT0086'      'EBELP'             'IT_SAIDA_0100' 'EBELP'          'Item'           '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZMMT0086'      'MATNR'             'IT_SAIDA_0100' 'MATNR'          'Material'       '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'MAKT'          'MAKTX'             'IT_SAIDA_0100' 'MAKTX'          'Desc.Material'  '25'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       07  'ZMMT0086'      'MENGE'             'IT_SAIDA_0100' 'MENGE'          'Qtd.Pedido'     '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       08  'ZMMT0086'      'MEINS'             'IT_SAIDA_0100' 'MEINS'          'Unid.'          '06'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       09  'ZMMT0086'      'WERKS'             'IT_SAIDA_0100' 'WERKS'          'Centro'         '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       10  'ZMMT0086'      'LGORT'             'IT_SAIDA_0100' 'LGORT'          'Deposito'       '08'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       11  'ZMMT0086'      'CHARG'             'IT_SAIDA_0100' 'CHARG'          'Lote'           '07'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       12  'ZMMT0086'      'SOBRA'             'IT_SAIDA_0100' 'SOBRA'          'Qtd.Sobra'      '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       13  ''              ''                  'IT_SAIDA_0100' 'ENVIAR_OPUS'    'Env.Opus'       '09'   ' '    ''  'X' 'C' 'X' ' ' '' ,
       14  ''              ''                  'IT_SAIDA_0100' 'ST_ENVIO'       'St.Envio'       '09'   ' '    ''  'X' 'C' ' ' ' ' '' ,
       13  'ZMMT0086'      'SOBRA_OPUS'        'IT_SAIDA_0100' 'SOBRA_OPUS'     'Sobra Opus'     '13'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       14  'ZMMT0086'      'DT_REGISTRO'       'IT_SAIDA_0100' 'DT_REGISTRO'    'Data'           '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       15  'ZMMT0086'      'HR_REGISTRO'       'IT_SAIDA_0100' 'HR_REGISTRO'    'Hora'           '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       16  'ZMMT0086'      'USUARIO'           'IT_SAIDA_0100' 'USUARIO'        'Usuário'        '10'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       17  'ZMMT0086'      'OBSERV'            'IT_SAIDA_0100' 'OBSERV'         'Observação'     '30'   ' '    ''  ' ' ' ' ' ' ' ' '' .

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
         WA_SAIDA_0110,
         IT_SAIDA_0110[],
         TG_0086[],
         TG_MAKT[].

ENDFORM.

FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPA_VARIAVEIS.

  IF P_EBELN-LOW IS INITIAL AND P_LIFNR-LOW IS INITIAL.
   MESSAGE 'Informe Pedido ou Fornecedor!' TYPE 'S'.
   EXIT.
  ENDIF.

  SELECT *
    FROM ZMMT0086 INTO TABLE TG_0086
   WHERE EBELN IN P_EBELN
     AND LIFNR IN P_LIFNR.

  CHECK TG_0086[] IS NOT INITIAL.

  SELECT *
    FROM MAKT INTO CORRESPONDING FIELDS OF TABLE TG_MAKT
     FOR ALL ENTRIES IN TG_0086
   WHERE MATNR = TG_0086-MATNR
     AND SPRAS = SY-LANGU.

ENDFORM.

FORM F_PROCESSA_DADOS .

  LOOP AT TG_0086.

    CLEAR: WA_SAIDA_0100.

    MOVE-CORRESPONDING TG_0086 TO WA_SAIDA_0100.

    WA_SAIDA_0100-ENVIAR_OPUS = ICON_EXECUTE_OBJECT.

    READ TABLE TG_MAKT WITH KEY MATNR = TG_0086-MATNR.
    IF SY-SUBRC = 0.
      WA_SAIDA_0100-MAKTX = TG_MAKT-MAKTX.
    ENDIF.

    WA_SAIDA_0100-ST_ENVIO = ICON_LIGHT_OUT.

    IF WA_SAIDA_0100-ST_ENVIO_OPUS = '1'. "Enviado
      WA_SAIDA_0100-ST_ENVIO = ICON_YELLOW_LIGHT.
    ELSEIF WA_SAIDA_0100-ST_ENVIO_OPUS = '2'. "Processado
      WA_SAIDA_0100-ST_ENVIO = ICON_GREEN_LIGHT.
    ENDIF.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.

ENDFORM.

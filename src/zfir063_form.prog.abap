*&---------------------------------------------------------------------*
*&  Include           ZFIR063_FORM
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

       01  'ZFIT0140'      'TXJCD'              'IT_SAIDA_0100'  'TXJCD'                     'Domicílio Fiscal'       '15'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       02  'ZFIT0140'      'DIA_VENCIMENTO'     'IT_SAIDA_0100'  'DIA_VENCIMENTO'            'Dia Vencimento'         '12'   ' '    ''  ' ' 'C' ' ' ' ' '' ,
       04  'ZFIT0140'      'DT_REGISTRO'        'IT_SAIDA_0100'  'DT_REGISTRO'               'Dt.Registro'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       05  'ZFIT0140'      'HR_REGISTRO'        'IT_SAIDA_0100'  'HR_REGISTRO'               'Hr.Registro'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' ,
       06  'ZFIT0140'      'US_REGISTRO'        'IT_SAIDA_0100'  'US_REGISTRO'               'Us.Registro'            '12'   ' '    ''  ' ' ' ' ' ' ' ' '' .

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
         TG_0140[].

ENDFORM.

FORM F_SELECIONAR_DADOS .

  PERFORM F_LIMPA_VARIAVEIS.

  SELECT *
    FROM ZFIT0140 INTO TABLE TG_0140.

  CHECK TG_0140[] IS NOT INITIAL.

ENDFORM.

FORM F_PROCESSA_DADOS .

  LOOP AT TG_0140.

    CLEAR: WA_SAIDA_0100.

    MOVE-CORRESPONDING TG_0140 TO WA_SAIDA_0100.

    APPEND WA_SAIDA_0100 TO IT_SAIDA_0100.

  ENDLOOP.

  SORT IT_SAIDA_0100 BY TXJCD.

ENDFORM.

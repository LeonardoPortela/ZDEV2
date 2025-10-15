*----------------------------------------------------------------------*
***INCLUDE MZMEMORANDO_0001 .
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0001 OUTPUT.

  IF VG_DYNNR_000 IS INITIAL.
    VG_DYNNR_000  = C_1000.
    T_PROPR       = C_X.
    T_TERCE       = C_X.
    CLEAR: IT_MEMORANDOS[].
  ENDIF.

  CASE VG_DYNNR_000.
    WHEN C_1000.
      CLEAR: IT_FCODE.
      AUTHORITY-CHECK OBJECT 'ZSDLIBMEMO' ID 'Z_LIB_MEMO' FIELD C_X.
      IF NOT SY-SUBRC IS INITIAL.
        WA_FCODE = C_FCODE_LIBMEMO.
        APPEND WA_FCODE TO IT_FCODE.

        WA_FCODE = C_FCODE_LIBRESP.
        APPEND WA_FCODE TO IT_FCODE.
      ENDIF.
      SET PF-STATUS 'PFCONSUL' EXCLUDING IT_FCODE.
      SET TITLEBAR 'TLCONSUL'.
    WHEN C_2000.
      CLEAR: IT_FCODE.
      IF TERCEIRO IS NOT INITIAL.
        ZDOC_MEMORANDO-DIRECAO = 2.
      ENDIF.

      IF VG_CONSUL_MEMO IS INITIAL.
        IF ZDOC_MEMORANDO IS INITIAL.
          SET TITLEBAR 'TLLANC'.
        ELSE.
          SET TITLEBAR 'TLALTE'.
        ENDIF.
        WA_FCODE = C_FCODE_CSNFMS.
        APPEND WA_FCODE TO IT_FCODE.
        WA_FCODE = C_FCODE_CSNFMM.
        APPEND WA_FCODE TO IT_FCODE.
      ELSE.
        SET TITLEBAR 'TLCONS'.
      ENDIF.
      IF ZDOC_MEMO_NF_EXP-PROPRIO IS INITIAL.
        WA_FCODE = C_FCODE_NFPROPRIA.
        APPEND WA_FCODE TO IT_FCODE.
      ENDIF.
      IF VG_ALTEROU_MEMORANDO IS INITIAL.
        IF VG_TOTAL_MEMORANDOS GE ZDOC_MEMO_NF_EXP-QUANTIDADE. "ALRS
          WA_FCODE = C_FCODE_SAVE.
          APPEND WA_FCODE TO IT_FCODE.
        ENDIF.
      ENDIF.
      SET PF-STATUS 'PFLANC' EXCLUDING IT_FCODE.
    WHEN C_4000.
      CLEAR: IT_FCODE.
      IF VG_ALTEROU_NOTAS IS INITIAL.
        WA_FCODE = C_FCODE_SAVE.
        APPEND WA_FCODE TO IT_FCODE.
      ENDIF.
      SET PF-STATUS 'PFVINC' EXCLUDING IT_FCODE.
      SET TITLEBAR 'TLVINC'.
    WHEN C_6000.
      CLEAR: IT_FCODE.
      IF VG_ALTEROU_NOTAS IS INITIAL.
        WA_FCODE = C_FCODE_SAVE.
        APPEND WA_FCODE TO IT_FCODE.
      ENDIF.
      SET PF-STATUS 'PFVINCS' EXCLUDING IT_FCODE.
      SET TITLEBAR 'TLVINCS'.
  ENDCASE.

ENDMODULE.                 " STATUS_0001  OUTPUT

*&---------------------------------------------------------------------*
*&      Form  CRIA_ALV_EDITAR_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CRIA_ALV_EDITAR_FIELDCAT .

  DATA: PLAN_GS_LAYOUT         TYPE LVC_S_LAYO.


  CONSTANTS: TABELA_PROGRAMA TYPE STRING VALUE 'IT_MEMORANDO_TELA'.


  IF VG_PRIMEIRO_VISUAL_ED IS INITIAL.

*   Create object for container
    CREATE OBJECT CTL_CCCONTAINER_EDITAR
      EXPORTING
        CONTAINER_NAME = 'EDITAR_FIELDCAT'.

    CREATE OBJECT CTL_ALV_MEMO_EDITAR
      EXPORTING
        I_PARENT = CTL_CCCONTAINER_EDITAR.




    PERFORM Z_ESTRUTURA_FIELDCAT_EDITAR TABLES IT_FIELDCATALOG USING:


    TABELA_PROGRAMA 'ICONE'                  SPACE    ' ' 01 03 'X'   ,
    TABELA_PROGRAMA 'TP_FINALIDADE'          TEXT-T03 ' ' 13 10 SPACE ,
    TABELA_PROGRAMA 'DIRECAO'                TEXT-T04 ' ' 12 08 SPACE ,
    TABELA_PROGRAMA 'NR_COMPLEMENTO'         TEXT-T04 ' ' 12 08 SPACE ,
    TABELA_PROGRAMA 'REPRESENTANTE'          TEXT-T26 ' ' 05 10 SPACE ,
    TABELA_PROGRAMA 'REMETENTE'              TEXT-T07 ' ' 08 10 SPACE ,
    TABELA_PROGRAMA 'DT_EMISSAO_MEMO'        TEXT-T13 ' ' 14 10 SPACE ,
    TABELA_PROGRAMA 'QUANTIDADE_MEMO'        TEXT-T18 ' ' 22 13 SPACE ,
    TABELA_PROGRAMA 'NUMERO_MEMO'            TEXT-T20 ' ' 03 08 'X'   ,
    TABELA_PROGRAMA 'STATUS'                 TEXT-T02 ' ' 11 10 SPACE ,
    TABELA_PROGRAMA 'VG_NOME_REPRESENTANTE'  TEXT-T07 ' ' 08 10 SPACE ,
    TABELA_PROGRAMA 'VG_NOME_REMETENTE    '  TEXT-T07 ' ' 08 10 SPACE ,
    TABELA_PROGRAMA 'VG_NOME_PAIS_ORIGEM'    TEXT-T23 ' ' 26 03 SPACE ,
    TABELA_PROGRAMA 'VG_NOME_PAIS_DESTINO'   TEXT-T23 ' ' 26 03 SPACE ,
    TABELA_PROGRAMA 'VG_NOME_UF_ORIGEM'      TEXT-T22 ' ' 25 03 SPACE .

    CLEAR: PLAN_GS_LAYOUT.
    PLAN_GS_LAYOUT-ZEBRA    = C_X.
    PLAN_GS_LAYOUT-SEL_MODE = 'A'.

    CALL METHOD CTL_ALV_MEMO_EDITAR->SET_TABLE_FOR_FIRST_DISPLAY
      EXPORTING
        IS_LAYOUT       = PLAN_GS_LAYOUT
        I_SAVE          = 'A'
      CHANGING
        IT_FIELDCATALOG = IT_FIELDCATALOG_EDITAR "formato
        IT_OUTTAB       = IT_MEMORANDO_TELA[].   "dados

    VG_PRIMEIRO_VISUAL_ED = C_X.
  ELSE.
    CALL METHOD CTL_ALV_MEMO_EDITAR->REFRESH_TABLE_DISPLAY.
  ENDIF.

ENDFORM.                    " CRIA_ALV_EDITAR_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  Z_ESTRUTURA_FIELDCAT_EDITAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDCATALOG  text
*      -->P_TABELA_PROGRAMA  text
*      -->P_0192   text
*      -->P_SPACE  text
*      -->P_0194   text
*      -->P_01     text
*      -->P_03     text
*      -->P_0197   text
*----------------------------------------------------------------------*

FORM Z_ESTRUTURA_FIELDCAT_EDITAR TABLES IT_FIELDCATALOG_EDITAR TYPE LVC_T_FCAT
                         USING P_TAB_NAME
                               P_FIELDNAME
                               P_TEXTO_GRANDE
                               P_HOT
                               P_POSICAO
                               P_OUTPUTLEN
                               P_FIX_COLUMN.


  CLEAR WA_FIELDCATALOG_EDITAR.
  WA_FIELDCATALOG_EDITAR-FIELDNAME   = P_FIELDNAME.
  WA_FIELDCATALOG_EDITAR-TABNAME     = 'IT_MEMORANDO_TELA'.
  WA_FIELDCATALOG_EDITAR-SCRTEXT_L   = P_TEXTO_GRANDE.
  WA_FIELDCATALOG_EDITAR-SCRTEXT_M   = P_TEXTO_GRANDE.
  WA_FIELDCATALOG_EDITAR-SCRTEXT_S   = P_TEXTO_GRANDE.
  WA_FIELDCATALOG_EDITAR-HOTSPOT     = P_HOT.
  WA_FIELDCATALOG_EDITAR-COL_POS     = P_POSICAO.
  WA_FIELDCATALOG_EDITAR-OUTPUTLEN   = P_OUTPUTLEN.
  WA_FIELDCATALOG_EDITAR-FIX_COLUMN  = P_FIX_COLUMN.

  APPEND WA_FIELDCATALOG_EDITAR TO IT_FIELDCATALOG_EDITAR.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT_EDITAR
*&---------------------------------------------------------------------*
*&      Module  CRIA_ALV_EDITAR_FIELDCAT  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE CRIA_ALV_EDITAR_FIELDCAT OUTPUT.
  PERFORM CRIA_ALV_EDITAR_FIELDCAT.
ENDMODULE.                 " CRIA_ALV_EDITAR_FIELDCAT  OUTPUT

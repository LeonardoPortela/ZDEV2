*----------------------------------------------------------------------*
***INCLUDE ZFIS37_F01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_MONTAR_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SPACE  text
*----------------------------------------------------------------------*
FORM F_MONTAR_LAYOUT  USING    P_SPACE.
  REFRESH T_FIELDCATALOG.
  PERFORM F_MONTAR_ESTRUTURA USING:
        1   'LFA1'     'LIFNR'       'TG_SAIDA'  'LIFNR'      'Fornecedor'         '10' 'X'    ' ' 'X',
        2   ''         ''            'TG_SAIDA'  'NAME1'      'Descrição'          '30' ''     ' ' 'X',
        2   ''         ''            'TG_SAIDA'  'DATA_ATUAL' 'Data'               '10' ''     ' ' 'X',
        2   ''         ''            'TG_SAIDA'  'HORA_ATUAL' 'Hora'               '10' ''     ' ' 'X',
        2   ''         ''            'TG_SAIDA'  'USUARIO'    'Usuário'            '15' ''     ' ' 'X'.
ENDFORM.

FORM F_MONTAR_ESTRUTURA  USING  P_COL_POS   P_REF_TABNAME P_REF_FIELDNAME P_TABNAME P_FIELD
                                P_SCRTEXT_L P_OUTPUTLEN   P_EDIT          P_SUM     P_EMPHASIZE.

  CLEAR W_FIELDCATALOG.
  W_FIELDCATALOG-FIELDNAME     = P_FIELD.
  W_FIELDCATALOG-TABNAME       = P_TABNAME.
  W_FIELDCATALOG-REF_TABLE     = P_REF_TABNAME.
  W_FIELDCATALOG-REF_FIELD     = P_REF_FIELDNAME.

  W_FIELDCATALOG-KEY           = ' '.


  W_FIELDCATALOG-EDIT          = P_EDIT.
  W_FIELDCATALOG-DO_SUM        = P_SUM.

  W_FIELDCATALOG-COL_POS       = P_COL_POS.

  IF P_OUTPUTLEN IS NOT INITIAL.
    W_FIELDCATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  ENDIF.

  W_FIELDCATALOG-NO_OUT        = ' '.
  W_FIELDCATALOG-REPTEXT       = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_S     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_M     = P_SCRTEXT_L.
  W_FIELDCATALOG-SCRTEXT_L     = P_SCRTEXT_L.
  W_FIELDCATALOG-EMPHASIZE     = P_EMPHASIZE.

  APPEND W_FIELDCATALOG TO T_FIELDCATALOG.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS .


  WG_ACAO = C_MODIF.
  SELECT ZFIT0145~LIFNR
         LFA1~NAME1
         ZFIT0145~DATA_ATUAL
         ZFIT0145~HORA_ATUAL
         ZFIT0145~USUARIO
    FROM ZFIT0145
    INNER JOIN LFA1
    ON LFA1~LIFNR = ZFIT0145~LIFNR
       INTO CORRESPONDING FIELDS OF TABLE TG_SAIDA.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_VERIFICA_ERROS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_VERIFICA_ERROS .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVA_DADOS .
  DATA: WL_ZFIT0145 TYPE ZFIT0145,
        TL_ZFIT0145 TYPE TABLE OF ZFIT0145.

  LOOP AT TG_SAIDA INTO WG_SAIDA.
    WL_ZFIT0145-LIFNR       = WG_SAIDA-LIFNR.
    WL_ZFIT0145-DATA_ATUAL  = WG_SAIDA-DATA_ATUAL.
    WL_ZFIT0145-HORA_ATUAL  = WG_SAIDA-HORA_ATUAL.
    WL_ZFIT0145-USUARIO     = WG_SAIDA-USUARIO.

    APPEND WL_ZFIT0145 TO TL_ZFIT0145.
  ENDLOOP.
  DELETE FROM ZFIT0145.

  MODIFY ZFIT0145 FROM TABLE TL_ZFIT0145.
  "
  MESSAGE S836(SD) WITH TEXT-M01  TEXT-M02.
ENDFORM.

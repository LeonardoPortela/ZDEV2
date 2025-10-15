*----------------------------------------------------------------------*
***INCLUDE MZBALANCO_XXXX .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
* Alimentar a tabela interna de estrutura fieldcat.
*----------------------------------------------------------------------*
FORM Z_ESTRUTURA_FIELDCAT TABLES IT_CATALOGO TYPE LVC_T_FCAT
                           USING P_TAB_NAME
                                 P_FIELDNAME
                                 P_TEXTO_GRANDE
                                 P_HOT
                                 P_POSICAO
                                 P_OUTPUTLEN
                                 P_FIX_COLUMN
                                 P_CONVEXIT
                                 P_DO_SUM
                                 P_ICON
                                 P_JUST
                                 P_EMPHASIZE
                                 P_EDIT
                                 P_CHECKBOX.

  DATA: WA_CATALOG TYPE LVC_S_FCAT.
  WA_CATALOG-TABNAME     = P_TAB_NAME.
  WA_CATALOG-FIELDNAME   = P_FIELDNAME.
  WA_CATALOG-SCRTEXT_L   = P_TEXTO_GRANDE.
  WA_CATALOG-SCRTEXT_M   = P_TEXTO_GRANDE.
  WA_CATALOG-SCRTEXT_S   = P_TEXTO_GRANDE.
  WA_CATALOG-HOTSPOT     = P_HOT.
  WA_CATALOG-COL_POS     = P_POSICAO.
  WA_CATALOG-OUTPUTLEN   = P_OUTPUTLEN.
  WA_CATALOG-FIX_COLUMN  = P_FIX_COLUMN.
  WA_CATALOG-CONVEXIT    = P_CONVEXIT.
  WA_CATALOG-DO_SUM      = P_DO_SUM.
  WA_CATALOG-ICON        = P_ICON.
  WA_CATALOG-JUST        = P_JUST.
  WA_CATALOG-EMPHASIZE   = P_EMPHASIZE.
  WA_CATALOG-EDIT        = P_EDIT.
  WA_CATALOG-CHECKBOX    = P_CHECKBOX.
  APPEND WA_CATALOG TO IT_CATALOGO.

ENDFORM.                    " Z_ESTRUTURA_FIELDCAT

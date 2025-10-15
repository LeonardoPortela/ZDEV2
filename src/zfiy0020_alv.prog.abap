*----------------------------------------------------------------------*
* Include ZFIY0020_ALV
*----------------------------------------------------------------------*

*-----------------------------------------------------------------------
* Form  F_CARGA_CAMPOS_HEADER
*-----------------------------------------------------------------------
FORM F_CARGA_CAMPOS_HEADER USING X_TYP X_KEY X_INFO .       "#EC *

  CLEAR T_HEAD.
  T_HEAD-TYP  = X_TYP.
  T_HEAD-KEY  = X_KEY.
  T_HEAD-INFO = X_INFO.
  APPEND T_HEAD TO T_HEADER.

ENDFORM.                    " F_CARGA_CAMPOS_HEADER
*-----------------------------------------------------------------------
* Form  F_CARGA_CAMPOS_COL
*-----------------------------------------------------------------------
FORM F_CARGA_CAMPOS_COL USING X_POS X_FIELD X_TAB X_REFC X_REF X_SUM
                                X_JUST X_HOT X_TEXT_S X_TEXT_M X_TEXT_L
                                X_KEY X_FIX_COL X_EMP X_CURR X_LEN
                                X_BOX X_INPUT X_ICON X_TYPE X_EDIT. "#EC *

* Preenche tabela interna com os valores para o layout.
  CLEAR T_AFIELD.
  T_AFIELD-COL_POS       = X_POS.
  T_AFIELD-FIELDNAME     = X_FIELD.
  T_AFIELD-TABNAME       = X_TAB.
  T_AFIELD-REF_FIELDNAME = X_REFC.
  T_AFIELD-REF_TABNAME   = X_REF.
  T_AFIELD-DO_SUM        = X_SUM.
  T_AFIELD-JUST          = X_JUST.
  T_AFIELD-HOTSPOT       = X_HOT.
  T_AFIELD-SELTEXT_S     = X_TEXT_S.
  T_AFIELD-SELTEXT_M     = X_TEXT_M.
  T_AFIELD-SELTEXT_L     = X_TEXT_L.
  T_AFIELD-KEY           = X_KEY.
  T_AFIELD-FIX_COLUMN    = X_FIX_COL.
  T_AFIELD-EMPHASIZE     = X_EMP.
  T_AFIELD-CURRENCY      = X_CURR.
  T_AFIELD-OUTPUTLEN     = X_LEN.
  T_AFIELD-CHECKBOX      = X_BOX.
  T_AFIELD-INPUT         = X_INPUT.
  T_AFIELD-ICON          = X_ICON.
  T_AFIELD-INTTYPE       = X_TYPE.
  T_AFIELD-EDIT          = X_EDIT.
  APPEND T_AFIELD TO T_FIELDCAT.

ENDFORM.                 " F_CARGA_CAMPOS_COL
*-----------------------------------------------------------------------
* Form  F_CARGA_CAMPOS_QUIEBRA
*-----------------------------------------------------------------------
FORM F_CARGA_CAMPOS_QUIEBRA USING X_POS X_FIELD X_GROUP X_SUBT X_UP . "#EC *

* Ordenação - Quebra
  CLEAR T_SORT.
  T_SORT-SPOS      = X_POS.
  T_SORT-FIELDNAME = X_FIELD.
  T_SORT-GROUP     = X_GROUP.
  T_SORT-SUBTOT    = X_SUBT.
  T_SORT-UP        = X_UP.
  APPEND T_SORT.

ENDFORM.                    " F_CARGA_CAMPOS_QUIEBRA
*-----------------------------------------------------------------------
* Form  F_CARGA_EVENTOS
*-----------------------------------------------------------------------
FORM F_CARGA_EVENTOS USING X_EVEN X_FORM.                   "#EC *

  IF T_EVENT[] IS INITIAL.
    CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
      EXPORTING
        I_LIST_TYPE = 0
      IMPORTING
        ET_EVENTS   = T_EVENT.
  ENDIF.

  READ TABLE T_EVENT WITH KEY NAME = X_EVEN INTO T_EVENTS.

  IF SY-SUBRC EQ 0.
    MOVE X_FORM     TO T_EVENTS-FORM.
    APPEND T_EVENTS TO T_EVENT.
    DELETE T_EVENT WHERE NAME = X_EVEN
                     AND FORM = SPACE.
  ENDIF.

ENDFORM.                    " F_CARGA_EVENTOS
*-----------------------------------------------------------------------
* Form  F_TABLA_ALV
*-----------------------------------------------------------------------
FORM F_TABLA_ALV.

  LOOP AT T_SAL INTO E_SAL.
    CLEAR T_OUT.
    MOVE-CORRESPONDING E_SAL TO T_OUT.
    APPEND T_OUT.
  ENDLOOP.

ENDFORM.                    " F_TABLA_ALV
*-----------------------------------------------------------------------
* Form  F_CARGA_CAMPOS_LAYOUT
*-----------------------------------------------------------------------
FORM F_CARGA_CAMPOS_LAYOUT USING X_GROUP X_NTOT X_TOT X_CELL X_OPT
                                 X_BOX X_FLEX X_DET X_TEXT X_TEXS
                                 X_F2C X_ZEBRA.             "#EC *

* Carrega as configurações do layout
  T_LAYOUT-COLTAB_FIELDNAME     = 'COLINFO'.
  T_LAYOUT-GROUP_BUTTONS        = X_GROUP.
  T_LAYOUT-NO_TOTALLINE         = X_NTOT.
  T_LAYOUT-TOTALS_ONLY          = X_TOT.
  T_LAYOUT-CELL_MERGE           = X_CELL.
  T_LAYOUT-COLWIDTH_OPTIMIZE    = X_OPT.
  T_LAYOUT-BOX_FIELDNAME        = X_BOX.
  T_LAYOUT-FLEXIBLE_KEY         = X_FLEX.
  T_LAYOUT-DETAIL_INITIAL_LINES = X_DET.
  T_LAYOUT-TOTALS_TEXT          = X_TEXT.
  T_LAYOUT-SUBTOTALS_TEXT       = X_TEXS.
  T_LAYOUT-F2CODE               = X_F2C.
  T_LAYOUT-ZEBRA                = X_ZEBRA.

  CLEAR T_LAY.
  REFRESH T_FCAT.

* Executa a função para carga de dados de colunas e layout
  CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'
    EXPORTING
      IT_FIELDCAT = T_FIELDCAT
      IS_LAYOUT   = T_LAYOUT
    IMPORTING
      ET_FIELDCAT = T_FCAT
      ES_LAYOUT   = T_LAY.

ENDFORM.                    " F_CARGA_CAMPOS_LAYOUT
*-----------------------------------------------------------------------
* Form  F_SALIDA_ALV
*-----------------------------------------------------------------------
FORM F_SALIDA_ALV.

  DATA: V_SAVE  TYPE C,
        V_REPID LIKE SY-REPID.

  CALL FUNCTION 'K_KKB_SAVE_MODE_GET'
    IMPORTING
      E_SAVE = V_SAVE.

  MOVE : SY-REPID TO V_REPID,
         V_REPID  TO T_VARIANT-REPORT.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      I_CALLBACK_PROGRAM       = V_REPID
      IS_LAYOUT                = T_LAY
      IT_FIELDCAT              = T_FCAT
      IT_SORT                  = T_SORT[]
      I_DEFAULT                = 'X'
      I_SAVE                   = V_SAVE
      IS_VARIANT               = T_VARIANT
      IT_EVENTS                = T_EVENT[]
    TABLES
      T_OUTTAB                 = T_OUT
    EXCEPTIONS
      PROGRAM_ERROR            = 1
      OTHERS                   = 2.

  IF SY-SUBRC NE 0.
    MESSAGE E011(PC) WITH TEXT-E00.                         "#EC NOTEXT
  ENDIF.

ENDFORM.                    " F_SALIDA_ALV
*-----------------------------------------------------------------------
* Form  F_TOP_OF_PAGE
*-----------------------------------------------------------------------
FORM F_TOP_OF_PAGE.                                         "#EC CALLED

* Imprime cabeçalho ALV
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      IT_LIST_COMMENTARY = T_HEADER.

ENDFORM.                    " F_TOP_OF_PAGE

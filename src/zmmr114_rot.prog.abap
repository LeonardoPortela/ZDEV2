
*----------------------------------------------------------------------*
***INCLUDE ZMMR114_ROT.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_BUSCA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_BUSCA_DADOS .
  DATA: WL_ZMMT0067     TYPE ZMMT0067,
        WL_ZMMT0067_AUX TYPE ZMMT0067,
        GT_ZMMT0067_AUX TYPE TABLE OF ZMMT0067,
        GT_MAKT         TYPE TABLE OF MAKT,
        WL_MAKT         TYPE MAKT.

  IF WG_ACAO IS NOT INITIAL.
    EXIT.
  ENDIF.
  IF WG_BUKRS IS INITIAL.
    MESSAGE  'Informe a empresa'   TYPE 'I'.
    EXIT.
  ENDIF.

  REFRESH R_WERKS.
  REFRESH GT_SAIDA_ITENS.

  SELECT *
    FROM ZMMT0067
    INNER JOIN J_1BBRANCH
    ON  BUKRS  = WG_BUKRS
    AND BRANCH = ZMMT0067~WERKS
    INTO CORRESPONDING FIELDS OF TABLE GT_ZMMT0067
    WHERE WERKS IN R_WERKS
    ORDER BY MATNR APROVADOR WERKS.

  CHECK GT_ZMMT0067[] IS NOT INITIAL.

  SELECT *
    FROM MAKT
    INTO TABLE GT_MAKT
    FOR ALL ENTRIES IN GT_ZMMT0067
    WHERE MATNR = GT_ZMMT0067-MATNR
    AND   SPRAS = SY-LANGU.

  SORT GT_MAKT BY MATNR.

  GT_ZMMT0067_AUX[] = GT_ZMMT0067[].
  DELETE ADJACENT DUPLICATES FROM GT_ZMMT0067_AUX COMPARING MATNR APROVADOR.


  LOOP AT GT_ZMMT0067_AUX INTO WL_ZMMT0067_AUX.
    CLEAR WL_SAIDA_ITENS.

    LOOP AT GT_ZMMT0067 INTO WL_ZMMT0067 WHERE MATNR     = WL_ZMMT0067_AUX-MATNR
                                         AND   APROVADOR = WL_ZMMT0067_AUX-APROVADOR.
      R_WERKS-OPTION = 'EQ'.
      R_WERKS-SIGN   = 'I'.
      R_WERKS-LOW    = WL_ZMMT0067-WERKS.
      "
      APPEND R_WERKS TO WL_SAIDA_ITENS-T_WERKS.

    ENDLOOP.
    WL_SAIDA_ITENS-WERKS      = ICON_TE_COSTS_ASSIGN.
    WL_SAIDA_ITENS-MATNR      = WL_ZMMT0067-MATNR.
    WL_SAIDA_ITENS-APROVADOR  = WL_ZMMT0067-APROVADOR.
    WL_SAIDA_ITENS-DATA_ATUAL = WL_ZMMT0067-DATA_ATUAL.
    WL_SAIDA_ITENS-HORA_ATUAL = WL_ZMMT0067-HORA_ATUAL.
    WL_SAIDA_ITENS-USUARIO    = WL_ZMMT0067-USUARIO.

    READ TABLE GT_MAKT INTO WL_MAKT WITH KEY MATNR = WL_ZMMT0067-MATNR BINARY SEARCH.
    WL_SAIDA_ITENS-TXZ01 = WL_MAKT-MAKTX.
    APPEND WL_SAIDA_ITENS TO GT_SAIDA_ITENS.

  ENDLOOP.

  GT_ZMMT0067_AUX[] = GT_ZMMT0067[].
  SORT GT_ZMMT0067_AUX BY WERKS.
  DELETE ADJACENT DUPLICATES FROM GT_ZMMT0067_AUX COMPARING WERKS.
  LOOP AT GT_ZMMT0067_AUX INTO WL_ZMMT0067_AUX.
    R_WERKS-OPTION = 'EQ'.
    R_WERKS-SIGN   = 'I'.
    R_WERKS-LOW    = WL_ZMMT0067_AUX-WERKS.
    APPEND R_WERKS.
  ENDLOOP.

  WG_ACAO = 'UPD'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELECTIONS_DIALOG
*&---------------------------------------------------------------------*

FORM F_SELECTIONS_DIALOG TABLES P_RANGE
                          USING P_TITLE
                                P_DISABLED
                                P_HELP.

  DATA: LW_RSOPTIONS TYPE RSOPTIONS.

  MOVE: ABAP_TRUE TO LW_RSOPTIONS-BT,
        ABAP_TRUE TO LW_RSOPTIONS-CP,
        ABAP_TRUE TO LW_RSOPTIONS-GE,
        ABAP_TRUE TO LW_RSOPTIONS-GT,
        ABAP_TRUE TO LW_RSOPTIONS-LE,
        ABAP_TRUE TO LW_RSOPTIONS-LT,
        ABAP_TRUE TO LW_RSOPTIONS-NB,
        ABAP_TRUE TO LW_RSOPTIONS-NP.

  DATA(_TITLE) = CONV SY-TITLE( P_TITLE ).

  CALL FUNCTION 'COMPLEX_SELECTIONS_DIALOG'
    EXPORTING
      TITLE             = _TITLE
      SEARCH_HELP       = P_HELP
      JUST_DISPLAY      = P_DISABLED
      EXCLUDED_OPTIONS  = LW_RSOPTIONS
    TABLES
      RANGE             = P_RANGE
    EXCEPTIONS
      NO_RANGE_TAB      = 1
      CANCELLED         = 2
      INTERNAL_ERROR    = 3
      INVALID_FIELDNAME = 4
      OTHERS            = 5.
ENDFORM.                    "F_SELECTIONS_DIALOG

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
FORM ALV_PREENCHE_CAT  USING: P_CAMPO         TYPE C
                              P_DESC          TYPE C
                              P_TAM           TYPE C
                              P_HOT           TYPE C
                              P_ZERO          TYPE C
                              P_JUST          TYPE C
                              P_ICON          TYPE C
                              P_EDIT          TYPE C
                              P_REF_TABNAME   LIKE DD02D-TABNAME
                              P_REF_FIELDNAME LIKE DD03D-FIELDNAME
                              P_TABNAME       LIKE DD02D-TABNAME
                              P_F4            TYPE DDF4AVAIL.

  WL_FCAT-FIELDNAME  = P_CAMPO.
  WL_FCAT-SCRTEXT_L  = P_DESC.
  WL_FCAT-SCRTEXT_M  = P_DESC.
  WL_FCAT-SCRTEXT_S  = P_DESC.
  WL_FCAT-HOTSPOT    = P_HOT.
  WL_FCAT-NO_ZERO    = P_ZERO.
  WL_FCAT-OUTPUTLEN  = P_TAM.
  WL_FCAT-JUST       = P_JUST.
  WL_FCAT-ICON       = P_ICON.
  WL_FCAT-EDIT       = P_EDIT.
  WL_FCAT-REF_TABLE  = P_REF_TABNAME.
  WL_FCAT-REF_FIELD  = P_REF_FIELDNAME.
  WL_FCAT-TABNAME    = P_TABNAME.
  WL_FCAT-F4AVAILABL = P_F4.

  IF ( WL_FCAT-FIELDNAME = 'WERKS' ).
    WL_FCAT-STYLE = CL_GUI_ALV_GRID=>MC_STYLE_BUTTON.
  ENDIF.

  APPEND WL_FCAT TO GT_FCAT_0110.


  CLEAR WL_FCAT.
ENDFORM.                    "ALV_PREENCHE_CAT

FORM EXCLUDE_TB_FUNCTIONS CHANGING WL_EXC_BUTTON TYPE UI_FUNCTIONS.

  APPEND CL_GUI_ALV_GRID=>MC_FC_REFRESH           TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_DELETE_ROW    TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_INSERT_ROW    TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_APPEND_ROW    TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY          TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_COPY_ROW      TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_CUT           TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_UNDO          TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE         TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_LOC_PASTE_NEW_ROW TO WL_EXC_BUTTON.
  APPEND CL_GUI_ALV_GRID=>MC_FC_CHECK             TO WL_EXC_BUTTON.
ENDFORM.                    "EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM F_GRAVA .
  DATA: WL_ZMMT0067 TYPE ZMMT0067,
        MW_WERKS    TYPE WERKS_RANG.

  REFRESH GT_ZMMT0067.
  SORT GT_SAIDA_ITENS BY T_WERKS MATNR.
  LOOP AT GT_SAIDA_ITENS INTO WL_SAIDA_ITENS.
    LOOP AT WL_SAIDA_ITENS-T_WERKS INTO MW_WERKS.
      CLEAR WL_ZMMT0067.
      WL_ZMMT0067-WERKS      = MW_WERKS-LOW.
      WL_ZMMT0067-MATNR      = WL_SAIDA_ITENS-MATNR.
      WL_ZMMT0067-MATKL      = WL_SAIDA_ITENS-MATKL.
      WL_ZMMT0067-APROVADOR  = WL_SAIDA_ITENS-APROVADOR.
      WL_ZMMT0067-DATA_ATUAL = SY-DATUM.
      WL_ZMMT0067-HORA_ATUAL = SY-UZEIT.
      WL_ZMMT0067-USUARIO    = SY-UNAME.
      APPEND WL_ZMMT0067 TO GT_ZMMT0067.
    ENDLOOP.
    DELETE FROM ZMMT0067 WHERE WERKS = MW_WERKS-LOW.
  ENDLOOP.

  LOOP AT R_WERKS[] INTO R_WERKS.
    DELETE FROM ZMMT0067 WHERE WERKS = @R_WERKS-LOW.
  ENDLOOP.

  MODIFY ZMMT0067 FROM TABLE GT_ZMMT0067.
  COMMIT WORK.

  CLEAR WG_ACAO.

  MESSAGE TEXT-S01 TYPE 'S'.

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
  DELETE GT_SAIDA_ITENS WHERE MATNR IS INITIAL OR APROVADOR IS INITIAL.

ENDFORM.

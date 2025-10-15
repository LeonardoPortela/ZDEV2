DATA: LT_ZMMT0067    TYPE TABLE OF ZMMT0067,
      LT_ZMMT0064    TYPE TABLE OF ZMMT0064,
      LW_NODE_TEXT   TYPE LVC_VALUE,
      LW_LAYOUT_NODE TYPE LVC_S_LAYN.

*&---------------------------------------------------------------------*
*&      Form  create_hierarchy
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_CREATE_HIERARCHY.

  DATA: LV_FILHO_KEY   TYPE LVC_NKEY,
        LV_LINES       TYPE CHAR2,
        LT_LAYOUT_ITEM TYPE LVC_T_LAYI,
        LW_LAYOUT_ITEM TYPE LVC_S_LAYI.

  REFRESH: LT_ZMMT0064,
           GT_MENU_TREE,
           LT_ZMMT0067.

  PERFORM F_SELECIONA_ESTRATEGIA TABLES LT_ZMMT0067.

  IF ( LT_ZMMT0067 IS NOT INITIAL ).
    PERFORM F_SELECIONA_CONTRATOS  TABLES LT_ZMMT0064.
  ENDIF.

  CASE SY-DYNNR.
    WHEN 0100.
      DESCRIBE TABLE LT_ZMMT0064 LINES LV_LINES.

      GW_MENU_TREE-NODE_IMAGE_FILHO = ICON_CONTRACT_ACCOUNT.
      GW_MENU_TREE-NODE_FILHO       = 'Criar/visualizar'.
      APPEND GW_MENU_TREE TO GT_MENU_TREE.

      "//Verifica se o usuário é um aprovador para exibir no treeview
      "//a coluna de aprovação;
      IF ( NOT LT_ZMMT0064 IS INITIAL ).

*        DATA(LINES) = LINES( LT_ZMMT0064 ).

        GW_MENU_TREE-NODE_IMAGE_FILHO = ICON_RELEASE.
        GW_MENU_TREE-NODE_FILHO = 'Workflow aprovação'.
        APPEND GW_MENU_TREE TO GT_MENU_TREE.
      ENDIF.
      "--

      LOOP AT GT_MENU_TREE INTO GW_MENU_TREE.
        IF ( NOT GW_MENU_TREE IS INITIAL ).

          ON CHANGE OF GW_MENU_TREE-NODE_FILHO.
            PERFORM F_ADD_FILHO USING
                                GW_MENU_TREE-NODE_IMAGE_FILHO
                                GW_MENU_TREE-NODE_FILHO
                                LT_LAYOUT_ITEM
                                CHANGING
                                LV_FILHO_KEY.
          ENDON.

        ENDIF.
      ENDLOOP.

    WHEN 0120.
      DATA: LV_ITEM TYPE MTREEITM,
            LV_NODE TYPE TREEV_NODE.

      REFRESH: GT_NODE_TABLE,
               GT_ITEM_TABLE.

      LOOP AT LT_ZMMT0064 INTO WL_ZMMT0064.
        LV_NODE-NODE_KEY = SY-TABIX.
        LV_NODE-HIDDEN   = ABAP_FALSE.
        LV_NODE-DISABLED = ABAP_FALSE.
        LV_NODE-ISFOLDER = ABAP_FALSE.
        APPEND LV_NODE TO GT_NODE_TABLE.

        CLEAR LV_ITEM.
        LV_ITEM-NODE_KEY  = LV_NODE-NODE_KEY.
        LV_ITEM-ITEM_NAME = C_COLUMN-COLUMN1.
        LV_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
        LV_ITEM-TEXT      = WL_ZMMT0064-EVRTN.
*        LV_ITEM-STYLE     = CL_TREE_CONTROL_BASE=>STYLE_EMPHASIZED_POSITIVE.
        APPEND LV_ITEM TO GT_ITEM_TABLE.

        CLEAR LV_ITEM.
        LV_ITEM-NODE_KEY  = LV_NODE-NODE_KEY.
        LV_ITEM-ITEM_NAME = C_COLUMN-COLUMN2.
        LV_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
        LV_ITEM-TEXT      = WL_ZMMT0064-EVRTP.
*        LV_ITEM-STYLE = '2'.
        APPEND LV_ITEM TO GT_ITEM_TABLE.

        CLEAR LV_ITEM.
        LV_ITEM-NODE_KEY  = LV_NODE-NODE_KEY.
        LV_ITEM-ITEM_NAME = C_COLUMN-COLUMN6.
        LV_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_TEXT.
        LV_ITEM-TEXT      = WL_ZMMT0064-WERKS.
        APPEND LV_ITEM TO GT_ITEM_TABLE.

        CLEAR LV_ITEM.
        LV_ITEM-NODE_KEY  = LV_NODE-NODE_KEY.
        LV_ITEM-ITEM_NAME = C_COLUMN-COLUMN3.
        LV_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_BUTTON.
        LV_ITEM-T_IMAGE   = ICON_DISPLAY.
        APPEND LV_ITEM TO GT_ITEM_TABLE.

        CLEAR LV_ITEM.
        LV_ITEM-NODE_KEY  = LV_NODE-NODE_KEY.
        LV_ITEM-ITEM_NAME = C_COLUMN-COLUMN4.
        LV_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_BUTTON.
        LV_ITEM-T_IMAGE   = ICON_ALLOW.
        APPEND LV_ITEM TO GT_ITEM_TABLE.

        CLEAR LV_ITEM.
        LV_ITEM-NODE_KEY  = LV_NODE-NODE_KEY.
        LV_ITEM-ITEM_NAME = C_COLUMN-COLUMN5.
        LV_ITEM-CLASS     = CL_GUI_COLUMN_TREE=>ITEM_CLASS_BUTTON.
        LV_ITEM-T_IMAGE   = ICON_REJECT.
        APPEND LV_ITEM TO GT_ITEM_TABLE.
      ENDLOOP.
  ENDCASE.
ENDFORM.                    "CREATE_HIERARCHY

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_CONTRATOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_SELECIONA_CONTRATOS TABLES T_ZMMT0064.
  CREATE OBJECT R_OPERACAO.

  LOOP AT LT_ZMMT0067 INTO DATA(_ZMMT0067).
    IF ( _ZMMT0067-MATKL IS INITIAL
    AND  _ZMMT0067-MATNR IS INITIAL ).

      SELECT SINGLE *
        FROM ZMMT0067
        INTO @DATA(LW_ZMMT0067)
       WHERE MATNR     NE @SPACE
          OR MATKL     NE @SPACE
         AND WERKS     EQ @_ZMMT0067-WERKS.

      IF SY-SUBRC IS INITIAL.
        SELECT *
     APPENDING TABLE T_ZMMT0064
          FROM ZMMT0064
         WHERE EMATN NE LW_ZMMT0067-MATNR
           AND MATKL NE LW_ZMMT0067-MATKL
           AND WERKS EQ _ZMMT0067-WERKS
           AND NIVEL NOT IN (R_OPERACAO->C_STATUS-APROVADO,
                             R_OPERACAO->C_STATUS-REJEITADO).
      ELSE.
        SELECT *
     APPENDING TABLE T_ZMMT0064
          FROM ZMMT0064
         WHERE WERKS EQ _ZMMT0067-WERKS
           AND NIVEL NOT IN (R_OPERACAO->C_STATUS-APROVADO,
                             R_OPERACAO->C_STATUS-REJEITADO).
      ENDIF.

    ELSEIF ( _ZMMT0067-MATKL IS INITIAL ).

      SELECT *
   APPENDING TABLE T_ZMMT0064
        FROM ZMMT0064
       WHERE WERKS EQ _ZMMT0067-WERKS
         AND EMATN EQ _ZMMT0067-MATNR
         AND NIVEL NOT IN (R_OPERACAO->C_STATUS-APROVADO,
                           R_OPERACAO->C_STATUS-REJEITADO).

    ELSEIF ( _ZMMT0067-MATNR IS INITIAL ).

      SELECT *
   APPENDING TABLE T_ZMMT0064
        FROM ZMMT0064
       WHERE WERKS EQ _ZMMT0067-WERKS
         AND MATKL EQ _ZMMT0067-MATKL
         AND NIVEL NOT IN (R_OPERACAO->C_STATUS-APROVADO,
                           R_OPERACAO->C_STATUS-REJEITADO).
    ENDIF.
  ENDLOOP.

ENDFORM.                    "F_SELECIONA_CONTRATOS

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_ESTRATEGIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM F_SELECIONA_ESTRATEGIA TABLES T_ZMMT0067.
  SELECT *
    FROM ZMMT0067
    INTO TABLE T_ZMMT0067
   WHERE APROVADOR = SY-UNAME.
ENDFORM.                    "F_SELECIONA_ESTRATEGIA

*----------------------------------------------------------------------*
*      -->P_HEADER   text
*----------------------------------------------------------------------*
FORM F_BUILD_HEADER USING
                    P_WIDTH
                    P_TITLE
                    CHANGING
                    P_HEADER TYPE TREEV_HHDR.

  P_HEADER-HEADING = P_TITLE.
  P_HEADER-TOOLTIP = P_TITLE.
  P_HEADER-WIDTH   = P_WIDTH.
ENDFORM.                    "BUILD_HEADER

*&---------------------------------------------------------------------*
*&      Form  ADD_PAI
*&---------------------------------------------------------------------*
*FORM F_ADD_PAI USING
*             P_RELAT_KEY  TYPE LVC_NKEY
*             P_NODE_IMAGE TYPE TV_IMAGE
*             P_NODE_PAI   TYPE CHAR50
*             CHANGING
*             P_PAI_KEY    TYPE LVC_NKEY.
*
*  LW_NODE_TEXT           = P_NODE_PAI.
*  LW_LAYOUT_NODE-N_IMAGE = P_NODE_IMAGE.
*
*  CALL METHOD OBJ_ALV_TREE_0100->ADD_NODE
*    EXPORTING
*      I_RELAT_NODE_KEY = P_RELAT_KEY
*      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
*      I_NODE_TEXT      = LW_NODE_TEXT
*      IS_NODE_LAYOUT   = LW_LAYOUT_NODE
*    IMPORTING
*      E_NEW_NODE_KEY   = P_PAI_KEY.
*
*ENDFORM.                    "ADD_PAI

*&---------------------------------------------------------------------*
*&      Form  ADD_FILHO
*&---------------------------------------------------------------------*
FORM F_ADD_FILHO USING
                 P_NODE_IMAGE  TYPE TV_IMAGE
                 P_NODE_FILHO  TYPE ANY
                 P_LAYOUT_ITEM TYPE LVC_T_LAYI
                 CHANGING
                 P_FILHO_KEY   TYPE LVC_NKEY.

  LW_NODE_TEXT           = P_NODE_FILHO.
  LW_LAYOUT_NODE-N_IMAGE = P_NODE_IMAGE.

  CALL METHOD OBJ_ALV_TREE_0100->ADD_NODE
    EXPORTING
      I_RELAT_NODE_KEY = SPACE
      I_RELATIONSHIP   = CL_GUI_COLUMN_TREE=>RELAT_LAST_CHILD
      I_NODE_TEXT      = LW_NODE_TEXT
      IS_NODE_LAYOUT   = LW_LAYOUT_NODE
      IT_ITEM_LAYOUT   = P_LAYOUT_ITEM
    IMPORTING
      E_NEW_NODE_KEY   = P_FILHO_KEY.

ENDFORM.                    "ADD_FILHO


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

  CASE SY-DYNNR.
    WHEN 0110.
      APPEND WL_FCAT TO GT_FCAT_0110.
    WHEN 0120.
      APPEND WL_FCAT TO GT_FCAT_0120.
    WHEN 0200.
      APPEND WL_FCAT TO GT_FCAT_0200.
    WHEN 0400.
      APPEND WL_FCAT TO GT_FCAT_0400.
  ENDCASE.

  CLEAR WL_FCAT.
ENDFORM.                    "ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
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
*&      Form  REGISTER_F4_FOR_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM REGISTER_F4_FOR_FIELDS.
  DATA: WL_F4 TYPE LVC_T_F4 WITH HEADER LINE.

  CLEAR WL_F4[].

  IF SY-DYNNR = 0100.
    WL_F4-FIELDNAME = 'CTG_CLAS'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    WL_F4-FIELDNAME = 'CTG_ITEM'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    WL_F4-FIELDNAME = 'MATERIAL'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    WL_F4-FIELDNAME = 'MAT_GROUP'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    WL_F4-FIELDNAME = 'NR_ORDEM'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    WL_F4-FIELDNAME = 'TAX_CODE'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    WL_F4-FIELDNAME = 'UNID_MED'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    CALL METHOD OBJ_ALV_0110->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = WL_F4[].

  ELSEIF SY-DYNNR = 0200.
    WL_F4-FIELDNAME = 'SAKNR'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    CALL METHOD OBJ_ALV_0200->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = WL_F4[].
  ENDIF.


ENDFORM.                    "REGISTER_F4_FOR_FIELDS

*&---------------------------------------------------------------------*
*&      Form  F_SELECTIONS_DIALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
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

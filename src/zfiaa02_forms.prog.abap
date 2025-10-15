*&---------------------------------------------------------------------*
*&  Include           ZFIAA02_FORMS
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
  FORM ALV_PREENCHE_CAT       USING: P_CAMPO         TYPE C
                                     P_DESC          TYPE C
                                     P_TAM           TYPE C
                                     P_HOT           TYPE C
                                     P_ZERO          TYPE C
                                     P_SUM           TYPE C
                                     P_CHECK         TYPE C
                                     P_EDIT          TYPE C
                                     P_ICON          TYPE C
                                     P_REF_TABNAME   LIKE DD02D-TABNAME
                                     P_REF_FIELDNAME LIKE DD03D-FIELDNAME
                                     P_TABNAME       LIKE DD02D-TABNAME
                                     P_F4            TYPE DDF4AVAIL
                                     P_HOTSPOT       TYPE C
                                     NO_OUT          TYPE C.

    WL_FCAT-FIELDNAME  = P_CAMPO.
    WL_FCAT-SCRTEXT_L  = P_DESC.
    WL_FCAT-SCRTEXT_M  = P_DESC.
    WL_FCAT-SCRTEXT_S  = P_DESC.
    WL_FCAT-HOTSPOT    = P_HOT.
    WL_FCAT-NO_ZERO    = P_ZERO.
    WL_FCAT-OUTPUTLEN  = P_TAM.
    WL_FCAT-EDIT       = P_EDIT.
    WL_FCAT-ICON       = P_ICON.
    WL_FCAT-CHECKBOX   = P_CHECK.
    WL_FCAT-REF_TABLE  = P_REF_TABNAME.
    WL_FCAT-REF_FIELD  = P_REF_FIELDNAME.
    WL_FCAT-TABNAME    = P_REF_TABNAME.
    WL_FCAT-F4AVAILABL = P_F4.
    WL_FCAT-HOTSPOT    = P_HOTSPOT.
    WL_FCAT-NO_OUT     = NO_OUT.

    CASE SY-DYNNR.
      WHEN 0110.
        APPEND WL_FCAT TO GT_FCAT_0110.
      WHEN 0140.
        APPEND WL_FCAT TO GT_FCAT_0140.
      WHEN 0300.
        APPEND WL_FCAT TO GT_FCAT_0300.
      WHEN OTHERS.
    ENDCASE.

    CLEAR WL_FCAT.
  ENDFORM.                    "ALV_PREENCHE_CAT

*&---------------------------------------------------------------------*
*&      Form  alv_preenche_cat_slis
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM ALV_PREENCHE_CAT_SLIS  USING FIELDNAME
                                    SELTEXT_M
                                    CHECKBOX
                                    EDIT
                                    NO_ZERO.

    WL_FCAT_SLIS-FIELDNAME   = FIELDNAME.
    WL_FCAT_SLIS-SELTEXT_M   = SELTEXT_M.
    WL_FCAT_SLIS-CHECKBOX    = CHECKBOX.
    WL_FCAT_SLIS-EDIT        = EDIT.
    WL_FCAT_SLIS-NO_ZERO     = NO_ZERO.

    APPEND WL_FCAT_SLIS TO GT_FCAT_SLIS.
    CLEAR WL_FCAT_SLIS.

  ENDFORM.                    "alv_preenche_cat_slis

*&---------------------------------------------------------------------*
*&      Form  REGISTER_F4_FOR_FIELDS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM REGISTER_F4_FOR_FIELDS.
    DATA WL_F4 TYPE LVC_T_F4 WITH HEADER LINE.

    WL_F4-FIELDNAME = 'DEP_RESP'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    WL_F4-FIELDNAME = 'WAERS'.
    WL_F4-REGISTER  = 'X' .
    WL_F4-GETBEFORE = 'X' .
    APPEND WL_F4.

    CALL METHOD OBJ_ALV_0140->REGISTER_F4_FOR_FIELDS
      EXPORTING
        IT_F4 = WL_F4[].

  ENDFORM.                    "REGISTER_F4_FOR_FIELDS

*&---------------------------------------------------------------------*
*&      Form  F_PREENCHER_DYNPRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_START    text
*      -->L_NAME     text
*      -->L_VALUE    text
*----------------------------------------------------------------------*
  FORM F_PREENCHER_DYNPRO USING L_START TYPE C
                                L_NAME  TYPE C
                                L_VALUE.

    MOVE L_START TO WL_BDC-DYNBEGIN.
    IF L_START = 'X'.
      MOVE:
          L_NAME  TO WL_BDC-PROGRAM,
          L_VALUE TO WL_BDC-DYNPRO.
    ELSE.
      MOVE:
          L_NAME  TO WL_BDC-FNAM,
          L_VALUE TO WL_BDC-FVAL.
    ENDIF.

    APPEND WL_BDC TO GT_BDC.
    CLEAR WL_BDC.
  ENDFORM.                    "F_PREENCHER_DYNPRO

*&---------------------------------------------------------------------*
*&      Form  F_PEGA_IMAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->NOME_LOGO  text
*      -->URL        text
*----------------------------------------------------------------------*
  FORM F_PEGA_IMAGEM  USING    NOME_LOGO
                      CHANGING URL.

    DATA: BEGIN OF GRAPHIC_TABLE OCCURS 0,
            LINE(255) TYPE X,
          END OF GRAPHIC_TABLE.

    DATA: L_GRAPHIC_CONV TYPE I.
    DATA: L_GRAPHIC_OFFS TYPE I.
    DATA: GRAPHIC_SIZE   TYPE I.
    DATA: L_GRAPHIC_XSTR TYPE XSTRING.

    REFRESH GRAPHIC_TABLE.
    CALL METHOD CL_SSF_XSF_UTILITIES=>GET_BDS_GRAPHIC_AS_BMP
      EXPORTING
        P_OBJECT = 'GRAPHICS'
        P_NAME   = NOME_LOGO
        P_ID     = 'BMAP'
        P_BTYPE  = 'BCOL'
      RECEIVING
        P_BMP    = L_GRAPHIC_XSTR.

    GRAPHIC_SIZE = XSTRLEN( L_GRAPHIC_XSTR ).
    L_GRAPHIC_CONV = GRAPHIC_SIZE.
    L_GRAPHIC_OFFS = 0.
    WHILE L_GRAPHIC_CONV > 255.
      GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(255).
      APPEND GRAPHIC_TABLE.
      L_GRAPHIC_OFFS = L_GRAPHIC_OFFS + 255.
      L_GRAPHIC_CONV = L_GRAPHIC_CONV - 255.
    ENDWHILE.
    GRAPHIC_TABLE-LINE = L_GRAPHIC_XSTR+L_GRAPHIC_OFFS(L_GRAPHIC_CONV).
    APPEND GRAPHIC_TABLE.
    CALL FUNCTION 'DP_CREATE_URL'
      EXPORTING
        TYPE     = 'IMAGE'
        SUBTYPE  = 'X-UNKNOWN'
        SIZE     = GRAPHIC_SIZE
        LIFETIME = 'T'
      TABLES
        DATA     = GRAPHIC_TABLE
      CHANGING
        URL      = URL.
  ENDFORM.                    " F_PEGA_IMAGEM

*&---------------------------------------------------------------------*
*&      Form  EXCLUDE_TB_FUNCTIONS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->WL_EXC_BUTTON  text
*----------------------------------------------------------------------*
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
*&      Form  TOP-OF-PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
  FORM TOP-OF-PAGE.

    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        IT_LIST_COMMENTARY = GT_HEADER.
*     I_LOGO                   =
*     I_END_OF_LIST_GRID       =

  ENDFORM.                    "TOP_PAGE_0150

*&---------------------------------------------------------------------*
*&      Form  header_title
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->TITLE      text
*----------------------------------------------------------------------*
  FORM HEADER_TITLE USING I_TITLE TYPE TEXT50.

    LT_HEADER-TYP  = 'H'.
    LT_HEADER-INFO = I_TITLE.
    APPEND LT_HEADER TO GT_HEADER.
    CLEAR LT_HEADER.

  ENDFORM.                    "header_title

*&---------------------------------------------------------------------*
*&      Form  header_info
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->I_KEY      text
*      -->I_INFO     text
*----------------------------------------------------------------------*
  FORM HEADER_INFO USING I_KEY
                         I_INFO.

    LT_HEADER-TYP  = 'S'.
    LT_HEADER-KEY  = I_KEY.
    LT_HEADER-INFO = I_INFO.
    APPEND LT_HEADER TO GT_HEADER.
    CLEAR LT_HEADER.

  ENDFORM.                    "header_info
